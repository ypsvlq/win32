using System.Reflection;
using System.Reflection.Metadata;
using System.Reflection.PortableExecutable;
using System.Runtime.InteropServices;
using System.Text;

namespace ZigWin32;

class Generator {
    readonly MetadataReader reader;
    readonly StreamWriter output;
    readonly ZigTypeDecoder zigTypeDecoder = new();
    readonly SortedList<string, List<TypeWithAttributes>> types = [];
    readonly SortedList<string, TypeDefinition> typedefs = [];
    readonly SortedList<string, List<TypeWithAttributes>> interfaces = [];
    readonly SortedList<string, List<TypeDefinition>> enums = [];
    readonly SortedList<string, List<MethodDefinition>> methods = [];
    readonly SortedList<string, FieldDefinition> fields = [];
    readonly Dictionary<string, int> names = [];

    public Generator(StreamWriter writer) {
        var file = File.OpenRead("Windows.Win32.winmd");
        var pe = new PEReader(file);
        reader = pe.GetMetadataReader();

        output = writer;

        foreach (var type in reader.TypeDefinitions.Select(reader.GetTypeDefinition)) {
            var name = reader.GetString(type.Name);
            if (name == "Apis") {
                foreach (var method in type.GetMethods().Select(reader.GetMethodDefinition)) {
                    name = reader.GetString(method.Name);
                    methods.TryAdd(name, []);
                    methods[name].Add(method);
                }
                foreach (var field in type.GetFields().Select(reader.GetFieldDefinition)) {
                    name = reader.GetString(field.Name);
                    fields[name] = field;
                }
            } else if (name != "<Module>" && !type.IsNested) {
                var info = new TypeWithAttributes(reader, type);
                if (type.BaseType.IsNil) {
                    interfaces.TryAdd(name, []);
                    interfaces[name].Add(info);
                    continue;
                }
                switch (reader.GetString(reader.GetTypeReference((TypeReferenceHandle)type.BaseType).Name)) {
                    case "Enum":
                        enums.TryAdd(name, []);
                        enums[name].Add(type);
                        break;
                    case "Attribute":
                        break;
                    default:
                        if (info.Attributes.ContainsKey("NativeTypedef") || info.Attributes.ContainsKey("MetadataTypedef")) {
                            typedefs[name] = info.Type;
                        } else {
                            types.TryAdd(name, []);
                            types[name].Add(info);
                        }
                        break;
                }
            }
        }

        foreach (var (name, list) in types) {
            if (list.Count > 1 && !list[0].Attributes.ContainsKey("SupportedArchitecture")) {
                zigTypeDecoder.Rename.Add(name);
            }
        }

        foreach (var (name, list) in enums) {
            zigTypeDecoder.Enums.Add(name, reader.GetFieldDefinition(list[0].GetFields().First()).DecodeSignature(zigTypeDecoder, null).Name);
            foreach (var type in list) {
                foreach (var field in type.GetFields().Skip(1).Select(reader.GetFieldDefinition).Select(field => field.Name).Select(reader.GetString)) {
                    names[field] = names.GetValueOrDefault(field) + 1;
                }
            }
        }

        foreach (var (name, list) in interfaces) {
            if (list.Count > 1) {
                zigTypeDecoder.Rename.Add(name);
                foreach (var info in list) {
                    zigTypeDecoder.Interfaces.Add(zigTypeDecoder.GetName(reader, info.Type));
                }
            } else {
                zigTypeDecoder.Interfaces.Add(name);
            }
        }
    }

    int indent;

    void Indent() {
        for (int i = 0; i < indent; i++) {
            output.Write("    ");
        }
    }

    public void Generate() {
        output.WriteLine("""
            const std = @import("std");
            const arch = @import("builtin").cpu.arch;
            const FlexibleArrayType = std.zig.c_translation.FlexibleArrayType;
            pub const GUID = std.os.windows.GUID;
            pub const WINAPI = std.os.windows.WINAPI;
            pub const L = std.unicode.utf8ToUtf16LeStringLiteral;
            test {
                @setEvalBranchQuota(1000000);
                std.testing.refAllDeclsRecursive(@This());
            }
            """);

        foreach (var (name, type) in typedefs) {
            if (name == "PSTR" || name == "PWSTR") continue;
            var value = reader.GetFieldDefinition(type.GetFields().First());
            var target = value.DecodeSignature(zigTypeDecoder, null);
            output.WriteLine($"pub const {name} = {target};");
        }

        foreach (var (name, list) in types) {
            if (zigTypeDecoder.Rename.Contains(name)) {
                foreach (var info in list) {
                    output.Write($"pub const {zigTypeDecoder.GetName(reader, info.Type)} = ");
                    GenerateType(info);
                    output.WriteLine(';');
                }
            } else if (list.Count > 1) {
                output.WriteLine($"pub const {name} = switch (arch) {{");
                indent++;
                foreach (var info in list) {
                    var architecture = (Architecture)info.Attributes["SupportedArchitecture"].FixedArguments[0];
                    Indent();
                    var prefix = "";
                    foreach (var flag in Enum.GetValues<Architecture>()) {
                        if (architecture.HasFlag(flag)) {
                            output.Write($"{prefix}.{flag}");
                            prefix = ", ";
                        }
                    }
                    output.Write(" => ");
                    GenerateType(info);
                    output.WriteLine(",");
                }
                indent--;
                output.WriteLine($$"""
                        else => @compileError("{{name}} unsupported for target architecture"),
                    };
                    """);
            } else {
                var info = list[0];
                if (info.Attributes.TryGetValue("Guid", out var attribute)) {
                    output.Write($"pub const CLSID_{name} = ");
                    GenerateGuid(attribute.FixedArguments);
                    output.WriteLine(';');
                    if (info.Type.GetFields().Count == 0) {
                        continue;
                    }
                }
                output.Write($"pub const {name} = ");
                GenerateType(info);
                output.WriteLine(';');
            }
        }

        foreach (var (name, list) in interfaces) {
            switch (name) {
                case "ICompositionCapabilitiesInteropFactory":
                case "ICompositorDesktopInterop":
                case "ICompositorInterop":
                case "ICompositorInterop2":
                case "IGraphicsEffectD2D1Interop":
                    continue;
            }
            foreach (var info in list) {
                var self = zigTypeDecoder.GetName(reader, info.Type);
                if (info.Attributes.TryGetValue("Guid", out var attribute)) {
                    output.Write($"pub const IID_{self} = ");
                    GenerateGuid(attribute.FixedArguments);
                    output.WriteLine(';');
                }
                GenerateInterface(self, info.Type);
            }
        }

        var architectureMethods = new SortedDictionary<Architecture, List<MethodDefinition>>();

        foreach (var (name, list) in methods) {
            switch (name) {
                case "CreateDispatcherQueueController":
                case "RoCreateNonAgilePropertySet":
                case "RoCreatePropertySetSerializer":
                    continue;
            }

            foreach (var method in list) {
                var attributes = new Attributes(reader, method);

                if (attributes.TryGetValue("SupportedArchitecture", out var attribute)) {
                    var architecture = (Architecture)attribute.FixedArguments[0];
                    foreach (var flag in Enum.GetValues<Architecture>()) {
                        if (architecture.HasFlag(flag)) {
                            architectureMethods.TryAdd(flag, []);
                            architectureMethods[flag].Add(method);
                        }
                    }
                    continue;
                } else if (list.Count > 1) {
                    break;
                }

                GenerateMethod(method);
            }
        }

        output.WriteLine("pub usingnamespace switch (arch) {");
        indent++;
        foreach (var (architecture, list) in architectureMethods) {
            Indent();
            output.WriteLine($".{architecture} => struct {{");
            indent++;
            if (architecture == Architecture.x86) {
                output.WriteLine("""
                            pub const GetClassLongPtrA = GetClassLongA;
                            pub const GetClassLongPtrW = GetClassLongW;
                            pub const GetWindowLongPtrA = GetWindowLongA;
                            pub const GetWindowLongPtrW = GetWindowLongW;
                            pub const SetClassLongPtrA = SetClassLongA;
                            pub const SetClassLongPtrW = SetClassLongW;
                            pub const SetWindowLongPtrA = SetWindowLongA;
                            pub const SetWindowLongPtrW = SetWindowLongW;
                    """);
            }
            foreach (var method in list) {
                GenerateMethod(method);
            }
            indent--;
            Indent();
            output.WriteLine("},");
        }
        output.WriteLine("""
                else => struct {},
            };
            """);

        var constants = new SortedList<string, string>();

        foreach (var (prefix, list) in enums) {
            foreach (var type in list) {
                var values = type.GetFields().Select(reader.GetFieldDefinition);
                var valueType = values.First().DecodeSignature(zigTypeDecoder, null);
                foreach (var field in values.Skip(1)) {
                    var name = reader.GetString(field.Name);
                    if (names[name] > 1 || types.ContainsKey(name) || interfaces.ContainsKey(name) || methods.ContainsKey(name)) {
                        name = $"{prefix}_{name}";
                    }
                    var value = valueType.ReadBlob(reader.GetBlobReader(reader.GetConstant(field.GetDefaultValue()).Value));
                    constants.Add(name, value);
                }
            }
        }

        foreach (var (name, field) in fields) {
            switch (name) {
                case "CONDITION_VARIABLE_INIT":
                case "INIT_ONCE_STATIC_INIT":
                case "SRWLOCK_INIT":
                    constants.Add(name, ".{ .Ptr = null }");
                    continue;
            }
            var value = "";
            var type = field.DecodeSignature(zigTypeDecoder, null);
            var attributes = new Attributes(reader, field);
            if (typedefs.TryGetValue(type.Name, out var typedef)) {
                type = reader.GetFieldDefinition(typedef.GetFields().First()).DecodeSignature(zigTypeDecoder, null);
            }
            switch (type.Name) {
                case "String":
                    var unicode = !attributes.ContainsKey("NativeEncoding");
                    if (unicode) value += "L(";
                    value += "\"";
                    foreach (var c in Encoding.Unicode.GetString(reader.GetBlobBytes(reader.GetConstant(field.GetDefaultValue()).Value))) {
                        value += c switch {
                            '\\' => @"\\",
                            '\n' => @"\n",
                            < ' ' => $@"\x{(int)c:x2}",
                            _ => c.ToString(),
                        };
                    }
                    value += "\"";
                    if (unicode) value += ")";
                    break;
                case "GUID":
                    value = ZigGuid(attributes["Guid"].FixedArguments);
                    break;
                case "anyopaque":
                    value = $"@as(?*anyopaque, @ptrFromInt({type.ReadBlob(reader.GetBlobReader(reader.GetConstant(field.GetDefaultValue()).Value))}))";
                    break;
                case "PROPERTYKEY":
                case "DEVPROPKEY":
                    var list = ((string)attributes["Constant"].FixedArguments[0])[1..].Replace("}", "").Split(',', StringSplitOptions.TrimEntries);
                    value = $"{type}{{ .fmtid = {ZigGuid(list)}, .pid = {list[11]} }}";
                    break;
                case "SID_IDENTIFIER_AUTHORITY":
                    var bytes = ((string)attributes["Constant"].FixedArguments[0])[1..^1];
                    value = $"SID_IDENTIFIER_AUTHORITY{{ .Value = .{{ {bytes} }} }}";
                    break;
                default:
                    value = type.ReadBlob(reader.GetBlobReader(reader.GetConstant(field.GetDefaultValue()).Value));
                    break;
            }
            var suffix = types.ContainsKey(name) ? "_" : "";
            constants.Add($"{name}{suffix}", value);
        }

        foreach (var (name, value) in constants) {
            output.WriteLine($"pub const {name} = {value};");
        }
    }

    void GenerateType(TypeWithAttributes info) {
        if (info.Attributes.TryGetValue("UnmanagedFunctionPointer", out var attribute)) {
            var method = reader.GetMethodDefinition(info.Type.GetMethods().Skip(1).First());
            output.Write("?*const fn (");
            GenerateMethodSignature(method, (CallingConvention)attribute.FixedArguments[0]);
            return;
        }

        var nested = new Dictionary<string, TypeDefinition>();
        foreach (var handle in info.Type.GetNestedTypes()) {
            var child = reader.GetTypeDefinition(handle);
            nested.Add(reader.GetString(child.Name), child);
        }

        var keyword = info.Type.Attributes.HasFlag(TypeAttributes.ExplicitLayout) ? "union" : "struct";
        output.Write($"extern {keyword} {{");
        indent++;

        var fields = info.Type.GetFields().Select(reader.GetFieldDefinition);
        foreach (var field in fields) {
            output.WriteLine();
            Indent();
            var fieldName = ZigName(field.Name);
            var fieldType = field.DecodeSignature(zigTypeDecoder, null);
            var fieldAttributes = new Attributes(reader, field);
            if (fieldAttributes.ContainsKey("FlexibleArray")) {
                if (nested.TryGetValue(fieldType.Name, out var child)) {
                    output.Write($"pub const {fieldType.Name} = ");
                    GenerateType(new TypeWithAttributes(reader, child));
                    output.WriteLine(';');
                    Indent();
                }
                if (fieldType.Name == fieldName) {
                    fieldType.Name = $"@This().{fieldType.Name}";
                }
                fieldType.Shape = new();
                output.WriteLine($"pub fn {fieldName}(self: anytype) FlexibleArrayType(@TypeOf(self), {fieldType}) {{");
                indent++;
                Indent();
                output.WriteLine("const Intermediate = FlexibleArrayType(@TypeOf(self), u8);");
                Indent();
                output.WriteLine($"return @ptrCast(@alignCast(@as(Intermediate, @ptrCast(self)) + @sizeOf(@TypeOf(self.*)) + @sizeOf(@TypeOf(self.*)) % @alignOf({fieldType})));");
                indent--;
                Indent();
                output.Write('}');
            } else {
                output.Write($"{fieldName}: ");
                if (nested.TryGetValue(fieldType.Name, out var child)) {
                    GenerateType(new TypeWithAttributes(reader, child));
                } else {
                    fieldType.Const = fieldAttributes.ContainsKey("Const");
                    output.Write(fieldType);
                }
                output.Write(',');
            }
        }

        indent--;
        if (fields.Any()) {
            output.WriteLine();
            Indent();
        }
        output.Write('}');
    }

    void GenerateInterface(string self, TypeDefinition type, int level = 0, HashSet<string>? maybeUsed = null) {
        var vtable = type.GetMethods().Select(reader.GetMethodDefinition);
        var counts = vtable.Select(method => method.Name).GroupBy(x => x).Where(group => group.Count() > 1).ToDictionary(group => group.Key, _ => 1);
        var used = maybeUsed ?? [];
        var maybeParent = type.GetInterfaceImplementations()
                              .Select(reader.GetInterfaceImplementation)
                              .Select(i => (TypeReferenceHandle)i.Interface)
                              .Select(handle => (TypeReference?)reader.GetTypeReference(handle))
                              .SingleOrDefault();

        if (level == 0) {
            output.WriteLine($$"""
                pub const {{self}} = extern struct {
                    lpVtbl: *VTable,
                    const VTable = extern struct {
                """);
            indent += 2;
            if (maybeParent is TypeReference) {
                Indent();
                output.WriteLine($"base: {zigTypeDecoder.GetName(reader, maybeParent.Value)}.VTable,");
            }

            foreach (var method in vtable) {
                Indent();
                output.Write($"{ZigName(method.Name)}");
                if (counts.TryGetValue(method.Name, out int count)) {
                    output.Write($"_{count}");
                    counts[method.Name] = ++count;
                }
                output.Write(": *const fn (self: *const anyopaque");
                GenerateMethodSignature(method, CallingConvention.Winapi, ", ");
                output.WriteLine(',');
            }
            counts = counts.Keys.ToDictionary(x => x, _ => 1);

            indent--;
            Indent();
            output.WriteLine("};");
        }

        var badNames = vtable.Select(method => method.Name).Select(reader.GetString).ToHashSet();

        foreach (var method in vtable) {
            var name = ZigName(method.Name);
            if (counts.TryGetValue(method.Name, out int count)) {
                name += $"_{count}";
                counts[method.Name] = ++count;
            }

            Indent();
            var prefix = used.Add(name) ? "" : $"{reader.GetString(type.Name)}_";
            output.Write($"pub fn {prefix}{name}(self: *const {self}");
            GenerateMethodSignature(method, 0, ", ", badNames);
            output.WriteLine(" {");
            indent++;
            Indent();
            output.Write("return self.lpVtbl.");
            for (int i = 0; i < level; i++) output.Write("base.");
            output.Write($"{name}(self");
            foreach (var parameter in method.GetParameters().Select(reader.GetParameter).Select(parameter => parameter.Name).Select(name => ZigName(name, badNames)).Where(name => name.Length > 0)) {
                output.Write($", {parameter}");
            }
            output.WriteLine(");");
            indent--;
            Indent();
            output.WriteLine("}");
        }

        if (maybeParent is TypeReference parent) {
            var name = reader.GetString(parent.Name);
            GenerateInterface(self, type = interfaces[name].Where(info => info.Type.Namespace == parent.Namespace).Single().Type, level + 1, used);
        }

        if (level == 0) {
            indent--;
            output.WriteLine("};");
        }
    }

    void GenerateMethod(MethodDefinition method) {
        var name = reader.GetString(method.Name);
        var attributes = new Attributes(reader, method);

        var import = method.GetImport();
        var dll = Path.GetFileNameWithoutExtension(reader.GetString(reader.GetModuleReference(import.Module).Name)).ToLowerInvariant();

        if (unknownDlls.Contains(dll)) {
            return;
        } else if (dll == "forceinline") {
            Indent();
            output.Write($"pub fn {name}(");
            GenerateMethodSignature(method, 0);
            output.WriteLine(" {");
            indent++;
            Indent();
            output.WriteLine($"return {attributes["Constant"].FixedArguments[0]};");
            indent--;
            output.WriteLine("}");
            return;
        }

        var callingConvention = import.Attributes.HasFlag(MethodImportAttributes.CallingConventionCDecl) ? CallingConvention.Cdecl : CallingConvention.Winapi;
        Indent();
        output.Write($"pub extern \"{dll}\" fn {name}(");
        GenerateMethodSignature(method, callingConvention);
        output.WriteLine(';');
    }

    void GenerateMethodSignature(MethodDefinition method, CallingConvention callingConvention, string prefix = "", HashSet<string>? badNames = null) {
        var signature = method.DecodeSignature(zigTypeDecoder, null);
        var parameters = method.GetParameters().Select(reader.GetParameter).Where(parameter => reader.GetString(parameter.Name).Length > 0);
        var names = parameters.Select(parameter => parameter.Name).Select(name => ZigName(name, badNames));
        var hasNames = prefix != "" || names.Any(name => !name.StartsWith("param"));

        foreach (var (name, parameter, type) in names.Zip(parameters, signature.ParameterTypes)) {
            type.Const = new Attributes(reader, parameter).ContainsKey("Const");
            output.Write(prefix);
            if (hasNames) output.Write($"{name}: ");
            output.Write(type);
            prefix = ", ";
        }
        if (signature.Header.CallingConvention == SignatureCallingConvention.VarArgs) {
            output.Write(", ...");
        }
        output.Write(") ");

        if (callingConvention != 0) {
            var callconv = callingConvention switch {
                CallingConvention.Winapi => "WINAPI",
                CallingConvention.Cdecl => ".C",
            };
            output.Write($"callconv({callconv}) ");
        }

        output.Write(new Attributes(reader, method).ContainsKey("DoesNotReturn") ? "noreturn" : signature.ReturnType);
    }

    void GenerateGuid(object[] guid) {
        output.Write(ZigGuid(guid));
    }

    static string ZigGuid(object[] guid) {
        return $"GUID{{ .Data1 = {guid[0]}, .Data2 = {guid[1]}, .Data3 = {guid[2]}, .Data4 = .{{ {guid[3]}, {guid[4]}, {guid[5]}, {guid[6]}, {guid[7]}, {guid[8]}, {guid[9]}, {guid[10]} }} }}";
    }

    string ZigName(StringHandle handle, HashSet<string>? badNames = null) {
        var name = reader.GetString(handle);
        if (badNames != null) {
            if (badNames.Contains(name) || types.ContainsKey(name) || interfaces.ContainsKey(name) || methods.ContainsKey(name) || fields.ContainsKey(name) || names.ContainsKey(name)) {
                return $"{name}_";
            } else if (name == "type" || name.Length > 1 && name.StartsWith('i') && name.Skip(1).All(char.IsDigit)) {
                return $"@\"{name}\"";
            }
        }
        return name switch {
            "opaque" or "defer" or "align" or "error" or "callconv" or "var" or "resume" or "suspend" or "fn" or "async" => $"@\"{name}\"",
            _ => name,
        };
    }

    class TypeWithAttributes(MetadataReader reader, TypeDefinition type) {
        public TypeDefinition Type = type;
        public Attributes Attributes = new(reader, type);
    }

    enum Architecture {
        x86 = 1,
        x86_64 = 2,
        aarch64 = 4,
    }

    readonly HashSet<string> unknownDlls = [
        "amsi",
        "api-ms-win-appmodel-runtime-l1-1-6",
        "api-ms-win-core-ioring-l1-1-0",
        "api-ms-win-core-marshal-l1-1-0",
        "api-ms-win-core-memory-l1-1-8",
        "api-ms-win-core-psm-appnotify-l1-1-1",
        "api-ms-win-dx-d3dkmt-l1-1-0",
        "api-ms-win-mm-misc-l1-1-1",
        "api-ms-win-service-core-l1-1-5",
        "api-ms-win-wsl-api-l1-1-0",
        "bcp47mrm",
        "bcryptprimitives",
        "certadm",
        "certpoleng",
        "chakra",
        "cldapi",
        "d3d10_1",
        "d3dcsx",
        "dbgmodel",
        "deviceaccess",
        "dflayout",
        "dmprocessxmlfiltered",
        "drt",
        "drtprov",
        "drttransport",
        "dxcompiler",
        "efswrt",
        "fhsvcctl",
        "firewallapi",
        "fxsutility",
        "hhctrl",
        "ieframe",
        "infocardapi",
        "isolatedwindowsenvironmentutils",
        "kernelbase",
        "keycredmgr",
        "ksproxy",
        "licenseprotection",
        "magnification",
        "mdmlocalmanagement",
        "mdmregistration",
        "mfsrcsnk",
        "mrmsupport",
        "mscoree",
        "msdelta",
        "mspatchc",
        "netsh",
        "ninput",
        "ntdllk",
        "ondemandconnroutehelper",
        "opmxbox",
        "peerdist",
        "projectedfslib",
        "rpcproxy",
        "rtworkq",
        "sas",
        "sechost",
        "sensorsutilsv2",
        "srpapi",
        "tokenbinding",
        "ualapi",
        "vertdll",
        "vmdevicehost",
        "vmsavedstatedumpprovider",
        "wcmapi",
        "wdsbp",
        "wdsmc",
        "wdspxe",
        "winbio",
        "windows.media.mediacontrol",
        "windows.ui.xaml",
        "windows.ui",
        "winml",
        "wldp",
        "wmvcore",
        "wnvapi",
        "wsclient",
        "wsnmp32",
        "xolehlp",
        "xpsprint",
    ];
}
