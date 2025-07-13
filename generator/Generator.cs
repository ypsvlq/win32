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
    readonly SortedDictionary<string, TypeDefinition> typedefs = [];
    readonly SortedDictionary<string, List<TypeWithAttributes>> types = [];
    readonly SortedDictionary<string, List<TypeWithAttributes>> interfaces = [];
    readonly SortedDictionary<string, List<TypeDefinition>> enums = [];
    readonly SortedDictionary<string, List<MethodWithAttributes>> methods = [];
    readonly SortedDictionary<string, FieldDefinition> fields = [];
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
                    methods[name].Add(new(reader, method));
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
            // Copyright (c) Microsoft Corporation
            //
            // Permission is hereby granted, free of charge, to any person obtaining a copy
            // of this software and associated documentation files (the "Software"), to deal
            // in the Software without restriction, including without limitation the rights
            // to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
            // copies of the Software, and to permit persons to whom the Software is
            // furnished to do so, subject to the following conditions:
            //
            // The above copyright notice and this permission notice shall be included in all
            // copies or substantial portions of the Software.
            //
            // THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
            // IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
            // FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
            // AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
            // LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
            // OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
            // SOFTWARE.
            const std = @import("std");
            const arch = @import("builtin").cpu.arch;
            const FlexibleArrayType = std.zig.c_translation.FlexibleArrayType;
            pub const GUID = std.os.windows.GUID;
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
                    if (info.Type.GetFields().Count == 0) {
                        output.Write("pub const ");
                        if (!name.Contains('_')) output.Write("CLSID_");
                        output.WriteLine($"{name}: GUID = {ZigGuid(attribute.FixedArguments)};");
                        continue;
                    }
                }
                output.Write($"pub const {name} = ");
                GenerateType(info);
                output.WriteLine(';');
            }
        }

        foreach (var list in interfaces.Values) {
            foreach (var info in list) {
                var self = zigTypeDecoder.GetName(reader, info.Type);

                if (info.Attributes.TryGetValue("Guid", out var attribute)) {
                    output.WriteLine($"pub const IID_{self}: GUID = {ZigGuid(attribute.FixedArguments)};");
                }

                switch (self) {
                    case "ICompositionCapabilitiesInteropFactory":
                    case "ICompositorDesktopInterop":
                    case "ICompositorInterop":
                    case "ICompositorInterop2":
                    case "IGraphicsEffectD2D1Interop":
                        continue;
                }

                output.WriteLine($$"""
                    pub const {{self}} = extern struct {
                        lpVtbl: *const Vtbl,
                        pub const Vtbl = extern struct {
                    """);
                indent += 2;

                Stack<List<MethodDefinition>> interfaceStack = [];
                TypeDefinition type = info.Type;
                while (true) {
                    interfaceStack.Push([.. type.GetMethods().Select(reader.GetMethodDefinition)]);

                    var implementations = type.GetInterfaceImplementations();
                    if (implementations.Count == 0) break;

                    var parent = implementations.Select(reader.GetInterfaceImplementation)
                        .Select(implementation => (TypeReferenceHandle)implementation.Interface)
                        .Select(reader.GetTypeReference)
                        .Single();
                    type = interfaces[reader.GetString(parent.Name)].Single(info => info.Type.Namespace == parent.Namespace).Type;
                }

                var overloads = interfaceStack.SelectMany(x => x)
                    .Select(method => method.Name)
                    .GroupBy(x => x)
                    .Where(group => group.Count() > 1)
                    .ToDictionary(group => group.Key, _ => 1);

                HashSet<string> badNames = [];

                foreach (var methods in interfaceStack) {
                    foreach (var method in methods) {
                        var name = ZigName(method.Name);
                        badNames.Add(name);
                        Indent();
                        output.Write($"{name}");
                        if (overloads.TryGetValue(method.Name, out var count)) {
                            output.Write($"_{count}");
                            overloads[method.Name]++;
                        }
                        output.Write($": *const fn (self: *{self}");
                        GenerateMethodSignature(method, CallingConvention.ThisCall, ", ");
                        output.WriteLine(',');
                    }
                }
                indent--;
                Indent();
                output.WriteLine("};");

                overloads = overloads.Keys.ToDictionary(x => x, _ => 1);

                foreach (var methods in interfaceStack) {
                    foreach (var method in methods) {
                        var name = ZigName(method.Name);
                        if (overloads.TryGetValue(method.Name, out var count)) {
                            name += $"_{count}";
                            overloads[method.Name]++;
                        }

                        Indent();
                        output.Write($"pub fn {name}(self: *{self}");
                        var signature = GenerateMethodSignature(method, 0, ", ", badNames);
                        output.WriteLine(" {");
                        indent++;

                        var returnType = signature.ReturnType;
                        if (typedefs.TryGetValue(returnType.ToString(), out var typedef)) {
                            returnType = reader.GetFieldDefinition(typedef.GetFields().First()).DecodeSignature(zigTypeDecoder, null);
                        }
                        var primitive = returnType.IsPrimitive();

                        Indent();
                        if (!primitive) {
                            output.WriteLine($"var result: {returnType} = undefined;");
                            Indent();
                        } else {
                            output.Write("return ");
                        }

                        output.Write($"self.lpVtbl.{name}(self");
                        if (!primitive) output.Write(", &result");
                        var parameters = method.GetParameters()
                            .Select(reader.GetParameter)
                            .Select(parameter => parameter.Name)
                            .Select(name => ZigName(name, badNames))
                            .Where(name => name.Length > 0);
                        foreach (var parameter in parameters) {
                            output.Write($", {parameter}");
                        }
                        output.WriteLine(");");

                        if (!primitive) {
                            Indent();
                            output.WriteLine("return result;");
                        }

                        indent--;
                        Indent();
                        output.WriteLine("}");
                    }
                }

                indent--;
                output.WriteLine("};");
            }
        }

        foreach (var (name, list) in methods) {
            switch (name) {
                case "CreateDispatcherQueueController":
                case "RoCreateNonAgilePropertySet":
                case "RoCreatePropertySetSerializer":
                    continue;
            }

            if (missingDlls.Contains(list[0].Dll)) {
                continue;
            } else if (list[0].Attributes.ContainsKey("SupportedArchitecture")) {
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
                    output.Write(" => @extern(*const fn (");
                    GenerateMethodSignature(info.Method, info.CallingConvention);
                    output.WriteLine($", .{{ .name = \"{name}\", .library_name = \"{info.Dll}\" }}),");
                }
                if (methodAliasesX86.TryGetValue(name, out var alias)) {
                    Indent();
                    output.WriteLine($".x86 => {alias},");
                }
                indent--;
                output.WriteLine($$"""
                        else => @compileError("{{name}} unsupported for target architecture"),
                    };
                    """);
            } else if (list.Count > 1) {
                output.WriteLine($"pub const {name} = @compileError(\"{name} is defined in multiple DLLs\");");
            } else {
                var info = list[0];
                if (info.Dll == "forceinline") {
                    Indent();
                    output.Write($"pub fn {name}(");
                    var signature = GenerateMethodSignature(info.Method, 0);
                    output.WriteLine(" {");
                    indent++;
                    var returnType = signature.ReturnType;
                    if (typedefs.TryGetValue(returnType.ToString(), out var typedef)) {
                        returnType = reader.GetFieldDefinition(typedef.GetFields().First()).DecodeSignature(zigTypeDecoder, null);
                    }
                    Indent();
                    output.WriteLine($"return {returnType.FormatConstant((string)info.Attributes["Constant"].FixedArguments[0])};");
                    indent--;
                    output.WriteLine("}");
                } else {
                    Indent();
                    output.Write($"pub extern \"{info.Dll}\" fn {name}(");
                    GenerateMethodSignature(info.Method, info.CallingConvention);
                    output.WriteLine(';');
                }
            }
        }

        var constants = new SortedDictionary<string, (string, string)>();

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
                    constants.Add(name, (valueType.ToString(), value));
                }
            }
        }

        foreach (var (name, field) in fields) {
            var value = "";
            var type = field.DecodeSignature(zigTypeDecoder, null);
            var typeName = type.ToString();
            var attributes = new Attributes(reader, field);
            if (typedefs.TryGetValue(typeName, out var typedef)) {
                type = reader.GetFieldDefinition(typedef.GetFields().First()).DecodeSignature(zigTypeDecoder, null);
            }
            switch (typeName) {
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
                case "PROPERTYKEY":
                case "DEVPROPKEY":
                    var list = ((string)attributes["Constant"].FixedArguments[0])[1..].Replace("}", "").Split(',', StringSplitOptions.TrimEntries);
                    value = $".{{ .fmtid = {ZigGuid(list)}, .pid = {list[11]} }}";
                    break;
                case "SID_IDENTIFIER_AUTHORITY":
                    value = $".{{ .Value = .{{ {((string)attributes["Constant"].FixedArguments[0])[1..^1]} }} }}";
                    break;
                case "CONDITION_VARIABLE":
                case "INIT_ONCE":
                case "SRWLOCK":
                    value = $".{{ .Ptr = @ptrFromInt({(string)attributes["Constant"].FixedArguments[0]}) }}";
                    break;
                default:
                    value = type.FormatConstant(type.ReadBlob(reader.GetBlobReader(reader.GetConstant(field.GetDefaultValue()).Value)));
                    break;
            }
            var suffix = types.ContainsKey(name) ? "_" : "";
            constants.Add($"{name}{suffix}", (typeName, value));
        }

        foreach (var (name, (type, value)) in constants) {
            output.Write($"pub const {name}");
            if (type != "String") output.Write($": {type}");
            output.WriteLine($" = {value};");
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
        var pack = info.Type.GetLayout().PackingSize;
        var align = pack == 0 ? "" : $" align({pack})";

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
                output.Write($"{align},");
            }
        }

        indent--;
        if (fields.Any()) {
            output.WriteLine();
            Indent();
        }
        output.Write('}');
    }

    MethodSignature<ZigType> GenerateMethodSignature(MethodDefinition method, CallingConvention callingConvention, string prefix = "", HashSet<string>? badNames = null) {
        var signature = method.DecodeSignature(zigTypeDecoder, null);
        var parameters = method.GetParameters().Select(reader.GetParameter).Where(parameter => reader.GetString(parameter.Name).Length > 0);
        var names = parameters.Select(parameter => parameter.Name).Select(name => ZigName(name, badNames));
        var hasNames = prefix != "" || names.Any(name => !name.StartsWith("param"));

        var returnTypeName = new Attributes(reader, method).ContainsKey("DoesNotReturn") ? "noreturn" : signature.ReturnType.ToString();

        if (callingConvention == CallingConvention.ThisCall) {
            var returnType = signature.ReturnType;
            if (typedefs.TryGetValue(returnType.ToString(), out var typedef)) {
                returnType = reader.GetFieldDefinition(typedef.GetFields().First()).DecodeSignature(zigTypeDecoder, null);
            }
            if (!returnType.IsPrimitive()) {
                output.Write($", result: *{returnType}");
                returnTypeName = "void";
            }
        }

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
                CallingConvention.Winapi or CallingConvention.ThisCall => "winapi",
                CallingConvention.Cdecl => "c",
            };
            output.Write($"callconv(.{callconv}) ");
        }

        output.Write(returnTypeName);

        return signature;
    }

    static string ZigGuid(object[] guid) {
        return $".{{ .Data1 = {guid[0]}, .Data2 = {guid[1]}, .Data3 = {guid[2]}, .Data4 = .{{ {guid[3]}, {guid[4]}, {guid[5]}, {guid[6]}, {guid[7]}, {guid[8]}, {guid[9]}, {guid[10]} }} }}";
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

    class MethodWithAttributes {
        public MethodDefinition Method;
        public Attributes Attributes;
        public string Dll;
        public CallingConvention CallingConvention;

        public MethodWithAttributes(MetadataReader reader, MethodDefinition method) {
            Method = method;
            Attributes = new(reader, method);

            var import = method.GetImport();
            Dll = Path.GetFileNameWithoutExtension(reader.GetString(reader.GetModuleReference(import.Module).Name)).ToLowerInvariant();
            CallingConvention = import.Attributes.HasFlag(MethodImportAttributes.CallingConventionCDecl) ? CallingConvention.Cdecl : CallingConvention.Winapi;
        }
    }

    enum Architecture {
        x86 = 1,
        x86_64 = 2,
        aarch64 = 4,
    }

    readonly Dictionary<string, string> methodAliasesX86 = new() {
        { "GetClassLongPtrA", "GetClassLongA" },
        { "GetClassLongPtrW", "GetClassLongW" },
        { "GetWindowLongPtrA", "GetWindowLongA" },
        { "GetWindowLongPtrW", "GetWindowLongW" },
        { "SetClassLongPtrA", "SetClassLongA" },
        { "SetClassLongPtrW", "SetClassLongW" },
        { "SetWindowLongPtrA", "SetWindowLongA" },
        { "SetWindowLongPtrW", "SetWindowLongW" },
    };

    readonly HashSet<string> missingDlls = [
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
