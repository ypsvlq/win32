using System.Collections.Immutable;
using System.Reflection.Metadata;

namespace ZigWin32;

interface IGenericContext { }

class ZigTypeDecoder : ISignatureTypeProvider<ZigType, IGenericContext?> {
    public HashSet<string> Rename = [];
    public HashSet<string> Interfaces = [];
    public Dictionary<string, string> Enums = [];

    public string GetName(MetadataReader reader, dynamic type) {
        var name = reader.GetString((StringHandle)type.Name);
        if (Rename.Contains(name)) {
            var prefix = reader.GetString((StringHandle)type.Namespace).Split('.').Last();
            return $"{prefix}_{name}";
        } else if (Enums.TryGetValue(name, out var result)) {
            name = result;
        }
        return name;
    }

    public ZigType GetArrayType(ZigType elementType, ArrayShape shape) {
        elementType.Shape = shape;
        return elementType;
    }

    public ZigType GetPointerType(ZigType elementType) {
        elementType.Pointers++;
        if (elementType.Name == "void") {
            elementType.Name = "anyopaque";
        }
        return elementType;
    }

    public ZigType GetPrimitiveType(PrimitiveTypeCode typeCode) => new(typeCode switch {
        PrimitiveTypeCode.Void => "void",
        PrimitiveTypeCode.Boolean => "bool",
        PrimitiveTypeCode.Char => "u16",
        PrimitiveTypeCode.SByte => "i8",
        PrimitiveTypeCode.Byte => "u8",
        PrimitiveTypeCode.Int16 => "i16",
        PrimitiveTypeCode.UInt16 => "u16",
        PrimitiveTypeCode.Int32 => "i32",
        PrimitiveTypeCode.UInt32 => "u32",
        PrimitiveTypeCode.Int64 => "i64",
        PrimitiveTypeCode.UInt64 => "u64",
        PrimitiveTypeCode.Single => "f32",
        PrimitiveTypeCode.Double => "f64",
        PrimitiveTypeCode.String => "String",
        PrimitiveTypeCode.IntPtr => "isize",
        PrimitiveTypeCode.UIntPtr => "usize",
    });

    public ZigType GetTypeFromDefinition(MetadataReader reader, TypeDefinitionHandle handle, byte rawTypeKind) {
        var type = reader.GetTypeDefinition(handle);
        var name = GetName(reader, type);
        var zigType = new ZigType(name);
        if (Interfaces.Contains(name)) {
            zigType.Pointers++;
        }
        return zigType;
    }

    public ZigType GetTypeFromReference(MetadataReader reader, TypeReferenceHandle handle, byte rawTypeKind) {
        var type = reader.GetTypeReference(handle);
        var name = GetName(reader, type);
        var forcePointer = Interfaces.Contains(name) || name == "PSTR" || name == "PWSTR";
        name = name switch {
            "Guid" => "GUID",
            "PSTR" => "u8",
            "PWSTR" => "u16",
            _ => name,
        };
        var zigType = new ZigType(name);
        if (forcePointer) {
            zigType.Pointers++;
        }
        return zigType;
    }

    public ZigType GetByReferenceType(ZigType elementType) => throw new NotImplementedException();
    public ZigType GetFunctionPointerType(MethodSignature<ZigType> signature) => throw new NotImplementedException();
    public ZigType GetGenericInstantiation(ZigType genericType, ImmutableArray<ZigType> typeArguments) => throw new NotImplementedException();
    public ZigType GetGenericMethodParameter(IGenericContext? genericContext, int index) => throw new NotImplementedException();
    public ZigType GetGenericTypeParameter(IGenericContext? genericContext, int index) => throw new NotImplementedException();
    public ZigType GetModifiedType(ZigType modifier, ZigType unmodifiedType, bool isRequired) => throw new NotImplementedException();
    public ZigType GetPinnedType(ZigType elementType) => throw new NotImplementedException();
    public ZigType GetSZArrayType(ZigType elementType) => throw new NotImplementedException();
    public ZigType GetTypeFromSpecification(MetadataReader reader, IGenericContext? genericContext, TypeSpecificationHandle handle, byte rawTypeKind) => throw new NotImplementedException();
}
