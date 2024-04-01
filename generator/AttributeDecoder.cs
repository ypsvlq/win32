using System.Reflection.Metadata;

namespace ZigWin32;

enum AttributeType {
    Other,
    EnumInt32,
}

class AttributeDecoder : ICustomAttributeTypeProvider<AttributeType> {
    public AttributeType GetPrimitiveType(PrimitiveTypeCode typeCode) => AttributeType.Other;

    public AttributeType GetTypeFromReference(MetadataReader reader, TypeReferenceHandle handle, byte rawTypeKind) {
        var type = reader.GetTypeReference(handle);
        var name = reader.GetString(type.Name);
        return name switch {
            "CallingConvention" or "Architecture" or "AttributeTargets" => AttributeType.EnumInt32,
        };
    }

    public PrimitiveTypeCode GetUnderlyingEnumType(AttributeType type) => type switch {
        AttributeType.EnumInt32 => PrimitiveTypeCode.Int32,
    };

    public bool IsSystemType(AttributeType type) => false;

    public AttributeType GetSystemType() => throw new NotImplementedException();
    public AttributeType GetSZArrayType(AttributeType elementType) => throw new NotImplementedException();
    public AttributeType GetTypeFromDefinition(MetadataReader reader, TypeDefinitionHandle handle, byte rawTypeKind) => throw new NotImplementedException();
    public AttributeType GetTypeFromSerializedName(string name) => throw new NotImplementedException();
}
