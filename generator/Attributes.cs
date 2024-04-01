using System.Reflection.Metadata;

namespace ZigWin32;

class AttributeArguments(object[] fixedArguments, Dictionary<string, object> namedArguments) {
    public object[] FixedArguments = fixedArguments;
    public Dictionary<string, object> NamedArguments = namedArguments;
}

class Attributes : Dictionary<string, AttributeArguments> {
    static readonly AttributeDecoder decoder = new();

    public Attributes(MetadataReader reader, dynamic target) {
        var attributes = ((CustomAttributeHandleCollection)target.GetCustomAttributes()).Select(reader.GetCustomAttribute);
        foreach (var attribute in attributes) {
            var member = reader.GetMemberReference((MemberReferenceHandle)attribute.Constructor);
            var parent = reader.GetTypeReference((TypeReferenceHandle)member.Parent);
            var name = reader.GetString(parent.Name)[..^"Attribute".Length];

            var value = attribute.DecodeValue(decoder);
            var fixedArguments = value.FixedArguments.Select(arg => arg.Value!).ToArray();
            var namedArguments = value.NamedArguments.ToDictionary(arg => arg.Name!, arg => arg.Value!);

            TryAdd(name, new AttributeArguments(fixedArguments, namedArguments));
        }
    }
}
