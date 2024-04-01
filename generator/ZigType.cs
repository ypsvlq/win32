using System.Reflection;
using System.Reflection.Metadata;

namespace ZigWin32;

class ZigType(string name) {
    public string Name = name;
    public ArrayShape Shape;
    public int Pointers;
    public bool Const;

    public override string ToString() {
        var result = "";
        if (Shape.Rank > 0) {
            result += $"[{Shape.Sizes[0]}]";
        }
        for (int i = 0; i < Pointers; i++) {
            if (Name == "anyopaque" && i == Pointers - 1) {
                result += "?*";
            } else {
                result += "[*c]";
            }
        }
        if (Const && Pointers > 0) {
            result += "const ";
        }
        result += Name;
        return result;
    }

    public string ReadBlob(BlobReader reader) => Name switch {
        "i8" => reader.ReadSByte().ToString(),
        "u8" => reader.ReadByte().ToString(),
        "i16" => reader.ReadInt16().ToString(),
        "u16" => reader.ReadUInt16().ToString(),
        "i32" or "isize" => reader.ReadInt32().ToString(),
        "u32" or "usize" or "anyopaque" => reader.ReadUInt32().ToString(),
        "i64" => reader.ReadInt64().ToString(),
        "u64" => reader.ReadUInt64().ToString(),
        "f32" => reader.ReadSingle().ToString(),
        "f64" => reader.ReadDouble().ToString(),
    };
};
