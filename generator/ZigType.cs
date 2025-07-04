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
        for (int i = 1; i <= Pointers; i++) {
            result += (Name == "anyopaque" && i == Pointers) ? "?*" : "[*c]";
        }
        if (Const && Pointers > 0) {
            result += "const ";
        }
        result += Name;
        return result;
    }

    public string ReadBlob(BlobReader reader) {
        if (Pointers > 0) {
            return reader.Length switch {
                4 => reader.ReadInt32().ToString(),
                2 => reader.ReadInt16().ToString(),
            };
        }
        return Name switch {
            "i8" => reader.ReadSByte().ToString(),
            "u8" => reader.ReadByte().ToString(),
            "i16" => reader.ReadInt16().ToString(),
            "u16" => reader.ReadUInt16().ToString(),
            "i32" or "isize" => reader.ReadInt32().ToString(),
            "u32" or "usize" => reader.ReadUInt32().ToString(),
            "i64" => reader.ReadInt64().ToString(),
            "u64" => reader.ReadUInt64().ToString(),
            "f32" => reader.ReadSingle().ToString(),
            "f64" => reader.ReadDouble().ToString(),
        };
    }

    public bool IsPrimitive() {
        return Pointers > 0 || Name switch {
            "void" or "bool" or "i8" or "u8" or "i16" or "u16" or "i32" or "u32" or "i64" or "u64" or "f32" or "f64" or "isize" or "usize" => true,
            _ => false,
        };
    }
};
