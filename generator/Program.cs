namespace ZigWin32;

internal class Program {
    private static void Main(string[] args) {
        Environment.CurrentDirectory = AppDomain.CurrentDomain.BaseDirectory;
        using var src = new StreamWriter("../../../../src/win32.zig") { NewLine = "\n" };
        using var test = new StreamWriter("../../../../src/test.zig") { NewLine = "\n" };
        var generator = new Generator(src, test);
        generator.Generate();
    }
}
