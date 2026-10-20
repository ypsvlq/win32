namespace ZigWin32;

internal class Program {
    private static void Main(string[] args) {
        Environment.CurrentDirectory = AppDomain.CurrentDomain.BaseDirectory;
        using var output = new StreamWriter("../../../../src/win32.zig") { NewLine = "\n" };
        var generator = new Generator(output);
        generator.Generate();
    }
}
