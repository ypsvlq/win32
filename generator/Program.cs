namespace ZigWin32;

internal class Program {
    private static void Main() {
        Environment.CurrentDirectory = AppDomain.CurrentDomain.BaseDirectory;
        using var output = new StreamWriter("win32.zig") { NewLine = "\n" };
        var generator = new Generator(output);
        generator.Generate();
    }
}
