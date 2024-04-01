namespace ZigWin32;

internal class Program {
    private static void Main(string[] args) {
        Environment.CurrentDirectory = AppDomain.CurrentDomain.BaseDirectory;
        using var output = new StreamWriter(args[0]) { NewLine = "\n" };
        var generator = new Generator(output);
        generator.Generate();
    }
}
