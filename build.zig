const std = @import("std");

pub fn build(b: *std.Build) void {
    _ = b.addModule("win32", .{
        .root_source_file = b.path("src/win32.zig"),
        .target = b.standardTargetOptions(.{}),
        .optimize = b.standardOptimizeOption(.{}),
    });

    const generate = b.addSystemCommand(&.{ "dotnet", "run", "--project", "generator" });
    generate.addFileArg(b.path("src/win32.zig"));

    const generate_step = b.step("generate", "Generate win32.zig");
    generate_step.dependOn(&generate.step);
}
