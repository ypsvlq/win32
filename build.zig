const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    _ = b.addModule("win32", .{
        .root_source_file = .{ .path = "src/win32.zig" },
        .target = target,
        .optimize = optimize,
    });

    const unit_tests = b.addTest(.{
        .root_source_file = .{ .path = "src/win32.zig" },
        .target = target,
        .optimize = optimize,
    });

    const run_unit_tests = b.addRunArtifact(unit_tests);

    const test_step = b.step("test", "Run unit tests");
    test_step.dependOn(&run_unit_tests.step);

    const generate = b.addSystemCommand(&.{ "dotnet", "run", "--project", "generator" });
    generate.addFileArg(.{ .path = "src/win32.zig" });

    const generate_step = b.step("generate", "Generate src/win32.zig");
    generate_step.dependOn(&generate.step);
}
