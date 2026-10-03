const std = @import("std");

pub fn build(b: *std.Build) void {
    _ = b.addModule("win32", .{
        .root_source_file = b.path("src/win32.zig"),
        .target = b.standardTargetOptions(.{}),
        .optimize = b.standardOptimizeOption(.{}),
    });
}
