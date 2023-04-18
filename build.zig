const std = @import("std");

/// Global instance of this module's builder to be used by functions invoked from other modules.
var builder_instance: ?*std.Build = null;

pub fn build(b: *std.Build) void {
    builder_instance = b;

    const optimize = b.standardOptimizeOption(.{});
    const target = b.standardTargetOptions(.{});

    _ = b.addModule("mach-sysjs", .{
        .source_file = .{ .path = "src/main.zig" },
    });

    const test_step = b.step("test", "Run library tests");
    test_step.dependOn(&testStep(b, optimize, target).step);
}

fn testStep(
    b: *std.Build,
    optimize: std.builtin.OptimizeMode,
    target: std.zig.CrossTarget,
) *std.build.RunStep {
    const main_tests = b.addTest(.{
        .name = "sysjs-tests",
        .root_source_file = .{ .path = "src/main.zig" },
        .target = target,
        .optimize = optimize,
    });
    return b.addRunArtifact(main_tests);
}

/// Returns the path to the JS code file, used for building artifacts.
///
/// The returned path is heap-allocated in the builder's arena.
pub fn getJSPath() []u8 {
    const b = builder_instance orelse @panic("Builder instance not initialized!");

    return b.pathFromRoot("src/mach-sysjs.js");
}
