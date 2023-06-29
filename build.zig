const std = @import("std");
const sysjs = @import("src/main.zig");

/// Global instance of this module's builder to be used by functions invoked from other modules.
var builder_instance: ?*std.Build = null;

pub fn build(b: *std.Build) !void {
    builder_instance = b;

    const optimize = b.standardOptimizeOption(.{});
    const target = b.standardTargetOptions(.{});

    _ = b.addModule("mach-sysjs", .{
        .source_file = .{ .path = "src/main.zig" },
    });

    // Use sysjs to generate bindings
    try sysjs.generateFiles(
        b.allocator,
        "example/sysjs.zig",
        "example/sysjs_generated.zig",
        "example/sysjs_generated.js",
    );

    const wasm32_freestanding = std.zig.CrossTarget{ .cpu_arch = .wasm32, .os_tag = .freestanding };
    const lib = b.addSharedLibrary(.{
        .name = "example",
        .root_source_file = .{ .path = "example/main.zig" },
        .optimize = optimize,
        .target = wasm32_freestanding,
    });
    lib.rdynamic = true;

    const example_install = b.addInstallArtifact(lib);
    var example_compile_step = b.step("example", "Compile example");
    example_compile_step.dependOn(&example_install.step);

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
