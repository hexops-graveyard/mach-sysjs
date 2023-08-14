const std = @import("std");
const sysjs = @import("src/main.zig");

pub fn build(b: *std.Build) !void {
    const optimize = b.standardOptimizeOption(.{});

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

    const app = try App.init(b, .{
        .name = "example",
        .src = "example/main.zig",
        .optimize = optimize,
        .deps = &.{
            .{
                .name = "sysjs",
                .generated_js_file = "example/sysjs_generated.js",
                .generated_zig_file = "example/sysjs_generated.zig",
            },
        },
    });

    var example_compile_step = b.step("example", "Compile example");
    example_compile_step.dependOn(&app.install.step);
}

pub const index_html = @embedFile("www/index.html");

pub const Dependency = struct {
    name: []const u8,
    generated_js_file: []const u8,
    generated_zig_file: []const u8,
};

pub const App = struct {
    b: *std.Build,
    name: []const u8,
    compile: *std.build.Step.Compile,
    install: *std.build.Step.InstallArtifact,

    pub fn init(b: *std.Build, options: struct {
        name: []const u8,
        src: []const u8,
        optimize: std.builtin.OptimizeMode,
        deps: []const Dependency = &.{},
    }) !App {
        b.lib_dir = b.fmt("{s}/www", .{b.install_path});

        const wasm32_freestanding = std.zig.CrossTarget{ .cpu_arch = .wasm32, .os_tag = .freestanding };
        const lib = b.addSharedLibrary(.{
            .name = options.name,
            .root_source_file = .{ .path = options.src },
            .optimize = options.optimize,
            .target = wasm32_freestanding,
        });
        lib.rdynamic = true;

        b.installArtifact(lib);
        const install = b.addInstallArtifact(lib, .{});

        var inits = std.ArrayList(u8).init(b.allocator);
        var imports = std.ArrayList(u8).init(b.allocator);
        const import_writer = imports.writer();
        for (options.deps) |dep| {
            try import_writer.print(
                "import * as {s} from './{s}';\n",
                .{ dep.name, std.fs.path.basename(dep.generated_js_file) },
            );

            try inits.writer().print("{s}.init(wasm);\n", .{dep.name});

            const module = b.createModule(.{
                .source_file = .{ .path = b.pathFromRoot(dep.generated_zig_file) },
            });
            lib.addModule(dep.name, module);

            const install_js = b.addInstallFile(
                .{ .path = dep.generated_js_file },
                try std.fs.path.join(b.allocator, &.{ "www/", std.fs.path.basename(dep.generated_js_file) }),
            );
            install.step.dependOn(&install_js.step);
        }

        try import_writer.writeAll("let imports = {\nenv: {\n");
        for (options.deps) |dep| {
            try import_writer.print("...{s},\n", .{dep.name});
        }
        try import_writer.writeAll("}\n};\n");

        // TODO: make the loader configurable
        const install_loader = b.addInstallFile(
            .{ .path = "www/loader.js" },
            try std.fs.path.join(b.allocator, &.{ "www", "loader.js" }),
        );
        install.step.dependOn(&install_loader.step);

        // TODO: do the format at runtime
        const index_formatted = try std.fmt.allocPrint(b.allocator, index_html, .{
            .app_name = options.name,
            .wasm_path = options.name,
            .imports = imports.items,
            .inits = inits.items,
        });

        const write_index_html = b.addWriteFile(b.getInstallPath(.lib, "index.html"), index_formatted);
        install.step.dependOn(&write_index_html.step);

        return .{
            .b = b,
            .name = options.name,
            .compile = lib,
            .install = install,
        };
    }
};
