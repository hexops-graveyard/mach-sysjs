const std = @import("std");
const testing = std.testing;

pub fn generate(comptime Bindings: type, zig_writer: anytype, js_writer: anytype) !void {
    switch (@typeInfo(Bindings)) {
        .Struct => {},
        else => @compileError("Expected struct, found '" ++ @typeName(Bindings) ++ "'"),
    }
    try emitNamespacePreamble([_][]const u8{}, zig_writer, js_writer, Bindings);
    try std.fmt.format(zig_writer, "\n", .{});
    try emitNamespace([_][]const u8{}, zig_writer, js_writer, Bindings, "", "");
}

fn emitNamespacePreamble(
    comptime namespaces: anytype,
    zig_writer: anytype,
    js_writer: anytype,
    comptime Namespace: type,
) !void {
    const decls: []const std.builtin.Type.Declaration = switch (@typeInfo(Namespace)) {
        .Struct => |info| info.decls,
        else => @compileError(formatNamespaces(namespaces) ++ ": expected struct, found '" ++ @typeName(Namespace) ++ "'"),
    };

    inline for (decls) |decl| {
        if (!decl.is_pub) continue;
        const D = @field(Namespace, decl.name);
        switch (@typeInfo(D)) {
            .Struct => {
                try emitNamespacePreamble(
                    namespaces ++ [_][]const u8{decl.name},
                    zig_writer,
                    js_writer,
                    D,
                );
            },
            .Fn => |info| try emitFunctionPreamble(namespaces, zig_writer, js_writer, decl.name, info),
            inline else => @compileError(formatNamespaces(namespaces) ++ ": expected `pub const foo = struct` or `pub const foo = fn`, found '" ++ @typeName(D) ++ "'"),
        }
    }
}

fn emitFunctionPreamble(
    comptime namespaces: anytype,
    zig_writer: anytype,
    js_writer: anytype,
    comptime name: []const u8,
    comptime func: std.builtin.Type.Fn,
) !void {
    _ = js_writer;
    comptime var abs_name: []const u8 = "sysjs_";
    inline for (namespaces) |ns| {
        abs_name = abs_name ++ ns;
        abs_name = abs_name ++ "_";
    }
    abs_name = abs_name ++ name;

    try std.fmt.format(zig_writer, "extern fn {s}(", .{abs_name});
    const extern_params = zigExternParams(func.params);
    inline for (extern_params, 0..) |p, index| {
        try std.fmt.format(zig_writer, "v{}: {s}", .{ index, zigType(p.type.?) });
        if (index != extern_params.len - 1) try std.fmt.format(zig_writer, ", ", .{});
    }
    try std.fmt.format(zig_writer, ")", .{});
    const return_type = if (func.return_type) |t| zigType(t) else "void";
    try std.fmt.format(zig_writer, " {s};\n", .{return_type});
}

fn emitNamespace(
    comptime namespaces: anytype,
    zig_writer: anytype,
    js_writer: anytype,
    comptime Namespace: type,
    comptime zig_indention: []const u8,
    comptime js_indention: []const u8,
) !void {
    const decls: []const std.builtin.Type.Declaration = switch (@typeInfo(Namespace)) {
        .Struct => |info| info.decls,
        else => @compileError(formatNamespaces(namespaces) ++ ": expected struct, found '" ++ @typeName(Namespace) ++ "'"),
    };

    inline for (decls) |decl| {
        if (!decl.is_pub) continue;
        const D = @field(Namespace, decl.name);
        switch (@typeInfo(D)) {
            .Struct => {
                try std.fmt.format(zig_writer, zig_indention ++ "pub const {s} = struct {{\n", .{decl.name});
                try emitNamespace(
                    namespaces ++ [_][]const u8{decl.name},
                    zig_writer,
                    js_writer,
                    D,
                    zig_indention ++ "    ",
                    js_indention ++ "  ",
                );
                try std.fmt.format(zig_writer, zig_indention ++ "}};\n", .{});
            },
            .Fn => |info| try emitFunction(namespaces, zig_writer, js_writer, decl.name, info, zig_indention, js_indention),
            inline else => @compileError(formatNamespaces(namespaces) ++ ": expected `pub const foo = struct` or `pub const foo = fn`, found '" ++ @typeName(D) ++ "'"),
        }
    }
}

fn emitFunction(
    comptime namespaces: anytype,
    zig_writer: anytype,
    js_writer: anytype,
    comptime name: []const u8,
    comptime func: std.builtin.Type.Fn,
    comptime zig_indention: []const u8,
    comptime js_indention: []const u8,
) !void {
    try std.fmt.format(js_writer, js_indention ++ "//namespace={s} func={s} params={} return_type={?}\n", .{
        formatNamespaces(namespaces),
        name,
        func.params.len,
        func.return_type,
    });
    try std.fmt.format(zig_writer, zig_indention ++ "pub inline fn {s}(", .{name});
    inline for (func.params, 0..) |p, index| {
        try std.fmt.format(zig_writer, "v{}: {s}", .{ index, zigType(p.type.?) });
        if (index != func.params.len - 1) try std.fmt.format(zig_writer, ", ", .{});
    }
    try std.fmt.format(zig_writer, ")", .{});
    const return_type = if (func.return_type) |t| zigType(t) else "void";
    try std.fmt.format(zig_writer, " {s}", .{return_type});
    try std.fmt.format(zig_writer, " {{\n", .{});

    // Zig function body
    comptime var abs_name: []const u8 = "sysjs_";
    inline for (namespaces) |ns| {
        abs_name = abs_name ++ ns;
        abs_name = abs_name ++ "_";
    }
    abs_name = abs_name ++ name;
    try std.fmt.format(zig_writer, zig_indention ++ "    {s}(", .{abs_name});

    inline for (func.params, 0..) |p, index| {
        switch (p.type.?) {
            []const u8 => {
                try std.fmt.format(zig_writer, "v{}.ptr, v{}.len", .{ index, index });
            },
            inline else => {
                try std.fmt.format(zig_writer, "v{}", .{index});
            },
        }
        if (index != func.params.len - 1) try std.fmt.format(zig_writer, ", ", .{});
    }

    try std.fmt.format(zig_writer, ");\n", .{});

    try std.fmt.format(zig_writer, zig_indention ++ "}}\n", .{});
}

fn formatNamespaces(comptime namespaces: anytype) []const u8 {
    comptime var v: []const u8 = "";
    inline for (namespaces) |ns| {
        v = v ++ "." ++ ns;
    }
    return v;
}

fn zigType(comptime T: type) []const u8 {
    return switch (T) {
        []const u8 => "[]const u8",
        *const u8 => "[*]const u8",
        u32 => "u32",
        void => "void",
        inline else => @compileError("Unsupported parameter/return type: '" ++ @typeName(T) ++ "'"),
    };
}

fn zigExternParams(comptime params: []const std.builtin.Type.Fn.Param) []const std.builtin.Type.Fn.Param {
    comptime var out: []const std.builtin.Type.Fn.Param = &.{};
    inline for (params) |p| {
        switch (p.type.?) {
            []const u8 => out = out ++ &[_]std.builtin.Type.Fn.Param{
                .{ .is_generic = false, .is_noalias = false, .type = *const u8 },
                .{ .is_generic = false, .is_noalias = false, .type = u32 },
            },
            inline else => out = out ++ &[_]std.builtin.Type.Fn.Param{p},
        }
    }
    return out;
}

test {
    // Typically stored in a sysjs.zig file
    const sysjs_zig = struct {
        // Actual example:
        pub const not_namespaced = fn (string: []const u8) void;
        pub const console = struct {
            pub const log = fn (string: []const u8) void;

            /// Deep namespacing
            pub const debug = struct {
                pub const log = fn (string: []const u8) void;
            };
        };

        /// Test how non-pub behaves:
        const bar_not_pub = struct {
            pub const pub_inside = fn (string: []const u8) void;
        };

        /// Test some other permutations
        const not_pub_foo = fn () void;
    };

    const allocator = testing.allocator;

    var generated_zig = std.ArrayList(u8).init(allocator);
    defer generated_zig.deinit();

    var generated_js = std.ArrayList(u8).init(allocator);
    defer generated_js.deinit();

    try generate(sysjs_zig, generated_zig.writer(), generated_js.writer());

    try testing.expectEqualStrings(
        \\extern fn sysjs_not_namespaced(v0: [*]const u8, v1: u32) void;
        \\extern fn sysjs_console_log(v0: [*]const u8, v1: u32) void;
        \\extern fn sysjs_console_debug_log(v0: [*]const u8, v1: u32) void;
        \\
        \\pub inline fn not_namespaced(v0: []const u8) void {
        \\    sysjs_not_namespaced(v0.ptr, v0.len);
        \\}
        \\pub const console = struct {
        \\    pub inline fn log(v0: []const u8) void {
        \\        sysjs_console_log(v0.ptr, v0.len);
        \\    }
        \\    pub const debug = struct {
        \\        pub inline fn log(v0: []const u8) void {
        \\            sysjs_console_debug_log(v0.ptr, v0.len);
        \\        }
        \\    };
        \\};
        \\
    , generated_zig.items);

    try testing.expectEqualStrings(
        \\//namespace= func=not_namespaced params=1 return_type=void
        \\  //namespace=.console func=log params=1 return_type=void
        \\    //namespace=.console.debug func=log params=1 return_type=void
        \\
    , generated_js.items);
}
