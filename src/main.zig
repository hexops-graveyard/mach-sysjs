const std = @import("std");
const testing = std.testing;
const Ast = std.zig.Ast;

const analysis = @import("analysis.zig");
const Type = @import("types.zig").Type;

pub fn generateFiles(
    allocator: std.mem.Allocator,
    bindings_path: []const u8,
    generated_sysjs_zig: []const u8,
    generated_sysjs_js: []const u8,
) !void {
    var generated_zig = try std.fs.cwd().createFile(generated_sysjs_zig, .{});
    var generated_js = try std.fs.cwd().createFile(generated_sysjs_js, .{});
    var bindings_code = try std.fs.cwd().readFileAllocOptions(allocator, bindings_path, 1024 * 1024 * 128, null, @alignOf(u8), 0);
    defer allocator.free(bindings_code);

    const zig_writer = generated_zig.writer();
    const js_writer = generated_js.writer();
    var gen = Generator(@TypeOf(zig_writer), @TypeOf(js_writer)){
        .zig = zig_writer,
        .js = js_writer,
        .allocator = allocator,
        .bindings_code = bindings_code,
    };
    try gen.root();

    generated_zig.close();
    generated_js.close();
}

fn Generator(comptime ZigWriter: type, comptime JSWriter: type) type {
    return struct {
        zig: ZigWriter,
        js: JSWriter,
        allocator: std.mem.Allocator,
        bindings_code: [:0]const u8,

        // internal fields
        tree: Ast = undefined,
        buf: [4096]u8 = undefined,
        namespace: std.ArrayListUnmanaged([]const u8) = .{},

        pub fn root(gen: *@This()) !void {
            gen.tree = try Ast.parse(gen.allocator, gen.bindings_code, .zig);
            defer gen.tree.deinit(gen.allocator);

            try std.fmt.format(gen.zig, "// Generated by `zig build`; DO NOT EDIT.\n", .{});
            try std.fmt.format(gen.js, "// Generated by `zig build`; DO NOT EDIT.\n", .{});

            try gen.container(0, 0);
        }

        pub fn container(gen: *@This(), node_idx: Ast.Node.Index, indent: u8) !void {
            var buf: [2]Ast.Node.Index = undefined;
            const container_decl = gen.tree.fullContainerDecl(&buf, node_idx) orelse return;

            for (container_decl.ast.members) |decl_idx| {
                const member_name_token = analysis.getDeclNameToken(gen.tree, decl_idx) orelse continue;
                const member_name = gen.tree.tokenSlice(member_name_token);

                // If this decl is a `pub const foo = struct {};` then consume it
                if (gen.tree.fullVarDecl(decl_idx)) |var_decl| {
                    // var/const
                    if (var_decl.visib_token) |visib_token| {
                        const visib = gen.tree.tokenSlice(visib_token);
                        if (std.mem.eql(u8, visib, "pub")) {
                            // pub var/const
                            const init_node = gen.tree.nodes.get(var_decl.ast.init_node);
                            switch (init_node.tag) {
                                .container_decl_trailing, .container_decl_two_trailing => {
                                    _ = try gen.zig.writeByte('\n');
                                    _ = try gen.zig.writeByteNTimes(' ', indent);
                                    try std.fmt.format(gen.zig, "pub const {s} = struct {{\n", .{member_name});
                                    try gen.namespace.append(gen.allocator, member_name);
                                    try gen.container(var_decl.ast.init_node, indent + 4);
                                    _ = try gen.zig.write("};\n");
                                    continue;
                                },
                                else => {},
                            }
                        }
                    }
                }

                // If this decl is a struct field `foo: fn () void,` then consume it.
                if (gen.tree.fullContainerField(decl_idx)) |field| {
                    if (field.ast.value_expr == 0) {
                        const type_expr = gen.tree.nodes.get(field.ast.type_expr);
                        switch (type_expr.tag) {
                            .fn_proto_simple, .fn_proto_multi => {
                                try gen.fnProto(field.ast.type_expr, field.ast.main_token, indent);
                                try gen.fnProtoWrapper(field.ast.type_expr, field.ast.main_token, indent);
                                continue;
                            },
                            else => {
                                std.debug.print("{s}\n", .{@tagName(type_expr.tag)});
                            },
                        }
                    }
                }

                // If it wasn't consumed, just emit it directly.
                _ = try gen.zig.writeByteNTimes(' ', indent);
                try std.fmt.format(gen.zig, "{s}", .{gen.tree.getNodeSource(decl_idx)});
                switch (gen.tree.nodes.get(decl_idx).tag) {
                    .global_var_decl,
                    .local_var_decl,
                    .simple_var_decl,
                    .aligned_var_decl,
                    => {
                        try std.fmt.format(gen.zig, ";\n", .{});
                    },
                    .container_field_init => {
                        try std.fmt.format(gen.zig, ",\n", .{});
                    },
                    else => |tag| {
                        std.debug.print("{s}\n", .{@tagName(tag)});
                    },
                }
            }
        }

        fn fnProto(gen: *@This(), node_index: Ast.Node.Index, name_token: Ast.TokenIndex, indent: u8) !void {
            // Generate namespaced name (`extern fn sysjs_foo_bar_baz`)
            var param_buf: [1]Ast.Node.Index = undefined;
            const fn_proto = gen.tree.fullFnProto(&param_buf, node_index).?;

            _ = try gen.zig.writeByteNTimes(' ', indent);
            try std.fmt.format(gen.zig, "extern fn sysjs_", .{});
            for (gen.namespace.items) |ns| try std.fmt.format(gen.zig, "{s}_", .{ns});
            try std.fmt.format(gen.zig, "{s}(", .{gen.tree.tokenSlice(name_token)});

            var params_iter = fn_proto.iterate(&gen.tree);
            var i: usize = 0;
            while (params_iter.next()) |param| : (i += 1) {
                if (i != 0) try std.fmt.format(gen.zig, ", ", .{});
                if (param.name_token) |param_name_token| {
                    const param_name = gen.tree.tokenSlice(param_name_token);
                    try gen.externParam(param_name, param.type_expr);
                } else {
                    try gen.externParam(null, param.type_expr);
                }
            }

            try std.fmt.format(gen.zig, ") {s};\n", .{gen.tree.getNodeSource(fn_proto.ast.return_type)});
        }

        fn fnProtoWrapper(gen: *@This(), node_index: Ast.Node.Index, name_token: Ast.TokenIndex, indent: u8) !void {
            // Generate namespaced name (`extern fn sysjs_foo_bar_baz`)
            var param_buf: [1]Ast.Node.Index = undefined;
            const fn_proto = gen.tree.fullFnProto(&param_buf, node_index).?;

            _ = try gen.zig.writeByteNTimes(' ', indent);
            try std.fmt.format(gen.zig, "pub inline fn {s}(", .{gen.tree.tokenSlice(name_token)});

            var params_iter = fn_proto.iterate(&gen.tree);
            var i: usize = 0;
            while (params_iter.next()) |param| : (i += 1) {
                if (i != 0) try std.fmt.format(gen.zig, ", ", .{});
                if (param.name_token) |param_name_token| {
                    try std.fmt.format(gen.zig, "{s}: {s}", .{ gen.tree.tokenSlice(param_name_token), gen.tree.getNodeSource(param.type_expr) });
                } else {
                    try std.fmt.format(gen.zig, "v{d}: {s}", .{ i, gen.tree.getNodeSource(param.type_expr) });
                }
            }

            // return sysjs_x_y_z(
            try std.fmt.format(gen.zig, ") {s} {{\n", .{gen.tree.getNodeSource(fn_proto.ast.return_type)});
            _ = try gen.zig.writeByteNTimes(' ', indent + 4);
            _ = try gen.zig.write("return sysjs_");
            for (gen.namespace.items) |ns| try std.fmt.format(gen.zig, "{s}_", .{ns});
            try std.fmt.format(gen.zig, "{s}(", .{gen.tree.tokenSlice(name_token)});

            // a, b, c);
            i = 0;
            var pass_params_iter = fn_proto.iterate(&gen.tree);
            while (pass_params_iter.next()) |param| : (i += 1) {
                if (i != 0) try std.fmt.format(gen.zig, ", ", .{});
                if (param.name_token) |param_name_token| {
                    const param_name = gen.tree.tokenSlice(param_name_token);
                    try gen.externArg(param_name, param.type_expr);
                } else {
                    const param_name = try std.fmt.allocPrint(gen.allocator, "v{d}", .{i});
                    defer gen.allocator.free(param_name);

                    try gen.externArg(param_name, param.type_expr);
                }
            }
            _ = try gen.zig.write(");\n");

            // }
            _ = try gen.zig.writeByteNTimes(' ', indent);
            _ = try gen.zig.write("}\n\n");
        }

        // Emits parameters for functions, but in their 'extern' form.
        // e.g. expanding `[]const u8` to a pointer and length or such.
        fn externParam(gen: *@This(), param_name: ?[]const u8, type_index: Ast.Node.Index) !void {
            const ty = try Type.fromAst(gen.allocator, gen.tree, type_index);
            try ty.emitExternParam(gen.zig, param_name);
        }

        // Emits arguments for function calls, but in their 'extern' form.
        // e.g. expanding `[]const u8` to a pointer and length or such.
        fn externArg(gen: *@This(), param_name: []const u8, type_index: Ast.Node.Index) !void {
            const ty = try Type.fromAst(gen.allocator, gen.tree, type_index);
            try ty.emitExternArg(gen.zig, param_name);
        }
    };
}
