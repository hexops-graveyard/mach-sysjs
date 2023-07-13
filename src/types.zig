const std = @import("std");
const Ast = std.zig.Ast;
const analysis = @import("analysis.zig");

pub const Container = struct {
    fields: []Field,
    decls: []Decl,
    std_code: []const u8,

    pub const Field = union(enum) {
        func: Function,
        std_field: []const u8,

        pub fn emit(field: Field, writer: anytype, indent: u8) !void {
            switch (field) {
                .func => |fun| {
                    try fun.emitExtern(writer, indent);
                    try fun.emitWrapper(writer, indent);
                },
                .std_field => |stdf| {
                    _ = try writer.writeByteNTimes(' ', indent);
                    try writer.writeAll(stdf);
                    try writer.writeAll(",\n");
                },
            }
        }

        pub fn fromAst(allocator: std.mem.Allocator, tree: Ast, node: Ast.Node.Index) !Field {
            const field = tree.fullContainerField(node).?;

            if (field.ast.value_expr == 0) {
                const type_expr = tree.nodes.get(field.ast.type_expr);
                switch (type_expr.tag) {
                    .fn_proto_simple, .fn_proto_multi => {
                        const fun = try Function.fromAst(
                            allocator,
                            tree,
                            field.ast.type_expr,
                            field.ast.main_token,
                        );

                        return Field{
                            .func = fun,
                        };
                    },
                    else => {},
                }
            }
            return Field{
                .std_field = tree.getNodeSource(node),
            };
        }
    };

    pub const Decl = struct {
        name: ?[]const u8,
        content: union(enum) {
            container: Container,
            std_decl: []const u8,
        },

        pub fn emit(decl: Decl, writer: anytype, indent: u8) anyerror!void {
            switch (decl.content) {
                .container => |cont| try cont.emit(writer, decl.name, indent),
                .std_decl => |stdd| {
                    _ = try writer.writeByteNTimes(' ', indent);
                    try writer.writeAll(stdd);
                    try writer.writeAll(";\n");
                },
            }
        }

        pub fn fromAst(allocator: std.mem.Allocator, tree: Ast, node: Ast.Node.Index) !Decl {
            const name = if (analysis.getDeclNameToken(tree, node)) |token| tree.tokenSlice(token) else null;

            const var_decl = tree.fullVarDecl(node).?;
            // var/const
            if (var_decl.visib_token) |visib_token| {
                const visib = tree.tokenSlice(visib_token);
                if (std.mem.eql(u8, visib, "pub")) {
                    // pub var/const
                    const init_node = tree.nodes.get(var_decl.ast.init_node);
                    switch (init_node.tag) {
                        .container_decl_trailing, .container_decl_two_trailing => {
                            return Decl{
                                .name = name,
                                .content = .{
                                    .container = try Container.fromAst(
                                        allocator,
                                        tree,
                                        var_decl.ast.init_node,
                                    ),
                                },
                            };
                        },
                        else => {},
                    }
                }
            }

            return Decl{
                .name = null,
                .content = .{
                    .std_decl = tree.getNodeSource(node),
                },
            };
        }
    };

    pub fn emit(container: Container, writer: anytype, name: ?[]const u8, indent: u8) !void {
        _ = try writer.writeByte('\n');

        var local_indent = indent;
        if (name) |n| {
            _ = try writer.writeByteNTimes(' ', indent);
            // TODO: namespacing
            try std.fmt.format(writer, "pub const {s} = struct {{\n", .{n});
            local_indent += 4;
        }

        // The correct order is not preserved from original file because fields
        // containing function decl are converted to a decl (from zig's perspective).
        // So mainting the original order could put a normal field between two decls
        // which is not legal in zig
        for (container.fields) |field|
            if (std.meta.activeTag(field) == .func)
                try field.emit(writer, local_indent);

        for (container.fields) |field|
            if (std.meta.activeTag(field) == .std_field)
                try field.emit(writer, local_indent);

        for (container.decls) |decl|
            try decl.emit(writer, local_indent);

        try writer.writeAll(container.std_code);

        if (name != null) {
            _ = try writer.writeByteNTimes(' ', indent);
            _ = try writer.write("};\n");
        }
    }

    pub fn fromAst(allocator: std.mem.Allocator, tree: Ast, node: Ast.Node.Index) anyerror!Container {
        var buf: [2]Ast.Node.Index = undefined;
        const container_decl = tree.fullContainerDecl(&buf, node).?;

        var fields: std.ArrayListUnmanaged(Field) = .{};
        var decls: std.ArrayListUnmanaged(Decl) = .{};
        var std_code: std.ArrayListUnmanaged(u8) = .{};
        defer fields.deinit(allocator);
        defer decls.deinit(allocator);

        for (container_decl.ast.members) |decl_idx| {
            switch (analysis.getDeclType(tree, decl_idx)) {
                // If this decl is a struct field `foo: fn () void,` then consume it.
                .field => {
                    var field = try Field.fromAst(allocator, tree, decl_idx);
                    try fields.append(allocator, field);
                    continue;
                },
                // If this decl is a `pub const foo = struct {};` then consume it
                .decl => {
                    var decl = try Decl.fromAst(allocator, tree, decl_idx);
                    try decls.append(allocator, decl);
                    continue;
                },
                .other => {
                    try std_code.appendSlice(allocator, tree.getNodeSource(decl_idx));
                    continue;
                },
            }
        }

        return Container{
            .fields = try fields.toOwnedSlice(allocator),
            .decls = try decls.toOwnedSlice(allocator),
            .std_code = try std_code.toOwnedSlice(allocator),
        };
    }
};

pub const Function = struct {
    name: []const u8,
    return_ty: Type,
    params: []Param,

    pub const Param = struct {
        name: ?[]const u8,
        type: Type,
    };

    pub fn emitExtern(fun: Function, writer: anytype, indent: u8) !void {
        _ = try writer.writeByteNTimes(' ', indent);
        try writer.writeAll("extern fn sysjs_");
        try writer.writeAll(fun.name); // TODO: namespaces
        try writer.writeByte('(');

        for (fun.params, 0..) |param, i| {
            if (i != 0) try writer.writeAll(", ");
            try param.type.emitExternParam(writer, param.name);
        }

        try writer.writeAll(") ");
        try fun.return_ty.emitExternParam(writer, null);
        try writer.writeAll(";\n");
    }

    pub fn emitWrapper(fun: Function, writer: anytype, indent: u8) !void {
        _ = try writer.writeByteNTimes(' ', indent);
        try writer.print("pub inline fn {s}(", .{fun.name});

        // No. of parameters in zig's self hosted wasm backend is limited to maxInt(u32).
        // The actual number of limit is however runtime implementation defined.
        const max_param_count = comptime std.math.log10_int(@as(u32, std.math.maxInt(u32))) + 1;

        for (fun.params, 0..) |param, i| {
            if (i != 0) try writer.writeAll(", ");
            if (param.name) |name| {
                try param.type.emitParam(writer, name);
            } else {
                var buf: [max_param_count]u8 = .{};
                const name = try std.fmt.bufPrint(&buf, "v{d}", .{i});
                try param.type.emitParam(writer, name);
            }
        }

        try writer.writeAll(") ");
        try fun.return_ty.emitParam(writer, null);
        try writer.writeAll(" {\n");

        _ = try writer.writeByteNTimes(' ', indent + 4);
        try writer.writeAll("return sysjs_");
        try writer.writeAll(fun.name); // TODO: namespaces
        try writer.writeByte('(');

        for (fun.params, 0..) |param, i| {
            if (i != 0) try writer.writeAll(", ");
            if (param.name) |name| {
                try param.type.emitExternArg(writer, name);
            } else {
                var buf: [max_param_count]u8 = .{};
                const name = try std.fmt.bufPrint(&buf, "v{d}", .{i});
                try param.type.emitExternArg(writer, name);
            }
        }

        try writer.writeAll(");\n");

        _ = try writer.writeByteNTimes(' ', indent);
        try writer.writeAll("}\n");
    }

    pub fn fromAst(allocator: std.mem.Allocator, tree: Ast, node_index: Ast.TokenIndex, name_token: Ast.TokenIndex) !Function {
        var param_buf: [1]Ast.Node.Index = undefined;
        const fn_proto = tree.fullFnProto(&param_buf, node_index).?;

        const name = tree.tokenSlice(name_token);
        const return_type = try Type.fromAst(allocator, tree, fn_proto.ast.return_type);

        var params: std.ArrayListUnmanaged(Param) = .{};

        var params_iter = fn_proto.iterate(&tree);
        var i: usize = 0;
        while (params_iter.next()) |param| : (i += 1) {
            try params.append(allocator, .{
                .name = if (param.name_token) |nt| tree.tokenSlice(nt) else null,
                .type = try Type.fromAst(allocator, tree, param.type_expr),
            });
        }

        return Function{
            .name = name,
            .return_ty = return_type,
            .params = params.items,
        };
    }
};

pub const Type = struct {
    slice: []const u8,
    info: union(enum) {
        int: Int,
        ptr: Ptr,
    },

    pub const Int = struct {
        bits: u32,
        signedness: Signedness,

        pub const Signedness = enum { signed, unsigned };
    };

    pub const Ptr = struct {
        size: Size,
        is_const: bool,
        base_ty: *Type,

        pub const Size = enum {
            one,
            many,
            slice,
        };
    };

    fn printParamName(writer: anytype, param_name: ?[]const u8, extension: ?[]const u8) !void {
        if (param_name) |param| {
            try writer.writeAll(param);
            if (extension) |ext| try writer.writeAll(ext);
            try writer.writeAll(": ");
        }
    }
    // Emits parameters for functions in zig's format.
    pub fn emitParam(ty: Type, writer: anytype, param_name: ?[]const u8) !void {
        try printParamName(writer, param_name, null);
        try writer.writeAll(ty.slice);
    }

    // Emits parameters for functions, but in their 'extern' form.
    // e.g. expanding `[]const u8` to a pointer and length or such.
    pub fn emitExternParam(ty: Type, writer: anytype, param_name: ?[]const u8) !void {
        try printParamName(writer, param_name, null);

        switch (ty.info) {
            .int => |int| try std.fmt.format(writer, "{c}{d}", .{
                @tagName(int.signedness)[0],
                int.bits,
            }),
            .ptr => |ptr| {
                switch (ptr.size) {
                    .slice => {
                        try std.fmt.format(writer, "[*]{s}", .{if (ptr.is_const) "const " else ""});
                        try ptr.base_ty.*.emitExternParam(writer, null);
                        try writer.writeAll(", ");
                        try printParamName(writer, param_name, "_len");
                        try writer.writeAll("u32");
                    },
                    else => {}, // TODO: one, many
                }
            },
        }
    }

    // Emits arguments for function calls, but in their 'extern' form.
    // e.g. expanding `[]const u8` to a pointer and length or such.
    pub fn emitExternArg(ty: Type, writer: anytype, arg_name: []const u8) !void {
        switch (ty.info) {
            .ptr => |ptr| switch (ptr.size) {
                .slice => try std.fmt.format(writer, "{s}.ptr, {s}.len", .{ arg_name, arg_name }),
                else => {}, // TODO: one, many
            },
            else => try writer.writeAll(arg_name),
        }
    }

    pub fn fromAst(allocator: std.mem.Allocator, tree: Ast, index: Ast.TokenIndex) !Type {
        const token_slice = tree.getNodeSource(index);

        const tags = tree.tokens.items(.tag);
        const first_token = tree.firstToken(index);
        const last_token = tree.lastToken(index);

        var i = first_token;
        var size: ?Ptr.Size = null;
        var state: enum { none, maybe_sequence } = .none;
        while (i < last_token) : (i += 1) {
            switch (tags[i]) {
                .l_bracket => if (state == .none) {
                    state = .maybe_sequence;
                },
                .r_bracket => if (state == .maybe_sequence) {
                    // TODO: handle arrays and multi-pointer
                    size = .slice;
                    break;
                },
                else => {},
            }
        }

        const is_const = tags[i + 1] == .keyword_const;

        // TODO: also parse the actual type
        if (size) |s| {
            const base_ty = try allocator.create(Type);
            base_ty.* = Type{
                .slice = token_slice,
                .info = .{ .int = .{
                    .bits = 8,
                    .signedness = .unsigned,
                } },
            };
            return Type{
                .slice = token_slice,
                .info = .{ .ptr = .{
                    .size = s,
                    .is_const = is_const,
                    .base_ty = base_ty,
                } },
            };
        } else {
            return Type{
                .slice = token_slice,
                .info = .{ .int = .{
                    .bits = 32,
                    .signedness = .unsigned,
                } },
            };
        }
    }
};
