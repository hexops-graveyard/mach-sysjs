const std = @import("std");
const Ast = std.zig.Ast;

pub const Namespace = struct {
    fields: []Field,
    decls: []Decl,

    pub const Field = union {
        func: Function,
        std_field: void,
    };

    pub const Decl = union {
        namespace: Namespace,
        std_decl: void,
    };
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

    pub fn emitWrapper(fun: Function, writer: anytype, allocator: std.mem.Allocator, indent: u8) !void {
        _ = try writer.writeByteNTimes(' ', indent);
        try writer.print("pub inline fn {s}(", .{fun.name});

        for (fun.params, 0..) |param, i| {
            if (i != 0) try writer.writeAll(", ");
            if (param.name) |name| {
                try param.type.emitParam(writer, name);
            } else {
                const name = try std.fmt.allocPrint(allocator, "v{d}", .{i});
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
                const name = try std.fmt.allocPrint(allocator, "v{d}", .{i});
                try param.type.emitExternArg(writer, name);
            }
        }

        try writer.writeAll(");\n");

        _ = try writer.writeByteNTimes(' ', indent);
        try writer.writeAll("}\n");
    }

    pub fn fromAst(allocator: std.mem.Allocator, tree: Ast, node_index: Ast.TokenIndex, name_token: Ast.TokenIndex) !Function {
        // Generate namespaced name (`extern fn sysjs_foo_bar_baz`)
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

    pub fn emitParam(ty: Type, writer: anytype, param_name: ?[]const u8) !void {
        try printParamName(writer, param_name, null);
        try writer.writeAll(ty.slice);
    }

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
