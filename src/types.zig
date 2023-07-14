const std = @import("std");
const Ast = std.zig.Ast;
const analysis = @import("analysis.zig");

pub const Container = struct {
    name: ?[]const u8 = null,
    parent: ?*Decl = null,
    fields: []*Field,
    decls: []*Decl,
    std_code: []const u8,

    pub const Field = struct {
        parent: ?*Container = null,
        content: union(enum) {
            func: Function,
            std_field: []const u8,
        },

        pub fn emit(field: Field, allocator: std.mem.Allocator, writer: anytype, indent: u8) !void {
            switch (field.content) {
                .func => |fun| {
                    var namespace: std.ArrayListUnmanaged(u8) = .{};
                    defer namespace.deinit(allocator);

                    var parent: ?*Container = field.parent;
                    while (parent) |par| {
                        if (par.name) |name| {
                            try namespace.insert(allocator, 0, '_');
                            try namespace.insertSlice(allocator, 0, name);
                        }

                        if (par.parent) |pd| {
                            parent = pd.parent;
                        } else {
                            break;
                        }
                    }

                    try fun.emitExtern(writer, indent, namespace.items);
                    try fun.emitWrapper(writer, indent, namespace.items);
                },
                .std_field => |stdf| {
                    _ = try writer.writeByteNTimes(' ', indent);
                    try writer.writeAll(stdf);
                    try writer.writeAll(",\n");
                },
            }
        }

        pub fn fromAst(allocator: std.mem.Allocator, tree: Ast, node: Ast.Node.Index) !*Field {
            const field = tree.fullContainerField(node).?;
            var field_obj = try allocator.create(Field);

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

                        field_obj.* = .{
                            .content = .{ .func = fun },
                        };
                        return field_obj;
                    },
                    else => {},
                }
            }
            field_obj.* = .{
                .content = .{ .std_field = tree.getNodeSource(node) },
            };
            return field_obj;
        }
    };

    pub const Decl = struct {
        parent: ?*Container = null,
        content: union(enum) {
            container: *Container,
            std_decl: []const u8,
        },

        pub fn emit(decl: Decl, allocator: std.mem.Allocator, writer: anytype, indent: u8) anyerror!void {
            switch (decl.content) {
                .container => |cont| try cont.emit(allocator, writer, indent),
                .std_decl => |stdd| {
                    _ = try writer.writeByteNTimes(' ', indent);
                    try writer.writeAll(stdd);
                    try writer.writeAll(";\n");
                },
            }
        }

        pub fn fromAst(allocator: std.mem.Allocator, tree: Ast, node: Ast.Node.Index) !*Decl {
            var decl = try allocator.create(Decl);

            const var_decl = tree.fullVarDecl(node).?;
            // var/const
            if (var_decl.visib_token) |visib_token| {
                const visib = tree.tokenSlice(visib_token);
                if (std.mem.eql(u8, visib, "pub")) {
                    // pub var/const
                    const init_node = tree.nodes.get(var_decl.ast.init_node);
                    switch (init_node.tag) {
                        .container_decl_trailing, .container_decl_two_trailing => {
                            var cont = try Container.fromAst(allocator, tree, var_decl.ast.init_node, node);
                            cont.parent = decl;
                            decl.* = .{
                                .content = .{ .container = cont },
                            };

                            return decl;
                        },
                        else => {},
                    }
                }
            }

            decl.* = .{
                .content = .{
                    .std_decl = tree.getNodeSource(node),
                },
            };
            return decl;
        }
    };

    pub fn emit(container: Container, allocator: std.mem.Allocator, writer: anytype, indent: u8) !void {
        _ = try writer.writeByte('\n');

        var local_indent = indent;
        if (container.name) |n| {
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
            if (std.meta.activeTag(field.*.content) == .func)
                try field.emit(allocator, writer, local_indent);

        for (container.fields) |field|
            if (std.meta.activeTag(field.*.content) == .std_field)
                try field.emit(allocator, writer, local_indent);

        for (container.decls) |decl|
            try decl.emit(allocator, writer, local_indent);

        try writer.writeAll(container.std_code);

        if (container.name != null) {
            _ = try writer.writeByteNTimes(' ', indent);
            _ = try writer.write("};\n");
        }
    }

    pub fn fromAst(allocator: std.mem.Allocator, tree: Ast, node: Ast.Node.Index, name_idx: Ast.Node.Index) anyerror!*Container {
        const name = if (analysis.getDeclNameToken(tree, name_idx)) |token| tree.tokenSlice(token) else null;

        var buf: [2]Ast.Node.Index = undefined;
        const container_decl = tree.fullContainerDecl(&buf, node).?;

        var fields: std.ArrayListUnmanaged(*Field) = .{};
        var decls: std.ArrayListUnmanaged(*Decl) = .{};
        var std_code: std.ArrayListUnmanaged(u8) = .{};
        defer fields.deinit(allocator);
        defer decls.deinit(allocator);

        var cont = try allocator.create(Container);

        for (container_decl.ast.members) |decl_idx| {
            switch (analysis.getDeclType(tree, decl_idx)) {
                // If this decl is a struct field `foo: fn () void,` then consume it.
                .field => {
                    var field = try Field.fromAst(allocator, tree, decl_idx);
                    try fields.append(allocator, field);
                    field.parent = cont;
                    continue;
                },
                // If this decl is a `pub const foo = struct {};` then consume it
                .decl => {
                    var decl = try Decl.fromAst(allocator, tree, decl_idx);
                    try decls.append(allocator, decl);
                    decl.parent = cont;
                    continue;
                },
                .other => {
                    try std_code.appendSlice(allocator, tree.getNodeSource(decl_idx));
                    continue;
                },
            }
        }

        cont.* = .{
            .name = name,
            .fields = try fields.toOwnedSlice(allocator),
            .decls = try decls.toOwnedSlice(allocator),
            .std_code = try std_code.toOwnedSlice(allocator),
        };
        return cont;
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

    pub fn emitExtern(fun: Function, writer: anytype, indent: u8, namespace: []const u8) !void {
        _ = try writer.writeByteNTimes(' ', indent);
        try writer.writeAll("extern fn sysjs_");
        try writer.writeAll(namespace);
        try writer.writeAll(fun.name);
        try writer.writeByte('(');

        for (fun.params, 0..) |param, i| {
            if (i != 0) try writer.writeAll(", ");
            try param.type.emitExternParam(writer, param.name);
        }

        try writer.writeAll(") ");
        try fun.return_ty.emitExternParam(writer, null);
        try writer.writeAll(";\n");
    }

    pub fn emitWrapper(fun: Function, writer: anytype, indent: u8, namespace: []const u8) !void {
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
        try writer.writeAll(namespace);
        try writer.writeAll(fun.name);
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
    info: TypeInfo,

    pub const TypeInfo = union(enum) {
        int: Int,
        float: Float,
        ptr: Ptr,
        void: void,
        bool: void,
        anyopaque: void,
    };

    pub const Int = struct {
        bits: u16,
        signedness: Signedness,

        pub const Signedness = enum { signed, unsigned };
    };

    pub const Float = struct {
        bits: u16,
    };

    pub const Ptr = struct {
        size: std.builtin.Type.Pointer.Size,
        is_const: bool,
        base_ty: *Type,
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
            .ptr => |ptr| {
                switch (ptr.size) {
                    .Slice => {
                        try std.fmt.format(writer, "[*]{s}", .{if (ptr.is_const) "const " else ""});
                        try ptr.base_ty.*.emitExternParam(writer, null);
                        try writer.writeAll(", ");
                        try printParamName(writer, param_name, "_len");
                        try writer.writeAll("u32");
                    },
                    else => {}, // TODO: one, many
                }
            },
            else => try writer.writeAll(ty.slice),
        }
    }

    // Emits arguments for function calls, but in their 'extern' form.
    // e.g. expanding `[]const u8` to a pointer and length or such.
    pub fn emitExternArg(ty: Type, writer: anytype, arg_name: []const u8) !void {
        switch (ty.info) {
            .ptr => |ptr| switch (ptr.size) {
                .Slice => try std.fmt.format(writer, "{s}.ptr, {s}.len", .{ arg_name, arg_name }),
                else => {}, // TODO: one, many
            },
            else => try writer.writeAll(arg_name),
        }
    }

    pub fn fromAst(allocator: std.mem.Allocator, tree: Ast, index: Ast.Node.Index) !Type {
        const token_slice = tree.getNodeSource(index);
        if (tree.fullPtrType(index)) |ptr| {
            const child_ty = try Type.fromAst(allocator, tree, ptr.ast.child_type);
            var base_ty = try allocator.create(Type);
            base_ty.* = child_ty;

            return Type{
                .slice = token_slice,
                .info = .{ .ptr = Ptr{
                    .size = ptr.size,
                    .is_const = ptr.const_token != null,
                    .base_ty = base_ty,
                } },
            };
        }

        if (std.zig.primitives.isPrimitive(token_slice)) {
            const signedness: ?Int.Signedness = switch (token_slice[0]) {
                'u' => .unsigned,
                'i' => .signed,
                else => null,
            };

            const float = token_slice[0] == 'f';
            const c_types = token_slice[0] == 'c' and token_slice[1] == '_';

            const size: ?u16 = std.fmt.parseInt(u16, token_slice[1..], 10) catch |err| blk: {
                switch (err) {
                    error.InvalidCharacter => break :blk null,
                    else => |e| return e,
                }
            };

            if (signedness) |sig| {
                // iXX or uXX
                if (size) |sz| {
                    return Type{
                        .slice = token_slice,
                        .info = .{
                            .int = Int{
                                .signedness = sig,
                                .bits = sz,
                            },
                        },
                    };
                } else {
                    // TODO: usize or isize
                }
            }

            if (float and size != null) {
                // fXX
                return Type{
                    .slice = token_slice,
                    .info = .{
                        .float = Float{
                            .bits = size.?,
                        },
                    },
                };
            }

            if (c_types) {
                // TODO c types
            }

            inline for (std.meta.fields(Type.TypeInfo)) |field| {
                if (field.type == void) {
                    if (std.mem.eql(u8, token_slice, field.name)) {
                        return Type{
                            .slice = token_slice,
                            .info = @unionInit(TypeInfo, field.name, {}),
                        };
                    }
                }
            }

            @panic("TODO: error on impossible types");
        } else {
            @panic("TODO: non primitive types");
        }
    }
};
