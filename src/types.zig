const std = @import("std");
const Ast = std.zig.Ast;
const analysis = @import("analysis.zig");

pub const Namespace = std.ArrayListUnmanaged(u8);

pub const Container = struct {
    name: ?[]const u8 = null,
    parent: ?*Container = null,
    contents: []Content,

    pub const Content = union(enum) {
        func: *Function,
        container: *Container,
        std_code: struct {
            type: analysis.DeclType = .other,
            data: []const u8,
        },
    };

    fn composeNamespace(container: *const Container, allocator: std.mem.Allocator) !Namespace {
        var namespace: Namespace = .{};

        var parent: ?*const Container = container;
        while (parent) |par| {
            if (par.name) |name| {
                try namespace.insert(allocator, 0, '_');
                try namespace.insertSlice(allocator, 0, name);
            }

            if (par.parent) |pd| {
                parent = pd;
            } else {
                break;
            }
        }

        return namespace;
    }

    pub fn emitZig(container: Container, allocator: std.mem.Allocator, writer: anytype, indent: u8) !void {
        _ = try writer.writeByte('\n');

        var local_indent = indent;
        if (container.name) |n| {
            _ = try writer.writeByteNTimes(' ', indent);
            // TODO: namespacing
            try std.fmt.format(writer, "pub const {s} = struct {{\n", .{n});
            local_indent += 4;
        }

        for (container.contents) |content|
            switch (content) {
                .func => {
                    var namespace = try container.composeNamespace(allocator);
                    defer namespace.deinit(allocator);

                    const fun = content.func;
                    try fun.emitExtern(writer, local_indent, namespace);
                    try fun.emitWrapper(writer, local_indent, namespace);
                },
                .container => try content.container.emitZig(allocator, writer, local_indent),
                .std_code => |stdc| {
                    _ = try writer.writeByteNTimes(' ', local_indent);
                    try writer.writeAll(stdc.data);

                    try writer.writeAll(switch (stdc.type) {
                        .field => ",\n",
                        .decl => ";\n",
                        else => "\n",
                    });
                },
            };

        if (container.name != null) {
            _ = try writer.writeByteNTimes(' ', indent);
            _ = try writer.write("};\n");
        }
    }

    pub fn emitJs(container: Container, allocator: std.mem.Allocator, writer: anytype, indent: u8) !void {
        for (container.contents) |content| {
            switch (content) {
                .func => {
                    var namespace = try container.composeNamespace(allocator);
                    defer namespace.deinit(allocator);

                    try content.func.emitBinding(writer, indent, namespace);
                },
                .container => try content.container.emitJs(allocator, writer, indent),
                else => {},
            }
        }
    }

    pub fn functionFromAst(allocator: std.mem.Allocator, tree: Ast, node: Ast.Node.Index) !?*Function {
        const field = tree.fullContainerField(node).?;

        if (field.ast.value_expr == 0) {
            const type_expr = tree.nodes.get(field.ast.type_expr);
            switch (type_expr.tag) {
                .fn_proto_simple, .fn_proto_multi => {
                    return try Function.fromAst(
                        allocator,
                        tree,
                        field.ast.type_expr,
                        field.ast.main_token,
                    );
                },
                else => {},
            }
        }
        return null;
    }

    pub fn containerFromAst(allocator: std.mem.Allocator, tree: Ast, node: Ast.Node.Index) !?*Container {
        const var_decl = tree.fullVarDecl(node).?;
        // var/const
        if (var_decl.visib_token) |visib_token| {
            const visib = tree.tokenSlice(visib_token);
            if (std.mem.eql(u8, visib, "pub")) {
                // pub var/const
                const init_node = tree.nodes.get(var_decl.ast.init_node);
                switch (init_node.tag) {
                    .container_decl_trailing, .container_decl_two_trailing => {
                        return try Container.fromAst(allocator, tree, var_decl.ast.init_node, node);
                    },
                    else => {},
                }
            }
        }

        return null;
    }

    pub fn fromAst(allocator: std.mem.Allocator, tree: Ast, node: Ast.Node.Index, name_idx: Ast.Node.Index) anyerror!*Container {
        const name = if (analysis.getDeclNameToken(tree, name_idx)) |token| tree.tokenSlice(token) else null;

        var buf: [2]Ast.Node.Index = undefined;
        const container_decl = tree.fullContainerDecl(&buf, node).?;

        var cont = try allocator.create(Container);
        var contents: std.ArrayListUnmanaged(Content) = .{};

        for (container_decl.ast.members) |decl_idx| {
            const std_type = analysis.getDeclType(tree, decl_idx);
            switch (std_type) {
                // If this decl is a struct field `foo: fn () void,` then consume it.
                .field => {
                    if (try Container.functionFromAst(allocator, tree, decl_idx)) |func| {
                        func.parent = cont;
                        try contents.append(allocator, .{ .func = func });
                        continue;
                    }
                },
                // If this decl is a `pub const foo = struct {};` then consume it
                .decl => {
                    if (try Container.containerFromAst(allocator, tree, decl_idx)) |container| {
                        var child = container;
                        child.parent = cont;
                        try contents.append(allocator, .{ .container = child });
                        continue;
                    }
                },
                .other => {},
            }

            try contents.append(allocator, .{ .std_code = .{
                .data = tree.getNodeSource(decl_idx),
                .type = std_type,
            } });
        }

        cont.* = .{
            .name = name,
            .contents = try contents.toOwnedSlice(allocator),
        };
        return cont;
    }
};

pub const Function = struct {
    name: []const u8,
    parent: ?*Container = null,
    return_ty: Type,
    params: []Param,

    // No. of parameters in zig's self hosted wasm backend is limited to maxInt(u32).
    // The actual number of limit is however runtime implementation defined.
    const max_param_char_count = std.math.log10_int(@as(u32, std.math.maxInt(u32))) + 1;

    pub const Param = struct {
        name: ?[]const u8,
        type: Type,
    };

    pub fn emitExtern(fun: Function, writer: anytype, indent: u8, namespace: Namespace) !void {
        _ = try writer.writeByteNTimes(' ', indent);
        try writer.writeAll("extern fn sysjs_");
        try writer.writeAll(namespace.items);
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

    pub fn emitWrapper(fun: Function, writer: anytype, indent: u8, namespace: Namespace) !void {
        _ = try writer.writeByteNTimes(' ', indent);
        try writer.print("pub inline fn {s}(", .{fun.name});

        for (fun.params, 0..) |param, i| {
            if (i != 0) try writer.writeAll(", ");
            if (param.name) |name| {
                try param.type.emitParam(writer, name);
            } else {
                var buf: [Function.max_param_char_count]u8 = .{};
                const name = try std.fmt.bufPrint(&buf, "v{d}", .{i});
                try param.type.emitParam(writer, name);
            }
        }

        try writer.writeAll(") ");
        try fun.return_ty.emitParam(writer, null);
        try writer.writeAll(" {\n");

        _ = try writer.writeByteNTimes(' ', indent + 4);
        try writer.writeAll("return ");

        const is_ret_obj = fun.return_ty.info == .composite_ref;
        if (is_ret_obj) {
            try writer.print("{s}{{.id = ", .{fun.return_ty.slice});
        }

        try writer.writeAll("sysjs_");
        try writer.writeAll(namespace.items);
        try writer.writeAll(fun.name);
        try writer.writeByte('(');

        for (fun.params, 0..) |param, i| {
            if (i != 0) try writer.writeAll(", ");
            if (param.name) |name| {
                try param.type.emitExternArg(writer, name);
            } else {
                var buf: [Function.max_param_char_count]u8 = .{};
                const name = try std.fmt.bufPrint(&buf, "v{d}", .{i});
                try param.type.emitExternArg(writer, name);
            }
        }

        try writer.writeByte(')');
        if (is_ret_obj) try writer.writeByte('}');
        try writer.writeAll(";\n");

        _ = try writer.writeByteNTimes(' ', indent);
        try writer.writeAll("}\n");
    }

    pub fn emitBinding(fun: Function, writer: anytype, indent: u8, namespace: Namespace) !void {
        _ = try writer.writeByteNTimes(' ', indent);
        try writer.print("export function sysjs_{s}{s}(", .{ namespace.items, fun.name });

        for (fun.params, 0..) |param, i| {
            if (i != 0) try writer.writeAll(", ");
            if (param.name) |name| {
                try param.type.emitBindingParam(writer, name);
            } else {
                var buf: [Function.max_param_char_count]u8 = .{};
                const name = try std.fmt.bufPrint(&buf, "v{d}", .{i});
                try param.type.emitBindingParam(writer, name);
            }
        }

        try writer.writeAll(") {\n");

        for (fun.params, 0..) |param, i| {
            _ = try writer.writeByteNTimes(' ', indent + 4);
            if (param.name) |name| {
                try param.type.emitBindingGet(writer, name, i);
            } else {
                var buf: [Function.max_param_char_count]u8 = .{};
                const name = try std.fmt.bufPrint(&buf, "v{d}", .{i});
                try param.type.emitBindingGet(writer, name, i);
            }
        }

        _ = try writer.writeByteNTimes(' ', indent + 4);

        const is_ret_obj = fun.return_ty.info == .composite_ref;

        const is_const = if (fun.parent) |parent|
            if (parent.name) |name|
                std.mem.eql(u8, name, fun.return_ty.slice)
            else
                false
        else
            false;

        const is_method = if (fun.params.len > 0) if (fun.parent) |parent|
            if (parent.name) |name|
                std.mem.eql(u8, name, fun.params[0].type.slice)
            else
                false
        else
            false else false;

        try writer.print("return {s}{s}", .{
            if (is_ret_obj) "wasmWrapObject(" else "",
            if (is_const) "new " else "",
        });

        var arg_index: u32 = 0;
        if (is_method) {
            try writer.writeAll("l0.");
            arg_index = 1;
        } else {
            // If its a constructor, remove the last '_'
            var end = namespace.items.len;
            end -= if (is_const) 1 else 0;

            // Replace all '_' with '.' for namespace
            std.mem.replaceScalar(u8, namespace.items, '_', '.');
            try writer.writeAll(namespace.items[0..end]);
        }

        try writer.print("{s}(", .{if (!is_const) fun.name else ""});

        for (arg_index..fun.params.len) |i| {
            if (i != arg_index) try writer.writeAll(", ");
            try writer.print("l{d}", .{i});
        }

        if (is_ret_obj) try writer.writeByte(')');
        try writer.writeAll(");\n}\n");
    }

    pub fn fromAst(allocator: std.mem.Allocator, tree: Ast, node_index: Ast.TokenIndex, name_token: Ast.TokenIndex) !*Function {
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

        var func_obj = try allocator.create(Function);
        func_obj.* = Function{
            .name = name,
            .return_ty = return_type,
            .params = params.items,
        };
        return func_obj;
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
        composite_ref: void,
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
            .composite_ref => try writer.writeAll("u32"),
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
            .composite_ref => try writer.print("{s}.id", .{arg_name}),
            else => try writer.writeAll(arg_name),
        }
    }

    pub fn emitBindingParam(ty: Type, writer: anytype, param_name: []const u8) !void {
        switch (ty.info) {
            .ptr => |ptr| switch (ptr.size) {
                .Slice => try std.fmt.format(writer, "{s}, {s}_len", .{ param_name, param_name }),
                else => {}, // TODO: one, many
            },
            else => try writer.writeAll(param_name),
        }
    }

    pub fn emitBindingGet(ty: Type, writer: anytype, param_name: []const u8, index: usize) !void {
        switch (ty.info) {
            .ptr => |ptr| switch (ptr.size) {
                .Slice => {
                    try writer.print("const l{d} = wasmGetSlice({s}, {s}_len);\n", .{ index, param_name, param_name });
                },
                else => {}, // TODO: one, many
            },
            .composite_ref => {
                try writer.print("const l{d} = wasmGetObject({s});\n", .{ index, param_name });
            },
            else => try writer.print("const l{d} = {s};\n", .{ index, param_name }),
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
        }

        return Type{
            .slice = token_slice,
            .info = .{ .composite_ref = {} },
        };
    }
};
