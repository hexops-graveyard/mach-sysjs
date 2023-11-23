const std = @import("std");
const Ast = std.zig.Ast;
const analysis = @import("analysis.zig");

pub const Container = struct {
    name: ?[]const u8 = null,
    parent: ?*Container = null,
    children: std.ArrayListUnmanaged(*Container) = .{},
    contents: std.ArrayListUnmanaged(Content) = .{},
    types: std.ArrayListUnmanaged(Type) = .{},
    fields: std.ArrayListUnmanaged(Type.Composite.Field) = .{},
    val_type: enum { namespace, struct_val } = .struct_val,

    const Namespace = std.ArrayListUnmanaged(u8);

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

        for (container.fields.items) |field| {
            try writer.writeByteNTimes(' ', local_indent);
            try writer.print("{s}: {s},\n", .{ field.name, field.type.slice });
        }

        try writer.writeByte('\n');

        for (container.contents.items) |content|
            switch (content) {
                .func => {
                    var namespace = try container.composeNamespace(allocator);
                    defer namespace.deinit(allocator);

                    const fun = content.func;
                    try fun.emitExtern(writer, local_indent, namespace);
                    try fun.emitWrapper(writer, local_indent, namespace);
                },
                .container => {
                    try content.container.emitZig(allocator, writer, local_indent);
                    try content.container.emitExtern(allocator, writer, local_indent);
                },
                .std_code => |stdc| {
                    _ = try writer.writeByteNTimes(' ', local_indent);
                    try writer.writeAll(stdc.data);

                    try writer.writeAll(switch (stdc.type) {
                        .field => unreachable,
                        .decl => ";\n",
                        else => "\n",
                    });
                },
            };

        if (container.name) |n| {
            if (container.val_type == .struct_val) {
                try writer.writeByteNTimes(' ', local_indent);
                try writer.print("pub fn toExtern(s: {s}) C{s} {{\n", .{ n, n });
                try writer.writeByteNTimes(' ', local_indent + 4);
                try writer.print("return C{s}{{\n", .{n});

                for (container.fields.items) |field| {
                    try writer.writeByteNTimes(' ', local_indent + 8);
                    try field.type.emitExternField(writer, field.name, "s");
                }

                try writer.writeByteNTimes(' ', local_indent + 4);
                try writer.writeAll("};\n");

                try writer.writeByteNTimes(' ', local_indent);
                try writer.writeAll("}\n");
            }

            _ = try writer.writeByteNTimes(' ', indent);
            _ = try writer.write("};\n");
        }
    }

    pub fn emitExtern(container: Container, allocator: std.mem.Allocator, writer: anytype, indent: u8) anyerror!void {
        if (container.val_type == .namespace)
            return;

        _ = try writer.writeByte('\n');

        var local_indent = indent;
        if (container.name) |n| {
            _ = try writer.writeByteNTimes(' ', indent);
            try std.fmt.format(writer, "pub const C{s} = extern struct {{\n", .{n});
            local_indent += 4;
        }

        for (container.fields.items) |field| {
            try writer.writeByteNTimes(' ', local_indent);
            try field.type.emitExternParam(writer, field.name);
            try writer.writeAll(",\n");
        }

        try writer.writeByte('\n');

        for (container.contents.items) |content|
            switch (content) {
                .func => {},
                .container => {
                    try content.container.emitZig(allocator, writer, local_indent);
                    try content.container.emitExtern(allocator, writer, local_indent);
                },
                .std_code => |stdc| {
                    _ = try writer.writeByteNTimes(' ', local_indent);
                    try writer.writeAll(stdc.data);

                    try writer.writeAll(switch (stdc.type) {
                        .field => unreachable,
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
        for (container.contents.items) |content| {
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
};

pub const Function = struct {
    name: []const u8,
    parent: ?*Container = null,
    return_ty: Type,
    params: []Param,
    val_ty: enum { constructor, method, none } = .none,

    // No. of parameters in zig's self hosted wasm backend is limited to maxInt(u32).
    // The actual number of limit is however runtime implementation defined.
    const max_param_char_count = std.math.log10_int(@as(u32, std.math.maxInt(u32))) + 1;

    pub const Param = struct {
        name: ?[]const u8,
        type: Type,
    };

    pub fn emitExtern(fun: Function, writer: anytype, indent: u8, namespace: Container.Namespace) !void {
        _ = try writer.writeByteNTimes(' ', indent);
        try writer.writeAll("extern fn sysjs_");
        try writer.writeAll(namespace.items);
        try writer.writeAll(fun.name);
        try writer.writeByte('(');

        for (fun.params, 0..) |param, i| {
            if (i != 0) try writer.writeAll(", ");
            try param.type.emitExternParam(writer, param.name);
        }

        switch (fun.return_ty.info) {
            .container_value => {
                try writer.writeAll(", ");
                try fun.return_ty.emitExternParam(writer, "return_val");
                try writer.writeAll(") void;\n");
            },
            else => {
                try writer.writeAll(") ");
                try fun.return_ty.emitExternParam(writer, null);
                try writer.writeAll(";\n");
            },
        }
    }

    pub fn emitWrapper(fun: Function, writer: anytype, indent: u8, namespace: Container.Namespace) !void {
        _ = try writer.writeByteNTimes(' ', indent);
        try writer.print("pub inline fn {s}(", .{fun.name});

        for (fun.params, 0..) |param, i| {
            if (i != 0) try writer.writeAll(", ");
            if (param.name) |name| {
                try param.type.emitParam(writer, name);
            } else {
                var buf: [Function.max_param_char_count]u8 = [_]u8{undefined} ** Function.max_param_char_count;
                const name = try std.fmt.bufPrint(&buf, "v{d}", .{i});
                try param.type.emitParam(writer, name);
            }
        }

        try writer.writeAll(") ");
        try fun.return_ty.emitParam(writer, null);
        try writer.writeAll(" {\n");

        _ = try writer.writeByteNTimes(' ', indent + 4);

        switch (fun.return_ty.info) {
            .void => {},
            .container_value => {
                try writer.writeAll("var return_val: ");
                try fun.return_ty.emitExternParam(writer, null);
                try writer.writeAll(" = undefined;\n");

                _ = try writer.writeByteNTimes(' ', indent + 4);
            },
            else => try writer.writeAll("return "),
        }

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
                var buf: [Function.max_param_char_count]u8 = [_]u8{undefined} ** Function.max_param_char_count;
                const name = try std.fmt.bufPrint(&buf, "v{d}", .{i});
                try param.type.emitExternArg(writer, name);
            }
        }

        if (fun.return_ty.info == .container_value) {
            try writer.writeAll(", &return_val);\n");
            _ = try writer.writeByteNTimes(' ', indent + 4);
            try writer.writeAll("return return_val");
        } else try writer.writeByte(')');

        if (is_ret_obj) try writer.writeByte('}');
        try writer.writeAll(";\n");

        _ = try writer.writeByteNTimes(' ', indent);
        try writer.writeAll("}\n");
    }

    pub fn emitBinding(fun: Function, writer: anytype, indent: u8, namespace: Container.Namespace) !void {
        _ = try writer.writeByteNTimes(' ', indent);
        try writer.print("export function sysjs_{s}{s}(", .{ namespace.items, fun.name });

        for (fun.params, 0..) |param, i| {
            if (i != 0) try writer.writeAll(", ");
            if (param.name) |name| {
                try param.type.emitBindingParam(writer, name);
            } else {
                var buf: [Function.max_param_char_count]u8 = [_]u8{undefined} ** Function.max_param_char_count;
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
                var buf: [Function.max_param_char_count]u8 = [_]u8{undefined} ** Function.max_param_char_count;
                const name = try std.fmt.bufPrint(&buf, "v{d}", .{i});
                try param.type.emitBindingGet(writer, name, i);
            }
        }

        _ = try writer.writeByteNTimes(' ', indent + 4);

        const is_ret_obj = fun.return_ty.info == .composite_ref;
        const is_const = fun.val_ty == .constructor;
        const is_method = fun.val_ty == .method;

        switch (fun.return_ty.info) {
            .void => {},
            .container_value => try writer.writeAll("const ret_val = "),
            .composite_ref => try writer.writeAll("return wasmWrapObject("),
            else => try writer.writeAll("return "),
        }

        if (is_const)
            try writer.writeAll("new ");

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
        name_ref: void,
        composite_ref: void,
        container_value: *Container,
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

    pub const Composite = struct {
        fields: []Field,

        pub const Field = struct {
            name: []const u8,
            type: Type,
        };
    };

    pub fn getJsSize(ty: Type) usize {
        return ty.getJsBitSize() / 8;
    }

    pub fn getJsBitSize(ty: Type) usize {
        return switch (ty.info) {
            .int => |int| std.math.powi(usize, 2, std.math.log2_int_ceil(usize, int.bits)) catch unreachable,
            .float => |float| std.math.clamp(float.bits, 32, 64),
            .ptr => 64,
            .bool => 8,
            .composite_ref => 32,
            else => 0,
        };
    }

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

        switch (ty.info) {
            .container_value => try writer.print("C{s}", .{ty.slice}),
            else => try writer.writeAll(ty.slice),
        }
    }

    // Emits parameters for functions, but in their 'extern' form.
    // e.g. expanding `[]const u8` to a pointer and length or such.
    pub fn emitExternParam(ty: Type, writer: anytype, param_name: ?[]const u8) !void {
        try printParamName(writer, param_name, null);

        switch (ty.info) {
            .int => |int| try writer.print("{c}{d}", .{ @tagName(int.signedness)[0], ty.getJsBitSize() }),
            .float => try writer.print("f{d}", .{ty.getJsBitSize()}),
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
            .container_value => try writer.print("*const C{s}", .{ty.slice}),

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
            .container_value => try writer.print("&{s}", .{arg_name}),
            else => try writer.writeAll(arg_name),
        }
    }

    pub fn emitExternField(ty: Type, writer: anytype, field_name: []const u8, struct_name: []const u8) !void {
        switch (ty.info) {
            .ptr => |ptr| switch (ptr.size) {
                .Slice => try writer.print(
                    ".{s} = {s}.{s}.ptr, .{s}_len = {s}.{s}.len,\n",
                    .{ field_name, struct_name, field_name, field_name, struct_name, field_name },
                ),
                else => {}, // TODO: one, many
            },
            .container_value => try writer.print(
                ".{s} = {s}.{s}.toExtern(),\n",
                .{ field_name, struct_name, field_name },
            ),
            .composite_ref => try writer.print(
                ".{s} = {s}.{s}.id,\n",
                .{ field_name, struct_name, field_name },
            ),
            else => try writer.print(".{s} = {s}.{s},\n", .{ field_name, struct_name, field_name }),
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
        try writer.print("const l{d} = ", .{index});
        switch (ty.info) {
            .ptr => |ptr| switch (ptr.size) {
                .Slice => try writer.print("wasmGetSlice({s}, {s}_len)", .{ param_name, param_name }),
                else => {}, // TODO: one, many
            },
            .composite_ref => {
                try writer.print("wasmGetObject({s})", .{param_name});
            },
            .container_value => |container| _ = try ty.containerBinding(writer, container, param_name),
            else => try writer.print("{s}", .{param_name}),
        }

        try writer.writeAll(";\n");
    }

    fn containerBinding(ty: Type, writer: anytype, container: *Container, param_name: []const u8) !usize {
        try writer.writeAll("{\n");

        const initial_size: usize = container.fields.items[0].type.getJsSize();
        var offset: usize = 0;
        for (container.fields.items) |field| {
            try writer.print("'{s}': ", .{field.name});
            switch (field.type.info) {
                .int => |int| try writer.print(
                    "wasmGetDataView().get{s}{d}({s} + {d}, true)",
                    .{
                        // TODO: u64/i64 support
                        if (int.signedness == .unsigned) "Uint" else "Int",
                        field.type.getJsBitSize(),
                        param_name,
                        offset,
                    },
                ),
                .float => try writer.print(
                    "wasmGetDataView().getFloat{d}({s} + {d}, true)",
                    .{ field.type.getJsBitSize(), param_name, offset },
                ),
                .ptr => |ptr| {
                    switch (ptr.size) {
                        .Slice => try writer.print(
                            "wasmGetSlice({s} + {d}, wasmGetDataView().getUint32({s} + {d} + 32, true))",
                            .{ param_name, offset, param_name, offset },
                        ),
                        else => {},
                    }
                },
                .bool => try writer.print(
                    "Boolean(wasmGetDataView().getUint8({s} + {d}, true))",
                    .{ param_name, offset },
                ),
                .container_value => |child_container| {
                    var buf: [256]u8 = undefined;
                    const buf_offset = try std.fmt.bufPrint(&buf, "{s} + {d}", .{ param_name, offset });

                    offset += try ty.containerBinding(writer, child_container, buf_offset);
                },
                .composite_ref => {
                    try writer.print("wasmGetObject(wasmGetDataView().getUint32({s} + {d}, true))", .{ param_name, offset });
                },
                else => {},
            }
            try writer.writeAll(",\n");

            offset += @max(initial_size, field.type.getJsSize());
        }
        try writer.writeAll("}");

        return offset;
    }
};
