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
        name: []const u8,
        type: Type,
    };
};

pub const Type = union(enum) {
    int: Int,
    ptr: Ptr,

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

    pub fn emitExternParam(ty: Type, writer: anytype, param_name: ?[]const u8) !void {
        try printParamName(writer, param_name, null);

        switch (ty) {
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
        switch (ty) {
            .ptr => |ptr| switch (ptr.size) {
                .slice => try std.fmt.format(writer, "{s}.ptr, {s}.len", .{ arg_name, arg_name }),
                else => {}, // TODO: one, many
            },
            else => try writer.writeAll(arg_name),
        }
    }

    pub fn fromAst(allocator: std.mem.Allocator, tree: Ast, index: Ast.TokenIndex) !Type {
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
            base_ty.* = Type{ .int = .{
                .bits = 8,
                .signedness = .unsigned,
            } };
            return Type{ .ptr = .{
                .size = s,
                .is_const = is_const,
                .base_ty = base_ty,
            } };
        } else {
            return Type{ .int = .{ .bits = 32, .signedness = .unsigned } };
        }
    }
};
