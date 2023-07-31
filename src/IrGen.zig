const std = @import("std");
const Ast = std.zig.Ast;
const analysis = @import("analysis.zig");
const types = @import("types.zig");
const Container = types.Container;
const Function = types.Function;
const Type = types.Type;

const IrGen = @This();

ast: Ast,
allocator: std.mem.Allocator,
root: *Container = undefined,

pub fn addRoot(gen: *IrGen) !void {
    gen.root = try gen.addContainer(null, gen.ast.containerDeclRoot(), null);
    gen.resolveContainer(gen.root);
}

fn addContainer(
    gen: IrGen,
    parent: ?*Container,
    container_decl: Ast.full.ContainerDecl,
    name: ?[]const u8,
) anyerror!*Container {
    var cont = try gen.allocator.create(Container);

    const is_struct_bindings = (name != null and std.mem.eql(u8, name.?, "bindings"));
    const is_parent_bindings = (parent != null and parent.?.val_type == .namespace);

    cont.* = Container{
        .name = if (is_struct_bindings) null else name,
        .parent = parent,
        .val_type = if (is_parent_bindings or is_struct_bindings)
            .namespace
        else
            .struct_val,
    };

    for (container_decl.ast.members) |decl_idx| {
        const std_type = analysis.getDeclType(gen.ast, decl_idx);
        switch (std_type) {
            // If this decl is a struct field `foo: fn () void,` then consume it.
            .field => {
                try gen.addContainerField(cont, decl_idx);
                continue;
            },
            // If this decl is a `pub const foo = struct {};` then consume it
            .decl => {
                if (try gen.addContainerDecl(cont, decl_idx))
                    continue;
            },
            .other => {},
        }

        try cont.contents.append(gen.allocator, .{ .std_code = .{
            .data = gen.ast.getNodeSource(decl_idx),
            .type = std_type,
        } });
    }

    if (cont.val_type == .namespace) {
        const emit_id_field: bool = blk: {
            for (cont.contents.items) |content| {
                switch (content) {
                    .func => |func| if (func.val_ty != .none)
                        break :blk true
                    else
                        continue,
                    else => continue,
                }
            }
            break :blk false;
        };

        if (emit_id_field) {
            try cont.fields.append(gen.allocator, .{
                .name = "id",
                .type = .{
                    .slice = "u32",
                    .info = .{ .int = .{ .bits = 32, .signedness = .unsigned } },
                },
            });
        }
    }

    return cont;
}

// Resolves all types in a container recursively
fn resolveContainer(gen: *IrGen, cont: *Container) void {
    for (cont.contents.items, 0..) |item, idx| {
        switch (item) {
            .func => {
                var fun = cont.contents.items[idx].func;
                fun.return_ty = gen.resolveType(cont, fun.return_ty);

                for (fun.params, 0..) |param, pid| {
                    var par = fun.params[pid];
                    par.type = gen.resolveType(cont, param.type);
                    fun.params[pid] = par;
                }
            },
            .container => |child| gen.resolveContainer(child),
            else => {},
        }
    }
}

fn addContainerField(
    gen: IrGen,
    container: *Container,
    node: Ast.Node.Index,
) !void {
    const field = gen.ast.fullContainerField(node).?;

    if (field.ast.value_expr == 0) {
        const type_expr = gen.ast.nodes.get(field.ast.type_expr);
        switch (type_expr.tag) {
            .fn_proto_simple, .fn_proto_multi => {
                if (container.val_type != .struct_val) {
                    try gen.addFunction(
                        container,
                        field.ast.type_expr,
                        field.ast.main_token,
                    );
                    return;
                }
            },
            else => {},
        }
    }

    try container.fields.append(gen.allocator, .{
        .name = gen.ast.tokenSlice(field.ast.main_token),
        .type = try gen.makeType(field.ast.type_expr),
    });
}

fn addContainerDecl(
    gen: IrGen,
    container: *Container,
    node: Ast.Node.Index,
) !bool {
    const var_decl = gen.ast.fullVarDecl(node).?;
    // var/const
    if (var_decl.visib_token) |visib_token| {
        const visib = gen.ast.tokenSlice(visib_token);
        if (std.mem.eql(u8, visib, "pub")) {
            // pub var/const
            const init_node = gen.ast.nodes.get(var_decl.ast.init_node);
            switch (init_node.tag) {
                .container_decl,
                .container_decl_two,
                .container_decl_trailing,
                .container_decl_two_trailing,
                => {
                    var buf: [2]Ast.Node.Index = undefined;
                    const container_decl = gen.ast.fullContainerDecl(&buf, var_decl.ast.init_node).?;

                    const name = if (analysis.getDeclNameToken(gen.ast, node)) |token|
                        gen.ast.tokenSlice(token)
                    else
                        null;

                    var cont = try gen.addContainer(container, container_decl, name);
                    try container.contents.append(gen.allocator, .{ .container = cont });

                    return true;
                },
                else => {},
            }
        }
    }

    return false;
}

fn addFunction(gen: IrGen, container: *Container, node_index: Ast.TokenIndex, name_token: Ast.TokenIndex) !void {
    var param_buf: [1]Ast.Node.Index = undefined;
    const fn_proto = gen.ast.fullFnProto(&param_buf, node_index).?;

    const name = gen.ast.tokenSlice(name_token);
    const return_type = try gen.makeType(fn_proto.ast.return_type);

    var params: std.ArrayListUnmanaged(Function.Param) = .{};

    var params_iter = fn_proto.iterate(&gen.ast);
    var i: usize = 0;
    while (params_iter.next()) |param| : (i += 1) {
        try params.append(gen.allocator, .{
            .name = if (param.name_token) |nt| gen.ast.tokenSlice(nt) else null,
            .type = try gen.makeType(param.type_expr),
        });
    }

    var func_obj = try gen.allocator.create(Function);
    func_obj.* = Function{
        .name = name,
        .return_ty = return_type,
        .params = params.items,
        .parent = container,
        .val_ty = if (container.name) |coname|
            if (std.mem.eql(u8, coname, return_type.slice))
                .constructor
            else if (params.items.len > 0 and
                std.mem.eql(u8, coname, params.items[0].type.slice))
                .method
            else
                .none
        else
            .none,
    };

    try container.contents.append(gen.allocator, .{ .func = func_obj });
}

fn makeType(gen: IrGen, index: Ast.Node.Index) !Type {
    const token_slice = gen.ast.getNodeSource(index);
    if (gen.ast.fullPtrType(index)) |ptr| {
        const child_ty = try gen.makeType(ptr.ast.child_type);
        var base_ty = try gen.allocator.create(Type);
        base_ty.* = child_ty;

        return Type{
            .slice = token_slice,
            .info = .{ .ptr = Type.Ptr{
                .size = ptr.size,
                .is_const = ptr.const_token != null,
                .base_ty = base_ty,
            } },
        };
    }

    if (std.zig.primitives.isPrimitive(token_slice)) {
        const signedness: ?Type.Int.Signedness = switch (token_slice[0]) {
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
                        .int = Type.Int{
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
                    .float = Type.Float{
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
                        .info = @unionInit(Type.TypeInfo, field.name, {}),
                    };
                }
            }
        }

        @panic("TODO: error on impossible types");
    }

    return Type{
        .slice = token_slice,
        .info = .{ .name_ref = {} },
    };
}

fn resolveType(
    gen: IrGen,
    container: *Container,
    ty: Type,
) Type {
    _ = gen;
    switch (ty.info) {
        .name_ref => {
            if (container.name) |cname| {
                if (std.mem.eql(u8, cname, ty.slice))
                    return Type{ .slice = cname, .info = .{ .composite_ref = {} } };
            }

            var container_parent = container.parent;
            while (container_parent) |parent| {
                for (parent.contents.items) |item| {
                    switch (item) {
                        .container => |cont| {
                            if (cont.name) |cname| {
                                if (std.mem.eql(u8, cname, ty.slice)) {
                                    if (cont.val_type == .namespace) {
                                        return Type{ .slice = cname, .info = .{ .composite_ref = {} } };
                                    } else {
                                        // TODO: resolve struct_val
                                    }
                                }
                            }
                        },
                        else => {},
                    }
                }

                container_parent = parent.parent;
            }
        },
        else => {},
    }

    return ty;
}

fn makeCompositeType(
    gen: IrGen,
    container: Ast.full.ContainerDecl,
    init_node: Ast.Node.Index,
) !Type {
    var fields: std.ArrayListUnmanaged(Type.Composite.Field) = .{};

    for (container.ast.members) |decl_idx| {
        const std_type = analysis.getDeclType(gen.ast, decl_idx);
        switch (std_type) {
            .field => {
                const field = gen.ast.fullContainerField(decl_idx);
                const name = gen.ast.tokenSlice(field.?.ast.main_token);
                const ty = try gen.makeType(field.?.ast.type_expr);
                var ty_alloc = try gen.allocator.create(Type);
                ty_alloc.* = ty;

                try fields.append(gen.allocator, .{
                    .name = name,
                    .type = ty_alloc,
                });
            },
            else => {}, // TODO
        }
    }

    return Type{ .slice = gen.ast.getNodeSource(init_node), .info = .{
        .composite = .{ .fields = try fields.toOwnedSlice(gen.allocator) },
    } };
}
