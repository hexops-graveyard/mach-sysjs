const std = @import("std");
const Ast = std.zig.Ast;

// Borrowed from ZLS analysis engine:
// https://github.com/zigtools/zls/blob/master/src/analysis.zig#L301
pub fn getDeclNameToken(tree: Ast, node: Ast.Node.Index) ?Ast.TokenIndex {
    const tags = tree.nodes.items(.tag);
    const datas = tree.nodes.items(.data);
    const main_token = tree.nodes.items(.main_token)[node];

    return switch (tags[node]) {
        // regular declaration names. + 1 to mut token because name comes after 'const'/'var'
        .local_var_decl,
        .global_var_decl,
        .simple_var_decl,
        .aligned_var_decl,
        => {
            const tok = tree.fullVarDecl(node).?.ast.mut_token + 1;
            return if (tok >= tree.tokens.len)
                null
            else
                tok;
        },
        // function declaration names
        .fn_proto,
        .fn_proto_multi,
        .fn_proto_one,
        .fn_proto_simple,
        .fn_decl,
        => blk: {
            var params: [1]Ast.Node.Index = undefined;
            break :blk tree.fullFnProto(&params, node).?.name_token;
        },

        // containers
        .container_field,
        .container_field_init,
        .container_field_align,
        => {
            const field = tree.fullContainerField(node).?.ast;
            return field.main_token;
        },

        .identifier => main_token,
        .error_value => {
            const tok = main_token + 2;
            return if (tok >= tree.tokens.len)
                null
            else
                tok;
        }, // 'error'.<main_token +2>

        .test_decl => datas[node].lhs,

        else => null,
    };
}

pub fn getDeclType(tree: Ast, decl_idx: Ast.Node.Index) enum { field, decl, other } {
    const tags = tree.nodes.items(.tag);
    return switch (tags[decl_idx]) {
        .container_field_init,
        .container_field_align,
        .container_field,
        => .field,
        .local_var_decl,
        .global_var_decl,
        .simple_var_decl,
        .aligned_var_decl,
        => .decl,
        else => .other,
    };
}
