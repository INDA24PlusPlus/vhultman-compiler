const std = @import("std");
const Ast = @This();
const Token = @import("Tokenizer.zig").Token;
const Allocator = std.mem.Allocator;

pub const Node = struct {
    type: Type,
    lhs: u32,
    rhs: u32,
    token_index: u32,

    pub const Type = enum(u8) {
        program, // lhs -> number of children, rhs -> index to extras with children indices.
        block,

        var_statement, // lhs -> ident, rhs -> expr,
        assignment_statement, // lhs -> ident, rhs -> expr,

        return_statement, // lhs -> expression
        expression_statement, // lhs -> child expression.
        negate, // rhs -> operand
        not, // rhs -> operand

        if_statement, // lhs -> condition, rhs -> body
        if_else_statement, // lhs -> condition, rhs -> index to extras with indices to body and else-body.
        while_loop, // lhs -> condition, rhs -> body.

        // binary operations, lhs, rhs are respective operands.
        add,
        sub,
        mul,
        div,
        equal,
        not_equal,
        less_than,
        greater_than,

        // leafs.
        identifier,
        int_literal,
        bool_literal,
    };
};

tokens: std.MultiArrayList(Token),
nodes: std.MultiArrayList(Node),
extra: []u32,
src: [:0]const u8,

pub fn deinit(self: *Ast, gpa: Allocator) void {
    self.tokens.deinit(gpa);
    self.nodes.deinit(gpa);
    gpa.free(self.extra);
}

pub fn print(ast: Ast, writer: anytype, depth: u32, node_idx: u32) anyerror!void {

    // Write indent.
    for (0..depth) |_| {
        try writer.print("  ", .{});
    }

    const node = ast.nodes.get(node_idx);

    // Write node.
    if (node.type == .int_literal or node.type == .identifier) {
        const token = ast.tokens.get(node.token_index);
        try writer.print("{s}: {s}\n", .{ @tagName(node.type), ast.src[token.start..token.end] });
    } else {
        try writer.print("{s}\n", .{@tagName(node.type)});
    }

    // Write children.
    switch (node.type) {
        .add,
        .sub,
        .mul,
        .div,
        .equal,
        .not_equal,
        .less_than,
        .greater_than,
        .var_statement,
        .if_statement,
        .assignment_statement,
        .while_loop,
        => {
            try ast.print(writer, depth + 1, node.lhs);
            try ast.print(writer, depth + 1, node.rhs);
        },
        .not, .negate => {
            try ast.print(writer, depth + 1, node.rhs);
        },
        .program, .block => {
            const child_count = node.lhs;
            const child_indices = node.rhs;
            for (ast.extra[child_indices .. child_indices + child_count]) |index| {
                try ast.print(writer, depth + 1, index);
            }
        },
        .if_else_statement => {
            const extra_index = node.rhs;
            const condition = node.lhs;
            const if_body = ast.extra[extra_index + 0];
            const else_body = ast.extra[extra_index + 1];

            try ast.print(writer, depth + 1, condition);
            try ast.print(writer, depth + 1, if_body);
            try ast.print(writer, depth + 1, else_body);
        },
        .expression_statement, .return_statement => {
            try ast.print(writer, depth + 1, node.lhs);
        },

        // leaf nodes.
        .int_literal, .identifier, .bool_literal => return,
    }
}
