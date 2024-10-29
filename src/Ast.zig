const std = @import("std");
const Ast = @This();
const Allocator = std.mem.Allocator;
const Token = @import("Tokenizer.zig").Token;
const TokenSlice = std.MultiArrayList(Token).Slice;

pub const Node = struct {
    kind: Kind,
    lhs: u32,
    rhs: u32,
    token: u32,

    pub const Kind = enum(u8) {
        // lhs -> child_count, rhs -> extra_index,
        // extra[0..child_count] -> indices of children.
        root,

        // lhs -> child_count, rhs -> extra_index,
        // extra[0..child_count] -> statements.
        block,

        // lhs -> child_count, rhs -> extra_index,
        // extra[0] -> fn name.
        // extra[1..count - 2] -> function args
        // extra[count - 1] -> function return type.
        // extra[count] > function body.
        fn_decl,

        // lhs -> child_count, rhs -> extra_index,
        // extra[0] = identifier.
        // extra[1..child_count] = expression.
        fn_call,

        var_decl, // lhs -> type_specifier, rhs -> expression
        const_decl, // lhs -> type_specifier, rhs -> expression
        type_specifier, // lhs -> identifier, rhs -> type_identifier

        // binary ops
        add,
        sub,
        mul,
        div,
        equal,
        not_equal,
        less_than,
        greater_than,

        // leaf nodes. lhs and rhs are unused.
        identifier,
        int_literal,
        type_identifier,
    };

    pub fn isBinOp(self: Node) bool {
        return switch (self.kind) {
            .add,
            .sub,
            .mul,
            .div,
            .equal,
            .not_equal,
            .less_than,
            .greater_than,
            => true,
            else => false,
        };
    }

    pub fn isUnaryOp(self: Node) bool {
        _ = self;
        return false;
    }
};

src: []const u8,
nodes: std.MultiArrayList(Node),
extra: std.ArrayListUnmanaged(u32),
tokens: TokenSlice,

pub fn deinit(self: *Ast, gpa: Allocator) void {
    self.nodes.deinit(gpa);
    self.extra.deinit(gpa);
    self.tokens.deinit(gpa);
}

pub fn print(self: *const Ast, writer: anytype, node_idx: u32, depth: u32) !void {
    for (0..depth) |_| {
        _ = try writer.write("  ");
    }

    const node = self.nodes.get(node_idx);
    switch (node.kind) {
        .root, .block, .fn_decl, .fn_call => {
            try writer.print("{s}\n", .{@tagName(node.kind)});
            const children = self.extra.items[node.rhs .. node.rhs + node.lhs];
            for (children) |child| {
                try self.print(writer, child, depth + 1);
            }
        },
        .var_decl,
        .const_decl,
        .type_specifier,
        .add,
        .sub,
        .mul,
        .div,
        .equal,
        .not_equal,
        .less_than,
        .greater_than,
        => {
            try writer.print("{s}\n", .{@tagName(node.kind)});
            try self.print(writer, node.lhs, depth + 1);
            try self.print(writer, node.rhs, depth + 1);
        },
        .identifier, .type_identifier, .int_literal => {
            const token = self.tokens.get(node.token);
            try writer.print("{s}: {s}\n", .{ @tagName(node.kind), self.src[token.start..token.end] });
        },
    }
}
