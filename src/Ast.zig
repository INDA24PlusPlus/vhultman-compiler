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
        root,

        fn_decl,

        identifier,
        type_identifier,
    };
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
        .root => {
            try writer.print("{s}\n", .{@tagName(node.kind)});
            const children = self.extra.items[node.rhs .. node.rhs + node.lhs];
            for (children) |child| {
                try self.print(writer, child, depth + 1);
            }
        },
        .fn_decl => {
            try writer.print("{s}\n", .{@tagName(node.kind)});
            try self.print(writer, node.lhs, depth + 1);
            try self.print(writer, node.rhs, depth + 1);
        },
        .identifier, .type_identifier => {
            const token = self.tokens.get(node.token);
            try writer.print("{s}: {s}\n", .{ @tagName(node.kind), self.src[token.start..token.end] });
        },
    }
}
