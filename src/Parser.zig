const std = @import("std");
const Ast = @import("Ast.zig");
const Parser = @This();
const Token = Tokenizer.Token;
const Tokenizer = @import("Tokenizer.zig");
const Allocator = std.mem.Allocator;
const TokenSlice = std.MultiArrayList(Token).Slice;

nodes: std.MultiArrayList(Ast.Node),
extra: std.ArrayListUnmanaged(u32),
errors: std.ArrayListUnmanaged(ParseError),

src: [:0]const u8,
tokens: TokenSlice,
token_index: u32,

gpa: Allocator,

curr_token: Token,
next_token: Token,

pub fn init(gpa: Allocator, src: [:0]const u8) !Parser {
    var tokens = std.MultiArrayList(Token){};
    var tokenizer = Tokenizer.init(src);
    while (tokenizer.next()) |token| {
        std.log.debug("{}: {s}", .{ token, src[token.start..token.end] });
        try tokens.append(gpa, token);
    }

    return .{
        .src = src,
        .nodes = std.MultiArrayList(Ast.Node){},
        .extra = std.ArrayListUnmanaged(u32){},
        .errors = std.ArrayListUnmanaged(ParseError){},
        .tokens = tokens.slice(),
        .token_index = 0,
        .curr_token = undefined,
        .next_token = tokens.get(0),
        .gpa = gpa,
    };
}

pub fn deinit(self: *Parser) void {
    self.errors.deinit(self.gpa);
}

pub fn parse(self: *Parser) !Ast {
    const root_node = try self.nodes.addOne(self.gpa);
    var child_nodes = std.ArrayListUnmanaged(u32){};
    defer child_nodes.deinit(self.gpa);

    while (self.next_token.kind != .eof) {
        const child = self.parseRootDecls() catch |err| switch (err) {
            error.ParseError => {
                self.syncRootDecl();
                continue;
            },
            else => return err,
        };

        try child_nodes.append(self.gpa, child);
    }

    const child_count = child_nodes.items.len;
    const extra_index = self.extra.items.len;
    try self.extra.appendSlice(self.gpa, child_nodes.items);

    self.nodes.set(root_node, .{
        .kind = .root,
        .lhs = @intCast(child_count),
        .rhs = @intCast(extra_index),
        .token = undefined,
    });

    return .{
        .nodes = self.nodes,
        .extra = self.extra,
        .tokens = self.tokens,
        .src = self.src,
    };
}

fn syncRootDecl(self: *Parser) void {
    @branchHint(.cold);
    while (self.next_token.kind != .eof) {
        self.advanceTokenStream();
        if (self.curr_token.kind == .@"}" or self.next_token.kind == .@"fn") {
            return;
        }
    }
}

fn advanceTokenStream(self: *Parser) void {
    self.curr_token = self.next_token;
    self.token_index += 1;

    if (self.token_index >= self.tokens.len) {
        @branchHint(.unlikely);
        self.next_token = .{ .end = 0, .start = 0, .kind = .eof };
    } else {
        self.next_token = self.tokens.get(self.token_index);
    }
}

fn expectNext(self: *Parser, kind: Token.Kind) !void {
    if (self.next_token.kind != kind) {
        @branchHint(.unlikely);
        try self.errors.append(self.gpa, .{
            .kind = .expected_token,
            .actual = self.next_token,
            .expected = kind,
        });

        return error.ParseError;
    }

    self.advanceTokenStream();
}

fn currentTokenIndex(self: *const Parser) u32 {
    return self.token_index - 1;
}

fn parseRootDecls(self: *Parser) !u32 {
    return switch (self.next_token.kind) {
        .@"fn" => self.parseFunction(),
        else => {
            try self.errors.append(self.gpa, .{
                .kind = .invalid_root_token,
                .actual = self.next_token,
            });
            return error.ParseError;
        },
    };
}

fn parseFunction(self: *Parser) !u32 {
    try self.expectNext(.@"fn");

    const node_index = try self.nodes.addOne(self.gpa);
    const fn_name = try self.parseIdentifier();
    try self.expectNext(.@"(");
    try self.expectNext(.@")");
    const return_type = try self.parseTypeIdentifier();

    self.nodes.set(node_index, .{
        .kind = .fn_decl,
        .token = self.token_index,
        .lhs = fn_name,
        .rhs = return_type,
    });

    return @intCast(node_index);
}

fn parseTypeIdentifier(self: *Parser) !u32 {
    try self.expectNext(.primitive_type);
    try self.nodes.append(self.gpa, .{
        .kind = .type_identifier,
        .token = self.currentTokenIndex(),
        .rhs = undefined,
        .lhs = undefined,
    });

    return @intCast(self.nodes.len - 1);
}

fn parseIdentifier(self: *Parser) !u32 {
    try self.expectNext(.identifier);
    try self.nodes.append(self.gpa, .{
        .kind = .identifier,
        .token = self.currentTokenIndex(),
        .rhs = undefined,
        .lhs = undefined,
    });

    return @intCast(self.nodes.len - 1);
}

pub const ParseError = struct {
    kind: Kind,
    actual: Token,
    expected: ?Token.Kind = null,

    const Kind = enum {
        invalid_root_token,
        expected_token,
    };
};
