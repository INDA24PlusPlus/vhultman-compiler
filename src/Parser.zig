const std = @import("std");
const Ast = @import("Ast.zig");
const Parser = @This();
const Token = Tokenizer.Token;
const Tokenizer = @import("Tokenizer.zig");
const Allocator = std.mem.Allocator;
const TokenSlice = std.MultiArrayList(Token).Slice;

const log = std.log.scoped(.parser);

nodes: std.MultiArrayList(Ast.Node),
extra: std.ArrayListUnmanaged(u32),
errors: std.ArrayListUnmanaged(ParseError),
owns_nodes: bool = true,

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
        try tokens.append(gpa, token);
    }

    const next_token: Token = if (tokens.len == 0)
        .{
            .end = @intCast(src.len - 1),
            .start = @intCast(src.len - 1),
            .kind = .eof,
        }
    else
        tokens.get(0);

    return .{
        .src = src,
        .nodes = std.MultiArrayList(Ast.Node){},
        .extra = std.ArrayListUnmanaged(u32){},
        .errors = std.ArrayListUnmanaged(ParseError){},
        .tokens = tokens.slice(),
        .token_index = 0,
        .curr_token = undefined,
        .next_token = next_token,
        .gpa = gpa,
    };
}

pub fn deinit(self: *Parser) void {
    self.errors.deinit(self.gpa);

    if (self.owns_nodes) {
        self.nodes.deinit(self.gpa);
        self.extra.deinit(self.gpa);
        self.tokens.deinit(self.gpa);
    }
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

    if (self.errors.items.len != 0) {
        return error.HadParseError;
    }
    self.owns_nodes = false;

    return .{
        .nodes = self.nodes,
        .extra = self.extra,
        .tokens = self.tokens,
        .src = self.src,
    };
}

const Precedence = enum {
    none,
    equals,
    greater_less,
    add,
    mul,
    prefix,
    call,
};

fn currentPrecedence(self: *Parser) Precedence {
    return switch (self.curr_token.kind) {
        .@"==", .@"!=" => .equals,
        .@"<", .@">", .@"<=", .@">=" => .greater_less,
        .@"+", .@"-" => .add,
        .@"*", .@"/" => .mul,
        .@"(" => .call,
        else => .none,
    };
}

fn nextPrecedence(self: *Parser) Precedence {
    return switch (self.next_token.kind) {
        .@"==", .@"!=" => .equals,
        .@"<", .@">", .@"<=", .@">=" => .greater_less,
        .@"+", .@"-" => .add,
        .@"*", .@"/" => .mul,
        .@"(" => .call,
        else => .none,
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

fn syncBlock(self: *Parser) void {
    @branchHint(.cold);
    while (self.next_token.kind != .eof and self.next_token.kind != .@"}") {
        self.advanceTokenStream();
        if (self.curr_token.kind == .@";") {
            return;
        }
    }
}

fn advanceTokenStream(self: *Parser) void {
    self.curr_token = self.next_token;
    self.token_index += 1;

    if (self.token_index >= self.tokens.len) {
        @branchHint(.unlikely);
        self.next_token = .{
            .end = @intCast(self.src.len - 1),
            .start = @intCast(self.src.len - 1),
            .kind = .eof,
        };
    } else {
        self.next_token = self.tokens.get(self.token_index);
    }
}

fn expectNext(self: *Parser, kind: Token.Kind) !void {
    if (self.next_token.kind != kind) {
        @branchHint(.unlikely);
        try self.errors.append(self.gpa, .{
            .kind = .expected_token,
            .error_token = self.next_token,
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
                .error_token = self.next_token,
            });
            return error.ParseError;
        },
    };
}

fn parseFunction(self: *Parser) !u32 {
    try self.expectNext(.@"fn");
    const node_index = try self.nodes.addOne(self.gpa);

    var child_nodes = std.ArrayListUnmanaged(u32){};
    defer child_nodes.deinit(self.gpa);

    const fn_name = try self.parseIdentifier();
    try child_nodes.append(self.gpa, fn_name);
    try self.expectNext(.@"(");

    if (self.next_token.kind != .@")") {
        try child_nodes.append(self.gpa, try self.parseTypeSpecifier());
        while (self.next_token.kind == .@",") {
            try self.expectNext(.@",");
            try child_nodes.append(self.gpa, try self.parseTypeSpecifier());
        }
    }

    try self.expectNext(.@")");
    const return_type = try self.parseTypeIdentifier();
    try child_nodes.append(self.gpa, return_type);

    const block = try self.parseBlock();
    try child_nodes.append(self.gpa, block);

    const child_count = child_nodes.items.len;
    const extra_index = self.extra.items.len;
    try self.extra.appendSlice(self.gpa, child_nodes.items);

    self.nodes.set(node_index, .{
        .kind = .fn_decl,
        .token = self.token_index,
        .lhs = @intCast(child_count),
        .rhs = @intCast(extra_index),
    });

    return @intCast(node_index);
}

fn parseBlock(self: *Parser) !u32 {
    try self.expectNext(.@"{");
    const token = self.currentTokenIndex();
    const node_index = try self.nodes.addOne(self.gpa);
    var child_nodes = std.ArrayListUnmanaged(u32){};
    defer child_nodes.deinit(self.gpa);

    while (self.next_token.kind != .@"}" and self.next_token.kind != .eof) {
        const child = self.parseStatement() catch |err| switch (err) {
            error.ParseError => {
                self.syncBlock();
                continue;
            },
            else => return err,
        };
        try child_nodes.append(self.gpa, child);
    }

    try self.expectNext(.@"}");
    const child_count = child_nodes.items.len;
    const extra_index = self.extra.items.len;
    try self.extra.appendSlice(self.gpa, child_nodes.items);

    self.nodes.set(node_index, .{
        .kind = .block,
        .lhs = @intCast(child_count),
        .rhs = @intCast(extra_index),
        .token = token,
    });

    return @intCast(node_index);
}

fn parseStatement(self: *Parser) !u32 {
    return switch (self.next_token.kind) {
        .@"var" => self.parseVarDecl(),
        .@"const" => self.parseConstDecl(),
        .@"return" => self.parseReturn(),
        .@"if" => self.parseIf(),
        .@"while" => self.parseWhile(),
        .@"{" => self.parseBlock(),
        .identifier => self.parseAssignment(),
        else => {
            try self.errors.append(self.gpa, .{
                .kind = .expected_statement,
                .error_token = self.next_token,
            });
            return error.ParseError;
        },
    };
}

fn parseWhile(self: *Parser) !u32 {
    try self.expectNext(.@"while");
    const node_index = try self.nodes.addOne(self.gpa);
    const token = self.currentTokenIndex();
    try self.expectNext(.@"(");
    const cond = try self.parseExpression(.none);
    try self.expectNext(.@")");
    const body = try self.parseStatement();

    self.nodes.set(node_index, .{
        .kind = .while_loop,
        .lhs = cond,
        .rhs = body,
        .token = token,
    });

    return @intCast(node_index);
}

fn parseIf(self: *Parser) !u32 {
    try self.expectNext(.@"if");
    const node_index = try self.nodes.addOne(self.gpa);
    const token = self.currentTokenIndex();
    try self.expectNext(.@"(");
    const cond = try self.parseExpression(.none);
    try self.expectNext(.@")");
    const body = try self.parseStatement();

    var child_nodes = try std.ArrayListUnmanaged(u32).initCapacity(self.gpa, 3);
    defer child_nodes.deinit(self.gpa);
    try child_nodes.append(self.gpa, cond);
    try child_nodes.append(self.gpa, body);

    if (self.next_token.kind == .@"else") {
        self.advanceTokenStream();
        const else_body = try self.parseStatement();
        try child_nodes.append(self.gpa, else_body);
    }

    const child_count = child_nodes.items.len;
    const extra_index = self.extra.items.len;
    try self.extra.appendSlice(self.gpa, child_nodes.items);

    self.nodes.set(node_index, .{
        .kind = .if_statement,
        .lhs = @intCast(child_count),
        .rhs = @intCast(extra_index),
        .token = token,
    });

    return @intCast(node_index);
}

fn parseAssignment(self: *Parser) !u32 {
    const identifier = try self.parseIdentifier();
    const node_index = try self.nodes.addOne(self.gpa);
    try self.expectNext(.@"=");
    const token = self.currentTokenIndex();

    const expr = try self.parseExpression(.none);
    try self.expectNext(.@";");
    self.nodes.set(node_index, .{
        .kind = .assignment,
        .lhs = identifier,
        .rhs = expr,
        .token = token,
    });

    return @intCast(node_index);
}

fn parseReturn(self: *Parser) !u32 {
    try self.expectNext(.@"return");
    const node_index = try self.nodes.addOne(self.gpa);
    const token = self.currentTokenIndex();
    const expr = try self.parseExpression(.none);
    try self.expectNext(.@";");

    self.nodes.set(node_index, .{
        .kind = .return_statement,
        .lhs = expr,
        .rhs = undefined,
        .token = token,
    });

    return @intCast(node_index);
}

fn parseVarDecl(self: *Parser) !u32 {
    try self.expectNext(.@"var");
    const token = self.currentTokenIndex();

    const node_index = try self.nodes.addOne(self.gpa);
    const type_specifier = try self.parseTypeSpecifier();
    try self.expectNext(.@"=");
    const expr = try self.parseExpression(.none);
    try self.expectNext(.@";");

    self.nodes.set(node_index, .{
        .kind = .var_decl,
        .lhs = type_specifier,
        .rhs = expr,
        .token = token,
    });

    return @intCast(node_index);
}

fn parseConstDecl(self: *Parser) !u32 {
    try self.expectNext(.@"const");
    const token = self.currentTokenIndex();

    const node_index = try self.nodes.addOne(self.gpa);
    const type_specifier = try self.parseTypeSpecifier();
    try self.expectNext(.@"=");
    const expr = try self.parseExpression(.none);
    try self.expectNext(.@";");

    self.nodes.set(node_index, .{
        .kind = .const_decl,
        .lhs = type_specifier,
        .rhs = expr,
        .token = token,
    });

    return @intCast(node_index);
}

fn parseExpression(self: *Parser, precedence: Precedence) anyerror!u32 {
    var lhs = switch (self.next_token.kind) {
        .identifier => try self.parseIdentifier(),
        .int_literal => try self.parseIntLiteral(),
        .bool_literal => try self.parseBoolLiteral(),
        .@"(" => try self.parseGroupExpression(),
        else => {
            try self.errors.append(self.gpa, .{ .kind = .expected_expression, .error_token = self.next_token });
            return error.ParseError;
        },
    };

    while (self.next_token.kind != .@";" and @intFromEnum(precedence) < @intFromEnum(self.nextPrecedence())) {
        lhs = switch (self.next_token.kind) {
            .@"+",
            .@"-",
            .@"*",
            .@"/",
            .@"==",
            .@"!=",
            .@"<",
            .@">",
            .@"<=",
            .@">=",
            => blk: {
                break :blk try self.parseBinaryExpression(lhs);
            },
            .@"(" => try self.parseFunctionCall(lhs),
            else => lhs,
        };
    }

    return lhs;
}

fn parseFunctionCall(self: *Parser, identifier: u32) !u32 {
    try self.expectNext(.@"(");
    const token = self.currentTokenIndex();
    const node_index = try self.nodes.addOne(self.gpa);

    var child_nodes = std.ArrayListUnmanaged(u32){};
    defer child_nodes.deinit(self.gpa);
    try child_nodes.append(self.gpa, identifier);

    if (self.next_token.kind != .@")") {
        try child_nodes.append(self.gpa, try self.parseExpression(.none));
        while (self.next_token.kind == .@",") {
            try self.expectNext(.@",");
            try child_nodes.append(self.gpa, try self.parseExpression(.none));
        }
    }
    try self.expectNext(.@")");

    const child_count = child_nodes.items.len;
    const extra_index = self.extra.items.len;
    try self.extra.appendSlice(self.gpa, child_nodes.items);
    self.nodes.set(node_index, .{
        .kind = .fn_call,
        .lhs = @intCast(child_count),
        .rhs = @intCast(extra_index),
        .token = token,
    });

    return @intCast(node_index);
}

fn parseGroupExpression(self: *Parser) !u32 {
    try self.expectNext(.@"(");
    const expr = try self.parseExpression(.none);
    try self.expectNext(.@")");

    return expr;
}

fn parseBinaryExpression(self: *Parser, lhs: u32) !u32 {
    const index = try self.nodes.addOne(self.gpa);
    const token = self.next_token.kind;

    const precedence = self.nextPrecedence();
    self.advanceTokenStream();
    const token_index = self.currentTokenIndex();
    const rhs = try self.parseExpression(precedence);

    self.nodes.set(index, .{
        .kind = switch (token) {
            .@"+" => .add,
            .@"-" => .sub,
            .@"*" => .mul,
            .@"/" => .div,
            .@"==" => .equal,
            .@"!=" => .not_equal,
            .@"<" => .less_than,
            .@">" => .greater_than,
            .@"<=" => .less_than_equal,
            .@">=" => .greater_than_equal,
            else => unreachable,
        },
        .rhs = rhs,
        .lhs = lhs,
        .token = token_index,
    });

    return @intCast(index);
}

fn parseTypeSpecifier(self: *Parser) !u32 {
    const node_index = try self.nodes.addOne(self.gpa);
    const identifier = try self.parseIdentifier();
    try self.expectNext(.@":");
    const t = try self.parseTypeIdentifier();

    self.nodes.set(node_index, .{
        .kind = .type_specifier,
        .lhs = identifier,
        .rhs = t,
        .token = undefined,
    });

    return @intCast(node_index);
}

fn parseBoolLiteral(self: *Parser) !u32 {
    try self.expectNext(.bool_literal);
    try self.nodes.append(self.gpa, .{
        .kind = .bool_literal,
        .token = self.currentTokenIndex(),
        .rhs = undefined,
        .lhs = undefined,
    });

    return @intCast(self.nodes.len - 1);
}

fn parseIntLiteral(self: *Parser) !u32 {
    try self.expectNext(.int_literal);
    try self.nodes.append(self.gpa, .{
        .kind = .int_literal,
        .token = self.currentTokenIndex(),
        .rhs = undefined,
        .lhs = undefined,
    });

    return @intCast(self.nodes.len - 1);
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
    error_token: Token,
    expected: ?Token.Kind = null,

    const Kind = enum {
        expected_expression,
        expected_statement,
        invalid_root_token,
        expected_token,
    };

    pub fn print(self: ParseError, writer: anytype, src: []const u8, file_name: []const u8) !void {
        const red = "\x1B[31m";
        const green = "\x1B[32m";
        const reset = "\x1B[0m";
        const error_line = self.error_token.computeLineInfo(src);

        try writer.print("{s}:{d}:{d}: ", .{
            file_name,
            error_line.line_number,
            error_line.column_number,
        });

        const print_source_line = self.error_token.kind != .eof;

        try writer.print(red ++ "error: " ++ reset, .{});
        switch (self.kind) {
            .invalid_root_token => {
                try writer.print("Expected function declaration but \"{s}\"\n", .{@tagName(self.error_token.kind)});
            },
            .expected_token => {
                try writer.print("Expected \"{s}\" but got \"{s}\"\n", .{ @tagName(self.expected.?), @tagName(self.error_token.kind) });
            },
            .expected_expression => {
                try writer.print("Expected expression but \"{s}\"\n", .{@tagName(self.error_token.kind)});
            },
            .expected_statement => {
                try writer.print("Expected statement but \"{s}\"\n", .{@tagName(self.error_token.kind)});
            },
        }

        if (print_source_line) {
            const offset = @intFromPtr(error_line.line.ptr) - @intFromPtr(src.ptr);
            try writer.print("{s}" ++ red ++ "{s}" ++ reset ++ "{s}\n", .{
                src[offset..self.error_token.start],
                src[self.error_token.start..self.error_token.end],
                src[self.error_token.end .. offset + error_line.line.len],
            });

            const error_token_start = self.error_token.start - offset;
            try writer.writeByteNTimes(' ', error_token_start);
            try writer.print(green ++ "^", .{});
            try writer.writeByteNTimes('~', self.error_token.end - self.error_token.start - 1);
            try writer.print(reset ++ "\n", .{});
        }
    }
};
