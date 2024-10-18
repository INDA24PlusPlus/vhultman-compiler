const std = @import("std");
const assert = std.debug.assert;
const Parser = @This();
const Ast = @import("Ast.zig");
const Tokenizer = @import("Tokenizer.zig");
const Token = Tokenizer.Token;
const Allocator = std.mem.Allocator;

nodes: std.MultiArrayList(Ast.Node),
errors: std.ArrayListUnmanaged(ParseError),
extra: std.ArrayListUnmanaged(u32),

gpa: Allocator,
src: [:0]const u8,
tokenizer: Tokenizer,
tokens: std.MultiArrayList(Token),

curr_token: Token,
next_token: Token,
token_index: u32,

pub fn advanceTokenStream(self: *Parser) !void {
    try self.tokens.append(self.gpa, self.curr_token);
    self.curr_token = self.next_token;
    self.next_token = self.tokenizer.next();
    self.token_index += 1;
}

pub fn init(gpa: Allocator, src: [:0]const u8) Parser {
    var self: Parser = .{
        .src = src,
        .gpa = gpa,
        .tokenizer = Tokenizer.init(src),
        .extra = std.ArrayListUnmanaged(u32){},
        .nodes = std.MultiArrayList(Ast.Node){},
        .tokens = std.MultiArrayList(Token){},
        .errors = std.ArrayListUnmanaged(ParseError){},
        .next_token = undefined,
        .curr_token = undefined,
        .token_index = 0,
    };
    self.curr_token = self.tokenizer.next();
    self.next_token = self.tokenizer.next();

    return self;
}

pub fn deinit(self: *Parser) void {
    self.errors.deinit(self.gpa);
}

// Recursive descent + pratt parsing for operator precedence (https://journal.stuffwithstuff.com/2011/03/19/pratt-parsers-expression-parsing-made-easy/)
pub fn parse(self: *Parser) !Ast {
    // Make space for the root node.
    const root_index = try self.nodes.addOne(self.gpa);

    // Indices of the root's children
    var children_indices = std.ArrayListUnmanaged(u32){};
    defer children_indices.deinit(self.gpa);

    // Parse statements
    while (self.curr_token.type != .eof) {
        if (self.parseStatement()) |index| {
            try children_indices.append(self.gpa, index);
        } else |err| {
            if (err != error.FailedToParse) {
                return err;
            }
        }

        try self.advanceTokenStream();
    }

    // Append root children indices to extra.
    const child_indices_start = self.extra.items.len;
    try self.extra.appendSlice(self.gpa, children_indices.items);

    self.nodes.set(root_index, .{
        .type = .program,
        .lhs = @intCast(children_indices.items.len),
        .rhs = @intCast(child_indices_start),
        .token_index = 0,
    });

    if (self.errors.items.len != 0) {
        return error.ParseError;
    }

    return .{
        .src = self.src,
        .nodes = self.nodes,
        .extra = try self.extra.toOwnedSlice(self.gpa),
        .tokens = self.tokens,
    };
}

fn parseStatement(self: *Parser) !u32 {
    return switch (self.curr_token.type) {
        .var_stmt => self.parseVarStatement(),
        .@"return" => self.parseReturnStatement(),
        .@"{" => self.parseBlockStatement(),
        .@"if" => self.parseIfStatement(),
        .@"while" => self.parseWhileLoop(),
        .identifier => if (self.next_token.type == .assign)
            try self.parseAssignmentStatement()
        else
            try self.parseExpressionStatement(),
        .invalid => {
            try self.errors.append(self.gpa, .{
                .token = self.curr_token,
                .type = .invalid_token,
            });
            return error.FailedToParse;
        },
        else => self.parseExpressionStatement(),
    };
}

fn parseWhileLoop(self: *Parser) !u32 {
    if (!try self.expectNext(.@"(")) {
        return error.FailedToParse;
    }

    const node_index = try self.nodes.addOne(self.gpa);

    try self.advanceTokenStream();
    const condition = try self.parseExpression(.none);

    if (!try self.expectNext(.@")")) {
        return error.FailedToParse;
    }

    if (!try self.expectNext(.@"{")) {
        return error.FailedToParse;
    }

    const loop_body = try self.parseBlockStatement();

    self.nodes.set(node_index, .{
        .type = .while_loop,
        .lhs = condition,
        .rhs = loop_body,
        .token_index = self.token_index,
    });

    return @intCast(node_index);
}

fn parseAssignmentStatement(self: *Parser) !u32 {
    const node_index = try self.nodes.addOne(self.gpa);
    const indentifier = try self.parseIdentifier();

    if (!try self.expectNext(.assign)) {
        return error.FailedToParse;
    }

    try self.advanceTokenStream();

    const expression = try self.parseExpression(.none);

    self.nodes.set(node_index, .{
        .type = .assignment_statement,
        .rhs = expression,
        .lhs = indentifier,
        .token_index = self.token_index,
    });

    if (!try self.expectNext(.@";")) {
        return error.FailedToParse;
    }

    return @intCast(node_index);
}

fn parseIfStatement(self: *Parser) !u32 {
    const node_index = try self.nodes.addOne(self.gpa);

    if (!try self.expectNext(.@"(")) {
        return error.FailedToParse;
    }
    try self.advanceTokenStream();

    const condition = try self.parseExpression(.none);

    if (!try self.expectNext(.@")")) {
        return error.FailedToParse;
    }

    if (!try self.expectNext(.@"{")) {
        return error.FailedToParse;
    }

    const if_body = try self.parseBlockStatement();

    const maybe_else_body = if (self.next_token.type == .@"else") blk: {
        try self.advanceTokenStream();
        try self.advanceTokenStream();
        break :blk try self.parseBlockStatement();
    } else null;

    if (maybe_else_body) |else_body| {
        const extra_data_index = self.extra.items.len;
        try self.extra.append(self.gpa, if_body);
        try self.extra.append(self.gpa, else_body);

        self.nodes.set(node_index, .{
            .type = .if_else_statement,
            .token_index = self.token_index,
            .lhs = condition,
            .rhs = @intCast(extra_data_index),
        });
    } else {
        self.nodes.set(node_index, .{
            .type = .if_statement,
            .token_index = self.token_index,
            .lhs = condition,
            .rhs = if_body,
        });
    }

    return @intCast(node_index);
}

fn parseBlockStatement(self: *Parser) !u32 {
    const node_index = try self.nodes.addOne(self.gpa);

    // Indices of the root's children
    var children_indices = std.ArrayListUnmanaged(u32){};
    defer children_indices.deinit(self.gpa);

    try self.advanceTokenStream();

    // Parse statements
    while (self.curr_token.type != .@"}" and self.curr_token.type != .eof) {
        if (self.parseStatement()) |index| {
            try children_indices.append(self.gpa, index);
        } else |err| {
            if (err != error.FailedToParse) {
                return err;
            }
        }

        try self.advanceTokenStream();
    }

    // Append root children indices to extra.
    const child_indices_start = self.extra.items.len;
    try self.extra.appendSlice(self.gpa, children_indices.items);

    self.nodes.set(node_index, .{
        .type = .block,
        .lhs = @intCast(children_indices.items.len),
        .rhs = @intCast(child_indices_start),
        .token_index = 0,
    });

    return @intCast(node_index);
}

fn parseReturnStatement(self: *Parser) !u32 {
    const node_index = try self.nodes.addOne(self.gpa);
    try self.advanceTokenStream();

    const expression = try self.parseExpression(.none);

    self.nodes.set(node_index, .{
        .type = .return_statement,
        .lhs = expression,
        .rhs = undefined,
        .token_index = self.token_index,
    });

    if (self.next_token.type == .@";") {
        try self.advanceTokenStream();
    }

    return @intCast(node_index);
}

fn parseVarStatement(self: *Parser) !u32 {
    if (!try self.expectNext(.identifier)) {
        return error.FailedToParse;
    }

    const node_index = try self.nodes.addOne(self.gpa);
    const identifier = try self.parseIdentifier();

    if (!try self.expectNext(.assign)) {
        return error.FailedToParse;
    }

    try self.advanceTokenStream();
    const expression = try self.parseExpression(.none);

    self.nodes.set(node_index, .{
        .type = .var_statement,
        .lhs = identifier,
        .rhs = expression,
        .token_index = self.token_index,
    });

    if (!try self.expectNext(.@";")) {
        return error.FailedToParse;
    }

    return @intCast(node_index);
}

fn parseExpressionStatement(self: *Parser) !u32 {
    const node_index = try self.nodes.addOne(self.gpa);
    const token_index = self.token_index;

    const child = try self.parseExpression(.none);

    if (!try self.expectNext(.@";")) {
        return error.FailedToParse;
    }
    try self.advanceTokenStream();

    self.nodes.set(node_index, .{
        .token_index = token_index,
        .lhs = child,
        .rhs = undefined,
        .type = .expression_statement,
    });

    return @intCast(node_index);
}

fn parseExpression(self: *Parser, precedence: Precedence) anyerror!u32 {
    var lhs = switch (self.curr_token.type) {
        .identifier => try self.parseIdentifier(),
        .int_literal => try self.parseIntLiteral(),
        .true, .false => try self.parseBoolLiteral(),
        .@"-", .@"!" => try self.parseUnaryExpression(),
        .@"(" => try self.parseGroupExpression(),
        else => {
            try self.errors.append(self.gpa, .{
                .type = .invalid_expression,
                .token = self.curr_token,
            });
            return error.FailedToParse;
        },
    };

    while (self.next_token.type != .@";" and @intFromEnum(precedence) < @intFromEnum(self.nextPrecedence())) {
        lhs = switch (self.next_token.type) {
            .@"+",
            .@"-",
            .@"*",
            .@"/",
            .@"==",
            .@"!=",
            .@"<",
            .@">",
            => blk: {
                try self.advanceTokenStream();
                break :blk try self.parseBinaryExpression(lhs);
            },
            else => lhs,
        };
    }

    return lhs;
}

fn parseGroupExpression(self: *Parser) !u32 {
    try self.advanceTokenStream();

    const expression = try self.parseExpression(.none);

    if (!try self.expectNext(.@")")) {
        return error.FailedToParse;
    }

    return expression;
}

fn parseBinaryExpression(self: *Parser, lhs: u32) !u32 {
    const index = try self.nodes.addOne(self.gpa);
    const token = self.curr_token.type;

    const precedence = self.currentPrecedence();
    try self.advanceTokenStream();
    const rhs = try self.parseExpression(precedence);

    self.nodes.set(index, .{
        .type = switch (token) {
            .@"+" => .add,
            .@"-" => .sub,
            .@"*" => .mul,
            .@"/" => .div,
            .@"==" => .equal,
            .@"!=" => .not_equal,
            .@"<" => .less_than,
            .@">" => .greater_than,
            else => unreachable,
        },
        .rhs = rhs,
        .lhs = lhs,
        .token_index = self.token_index,
    });

    return @intCast(index);
}

fn parseUnaryExpression(self: *Parser) !u32 {
    const index = try self.nodes.addOne(self.gpa);
    const token = self.curr_token.type;

    try self.advanceTokenStream();

    const rhs = try self.parseExpression(.prefix);

    self.nodes.set(index, .{
        .type = switch (token) {
            .@"-" => .negate,
            .@"!" => .not,
            else => unreachable,
        },
        .rhs = rhs,
        .lhs = undefined,
        .token_index = self.token_index,
    });

    return @intCast(index);
}

fn parseBoolLiteral(self: *Parser) !u32 {
    const index = try self.nodes.addOne(self.gpa);
    self.nodes.set(index, .{
        .type = .bool_literal,
        .rhs = undefined,
        .lhs = undefined,
        .token_index = self.token_index,
    });

    return @intCast(index);
}

fn parseIntLiteral(self: *Parser) !u32 {
    const index = try self.nodes.addOne(self.gpa);
    self.nodes.set(index, .{
        .type = .int_literal,
        .rhs = undefined,
        .lhs = undefined,
        .token_index = self.token_index,
    });

    return @intCast(index);
}

fn parseIdentifier(self: *Parser) !u32 {
    const index = try self.nodes.addOne(self.gpa);
    self.nodes.set(index, .{
        .type = .identifier,
        .rhs = undefined,
        .lhs = undefined,
        .token_index = self.token_index,
    });

    return @intCast(index);
}

fn currentPrecedence(self: *Parser) Precedence {
    return switch (self.curr_token.type) {
        .@"==", .@"!=" => .equals,
        .@"<", .@">" => .greater_less,
        .@"+", .@"-" => .additive,
        .@"*", .@"/" => .multiplicitive,
        else => .none,
    };
}

fn nextPrecedence(self: *Parser) Precedence {
    return switch (self.next_token.type) {
        .@"==", .@"!=" => .equals,
        .@"<", .@">" => .greater_less,
        .@"+", .@"-" => .additive,
        .@"*", .@"/" => .multiplicitive,
        else => .none,
    };
}

fn expectNext(self: *Parser, t: Token.Type) !bool {
    if (self.next_token.type == t) {
        try self.advanceTokenStream();
        return true;
    }

    try self.errors.append(self.gpa, .{
        .token = self.next_token,
        .type = .unexpected_token,
        .expected_token_type = t,
    });

    return false;
}

const Precedence = enum {
    none,
    equals,
    greater_less,
    additive,
    multiplicitive,
    prefix,
    call,
};

const ParseError = struct {
    type: Type,
    token: Token,
    expected_token_type: ?Token.Type = null,

    pub fn print(self: ParseError, writer: anytype, src: []const u8) !void {

        // #PERF: This is horrible.
        const line_number = ln: {
            var ln: usize = 1;
            var idx: usize = std.mem.indexOfScalar(u8, src, '\n') orelse break :ln ln;

            while (idx < self.token.start) : (ln += 1) {
                idx += std.mem.indexOfScalar(u8, src[idx + 1 ..], '\n') orelse break;
                idx += 1;
            }

            break :ln ln;
        };

        const line_end = self.token.start + (std.mem.indexOfScalar(u8, src[self.token.start..], '\n') orelse
            self.token.end - self.token.start - 1);

        const line_start = blk: {
            var i: usize = self.token.start;
            while (i > 0) {
                i -= 1;
                if (src[i] == '\n') break :blk i + 1;
            }

            break :blk 0;
        };
        const source_line = src[line_start..line_end];

        switch (self.type) {
            .unexpected_token => {
                try writer.print("{d}: Expected token {s} but got {s}\n", .{
                    line_number,
                    @tagName(self.expected_token_type.?),
                    @tagName(self.token.type),
                });

                try writer.print("{s}\n", .{source_line});
            },
            .invalid_token => {
                try writer.print("{d}: Invalid token {s}\n", .{
                    line_number,
                    src[self.token.start..self.token.end],
                });

                try writer.print("{s}\n", .{source_line});
            },
            .invalid_expression => {
                try writer.print("{d}: Invalid expression, got token {s}\n", .{
                    line_number,
                    @tagName(self.token.type),
                });

                try writer.print("{s}\n", .{source_line});
            },
        }
    }

    const Type = enum {
        unexpected_token,
        invalid_token,
        invalid_expression,
    };
};
