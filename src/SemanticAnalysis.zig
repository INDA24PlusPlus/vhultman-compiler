const std = @import("std");
const Ast = @import("Ast.zig");
const SemanticAnalysis = @This();
const Allocator = std.mem.Allocator;
const Token = @import("Tokenizer.zig").Token;

const Type = enum { int, bool };

const Symbol = struct {
    token_index: u32,
    type: Type,
};

// TODO: This can be done better.
const SymbolTable = struct {
    scopes: std.ArrayListUnmanaged(std.StringHashMapUnmanaged(Symbol)),

    pub fn init(allocator: Allocator) !SymbolTable {
        var scopes = try std.ArrayListUnmanaged(std.StringHashMapUnmanaged(Symbol)).initCapacity(allocator, 5);
        try scopes.append(allocator, std.StringHashMapUnmanaged(Symbol){});

        return .{ .scopes = scopes };
    }

    pub fn deinit(self: *SymbolTable, allocator: Allocator) void {
        for (self.scopes.items) |*map| {
            map.deinit(allocator);
        }

        self.scopes.deinit(allocator);
    }

    pub fn getCurrent(self: *const SymbolTable, name: []const u8) ?Symbol {
        return self.scopes.items[self.scopes.items.len - 1].get(name);
    }

    pub fn get(self: *const SymbolTable, name: []const u8) ?Symbol {
        return for (self.scopes.items) |*map| {
            if (map.get(name)) |symbol| {
                break symbol;
            }
        } else null;
    }

    pub fn put(self: *SymbolTable, allocator: Allocator, symbol: Symbol, name: []const u8) !void {
        try self.scopes.items[self.scopes.items.len - 1].put(allocator, name, symbol);
    }

    pub fn enterScope(self: *SymbolTable, allocator: Allocator) !void {
        try self.scopes.append(allocator, .{});
    }

    pub fn exitScope(self: *SymbolTable, allocator: Allocator) void {
        var map = self.scopes.pop();
        map.deinit(allocator);
    }
};

ast: *Ast,
sym_table: SymbolTable,
tokens: []const Token,
src: []const u8,
gpa: Allocator,
errors: std.ArrayListUnmanaged(SemaError),

pub fn init(allocator: Allocator, tokens: []const Token, src: []const u8, ast: *Ast) !SemanticAnalysis {
    return .{
        .ast = ast,
        .sym_table = try SymbolTable.init(allocator),
        .tokens = tokens,
        .src = src,
        .gpa = allocator,
        .errors = std.ArrayListUnmanaged(SemaError){},
    };
}

pub fn deinit(self: *SemanticAnalysis) void {
    self.sym_table.deinit(self.gpa);
    self.errors.deinit(self.gpa);
}

pub fn resolveProgram(self: *SemanticAnalysis) !void {
    // Node 0 is always root node.
    const root_node = self.ast.nodes.get(0);
    const children = self.ast.extra[root_node.rhs .. root_node.rhs + root_node.lhs];
    for (children) |child| {
        const child_node = self.ast.nodes.get(child);
        try self.resolveStatement(&child_node);
    }

    if (self.errors.items.len != 0) {
        return error.ResolveError;
    }
}

pub fn resolveStatement(self: *SemanticAnalysis, node: *const Ast.Node) anyerror!void {
    switch (node.type) {
        .var_statement => try self.resolveVarStatement(node),
        .assignment_statement => try self.resolveAssignStatement(node),
        .block => try self.resolveBlock(node),
        // While loop and if statement have the same semantics.
        .if_statement, .while_loop => try self.resolveIfStatement(node),
        .if_else_statement => try self.resolveIfElseStatement(node),
        else => unreachable,
    }
}

fn resolveIfElseStatement(self: *SemanticAnalysis, node: *const Ast.Node) !void {
    const condition_expr = self.ast.nodes.get(node.lhs);
    const block = self.ast.nodes.get(self.ast.extra[node.rhs + 0]);
    const else_block = self.ast.nodes.get(self.ast.extra[node.rhs + 1]);

    try self.resolveExpression(&condition_expr);
    try self.resolveBlock(&block);
    try self.resolveBlock(&else_block);
}

fn resolveIfStatement(self: *SemanticAnalysis, node: *const Ast.Node) !void {
    const condition_expr = self.ast.nodes.get(node.lhs);
    const block = self.ast.nodes.get(node.rhs);

    try self.resolveExpression(&condition_expr);
    try self.resolveBlock(&block);
}

fn resolveBlock(self: *SemanticAnalysis, node: *const Ast.Node) !void {
    try self.sym_table.enterScope(self.gpa);
    const children = self.ast.extra[node.rhs .. node.rhs + node.lhs];

    for (children) |child| {
        try self.resolveStatement(&self.ast.nodes.get(child));
    }
    self.sym_table.exitScope(self.gpa);
}

fn resolveAssignStatement(self: *SemanticAnalysis, node: *const Ast.Node) !void {
    const ident_node = self.ast.nodes.get(node.lhs);
    const ident_bytes = self.getIdentBytes(ident_node.token_index);

    if (self.sym_table.get(ident_bytes) == null) {
        try self.errors.append(
            self.gpa,
            .{ .use_of_undeclared = .{ .identifier_token = ident_node.token_index } },
        );
    }

    const expr_node = self.ast.nodes.get(node.rhs);
    try self.resolveExpression(&expr_node);
}

fn resolveVarStatement(self: *SemanticAnalysis, node: *const Ast.Node) !void {
    const ident_node = self.ast.nodes.get(node.lhs);
    const ident_bytes = self.getIdentBytes(ident_node.token_index);

    if (self.sym_table.get(ident_bytes)) |*symbol| {
        try self.errors.append(self.gpa, .{ .redecl = .{
            .first_decl = symbol.token_index,
            .redecl = ident_node.token_index,
        } });
    } else {
        const sym: Symbol = .{
            .token_index = ident_node.token_index,
            .type = .int,
        };

        try self.sym_table.put(self.gpa, sym, ident_bytes);

        const expr_node = self.ast.nodes.get(node.rhs);
        try self.resolveExpression(&expr_node);
    }
}

fn resolveExpression(self: *SemanticAnalysis, node: *const Ast.Node) !void {
    // This needs to be refactored (probably a some kind of ChildIterator for the Ast)
    switch (node.type) {
        .identifier => {
            const ident_bytes = self.getIdentBytes(node.token_index);
            if (self.sym_table.get(ident_bytes) == null) {
                try self.errors.append(
                    self.gpa,
                    .{ .use_of_undeclared = .{ .identifier_token = node.token_index } },
                );
            }
        },
        .add,
        .sub,
        .mul,
        .div,
        .equal,
        .not_equal,
        .less_than,
        .greater_than,
        => {
            try self.resolveExpression(&self.ast.nodes.get(node.lhs));
            try self.resolveExpression(&self.ast.nodes.get(node.rhs));
        },
        .negate, .not => try self.resolveExpression(&self.ast.nodes.get(node.rhs)),

        .int_literal, .bool_literal => {},
        else => unreachable,
    }
}

fn getIdentBytes(self: *const SemanticAnalysis, token_index: u32) []const u8 {
    return self.src[self.tokens[token_index].start..self.tokens[token_index].end];
}

const SemaError = union(enum) {
    redecl: struct {
        first_decl: u32,
        redecl: u32,
    },
    use_of_undeclared: struct {
        identifier_token: u32,
    },

    pub fn print(self: SemaError, writer: anytype, src: []const u8, tokens: []const Token) !void {
        switch (self) {
            .redecl => |info| {
                const ident_token = tokens[info.redecl];
                try writer.print("Error: Redecleration of \"{s}\"\n", .{
                    src[ident_token.start..ident_token.end],
                });
            },
            .use_of_undeclared => |info| {
                const ident_token = tokens[info.identifier_token];
                try writer.print("Error: Use of undeclared identifier \"{s}\"\n", .{
                    src[ident_token.start..ident_token.end],
                });
            },
        }
    }
};
