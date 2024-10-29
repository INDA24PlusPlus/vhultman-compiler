const std = @import("std");
const Allocator = std.mem.Allocator;
const Ast = @import("Ast.zig");
const Sema = @This();
const Token = @import("Tokenizer.zig").Token;

ast: *Ast,
gpa: Allocator,
sym_table: SymbolTable,
errors: std.ArrayListUnmanaged(SemaError),

pub fn init(gpa: Allocator, ast: *Ast) !Sema {
    return .{
        .gpa = gpa,
        .ast = ast,
        .sym_table = try SymbolTable.init(gpa),
        .errors = std.ArrayListUnmanaged(SemaError){},
    };
}

pub fn deinit(self: *Sema) void {
    self.sym_table.deinit(self.gpa);
    self.errors.deinit(self.gpa);
}

pub fn resolve(self: *Sema) !void {
    try self.globalScopePass();

    const root = self.ast.nodes.get(0);
    const fn_decls = self.ast.extra.items[root.rhs .. root.rhs + root.lhs];
    for (fn_decls) |fn_decl| {
        const fn_node = self.ast.nodes.get(fn_decl);
        try self.resolveFunction(&fn_node);
    }

    if (self.errors.items.len != 0) {
        return error.HadSemaError;
    }
}

fn srcBytes(self: *const Sema, node: u32) []const u8 {
    const token_index = self.ast.nodes.items(.token)[node];
    const token = self.ast.tokens.get(token_index);
    return self.ast.src[token.start..token.end];
}

fn resolveFunction(self: *Sema, node: *const Ast.Node) !void {
    const block_index = self.ast.extra.items[node.rhs + node.lhs - 1];
    const block_node = self.ast.nodes.get(block_index);
    try self.resolveBlock(&block_node);
}

fn resolveBlock(self: *Sema, node: *const Ast.Node) !void {
    const children = self.ast.extra.items[node.rhs .. node.rhs + node.lhs];
    for (children) |stmt| {
        try self.resolveStatement(&self.ast.nodes.get(stmt));
    }
}

fn resolveStatement(self: *Sema, node: *const Ast.Node) !void {
    switch (node.kind) {
        .var_decl, .const_decl => try self.resolveVarDecl(node),
        else => unreachable,
    }
}

fn resolveVarDecl(self: *Sema, node: *const Ast.Node) !void {
    const is_const = node.kind == .const_decl;
    const type_specifier = self.ast.nodes.get(node.lhs);
    const expr = self.ast.nodes.get(node.rhs);

    const ident_token = self.ast.nodes.items(.token)[type_specifier.lhs];
    const bytes = self.srcBytes(type_specifier.lhs);

    if (self.sym_table.get(bytes)) |sym| {
        try self.errors.append(self.gpa, .{
            .kind = .var_redecl,
            .token = self.ast.tokens.get(ident_token),
            .other_tok = self.ast.tokens.get(sym.token),
        });
    } else {
        try self.sym_table.put(self.gpa, .{ .token = ident_token, .type = 0 }, bytes);
    }
    try self.resolveExpression(&expr);

    std.debug.print("is const? {}: {s}\n", .{ is_const, bytes });
}

fn resolveExpression(self: *Sema, node: *const Ast.Node) !void {
    if (node.isBinOp()) {
        try self.resolveExpression(&self.ast.nodes.get(node.lhs));
        try self.resolveExpression(&self.ast.nodes.get(node.rhs));
    } else if (node.isUnaryOp()) {
        try self.resolveExpression(&self.ast.nodes.get(node.rhs));
    } else {
        switch (node.kind) {
            .identifier => {
                const ident_token = self.ast.tokens.get(node.token);
                const bytes = self.ast.src[ident_token.start..ident_token.end];
                if (self.sym_table.get(bytes) == null) {
                    try self.errors.append(self.gpa, .{
                        .kind = .use_of_undecl,
                        .token = ident_token,
                    });
                }
            },
            .fn_call => {
                const ident_node = self.ast.nodes.get(self.ast.extra.items[node.rhs]);
                const ident_token = self.ast.tokens.get(ident_node.token);
                const bytes = self.ast.src[ident_token.start..ident_token.end];
                if (self.sym_table.get(bytes) == null) {
                    try self.errors.append(self.gpa, .{
                        .kind = .use_of_undecl,
                        .token = ident_token,
                    });
                }
            },
            .int_literal => {},
            else => unreachable,
        }
    }
}

const Primitive = enum(u32) {
    void,
    u8,
    u16,
    u32,
    u64,
    i8,
    i16,
    i32,
    i64,
    f32,
    f64,
    bool,

    pub fn isNumeric(self: Primitive) bool {
        return switch (self) {
            .u8,
            .u16,
            .u32,
            .u64,
            .i8,
            .i16,
            .i32,
            .i64,
            .f32,
            .f64,
            => true,
            else => false,
        };
    }
};

fn typeCheckExpression(self: *Sema, node: *const Ast.Node) !u32 {
    switch (node.kind) {
        // Int binary ops.
        .add,
        .sub,
        .mul,
        .div,
        .less_than,
        .greater_than,
        => {
            const lhs_node = self.ast.nodes.get(node.lhs);
            const rhs_node = self.ast.nodes.get(node.rhs);
            const lhs_type = try self.typeCheckExpression(&lhs_node);
            const rhs_type = try self.typeCheckExpression(&rhs_node);

            if (lhs_type != rhs_type) {}
        },
    }
}

fn globalScopePass(self: *Sema) !void {
    const root = self.ast.nodes.get(0);
    const fn_decls = self.ast.extra.items[root.rhs .. root.rhs + root.lhs];

    for (fn_decls) |decl| {
        const node = self.ast.nodes.get(decl);
        const identifier = self.ast.nodes.get(self.ast.extra.items[node.rhs]);
        const tok = self.ast.tokens.get(identifier.token);
        const bytes = self.ast.src[tok.start..tok.end];

        if (self.sym_table.get(bytes)) |sym| {
            try self.errors.append(self.gpa, .{
                .kind = .fn_redecl,
                .token = tok,
                .other_tok = self.ast.tokens.get(sym.token),
            });
        } else {
            try self.sym_table.put(self.gpa, .{ .token = identifier.token, .type = 0 }, bytes);
        }
    }
}

const Symbol = struct {
    token: u32,
    type: u32,
};

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

pub const SemaError = struct {
    // Left of here: fix.
    kind: Kind,
    token: Token,
    other_tok: ?Token = null,

    type1: u32 = 0,
    type2: u32 = 0,

    pub const Kind = enum {
        var_redecl,
        fn_redecl,
        use_of_undecl,
        mismatched_types,
    };

    pub fn print(self: SemaError, writer: anytype, src: []const u8, file_name: []const u8) !void {
        const red = "\x1B[31m";
        const reset = "\x1B[0m";
        const error_line = self.token.computeLineInfo(src);

        try writer.print("{s}:{d}:{d}: ", .{
            file_name,
            error_line.line_number,
            error_line.column_number,
        });

        var print_source_line = self.token.kind != .eof;

        try writer.print(red ++ "error: " ++ reset, .{});
        switch (self.kind) {
            .fn_redecl => {
                try writer.print("Redeclaration of function \"{s}\"\n", .{src[self.token.start..self.token.end]});
            },
            .var_redecl => {
                try writer.print("Redeclaration of \"{s}\"\n", .{src[self.token.start..self.token.end]});
            },
            .use_of_undecl => {
                try writer.print("Use of undeclared identifier \"{s}\"\n", .{src[self.token.start..self.token.end]});
            },
            .mismatched_types => {
                print_source_line = false;
                try writer.print("Mismatched types\n", .{src[self.token.start..self.token.end]});
            },
        }

        if (print_source_line) {
            const offset = @intFromPtr(error_line.line.ptr) - @intFromPtr(src.ptr);
            try printHightlight(writer, error_line.line, .{ self.token.start - offset, self.token.end - offset });
        }
    }
};

fn printHightlight(writer: anytype, text: []const u8, highlight_range: [2]usize) !void {
    const red = "\x1B[31m";
    const green = "\x1B[32m";
    const reset = "\x1B[0m";

    try writer.print("{s}" ++ red ++ "{s}" ++ reset ++ "{s}\n", .{
        text[0..highlight_range[0]],
        text[highlight_range[0]..highlight_range[1]],
        text[highlight_range[1]..text.len],
    });

    try writer.writeByteNTimes(' ', highlight_range[0]);
    try writer.print(green ++ "^", .{});
    try writer.writeByteNTimes('~', highlight_range[1] - highlight_range[0] - 1);
    try writer.print(reset ++ "\n", .{});
}
