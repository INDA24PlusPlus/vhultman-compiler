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
}

pub fn resolve(self: *Sema) !void {
    try self.globalScopePass();
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
        }

        std.debug.print("fn ident: {s}\n", .{bytes});
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
    kind: Kind,
    token: Token,
    other_tok: Token,

    pub const Kind = enum {
        fn_redecl,
    };
};
