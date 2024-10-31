const std = @import("std");
const Allocator = std.mem.Allocator;
const Ast = @import("Ast.zig");
const Sema = @This();
const Token = @import("Tokenizer.zig").Token;

const log = std.log.scoped(.sema);

ast: *Ast,
gpa: Allocator,
sym_table: SymbolTable,
type_reg: TypeRegistry,
errors: std.ArrayListUnmanaged(SemaError),

pub fn init(gpa: Allocator, ast: *Ast) !Sema {
    return .{
        .gpa = gpa,
        .ast = ast,
        .sym_table = try SymbolTable.init(gpa),
        .type_reg = TypeRegistry.init(),
        .errors = std.ArrayListUnmanaged(SemaError){},
    };
}

pub fn deinit(self: *Sema) void {
    self.sym_table.deinit(self.gpa);
    self.errors.deinit(self.gpa);
    self.type_reg.deinit(self.gpa);
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

fn srcBytesNode(self: *const Sema, node: *const Ast.Node) []const u8 {
    const token = self.ast.tokens.get(node.token);
    return self.ast.src[token.start..token.end];
}

fn resolveFunction(self: *Sema, node: *const Ast.Node) !void {
    const block_index = self.ast.extra.items[node.rhs + node.lhs - 1];
    const block_node = self.ast.nodes.get(block_index);

    try self.sym_table.enterScope(self.gpa);

    const fn_ident_node = self.ast.extra.items[node.rhs];
    const fn_identifier = self.srcBytes(fn_ident_node);

    const fn_header = self.type_reg.functions.get(fn_identifier).?;
    const arg_nodes = self.ast.extra.items[node.rhs + 1 .. node.rhs + node.lhs - 2];

    for (arg_nodes, fn_header.args) |node_index, arg_type| {
        const arg_node = self.ast.nodes.get(node_index);
        const bytes = self.srcBytes(arg_node.lhs);
        try self.sym_table.put(self.gpa, .{ .type = arg_type, .token = arg_node.lhs, .is_const = true }, bytes);
    }

    const has_return = try self.resolveBlock(&block_node, fn_header.return_type);
    if (fn_header.return_type != .void and !has_return) {
        try self.errors.append(self.gpa, .{
            .kind = .missing_return,
            .other_tok = self.ast.tokens.get(self.ast.nodes.items(.token)[fn_ident_node]),
            .token = self.ast.tokens.get(self.ast.nodes.items(.token)[self.ast.extra.items[node.rhs + node.lhs - 2]]),
            .type1 = fn_header.return_type,
        });
    }

    self.sym_table.exitScope(self.gpa);
}

fn resolveBlock(self: *Sema, node: *const Ast.Node, return_type: TypeHandle) anyerror!bool {
    const children = self.ast.extra.items[node.rhs .. node.rhs + node.lhs];
    var has_return = false;
    for (children) |stmt| {
        has_return = has_return or try self.resolveStatement(&self.ast.nodes.get(stmt), return_type);
    }

    return has_return;
}

fn resolveStatement(self: *Sema, node: *const Ast.Node, return_type: TypeHandle) !bool {
    switch (node.kind) {
        .var_decl, .const_decl => {
            try self.resolveVarDecl(node);
            return false;
        },
        .assignment => {
            try self.resolveAssignment(node);
            return false;
        },
        .block => {
            return try self.resolveBlock(node, return_type);
        },
        .if_statement => {
            // TODO: fix this return resolution shit. Ideally I don't want to do a CFG, but it is lookg more likely I have to.
            return try self.resolveIf(node, return_type);
        },
        .while_loop => {
            try self.resolveWhile(node, return_type);
            return false;
        },
        .print_statement => {
            try self.resolvePrint(node);
            return false;
        },
        .return_statement => {
            try self.resolveReturn(node, return_type);
            return true;
        },
        else => std.debug.panic("Semantic analysis not supported for {}", .{node.kind}),
    }
}

fn resolvePrint(self: *Sema, node: *const Ast.Node) !void {
    const identifier = self.ast.nodes.get(node.lhs);
    const identifier_type = try self.typeCheckExpression(&identifier);
    self.ast.nodes.items(.lhs)[node.lhs] = @intFromEnum(identifier_type);
}

fn resolveWhile(self: *Sema, node: *const Ast.Node, return_type: TypeHandle) anyerror!void {
    const cond = self.ast.nodes.get(node.lhs);
    const body = self.ast.nodes.get(node.rhs);

    const cond_type = try self.typeCheckExpression(&cond);
    if (!cond_type.coercesTo(.bool)) {
        try self.errors.append(self.gpa, .{
            .kind = .expected_type,
            .token = self.ast.tokens.get(node.token),
            .type1 = .bool,
            .type2 = cond_type,
        });
    }
    _ = try self.resolveStatement(&body, return_type);
}

fn resolveIf(self: *Sema, node: *const Ast.Node, return_type: TypeHandle) anyerror!bool {
    const cond = self.ast.nodes.get(self.ast.extra.items[node.rhs]);
    const body = self.ast.nodes.get(self.ast.extra.items[node.rhs + 1]);

    const cond_type = try self.typeCheckExpression(&cond);
    if (!cond_type.coercesTo(.bool)) {
        try self.errors.append(self.gpa, .{
            .kind = .expected_type,
            .token = self.ast.tokens.get(node.token),
            .type1 = .bool,
            .type2 = cond_type,
        });
    }

    _ = try self.resolveStatement(&body, return_type);
    if (node.lhs == 3) {
        const else_body = self.ast.nodes.get(self.ast.extra.items[node.rhs + 2]);
        return try self.resolveStatement(&else_body, return_type);
    }

    return false;
}

fn resolveAssignment(self: *Sema, node: *const Ast.Node) !void {
    const identifier = self.ast.nodes.get(node.lhs);
    const ident_token = self.ast.tokens.get(identifier.token);
    const expression = self.ast.nodes.get(node.rhs);

    if (self.sym_table.get(identifier.srcBytes(self.ast))) |sym| {
        if (sym.is_const) {
            try self.errors.append(self.gpa, .{
                .kind = .reassign_of_const,
                .token = ident_token,
            });

            return;
        }

        const expr_type = try self.typeCheckExpression(&expression);
        if (!expr_type.coercesTo(sym.type)) {
            try self.errors.append(self.gpa, .{
                .kind = .mismatched_assign_type,
                .token = ident_token,
                .type1 = sym.type,
                .type2 = expr_type,
            });
        }
    } else {
        try self.errors.append(self.gpa, .{
            .kind = .use_of_undecl_ident,
            .token = ident_token,
        });
    }
}

fn resolveReturn(self: *Sema, node: *const Ast.Node, return_type: TypeHandle) !void {
    const expr = self.ast.nodes.get(node.lhs);
    const expr_type = try self.typeCheckExpression(&expr);
    if (!expr_type.coercesTo(return_type)) {
        try self.errors.append(self.gpa, .{
            .kind = .wrong_return_type,
            .token = self.ast.tokens.get(expr.token),
            .type1 = return_type,
            .type2 = expr_type,
        });
    }
}

fn resolveVarDecl(self: *Sema, node: *const Ast.Node) !void {
    const is_const = node.kind == .const_decl;
    const type_specifier = self.ast.nodes.get(node.lhs);
    const expr = self.ast.nodes.get(node.rhs);

    const ident_token = self.ast.nodes.items(.token)[type_specifier.lhs];
    const bytes = self.srcBytes(type_specifier.lhs);

    const specified_type = TypeHandle.primtives_map.get(self.srcBytes(type_specifier.rhs)) orelse @panic("User specified types not supported yet");
    const expr_type = try self.typeCheckExpression(&expr);

    // TODO: fix floats
    if (expr_type == .int_literal and specified_type != .f32 and specified_type != .f64) {
        const value = std.fmt.parseUnsigned(u64, expr.srcBytes(self.ast), 10) catch |err| switch (err) {
            error.Overflow => blk: {
                try self.errors.append(self.gpa, .{
                    .kind = .int_too_big,
                    .token = self.ast.tokens.get(expr.token),
                    .type1 = specified_type,
                });

                break :blk 0;
            },
            else => unreachable,
        };

        const shift: u7 = switch (specified_type) {
            .u8 => 8,
            .u16 => 16,
            .u32 => 32,
            .u64 => 64,
            .i8 => 7,
            .i16 => 15,
            .i32 => 31,
            .i64 => 63,
            else => unreachable,
        };

        if (value >= (@as(u128, 1) << shift)) {
            try self.errors.append(self.gpa, .{
                .kind = .int_too_big,
                .token = self.ast.tokens.get(expr.token),
                .type1 = specified_type,
            });
        }
    }

    if (!expr_type.coercesTo(specified_type)) {
        try self.errors.append(self.gpa, .{
            .kind = .mismatched_specifier_type,
            .token = self.ast.tokens.get(self.ast.nodes.get(type_specifier.rhs).token),
            .type1 = specified_type,
            .type2 = expr_type,
        });
    }

    if (self.sym_table.get(bytes)) |sym| {
        try self.errors.append(self.gpa, .{
            .kind = .var_redecl,
            .token = self.ast.tokens.get(ident_token),
            .other_tok = self.ast.tokens.get(sym.token),
        });
    } else {
        try self.sym_table.put(self.gpa, .{
            .token = ident_token,
            .type = specified_type,
            .is_const = is_const,
        }, bytes);
    }
}

fn typeCheckExpression(self: *Sema, node: *const Ast.Node) !TypeHandle {
    switch (node.kind) {
        .add,
        .sub,
        .mul,
        .div,
        => {
            const lhs_node = self.ast.nodes.get(node.lhs);
            const rhs_node = self.ast.nodes.get(node.rhs);
            const lhs_type = try self.typeCheckExpression(&lhs_node);
            const rhs_type = try self.typeCheckExpression(&rhs_node);

            const largest_type = if (lhs_type.coercesTo(rhs_type)) rhs_type else if (rhs_type.coercesTo(lhs_type)) lhs_type else blk: {
                try self.errors.append(self.gpa, .{
                    .kind = .invalid_types_for_op,
                    .token = self.ast.tokens.get(lhs_node.token),
                    .other_tok = self.ast.tokens.get(lhs_node.token),
                    .type1 = lhs_type,
                    .type2 = rhs_type,
                });

                break :blk .undefined;
            };
            if (!largest_type.isNumeric()) {
                try self.errors.append(self.gpa, .{
                    .kind = .expected_numeric_type,
                    .token = self.ast.tokens.get(lhs_node.token),
                    .other_tok = self.ast.tokens.get(lhs_node.token),
                    .type2 = lhs_type,
                });
            }

            return largest_type;
        },
        .int_literal => return .int_literal,
        .bool_literal => return .bool,
        .identifier => {
            const ident_token = self.ast.tokens.get(node.token);
            const bytes = self.ast.src[ident_token.start..ident_token.end];
            if (self.sym_table.get(bytes)) |sym| {
                return sym.type;
            } else {
                try self.errors.append(self.gpa, .{
                    .kind = .use_of_undecl_ident,
                    .token = ident_token,
                });

                return .undefined;
            }
        },
        .fn_call => {
            const fn_name = self.ast.extra.items[node.rhs];
            const name_bytes = self.srcBytes(fn_name);

            const info = self.type_reg.functions.get(name_bytes) orelse {
                try self.errors.append(self.gpa, .{
                    .kind = .use_of_undecl_fn,
                    .token = self.ast.tokens.get(self.ast.nodes.items(.token)[fn_name]),
                });
                return .undefined;
            };
            const arg_nodes = self.ast.extra.items[node.rhs + 1 .. node.rhs + node.lhs];
            if (arg_nodes.len != info.args.len) {
                try self.errors.append(self.gpa, .{
                    .kind = .wrong_arg_count,
                    .token = self.ast.tokens.get(self.ast.nodes.items(.token)[fn_name]),
                    .type1 = @enumFromInt(info.args.len),
                    .type2 = @enumFromInt(arg_nodes.len),
                });
                return info.return_type;
            }

            for (arg_nodes, info.args) |arg_node_index, parameter_type| {
                const arg_node = self.ast.nodes.get(arg_node_index);
                const arg_expr_type = try self.typeCheckExpression(&arg_node);
                if (!arg_expr_type.coercesTo(parameter_type)) {
                    try self.errors.append(self.gpa, .{
                        .kind = .mismatched_types_for_fn_call,
                        .token = self.ast.tokens.get(arg_node.token),
                        .type1 = parameter_type,
                        .type2 = arg_expr_type,
                    });
                }
            }
            return info.return_type;
        },
        // int comparisions.
        .less_than,
        .greater_than,
        .less_than_equal,
        .greater_than_equal,
        => {
            const lhs_node = self.ast.nodes.get(node.lhs);
            const rhs_node = self.ast.nodes.get(node.rhs);
            const lhs_type = try self.typeCheckExpression(&lhs_node);
            const rhs_type = try self.typeCheckExpression(&rhs_node);

            if (!rhs_type.coercesTo(lhs_type)) {
                try self.errors.append(self.gpa, .{
                    .kind = .invalid_types_for_op,
                    .token = self.ast.tokens.get(lhs_node.token),
                    .other_tok = self.ast.tokens.get(lhs_node.token),
                    .type1 = lhs_type,
                    .type2 = rhs_type,
                });
            }

            if (!lhs_type.isNumeric()) {
                try self.errors.append(self.gpa, .{
                    .kind = .expected_numeric_type,
                    .token = self.ast.tokens.get(lhs_node.token),
                    .other_tok = self.ast.tokens.get(lhs_node.token),
                    .type2 = rhs_type,
                });
            }

            return .bool;
        },
        .equal,
        .not_equal,
        => {
            const lhs_node = self.ast.nodes.get(node.lhs);
            const rhs_node = self.ast.nodes.get(node.rhs);
            const lhs_type = try self.typeCheckExpression(&lhs_node);
            const rhs_type = try self.typeCheckExpression(&rhs_node);

            if (!rhs_type.coercesTo(lhs_type)) {
                try self.errors.append(self.gpa, .{
                    .kind = .invalid_types_for_op,
                    .token = self.ast.tokens.get(lhs_node.token),
                    .other_tok = self.ast.tokens.get(lhs_node.token),
                    .type1 = lhs_type,
                    .type2 = rhs_type,
                });
            }

            if (!lhs_type.isNumeric() and lhs_type != .bool) {
                try self.errors.append(self.gpa, .{
                    .kind = .expected_numeric_type,
                    .token = self.ast.tokens.get(lhs_node.token),
                    .other_tok = self.ast.tokens.get(lhs_node.token),
                    .type2 = rhs_type,
                });
            }

            return .bool;
        },
        else => std.debug.panic("Didnt impl type checking for {}", .{node}),
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
            const arg_count = node.lhs - 3; // -3 for the name, return type and body.
            const arg_types = try self.gpa.alloc(TypeHandle, arg_count);
            // skip name and leave out body and return type (2 last nodes)
            const arg_nodes = self.ast.extra.items[node.rhs + 1 .. node.rhs + node.lhs - 2];

            const return_type_node = self.ast.extra.items[node.rhs + node.lhs - 2];
            const return_type_bytes = self.srcBytes(return_type_node);

            const return_type = TypeHandle.primtives_map.get(return_type_bytes) orelse @panic("User specified types not supported yet");

            for (arg_types, arg_nodes) |*arg, arg_node_index| {
                const type_specifier_node = self.ast.nodes.items(.rhs)[arg_node_index];
                const arg_bytes = self.srcBytes(type_specifier_node);
                arg.* = TypeHandle.primtives_map.get(arg_bytes) orelse @panic("User specified types not supported yet");
            }

            try self.type_reg.functions.put(self.gpa, bytes, .{
                .args = arg_types,
                .return_type = return_type,
            });
        }
    }

    if (self.type_reg.functions.get("main")) |info| {
        if (info.args.len != 0) {
            try self.errors.append(self.gpa, .{
                .kind = .incorrect_main_args,
                .token = .{ .start = 0, .end = 1, .kind = .eof },
            });
        }

        if (info.return_type != .void) {
            try self.errors.append(self.gpa, .{
                .kind = .incorrect_main_ret_type,
                .token = .{ .start = 0, .end = 1, .kind = .eof },
            });
        }
    } else {
        try self.errors.append(self.gpa, .{
            .kind = .missing_main_fn,
            .token = .{ .start = 0, .end = 1, .kind = .eof },
        });
    }
}

pub const TypeHandle = enum(u32) {
    undefined, // currently ONLY for expressions that fail type checking
    void,
    int_literal,
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
    _, //<-- use this later for user defined types.

    const primtives_map = std.StaticStringMap(TypeHandle).initComptime(.{
        .{ "u8", .u8 },
        .{ "u16", .u16 },
        .{ "u32", .u32 },
        .{ "u64", .u64 },
        .{ "i8", .i8 },
        .{ "i16", .i16 },
        .{ "i32", .i32 },
        .{ "i64", .i64 },
        .{ "f32", .f32 },
        .{ "f64", .f64 },
        .{ "bool", .bool },
        .{ "void", .void },
    });

    pub fn isNumeric(self: TypeHandle) bool {
        return switch (self) {
            .int_literal,
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

    pub fn coercesTo(self: TypeHandle, other: TypeHandle) bool {
        return switch (self) {
            .undefined => true,
            .int_literal => other.isNumeric(),
            .bool => other == .bool,
            .void => other == .void,

            // unsigned
            .u8 => switch (other) {
                .u8, .u16, .u32, .u64 => true,
                else => false,
            },
            .u16 => switch (other) {
                .u16, .u32, .u64 => true,
                else => false,
            },
            .u32 => switch (other) {
                .u32, .u64 => true,
                else => false,
            },
            .u64 => other == .u64,

            // signed
            .i8 => switch (other) {
                .i8, .i16, .i32, .i64 => true,
                else => false,
            },
            .i16 => switch (other) {
                .i16, .i32, .i64 => true,
                else => false,
            },
            .i32 => switch (other) {
                .i32, .i64 => true,
                else => false,
            },
            .i64 => other == .i64,

            // floats
            .f32 => switch (other) {
                .f32, .f64 => true,
                else => false,
            },
            .f64 => other == .f64,

            // for user defined types there are no coercions.
            _ => self == other,
        };
    }
};

const TypeRegistry = struct {
    functions: std.StringHashMapUnmanaged(FunctionHeader),

    pub fn init() TypeRegistry {
        return .{
            .functions = std.StringHashMapUnmanaged(FunctionHeader){},
        };
    }

    pub fn deinit(self: *TypeRegistry, gpa: Allocator) void {
        var iter = self.functions.valueIterator();
        while (iter.next()) |value| {
            gpa.free(value.args);
        }

        self.functions.deinit(gpa);
    }
};

const FunctionHeader = struct {
    args: []TypeHandle,
    return_type: TypeHandle,
};

const Symbol = struct {
    token: u32,
    type: TypeHandle,
    is_const: bool, // bad but whatever.
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
    other_tok: ?Token = null,

    type1: TypeHandle = .undefined,
    type2: TypeHandle = .undefined,

    pub const Kind = enum {
        var_redecl,
        fn_redecl,
        use_of_undecl_fn,
        use_of_undecl_ident,
        mismatched_specifier_type,
        invalid_types_for_op,
        mismatched_types_for_fn_call,
        expected_numeric_type,
        wrong_return_type,
        missing_main_fn,
        incorrect_main_args,
        incorrect_main_ret_type,
        missing_return,
        wrong_arg_count,
        reassign_of_const,
        mismatched_assign_type,
        expected_type,
        int_too_big,
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

        const print_source_line = self.token.kind != .eof;

        try writer.print(red ++ "error" ++ reset ++ ": ", .{});
        switch (self.kind) {
            .fn_redecl => {
                try writer.print("Redeclaration of function \"{s}\"\n", .{src[self.token.start..self.token.end]});
            },
            .var_redecl => {
                try writer.print("Redeclaration of \"{s}\"\n", .{src[self.token.start..self.token.end]});
            },
            .use_of_undecl_fn => {
                try writer.print("Use of undeclared function \"{s}\"\n", .{src[self.token.start..self.token.end]});
            },
            .use_of_undecl_ident => {
                try writer.print("Use of undeclared identifier \"{s}\"\n", .{src[self.token.start..self.token.end]});
            },
            .mismatched_specifier_type => {
                try writer.print("Mismatched types between specifier ({s}) and expression ({s})\n", .{ @tagName(self.type1), @tagName(self.type2) });
            },
            .mismatched_assign_type => {
                try writer.print("Mismatched types between identifier ({s}) and assignment expression ({s})\n", .{ @tagName(self.type1), @tagName(self.type2) });
            },
            .invalid_types_for_op => {
                try writer.print("Invalid types for operation ({s} and {s})\n", .{ @tagName(self.type1), @tagName(self.type2) });
            },
            .mismatched_types_for_fn_call => {
                try writer.print("Mismatched type for function argument. Expected type {s} but got type {s}\n", .{ @tagName(self.type1), @tagName(self.type2) });
            },
            .expected_numeric_type => {
                try writer.print("Type must be numeric for operation but instead got type {s}.\n", .{@tagName(self.type2)});
            },
            .wrong_return_type => {
                try writer.print("Wrong return type for function. Expected type {s} but got {s}.\n", .{ @tagName(self.type1), @tagName(self.type2) });
            },
            .missing_main_fn => {
                try writer.print("Missing required function \"main\"\n", .{});
            },
            .incorrect_main_args => {
                try writer.print("main function cannot accept any arguments!\n", .{});
            },
            .incorrect_main_ret_type => {
                try writer.print("main function must return type void\n", .{});
            },
            .missing_return => {
                try writer.print("Function \"{s}\" with return type {s} has code paths that don't return\n", .{ src[self.other_tok.?.start..self.other_tok.?.end], @tagName(self.type1) });
            },
            .reassign_of_const => {
                try writer.print("Cannot reassign a constant\n", .{});
            },
            .wrong_arg_count => {
                try writer.print("Expected {d} arguments to function call but found {d}\n", .{ @intFromEnum(self.type1), @intFromEnum(self.type2) });
            },
            .expected_type => {
                try writer.print("Expected type {s} but found type {s}\n", .{ @tagName(self.type1), @tagName(self.type2) });
            },
            .int_too_big => {
                try writer.print("Integer literal too big for destination type {s}\n", .{@tagName(self.type1)});
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
