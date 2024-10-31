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

    log.debug("Putting the args in the symbol table for function: {s}\n", .{fn_identifier});
    const fn_header = self.type_reg.functions.get(fn_identifier).?;
    const arg_nodes = self.ast.extra.items[node.rhs + 1 .. node.rhs + node.lhs - 2];

    for (arg_nodes, fn_header.args) |node_index, arg_type| {
        const arg_node = self.ast.nodes.get(node_index);
        const bytes = self.srcBytes(arg_node.lhs);
        log.debug("putting in {s} with type {}", .{ bytes, arg_type });
        try self.sym_table.put(self.gpa, .{ .type = arg_type, .token = arg_node.lhs }, bytes);
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

fn resolveBlock(self: *Sema, node: *const Ast.Node, return_type: TypeHandle) !bool {
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
        .return_statement => {
            try self.resolveReturn(node, return_type);
            return true;
        },
        else => std.debug.panic("Semantic analysis not supported for {}", .{node.kind}),
    }
}

fn resolveReturn(self: *Sema, node: *const Ast.Node, return_type: TypeHandle) !void {
    const expr = self.ast.nodes.get(node.lhs);
    const expr_type = try self.typeCheckExpression(&expr);

    log.debug("return type is {}", .{return_type});
    log.debug("expr type that is returned is {}", .{expr_type});

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

    // TODO: this does not work for user specified types.
    const specified_type = TypeHandle.primtives_map.get(self.srcBytes(type_specifier.rhs)) orelse @panic("User specified types not supported yet");
    const expr_type = try self.typeCheckExpression(&expr);

    log.debug("specified type for {s} is {}", .{ bytes, specified_type });
    log.debug("expr type for {s} is {}", .{ bytes, expr_type });

    log.info("TODO: Implement size checking for the int_literal type", .{});
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
        try self.sym_table.put(self.gpa, .{ .token = ident_token, .type = specified_type }, bytes);
    }

    std.debug.print("is const? {}: {s}\n", .{ is_const, bytes });
}

fn typeCheckExpression(self: *Sema, node: *const Ast.Node) !TypeHandle {
    switch (node.kind) {
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

            if (lhs_type != rhs_type and lhs_type != .undefined and rhs_type != .undefined) {
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
                    .type1 = lhs_type,
                    .type2 = rhs_type,
                });
            }

            return lhs_type;
        },
        .int_literal => {
            _ = std.fmt.parseUnsigned(u64, self.srcBytesNode(node), 10) catch |err| switch (err) {
                error.Overflow => std.debug.panic("Integer too large!", .{}),
                else => unreachable,
            };
            return .int_literal;
        },
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

            log.debug("type checking for fn call to {s}", .{name_bytes});
            const info = self.type_reg.functions.get(name_bytes) orelse {
                try self.errors.append(self.gpa, .{
                    .kind = .use_of_undecl_fn,
                    .token = self.ast.tokens.get(self.ast.nodes.items(.token)[fn_name]),
                });
                return .undefined;
            };

            const arg_nodes = self.ast.extra.items[node.rhs + 1 .. node.rhs + node.lhs];
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

        .equal, .not_equal => {
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
                    .type1 = lhs_type,
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

            log.debug("Function has return type: {s}", .{return_type_bytes});
            const return_type = TypeHandle.primtives_map.get(return_type_bytes) orelse @panic("User specified types not supported yet");

            for (arg_types, arg_nodes) |*arg, arg_node_index| {
                const type_specifier_node = self.ast.nodes.items(.rhs)[arg_node_index];
                const arg_bytes = self.srcBytes(type_specifier_node);
                log.debug("Arg type: {s}", .{arg_bytes});
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

const TypeHandle = enum(u32) {
    TODO,
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
            .TODO => @panic("trying to coerece todo"),
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
            .invalid_types_for_op => {
                try writer.print("Invalid types for operation ({s} and {s})\n", .{ @tagName(self.type1), @tagName(self.type2) });
            },
            .mismatched_types_for_fn_call => {
                try writer.print("Mismatched type for function argument. Expected type {s} but got type {s}\n", .{ @tagName(self.type1), @tagName(self.type2) });
            },
            .expected_numeric_type => {
                try writer.print("Type must be numeric for operation but instead got type {s} and {s}.\n", .{ @tagName(self.type1), @tagName(self.type2) });
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
                try writer.print("Function \"{s}\" with return type {s} has no return statements\n", .{ src[self.other_tok.?.start..self.other_tok.?.end], @tagName(self.type1) });
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
