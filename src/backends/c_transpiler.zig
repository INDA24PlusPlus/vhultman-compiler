const std = @import("std");
const Ast = @import("../Ast.zig");
const Sema = @import("../Sema.zig");

pub fn CTranspiler(comptime WriterType: type) type {
    return struct {
        const Self = @This();

        writer: WriterType,
        ast: *const Ast,

        const primtives_map = std.StaticStringMap([]const u8).initComptime(.{
            .{ "u8", "uint8_t" },
            .{ "u16", "uint16_t" },
            .{ "u32", "uint32_t" },
            .{ "u64", "uint64_t" },
            .{ "i8", "int8_t" },
            .{ "i16", "int16_t" },
            .{ "i32", "int32_t" },
            .{ "i64", "int64_t" },
            .{ "f32", "float" },
            .{ "f64", "double" },
            .{ "bool", "bool" },
            .{ "void", "void" },
        });

        pub fn init(writer: WriterType, ast: *const Ast) Self {
            return .{
                .writer = writer,
                .ast = ast,
            };
        }

        pub fn transpile(self: *Self) !void {
            try self.writePreamble();
            try self.writeFunctionDefinitions();
            try self.writeFunctionBodies();
        }

        fn writePreamble(self: *Self) !void {
            try self.writer.print("#include <stdint.h>\n", .{});
            try self.writer.print("#include <stdbool.h>\n", .{});
            try self.writer.print("#include <stdio.h>\n", .{});
        }

        fn writeFunctionDefinitions(self: *Self) !void {
            const root = self.ast.nodes.get(0);
            const fn_decls = self.ast.extra.items[root.rhs .. root.rhs + root.lhs];
            for (fn_decls) |fn_decl| {
                try self.writeFunctionDefinition(&self.ast.nodes.get(fn_decl));
                _ = try self.writer.write(";\n");
            }
        }

        fn writeFunctionDefinition(self: *Self, fn_node: *const Ast.Node) !void {
            const identifier = self.ast.nodes.get(self.ast.extra.items[fn_node.rhs]);
            const fn_name_token = self.ast.tokens.get(identifier.token);
            const fn_name = self.ast.src[fn_name_token.start..fn_name_token.end];

            const return_type_node = self.ast.nodes.get(self.ast.extra.items[fn_node.rhs + fn_node.lhs - 2]);
            var return_type_bytes = primtives_map.get(return_type_node.srcBytes(self.ast)).?;

            if (std.mem.eql(u8, fn_name, "main")) {
                return_type_bytes = "int";
            }
            try self.writer.print("{s} {s}(", .{ return_type_bytes, fn_name });
            const arg_nodes = self.ast.extra.items[fn_node.rhs + 1 .. fn_node.rhs + fn_node.lhs - 2];
            if (arg_nodes.len != 0) {
                try self.writeTypeSpecifier(&self.ast.nodes.get(arg_nodes[0]));
                var idx: usize = 1;
                while (idx < arg_nodes.len) : (idx += 1) {
                    const arg_index = arg_nodes[idx];
                    try self.writer.print(", ", .{});
                    const arg_node = self.ast.nodes.get(arg_index);
                    try self.writeTypeSpecifier(&arg_node);
                }
            }

            try self.writer.print(")", .{});
        }

        fn writeFunctionBodies(self: *Self) !void {
            const root = self.ast.nodes.get(0);
            const fn_decls = self.ast.extra.items[root.rhs .. root.rhs + root.lhs];
            for (fn_decls) |fn_decl| {
                try self.writeFunctionBody(&self.ast.nodes.get(fn_decl));
            }
        }

        fn writeFunctionBody(self: *Self, node: *const Ast.Node) !void {
            try self.writeFunctionDefinition(node);
            try self.writeBlock(&self.ast.nodes.get(self.ast.extra.items[node.rhs + node.lhs - 1]));
        }

        fn writeBlock(self: *Self, node: *const Ast.Node) anyerror!void {
            try self.writer.print(" {{\n", .{});
            const statements = self.ast.extra.items[node.rhs .. node.rhs + node.lhs];
            for (statements) |statement| {
                const statement_node = self.ast.nodes.get(statement);
                try self.writeStatement(&statement_node);
                try self.writer.print("\n", .{});
            }
            try self.writer.print("}}\n", .{});
        }

        fn writeStatement(self: *Self, node: *const Ast.Node) !void {
            switch (node.kind) {
                .var_decl, .const_decl => try self.writeVarDecl(node),
                .return_statement => try self.writeReturn(node),
                .assignment => try self.writeAssignment(node),
                .block => try self.writeBlock(node),
                .if_statement => try self.writeIf(node),
                .while_loop => try self.writeWhile(node),
                .print_statement => try self.writePrint(node),
                else => std.debug.panic("Code gen not supported for {}", .{node.kind}),
            }
        }

        fn writePrint(self: *Self, node: *const Ast.Node) !void {
            const identifier = self.ast.nodes.get(node.lhs);
            const print_type: Sema.TypeHandle = @enumFromInt(identifier.lhs);
            const format_specifier = switch (print_type) {
                .u8 => "hhu",
                .u16 => "hu",
                .u32 => "u",
                .u64 => "llu",
                .i8 => "hhd",
                .i16 => "hd",
                .i32 => "d",
                .i64 => "lld",
                .f32 => "f",
                .f64 => "lf",
                .bool => "d",
                .int_literal => unreachable,
                .void => unreachable,
                .undefined => unreachable,
                _ => unreachable,
            };
            try self.writer.print("printf(\"%{s}\\n\", ", .{format_specifier});
            try self.writeIdentifier(&identifier);
            try self.writer.print(");\n", .{});
        }

        fn writeWhile(self: *Self, node: *const Ast.Node) anyerror!void {
            try self.writer.print("while (", .{});
            const cond = self.ast.nodes.get(node.lhs);
            try self.writeExpression(&cond, .none);
            try self.writer.print(")", .{});
            const body = self.ast.nodes.get(node.rhs);
            try self.writeStatement(&body);
        }

        fn writeIf(self: *Self, node: *const Ast.Node) anyerror!void {
            try self.writer.print("if (", .{});
            const cond = self.ast.nodes.get(self.ast.extra.items[node.rhs]);
            try self.writeExpression(&cond, .none);
            try self.writer.print(")", .{});
            const body = self.ast.nodes.get(self.ast.extra.items[node.rhs + 1]);
            try self.writeStatement(&body);

            if (node.lhs == 3) {
                const else_body = self.ast.nodes.get(self.ast.extra.items[node.rhs + 2]);
                try self.writer.print("else ", .{});
                try self.writeStatement(&else_body);
            }
        }

        fn writeAssignment(self: *Self, node: *const Ast.Node) !void {
            const identifier = self.ast.nodes.get(node.lhs);
            const expr = self.ast.nodes.get(node.rhs);
            try self.writeIdentifier(&identifier);
            try self.writer.print(" = ", .{});
            try self.writeExpression(&expr, .none);
            try self.writer.print(";", .{});
        }

        fn writeReturn(self: *Self, node: *const Ast.Node) !void {
            try self.writer.print("return ", .{});
            try self.writeExpression(&self.ast.nodes.get(node.lhs), .none);
            try self.writer.print(";", .{});
        }

        fn writeVarDecl(self: *Self, node: *const Ast.Node) !void {
            const type_specifier = self.ast.nodes.get(node.lhs);
            const expr = self.ast.nodes.get(node.rhs);
            try self.writeTypeSpecifier(&type_specifier);
            try self.writer.print(" = ", .{});
            try self.writeExpression(&expr, .none);
            try self.writer.print(";", .{});
        }

        const Precedence = enum {
            none,
            equals,
            greater_less,
            add,
            mul,
            prefix,
            call,

            fn nodePrecedence(node_kind: Ast.Node.Kind) Precedence {
                return switch (node_kind) {
                    .equal, .not_equal => .equals,
                    .less_than, .greater_than, .less_than_equal, .greater_than_equal => .greater_less,
                    .add, .sub => .add,
                    .mul, .div => .mul,
                    .fn_call => .call,
                    else => std.debug.panic("I think this should never occur", .{}),
                };
            }
        };

        fn writeExpression(self: *Self, node: *const Ast.Node, precedence: Precedence) anyerror!void {
            const token = self.ast.tokens.get(node.token);
            const bytes = self.ast.src[token.start..token.end];
            if (node.isBinOp()) {
                const curr_precedence = Precedence.nodePrecedence(node.kind);
                const is_grouped = @intFromEnum(curr_precedence) < @intFromEnum(precedence);

                if (is_grouped) {
                    try self.writer.print("(", .{});
                }

                const lhs = self.ast.nodes.get(node.lhs);
                const rhs = self.ast.nodes.get(node.rhs);
                try self.writeExpression(&lhs, curr_precedence);
                _ = try self.writer.print(" {s} ", .{bytes});
                try self.writeExpression(&rhs, curr_precedence);

                if (is_grouped) {
                    try self.writer.print(")", .{});
                }

                return;
            } else switch (node.kind) {
                .int_literal, .identifier, .bool_literal => {
                    try self.writer.print("{s}", .{bytes});
                    return;
                },
                .fn_call => try self.writeFunctionCall(node),
                else => std.debug.panic("Should not be here: {}", .{node.kind}),
            }
        }

        fn writeFunctionCall(self: *Self, node: *const Ast.Node) !void {
            const children = self.ast.extra.items[node.rhs .. node.rhs + node.lhs];
            try self.writeIdentifier(&self.ast.nodes.get(children[0]));
            try self.writer.print("(", .{});

            const args = children[1..];
            if (args.len != 0) {
                var arg_index = args[0];
                var expr = self.ast.nodes.get(arg_index);
                try self.writeExpression(&expr, .none);

                var idx: usize = 1;
                while (idx < args.len) : (idx += 1) {
                    try self.writer.print(", ", .{});
                    arg_index = args[idx];
                    expr = self.ast.nodes.get(arg_index);
                    try self.writeExpression(&expr, .none);
                }
            }
            try self.writer.print(")", .{});
        }

        fn writeTypeSpecifier(self: *Self, node: *const Ast.Node) !void {
            const identifer = self.ast.nodes.get(node.lhs);
            const type_identifier = self.ast.nodes.get(node.rhs);

            try self.writeTypeIdentifier(&type_identifier);
            try self.writer.writeByte(' ');
            try self.writeIdentifier(&identifer);
        }

        fn writeTypeIdentifier(self: *Self, node: *const Ast.Node) !void {
            const bytes = primtives_map.get(node.srcBytes(self.ast)).?;
            try self.writer.print("{s}", .{bytes});
        }

        fn writeIdentifier(self: *Self, node: *const Ast.Node) !void {
            const bytes = node.srcBytes(self.ast);
            try self.writer.print("{s}", .{bytes});
        }
    };
}
