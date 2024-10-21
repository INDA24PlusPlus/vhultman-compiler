const std = @import("std");
const Ast = @import("../Ast.zig");
const Token = @import("../Tokenizer.zig").Token;

pub fn CodeGen(comptime WriterType: type) type {
    return struct {
        const Self = @This();
        ast: *const Ast,
        tokens: []const Token,
        src: []const u8,
        writer: WriterType,

        // I do not want to support a scenario where this overflows
        anon_block_count: u64 = 0,

        // Temp storage for a single label name.
        fmt_buf: [1024]u8 = undefined,

        pub fn init(writer: WriterType, ast: *const Ast, tokens: []Token, src: []const u8) Self {
            return .{
                .writer = writer,
                .ast = ast,
                .tokens = tokens,
                .src = src,
            };
        }

        pub fn run(self: *Self) !void {
            _ = try self.writer.write("export function l $main() {\n@start\n");

            const root_node = self.ast.nodes.get(0);
            const children = self.ast.extra[root_node.rhs .. root_node.rhs + root_node.lhs];
            for (children) |child| {
                const child_node = self.ast.nodes.get(child);
                try self.genStatement(&child_node);
            }
            _ = try self.writer.write("}\n");

            // TEMP print code until we can actually print (checking return code only gives the first byte)
            _ = try self.writer.write("data $fmt = { b \"Return value is %ld!\\n\", b 0 }");
        }

        pub fn genStatement(cg: *Self, node: *const Ast.Node) anyerror!void {
            switch (node.type) {
                .var_statement => try cg.genVarStatement(node),
                .return_statement => try cg.genReturnStatement(node),
                .assignment_statement => try cg.genAssignStatement(node),
                .block => try cg.genBlock(node, cg.anonBlockName()),
                .if_statement => try cg.genIfStatement(node),
                .while_loop => try cg.genWhileLoop(node),
                .if_else_statement => try cg.genIfElseStatement(node),

                // These should not be able to produce any side effect so we *SHOULD* be able to ignore code gen for them.
                .expression_statement => {},
                else => std.debug.panic("{}", .{node.type}),
            }
        }

        fn genWhileLoop(cg: *Self, node: *const Ast.Node) !void {
            const condition_node = cg.ast.nodes.get(node.lhs);
            const block_node = cg.ast.nodes.get(node.rhs);

            var buf: [256]u8 = undefined;
            const block_label = blk: {
                const block_label = cg.anonBlockName();
                @memcpy(buf[0..block_label.len], block_label);
                break :blk buf[0..block_label.len];
            };

            const loop_label = cg.anonBlockName();

            try cg.writer.print("@{s}\n", .{loop_label});
            const condition_reg = try cg.genExpression(&condition_node, "while_cond", 0);
            try cg.writer.print("jnz %while_cond{d}, @{s}, @{s}_end\n", .{ condition_reg, block_label, block_label });
            try cg.writer.print("@{s}\n", .{block_label});

            const children = cg.ast.extra[block_node.rhs .. block_node.rhs + block_node.lhs];
            for (children) |child| {
                const child_node = cg.ast.nodes.get(child);
                try cg.genStatement(&child_node);
            }

            try cg.writer.print("jmp @{s}\n", .{loop_label});
            try cg.writer.print("@{s}_end\n", .{block_label});
        }

        fn genIfElseStatement(cg: *Self, node: *const Ast.Node) !void {
            const condition_node = cg.ast.nodes.get(node.lhs);

            const block_indices = cg.ast.extra[node.rhs .. node.rhs + 2];
            const block_node = cg.ast.nodes.get(block_indices[0]);
            const else_block_node = cg.ast.nodes.get(block_indices[1]);

            const condition_reg = try cg.genExpression(&condition_node, "if_cond", 0);

            var buf: [256]u8 = undefined;
            const block_label = blk: {
                const block_label = cg.anonBlockName();
                @memcpy(buf[0..block_label.len], block_label);
                break :blk buf[0..block_label.len];
            };

            const else_label = cg.anonBlockName();

            try cg.writer.print("jnz %if_cond{d}, @{s}, @{s}\n", .{ condition_reg, block_label, else_label });
            try cg.writer.print("@{s}\n", .{block_label});
            {
                const children = cg.ast.extra[block_node.rhs .. block_node.rhs + block_node.lhs];
                for (children) |child| {
                    const child_node = cg.ast.nodes.get(child);
                    try cg.genStatement(&child_node);
                }
                try cg.writer.print("jmp @{s}_end\n", .{block_label});
            }

            try cg.writer.print("@{s}\n", .{else_label});
            {
                const children = cg.ast.extra[else_block_node.rhs .. else_block_node.rhs + else_block_node.lhs];
                for (children) |child| {
                    const child_node = cg.ast.nodes.get(child);
                    try cg.genStatement(&child_node);
                }
                try cg.writer.print("jmp @{s}_end\n", .{block_label});
            }

            try cg.writer.print("@{s}_end\n", .{block_label});
        }

        fn genIfStatement(cg: *Self, node: *const Ast.Node) !void {
            const condition_node = cg.ast.nodes.get(node.lhs);
            const block_node = cg.ast.nodes.get(node.rhs);

            const condition_reg = try cg.genExpression(&condition_node, "if_cond", 0);
            const block_name = cg.anonBlockName();

            try cg.writer.print("jnz %if_cond{d}, @{s}, @{s}_end\n", .{ condition_reg, block_name, block_name });
            try cg.genBlock(&block_node, block_name);
        }

        fn genReturnStatement(cg: *Self, node: *const Ast.Node) !void {
            const expr_node = cg.ast.nodes.get(node.lhs);
            const expr_register = try cg.genExpression(&expr_node, "return", 0);

            // temp code so we can see the entire return value.
            _ = try cg.writer.write("call $printf(l $fmt, ..., l %return0)\n");
            try cg.writer.print("ret %{s}{d}\n", .{ "return", expr_register });
        }

        fn genBlock(cg: *Self, node: *const Ast.Node, block_label: []const u8) !void {
            try cg.writer.print("@{s}\n", .{block_label});
            const children = cg.ast.extra[node.rhs .. node.rhs + node.lhs];
            for (children) |child| {
                const child_node = cg.ast.nodes.get(child);
                try cg.genStatement(&child_node);
            }
            try cg.writer.print("jmp @{s}_end\n", .{block_label});
            try cg.writer.print("@{s}_end\n", .{block_label});
        }

        fn genAssignStatement(cg: *Self, node: *const Ast.Node) !void {
            const ident_node = cg.ast.nodes.get(node.lhs);
            const expr_node = cg.ast.nodes.get(node.rhs);
            const ident_bytes = cg.getIdentBytes(ident_node.token_index);

            const expr_register = try cg.genExpression(&expr_node, ident_bytes, 0);
            try cg.writer.print("%{s} =l copy %{s}{d}\n", .{ ident_bytes, ident_bytes, expr_register });
        }

        fn genVarStatement(cg: *Self, node: *const Ast.Node) !void {
            const ident_node = cg.ast.nodes.get(node.lhs);
            const expr_node = cg.ast.nodes.get(node.rhs);
            const ident_bytes = cg.getIdentBytes(ident_node.token_index);

            const expr_register = try cg.genExpression(&expr_node, ident_bytes, 0);
            try cg.writer.print("%{s} =l copy %{s}{d}\n", .{ ident_bytes, ident_bytes, expr_register });
        }

        fn genExpression(cg: *Self, node: *const Ast.Node, prefix: []const u8, depth: u32) !u32 {
            switch (node.type) {
                .add, .sub, .mul, .div => {
                    const lhs = try cg.genExpression(&cg.ast.nodes.get(node.lhs), prefix, depth * 2 + 0);
                    const rhs = try cg.genExpression(&cg.ast.nodes.get(node.rhs), prefix, depth * 2 + 1);

                    try cg.writer.print("%{s}{d} =l {s} %{s}{d}, %{s}{d}\n", .{
                        prefix,
                        depth,
                        @tagName(node.type),
                        prefix,
                        lhs,
                        prefix,
                        rhs,
                    });
                    return depth;
                },
                .less_than, .greater_than, .equal, .not_equal => {
                    const lhs = try cg.genExpression(&cg.ast.nodes.get(node.lhs), prefix, depth * 2 + 0);
                    const rhs = try cg.genExpression(&cg.ast.nodes.get(node.rhs), prefix, depth * 2 + 1);
                    const op = switch (node.type) {
                        .less_than => "csltl",
                        .greater_than => "csgtl",
                        .equal => "ceql",
                        .not_equal => "cnel",
                        else => unreachable,
                    };

                    try cg.writer.print("%{s}{d} =l {s} %{s}{d}, %{s}{d}\n", .{ prefix, depth, op, prefix, lhs, prefix, rhs });
                    return depth;
                },
                .not => {
                    const rhs = try cg.genExpression(&cg.ast.nodes.get(node.rhs), prefix, depth * 2 + 1);
                    try cg.writer.print("%{s}{d} =l xor %{s}{d}, 1\n", .{ prefix, depth, prefix, rhs });
                    return depth;
                },
                .negate => {
                    const rhs = try cg.genExpression(&cg.ast.nodes.get(node.rhs), prefix, depth * 2 + 1);
                    try cg.writer.print("%{s}{d} =l neg %{s}{d}\n", .{ prefix, depth, prefix, rhs });
                    return depth;
                },
                .bool_literal => {
                    const bytes = cg.getIdentBytes(node.token_index);
                    const value: u8 = if (bytes[0] == 't') '1' else '0';
                    try cg.writer.print("%{s}{d} =l copy {c}\n", .{ prefix, depth, value });
                    return depth;
                },
                .int_literal => {
                    try cg.writer.print("%{s}{d} =l copy {s}\n", .{ prefix, depth, cg.getIdentBytes(node.token_index) });
                    return depth;
                },
                .identifier => {
                    try cg.writer.print("%{s}{d} =l copy %{s}\n", .{ prefix, depth, cg.getIdentBytes(node.token_index) });
                    return depth;
                },
                else => unreachable,
            }
        }

        fn getIdentBytes(self: *const Self, token_index: u32) []const u8 {
            return self.src[self.tokens[token_index].start..self.tokens[token_index].end];
        }

        fn anonBlockName(self: *Self) []const u8 {
            const r = std.fmt.bufPrint(&self.fmt_buf, "anon_block{d}", .{self.anon_block_count}) catch unreachable;
            self.anon_block_count += 1;
            return r;
        }
    };
}
