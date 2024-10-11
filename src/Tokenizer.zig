const std = @import("std");
const Tokenizer = @This();

pub const Token = struct {
    type: Type,
    start: u32,
    end: u32,

    const keywords = std.StaticStringMap(Type).initComptime(.{
        .{ "if", .@"if" },
        .{ "else", .@"else" },
        .{ "var", .var_stmt },
        .{ "return", .@"return" },
        .{ "true", .true },
        .{ "false", .false },
        .{ "while", .@"while" },
    });

    pub const Type = enum(u8) {
        @"+",
        @"-",
        @"*",
        @"/",
        @"!",
        @"!=",
        @"==",
        @"<",
        @">",
        @"if",
        @"else",
        assign,
        @";",
        @"(",
        @")",
        @"{",
        @"}",
        var_stmt,
        @"return",
        @"while",
        true,
        false,
        int_literal,
        invalid,
        identifier,
        eof,
    };
};

// Only having one enum value in this will crash the compiler ¯\_(ツ)_/¯
const State = enum {
    start,
    identifier,
    int_literal,
};

src: [:0]const u8,
index: u32,

pub fn init(src: [:0]const u8) Tokenizer {
    return .{ .src = src, .index = 0 };
}

pub fn next(self: *Tokenizer) Token {
    var result: Token = .{
        .type = undefined,
        .start = self.index,
        .end = undefined,
    };

    if (self.index >= self.src.len) {
        result.type = .eof;
        return result;
    }

    state: switch (State.start) {
        .start => switch (self.src[self.index]) {
            '\n', '\t', ' ', '\r' => {
                self.index += 1;
                result.start = self.index;
                continue :state .start;
            },
            // Could combine these if we commit to having a fixed enum order.
            '+' => {
                self.index += 1;
                result.type = .@"+";
            },
            '-' => {
                self.index += 1;
                result.type = .@"-";
            },
            '*' => {
                self.index += 1;
                result.type = .@"*";
            },
            '/' => {
                self.index += 1;
                result.type = .@"/";
            },
            '!' => {
                self.index += 1;
                if (self.src[self.index] == '=') {
                    self.index += 1;
                    result.type = .@"!=";
                } else {
                    result.type = .@"!";
                }
            },
            '=' => {
                self.index += 1;
                if (self.src[self.index] == '=') {
                    self.index += 1;
                    result.type = .@"==";
                } else {
                    result.type = .assign;
                }
            },

            '<' => {
                self.index += 1;
                result.type = .@"<";
            },
            '>' => {
                self.index += 1;
                result.type = .@">";
            },
            ';' => {
                self.index += 1;
                result.type = .@";";
            },
            '(' => {
                self.index += 1;
                result.type = .@"(";
            },
            ')' => {
                self.index += 1;
                result.type = .@")";
            },
            '{' => {
                self.index += 1;
                result.type = .@"{";
            },
            '}' => {
                self.index += 1;
                result.type = .@"}";
            },
            'a'...'z', 'A'...'Z', '_' => {
                result.type = .identifier;
                continue :state .identifier;
            },
            '0'...'9' => {
                result.type = .int_literal;
                continue :state .int_literal;
            },
            0 => {
                if (self.index >= self.src.len) {
                    result.type = .eof;
                } else {
                    result.type = .invalid;
                }

                self.index += 1;
            },
            else => {
                result.type = .invalid;
                self.index += 1;
            },
        },
        .identifier => {
            self.index += 1;
            switch (self.src[self.index]) {
                // Still going
                'a'...'z', 'A'...'Z', '_', '0'...'9' => continue :state .identifier,

                // Hit end.
                else => {
                    const identifier_string = self.src[result.start..self.index];
                    if (Token.keywords.get(identifier_string)) |t| {
                        result.type = t;
                    }
                },
            }
        },
        .int_literal => {
            self.index += 1;
            switch (self.src[self.index]) {
                '0'...'9' => {
                    continue :state .int_literal;
                },
                else => {},
            }
        },
    }

    result.end = self.index;
    return result;
}

test next {
    const global = struct {
        fn testOne(input: []const u8) anyerror!void {
            const inputZ = try std.testing.allocator.dupeZ(u8, input);
            defer std.testing.allocator.free(inputZ);

            var tokenizer = Tokenizer.init(inputZ);
            while (tokenizer.next()) |token| {
                _ = token;
            }
        }
    };
    try std.testing.fuzz(global.testOne, .{});
}
