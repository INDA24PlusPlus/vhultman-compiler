const std = @import("std");
const Tokenizer = @This();

pub const Token = struct {
    kind: Kind,
    start: u32,
    end: u32,

    const keywords = std.StaticStringMap(Kind).initComptime(.{
        .{ "fn", .@"fn" },
        .{ "var", .@"var" },
        .{ "const", .@"const" },
        .{ "return", .@"return" },
        .{ "true", .true },
        .{ "false", .false },

        .{ "u8", .primitive_type },
        .{ "u16", .primitive_type },
        .{ "u32", .primitive_type },
        .{ "u64", .primitive_type },
        .{ "void", .primitive_type },
    });

    pub const Kind = enum(u8) {
        @"+",
        @"-",
        @"*",
        @"/",
        @"!",
        @"!=",
        @"==",
        @"<",
        @">",

        @"return",

        @"=",
        @";",
        @":",
        @"(",
        @")",
        @"{",
        @"}",

        @"var",
        @"const",
        @"fn",

        true,
        false,
        int_literal,
        identifier,
        primitive_type,

        invalid,
        eof,
    };
};

// Only having one enum value in this will crash the compiler ¯\_(ツ)_/¯
const State = enum {
    start,
    identifier,
    int_literal,
    string_literal,
};

src: [:0]const u8,
index: u32,

pub fn init(src: [:0]const u8) Tokenizer {
    return .{ .src = src, .index = 0 };
}

pub fn next(self: *Tokenizer) ?Token {
    var result: Token = .{
        .kind = undefined,
        .start = self.index,
        .end = undefined,
    };

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
                result.kind = .@"+";
            },
            '-' => {
                self.index += 1;
                result.kind = .@"-";
            },
            '*' => {
                self.index += 1;
                result.kind = .@"*";
            },
            '/' => {
                self.index += 1;
                if (self.src[self.index] == '/') {
                    while (self.src[self.index] != '\n' and self.src[self.index] != 0) {
                        self.index += 1;
                    }
                    continue :state .start;
                } else {
                    result.kind = .@"/";
                }
            },
            '!' => {
                self.index += 1;
                if (self.src[self.index] == '=') {
                    self.index += 1;
                    result.kind = .@"!=";
                } else {
                    result.kind = .@"!";
                }
            },
            '=' => {
                self.index += 1;
                if (self.src[self.index] == '=') {
                    self.index += 1;
                    result.kind = .@"==";
                } else {
                    result.kind = .@"=";
                }
            },
            '<' => {
                self.index += 1;
                result.kind = .@"<";
            },
            '>' => {
                self.index += 1;
                result.kind = .@">";
            },
            ';' => {
                self.index += 1;
                result.kind = .@";";
            },
            ':' => {
                self.index += 1;
                result.kind = .@":";
            },
            '(' => {
                self.index += 1;
                result.kind = .@"(";
            },
            ')' => {
                self.index += 1;
                result.kind = .@")";
            },
            '{' => {
                self.index += 1;
                result.kind = .@"{";
            },
            '}' => {
                self.index += 1;
                result.kind = .@"}";
            },
            'a'...'z', 'A'...'Z', '_' => {
                result.kind = .identifier;
                continue :state .identifier;
            },
            '0'...'9' => {
                result.kind = .int_literal;
                continue :state .int_literal;
            },
            0 => {
                if (self.index >= self.src.len) {
                    return null;
                } else {
                    result.kind = .invalid;
                }

                self.index += 1;
            },
            else => {
                result.kind = .invalid;
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
                        result.kind = t;
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
        .string_literal => {
            self.index += 1;
            if (self.src[self.index] == '"' or self.src[self.index] == 0) {} else {
                continue :state .string_literal;
            }
        },
    }

    result.end = self.index;
    return result;
}
