const std = @import("std");
const Tokenizer = @This();

const TokenType = enum(u8) {
    @"+",
    @"-",
    @"*",
    @"/",
    @"if",
    @"else",
    @"=",
    @";",
    @"(",
    @")",
    int_literal,
    invalid,
    identifier,
};

const Token = struct {
    type: TokenType,
    start: u32,
    end: u32,

    const keywords = std.StaticStringMap(TokenType).initComptime(.{
        .{ "if", .@"if" },
        .{ "else", .@"else" },
    });
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

pub fn next(self: *Tokenizer) ?Token {
    var result: Token = .{
        .type = undefined,
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
            '=' => {
                self.index += 1;
                result.type = .@"=";
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
            'a'...'z', 'A'...'Z', '_' => {
                result.type = .identifier;
                continue :state .identifier;
            },
            '0'...'9' => {
                result.type = .int_literal;
                continue :state .int_literal;
            },
            0 => {
                if (self.index == self.src.len) {
                    return null;
                }

                result.type = .invalid;
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
