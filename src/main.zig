const std = @import("std");
const assert = std.debug.assert;
const Allocator = std.mem.Allocator;
const Tokenizer = @import("Tokenizer.zig");

pub fn main() !void {
    var gpa_state = std.heap.GeneralPurposeAllocator(.{}){};
    defer assert(gpa_state.deinit() == .ok);

    const gpa = gpa_state.allocator();

    const args = try std.process.argsAlloc(gpa);
    defer std.process.argsFree(gpa, args);

    if (args.len != 2) {
        // No error reporting for now.
        return error.InvalidArgs;
    }

    const source_code = try std.fs.cwd().readFileAllocOptions(gpa, args[1], 1_000_000_000, null, std.mem.page_size, 0);

    defer gpa.free(source_code);

    var tokenizer = Tokenizer.init(source_code);
    while (tokenizer.next()) |token| {
        std.debug.print("{any}: {s}\n", .{ token, source_code[token.start..token.end] });
    }
}

test {
    _ = Tokenizer;
}
