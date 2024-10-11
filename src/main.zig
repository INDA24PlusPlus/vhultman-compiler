const std = @import("std");
const assert = std.debug.assert;
const Allocator = std.mem.Allocator;
const Tokenizer = @import("Tokenizer.zig");
const Parser = @import("Parser.zig");
const Ast = @import("Ast.zig");

pub fn main() !void {
    // #PERF: Replace this in release builds.
    var gpa_state = std.heap.GeneralPurposeAllocator(.{}){};
    defer assert(gpa_state.deinit() == .ok);
    const gpa = gpa_state.allocator();

    const args = try std.process.argsAlloc(gpa);
    defer std.process.argsFree(gpa, args);

    if (args.len != 2) {
        _ = try std.io.getStdErr().write("Usage: compile <file>\n");
        return;
    }

    const source_code = try std.fs.cwd().readFileAllocOptions(
        gpa,
        args[1],
        std.math.maxInt(u32) - 100, // tokenizer uses 32 bit ints so ~4 GB is the file size limit. (plus some fudge for the parser)
        null,
        std.mem.page_size,
        0,
    );
    defer gpa.free(source_code);

    var parser = Parser.init(gpa, source_code);
    defer parser.deinit();

    var ast = parser.parse() catch |e| switch (e) {
        error.ParseError => {
            var bw = std.io.bufferedWriter(std.io.getStdErr().writer());
            const writer = bw.writer();

            for (parser.errors.items) |err| {
                try err.print(writer, source_code);
            }

            try bw.flush();
            std.process.fatal("Compilation failed.", .{});
        },
        else => return e,
    };

    defer ast.deinit(gpa);

    var bw = std.io.bufferedWriter(std.io.getStdOut().writer());
    const writer = bw.writer();
    try ast.print(writer, 0, 0);
    try bw.flush();
}

test {
    _ = Parser;
    _ = Tokenizer;
}
