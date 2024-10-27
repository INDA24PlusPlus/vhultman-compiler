const std = @import("std");
const Allocator = std.mem.Allocator;
const Tokenizer = @import("Tokenizer.zig");
const Parser = @import("Parser.zig");
const assert = std.debug.assert;

pub fn main() !void {
    var gpa_state = std.heap.GeneralPurposeAllocator(.{}){};
    defer assert(gpa_state.deinit() == .ok);
    const gpa = gpa_state.allocator();

    const args = try std.process.argsAlloc(gpa);
    defer std.process.argsFree(gpa, args);

    const src = try std.fs.cwd().readFileAllocOptions(
        gpa,
        args[1],
        std.math.maxInt(u32),
        null,
        std.mem.page_size,
        0,
    );
    defer gpa.free(src);

    var parser = try Parser.init(gpa, src);
    defer parser.deinit();

    var ast = try parser.parse();
    defer ast.deinit(gpa);

    try ast.print(std.io.getStdOut().writer(), 0, 0);
}
