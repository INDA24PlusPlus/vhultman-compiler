const std = @import("std");
const Allocator = std.mem.Allocator;
const Tokenizer = @import("Tokenizer.zig");
const Parser = @import("Parser.zig");
const Sema = @import("Sema.zig");
const CTranspiler = @import("backends/c_transpiler.zig").CTranspiler;
const assert = std.debug.assert;

pub fn main() !void {
    var gpa_state = std.heap.GeneralPurposeAllocator(.{}){};
    defer assert(gpa_state.deinit() == .ok);
    const gpa = gpa_state.allocator();

    var err_buffer = std.io.bufferedWriter(std.io.getStdErr().writer());
    const err_writer = err_buffer.writer();

    const args = try std.process.argsAlloc(gpa);
    defer std.process.argsFree(gpa, args);

    const file_path = args[1];

    const src = blk: {
        const src = try std.fs.cwd().readFileAllocOptions(
            gpa,
            file_path,
            std.math.maxInt(u32),
            null,
            std.mem.page_size,
            0,
        );
        defer gpa.free(src);

        const tab_free_src = try tabToSpace(gpa, src);
        break :blk tab_free_src;
    };
    defer gpa.free(src);

    var parser = try Parser.init(gpa, src);
    defer parser.deinit();

    const parse_result = parser.parse();
    if (parse_result == error.HadParseError) {
        for (parser.errors.items) |*err| {
            try err.print(err_writer, src, file_path);
        }
        try err_buffer.flush();
        std.process.fatal("Compilation failed", .{});
    }

    var ast = try parse_result;
    defer ast.deinit(gpa);

    try ast.print(std.io.getStdOut().writer(), 0, 0);

    var sema = try Sema.init(gpa, &ast);
    defer sema.deinit();

    if (sema.resolve() == error.HadSemaError) {
        for (sema.errors.items) |*err| {
            try err.print(err_writer, src, file_path);
        }
        try err_buffer.flush();
        std.process.fatal("Compilation failed", .{});
    }

    const out = try std.fs.cwd().createFile("out.c", .{});
    defer out.close();

    var out_buffer = std.io.bufferedWriter(out.writer());
    const out_writer = out_buffer.writer();

    var transpiler = CTranspiler(@TypeOf(out_writer)).init(out_writer, &ast);
    try transpiler.transpile();
    try out_buffer.flush();
}

fn tabToSpace(gpa: Allocator, src: [:0]const u8) ![:0]const u8 {
    var new_src = try std.ArrayList(u8).initCapacity(gpa, src.len);
    for (src) |c| {
        if (c == '\t') {
            try new_src.appendNTimes(' ', 4);
        } else {
            try new_src.append(c);
        }
    }

    if (new_src.items.len > std.math.maxInt(u32)) {
        return error.FileToBig;
    }

    return try new_src.toOwnedSliceSentinel(0);
}
