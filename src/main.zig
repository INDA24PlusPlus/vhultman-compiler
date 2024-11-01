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
    const gpa = if (@import("builtin").mode != .ReleaseFast) gpa_state.allocator() else std.heap.c_allocator;

    var err_buffer = std.io.bufferedWriter(std.io.getStdErr().writer());
    const err_writer = err_buffer.writer();

    const args = try std.process.argsAlloc(gpa);
    defer std.process.argsFree(gpa, args);

    const file_path = args[1];

    var timer = try std.time.Timer.start();

    const tab_convert_start = timer.read();
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
    const tab_convert_end = timer.read();

    var parser = try Parser.init(gpa, src);
    defer parser.deinit();

    const parser_start = timer.read();
    const parse_result = parser.parse();
    const parser_end = timer.read();

    if (parse_result == error.HadParseError) {
        for (parser.errors.items) |*err| {
            try err.print(err_writer, src, file_path);
        }
        try err_buffer.flush();
        std.process.fatal("Compilation failed", .{});
    }

    var ast = try parse_result;
    defer ast.deinit(gpa);

    var sema = try Sema.init(gpa, &ast);
    defer sema.deinit();

    const sema_start = timer.read();
    const sema_result = sema.resolve();
    const sema_end = timer.read();
    if (sema_result == error.HadSemaError) {
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
    const backend_start = timer.read();
    try transpiler.transpile();
    const backend_end = timer.read();
    try out_buffer.flush();

    try printTimings(
        std.io.getStdOut().writer(),
        timer.read(),
        tab_convert_end - tab_convert_start,
        parser_end - parser_start,
        sema_end - sema_start,
        backend_end - backend_start,
    );
}
fn printTimings(
    writer: anytype,
    total_dur: u64,
    tab_dur: u64,
    parse_dur: u64,
    sema_dur: u64,
    backend_dur: u64,
) !void {
    const total_time: f64 = @as(f64, @floatFromInt(total_dur)) / std.time.ns_per_ms;
    const tab_time: f64 = @as(f64, @floatFromInt(tab_dur)) / std.time.ns_per_ms;
    const parse_time: f64 = @as(f64, @floatFromInt(parse_dur)) / std.time.ns_per_ms;
    const sema_time: f64 = @as(f64, @floatFromInt(sema_dur)) / std.time.ns_per_ms;
    const backend_time: f64 = @as(f64, @floatFromInt(backend_dur)) / std.time.ns_per_ms;

    try writer.print("Tab to space conversion took {d}ms\n", .{tab_time});
    try writer.print("Parsing took {d}ms\n", .{parse_time});
    try writer.print("Semantic analysis took {d}ms\n", .{sema_time});
    try writer.print("C backend took {d}ms\n", .{backend_time});
    try writer.print("Total time {d}ms\n", .{total_time});
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
