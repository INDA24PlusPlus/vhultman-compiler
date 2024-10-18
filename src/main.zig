const std = @import("std");
const assert = std.debug.assert;
const Allocator = std.mem.Allocator;
const Tokenizer = @import("Tokenizer.zig");
const Parser = @import("Parser.zig");
const Ast = @import("Ast.zig");
const Sema = @import("SemanticAnalysis.zig");

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

    var timer = try std.time.Timer.start();

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

    const parse_start = timer.read();
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
    const parse_end = timer.read();

    // TODO: Unify token representation.
    var tokens = try gpa.alloc(Tokenizer.Token, ast.tokens.len);
    defer gpa.free(tokens);

    for (0..ast.tokens.len) |i| {
        tokens[i] = ast.tokens.get(i);
    }

    const sema_start = timer.read();
    var sema = try Sema.init(gpa, tokens, source_code, &ast);
    defer sema.deinit();
    sema.resolveProgram() catch |e| switch (e) {
        error.ResolveError => {
            var bw = std.io.bufferedWriter(std.io.getStdErr().writer());
            const writer = bw.writer();
            for (sema.errors.items) |err| {
                try err.print(writer, source_code, tokens);
            }

            try bw.flush();
            std.process.fatal("Compilation failed.", .{});
        },
        else => return e,
    };
    const sema_end = timer.read();

    try printTimings(
        std.io.getStdOut().writer(),
        parse_end - parse_start,
        sema_end - sema_start,
    );
}

fn printTimings(writer: anytype, parse_dur: u64, sema_dur: u64) !void {
    const parse_time: f64 = @as(f64, @floatFromInt(parse_dur)) / std.time.ns_per_s;
    const sema_time: f64 = @as(f64, @floatFromInt(sema_dur)) / std.time.ns_per_s;

    try writer.print("Parsing took {d}s\n", .{parse_time});
    try writer.print("Semantic analysis took {d}s\n", .{sema_time});
}
