const std = @import("std");
const assert = std.debug.assert;
const qbe = @import("qbe");
const Allocator = std.mem.Allocator;
const Tokenizer = @import("Tokenizer.zig");
const Parser = @import("Parser.zig");
const Ast = @import("Ast.zig");
const Sema = @import("SemanticAnalysis.zig");
const CodeGen = @import("backends/qbe.zig").CodeGen;

pub fn main() !void {
    var timer = try std.time.Timer.start();

    var gpa_state = std.heap.GeneralPurposeAllocator(.{}){};
    defer assert(gpa_state.deinit() == .ok);
    const gpa = if (@import("builtin").mode == .ReleaseFast)
        std.heap.c_allocator
    else
        gpa_state.allocator();

    var args = try std.process.argsWithAllocator(gpa);
    defer args.deinit();
    _ = args.next();

    var out_file: []const u8 = "out";
    var maybe_source: ?[]const u8 = null;

    while (args.next()) |arg| {
        if (std.mem.eql(u8, arg, "-o")) {
            out_file = args.next() orelse std.process.fatal("-o was specified but no file name was given", .{});
        } else if (std.mem.eql(u8, arg, "-h")) {
            _ = try std.io.getStdOut().write("Usage: compile source_file -o out\n");
            return;
        } else {
            if (maybe_source == null) {
                maybe_source = arg;
            } else {
                std.process.fatal("Only one source file supported!", .{});
            }
        }
    }

    if (maybe_source == null) {
        std.process.fatal("Missing source file", .{});
    }
    const src_file = maybe_source.?;

    const source_code = try std.fs.cwd().readFileAllocOptions(
        gpa,
        src_file,
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

    const ir_file = try std.fs.cwd().createFile("out.ssa", .{});
    defer ir_file.close();

    // Codegen

    var bw = std.io.bufferedWriter(ir_file.writer());
    const ssa_writer = bw.writer();
    var code_gen = CodeGen(@TypeOf(ssa_writer)).init(ssa_writer, &ast, tokens, source_code);

    const qbe_ir_start = timer.read();
    try code_gen.run();
    const qbe_ir_end = timer.read();

    try bw.flush();

    const qbe_args = [_][*:0]const u8{
        "",
        "-o",
        "out.s",
        "out.ssa",
    };

    const qbe_start = timer.read();
    const return_code = qbe.invoke(4, &qbe_args);

    const qbe_end = timer.read();
    assert(return_code == 0);

    // TODO: This leaks if the string literal is not used.
    const cc = std.process.getEnvVarOwned(gpa, "cc") catch blk: {
        const err = std.fs.accessAbsolute("/usr/bin/cc", .{});
        if (err) |_| {
            break :blk "/usr/bin/cc";
        } else |_| {
            std.process.fatal("Cannot find C compiler (cc), try setting \"cc\" to valid C compiler", .{});
        }
    };

    const as_start = timer.read();
    var cc_process = std.process.Child.init(&.{ cc, "out.s", "-o", out_file }, gpa);
    _ = try cc_process.spawnAndWait();
    const as_end = timer.read();

    try printTimings(
        std.io.getStdOut().writer(),
        timer.read(),
        parse_end - parse_start,
        sema_end - sema_start,
        qbe_ir_end - qbe_ir_start,
        qbe_end - qbe_start,
        as_end - as_start,
    );

    try std.fs.cwd().deleteFile("out.s");
    try std.fs.cwd().deleteFile("out.ssa");
}

fn printTimings(
    writer: anytype,
    total_dur: u64,
    parse_dur: u64,
    sema_dur: u64,
    qbe_ir_dur: u64,
    qbe_dur: u64,
    as_dur: u64,
) !void {
    const total_time: f64 = @as(f64, @floatFromInt(total_dur)) / std.time.ns_per_ms;
    const parse_time: f64 = @as(f64, @floatFromInt(parse_dur)) / std.time.ns_per_ms;
    const sema_time: f64 = @as(f64, @floatFromInt(sema_dur)) / std.time.ns_per_ms;
    const qbe_ir_time: f64 = @as(f64, @floatFromInt(qbe_ir_dur)) / std.time.ns_per_ms;
    const qbe_time: f64 = @as(f64, @floatFromInt(qbe_dur)) / std.time.ns_per_ms;
    const as_time: f64 = @as(f64, @floatFromInt(as_dur)) / std.time.ns_per_ms;

    try writer.print("Parsing took {d}ms\n", .{parse_time});
    try writer.print("Semantic analysis took {d}ms\n", .{sema_time});
    try writer.print("QBE IR construction took {d}ms\n", .{qbe_ir_time});
    try writer.print("QBE code generation took {d}ms\n", .{qbe_time});
    try writer.print("Assembler + linker took {d}ms\n", .{as_time});
    try writer.print("Total time {d}ms\n", .{total_time});
}
