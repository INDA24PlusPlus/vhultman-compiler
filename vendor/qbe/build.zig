const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const lib = b.addStaticLibrary(.{
        .name = "qbe",
        .optimize = optimize,
        .target = target,
        .link_libc = true,
    });

    const common_sources = [_][]const u8{
        "src/main.c",
        "src/util.c",
        "src/parse.c",
        "src/abi.c",
        "src/cfg.c",
        "src/mem.c",
        "src/ssa.c",
        "src/alias.c",
        "src/load.c",
        "src/copy.c",
        "src/fold.c",
        "src/simpl.c",
        "src/live.c",
        "src/spill.c",
        "src/rega.c",
        "src/emit.c",
    };

    const amd64_sources = [_][]const u8{
        "src/amd64/targ.c",
        "src/amd64/sysv.c",
        "src/amd64/isel.c",
        "src/amd64/emit.c",
    };

    const arm64_sources = [_][]const u8{
        "src/arm64/targ.c",
        "src/arm64/abi.c",
        "src/arm64/isel.c",
        "src/arm64/emit.c",
    };

    const rv64_sources = [_][]const u8{
        "src/rv64/targ.c",
        "src/rv64/abi.c",
        "src/rv64/isel.c",
        "src/rv64/emit.c",
    };

    lib.addCSourceFiles(.{
        .files = &(common_sources ++ amd64_sources ++ arm64_sources ++ rv64_sources),
        .flags = &.{"-fno-sanitize=undefined"},
    });

    const ConfigHeader = struct {
        Deftgt: @TypeOf(.enum_literal),
    };

    const config_header = std.Build.Step.ConfigHeader.create(b, .{});
    switch (target.result.cpu.arch) {
        .x86_64 => if (target.result.os.tag == .macos) {
            config_header.addValues(ConfigHeader{ .Deftgt = .T_amd64_apple });
        } else {
            config_header.addValues(ConfigHeader{ .Deftgt = .T_amd64_sysv });
        },
        .aarch64 => if (target.result.os.tag == .macos) {
            config_header.addValues(ConfigHeader{ .Deftgt = .T_arm64_apple });
        } else {
            config_header.addValues(ConfigHeader{ .Deftgt = .T_arm64 });
        },
        .riscv64 => config_header.addValues(ConfigHeader{ .Deftgt = .T_rv64 }),
        else => @panic("Unsupported build target"),
    }

    //const header = b.addTranslateC(.{
    //    .target = target,
    //    .optimize = optimize,
    //    .root_source_file = b.path("qbe.h"),
    //});
    //_ = header.addModule("qbe");
    _ = b.addModule("qbe", .{
        .root_source_file = b.path("qbe.zig"),
    });

    lib.addConfigHeader(config_header);
    b.installArtifact(lib);
}
