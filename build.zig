const std = @import("std");

pub fn build(b: *std.Build) void {
    _ = b.addModule("bexpr", .{
        .source_file = .{ .path = "src/parse.zig" },
    });

    const test_step = b.addTest(.{
        .root_source_file = .{ .path = "src/parse.zig" },
    });
    b.step("test", "Run library tests")
        .dependOn(&b.addRunArtifact(test_step).step);
}
