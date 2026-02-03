const std = @import("std");
const TriArrayList = @import("TriArrayList");

pub fn main() !void {
    const arr = std.simd.iota(usize, 256);
    const arr2 = std.simd.iota(usize, 256);
    const out = arr - arr2;
    std.debug.print("{any}\n", .{arr});
    std.debug.print("{any}\n", .{out});
}
