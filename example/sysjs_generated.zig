extern fn sysjs_console_log(v0: [*]const u8, v1: u32) void;

pub const console = struct {
    pub inline fn log(v0: []const u8) void {
        sysjs_console_log(v0.ptr, @intCast(u32, v0.len));
    }
};

const user_code = @import("sysjs_exports.zig");

export fn doPrint() void {
    return user_code.doPrint();
}
