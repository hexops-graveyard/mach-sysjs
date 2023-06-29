//! A file-level comment

const builtin = @import("builtin");

pub const console = struct {
    /// console.log invokes the JS console.log API
    log: fn (string: []const u8) void,

    log2: fn (string: []const u8, string2: []const u8) void,
};

/// doPrint does stuff
pub fn doPrint() void {
    // use console.log
    console.log("zig:js.console.log(\"hello from Zig\")");
}
