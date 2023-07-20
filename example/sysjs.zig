//! A file-level comment

const builtin = @import("builtin");

pub const console = struct {
    /// console.log invokes the JS console.log API
    log: fn (string: []const u8) void,
    log2: fn (string: []const u8, []const u8) void,
};

pub const TextDecoder = struct {
    id: u32,
    new: fn () TextDecoder,
    decode: fn (td: TextDecoder, str: []const u8) String,
};

pub const String = struct {
    id: u32,
    new: fn (buf: []const u8) String,
    charAt: fn (string: String, index: u32) u8,
};

/// doPrint does stuff
pub fn doPrint() void {
    // use console.log
    console.log("zig:js.console.log(\"hello from Zig\")");
}
