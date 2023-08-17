//! A file-level comment

const builtin = @import("builtin");

pub const bindings = struct {
    pub const console = struct {
        /// console.log invokes the JS console.log API
        log: fn (string: String) void,
        log2: fn (string: []const u8, []const u8) void,
    };

    pub const TextDecoder = struct {
        new: fn () TextDecoder,
        decode: fn (td: TextDecoder, str: []const u8) String,
    };

    pub const String = struct {
        new: fn (buf: []const u8) String,
        charAt: fn (string: String, index: u32) String,
    };

    pub const navigator = struct {
        pub const gpu = struct {
            requestAdapter: fn (options: RequestAdapterOptions) void,
        };
    };
};

pub const RequestAdapterOptions = struct {
    powerPreference: String,
};

/// doPrint does stuff
pub fn doPrint() void {
    // use console.log
    console.log("zig:js.console.log(\"hello from Zig\")");
}
