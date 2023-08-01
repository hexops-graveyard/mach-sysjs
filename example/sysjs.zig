//! A file-level comment

const builtin = @import("builtin");

pub const bindings = struct {
    pub const console = struct {
        /// console.log invokes the JS console.log API
        log: fn (string: []const u8) void,
        log2: fn (string: []const u8, []const u8) void,
    };

    pub const TextDecoder = struct {
        new: fn () TextDecoder,
        decode: fn (td: TextDecoder, str: []const u8) String,
    };

    pub const String = struct {
        new: fn (buf: []const u8) String,
        charAt: fn (string: String, index: u32) u8,
    };

    pub const SomeClass = struct {
        someFunc: fn (str: TestingValueStructs) InternalStruct,
    };
};

pub const TestingValueStructs = struct {
    index: u32,
    name: []const u8,
    is_test: bool,
    value: InternalStruct,
    a_val: f16,
};

pub const InternalStruct = struct {
    idx: u32,
    any: f64,
};

/// doPrint does stuff
pub fn doPrint() void {
    // use console.log
    console.log("zig:js.console.log(\"hello from Zig\")");
}
