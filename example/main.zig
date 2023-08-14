const std = @import("std");
const js = @import("sysjs_generated.zig");

pub export fn main() u8 {
    const td = js.TextDecoder.new();
    const string = td.decode("Hello, world!\n");

    js.console.log(string);
    js.console.log(string.charAt(3));
    return 0;
}
