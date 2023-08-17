const std = @import("std");
const js = @import("sysjs_generated.zig");

pub export fn main() u8 {
    const td = js.TextDecoder.new();
    const string = td.decode("Hello, world!\n");

    js.console.log(string);
    js.console.log(string.charAt(3));

    const power = td.decode("low-power");
    const options = js.RequestAdapterOptions{ .powerPreference = power };
    js.navigator.gpu.requestAdapter(options.toExtern());

    return 0;
}
