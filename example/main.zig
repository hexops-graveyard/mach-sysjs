// TODO: this doesn't actually end up `export`ing doPrint, why?
// const js = @import("sysjs_generated.zig");
// var _ = js.doPrint;

// This does:
const js = @import("sysjs_generated.zig");

pub export fn main() u8 {
    const td = js.TextDecoder.new();
    const string = td.decode("Hello, world!\n");

    js.console.log(string);
    return 0;
}
