const js = @import("sysjs_generated.zig");

export fn doPrint() void {
    js.console.log("foobar");
}
