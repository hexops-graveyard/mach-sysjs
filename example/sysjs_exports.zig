const js = @import("sysjs_generated.zig");
const builtin = @import("builtin");

pub fn doPrint() void {
    switch (builtin.cpu.arch) {
        .wasm32 => js.console.log("zig:js.console.log(\"hello from Zig\")"),
        else => {},
    }
}
