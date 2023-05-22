const fs = require('fs');
const zig = require('./sysjs_generated');
const source = fs.readFileSync("zig-out/lib/example.wasm");
const typedArray = new Uint8Array(source);
let text_decoder = new TextDecoder();

let memory = undefined;

WebAssembly.instantiate(typedArray, {
  env: {
    sysjs_console_log: (v0_str_ptr, v0_str_len) => {
      const v0 = text_decoder.decode(new Uint8Array(memory.buffer, v0_str_ptr, v0_str_len));
      console.log(`JS:console.log("${v0}")`);
    }
  },
}).then(result => {
    const doPrint = result.instance.exports.doPrint;
    memory = result.instance.exports.memory;
    doPrint();
});
