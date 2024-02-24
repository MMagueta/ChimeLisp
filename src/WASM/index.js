import fs from 'fs'

const wasm = fs.readFileSync('./test.wasm');

let method = null;
let memory = new WebAssembly.Memory ({ initial: 1 });
let start_string_index = 0;

let importObject = {
    env: {
	buffer: memory,
	start_string: start_string_index,
	print_string: function (str_len) {
	    const bytes = new Uint8Array (memory.buffer,
					  start_string_index, str_len);
	    const log_string = new TextDecoder('utf8').decode(bytes);
	    console.log (log_string);
	}
    }

}

// const x = await WebAssembly.instantiate(new Uint8Array(wasm), importObject).then(res => res.instance.exports);

const x = async () => {
    let obj = await WebAssembly.instantiate(new Uint8Array (wasm), importObject);
    ({$inc: method} = obj.instance.exports);
    console.log(method(1));
};

x();

/*
(module
(import "env" "print_string" (func $print_string( param i32 )))
(import "env" "buffer" (memory 1))
(global $start_string (import "env" "start_string") i32)
(global $string_len i32 (i32.const 12))
(data (global.get $start_string) "hello world!")
(func (export "name")
(call $print_string (global.get $string_len))))
*/
