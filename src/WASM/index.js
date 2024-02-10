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
    ({abc: method} = obj.instance.exports);
    console.log(method());
};

x();
