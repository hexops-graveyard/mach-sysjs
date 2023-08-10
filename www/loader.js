export async function load(url, imports) {
	const response = await fetch(url);
	const buffer = await response.arrayBuffer();
	const wasm = await WebAssembly.instantiate(buffer, imports);

	return wasm.instance;
}

export function init(instance) {
	instance.exports.main();
}
