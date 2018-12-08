all:
	cargo +nightly build --target wasm32-unknown-unknown
	wasm-bindgen target/wasm32-unknown-unknown/debug/rust_wasm_bindgen_test.wasm --out-dir wasm-bindgen --no-modules

server:
	python3 server.py
