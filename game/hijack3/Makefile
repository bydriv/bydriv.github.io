.PHONY: all
all:
	make exe
	make js

.PHONY: exe
exe:
	cargo +nightly build --target x86_64-pc-windows-gnu

.PHONY: js
js:
	cargo +nightly build --target wasm32-unknown-unknown
	wasm-bindgen target/wasm32-unknown-unknown/debug/hijackjs.wasm --out-dir wasm --no-modules

.PHONY: server
server:
	python3 server.py
