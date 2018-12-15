MKCONFIG := tools/mkconfig
MKASSETS := tools/mkassets
PIXELARTS := $(shell find pixelart -name '*.png')

.PHONY: all
all:
	make exe
	make js

.PHONY: exe
exe: src/hijack/config.c src/hijack/assets.c
	cargo +nightly build --target x86_64-pc-windows-gnu

.PHONY: js
js:
	cargo +nightly build --target wasm32-unknown-unknown
	wasm-bindgen target/wasm32-unknown-unknown/debug/hijackjs.wasm --out-dir wasm --no-modules

.PHONY: server
server:
	python3 server.py

src/hijack/config.c: json/config.json
	$(MKCONFIG) $^ > $@

src/hijack/assets.c: $(PIXELARTS)
	$(MKASSETS) $^ > $@
