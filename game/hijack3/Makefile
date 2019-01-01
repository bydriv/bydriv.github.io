MKCONFIG := tools/mkconfig
MKASSETS := tools/mkassets
MKEPISODE := tools/mkepisode
MKMAP := tools/mkmap
PIXELARTS := $(shell find pixelart -name '*.png')
ILLUST := $(shell find illust -name '*.png')

.PHONY: all
all:
	make exe
	make js

.PHONY: exe
exe: src/config.rs src/assets.rs hijack/src/map/boston.rs hijack/src/episode/boston.rs
	cargo +nightly build --target x86_64-pc-windows-gnu
	rustc src/hijack.rs --emit obj --target=x86_64-pc-windows-gnu -L target/x86_64-pc-windows-gnu/debug/deps
	x86_64-w64-mingw32-gcc -UUNICODE -std=c11 -pedantic-errors -Wall -Werror -Iinclude -mwindows ./hijack.o src/hijack/platform/windows.c target/x86_64-pc-windows-gnu/debug/hijack.lib -lopengl32 -lgdiplus -lole32 -lwinmm -lws2_32 -luserenv

.PHONY: js
js:
	cargo +nightly build --target wasm32-unknown-unknown
	wasm-bindgen target/wasm32-unknown-unknown/debug/hijackjs.wasm --out-dir wasm --no-modules

.PHONY: server
server:
	(cd ../.. && python3 game/hijack3/server.py)

src/config.rs: json/config.json
	$(MKCONFIG) $^ > $@

src/assets.rs: $(PIXELARTS) $(ILLUST)
	$(MKASSETS) $^ > $@

hijack/src/episode/%.rs: episode/%.json
	$(MKEPISODE) $^ > $@

hijack/src/map/%.rs: map/%.json
	$(MKMAP) $^ > $@
