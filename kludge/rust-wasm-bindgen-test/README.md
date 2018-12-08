# wasm-bindgen test

Rust で wasm するやつ。
Firefox でテスト済。

1. チュートリアルに従って wasm-bindgen をインストール https://rustwasm.github.io/wasm-bindgen/

```
$ rustup default nightly
$ rustup target add wasm32-unknown-unknown --toolchain nightly
$ cargo +nightly install wasm-bindgen-cli
```

2. `make` でビルド

```
$ make
```

3. `make server` でサーバを起動

```
$ make server
```

4. Firefox で `localhost:8000` にアクセス、開発ツールでログを見る

*Note*: Firefox 以外のブラウザでも動くかもしれないけどテストはしてない。
TextEncoder とかの Polyfill が必要かも。
