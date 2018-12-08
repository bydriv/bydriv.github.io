extern crate wasm_bindgen;
use wasm_bindgen::prelude::*;

#[wasm_bindgen]
pub fn deep_thought(msg : &str) -> String {
  match msg {
    "Answer to the Ultimate Question of Life, the Universe, and Everything" =>
      "42".to_string(),
    _ =>
      "I CAN'T THINK OF IT".to_string(),
  }
}
