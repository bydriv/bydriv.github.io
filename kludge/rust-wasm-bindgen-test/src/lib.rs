extern crate wasm_bindgen;
use wasm_bindgen::prelude::*;

#[wasm_bindgen]
extern "C" {
  pub type Question;
  fn question_message(x : &Question) -> String;
}

#[wasm_bindgen]
pub struct Answer {
  msg : String,
}

#[wasm_bindgen]
pub fn deep_thought(q : &Question) -> Answer {
  match question_message(q).as_str() {
    "Answer to the Ultimate Question of Life, the Universe, and Everything" =>
      Answer { msg: "42".to_string() },
    _ =>
      Answer { msg: "I CAN'T THINK OF IT".to_string() },
  }
}

#[wasm_bindgen]
pub fn answer_message(a : Answer) -> String {
  a.msg
}
