extern crate wasm_bindgen;
use wasm_bindgen::prelude::*;

#[wasm_bindgen]
extern "C" {
    pub type Inputs;
    pub type Input;
}

#[wasm_bindgen]
pub struct Game {}

#[wasm_bindgen]
pub struct Views {
    views: Vec<View>,
}

#[derive(Clone)]
pub enum View {
    Image(String, i32, i32),
}

#[wasm_bindgen]
pub fn views_length(views: &Views) -> usize {
    views.views.len()
}

#[wasm_bindgen]
pub fn view_is_image(i: usize, views: &Views) -> bool {
    match views.views[i] {
        View::Image(_, _, _) => true,
    }
}

#[wasm_bindgen]
pub fn view_image_name(i: usize, views: &Views) -> Option<String> {
    match views.views[i].clone() {
        View::Image(name, _, _) => Some(name),
    }
}

#[wasm_bindgen]
pub fn view_image_x(i: usize, views: &Views) -> Option<i32> {
    match views.views[i] {
        View::Image(_, x, _) => Some(x),
    }
}

#[wasm_bindgen]
pub fn view_image_y(i: usize, views: &Views) -> Option<i32> {
    match views.views[i] {
        View::Image(_, _, y) => Some(y),
    }
}

#[wasm_bindgen(js_name = new_)]
pub fn new() -> Game {
    Game {}
}

#[wasm_bindgen]
pub fn step(_inputs: &Inputs, _game: &Game) -> Game {
    Game {}
}

#[wasm_bindgen]
pub fn views(_game: &Game) -> Views {
    Views {
        views: vec![View::Image(
            "pixelart/teiri/walk/front/0.png".to_string(),
            0,
            0,
        )],
    }
}
