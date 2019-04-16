extern crate brownfox;
extern crate hijack;
extern crate wasm_bindgen;

mod config;

use brownfox::Moore;
use hijack::*;
use std::collections::HashMap;
use wasm_bindgen::prelude::*;

#[wasm_bindgen]
extern "C" {
    pub type Inputs;
    fn inputs_length(inputs: &Inputs) -> usize;
    fn input_x(i: usize, inputs: &Inputs) -> Option<f32>;
    fn input_y(i: usize, inputs: &Inputs) -> Option<f32>;
    fn input_button(i: usize, j: usize, inputs: &Inputs) -> Option<bool>;
}

#[wasm_bindgen]
pub struct Game {
    hijack: Hijack,
}

#[wasm_bindgen]
pub struct ViewMap {
    x: i32,
    y: i32,
    view_map: Vec<(i32, Vec<View>)>,
}

#[wasm_bindgen]
#[derive(PartialEq)]
pub struct Views {
    views: Vec<View>,
}

#[wasm_bindgen]
pub fn view_map_length(view_map: &ViewMap) -> usize {
    view_map.view_map.len()
}

#[wasm_bindgen]
pub fn view_map_x(view_map: &ViewMap) -> i32 {
    view_map.x
}

#[wasm_bindgen]
pub fn view_map_y(view_map: &ViewMap) -> i32 {
    view_map.y
}

#[wasm_bindgen]
pub fn view_map_z(i: usize, view_map: &ViewMap) -> i32 {
    view_map.view_map[i].0
}

#[wasm_bindgen]
pub fn view_map_views(i: usize, view_map: &ViewMap) -> Views {
    Views {
        views: view_map.view_map[i].1.clone(),
    }
}

#[wasm_bindgen]
pub fn views_length(views: &Views) -> usize {
    views.views.len()
}

#[wasm_bindgen]
pub fn views_eq(views1: &Views, views2: &Views) -> bool {
    views1 == views2
}

#[wasm_bindgen]
pub fn view_is_image(i: usize, views: &Views) -> bool {
    match views.views[i] {
        View::Image(_, _, _, _) => true,
        _ => false,
    }
}

#[wasm_bindgen]
pub fn view_image_name(i: usize, views: &Views) -> Option<String> {
    match views.views[i].clone() {
        View::Image(name, _, _, _) => Some(name),
        _ => None,
    }
}

#[wasm_bindgen]
pub fn view_image_x(i: usize, views: &Views) -> Option<i32> {
    match views.views[i] {
        View::Image(_, x, _, _) => Some(x),
        _ => None,
    }
}

#[wasm_bindgen]
pub fn view_image_y(i: usize, views: &Views) -> Option<i32> {
    match views.views[i] {
        View::Image(_, _, y, _) => Some(y),
        _ => None,
    }
}

#[wasm_bindgen]
pub fn view_image_z(i: usize, views: &Views) -> Option<i32> {
    match views.views[i] {
        View::Image(_, _, _, z) => Some(z),
        _ => None,
    }
}

impl Game {
    pub fn new() -> Game {
        Game {
            hijack: Hijack::new(config::MAPS(), config::EPISODES()),
        }
    }
}

impl brownfox::Moore<(i32, Inputs), object::Output> for Game {
    fn transit(&self, (fps, inputs): &(i32, Inputs)) -> Game {
        let inputs = (0..inputs_length(inputs))
            .map(|i| {
                if let Some(x) = input_x(i, inputs) {
                    if let Some(y) = input_y(i, inputs) {
                        let buttons = &(0..32)
                            .map(|j| {
                                if let Some(b) = input_button(i, j, inputs) {
                                    b
                                } else {
                                    false
                                }
                            })
                            .collect();
                        brownfox::Input::new(x.into(), y.into(), buttons)
                    } else {
                        brownfox::Input::new(0.0, 0.0, &(0..32).map(|_| false).collect())
                    }
                } else {
                    brownfox::Input::new(0.0, 0.0, &(0..32).map(|_| false).collect())
                }
            })
            .collect();
        Game {
            hijack: self.hijack.transit(&(*fps, inputs)),
        }
    }

    fn output(&self) -> object::Output {
        self.hijack.output()
    }
}

#[wasm_bindgen(js_name = new_)]
pub fn new() -> Game {
    Game::new()
}

#[wasm_bindgen]
pub fn step(fps: i32, inputs: Inputs, game: &Game) -> Game {
    game.transit(&(fps, inputs))
}

#[wasm_bindgen]
pub fn view_map(game: &Game) -> ViewMap {
    let views = game.output().views;
    let mut view_map = HashMap::new();

    for view in views {
        let z = match view {
            View::Image(_, _, _, z) => z,
        };
        let mut views = view_map.entry(z).or_insert(vec![]);
        views.push(view);
    }

    let mut view_map: Vec<(i32, Vec<View>)> = view_map.into_iter().collect();
    view_map.sort_by(|(z1, _), (z2, _)| z1.cmp(z2));

    ViewMap {
        x: game.hijack.x,
        y: game.hijack.y,
        view_map: view_map,
    }
}
