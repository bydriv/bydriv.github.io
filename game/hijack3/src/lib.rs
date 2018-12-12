extern crate brownfox;
extern crate wasm_bindgen;

mod object;

use wasm_bindgen::prelude::*;

use brownfox::Moore;

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
    objects: Vec<object::Object>,
}

pub struct Events {
    events: Vec<Event>,
}

pub enum Event {
    Focus(i32, i32, i32, i32),
}

#[wasm_bindgen]
pub struct Views {
    views: Vec<View>,
}

#[derive(Clone)]
pub enum View {
    Image(String, i32, i32),
    Pattern(String, u32, u32, i32, i32),
}

#[wasm_bindgen]
pub fn views_length(views: &Views) -> usize {
    views.views.len()
}

#[wasm_bindgen]
pub fn view_is_image(i: usize, views: &Views) -> bool {
    match views.views[i] {
        View::Image(_, _, _) => true,
        _ => false,
    }
}

#[wasm_bindgen]
pub fn view_image_name(i: usize, views: &Views) -> Option<String> {
    match views.views[i].clone() {
        View::Image(name, _, _) => Some(name),
        _ => None,
    }
}

#[wasm_bindgen]
pub fn view_image_x(i: usize, views: &Views) -> Option<i32> {
    match views.views[i] {
        View::Image(_, x, _) => Some(x),
        _ => None,
    }
}

#[wasm_bindgen]
pub fn view_image_y(i: usize, views: &Views) -> Option<i32> {
    match views.views[i] {
        View::Image(_, _, y) => Some(y),
        _ => None,
    }
}

#[wasm_bindgen]
pub fn view_is_pattern(i: usize, views: &Views) -> bool {
    match views.views[i] {
        View::Pattern(_, _, _, _, _) => true,
        _ => false,
    }
}

#[wasm_bindgen]
pub fn view_pattern_name(i: usize, views: &Views) -> Option<String> {
    match views.views[i].clone() {
        View::Pattern(name, _, _, _, _) => Some(name),
        _ => None,
    }
}

#[wasm_bindgen]
pub fn view_pattern_width(i: usize, views: &Views) -> Option<u32> {
    match views.views[i] {
        View::Pattern(_, width, _, _, _) => Some(width),
        _ => None,
    }
}

#[wasm_bindgen]
pub fn view_pattern_height(i: usize, views: &Views) -> Option<u32> {
    match views.views[i] {
        View::Pattern(_, _, height, _, _) => Some(height),
        _ => None,
    }
}

#[wasm_bindgen]
pub fn view_pattern_x(i: usize, views: &Views) -> Option<i32> {
    match views.views[i] {
        View::Pattern(_, _, _, x, _) => Some(x),
        _ => None,
    }
}

#[wasm_bindgen]
pub fn view_pattern_y(i: usize, views: &Views) -> Option<i32> {
    match views.views[i] {
        View::Pattern(_, _, _, _, y) => Some(y),
        _ => None,
    }
}

impl Game {
    pub fn new() -> Game {
        Game {
            objects: vec![
                object::Object::Archimedes(object::archimedes::new(0, 0, 40, 30)),
                object::Object::Teiri(object::teiri::new(0, 0)),
                object::Object::Verity(object::verity::new(16, 16)),
            ],
        }
    }
}

impl brownfox::Moore<Inputs, object::Output> for Game {
    fn transit(&self, inputs: &Inputs) -> Game {
        Game {
            objects: brownfox::Vec::new(self.objects.clone())
                .transit(&(inputs, self))
                .machines,
        }
    }

    fn output(&self) -> object::Output {
        (
            Events {
                events: self
                    .objects
                    .iter()
                    .flat_map(|object| object.output().0.events)
                    .collect(),
            },
            Views {
                views: self
                    .objects
                    .iter()
                    .flat_map(|object| object.output().1.views)
                    .collect(),
            },
        )
    }
}

#[wasm_bindgen(js_name = new_)]
pub fn new() -> Game {
    Game::new()
}

#[wasm_bindgen]
pub fn step(inputs: &Inputs, game: &Game) -> Game {
    game.transit(inputs)
}

const SCALE: i32 = 2;
const WIDTH: i32 = 320;
const HEIGHT: i32 = 240;

#[wasm_bindgen]
pub fn views(game: &Game) -> Views {
    let mut central_x = 0;
    let mut central_y = 0;
    let (events, views) = game.output();

    for event in events.events {
        match event {
            Event::Focus(x, y, width, height) => {
                central_x = x + width / 2;
                central_y = y + height / 2;
            }
        }
    }

    let left = central_x - WIDTH / 2;
    let top = central_y - HEIGHT / 2;

    Views {
        views: views
            .views
            .iter()
            .map(|view| match view {
                View::Image(name, x, y) => View::Image(name.to_string(), x - left, y - top),
                View::Pattern(name, width, height, x, y) => {
                    View::Pattern(name.to_string(), *width, *height, x - left, y - top)
                }
            })
            .collect(),
    }
}
