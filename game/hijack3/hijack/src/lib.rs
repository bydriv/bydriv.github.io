extern crate brownfox;

pub mod episode;
pub mod object;
pub mod system;
pub mod text;

use brownfox::Moore;

#[derive(Clone)]
pub struct Hijack {
    pub x: i32,
    pub y: i32,
    pub width: i32,
    pub height: i32,
    objects: Vec<(brownfox::Control, object::Object)>,
    events: Vec<Event>,
}

#[derive(Clone)]
pub enum Event {
    Focus(i32, i32, i32, i32),
    Check(i32, i32, i32, i32),
}

#[derive(Clone, PartialEq)]
pub enum View {
    Image(String, i32, i32, i32),
}

impl Hijack {
    pub fn new() -> Hijack {
        episode::boston::new()
    }
}

impl brownfox::Moore<Vec<brownfox::Input>, object::Output> for Hijack {
    fn transit(&self, inputs: &Vec<brownfox::Input>) -> Hijack {
        let events = self.output().events;
        let mut central_x = 0;
        let mut central_y = 0;

        for event in &events {
            match event {
                &Event::Focus(x, y, width, height) => {
                    central_x = x + width / 2;
                    central_y = y + height / 2;
                }
                _ => (),
            }
        }

        let left = central_x - self.width / 2;
        let top = central_y - self.height / 2;

        Hijack {
            x: left,
            y: top,
            width: self.width,
            height: self.height,
            objects: self
                .objects
                .iter()
                .map(|object| {
                    let control = object.0.transit(inputs);
                    let input = control.output();
                    (
                        control,
                        object.1.transit(&object::Input {
                            inputs: vec![input],
                            previous: self.clone(),
                        }),
                    )
                })
                .collect(),
            events: events,
        }
    }

    fn output(&self) -> object::Output {
        let events: Vec<Event> = self
            .objects
            .iter()
            .flat_map(|object| object.1.output().events)
            .collect();

        object::Output {
            events: events,
            views: self
                .objects
                .iter()
                .flat_map(|object| object.1.output().views)
                .collect(),
        }
    }
}
