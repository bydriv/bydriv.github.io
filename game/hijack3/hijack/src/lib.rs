extern crate brownfox;

pub mod episode;
pub mod object;
pub mod system;
pub mod text;

use brownfox::Moore;

#[derive(Clone)]
pub struct Hijack {
    objects: Vec<(brownfox::Control, object::Object)>,
    events: Vec<Event>,
}

#[derive(Clone)]
pub enum Event {
    Focus(i32, i32, i32, i32),
    Check(i32, i32, i32, i32),
}

#[derive(Clone)]
pub enum View {
    Image(String, i32, i32, i32),
}

impl Hijack {
    pub fn new() -> Hijack {
        episode::test::new()
    }
}

impl brownfox::Moore<Vec<brownfox::Input>, object::Output> for Hijack {
    fn transit(&self, inputs: &Vec<brownfox::Input>) -> Hijack {
        Hijack {
            objects: self
                .objects
                .iter()
                .map(|object| {
                    let control = object.0.transit(inputs);
                    let input = control.output();
                    (control, object.1.transit(&(vec![input], self.clone())))
                })
                .collect(),
            events: self.output().0,
        }
    }

    fn output(&self) -> object::Output {
        let mut central_x = 0;
        let mut central_y = 0;
        let events: Vec<Event> = self
            .objects
            .iter()
            .flat_map(|object| object.1.output().0)
            .collect();

        for event in &events {
            match event {
                &Event::Focus(x, y, width, height) => {
                    central_x = x + width / 2;
                    central_y = y + height / 2;
                }
                _ => (),
            }
        }

        let left = central_x - WIDTH / 2;
        let top = central_y - HEIGHT / 2;

        (
            events,
            self.objects
                .iter()
                .flat_map(|object| object.1.output().1)
                .map(|view| match view {
                    View::Image(name, x, y, z) => {
                        View::Image(name.to_string(), x - left, y - top, z)
                    }
                })
                .collect(),
        )
    }
}

pub const SCALE: i32 = 2;
pub const WIDTH: i32 = 320;
pub const HEIGHT: i32 = 240;
