extern crate brownfox;

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
    Image(String, i32, i32),
    Pattern(String, u32, u32, i32, i32),
}

impl Hijack {
    pub fn new() -> Hijack {
        Hijack {
            objects: vec![
                (
                    brownfox::Control::Immovable(brownfox::Immovable::new()),
                    object::Object::Archimedes(object::archimedes::new(0, 0, 40, 30)),
                ),
                (
                    brownfox::Control::Player(brownfox::Player::new(0)),
                    object::Object::Teiri(object::teiri::new(16, 16)),
                ),
                (
                    brownfox::Control::Player(brownfox::Player::new(0)),
                    object::Object::Verity(object::verity::new(64, 16)),
                ),
                (
                    brownfox::Control::Player(brownfox::Player::new(0)),
                    object::Object::Emily(object::emily::new(96, 16)),
                ),
                (
                    brownfox::Control::Player(brownfox::Player::new(0)),
                    object::Object::Mathprobe(object::mathprobe::new(32, 16)),
                ),
                (
                    brownfox::Control::Player(brownfox::Player::new(0)),
                    object::Object::Lonelygem(object::lonelygem::new(112, 16)),
                ),
                (
                    brownfox::Control::Player(brownfox::Player::new(0)),
                    object::Object::SecurityDrone(object::security_drone::new(80, 48)),
                ),
            ],
            events: vec![],
        }
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
                    View::Image(name, x, y) => View::Image(name.to_string(), x - left, y - top),
                    View::Pattern(name, width, height, x, y) => {
                        View::Pattern(name.to_string(), width, height, x - left, y - top)
                    }
                })
                .collect(),
        )
    }
}

pub const SCALE: i32 = 2;
pub const WIDTH: i32 = 320;
pub const HEIGHT: i32 = 240;
