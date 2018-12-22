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
}

impl Hijack {
    pub fn new() -> Hijack {
        Hijack {
            objects: vec![
                (
                    brownfox::Control::Immovable(brownfox::Immovable::new()),
                    object::Object::Maptip(object::maptip::new(
                        0,
                        0,
                        16,
                        16,
                        vec![brownfox::Rectangle::new(0, 0, 20, 15)],
                        "pixelart/maptip/ground/center.png".to_string(),
                        "pixelart/maptip/ground/left.png".to_string(),
                        "pixelart/maptip/ground/top.png".to_string(),
                        "pixelart/maptip/ground/right.png".to_string(),
                        "pixelart/maptip/ground/bottom.png".to_string(),
                        "pixelart/maptip/ground/top-left.png".to_string(),
                        "pixelart/maptip/ground/top-right.png".to_string(),
                        "pixelart/maptip/ground/bottom-left.png".to_string(),
                        "pixelart/maptip/ground/bottom-right.png".to_string(),
                        "pixelart/maptip/ground/top-left_inv.png".to_string(),
                        "pixelart/maptip/ground/top-right_inv.png".to_string(),
                        "pixelart/maptip/ground/bottom-left_inv.png".to_string(),
                        "pixelart/maptip/ground/bottom-right_inv.png".to_string(),
                    )),
                ),
                (
                    brownfox::Control::Immovable(brownfox::Immovable::new()),
                    object::Object::Maptip(object::maptip::new(
                        0,
                        0,
                        16,
                        16,
                        vec![
                            brownfox::Rectangle::new(0, 7, 20, 4),
                            brownfox::Rectangle::new(8, 0, 4, 10),
                        ],
                        "pixelart/maptip/asphalt/center.png".to_string(),
                        "pixelart/maptip/asphalt/left.png".to_string(),
                        "pixelart/maptip/asphalt/top.png".to_string(),
                        "pixelart/maptip/asphalt/right.png".to_string(),
                        "pixelart/maptip/asphalt/bottom.png".to_string(),
                        "pixelart/maptip/asphalt/top-left.png".to_string(),
                        "pixelart/maptip/asphalt/top-right.png".to_string(),
                        "pixelart/maptip/asphalt/bottom-left.png".to_string(),
                        "pixelart/maptip/asphalt/bottom-right.png".to_string(),
                        "pixelart/maptip/asphalt/top-left_inv.png".to_string(),
                        "pixelart/maptip/asphalt/top-right_inv.png".to_string(),
                        "pixelart/maptip/asphalt/bottom-left_inv.png".to_string(),
                        "pixelart/maptip/asphalt/bottom-right_inv.png".to_string(),
                    )),
                ),
                (
                    brownfox::Control::Immovable(brownfox::Immovable::new()),
                    object::Object::Maptip(object::maptip::new(
                        0,
                        0,
                        32,
                        32,
                        vec![
                            brownfox::Rectangle::new(0, 0, 4, 4),
                            brownfox::Rectangle::new(6, 0, 4, 4),
                            brownfox::Rectangle::new(0, 5, 10, 3),
                        ],
                        "pixelart/maptip/tree/center.png".to_string(),
                        "pixelart/maptip/tree/left.png".to_string(),
                        "pixelart/maptip/tree/top.png".to_string(),
                        "pixelart/maptip/tree/right.png".to_string(),
                        "pixelart/maptip/tree/bottom.png".to_string(),
                        "pixelart/maptip/tree/top-left.png".to_string(),
                        "pixelart/maptip/tree/top-right.png".to_string(),
                        "pixelart/maptip/tree/bottom-left.png".to_string(),
                        "pixelart/maptip/tree/bottom-right.png".to_string(),
                        "pixelart/maptip/tree/top-left.png".to_string(),
                        "pixelart/maptip/tree/top-right.png".to_string(),
                        "pixelart/maptip/tree/bottom-left.png".to_string(),
                        "pixelart/maptip/tree/bottom-right.png".to_string(),
                    )),
                ),
                (
                    brownfox::Control::Player(brownfox::Player::new(0)),
                    object::Object::Teiri(object::teiri::new(152, 112)),
                ),
                (
                    brownfox::Control::Immovable(brownfox::Immovable::new()),
                    object::Object::Verity(object::verity::new(144, 96)),
                ),
                (
                    brownfox::Control::Immovable(brownfox::Immovable::new()),
                    object::Object::Emily(object::emily::new(176, 96)),
                ),
                (
                    brownfox::Control::Immovable(brownfox::Immovable::new()),
                    object::Object::Mathprobe(object::mathprobe::new(192, 112)),
                ),
                (
                    brownfox::Control::Immovable(brownfox::Immovable::new()),
                    object::Object::Lonelygem(object::lonelygem::new(192, 128)),
                ),
                (
                    brownfox::Control::Immovable(brownfox::Immovable::new()),
                    object::Object::SecurityDrone(object::security_drone::new(176, 128)),
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
                })
                .collect(),
        )
    }
}

pub const SCALE: i32 = 2;
pub const WIDTH: i32 = 320;
pub const HEIGHT: i32 = 240;
