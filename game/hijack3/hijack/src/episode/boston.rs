use super::*;

pub fn new() -> Hijack {
    Hijack {
        x: 0,
        y: 0,
        width: 320,
        height: 240,
        objects: vec![
            (
                brownfox::Control::Player(brownfox::Player::new(0)),
                object::Object::Teiri(object::teiri::new(0, 0, 0)),
            ),
            (
                brownfox::Control::Immovable(brownfox::Immovable::new()),
                object::Object::Maptip(object::maptip::new(
                    0,
                    0,
                    -1000,
                    16,
                    16,
                    vec![brownfox::Rectangle::new(0, 0, 20, 15)],
                    "pixelart/maptip/ground/".to_string(),
                    ".png".to_string(),
                )),
            ),
        ],
        events: vec![],
    }
}
