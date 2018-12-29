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
                    vec![brownfox::Rectangle::new(0, 0, 200, 150)],
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
        ],
        events: vec![],
    }
}
