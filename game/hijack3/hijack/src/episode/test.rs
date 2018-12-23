use super::*;

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
                object::Object::Emily(object::emily::new(175, 96)),
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
