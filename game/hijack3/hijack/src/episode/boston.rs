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
        object::Object::Teiri(object::teiri::new(192, -128, 0))
      ),
      (
        brownfox::Control::Immovable(brownfox::Immovable::new()),
        object::Object::Maptip(object::maptip::new(
          0,
          -32,
          -1000,
          16,
          16,
          vec![
            brownfox::Rectangle::new(0, 0, 200, 152),
          ],
          "pixelart/maptip/ground/".to_string(),
          ".png".to_string(),
        ))
      ),
      (
        brownfox::Control::Immovable(brownfox::Immovable::new()),
        object::Object::Maptip(object::maptip::new(
          0,
          -480,
          -1000,
          16,
          16,
          vec![
            brownfox::Rectangle::new(3, 0, 8, 33),
            brownfox::Rectangle::new(3, 33, 30, 8),
          ],
          "pixelart/maptip/asphalt/".to_string(),
          ".png".to_string(),
        ))
      ),
      (
        brownfox::Control::Immovable(brownfox::Immovable::new()),
        object::Object::Maptip(object::maptip::new(
          0,
          -480,
          -1000,
          16,
          16,
          vec![
            brownfox::Rectangle::new(0, 0, 4, 30),
            brownfox::Rectangle::new(10, 0, 4, 30),
            brownfox::Rectangle::new(10, 30, 30, 4),
          ],
          "pixelart/maptip/mortar/".to_string(),
          ".png".to_string(),
        ))
      ),
      (
        brownfox::Control::Immovable(brownfox::Immovable::new()),
        object::Object::Maptip(object::maptip::new(
          16,
          -304,
          -1000,
          32,
          128,
          vec![
            brownfox::Rectangle::new(0, 0, 1, 1),
          ],
          "pixelart/maptip/halfway-to-hell/left/".to_string(),
          ".png".to_string(),
        ))
      ),
      (
        brownfox::Control::Immovable(brownfox::Immovable::new()),
        object::Object::Maptip(object::maptip::new(
          176,
          -304,
          -1000,
          32,
          128,
          vec![
            brownfox::Rectangle::new(0, 0, 1, 1),
          ],
          "pixelart/maptip/halfway-to-hell/right/".to_string(),
          ".png".to_string(),
        ))
      ),
      (
        brownfox::Control::Immovable(brownfox::Immovable::new()),
        object::Object::Maptip(object::maptip::new(
          44,
          -480,
          -1000,
          32,
          32,
          vec![
            brownfox::Rectangle::new(0, 0, 1, 15),
          ],
          "pixelart/maptip/street-light/right/".to_string(),
          ".png".to_string(),
        ))
      ),
      (
        brownfox::Control::Immovable(brownfox::Immovable::new()),
        object::Object::Maptip(object::maptip::new(
          148,
          -480,
          -1000,
          32,
          32,
          vec![
            brownfox::Rectangle::new(0, 0, 1, 15),
          ],
          "pixelart/maptip/street-light/left/".to_string(),
          ".png".to_string(),
        ))
      ),
      (
        brownfox::Control::Immovable(brownfox::Immovable::new()),
        object::Object::Maptip(object::maptip::new(
          192,
          -64,
          -1000,
          32,
          32,
          vec![
            brownfox::Rectangle::new(0, 0, 70, 3),
          ],
          "pixelart/maptip/tree/".to_string(),
          ".png".to_string(),
        ))
      ),
      (
        brownfox::Control::Immovable(brownfox::Immovable::new()),
        object::Object::Maptip(object::maptip::new(
          256,
          -128,
          1000,
          16,
          16,
          vec![
            brownfox::Rectangle::new(0, 0, 8, 6),
          ],
          "pixelart/maptip/building/top/".to_string(),
          ".png".to_string(),
        ))
      ),
      (
        brownfox::Control::Immovable(brownfox::Immovable::new()),
        object::Object::Maptip(object::maptip::new(
          256,
          -128,
          1000,
          16,
          16,
          vec![
            brownfox::Rectangle::new(0, 5, 8, 20),
          ],
          "pixelart/maptip/building/bottom/".to_string(),
          ".png".to_string(),
        ))
      ),
    ],
    events: vec![],
  }
}
