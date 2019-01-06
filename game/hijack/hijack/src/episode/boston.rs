use super::*;

pub fn new() -> Episode {
    Episode {
        map: "map/boston/0000-0000.json".to_string(),
        objects: vec![(
            brownfox::Control::Player(brownfox::Player::new(0)),
            object::Object::Teiri(object::teiri::new(64, 32, 0)),
        )],
    }
}