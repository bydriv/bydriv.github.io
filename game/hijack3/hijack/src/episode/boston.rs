use super::*;

pub fn new() -> Episode {
    Episode {
        map: "map/boston/1000-1000.json".to_string(),
        objects: vec![(
            brownfox::Control::Player(brownfox::Player::new(0)),
            object::Object::Teiri(object::teiri::new(0, 0, 0)),
        )],
    }
}
