use super::*;

pub fn new() -> Episode {
    Episode {
        map: "map/boston/0000-0000.json".to_string(),
        objects: vec![(
            brownfox::Control::Player(brownfox::Player::new(0)),
            object::Object::Teiri(object::teiri::new("teiri".to_string(), 64, 32, 0)),
        )],
        clear_condition: clear_condition::ClearCondition::Flag(clear_condition::flag::Flag::new(
            vec![
                ("map/boston/0000-0000/drone-0000/disabled".to_string(), true),
                ("map/boston/0000-0000/drone-0001/disabled".to_string(), true),
                ("map/boston/0000-0000/drone-0002/disabled".to_string(), true),
                ("map/boston/0000-0000/drone-0003/disabled".to_string(), true),
            ]
            .into_iter()
            .collect(),
        )),
    }
}
