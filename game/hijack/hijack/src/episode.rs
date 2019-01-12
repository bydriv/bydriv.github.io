use super::*;

pub mod boston;

#[derive(Clone)]
pub struct Episode {
    pub map: String,
    pub objects: Vec<(brownfox::Control<i32>, object::Object)>,
    pub clear_condition: clear_condition::ClearCondition,
}
