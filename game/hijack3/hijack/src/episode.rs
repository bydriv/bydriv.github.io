use super::*;

pub mod boston;

#[derive(Clone)]
pub struct Episode {
    pub map: String,
    pub objects: Vec<(brownfox::Control, object::Object)>,
}
