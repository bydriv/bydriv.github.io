pub mod teiri;

use super::*;

#[derive(Clone)]
pub enum Object {
    Teiri(teiri::Teiri),
}

impl brownfox::Moore<(&Inputs, &Game), Views> for Object {
    fn transit(&self, input: &(&Inputs, &Game)) -> Object {
        match self {
            Object::Teiri(teiri) => Object::Teiri(teiri.transit(input)),
        }
    }

    fn output(&self) -> Views {
        match self {
            Object::Teiri(teiri) => teiri.output(),
        }
    }
}
