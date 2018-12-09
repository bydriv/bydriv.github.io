pub mod archimedes;
pub mod teiri;

use super::*;

#[derive(Clone)]
pub enum Object {
    Teiri(teiri::Teiri),
    Archimedes(archimedes::Archimedes),
}

impl brownfox::Moore<(&Inputs, &Game), Views> for Object {
    fn transit(&self, input: &(&Inputs, &Game)) -> Object {
        match self {
            Object::Teiri(teiri) => Object::Teiri(teiri.transit(input)),
            Object::Archimedes(archimedes) => Object::Archimedes(archimedes.transit(input)),
        }
    }

    fn output(&self) -> Views {
        match self {
            Object::Teiri(teiri) => teiri.output(),
            Object::Archimedes(archimedes) => archimedes.output(),
        }
    }
}
