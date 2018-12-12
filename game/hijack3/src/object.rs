pub mod archimedes;
pub mod teiri;
pub mod verity;

use super::*;

pub type Output = (Events, Views);

#[derive(Clone)]
pub enum Object {
    Teiri(teiri::Teiri),
    Verity(verity::Verity),
    Archimedes(archimedes::Archimedes),
}

impl brownfox::Moore<(&Inputs, &Game), Output> for Object {
    fn transit(&self, input: &(&Inputs, &Game)) -> Object {
        match self {
            Object::Teiri(teiri) => Object::Teiri(teiri.transit(input)),
            Object::Verity(verity) => Object::Verity(verity.transit(input)),
            Object::Archimedes(archimedes) => Object::Archimedes(archimedes.transit(input)),
        }
    }

    fn output(&self) -> Output {
        match self {
            Object::Teiri(teiri) => teiri.output(),
            Object::Verity(verity) => verity.output(),
            Object::Archimedes(archimedes) => archimedes.output(),
        }
    }
}
