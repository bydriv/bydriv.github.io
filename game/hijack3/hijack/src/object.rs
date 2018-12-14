pub mod archimedes;
pub mod teiri;
pub mod verity;

use super::*;

pub type Input = (Vec<brownfox::Input>, Hijack);
pub type Output = (Vec<Event>, Vec<View>);

#[derive(Clone)]
pub enum Object {
    Teiri(teiri::Teiri),
    Verity(verity::Verity),
    Archimedes(archimedes::Archimedes),
}

impl brownfox::Moore<Input, Output> for Object {
    fn transit(&self, input: &Input) -> Object {
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
