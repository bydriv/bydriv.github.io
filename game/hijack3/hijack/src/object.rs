pub mod archimedes;
pub mod teiri;
pub mod verity;
pub mod emily;
pub mod mathprobe;
pub mod security_drone;

use super::*;

pub type Input = (Vec<brownfox::Input>, Hijack);
pub type Output = (Vec<Event>, Vec<View>);

#[derive(Clone)]
pub enum Object {
    Teiri(teiri::Teiri),
    Verity(verity::Verity),
    Emily(emily::Emily),
    Mathprobe(mathprobe::Mathprobe),
    SecurityDrone(security_drone::SecurityDrone),
    Archimedes(archimedes::Archimedes),
}

impl brownfox::Moore<Input, Output> for Object {
    fn transit(&self, input: &Input) -> Object {
        match self {
            Object::Teiri(teiri) => Object::Teiri(teiri.transit(input)),
            Object::Verity(verity) => Object::Verity(verity.transit(input)),
            Object::Emily(emily) => Object::Emily(emily.transit(input)),
            Object::Mathprobe(mathprobe) => Object::Mathprobe(mathprobe.transit(input)),
            Object::SecurityDrone(security_drone) => Object::SecurityDrone(security_drone.transit(input)),
            Object::Archimedes(archimedes) => Object::Archimedes(archimedes.transit(input)),
        }
    }

    fn output(&self) -> Output {
        match self {
            Object::Teiri(teiri) => teiri.output(),
            Object::Verity(verity) => verity.output(),
            Object::Emily(emily) => emily.output(),
            Object::Mathprobe(mathprobe) => mathprobe.output(),
            Object::SecurityDrone(security_drone) => security_drone.output(),
            Object::Archimedes(archimedes) => archimedes.output(),
        }
    }
}
