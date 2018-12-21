pub mod maptip;
pub mod tree;
pub mod teiri;
pub mod verity;
pub mod emily;
pub mod mathprobe;
pub mod lonelygem;
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
    Lonelygem(lonelygem::Lonelygem),
    SecurityDrone(security_drone::SecurityDrone),
    Maptip(maptip::Maptip),
    Tree(tree::Tree),
}

impl brownfox::Moore<Input, Output> for Object {
    fn transit(&self, input: &Input) -> Object {
        match self {
            Object::Teiri(teiri) => Object::Teiri(teiri.transit(input)),
            Object::Verity(verity) => Object::Verity(verity.transit(input)),
            Object::Emily(emily) => Object::Emily(emily.transit(input)),
            Object::Mathprobe(mathprobe) => Object::Mathprobe(mathprobe.transit(input)),
            Object::Lonelygem(lonelygem) => Object::Lonelygem(lonelygem.transit(input)),
            Object::SecurityDrone(security_drone) => Object::SecurityDrone(security_drone.transit(input)),
            Object::Maptip(maptip) => Object::Maptip(maptip.transit(input)),
            Object::Tree(tree) => Object::Tree(tree.transit(input)),
        }
    }

    fn output(&self) -> Output {
        match self {
            Object::Teiri(teiri) => teiri.output(),
            Object::Verity(verity) => verity.output(),
            Object::Emily(emily) => emily.output(),
            Object::Mathprobe(mathprobe) => mathprobe.output(),
            Object::Lonelygem(lonelygem) => lonelygem.output(),
            Object::SecurityDrone(security_drone) => security_drone.output(),
            Object::Maptip(maptip) => maptip.output(),
            Object::Tree(tree) => tree.output(),
        }
    }
}
