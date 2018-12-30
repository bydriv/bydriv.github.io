pub mod animated;
pub mod drone;
pub mod maptip;
pub mod npc;
pub mod teiri;
pub mod transport;

use super::*;

pub struct Input {
    pub inputs: Vec<brownfox::Input>,
    pub previous: Hijack,
}

pub struct Output {
    pub events: Vec<Event>,
    pub views: Vec<View>,
}

#[derive(Clone)]
pub enum Object {
    Teiri(teiri::Teiri),
    NPC(npc::NPC),
    Drone(drone::Drone),
    Maptip(maptip::Maptip),
    Animated(animated::Animated),
    Transport(transport::Transport),
}

impl brownfox::Moore<Input, Output> for Object {
    fn transit(&self, input: &Input) -> Object {
        match self {
            Object::Teiri(teiri) => Object::Teiri(teiri.transit(input)),
            Object::NPC(npc) => Object::NPC(npc.transit(input)),
            Object::Drone(drone) => Object::Drone(drone.transit(input)),
            Object::Maptip(maptip) => Object::Maptip(maptip.transit(input)),
            Object::Animated(animated) => Object::Animated(animated.transit(input)),
            Object::Transport(transport) => Object::Transport(transport.transit(input)),
        }
    }

    fn output(&self) -> Output {
        match self {
            Object::Teiri(teiri) => teiri.output(),
            Object::NPC(npc) => npc.output(),
            Object::Drone(drone) => drone.output(),
            Object::Maptip(maptip) => maptip.output(),
            Object::Animated(animated) => animated.output(),
            Object::Transport(transport) => transport.output(),
        }
    }
}

impl Object {
    pub fn transport(&self, from_x: i32, from_y: i32, to_x: i32, to_y: i32) -> Object {
        match self {
            Object::Teiri(teiri) => Object::Teiri(teiri.transport(from_x, from_y, to_x, to_y)),
            Object::NPC(npc) => Object::NPC(npc.transport(from_x, from_y, to_x, to_y)),
            Object::Drone(drone) => Object::Drone(drone.transport(from_x, from_y, to_x, to_y)),
            Object::Maptip(maptip) => Object::Maptip(maptip.transport(from_x, from_y, to_x, to_y)),
            Object::Animated(animated) => {
                Object::Animated(animated.transport(from_x, from_y, to_x, to_y))
            }
            Object::Transport(transport) => {
                Object::Transport(transport.transport(from_x, from_y, to_x, to_y))
            }
        }
    }
}
