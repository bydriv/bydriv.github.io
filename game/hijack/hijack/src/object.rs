pub mod animated;
pub mod drone;
pub mod maptip;
pub mod npc;
pub mod shot;
pub mod teiri;
pub mod transport;

use super::*;

pub struct Input {
    pub inputs: Vec<brownfox::Input>,
    pub previous: Hijack,
}

pub struct Output {
    pub instrs: Vec<Instr>,
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
    pub fn id(&self) -> String {
        match self {
            Object::Teiri(teiri) => teiri.id(),
            Object::NPC(npc) => npc.id(),
            Object::Drone(drone) => drone.id(),
            Object::Maptip(maptip) => maptip.id(),
            Object::Animated(animated) => animated.id(),
            Object::Transport(transport) => transport.id(),
        }
    }

    pub fn x(&self) -> i32 {
        match self {
            Object::Teiri(teiri) => teiri.x(),
            Object::NPC(npc) => npc.x(),
            Object::Drone(drone) => drone.x(),
            Object::Maptip(maptip) => maptip.x(),
            Object::Animated(animated) => animated.x(),
            Object::Transport(transport) => transport.x(),
        }
    }

    pub fn y(&self) -> i32 {
        match self {
            Object::Teiri(teiri) => teiri.y(),
            Object::NPC(npc) => npc.y(),
            Object::Drone(drone) => drone.y(),
            Object::Maptip(maptip) => maptip.y(),
            Object::Animated(animated) => animated.y(),
            Object::Transport(transport) => transport.y(),
        }
    }

    pub fn z(&self) -> i32 {
        match self {
            Object::Teiri(teiri) => teiri.z(),
            Object::NPC(npc) => npc.z(),
            Object::Drone(drone) => drone.z(),
            Object::Maptip(maptip) => maptip.z(),
            Object::Animated(animated) => animated.z(),
            Object::Transport(transport) => transport.z(),
        }
    }

    pub fn on(&self, event: &Event) -> Object {
        match self {
            Object::Teiri(teiri) => Object::Teiri(teiri.on(event)),
            Object::NPC(npc) => Object::NPC(npc.on(event)),
            Object::Drone(drone) => Object::Drone(drone.on(event)),
            Object::Maptip(maptip) => Object::Maptip(maptip.on(event)),
            Object::Animated(animated) => Object::Animated(animated.on(event)),
            Object::Transport(transport) => Object::Transport(transport.on(event)),
        }
    }

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
