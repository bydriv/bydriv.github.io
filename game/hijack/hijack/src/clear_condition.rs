use super::*;

pub mod flag;

#[derive(Clone)]
pub enum ClearCondition {
    Flag(flag::Flag),
}

impl ClearCondition {
    pub fn satisfy(&self, hijack: &Hijack) -> bool {
        match self {
            ClearCondition::Flag(flag) => flag.satisfy(hijack),
        }
    }
}
