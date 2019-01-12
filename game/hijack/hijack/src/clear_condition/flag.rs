use super::*;

use std::collections::HashMap;

#[derive(Clone)]
pub struct Flag {
    flags: HashMap<String, bool>,
}

impl Flag {
    pub fn new(flags: HashMap<String, bool>) -> Flag {
        Flag { flags: flags }
    }

    pub fn satisfy(&self, hijack: &Hijack) -> bool {
        for (name, value) in self.flags.iter() {
            if hijack.flags().get(name) != Some(value) {
                return false;
            }
        }

        true
    }
}
