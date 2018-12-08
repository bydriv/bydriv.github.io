pub mod teiri;

use super::*;

pub enum Object {
    Teiri(teiri::Teiri),
}

pub fn step(inputs: &Inputs, object: &Object) -> Object {
    match object {
        Object::Teiri(teiri) => Object::Teiri(teiri::step(inputs, teiri)),
    }
}

pub fn views(object: &Object) -> Views {
    match object {
        Object::Teiri(teiri) => teiri::views(teiri),
    }
}
