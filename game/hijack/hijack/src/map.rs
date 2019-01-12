use super::*;
use std::collections::HashMap;

pub mod boston;

#[derive(Clone)]
pub struct Map {
    pub x: i32,
    pub y: i32,
    pub width: i32,
    pub height: i32,
    pub objects: Vec<(brownfox::Control<i32>, object::Object)>,
}
