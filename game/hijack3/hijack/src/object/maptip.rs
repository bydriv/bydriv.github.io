use super::*;

#[derive(Clone)]
pub struct Maptip {
    x: i32,
    y: i32,
    width: u32,
    height: u32,
    name: String,
}

pub fn new(x: i32, y: i32, width: u32, height: u32, name: &String) -> Maptip {
    Maptip {
        x: x,
        y: y,
        width: width,
        height: height,
        name: name.clone(),
    }
}

impl brownfox::Moore<Input, Output> for Maptip {
    fn transit(&self, _input: &Input) -> Maptip {
        self.clone()
    }

    fn output(&self) -> Output {
        (
            vec![],
            vec![View::Image(
                self.name.clone(),
                self.x,
                self.y,
            )],
        )
    }
}
