use super::*;

#[derive(Clone)]
pub struct Archimedes {
    x: i32,
    y: i32,
    width: u32,
    height: u32,
}

pub fn new(x: i32, y: i32, width: u32, height: u32) -> Archimedes {
    Archimedes {
        x: x,
        y: y,
        width: width,
        height: height,
    }
}

impl<'a> brownfox::Moore<Input<'a>, Output> for Archimedes {
    fn transit(&self, _input: &Input) -> Archimedes {
        self.clone()
    }

    fn output(&self) -> Output {
        (
            Events { events: vec![] },
            Views {
                views: vec![View::Pattern(
                    "pixelart/maptip/archimedes.png".to_string(),
                    self.width,
                    self.height,
                    self.x,
                    self.y,
                )],
            },
        )
    }
}
