use super::*;

#[derive(Clone)]
pub struct Maptip {
    x: i32,
    y: i32,
    xskip: i32,
    yskip: i32,
    width: u32,
    height: u32,
    name: String,
}

pub fn new(
    x: i32,
    y: i32,
    xskip: i32,
    yskip: i32,
    width: u32,
    height: u32,
    name: &String,
) -> Maptip {
    Maptip {
        x: x,
        y: y,
        xskip: xskip,
        yskip: yskip,
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
            (0..self.width as i32)
                .flat_map(|i| {
                    (0..self.height as i32).map(move |j| {
                        View::Image(
                            self.name.clone(),
                            self.x + i * self.xskip,
                            self.y + j * self.yskip,
                        )
                    })
                })
                .collect(),
        )
    }
}
