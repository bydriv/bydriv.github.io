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

impl brownfox::Moore<(&Inputs, &Game), Views> for Archimedes {
    fn transit(&self, (inputs, game): &(&Inputs, &Game)) -> Archimedes {
        self.clone()
    }

    fn output(&self) -> Views {
        Views {
            views: (0..self.width)
                .flat_map(|x| {
                    (0..self.height).map(move |y| {
                        View::Image(
                            format!("pixelart/maptip/archimedes/{}-{}.png", x % 2, y % 2),
                            self.x + (x as i32) * 16,
                            self.y + (y as i32) * 16,
                        )
                    })
                })
                .collect(),
        }
    }
}
