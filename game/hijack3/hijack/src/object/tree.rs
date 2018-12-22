use super::*;

#[derive(Clone)]
pub struct Tree {
    x: i32,
    y: i32,
    width: i32,
    height: i32,
}

pub fn new(x: i32, y: i32, width: i32, height: i32) -> Tree {
    Tree {
        x: x,
        y: y,
        width: width,
        height: height
    }
}

impl brownfox::Moore<Input, Output> for Tree {
    fn transit(&self, _input: &Input) -> Tree {
        self.clone()
    }

    fn output(&self) -> Output {
        let views = (1..self.width - 1).flat_map(|i| {
            (1..self.height - 1).map(move |j| {
                View::Image(
                    "pixelart/maptip/tree/center.png".to_string(),
                    self.x + i * 32,
                    self.y + j * 32,
                )
            })
        });
        let views = views.chain((1..self.width - 1).map(|i| {
            View::Image("pixelart/maptip/tree/top.png".to_string(), self.x + i * 32, self.y)
        }));
        let views = views.chain((1..self.height - 1).map(|j| {
            View::Image("pixelart/maptip/tree/left.png".to_string(), self.x, self.y + j * 32)
        }));
        let views = views.chain((1..self.width - 1).map(|i| {
            View::Image(
                "pixelart/maptip/tree/bottom.png".to_string(),
                self.x + i * 32,
                self.y + (self.height - 1) * 32,
            )
        }));
        let views = views.chain((1..self.height - 1).map(|j| {
            View::Image(
                "pixelart/maptip/tree/right.png".to_string(),
                self.x + (self.width - 1) * 32,
                self.y + j * 32,
            )
        }));
        let views = views.chain(vec![
            View::Image("pixelart/maptip/tree/top-left.png".to_string(), self.x, self.y),
            View::Image(
                "pixelart/maptip/tree/bottom-left.png".to_string(),
                self.x,
                self.y + (self.height - 1) * 32,
            ),
            View::Image(
                "pixelart/maptip/tree/top-right.png".to_string(),
                self.x + (self.width - 1) * 32,
                self.y,
            ),
            View::Image(
                "pixelart/maptip/tree/bottom-right.png".to_string(),
                self.x + (self.width - 1) * 32,
                self.y + (self.height - 1) * 32,
            ),
        ]);

        (
            vec![],
            views.collect(),
        )
    }
}
