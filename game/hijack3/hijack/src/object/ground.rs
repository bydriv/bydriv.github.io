use super::*;

#[derive(Clone)]
pub struct Ground {
    x: i32,
    y: i32,
    width: i32,
    height: i32,
}

pub fn new(x: i32, y: i32, width: i32, height: i32) -> Ground {
    Ground {
        x: x,
        y: y,
        width: width,
        height: height
    }
}

impl brownfox::Moore<Input, Output> for Ground {
    fn transit(&self, _input: &Input) -> Ground {
        self.clone()
    }

    fn output(&self) -> Output {
        let views = (1..self.width - 1).flat_map(|i| {
            (1..self.height - 1).map(move |j| {
                View::Image(
                    "pixelart/maptip/ground/center.png".to_string(),
                    self.x + i * 16,
                    self.y + j * 16,
                )
            })
        });
        let views = views.chain((1..self.width - 1).map(|i| {
            View::Image("pixelart/maptip/ground/top.png".to_string(), self.x + i * 16, self.y)
        }));
        let views = views.chain((1..self.height - 1).map(|j| {
            View::Image("pixelart/maptip/ground/left.png".to_string(), self.x, self.y + j * 16)
        }));
        let views = views.chain((1..self.width - 1).map(|i| {
            View::Image(
                "pixelart/maptip/ground/bottom.png".to_string(),
                self.x + i * 16,
                self.y + (self.height - 1) * 16,
            )
        }));
        let views = views.chain((1..self.height - 1).map(|j| {
            View::Image(
                "pixelart/maptip/ground/right.png".to_string(),
                self.x + (self.width - 1) * 16,
                self.y + j * 16,
            )
        }));
        let views = views.chain(vec![
            View::Image("pixelart/maptip/ground/top-left.png".to_string(), self.x, self.y),
            View::Image(
                "pixelart/maptip/ground/bottom-left.png".to_string(),
                self.x,
                self.y + (self.height - 1) * 16,
            ),
            View::Image(
                "pixelart/maptip/ground/top-right.png".to_string(),
                self.x + (self.width - 1) * 16,
                self.y,
            ),
            View::Image(
                "pixelart/maptip/ground/bottom-right.png".to_string(),
                self.x + (self.width - 1) * 16,
                self.y + (self.height - 1) * 16,
            ),
        ]);

        (
            vec![],
            views.collect(),
        )
    }
}
