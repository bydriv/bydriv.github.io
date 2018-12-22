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
        height: height,
    }
}

impl brownfox::Moore<Input, Output> for Tree {
    fn transit(&self, _input: &Input) -> Tree {
        self.clone()
    }

    fn output(&self) -> Output {
        let views = vec![
            View::Pattern(
                "pixelart/maptip/tree/center.png".to_string(),
                self.width as u32 - 2,
                self.height as u32 - 2,
                self.x + 32,
                self.y + 32,
            ),
            View::Pattern(
                "pixelart/maptip/tree/top.png".to_string(),
                self.width as u32 - 2,
                1,
                self.x + 32,
                self.y,
            ),
            View::Pattern(
                "pixelart/maptip/tree/left.png".to_string(),
                1,
                self.height as u32 - 2,
                self.x,
                self.y + 32,
            ),
            View::Pattern(
                "pixelart/maptip/tree/bottom.png".to_string(),
                self.width as u32 - 2,
                1,
                self.x + 32,
                self.y + (self.height - 1) * 32,
            ),
            View::Pattern(
                "pixelart/maptip/tree/right.png".to_string(),
                1,
                self.height as u32 - 2,
                self.x + (self.width - 1) * 32,
                self.y + 32,
            ),
            View::Image(
                "pixelart/maptip/tree/top-left.png".to_string(),
                self.x,
                self.y,
            ),
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
        ];

        (vec![], views)
    }
}
