use super::*;

use brownfox::Shape;

#[derive(Clone)]
pub struct Maptip {
    x: i32,
    y: i32,
    z: i32,
    width: i32,
    height: i32,
    rectangles: Vec<brownfox::Rectangle<i32>>,
    prefix: String,
    suffix: String,
    names: Vec<(i32, i32, String)>,
}

pub fn new(
    x: i32,
    y: i32,
    z: i32,
    width: i32,
    height: i32,
    rectangles: Vec<brownfox::Rectangle<i32>>,
    prefix: String,
    suffix: String,
) -> Maptip {
    let mut maptip = Maptip {
        x: x,
        y: y,
        z: z,
        width: width,
        height: height,
        rectangles: rectangles,
        prefix: prefix,
        suffix: suffix,
        names: vec![],
    };
    maptip.names = maptip.names();
    maptip
}

impl brownfox::Moore<Input, Output> for Maptip {
    fn transit(&self, input: &Input) -> Maptip {
        self.clone()
    }

    fn output(&self) -> Output {
        let mut views = vec![];

        for (i, j, name) in self.names.clone() {
            let x = self.x + i * self.width;
            let y = self.y + j * self.height;

            views.push(View::Image(name, x, y, self.z));
        }

        Output {
            events: vec![],
            views: views,
        }
    }
}

impl Maptip {
    pub fn transport(&self, from_x: i32, from_y: i32, to_x: i32, to_y: i32) -> Maptip {
        let mut other = self.clone();
        other.x = other.x - from_x + to_x;
        other.y = other.y - from_y + to_y;
        other
    }

    fn names(&self) -> Vec<(i32, i32, String)> {
        let mut names = vec![];

        for rectangle in self.rectangles.clone() {
            for i in 0..rectangle.width {
                for j in 0..rectangle.height {
                    let left = brownfox::Rectangle::new(rectangle.x + i - 1, rectangle.y + j, 1, 1);
                    let top = brownfox::Rectangle::new(rectangle.x + i, rectangle.y + j - 1, 1, 1);
                    let right =
                        brownfox::Rectangle::new(rectangle.x + i + 1, rectangle.y + j, 1, 1);
                    let bottom =
                        brownfox::Rectangle::new(rectangle.x + i, rectangle.y + j + 1, 1, 1);
                    let top_left =
                        brownfox::Rectangle::new(rectangle.x + i - 1, rectangle.y + j - 1, 1, 1);
                    let top_right =
                        brownfox::Rectangle::new(rectangle.x + i + 1, rectangle.y + j - 1, 1, 1);
                    let bottom_left =
                        brownfox::Rectangle::new(rectangle.x + i - 1, rectangle.y + j + 1, 1, 1);
                    let bottom_right =
                        brownfox::Rectangle::new(rectangle.x + i + 1, rectangle.y + j + 1, 1, 1);

                    let l = self.rectangles.iter().any(|r| r.collision(left.clone()));
                    let t = self.rectangles.iter().any(|r| r.collision(top.clone()));
                    let r = self.rectangles.iter().any(|r| r.collision(right.clone()));
                    let b = self.rectangles.iter().any(|r| r.collision(bottom.clone()));
                    let tl = self
                        .rectangles
                        .iter()
                        .any(|r| r.collision(top_left.clone()));
                    let tr = self
                        .rectangles
                        .iter()
                        .any(|r| r.collision(top_right.clone()));
                    let bl = self
                        .rectangles
                        .iter()
                        .any(|r| r.collision(bottom_left.clone()));
                    let br = self
                        .rectangles
                        .iter()
                        .any(|r| r.collision(bottom_right.clone()));

                    let name = match (l, t, r, b, tl, tr, bl, br) {
                        (true, true, true, true, true, true, true, true) => {
                            format!("{}center{}", self.prefix, self.suffix)
                        }
                        (false, true, true, true, _, _, _, _) => {
                            format!("{}left{}", self.prefix, self.suffix)
                        }
                        (true, false, true, true, _, _, _, _) => {
                            format!("{}top{}", self.prefix, self.suffix)
                        }
                        (true, true, false, true, _, _, _, _) => {
                            format!("{}right{}", self.prefix, self.suffix)
                        }
                        (true, true, true, false, _, _, _, _) => {
                            format!("{}bottom{}", self.prefix, self.suffix)
                        }
                        (false, false, true, true, _, _, _, _) => {
                            format!("{}top-left{}", self.prefix, self.suffix)
                        }
                        (true, false, false, true, _, _, _, _) => {
                            format!("{}top-right{}", self.prefix, self.suffix)
                        }
                        (false, true, true, false, _, _, _, _) => {
                            format!("{}bottom-left{}", self.prefix, self.suffix)
                        }
                        (true, true, false, false, _, _, _, _) => {
                            format!("{}bottom-right{}", self.prefix, self.suffix)
                        }
                        (true, true, true, true, true, true, true, false) => {
                            format!("{}top-left_inv{}", self.prefix, self.suffix)
                        }
                        (true, true, true, true, true, true, false, true) => {
                            format!("{}top-right_inv{}", self.prefix, self.suffix)
                        }
                        (true, true, true, true, true, false, true, true) => {
                            format!("{}bottom-left_inv{}", self.prefix, self.suffix)
                        }
                        (true, true, true, true, false, true, true, true) => {
                            format!("{}bottom-right_inv{}", self.prefix, self.suffix)
                        }
                        _ => format!("{}center{}", self.prefix, self.suffix),
                    };

                    names.push((rectangle.x + i, rectangle.y + j, name));
                }
            }
        }

        names
    }
}
