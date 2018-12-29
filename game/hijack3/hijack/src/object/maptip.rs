use super::*;

use brownfox::Shape;

#[derive(Clone)]
pub struct Maptip {
    x: i32,
    y: i32,
    z: i32,
    width: i32,
    height: i32,
    rectangles: Vec<brownfox::Rectangle>,
    center: String,
    left: String,
    top: String,
    right: String,
    bottom: String,
    top_left: String,
    top_right: String,
    bottom_left: String,
    bottom_right: String,
    top_left_inv: String,
    top_right_inv: String,
    bottom_left_inv: String,
    bottom_right_inv: String,
    rectangle: brownfox::Rectangle,
}

pub fn new(
    x: i32,
    y: i32,
    z: i32,
    width: i32,
    height: i32,
    rectangles: Vec<brownfox::Rectangle>,
    center: String,
    left: String,
    top: String,
    right: String,
    bottom: String,
    top_left: String,
    top_right: String,
    bottom_left: String,
    bottom_right: String,
    top_left_inv: String,
    top_right_inv: String,
    bottom_left_inv: String,
    bottom_right_inv: String,
) -> Maptip {
    Maptip {
        x: x,
        y: y,
        z: z,
        width: width,
        height: height,
        rectangles: rectangles,
        center: center,
        left: left,
        top: top,
        right: right,
        bottom: bottom,
        top_left: top_left,
        top_right: top_right,
        bottom_left: bottom_left,
        bottom_right: bottom_right,
        top_left_inv: top_left_inv,
        top_right_inv: top_right_inv,
        bottom_left_inv: bottom_left_inv,
        bottom_right_inv: bottom_right_inv,
        rectangle: brownfox::Rectangle::new(0, 0, 0, 0),
    }
}

impl brownfox::Moore<Input, Output> for Maptip {
    fn transit(&self, input: &Input) -> Maptip {
        let mut other = self.clone();
        other.rectangle = brownfox::Rectangle::new(
            ((input.previous.x - self.x) / self.width - 1).into(),
            ((input.previous.y - self.y) / self.height - 1).into(),
            (input.previous.width / self.width + 3).into(),
            (input.previous.height / self.height + 3).into(),
        );
        other
    }

    fn output(&self) -> Output {
        let mut views = vec![];

        for rectangle in self.rectangles.clone() {
            let rectangle = self.rectangle.intersection(rectangle);

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
                        (true, true, true, true, true, true, true, true) => self.center.clone(),
                        (false, true, true, true, _, _, _, _) => self.left.clone(),
                        (true, false, true, true, _, _, _, _) => self.top.clone(),
                        (true, true, false, true, _, _, _, _) => self.right.clone(),
                        (true, true, true, false, _, _, _, _) => self.bottom.clone(),
                        (false, false, true, true, _, _, _, _) => self.top_left.clone(),
                        (true, false, false, true, _, _, _, _) => self.top_right.clone(),
                        (false, true, true, false, _, _, _, _) => self.bottom_left.clone(),
                        (true, true, false, false, _, _, _, _) => self.bottom_right.clone(),
                        (true, true, true, true, true, true, true, false) => {
                            self.top_left_inv.clone()
                        }
                        (true, true, true, true, true, true, false, true) => {
                            self.top_right_inv.clone()
                        }
                        (true, true, true, true, true, false, true, true) => {
                            self.bottom_left_inv.clone()
                        }
                        (true, true, true, true, false, true, true, true) => {
                            self.bottom_right_inv.clone()
                        }
                        _ => self.center.clone(),
                    };

                    let x = self.x + (rectangle.x + i) as i32 * self.width;
                    let y = self.y + (rectangle.y + j) as i32 * self.height;

                    views.push(View::Image(name, x, y, self.z));
                }
            }
        }

        Output {
            events: vec![],
            views: views,
        }
    }
}
