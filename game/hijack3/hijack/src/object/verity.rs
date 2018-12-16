use super::*;
use brownfox::Shape;

#[derive(Clone)]
enum Pose {
    Walk,
    Hijack,
}

#[derive(Clone)]
enum Direction {
    Left,
    Back,
    Right,
    Front,
}

#[derive(Clone)]
pub struct Verity {
    frame_count: brownfox::FrameCount,
    x: i32,
    y: i32,
    pose: Pose,
    direction: Direction,
    checked: bool,
}

pub fn new(x: i32, y: i32) -> Verity {
    Verity {
        frame_count: brownfox::FrameCount::new(0),
        x: x,
        y: y,
        pose: Pose::Walk,
        direction: Direction::Front,
        checked: false,
    }
}

impl brownfox::Moore<Input, Output> for Verity {
    fn transit(&self, input: &Input) -> Verity {
        let xshift = if input.0.len() > 0 {
            if input.0[0].x < -0.25 {
                -1
            } else if input.0[0].x > 0.25 {
                1
            } else {
                0
            }
        } else {
            0
        };
        let yshift = if input.0.len() > 0 {
            if input.0[0].y < -0.25 {
                -1
            } else if input.0[0].y > 0.25 {
                1
            } else {
                0
            }
        } else {
            0
        };
        let direction = if yshift < 0 {
            Direction::Back
        } else if yshift > 0 {
            Direction::Front
        } else if xshift < 0 {
            Direction::Left
        } else if xshift > 0 {
            Direction::Right
        } else {
            self.direction.clone()
        };
        let mut checked = false;
        for event in &input.1.events {
            match event {
                &Event::Check(x, y, width, height) => {
                    let rect1 =
                        brownfox::Rectangle::new(x as i64, y as i64, width as i64, height as i64);
                    let rect2 = brownfox::Rectangle::new(self.x as i64, self.y as i64, 16, 16);
                    checked = checked || rect1.collision(rect2);
                }
                _ => (),
            }
        }
        Verity {
            frame_count: self.frame_count.transit(&()),
            x: self.x + xshift,
            y: self.y + yshift,
            pose: self.pose.clone(),
            direction: direction,
            checked: checked,
        }
    }

    fn output(&self) -> Output {
        let mut views = vec![View::Image(
            format!(
                "pixelart/verity/walk/{}/{}.png",
                string_of_direction(&self.direction),
                self.frame_count.i / 8 % 4
            ),
            self.x,
            self.y,
        )];
        if self.checked {
            views.append(&mut system::window(self.x - 36, self.y - 32, 11, 4, 5, 3));
            views.append(&mut text::text(
                self.x - 28,
                self.y - 24,
                "ようもないのに、".to_string(),
            ));
            views.append(&mut text::text(
                self.x - 28,
                self.y - 16,
                "はなしかけないで。".to_string(),
            ));
        }
        (vec![], views)
    }
}

fn string_of_direction(direction: &Direction) -> String {
    match direction {
        Direction::Left => "left".to_string(),
        Direction::Back => "back".to_string(),
        Direction::Right => "right".to_string(),
        Direction::Front => "front".to_string(),
    }
}
