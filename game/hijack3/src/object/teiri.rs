use super::*;

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
pub struct Teiri {
    frame_count: brownfox::FrameCount,
    x: i32,
    y: i32,
    pose: Pose,
    direction: Direction,
}

pub fn new(x: i32, y: i32) -> Teiri {
    Teiri {
        frame_count: brownfox::FrameCount::new(0),
        x: x,
        y: y,
        pose: Pose::Walk,
        direction: Direction::Front,
    }
}

impl brownfox::Moore<(&Inputs, &Game), Output> for Teiri {
    fn transit(&self, (inputs, game): &(&Inputs, &Game)) -> Teiri {
        let xshift = if let Some(x) = input_x(0, inputs) {
            if x < -0.25 {
                -1
            } else if x > 0.25 {
                1
            } else {
                0
            }
        } else {
            0
        };
        let yshift = if let Some(y) = input_y(0, inputs) {
            if y < -0.25 {
                -1
            } else if y > 0.25 {
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
        Teiri {
            frame_count: self.frame_count.transit(&()),
            x: self.x + xshift,
            y: self.y + yshift,
            pose: self.pose.clone(),
            direction: direction,
        }
    }

    fn output(&self) -> Output {
        (
            Events {
                events: vec![Event::Focus(self.x, self.y, 16, 16)],
            },
            Views {
                views: vec![View::Image(
                    format!(
                        "pixelart/teiri/walk/{}/{}.png",
                        string_of_direction(&self.direction),
                        self.frame_count.i / 8 % 4
                    ),
                    self.x,
                    self.y,
                )],
            },
        )
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
