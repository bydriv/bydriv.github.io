use super::*;
use brownfox::Shape;

#[derive(Clone)]
enum Pose {
    Fly,
}

#[derive(Clone)]
enum Direction {
    Left,
    Back,
    Right,
    Front,
}

#[derive(Clone)]
pub struct Drone {
    frame_count: brownfox::FrameCount,
    x: i32,
    y: i32,
    pose: Pose,
    direction: Direction,
    name: String,
}

pub fn new(x: i32, y: i32, name: String) -> Drone {
    Drone {
        frame_count: brownfox::FrameCount::new(0),
        x: x,
        y: y,
        pose: Pose::Fly,
        direction: Direction::Front,
        name: name,
    }
}

impl brownfox::Moore<Input, Output> for Drone {
    fn transit(&self, input: &Input) -> Drone {
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
        Drone {
            frame_count: self.frame_count.transit(&()),
            x: self.x + xshift,
            y: self.y + yshift,
            pose: self.pose.clone(),
            direction: direction,
            name: self.name.clone(),
        }
    }

    fn output(&self) -> Output {
        let views = vec![View::Image(
            format!(
                "{}/fly/{}/{}.png",
                self.name,
                string_of_direction(&self.direction),
                self.frame_count.i % 4
            ),
            self.x,
            self.y,
        )];
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
