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
    frame_count: brownfox::FrameCount<i32>,
    x: i32,
    y: i32,
    z: i32,
    pose: Pose,
    direction: Direction,
    name: String,
}

pub fn new(x: i32, y: i32, z: i32, name: String) -> Drone {
    Drone {
        frame_count: brownfox::FrameCount::new(0),
        x: x,
        y: y,
        z: z,
        pose: Pose::Fly,
        direction: Direction::Front,
        name: name,
    }
}

impl brownfox::Moore<Input, Output> for Drone {
    fn transit(&self, input: &Input) -> Drone {
        let xshift = if input.inputs.len() > 0 {
            if input.inputs[0].x < -0.25 {
                -(60 / input.previous.fps)
            } else if input.inputs[0].x > 0.25 {
                60 / input.previous.fps
            } else {
                0
            }
        } else {
            0
        };

        let yshift = if input.inputs.len() > 0 {
            if input.inputs[0].y < -0.25 {
                -(60 / input.previous.fps)
            } else if input.inputs[0].y > 0.25 {
                60 / input.previous.fps
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
            frame_count: (0..60 / input.previous.fps)
                .fold(self.frame_count.clone(), |control, _| control.transit(&())),
            x: self.x + xshift,
            y: self.y + yshift,
            z: self.z,
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
                self.frame_count.output() % 4
            ),
            self.x,
            self.y,
            self.z,
        )];

        Output {
            events: vec![],
            views: views,
        }
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

impl Drone {
    pub fn transport(&self, from_x: i32, from_y: i32, to_x: i32, to_y: i32) -> Drone {
        let mut other = self.clone();
        other.x = other.x - from_x + to_x;
        other.y = other.y - from_y + to_y;
        other
    }
}
