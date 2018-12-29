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
pub struct NPC {
    frame_count: brownfox::FrameCount,
    x: i32,
    y: i32,
    z: i32,
    pose: Pose,
    direction: Direction,
    name: String,
}

pub fn new(x: i32, y: i32, z: i32, name: String) -> NPC {
    NPC {
        frame_count: brownfox::FrameCount::new(0),
        x: x,
        y: y,
        z: z,
        pose: Pose::Walk,
        direction: Direction::Front,
        name: name,
    }
}

impl brownfox::Moore<Input, Output> for NPC {
    fn transit(&self, input: &Input) -> NPC {
        NPC {
            frame_count: self.frame_count.transit(&()),
            x: self.x,
            y: self.y,
            z: self.z,
            pose: self.pose.clone(),
            direction: self.direction.clone(),
            name: self.name.clone(),
        }
    }

    fn output(&self) -> Output {
        let mut views = vec![View::Image(
            format!(
                "{}/walk/{}/{}.png",
                self.name,
                string_of_direction(&self.direction),
                self.frame_count.i / 8 % 4
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
