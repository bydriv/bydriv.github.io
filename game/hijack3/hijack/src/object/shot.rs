use super::*;
use brownfox::Shape;

#[derive(Clone)]
pub enum Direction {
    Left,
    Back,
    Right,
    Front,
}

#[derive(Clone)]
pub struct Shot {
    frame_count: brownfox::FrameCount<i32>,
    x: i32,
    y: i32,
    z: i32,
    direction: Direction,
}

pub fn new(x: i32, y: i32, z: i32, direction: Direction) -> Shot {
    Shot {
        frame_count: brownfox::FrameCount::new(0),
        x: x,
        y: y,
        z: z,
        direction: direction,
    }
}

impl brownfox::Moore<Input, Output> for Shot {
    fn transit(&self, input: &Input) -> Shot {
        let mut other = self.clone();
        other.frame_count = other.frame_count.transit(&());
        match self.direction {
            Direction::Left => {
                other.x -= 60 / input.previous.fps * 8;
            }
            Direction::Back => {
                other.y -= 60 / input.previous.fps * 8;
            }
            Direction::Right => {
                other.x += 60 / input.previous.fps * 8;
            }
            Direction::Front => {
                other.y += 60 / input.previous.fps * 8;
            }
        }
        other
    }

    fn output(&self) -> Output {
        Output {
            events: vec![Event::Attack(brownfox::Rectangle::new(
                self.x, self.y, 2, 2,
            ))],
            views: vec![View::Image(
                "pixelart/effect/shot.png".to_string(),
                self.x - 7,
                self.y - 7,
                self.z,
            )],
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

impl Shot {
    pub fn transport(&self, from_x: i32, from_y: i32, to_x: i32, to_y: i32) -> Shot {
        let mut other = self.clone();
        other.x = other.x - from_x + to_x;
        other.y = other.y - from_y + to_y;
        other
    }
}
