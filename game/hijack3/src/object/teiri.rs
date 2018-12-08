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

pub struct Teiri {
    i: u32,
    x: i32,
    y: i32,
    pose: Pose,
    direction: Direction,
}

pub fn new(x: i32, y: i32) -> Teiri {
    Teiri {
        i: 0,
        x: x,
        y: y,
        pose: Pose::Walk,
        direction: Direction::Front,
    }
}

pub fn step(inputs: &Inputs, teiri: &Teiri) -> Teiri {
    Teiri {
        i: teiri.i + 1,
        x: teiri.x,
        y: teiri.y,
        pose: teiri.pose.clone(),
        direction: teiri.direction.clone(),
    }
}

pub fn views(teiri: &Teiri) -> Views {
    Views {
        views: vec![View::Image(
            format!("pixelart/teiri/walk/front/{}.png", teiri.i / 8 % 4),
            teiri.x,
            teiri.y,
        )],
    }
}
