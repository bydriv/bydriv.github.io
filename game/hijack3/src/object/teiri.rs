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

impl brownfox::Moore<(&Inputs, &Game), Views> for Teiri {
    fn transit(&self, (inputs, game): &(&Inputs, &Game)) -> Teiri {
        Teiri {
            frame_count: self.frame_count.transit(&()),
            x: self.x,
            y: self.y,
            pose: self.pose.clone(),
            direction: self.direction.clone(),
        }
    }

    fn output(&self) -> Views {
        Views {
            views: vec![View::Image(
                format!(
                    "pixelart/teiri/walk/front/{}.png",
                    self.frame_count.i / 8 % 4
                ),
                self.x,
                self.y,
            )],
        }
    }
}
