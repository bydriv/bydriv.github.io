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
    frame_count: brownfox::FrameCount<i32>,
    id: String,
    x: i32,
    y: i32,
    z: i32,
    pose: Pose,
    direction: Direction,
    name: String,
    talk: Option<Vec<(String, String)>>,
    checked: bool,
}

pub fn new(
    id: String,
    x: i32,
    y: i32,
    z: i32,
    name: String,
    talk: Option<Vec<(String, String)>>,
) -> NPC {
    NPC {
        frame_count: brownfox::FrameCount::new(0),
        id: id,
        x: x,
        y: y,
        z: z,
        pose: Pose::Walk,
        direction: Direction::Front,
        name: name,
        talk: talk,
        checked: false,
    }
}

impl brownfox::Moore<Input, Output> for NPC {
    fn transit(&self, input: &Input) -> NPC {
        let mut other = self.clone();

        if input.inputs.len() > 0 {
            if input.inputs[0].x < -0.25 {
                other.x -= 60 / input.previous.fps;
                other.direction = Direction::Left;
            } else if input.inputs[0].x > 0.25 {
                other.x += 60 / input.previous.fps;
                other.direction = Direction::Right;
            }
        }

        if input.inputs.len() > 0 {
            if input.inputs[0].y < -0.25 {
                other.y -= 60 / input.previous.fps;
                other.direction = Direction::Back;
            } else if input.inputs[0].y > 0.25 {
                other.y += 60 / input.previous.fps;
                other.direction = Direction::Front;
            }
        }

        other.checked = false;
        other.frame_count =
            (0..60 / input.previous.fps).fold(other.frame_count, |control, _| control.transit(&()));

        other
    }

    fn output(&self) -> Output {
        let mut instrs = vec![];
        if self.checked {
            if let Some(ref texts) = self.talk {
                instrs.push(Instr::Text(texts.clone()));
            }
        }
        let mut views = vec![View::Image(
            format!(
                "{}/walk/{}/{}.png",
                self.name,
                string_of_direction(&self.direction),
                self.frame_count.output() / 8 % 4
            ),
            self.x,
            self.y,
            self.z,
        )];

        Output {
            instrs: instrs,
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

impl NPC {
    pub fn id(&self) -> String {
        self.id.clone()
    }

    pub fn x(&self) -> i32 {
        self.x
    }

    pub fn y(&self) -> i32 {
        self.y
    }
    pub fn z(&self) -> i32 {
        self.z
    }

    pub fn on(&self, event: &Event) -> NPC {
        let mut other = self.clone();
        match event {
            Event::Check(rect) => {
                if rect.collision(brownfox::Rectangle::new(self.x, self.y, 16, 16)) {
                    other.checked = true;
                    other
                } else {
                    other
                }
            }
            _ => other,
        }
    }

    pub fn transport(&self, from_x: i32, from_y: i32, to_x: i32, to_y: i32) -> NPC {
        let mut other = self.clone();
        other.x = other.x - from_x + to_x;
        other.y = other.y - from_y + to_y;
        other
    }
}
