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
    id: String,
    x: i32,
    y: i32,
    z: i32,
    pose: Pose,
    direction: Direction,
    name: String,
    durability: i32,
    durability_damage: i32,
    security: i32,
    security_damage: i32,
    shots: Vec<shot::Shot>,
    fps: i32,
    disabled: bool,
}

pub fn new(id: String, x: i32, y: i32, z: i32, name: String) -> Drone {
    Drone {
        frame_count: brownfox::FrameCount::new(0),
        id: id,
        x: x,
        y: y,
        z: z,
        pose: Pose::Fly,
        direction: Direction::Front,
        name: name,
        durability: 4,
        durability_damage: 0,
        security: 20,
        security_damage: 0,
        shots: vec![],
        fps: 60,
        disabled: false,
    }
}

impl brownfox::Moore<Input, Output> for Drone {
    fn transit(&self, input: &Input) -> Drone {
        let mut other = self.clone();

        if other.disabled {
            return other;
        }

        if let Some(true) = input.previous.flags.get(&format!("{}/disabled", self.id)) {
            other.disabled = true;
            return other;
        }

        if other.durability_damage >= other.durability {
            other.disabled = true;
            return other;
        }

        if other.security_damage >= other.security {
            other.disabled = true;
            return other;
        }

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

        let button1 = input.inputs.len() > 0
            && input.inputs[0].buttons.len() > 0
            && input.inputs[0].buttons[0]
            && self.frame_count.output() % 8 < (60 / input.previous.fps);

        if button1 {
            other.shots.push(shot::new(
                format!("{}/{}", self.id, self.frame_count.output()),
                other.x
                    + 7
                    + match other.direction {
                        Direction::Left => -16,
                        Direction::Right => 16,
                        _ => 0,
                    },
                other.y
                    + 7
                    + match other.direction {
                        Direction::Back => -16,
                        Direction::Front => 16,
                        _ => 0,
                    },
                other.z,
                match other.direction {
                    Direction::Left => shot::Direction::Left,
                    Direction::Back => shot::Direction::Back,
                    Direction::Right => shot::Direction::Right,
                    Direction::Front => shot::Direction::Front,
                },
            ));
        }

        other.shots = other
            .shots
            .iter()
            .filter(|shot| !shot.disabled)
            .map(|shot| shot.transit(input))
            .collect();

        other.frame_count = (0..60 / input.previous.fps)
            .fold(self.frame_count.clone(), |control, _| control.transit(&()));

        other
    }

    fn output(&self) -> Output {
        if self.disabled {
            return Output {
                instrs: vec![Instr::Flag(format!("{}/disabled", self.id), true)],
                events: vec![],
                views: vec![],
            };
        }

        let mut instrs = vec![];
        let mut events = vec![];

        if self.security_damage > 0 {
            let mut other = self.clone();
            other.id = format!("{}/hijacked", other.id);
            other.security_damage = 0;
            other.disabled = false;

            events.push(Event::Hijacked(
                self.security,
                self.security_damage,
                Object::Drone(other),
            ));
        }

        let mut views = vec![View::Image(
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

        for shot in &self.shots {
            let mut out = shot.output();

            instrs.append(&mut out.instrs);
            events.append(&mut out.events);
            views.append(&mut out.views);
        }

        Output {
            instrs: instrs,
            events: events,
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

    pub fn on(&self, event: &Event) -> Drone {
        let mut other = self.clone();

        if self.disabled {
            return other;
        }

        match event {
            Event::Attack(rect) => {
                if rect.collision(brownfox::Rectangle::new(self.x, self.y, 16, 16)) {
                    other.durability_damage += 60 / other.fps;
                }
            }
            Event::Hijack(rect) => {
                if rect.collision(brownfox::Rectangle::new(self.x, self.y, 16, 16)) {
                    other.security_damage += 60 / other.fps;
                }
            }
            _ => {}
        }

        other
    }

    pub fn transport(&self, from_x: i32, from_y: i32, to_x: i32, to_y: i32) -> Drone {
        let mut other = self.clone();
        other.x = other.x - from_x + to_x;
        other.y = other.y - from_y + to_y;
        other
    }
}
