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
struct Cursor {
    x: i32,
    y: i32,
}

#[derive(Clone)]
pub struct Teiri {
    id: String,
    frame_count: brownfox::FrameCount<i32>,
    x: i32,
    y: i32,
    z: i32,
    pose: Pose,
    direction: Direction,
    status: bool,
    combat: bool,
    cursor: Option<Cursor>,
    hijacking: Vec<(i32, i32)>,
    hijacked: Vec<Object>,
}

pub fn new(x: i32, y: i32, z: i32) -> Teiri {
    Teiri {
        id: "teiri".to_string(),
        frame_count: brownfox::FrameCount::new(0),
        x: x,
        y: y,
        z: z,
        pose: Pose::Walk,
        direction: Direction::Front,
        status: false,
        combat: false,
        cursor: None,
        hijacking: vec![],
        hijacked: vec![],
    }
}

impl brownfox::Moore<Input, Output> for Teiri {
    fn transit(&self, input: &Input) -> Teiri {
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

        let pose = if input.inputs.len() > 0
            && input.inputs[0].buttons.len() > 4
            && input.inputs[0].buttons[4]
            && self.frame_count.output() % 8 < (60 / input.previous.fps)
        {
            match self.pose {
                Pose::Hijack => Pose::Walk,
                _ => Pose::Hijack,
            }
        } else {
            self.pose.clone()
        };

        let mut hijacked = self.hijacked.clone();

        if self.combat {
            hijacked = hijacked
                .iter()
                .map(|object| object.transit(input))
                .collect();
        }

        let (combat, pose) = if input.inputs.len() > 0
            && input.inputs[0].buttons.len() > 5
            && input.inputs[0].buttons[5]
            && self.frame_count.output() % 8 < (60 / input.previous.fps)
        {
            if !self.combat {
                (true, Pose::Walk)
            } else {
                (false, pose)
            }
        } else {
            match pose {
                Pose::Hijack => (false, pose),
                _ => (self.combat, pose),
            }
        };

        match pose {
            Pose::Hijack => {
                let cursor = if let Some(cursor) = self.cursor.clone() {
                    Cursor {
                        x: cursor.x + xshift * 8,
                        y: cursor.y + yshift * 8,
                    }
                } else {
                    match self.direction {
                        Direction::Left => Cursor {
                            x: self.x + xshift * 8 - 16,
                            y: self.y + yshift * 8,
                        },
                        Direction::Back => Cursor {
                            x: self.x + xshift * 8,
                            y: self.y + yshift * 8 - 16,
                        },
                        Direction::Right => Cursor {
                            x: self.x + xshift * 8 + 16,
                            y: self.y + yshift * 8,
                        },
                        Direction::Front => Cursor {
                            x: self.x + xshift * 8,
                            y: self.y + yshift * 8 + 16,
                        },
                    }
                };

                let direction = if cursor.x - self.x < 0 {
                    Direction::Left
                } else if cursor.x - self.x > 0 {
                    Direction::Right
                } else if cursor.y - self.y < 0 {
                    Direction::Back
                } else if cursor.y - self.y > 0 {
                    Direction::Front
                } else {
                    self.direction.clone()
                };

                Teiri {
                    id: self.id.clone(),
                    frame_count: (0..60 / input.previous.fps)
                        .fold(self.frame_count.clone(), |control, _| control.transit(&())),
                    x: self.x,
                    y: self.y,
                    z: self.z,
                    pose: pose,
                    direction: direction,
                    status: if input.inputs.len() > 0
                        && input.inputs[0].buttons.len() > 7
                        && input.inputs[0].buttons[7]
                        && self.frame_count.output() % 8 < (60 / input.previous.fps)
                    {
                        !self.status
                    } else {
                        self.status
                    },
                    combat: combat,
                    cursor: Some(cursor),
                    hijacking: vec![],
                    hijacked: hijacked,
                }
            }
            Pose::Walk => {
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
                    id: self.id.clone(),
                    frame_count: (0..60 / input.previous.fps)
                        .fold(self.frame_count.clone(), |control, _| control.transit(&())),
                    x: self.x + xshift,
                    y: self.y + yshift,
                    z: self.z,
                    pose: pose,
                    direction: direction,
                    status: if input.inputs.len() > 0
                        && input.inputs[0].buttons.len() > 7
                        && input.inputs[0].buttons[7]
                        && self.frame_count.output() % 8 < (60 / input.previous.fps)
                    {
                        !self.status
                    } else {
                        self.status
                    },
                    combat: combat,
                    cursor: None,
                    hijacking: vec![],
                    hijacked: hijacked,
                }
            }
        }
    }

    fn output(&self) -> Output {
        let mut events = vec![];

        events.push(Event::Trigger(brownfox::Rectangle::new(
            self.x, self.y, 16, 16,
        )));

        let mut views = if let Pose::Hijack = self.pose {
            vec![
                View::Image(
                    format!(
                        "pixelart/teiri/{}/{}/{}.png",
                        string_of_pose(&self.pose),
                        string_of_direction(&self.direction),
                        self.frame_count.output() / 8 % 4
                    ),
                    self.x(),
                    self.y(),
                    1100,
                ),
                View::Image("pixelart/effect/dark.png".to_string(), 0, 0, 1000),
            ]
        } else {
            vec![View::Image(
                format!(
                    "pixelart/teiri/{}/{}/{}.png",
                    string_of_pose(&self.pose),
                    string_of_direction(&self.direction),
                    self.frame_count.output() / 8 % 4
                ),
                self.x(),
                self.y(),
                self.z,
            )]
        };

        for (i, (security, security_damage)) in self.hijacking.iter().enumerate() {
            let progress = (*security_damage as f64 / *security as f64) * 100.0;
            views.append(&mut text::text_green(
                0,
                (i * 8) as i32,
                1300,
                format!(
                    "[{} of {}] Hijacking {}%",
                    (i + 1),
                    self.hijacking.len(),
                    progress as u32
                ),
            ));
        }

        if let Some(cursor) = self.cursor.clone() {
            events.push(Event::Hijack(brownfox::Rectangle::new(
                cursor.x, cursor.y, 16, 16,
            )));

            views.push(View::Image(
                "pixelart/effect/cursor.png".to_string(),
                cursor.x,
                cursor.y,
                1200,
            ));
        }

        if self.status {
            views.push(View::Image(
                "illust/teiri/neutral/middle.png".to_string(),
                0,
                0,
                2000,
            ));
            views.push(View::Image(
                "pixelart/effect/dark.png".to_string(),
                0,
                0,
                1900,
            ));
            views.append(&mut text::text(
                176,
                32,
                2100,
                "ミナガワ・テーリ".to_string(),
            ));
        }

        if self.combat {
            for object in &self.hijacked {
                let mut out = object.output();
                events.append(&mut out.events);
                views.append(&mut out.views);
            }
        }

        Output {
            events: events,
            views: views,
        }
    }
}

impl Teiri {
    fn x(&self) -> i32 {
        match self.pose {
            Pose::Walk => self.x,
            Pose::Hijack => self.x - 8,
        }
    }

    fn y(&self) -> i32 {
        match self.pose {
            Pose::Walk => self.y,
            Pose::Hijack => self.y - 8,
        }
    }

    pub fn on(&self, event: &Event) -> Teiri {
        let mut other = self.clone();

        other.hijacked = other
            .hijacked
            .iter()
            .map(|object| object.on(event))
            .collect();

        match event {
            Event::Hijacked(security, security_damage, object) => {
                if *security > *security_damage {
                    other.hijacking.push((*security, *security_damage));
                } else {
                    other.hijacked.push(object.clone());
                }
            }
            _ => {}
        }

        other
    }

    pub fn transport(&self, from_x: i32, from_y: i32, to_x: i32, to_y: i32) -> Teiri {
        let mut other = self.clone();
        other.x = other.x - from_x + to_x;
        other.y = other.y - from_y + to_y;
        other
    }
}

fn string_of_pose(pose: &Pose) -> String {
    match pose {
        Pose::Walk => "walk".to_string(),
        Pose::Hijack => "hijack".to_string(),
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
