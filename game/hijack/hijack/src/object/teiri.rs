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
    frame_count: brownfox::FrameCount<i32>,
    id: String,
    x: i32,
    y: i32,
    z: i32,
    pose: Pose,
    direction: Direction,
    status: bool,
    combat: bool,
    cursor: Option<Cursor>,
    hijacking: Vec<(i32, i32)>,
    drones: Vec<Object>,
}

pub fn new(id: String, x: i32, y: i32, z: i32) -> Teiri {
    Teiri {
        frame_count: brownfox::FrameCount::new(0),
        id: id,
        x: x,
        y: y,
        z: z,
        pose: Pose::Walk,
        direction: Direction::Front,
        status: false,
        combat: false,
        cursor: None,
        hijacking: vec![],
        drones: vec![],
    }
}

impl brownfox::Moore<Input, Output> for Teiri {
    fn transit(&self, input: &Input) -> Teiri {
        let mut other = self.clone();
        other.hijacking = vec![];

        let button5 = input.inputs.len() > 0
            && input.inputs[0].buttons.len() > 4
            && input.inputs[0].buttons[4]
            && self.frame_count.output() % 8 < (60 / input.previous.fps);

        let button6 = input.inputs.len() > 0
            && input.inputs[0].buttons.len() > 5
            && input.inputs[0].buttons[5]
            && self.frame_count.output() % 8 < (60 / input.previous.fps);

        if button5 {
            other.combat = false;

            match other.pose {
                Pose::Hijack => {
                    other.pose = Pose::Walk;
                    other.cursor = None;
                }
                _ => {
                    other.pose = Pose::Hijack;
                    let cursor = match self.direction {
                        Direction::Left => Cursor {
                            x: self.x - 16,
                            y: self.y,
                        },
                        Direction::Back => Cursor {
                            x: self.x,
                            y: self.y - 16,
                        },
                        Direction::Right => Cursor {
                            x: self.x + 16,
                            y: self.y,
                        },
                        Direction::Front => Cursor {
                            x: self.x,
                            y: self.y + 16,
                        },
                    };
                    other.cursor = Some(cursor);
                }
            }
        } else if button6 {
            other.combat = !other.combat;

            if other.combat {
                other.pose = Pose::Walk;
                other.cursor = None;
                other.drones = other
                    .drones
                    .iter()
                    .enumerate()
                    .map(|(i, drone)| {
                        drone.transport(
                            drone.x(),
                            drone.y(),
                            other.x()
                                + ((other.frame_count.output() + i as i32 ^ 0x55555555) % 5 - 2)
                                    * 16,
                            other.y()
                                + ((other.frame_count.output() + i as i32 ^ 0x33333333) % 5 - 2)
                                    * 16,
                        )
                    })
                    .collect();
            }
        }

        if other.combat {
            other.drones = other
                .drones
                .iter()
                .map(|object| object.transit(input))
                .collect();
        }

        if let Some(cursor) = other.cursor {
            let mut cursor = cursor.clone();

            if input.inputs.len() > 0 {
                if input.inputs[0].x < -0.25 {
                    cursor.x -= 60 / input.previous.fps * 4;
                } else if input.inputs[0].x > 0.25 {
                    cursor.x += 60 / input.previous.fps * 4
                }
            }

            if input.inputs.len() > 0 {
                if input.inputs[0].y < -0.25 {
                    cursor.y -= 60 / input.previous.fps * 4;
                } else if input.inputs[0].y > 0.25 {
                    cursor.y += 60 / input.previous.fps * 4;
                }
            }

            if cursor.x - other.x < 0 {
                other.direction = Direction::Left;
            } else if cursor.x - other.x > 0 {
                other.direction = Direction::Right;
            } else if cursor.y - other.y < 0 {
                other.direction = Direction::Back;
            } else if cursor.y - other.y > 0 {
                other.direction = Direction::Front;
            }

            other.cursor = Some(cursor);
        } else {
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
        }

        other.frame_count =
            (0..60 / input.previous.fps).fold(other.frame_count, |control, _| control.transit(&()));

        if input.inputs.len() > 0
            && input.inputs[0].buttons.len() > 7
            && input.inputs[0].buttons[7]
            && self.frame_count.output() % 8 < (60 / input.previous.fps)
        {
            other.status = !other.status;
        }

        other
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
                    self.image_x(),
                    self.image_y(),
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
                self.image_x(),
                self.image_y(),
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
            for object in &self.drones {
                let mut out = object.output();
                events.append(&mut out.events);
                views.append(&mut out.views);
            }
        }

        Output {
            instrs: vec![],
            events: events,
            views: views,
        }
    }
}

impl Teiri {
    pub fn image_x(&self) -> i32 {
        match self.pose {
            Pose::Walk => self.x,
            Pose::Hijack => self.x - 8,
        }
    }

    pub fn image_y(&self) -> i32 {
        match self.pose {
            Pose::Walk => self.y,
            Pose::Hijack => self.y - 8,
        }
    }

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

    pub fn on(&self, event: &Event) -> Teiri {
        let mut other = self.clone();

        if other.combat {
            other.drones = other.drones.iter().map(|object| object.on(event)).collect();
        }

        match event {
            Event::Hijacked(security, security_damage, object) => {
                if *security > *security_damage {
                    other.cursor = Some(Cursor {
                        x: object.x(),
                        y: object.y(),
                    });

                    other.hijacking.push((*security, *security_damage));
                } else {
                    other.drones.push(object.transport(
                        object.x(),
                        object.y(),
                        self.x() + ((self.frame_count.output() ^ 0x55555555) % 5 - 2) * 16,
                        self.y() + ((self.frame_count.output() ^ 0x33333333) % 5 - 2) * 16,
                    ));
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
