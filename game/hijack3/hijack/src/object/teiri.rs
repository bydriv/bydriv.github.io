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
    frame_count: brownfox::FrameCount,
    x: i32,
    y: i32,
    z: i32,
    pose: Pose,
    direction: Direction,
    check: bool,
    cursor: Option<Cursor>,
}

pub fn new(x: i32, y: i32, z: i32) -> Teiri {
    Teiri {
        frame_count: brownfox::FrameCount::new(0),
        x: x,
        y: y,
        z: z,
        pose: Pose::Walk,
        direction: Direction::Front,
        check: false,
        cursor: None,
    }
}

impl brownfox::Moore<Input, Output> for Teiri {
    fn transit(&self, input: &Input) -> Teiri {
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

        let pose = if input.0.len() > 0 && input.0[0].buttons.len() > 4 && input.0[0].buttons[4] {
            Pose::Hijack
        } else {
            Pose::Walk
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
                    frame_count: self.frame_count.transit(&()),
                    x: self.x,
                    y: self.y,
                    z: self.z,
                    pose: pose,
                    direction: direction,
                    check: input.0.len() > 0
                        && input.0[0].buttons.len() > 0
                        && input.0[0].buttons[0],
                    cursor: Some(cursor),
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
                    frame_count: self.frame_count.transit(&()),
                    x: self.x + xshift,
                    y: self.y + yshift,
                    z: self.z,
                    pose: pose,
                    direction: direction,
                    check: input.0.len() > 0
                        && input.0[0].buttons.len() > 0
                        && input.0[0].buttons[0],
                    cursor: None,
                }
            }
        }
    }

    fn output(&self) -> Output {
        let mut events = vec![Event::Focus(self.x, self.y, 16, 16)];

        if self.check {
            let x = match self.direction {
                Direction::Left => self.x - 16,
                Direction::Right => self.x + 16,
                _ => self.x,
            };
            let y = match self.direction {
                Direction::Back => self.y - 16,
                Direction::Front => self.y + 16,
                _ => self.y,
            };
            events.push(Event::Check(x, y, 16, 16));
        }

        let mut views = if let Pose::Hijack = self.pose {
            vec![
                View::Image(
                    format!(
                        "pixelart/teiri/{}/{}/{}.png",
                        string_of_pose(&self.pose),
                        string_of_direction(&self.direction),
                        self.frame_count.i / 8 % 4
                    ),
                    self.x(),
                    self.y(),
                    1100,
                ),
                View::Image(
                    "pixelart/effect/dark.png".to_string(),
                    self.x() - 160 + 16,
                    self.y() - 120 + 16,
                    1000,
                ),
            ]
        } else {
            vec![View::Image(
                format!(
                    "pixelart/teiri/{}/{}/{}.png",
                    string_of_pose(&self.pose),
                    string_of_direction(&self.direction),
                    self.frame_count.i / 8 % 4
                ),
                self.x(),
                self.y(),
                self.z,
            )]
        };

        if let Some(cursor) = self.cursor.clone() {
            views.push(View::Image(
                "pixelart/effect/cursor.png".to_string(),
                cursor.x,
                cursor.y,
                1200,
            ));
        }

        (events, views)
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
