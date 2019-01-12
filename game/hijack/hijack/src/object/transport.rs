use super::*;
use brownfox::Shape;

#[derive(Clone)]
pub struct Transport {
    id: String,
    from_x: i32,
    from_y: i32,
    from_width: i32,
    from_height: i32,
    to_map: String,
    to_x: i32,
    to_y: i32,
    triggered: bool,
}

pub fn new(
    id: String,
    from_x: i32,
    from_y: i32,
    from_width: i32,
    from_height: i32,
    to_map: String,
    to_x: i32,
    to_y: i32,
) -> Transport {
    Transport {
        id: id,
        from_x: from_x,
        from_y: from_y,
        from_width: from_width,
        from_height: from_height,
        to_map: to_map,
        to_x: to_x,
        to_y: to_y,
        triggered: false,
    }
}

impl brownfox::Moore<Input, Output> for Transport {
    fn transit(&self, input: &Input) -> Transport {
        self.clone()
    }

    fn output(&self) -> Output {
        if self.triggered {
            Output {
                instrs: vec![Instr::Transport(
                    self.from_x,
                    self.from_y,
                    self.to_map.clone(),
                    self.to_x,
                    self.to_y,
                )],
                events: vec![],
                views: vec![],
            }
        } else {
            Output {
                instrs: vec![],
                events: vec![],
                views: vec![],
            }
        }
    }
}

impl Transport {
    pub fn id(&self) -> String {
        self.id.clone()
    }

    pub fn x(&self) -> i32 {
        self.from_x
    }

    pub fn y(&self) -> i32 {
        self.from_y
    }
    pub fn z(&self) -> i32 {
        0
    }

    pub fn on(&self, event: &Event) -> Transport {
        let mut triggered = false;

        match event {
            &Event::Trigger(ref rect1) => {
                let rect2 = brownfox::Rectangle::new(
                    self.from_x,
                    self.from_y,
                    self.from_width,
                    self.from_height,
                );
                if rect1.collision(rect2) {
                    triggered = true;
                }
            }
            _ => {}
        }

        let mut other = self.clone();
        other.triggered = triggered;
        other
    }

    pub fn transport(&self, from_x: i32, from_y: i32, to_x: i32, to_y: i32) -> Transport {
        let mut other = self.clone();
        other.from_x = other.from_x - from_x + to_x;
        other.from_y = other.from_y - from_y + to_y;
        other
    }
}
