use super::*;

#[derive(Clone)]
pub struct Animated {
    objects: Vec<Object>,
    i: i32,
    frame_count: brownfox::FrameCount,
}

pub fn new(i: i32, objects: Vec<Object>) -> Animated {
    Animated {
        objects: objects,
        i: i,
        frame_count: brownfox::FrameCount::new(0),
    }
}

impl brownfox::Moore<Input, Output> for Animated {
    fn transit(&self, input: &Input) -> Animated {
        Animated {
            objects: self.objects.transit(input),
            i: self.i,
            frame_count: if input.previous.fps == 30 {
                self.frame_count.transit(&()).transit(&())
            } else {
                self.frame_count.transit(&())
            },
        }
    }

    fn output(&self) -> Output {
        self.objects[self.frame_count.i as usize / self.i as usize % self.objects.len()].output()
    }
}

impl Animated {
    pub fn transport(&self, from_x: i32, from_y: i32, to_x: i32, to_y: i32) -> Animated {
        let mut other = self.clone();
        other.objects = other
            .objects
            .iter()
            .map(|object| object.transport(from_x, from_y, to_x, to_y))
            .collect();
        other
    }
}
