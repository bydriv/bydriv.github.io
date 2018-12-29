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
            frame_count: self.frame_count.transit(&()),
        }
    }

    fn output(&self) -> Output {
        self.objects[self.frame_count.i as usize / self.i as usize % self.objects.len()].output()
    }
}
