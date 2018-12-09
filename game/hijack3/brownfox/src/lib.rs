pub trait Moore<I, O> {
    fn transit(&self, input: &I) -> Self;
    fn output(&self) -> O;
}

#[derive(Clone)]
pub struct FrameCount {
    pub i: u64,
}

impl FrameCount {
    pub fn new(i: u64) -> FrameCount {
        FrameCount { i: i }
    }
}

impl Moore<(), ()> for FrameCount {
    fn transit(&self, _input: &()) -> FrameCount {
        FrameCount { i: self.i + 1 }
    }
    fn output(&self) -> () {
        ()
    }
}

#[derive(Clone)]
pub struct Vec<M: Moore<I, O>, I, O> {
    input_type: std::marker::PhantomData<I>,
    output_type: std::marker::PhantomData<O>,
    pub machines: std::vec::Vec<M>,
}

impl<M: Moore<I, O>, I, O> Vec<M, I, O> {
    pub fn new(machines: std::vec::Vec<M>) -> Vec<M, I, O> {
        Vec {
            input_type: std::marker::PhantomData,
            output_type: std::marker::PhantomData,
            machines: machines,
        }
    }
}

impl<M: Moore<I, O>, I, O> Moore<I, std::vec::Vec<O>> for Vec<M, I, O> {
    fn transit(&self, input: &I) -> Vec<M, I, O> {
        Vec {
            input_type: self.input_type,
            output_type: self.output_type,
            machines: self
                .machines
                .iter()
                .map(|machine| machine.transit(input))
                .collect(),
        }
    }

    fn output(&self) -> std::vec::Vec<O> {
        self.machines
            .iter()
            .map(|machine| machine.output())
            .collect()
    }
}
