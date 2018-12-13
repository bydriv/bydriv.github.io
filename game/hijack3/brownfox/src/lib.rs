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

pub trait Shape {
    fn collision(&self, other: Self) -> bool;
}

#[derive(Clone)]
pub struct Rectangle {
    pub x: i64,
    pub y: i64,
    pub width: i64,
    pub height: i64,
}

impl Rectangle {
    pub fn new(x: i64, y: i64, width: i64, height: i64) -> Rectangle {
        Rectangle {
            x: x,
            y: y,
            width: width,
            height: height,
        }
    }
}

impl Shape for Rectangle {
    fn collision(&self, other: Self) -> bool {
        let left = std::cmp::max(self.x, other.x);
        let top = std::cmp::max(self.y, other.y);
        let right = std::cmp::max(self.x + self.width, other.x + other.width);
        let bottom = std::cmp::max(self.y + self.height, other.y + other.height);
        let width = right - left;
        let height = bottom - top;
        width > 0 && height > 0
    }
}

#[derive(Clone)]
pub struct Input {
    pub x: f64,
    pub y: f64,
    pub buttons: std::vec::Vec<bool>,
}

impl Input {
    pub fn new(x: f64, y: f64, buttons: &std::vec::Vec<bool>) -> Input {
        Input {
            x: x,
            y: y,
            buttons: buttons.clone(),
        }
    }
}
