extern crate num;

pub trait Moore<I, O> {
    fn transit(&self, input: &I) -> Self;
    fn output(&self) -> O;
}

#[derive(Clone)]
pub struct FrameCount<N: num::PrimInt> {
    i: N,
}

impl<N: num::PrimInt> FrameCount<N> {
    pub fn new(i: N) -> FrameCount<N> {
        FrameCount { i: i }
    }
}

impl<N: num::PrimInt> Moore<(), N> for FrameCount<N> {
    fn transit(&self, _input: &()) -> FrameCount<N> {
        FrameCount {
            i: self.i + N::one(),
        }
    }

    fn output(&self) -> N {
        self.i
    }
}

impl<M: Moore<I, O>, I, O> Moore<I, Vec<O>> for Vec<M> {
    fn transit(&self, input: &I) -> Vec<M> {
        self.iter().map(|machine| machine.transit(input)).collect()
    }

    fn output(&self) -> Vec<O> {
        self.iter().map(|machine| machine.output()).collect()
    }
}

pub trait Shape {
    fn collision(&self, other: Self) -> bool;
}

#[derive(Clone)]
pub struct Rectangle<N: num::PrimInt> {
    pub x: N,
    pub y: N,
    pub width: N,
    pub height: N,
}

impl<N: num::PrimInt> Rectangle<N> {
    pub fn new(x: N, y: N, width: N, height: N) -> Rectangle<N> {
        Rectangle {
            x: x,
            y: y,
            width: width,
            height: height,
        }
    }

    pub fn intersection(&self, other: Rectangle<N>) -> Rectangle<N> {
        let left = std::cmp::max(self.x, other.x);
        let top = std::cmp::max(self.y, other.y);
        let right = std::cmp::min(self.x + self.width, other.x + other.width);
        let bottom = std::cmp::min(self.y + self.height, other.y + other.height);
        let width = right - left;
        let height = bottom - top;

        Rectangle::new(left, top, width, height)
    }
}

impl<N: num::PrimInt> Shape for Rectangle<N> {
    fn collision(&self, other: Self) -> bool {
        let inter = self.intersection(other);
        inter.width > N::zero() && inter.height > N::zero()
    }
}

#[derive(Clone)]
pub struct Input {
    pub x: f64,
    pub y: f64,
    pub buttons: Vec<bool>,
}

impl Input {
    pub fn new(x: f64, y: f64, buttons: &Vec<bool>) -> Input {
        Input {
            x: x,
            y: y,
            buttons: buttons.clone(),
        }
    }

    pub fn empty() -> Input {
        Input {
            x: 0.0,
            y: 0.0,
            buttons: vec![
                false, false, false, false, false, false, false, false, false, false, false, false,
                false, false, false, false, false, false, false, false, false, false, false, false,
                false, false, false, false, false, false, false, false,
            ],
        }
    }
}

#[derive(Clone)]
pub struct Player {
    pub i: usize,
    pub input: Input,
}

impl Player {
    pub fn new(i: usize) -> Player {
        Player {
            i: i,
            input: Input::empty(),
        }
    }
}

#[derive(Clone)]
pub struct Immovable {}

impl Immovable {
    pub fn new() -> Immovable {
        Immovable {}
    }
}

#[derive(Clone)]
pub struct Repeat<N: num::PrimInt> {
    frame_count: FrameCount<N>,
    step: N,
    inputs: Vec<Input>,
}

impl<N: num::PrimInt> Repeat<N> {
    pub fn new(step: N, inputs: Vec<Input>) -> Repeat<N> {
        Repeat {
            frame_count: FrameCount::new(N::zero()),
            step: step,
            inputs: inputs,
        }
    }
}

#[derive(Clone)]
pub enum Control<N: num::PrimInt> {
    Player(Player),
    Immovable(Immovable),
    Repeat(Repeat<N>),
}

impl<N: num::PrimInt> Moore<Vec<Input>, Input> for Control<N> {
    fn transit(&self, inputs: &Vec<Input>) -> Control<N> {
        match self {
            Control::Player(player) => Control::Player(player.transit(inputs)),
            Control::Immovable(immovable) => Control::Immovable(immovable.transit(inputs)),
            Control::Repeat(repeat) => Control::Repeat(repeat.transit(inputs)),
        }
    }

    fn output(&self) -> Input {
        match self {
            Control::Player(player) => player.output(),
            Control::Immovable(immovable) => immovable.output(),
            Control::Repeat(repeat) => repeat.output(),
        }
    }
}

impl Moore<Vec<Input>, Input> for Player {
    fn transit(&self, inputs: &Vec<Input>) -> Player {
        Player {
            i: self.i,
            input: inputs[self.i].clone(),
        }
    }

    fn output(&self) -> Input {
        self.input.clone()
    }
}

impl Moore<Vec<Input>, Input> for Immovable {
    fn transit(&self, _input: &Vec<Input>) -> Immovable {
        self.clone()
    }

    fn output(&self) -> Input {
        Input::empty()
    }
}

impl<N: num::PrimInt> Moore<Vec<Input>, Input> for Repeat<N> {
    fn transit(&self, _input: &Vec<Input>) -> Repeat<N> {
        let mut other = self.clone();
        other.frame_count = other.frame_count.transit(&());
        other
    }

    fn output(&self) -> Input {
        if let Some(len) = N::from(self.inputs.len()) {
            if let Some(i) = N::to_usize(&(self.frame_count.output() / self.step % len)) {
                self.inputs[i].clone()
            } else {
                panic!("???")
            }
        } else {
            panic!("???")
        }
    }
}

impl<M: Moore<A, C>, N: Moore<B, D>, A, B, C, D> Moore<(A, B), (C, D)> for (M, N) {
    fn transit(&self, input: &(A, B)) -> (M, N) {
        (self.0.transit(&input.0), self.1.transit(&input.1))
    }

    fn output(&self) -> (C, D) {
        (self.0.output(), self.1.output())
    }
}
