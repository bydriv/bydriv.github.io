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
pub enum Control {
    Player(Player),
    Immovable(Immovable),
}

impl Moore<Vec<Input>, Input> for Control {
    fn transit(&self, inputs: &Vec<Input>) -> Control {
        match self {
            Control::Player(player) => Control::Player(player.transit(inputs)),
            Control::Immovable(immovable) => Control::Immovable(immovable.transit(inputs)),
        }
    }

    fn output(&self) -> Input {
        match self {
            Control::Player(player) => player.output(),
            Control::Immovable(immovable) => immovable.output(),
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

impl<M: Moore<A, C>, N: Moore<B, D>, A, B, C, D> Moore<(A, B), (C, D)> for (M, N) {
    fn transit(&self, input: &(A, B)) -> (M, N) {
        (self.0.transit(&input.0), self.1.transit(&input.1))
    }

    fn output(&self) -> (C, D) {
        (self.0.output(), self.1.output())
    }
}
