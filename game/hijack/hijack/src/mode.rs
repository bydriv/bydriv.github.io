use super::*;

pub mod base;
pub mod episode;
pub mod title;

#[derive(Clone)]
pub enum Mode {
    Title(mode::title::Title),
    Base(mode::base::Base),
    Episode(mode::episode::Episode),
}

impl brownfox::Moore<object::Input, object::Output> for Mode {
    fn transit(&self, input: &object::Input) -> Mode {
        match self {
            Mode::Title(title) => Mode::Title(title.transit(input)),
            Mode::Base(base) => Mode::Base(base.transit(input)),
            Mode::Episode(episode) => Mode::Episode(episode.transit(input)),
        }
    }

    fn output(&self) -> object::Output {
        match self {
            Mode::Title(title) => title.output(),
            Mode::Base(base) => base.output(),
            Mode::Episode(episode) => episode.output(),
        }
    }
}
