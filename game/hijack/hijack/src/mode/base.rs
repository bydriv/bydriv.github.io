use super::*;

#[derive(Clone)]
pub struct Base {
    into_mode: Option<Box<Mode>>,
    episodes: Vec<(String, String)>,
}

pub fn new() -> Base {
    Base {
        into_mode: None,
        episodes: vec![],
    }
}

impl brownfox::Moore<object::Input, object::Output> for Base {
    fn transit(&self, input: &object::Input) -> Base {
        let mut other = self.clone();
        let button1 = input.inputs.len() > 0
            && input.inputs[0].buttons.len() > 0
            && input.inputs[0].buttons[0];
        if button1 {
            other.into_mode = Some(Box::new(Mode::Episode(mode::episode::new(
                input
                    .previous
                    .episodes
                    .get("episode/boston.json")
                    .unwrap()
                    .clone(),
                input
                    .previous
                    .maps
                    .get("map/boston/0000-0000.json")
                    .unwrap()
                    .clone(),
            ))));
        }
        other.episodes = input
            .previous
            .episodes
            .iter()
            .map(|(name, episode)| (name.clone(), episode.title.clone()))
            .collect();
        other
    }

    fn output(&self) -> object::Output {
        let mut instrs = vec![];
        if let Some(mode) = self.into_mode.clone() {
            instrs.push(Instr::Mode(*mode));
        }
        let views = self
            .episodes
            .iter()
            .enumerate()
            .flat_map(|(i, (_, title))| text::text(0, i as i32 * 8, 0, title.clone()))
            .collect();
        object::Output {
            instrs: instrs,
            events: vec![],
            views: views,
        }
    }
}
