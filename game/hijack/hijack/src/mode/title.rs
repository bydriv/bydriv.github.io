use super::*;

#[derive(Clone)]
pub struct Title {
    into_mode: Option<Box<Mode>>,
}

pub fn new() -> Title {
    Title { into_mode: None }
}

impl brownfox::Moore<object::Input, object::Output> for Title {
    fn transit(&self, input: &object::Input) -> Title {
        let any_button =
            input.inputs.len() > 0 && input.inputs[0].buttons.iter().any(|button| *button);
        if any_button {
            let mut other = self.clone();
            other.into_mode = Some(Box::new(Mode::Base(mode::base::new())));
            other
        } else {
            self.clone()
        }
    }

    fn output(&self) -> object::Output {
        let mut instrs = vec![];
        if let Some(mode) = self.into_mode.clone() {
            instrs.push(Instr::Mode(*mode));
        }
        let mut views = vec![View::Image(
            "pixelart/system/logo.png".to_string(),
            32,
            64,
            0,
        )];
        views.append(&mut text::text_green(
            96,
            144,
            0,
            "press any button".to_string(),
        ));
        object::Output {
            instrs: instrs,
            events: vec![],
            views: views,
        }
    }
}
