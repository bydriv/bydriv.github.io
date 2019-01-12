extern crate brownfox;

pub mod episode;
pub mod map;
pub mod object;
pub mod system;
pub mod text;

use std::collections::HashMap;
use brownfox::Moore;

#[derive(Clone)]
pub enum Mode {
    Title,
    Main,
}

#[derive(Clone)]
pub struct Hijack {
    pub fps: i32,
    pub x: i32,
    pub y: i32,
    pub width: i32,
    pub height: i32,
    episode_objects: Vec<(brownfox::Control<i32>, object::Object)>,
    map_objects: Vec<(brownfox::Control<i32>, object::Object)>,
    flags: HashMap<String, bool>,
    mode: Mode,
}

#[derive(Clone)]
pub enum Event {
    Check(brownfox::Rectangle<i32>),
    Trigger(brownfox::Rectangle<i32>),
    Hijack(brownfox::Rectangle<i32>),
    Hijacked(i32, i32, object::Object),
    Attack(brownfox::Rectangle<i32>),
}

#[derive(Clone)]
pub enum Instr {
    Transport(i32, i32, String, i32, i32),
    Flag(String, bool),
}

#[derive(Clone, PartialEq)]
pub enum View {
    Image(String, i32, i32, i32),
}

impl Hijack {
    pub fn new() -> Hijack {
        let map = map::boston::new();
        let episode = episode::boston::new();
        let template = map.templates.get(&episode.map).unwrap();

        Hijack {
            fps: 60,
            x: template.x,
            y: template.y,
            width: template.width,
            height: template.height,
            episode_objects: episode.objects.clone(),
            map_objects: template.objects.clone(),
            flags: HashMap::new(),
            mode: Mode::Title,
        }
    }
}

impl brownfox::Moore<(i32, Vec<brownfox::Input>), object::Output> for Hijack {
    fn transit(&self, (fps, inputs): &(i32, Vec<brownfox::Input>)) -> Hijack {
        match self.mode {
            Mode::Title => {
                let any_button = inputs.len() > 0 && inputs[0].buttons.iter().any(|button| *button);
                if any_button {
                    let mut other = self.clone();
                    other.mode = Mode::Main;
                    other
                } else {
                    self.clone()
                }
            }
            Mode::Main => {
                let mut objects = self.episode_objects.clone();
                objects.append(&mut self.map_objects.clone());

                let instrs: Vec<Instr> = objects
                    .iter()
                    .flat_map(|object| object.1.output().instrs)
                    .collect();

                let events: Vec<Event> = objects
                    .iter()
                    .flat_map(|object| object.1.output().events)
                    .collect();

                let mut episode_objects = Box::new(self.episode_objects.iter().map(|object| {
                    let control = (0..60 / self.fps)
                        .fold(object.0.clone(), |control, _| control.transit(inputs));
                    let input = control.output();
                    (
                        control,
                        object.1.transit(&object::Input {
                            inputs: vec![input],
                            previous: self.clone(),
                        }),
                    )
                }))
                    as Box<Iterator<Item = (brownfox::Control<i32>, object::Object)>>;

                let mut map_objects = Box::new(self.map_objects.iter().map(|object| {
                    let control = (0..60 / self.fps)
                        .fold(object.0.clone(), |control, _| control.transit(inputs));
                    let input = control.output();
                    (
                        control,
                        object.1.transit(&object::Input {
                            inputs: vec![input],
                            previous: self.clone(),
                        }),
                    )
                }))
                    as Box<Iterator<Item = (brownfox::Control<i32>, object::Object)>>;

                let mut flags = self.flags.clone();

                for instr in &instrs {
                    match instr {
                        &Instr::Transport(from_x, from_y, ref to_map, to_x, to_y) => {
                            let map = map::boston::new();
                            let template = map.templates.get(to_map).unwrap();

                            return Hijack {
                                fps: *fps,
                                x: template.x,
                                y: template.y,
                                width: template.width,
                                height: template.height,
                                episode_objects: episode_objects
                                    .map(|object| {
                                        (
                                            object.0.clone(),
                                            object.1.transport(from_x, from_y, to_x, to_y),
                                        )
                                    })
                                    .collect(),
                                map_objects: template.objects.clone(),
                                flags: flags,
                                mode: Mode::Main,
                            };
                        }
                        &Instr::Flag(ref name, value) => {
                            flags.insert(name.clone(), value);
                        }
                    }
                }

                for event in &events {
                    episode_objects = Box::new(
                        episode_objects
                            .map(move |object| (object.0.clone(), object.1.on(event))),
                    )
                        as Box<Iterator<Item = (brownfox::Control<i32>, object::Object)>>;
                    map_objects = Box::new(
                        map_objects
                            .map(move |object| (object.0.clone(), object.1.on(event))),
                    )
                        as Box<Iterator<Item = (brownfox::Control<i32>, object::Object)>>;
                }

                Hijack {
                    fps: *fps,
                    x: self.x,
                    y: self.y,
                    width: self.width,
                    height: self.height,
                    episode_objects: episode_objects.collect(),
                    map_objects: map_objects.collect(),
                    flags: flags,
                    mode: Mode::Main,
                }
            }
        }
    }

    fn output(&self) -> object::Output {
        match self.mode {
            Mode::Title => {
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
                    instrs: vec![],
                    events: vec![],
                    views: views,
                }
            }
            Mode::Main => {
                let mut objects = self.episode_objects.clone();
                objects.append(&mut self.map_objects.clone());

                object::Output {
                    instrs: vec![],
                    events: vec![],
                    views: objects
                        .iter()
                        .flat_map(|object| object.1.output().views)
                        .collect(),
                }
            }
        }
    }
}
