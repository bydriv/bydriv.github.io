extern crate brownfox;

pub mod clear_condition;
pub mod episode;
pub mod map;
pub mod mode;
pub mod object;
pub mod system;
pub mod text;

use brownfox::Moore;
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Clone)]
pub struct Hijack {
    pub fps: i32,
    pub x: i32,
    pub y: i32,
    pub width: i32,
    pub height: i32,
    mode: mode::Mode,
    maps: Rc<HashMap<String, map::Map>>,
    episodes: Rc<HashMap<String, episode::Episode>>,
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
    Mode(mode::Mode),
    Text(Vec<(String, String)>),
}

#[derive(Clone, PartialEq)]
pub enum View {
    Image(String, i32, i32, i32),
}

impl Hijack {
    pub fn new(
        maps: HashMap<String, map::Map>,
        episodes: HashMap<String, episode::Episode>,
    ) -> Hijack {
        Hijack {
            fps: 60,
            x: 0,
            y: 0,
            width: 320,
            height: 320,
            mode: mode::Mode::Title(mode::title::new()),
            maps: Rc::new(maps),
            episodes: Rc::new(episodes),
        }
    }
}

impl brownfox::Moore<(i32, Vec<brownfox::Input>), object::Output> for Hijack {
    fn transit(&self, (fps, inputs): &(i32, Vec<brownfox::Input>)) -> Hijack {
        let input = object::Input {
            inputs: inputs.clone(),
            previous: self.clone(),
        };
        let mut other = self.clone();
        let mut flags = if let mode::Mode::Episode(ref episode) = &self.mode {
            episode.flags.clone()
        } else {
            HashMap::new()
        };

        for instr in &self.mode.output().instrs {
            match instr {
                &Instr::Transport(from_x, from_y, ref to_map, to_x, to_y) => {
                    if let mode::Mode::Episode(ref episode) = &self.mode {
                        let map = self.maps.get(to_map).unwrap();

                        return Hijack {
                            fps: *fps,
                            x: self.x,
                            y: self.y,
                            width: self.width,
                            height: self.height,
                            mode: mode::Mode::Episode(mode::episode::Episode {
                                x: map.x,
                                y: map.y,
                                width: map.width,
                                height: map.height,
                                clear_condition: episode.clear_condition.clone(),
                                cleared: episode.cleared,
                                episode_objects: episode
                                    .episode_objects
                                    .iter()
                                    .map(|object| {
                                        (
                                            object.0.clone(),
                                            object.1.transport(from_x, from_y, to_x, to_y),
                                        )
                                    })
                                    .collect(),
                                map_objects: map.objects.clone(),
                                flags: flags,
                                mode: mode::episode::Mode::Normal,
                            }),
                            maps: self.maps.clone(),
                            episodes: self.episodes.clone(),
                        };
                    }
                }
                &Instr::Flag(ref name, value) => {
                    flags.insert(name.clone(), value);
                }
                &Instr::Mode(ref mode) => {
                    other.mode = mode.clone();
                }
                &Instr::Text(ref texts) => {
                    if let mode::Mode::Episode(ref mut episode) = other.mode {
                        episode.mode = mode::episode::Mode::Text(0, 0, texts.clone());
                    }
                }
            }
        }

        if let mode::Mode::Episode(ref mut episode) = other.mode {
            episode.flags = flags;
        }
        other.fps = *fps;
        other.mode = other.mode.transit(&input);
        other
    }

    fn output(&self) -> object::Output {
        self.mode.output()
    }
}

impl Hijack {
    pub fn flags(&self) -> HashMap<String, bool> {
        if let mode::Mode::Episode(ref episode) = &self.mode {
            episode.flags.clone()
        } else {
            HashMap::new()
        }
    }
}
