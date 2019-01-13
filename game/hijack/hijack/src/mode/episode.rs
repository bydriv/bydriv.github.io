use super::*;

#[derive(Clone)]
pub enum Mode {
    Normal,
    Text(i32, i32, Vec<(String, String)>),
}

#[derive(Clone)]
pub struct Episode {
    pub x: i32,
    pub y: i32,
    pub width: i32,
    pub height: i32,
    pub clear_condition: clear_condition::ClearCondition,
    pub cleared: bool,
    pub episode_objects: Vec<(brownfox::Control<i32>, object::Object)>,
    pub map_objects: Vec<(brownfox::Control<i32>, object::Object)>,
    pub flags: HashMap<String, bool>,
    pub mode: Mode,
}

pub fn new(episode: ::episode::Episode, map: ::map::Map) -> Episode {
    Episode {
        x: map.x,
        y: map.y,
        width: map.width,
        height: map.height,
        clear_condition: episode.clear_condition.clone(),
        cleared: false,
        episode_objects: episode.objects.clone(),
        map_objects: map.objects.clone(),
        flags: HashMap::new(),
        mode: Mode::Normal,
    }
}

impl brownfox::Moore<object::Input, object::Output> for Episode {
    fn transit(&self, input: &object::Input) -> Episode {
        match self.mode {
            Mode::Normal => {
                let mut input1 = object::Input {
                    inputs: vec![],
                    previous: input.previous.clone(),
                };
                let mut input2 = object::Input {
                    inputs: vec![],
                    previous: input.previous.clone(),
                };
                let cleared = self.clear_condition.satisfy(&input.previous);

                let mut objects = self.episode_objects.clone();
                objects.append(&mut self.map_objects.clone());

                let events: Vec<Event> = objects
                    .iter()
                    .flat_map(|object| object.1.output().events)
                    .collect();

                let mut episode_objects = Box::new(self.episode_objects.iter().map(|object| {
                    let control = (0..60 / input.previous.fps)
                        .fold(object.0.clone(), |control, _| {
                            control.transit(&input.inputs)
                        });
                    input1.inputs = vec![control.output()];
                    (control, object.1.transit(&input1))
                }))
                    as Box<Iterator<Item = (brownfox::Control<i32>, object::Object)>>;

                let mut map_objects = Box::new(self.map_objects.iter().map(|object| {
                    let control = (0..60 / input.previous.fps)
                        .fold(object.0.clone(), |control, _| {
                            control.transit(&input.inputs)
                        });
                    input2.inputs = vec![control.output()];
                    (control, object.1.transit(&input2))
                }))
                    as Box<Iterator<Item = (brownfox::Control<i32>, object::Object)>>;
                for event in &events {
                    episode_objects = Box::new(
                        episode_objects.map(move |object| (object.0.clone(), object.1.on(event))),
                    )
                        as Box<Iterator<Item = (brownfox::Control<i32>, object::Object)>>;
                    map_objects = Box::new(
                        map_objects.map(move |object| (object.0.clone(), object.1.on(event))),
                    )
                        as Box<Iterator<Item = (brownfox::Control<i32>, object::Object)>>;
                }

                Episode {
                    x: self.x,
                    y: self.y,
                    width: self.width,
                    height: self.height,
                    clear_condition: self.clear_condition.clone(),
                    cleared: cleared,
                    episode_objects: episode_objects.collect(),
                    map_objects: map_objects.collect(),
                    flags: self.flags.clone(),
                    mode: Mode::Normal,
                }
            }
            Mode::Text(i, j, ref texts) => {
                let input0 = object::Input {
                    inputs: vec![],
                    previous: input.previous.clone(),
                };
                let cleared = self.clear_condition.satisfy(&input.previous);

                let mut objects = self.episode_objects.clone();
                objects.append(&mut self.map_objects.clone());

                let events: Vec<Event> = objects
                    .iter()
                    .flat_map(|object| object.1.output().events)
                    .collect();

                let mut episode_objects = Box::new(self.episode_objects.iter().map(|object| {
                    let control = (0..60 / input.previous.fps)
                        .fold(object.0.clone(), |control, _| {
                            control.transit(&input0.inputs)
                        });
                    (control, object.1.transit(&input0))
                }))
                    as Box<Iterator<Item = (brownfox::Control<i32>, object::Object)>>;

                let mut map_objects = Box::new(self.map_objects.iter().map(|object| {
                    let control = (0..60 / input.previous.fps)
                        .fold(object.0.clone(), |control, _| {
                            control.transit(&input0.inputs)
                        });
                    (control, object.1.transit(&input0))
                }))
                    as Box<Iterator<Item = (brownfox::Control<i32>, object::Object)>>;
                for event in &events {
                    episode_objects = Box::new(
                        episode_objects.map(move |object| (object.0.clone(), object.1.on(event))),
                    )
                        as Box<Iterator<Item = (brownfox::Control<i32>, object::Object)>>;
                    map_objects = Box::new(
                        map_objects.map(move |object| (object.0.clone(), object.1.on(event))),
                    )
                        as Box<Iterator<Item = (brownfox::Control<i32>, object::Object)>>;
                }

                let mode = if i < texts.len() as i32 {
                    let button1 = input.inputs.len() > 0
                        && input.inputs[0].buttons.len() > 0
                        && input.inputs[0].buttons[0];
                    let (ref text1, ref text2) = texts[i as usize];
                    if j < (text1.chars().collect::<Vec<char>>().len()
                        + text2.chars().collect::<Vec<char>>().len())
                        as i32
                    {
                        Mode::Text(i, j + 1, texts.clone())
                    } else if i + 1 < texts.len() as i32 && button1 {
                        Mode::Text(i + 1, 0, texts.clone())
                    } else if button1 {
                        Mode::Normal
                    } else {
                        Mode::Text(i, j, texts.clone())
                    }
                } else {
                    Mode::Normal
                };

                Episode {
                    x: self.x,
                    y: self.y,
                    width: self.width,
                    height: self.height,
                    clear_condition: self.clear_condition.clone(),
                    cleared: cleared,
                    episode_objects: episode_objects.collect(),
                    map_objects: map_objects.collect(),
                    flags: self.flags.clone(),
                    mode: mode,
                }
            }
        }
    }

    fn output(&self) -> object::Output {
        let mut objects = self.episode_objects.clone();
        objects.append(&mut self.map_objects.clone());

        let instrs: Vec<Instr> = objects
            .iter()
            .flat_map(|object| object.1.output().instrs)
            .collect();

        let mut views: Vec<View> = objects
            .iter()
            .flat_map(|object| object.1.output().views)
            .collect();
        if self.cleared {
            views.append(&mut text::text(0, 0, 3000, "all clear!".to_string()));
        }
        if let Mode::Text(i, j, ref texts) = self.mode {
            if i < texts.len() as i32 {
                let (ref text1, ref text2) = texts[i as usize];
                let len = text1.chars().collect::<Vec<char>>().len() as i32;
                let k = std::cmp::min(j, len);
                let l = std::cmp::max(j - len, 0);
                let text1 = &text1.as_str().chars().collect::<Vec<char>>()[..k as usize];
                let text2 = &text2.as_str().chars().collect::<Vec<char>>()[..l as usize];
                views.push(View::Image("pixelart/effect/mask.png".to_string(), 0, 0, 0));
                views.append(&mut text::text(104, 204, 3000, text1.into_iter().collect()));
                views.append(&mut text::text(104, 220, 3000, text2.into_iter().collect()));
            }
        }
        object::Output {
            instrs: instrs,
            events: vec![],
            views: views,
        }
    }
}
