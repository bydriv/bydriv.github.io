use super::*;

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
    }
}

impl brownfox::Moore<object::Input, object::Output> for Episode {
    fn transit(&self, input: &object::Input) -> Episode {
        let cleared = self.clear_condition.satisfy(&input.previous);

        let mut objects = self.episode_objects.clone();
        objects.append(&mut self.map_objects.clone());

        let events: Vec<Event> = objects
            .iter()
            .flat_map(|object| object.1.output().events)
            .collect();

        let mut episode_objects = Box::new(self.episode_objects.iter().map(|object| {
            let control = (0..60 / input.previous.fps).fold(object.0.clone(), |control, _| {
                control.transit(&input.inputs)
            });
            let input0 = control.output();
            (
                control,
                object.1.transit(&object::Input {
                    inputs: vec![input0],
                    previous: input.previous.clone(),
                }),
            )
        }))
            as Box<Iterator<Item = (brownfox::Control<i32>, object::Object)>>;

        let mut map_objects = Box::new(self.map_objects.iter().map(|object| {
            let control = (0..60 / input.previous.fps).fold(object.0.clone(), |control, _| {
                control.transit(&input.inputs)
            });
            let input0 = control.output();
            (
                control,
                object.1.transit(&object::Input {
                    inputs: vec![input0],
                    previous: input.previous.clone(),
                }),
            )
        }))
            as Box<Iterator<Item = (brownfox::Control<i32>, object::Object)>>;
        for event in &events {
            episode_objects =
                Box::new(episode_objects.map(move |object| (object.0.clone(), object.1.on(event))))
                    as Box<Iterator<Item = (brownfox::Control<i32>, object::Object)>>;
            map_objects =
                Box::new(map_objects.map(move |object| (object.0.clone(), object.1.on(event))))
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
        object::Output {
            instrs: instrs,
            events: vec![],
            views: views,
        }
    }
}
