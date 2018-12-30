use super::*;

pub fn new() -> Map {
    Map {
        templates: vec![
            (
                "map/boston/1000-1000.json".to_string(),
                MapTemplate {
                    x: 0,
                    y: 0,
                    width: 320,
                    height: 240,
                    objects: vec![
                        (
                            brownfox::Control::Immovable(brownfox::Immovable::new()),
                            object::Object::Transport(object::transport::new(
                                48,
                                -1,
                                224,
                                1,
                                "map/boston/1000-999.json".to_string(),
                                48,
                                224,
                            )),
                        ),
                        (
                            brownfox::Control::Immovable(brownfox::Immovable::new()),
                            object::Object::Animated(object::animated::new(
                                8,
                                vec![
                                    object::Object::Maptip(object::maptip::new(
                                        -16,
                                        -16,
                                        -1000,
                                        16,
                                        16,
                                        vec![brownfox::Rectangle::new(0, 0, 22, 3)],
                                        "pixelart/maptip/water/".to_string(),
                                        "/0.png".to_string(),
                                    )),
                                    object::Object::Maptip(object::maptip::new(
                                        -16,
                                        -16,
                                        -1000,
                                        16,
                                        16,
                                        vec![brownfox::Rectangle::new(0, 0, 22, 3)],
                                        "pixelart/maptip/water/".to_string(),
                                        "/1.png".to_string(),
                                    )),
                                    object::Object::Maptip(object::maptip::new(
                                        -16,
                                        -16,
                                        -1000,
                                        16,
                                        16,
                                        vec![brownfox::Rectangle::new(0, 0, 22, 3)],
                                        "pixelart/maptip/water/".to_string(),
                                        "/2.png".to_string(),
                                    )),
                                    object::Object::Maptip(object::maptip::new(
                                        -16,
                                        -16,
                                        -1000,
                                        16,
                                        16,
                                        vec![brownfox::Rectangle::new(0, 0, 22, 3)],
                                        "pixelart/maptip/water/".to_string(),
                                        "/3.png".to_string(),
                                    )),
                                    object::Object::Maptip(object::maptip::new(
                                        -16,
                                        -16,
                                        -1000,
                                        16,
                                        16,
                                        vec![brownfox::Rectangle::new(0, 0, 22, 3)],
                                        "pixelart/maptip/water/".to_string(),
                                        "/4.png".to_string(),
                                    )),
                                    object::Object::Maptip(object::maptip::new(
                                        -16,
                                        -16,
                                        -1000,
                                        16,
                                        16,
                                        vec![brownfox::Rectangle::new(0, 0, 22, 3)],
                                        "pixelart/maptip/water/".to_string(),
                                        "/5.png".to_string(),
                                    )),
                                    object::Object::Maptip(object::maptip::new(
                                        -16,
                                        -16,
                                        -1000,
                                        16,
                                        16,
                                        vec![brownfox::Rectangle::new(0, 0, 22, 3)],
                                        "pixelart/maptip/water/".to_string(),
                                        "/6.png".to_string(),
                                    )),
                                    object::Object::Maptip(object::maptip::new(
                                        -16,
                                        -16,
                                        -1000,
                                        16,
                                        16,
                                        vec![brownfox::Rectangle::new(0, 0, 22, 3)],
                                        "pixelart/maptip/water/".to_string(),
                                        "/7.png".to_string(),
                                    )),
                                ],
                            )),
                        ),
                        (
                            brownfox::Control::Immovable(brownfox::Immovable::new()),
                            object::Object::Maptip(object::maptip::new(
                                0,
                                0,
                                -1000,
                                16,
                                16,
                                vec![brownfox::Rectangle::new(-1, 1, 22, 4)],
                                "pixelart/maptip/ground/".to_string(),
                                ".png".to_string(),
                            )),
                        ),
                        (
                            brownfox::Control::Immovable(brownfox::Immovable::new()),
                            object::Object::Maptip(object::maptip::new(
                                0,
                                0,
                                -1000,
                                16,
                                16,
                                vec![
                                    brownfox::Rectangle::new(6, -1, 8, 17),
                                    brownfox::Rectangle::new(-1, 6, 22, 8),
                                ],
                                "pixelart/maptip/asphalt/".to_string(),
                                ".png".to_string(),
                            )),
                        ),
                        (
                            brownfox::Control::Immovable(brownfox::Immovable::new()),
                            object::Object::Maptip(object::maptip::new(
                                0,
                                0,
                                -1000,
                                16,
                                16,
                                vec![
                                    brownfox::Rectangle::new(3, -1, 4, 8),
                                    brownfox::Rectangle::new(-1, 3, 4, 4),
                                    brownfox::Rectangle::new(13, -1, 4, 8),
                                    brownfox::Rectangle::new(17, 3, 4, 4),
                                    brownfox::Rectangle::new(-1, 13, 8, 4),
                                    brownfox::Rectangle::new(13, 13, 8, 4),
                                ],
                                "pixelart/maptip/mortar/".to_string(),
                                ".png".to_string(),
                            )),
                        ),
                        (
                            brownfox::Control::Immovable(brownfox::Immovable::new()),
                            object::Object::Maptip(object::maptip::new(
                                92,
                                0,
                                -1000,
                                32,
                                32,
                                vec![
                                    brownfox::Rectangle::new(0, 0, 1, 3),
                                    brownfox::Rectangle::new(0, 6, 1, 2),
                                ],
                                "pixelart/maptip/street-light/right/".to_string(),
                                ".png".to_string(),
                            )),
                        ),
                        (
                            brownfox::Control::Immovable(brownfox::Immovable::new()),
                            object::Object::Maptip(object::maptip::new(
                                196,
                                0,
                                -1000,
                                32,
                                32,
                                vec![
                                    brownfox::Rectangle::new(0, 0, 1, 3),
                                    brownfox::Rectangle::new(0, 6, 1, 2),
                                ],
                                "pixelart/maptip/street-light/left/".to_string(),
                                ".png".to_string(),
                            )),
                        ),
                        (
                            brownfox::Control::Immovable(brownfox::Immovable::new()),
                            object::Object::Maptip(object::maptip::new(
                                0,
                                80,
                                -1000,
                                32,
                                32,
                                vec![
                                    brownfox::Rectangle::new(0, 0, 3, 1),
                                    brownfox::Rectangle::new(7, 0, 3, 1),
                                ],
                                "pixelart/maptip/street-light/front/".to_string(),
                                ".png".to_string(),
                            )),
                        ),
                        (
                            brownfox::Control::Immovable(brownfox::Immovable::new()),
                            object::Object::Maptip(object::maptip::new(
                                0,
                                180,
                                -1000,
                                32,
                                32,
                                vec![
                                    brownfox::Rectangle::new(0, 0, 3, 1),
                                    brownfox::Rectangle::new(7, 0, 3, 1),
                                ],
                                "pixelart/maptip/street-light/back/".to_string(),
                                ".png".to_string(),
                            )),
                        ),
                        (
                            brownfox::Control::Immovable(brownfox::Immovable::new()),
                            object::Object::Maptip(object::maptip::new(
                                -16,
                                0,
                                -1000,
                                32,
                                32,
                                vec![
                                    brownfox::Rectangle::new(-1, 0, 4, 2),
                                    brownfox::Rectangle::new(8, 0, 4, 2),
                                    brownfox::Rectangle::new(-1, 6, 4, 2),
                                    brownfox::Rectangle::new(7, 6, 2, 2),
                                ],
                                "pixelart/maptip/tree/".to_string(),
                                ".png".to_string(),
                            )),
                        ),
                        (
                            brownfox::Control::Immovable(brownfox::Immovable::new()),
                            object::Object::Maptip(object::maptip::new(
                                256,
                                64,
                                1000,
                                16,
                                16,
                                vec![brownfox::Rectangle::new(0, 0, 8, 6)],
                                "pixelart/maptip/building/top/".to_string(),
                                ".png".to_string(),
                            )),
                        ),
                        (
                            brownfox::Control::Immovable(brownfox::Immovable::new()),
                            object::Object::Maptip(object::maptip::new(
                                256,
                                64,
                                1000,
                                16,
                                16,
                                vec![brownfox::Rectangle::new(0, 5, 8, 20)],
                                "pixelart/maptip/building/bottom/".to_string(),
                                ".png".to_string(),
                            )),
                        ),
                    ],
                },
            ),
            (
                "map/boston/1000-999.json".to_string(),
                MapTemplate {
                    x: 0,
                    y: 0,
                    width: 320,
                    height: 240,
                    objects: vec![
                        (
                            brownfox::Control::Immovable(brownfox::Immovable::new()),
                            object::Object::Transport(object::transport::new(
                                48,
                                240,
                                224,
                                1,
                                "map/boston/1000-1000.json".to_string(),
                                48,
                                16,
                            )),
                        ),
                        (
                            brownfox::Control::Immovable(brownfox::Immovable::new()),
                            object::Object::Animated(object::animated::new(
                                8,
                                vec![
                                    object::Object::Maptip(object::maptip::new(
                                        -16,
                                        -16,
                                        -1000,
                                        16,
                                        16,
                                        vec![brownfox::Rectangle::new(0, 0, 22, 17)],
                                        "pixelart/maptip/water/".to_string(),
                                        "/0.png".to_string(),
                                    )),
                                    object::Object::Maptip(object::maptip::new(
                                        -16,
                                        -16,
                                        -1000,
                                        16,
                                        16,
                                        vec![brownfox::Rectangle::new(0, 0, 22, 17)],
                                        "pixelart/maptip/water/".to_string(),
                                        "/1.png".to_string(),
                                    )),
                                    object::Object::Maptip(object::maptip::new(
                                        -16,
                                        -16,
                                        -1000,
                                        16,
                                        16,
                                        vec![brownfox::Rectangle::new(0, 0, 22, 17)],
                                        "pixelart/maptip/water/".to_string(),
                                        "/2.png".to_string(),
                                    )),
                                    object::Object::Maptip(object::maptip::new(
                                        -16,
                                        -16,
                                        -1000,
                                        16,
                                        16,
                                        vec![brownfox::Rectangle::new(0, 0, 22, 17)],
                                        "pixelart/maptip/water/".to_string(),
                                        "/3.png".to_string(),
                                    )),
                                    object::Object::Maptip(object::maptip::new(
                                        -16,
                                        -16,
                                        -1000,
                                        16,
                                        16,
                                        vec![brownfox::Rectangle::new(0, 0, 22, 17)],
                                        "pixelart/maptip/water/".to_string(),
                                        "/4.png".to_string(),
                                    )),
                                    object::Object::Maptip(object::maptip::new(
                                        -16,
                                        -16,
                                        -1000,
                                        16,
                                        16,
                                        vec![brownfox::Rectangle::new(0, 0, 22, 17)],
                                        "pixelart/maptip/water/".to_string(),
                                        "/5.png".to_string(),
                                    )),
                                    object::Object::Maptip(object::maptip::new(
                                        -16,
                                        -16,
                                        -1000,
                                        16,
                                        16,
                                        vec![brownfox::Rectangle::new(0, 0, 22, 17)],
                                        "pixelart/maptip/water/".to_string(),
                                        "/6.png".to_string(),
                                    )),
                                    object::Object::Maptip(object::maptip::new(
                                        -16,
                                        -16,
                                        -1000,
                                        16,
                                        16,
                                        vec![brownfox::Rectangle::new(0, 0, 22, 17)],
                                        "pixelart/maptip/water/".to_string(),
                                        "/7.png".to_string(),
                                    )),
                                ],
                            )),
                        ),
                        (
                            brownfox::Control::Immovable(brownfox::Immovable::new()),
                            object::Object::Maptip(object::maptip::new(
                                0,
                                0,
                                -1000,
                                16,
                                16,
                                vec![brownfox::Rectangle::new(6, -1, 8, 17)],
                                "pixelart/maptip/asphalt/".to_string(),
                                ".png".to_string(),
                            )),
                        ),
                        (
                            brownfox::Control::Immovable(brownfox::Immovable::new()),
                            object::Object::Maptip(object::maptip::new(
                                0,
                                0,
                                -1000,
                                16,
                                16,
                                vec![
                                    brownfox::Rectangle::new(3, -1, 4, 17),
                                    brownfox::Rectangle::new(13, -1, 4, 17),
                                ],
                                "pixelart/maptip/mortar/".to_string(),
                                ".png".to_string(),
                            )),
                        ),
                        (
                            brownfox::Control::Immovable(brownfox::Immovable::new()),
                            object::Object::Maptip(object::maptip::new(
                                64,
                                56,
                                -1000,
                                32,
                                128,
                                vec![brownfox::Rectangle::new(0, 0, 1, 1)],
                                "pixelart/maptip/halfway-to-hell/left/".to_string(),
                                ".png".to_string(),
                            )),
                        ),
                        (
                            brownfox::Control::Immovable(brownfox::Immovable::new()),
                            object::Object::Maptip(object::maptip::new(
                                224,
                                56,
                                -1000,
                                32,
                                128,
                                vec![brownfox::Rectangle::new(0, 0, 1, 1)],
                                "pixelart/maptip/halfway-to-hell/right/".to_string(),
                                ".png".to_string(),
                            )),
                        ),
                        (
                            brownfox::Control::Immovable(brownfox::Immovable::new()),
                            object::Object::Maptip(object::maptip::new(
                                92,
                                0,
                                -1000,
                                32,
                                32,
                                vec![brownfox::Rectangle::new(0, 0, 1, 7)],
                                "pixelart/maptip/street-light/right/".to_string(),
                                ".png".to_string(),
                            )),
                        ),
                        (
                            brownfox::Control::Immovable(brownfox::Immovable::new()),
                            object::Object::Maptip(object::maptip::new(
                                196,
                                0,
                                -1000,
                                32,
                                32,
                                vec![brownfox::Rectangle::new(0, 0, 1, 7)],
                                "pixelart/maptip/street-light/left/".to_string(),
                                ".png".to_string(),
                            )),
                        ),
                    ],
                },
            ),
        ]
        .into_iter()
        .collect(),
    }
}
