window.addEventListener("load", () => {
    Game.create().then(game => {
        requestAnimationFrame(function step () {
            Game.step(game).then(next_game => {
                game = next_game;
                requestAnimationFrame(step);
            });
        });
    });
});

window.addEventListener("resize", () => {
    Game.resize();
});

window.addEventListener("keydown", (e) => {
    if (e.ctrlKey) {
        switch (e.key) {
        case "b":
            Game.Input.PLAYER.x = -1;
            return e.preventDefault();
        case "p":
            Game.Input.PLAYER.y = -1;
            return e.preventDefault();
        case "f":
            Game.Input.PLAYER.x = 1;
            return e.preventDefault();
        case "n":
            Game.Input.PLAYER.y = 1;
            return e.preventDefault();
        };
    } else {
        switch (e.key) {
        case "h":
        case "a":
        case "Left":
        case "ArrowLeft":
            Game.Input.PLAYER.x = -1;
            return e.preventDefault();
        case "k":
        case "w":
        case "Up":
        case "ArrowUp":
            Game.Input.PLAYER.y = -1;
            return e.preventDefault();
        case "l":
        case "d":
        case "Right":
        case "ArrowRight":
            Game.Input.PLAYER.x = 1;
            return e.preventDefault();
        case "j":
        case "s":
        case "Down":
        case "ArrowDown":
            Game.Input.PLAYER.y = 1;
            return e.preventDefault();
        };
    }
});

window.addEventListener("keyup", (e) => {
    if (e.ctrlKey) {
        switch (e.key) {
        case "b":
        case "p":
        case "f":
        case "n":
            Game.Input.PLAYER.x = 0;
            Game.Input.PLAYER.y = 0;
            return e.preventDefault();
        };
    } else {
        switch (e.key) {
        case "h":
        case "k":
        case "l":
        case "j":
        case "a":
        case "w":
        case "d":
        case "s":
        case "Left":
        case "Up":
        case "Right":
        case "Down":
        case "ArrowLeft":
        case "ArrowUp":
        case "ArrowRight":
        case "ArrowDown":
            Game.Input.PLAYER.x = 0;
            Game.Input.PLAYER.y = 0;
            return e.preventDefault();
        };
    }
});

const ASSETS = [
    "hijack/pixelart/teiri/walk.png"
];

const TEXTURES = {};

const Game = {
    create: async () => {
        PIXI.settings.SCALE_MODE = PIXI.SCALE_MODES.NEAREST;

        await Promise.all([font_load(), assets_load()]);

        Game.resize();

        const app = new PIXI.Application({autoStart: false, width: 1920, height: 1280});
        app.stage.scale.set(8, 8);
        app.renderer.backgroundColor = 0xC0C0C0;

        document.getElementById("game").appendChild(app.view);

        const teiri = await Game.Object.Teiri.create(0, 0, await Game.Control.Playable.create());
        const teiri2 = await Game.Object.Teiri.create(60, 40, await Game.Control.Random.create(0.252));
        const teiri3 = await Game.Object.Teiri.create(120, 40, await Game.Control.Random.create(0.252));
        const teiri4 = await Game.Object.Teiri.create(60, 80, await Game.Control.Random.create(0.252));
        const teiri5 = await Game.Object.Teiri.create(120, 80, await Game.Control.Random.create(0.252));
        teiri.sprite.play();
        app.stage.addChild(teiri.sprite);
        teiri2.sprite.play();
        app.stage.addChild(teiri2.sprite);
        teiri3.sprite.play();
        app.stage.addChild(teiri3.sprite);
        teiri4.sprite.play();
        app.stage.addChild(teiri4.sprite);
        teiri5.sprite.play();
        app.stage.addChild(teiri5.sprite);

        return {
            app: app,
            objects: [teiri, teiri2, teiri3, teiri4, teiri5]
        };
    },
    step: async (game) => {
        const gamepad = navigator.getGamepads()[0];

        if (gamepad) {
            Game.Input.PLAYER.x = gamepad.axes[0];
            Game.Input.PLAYER.y = gamepad.axes[1];
        }

        for (var i = 0; i < game.objects.length; ++i)
            await Game.Object.step(game, game.objects[i]);

        game.app.renderer.render(game.app.stage);

        return game;
    },
    resize: () => {
        if (document.body.clientWidth < 480)
            document.getElementById("game").setAttribute("class", "single");
        else if (document.body.clientWidth < 960)
            document.getElementById("game").setAttribute("class", "double");
        else
            document.getElementById("game").setAttribute("class", "quadruple");
    },
    Input: {
        EMPTY: {
            x: 0,
            y: 0
        },
        PLAYER: {
            x: 0,
            y: 0
        }
    },
    Control: {
        Playable: {
            create: async () => {
                return {
                    type: "playable",
                    state: null,
                    input: Game.Input.PLAYER
                };
            },
            step: async (game, control) => {
                return control;
            }
        },
        Random: {
            create: async (bias) => {
                return {
                    type: "random",
                    state: bias,
                    input: {
                        x: (Math.random() * 2 - 1) * bias,
                        y: (Math.random() * 2 - 1) * bias
                    }
                };
            },
            step: async (game, control) => {
                return await Game.Control.Random.create(control.state);
            }
        },
        step: async (game, control) => {
            switch (control.type) {
            case "playable":
                return await Game.Control.Playable.step(game, control);
            case "random":
                return await Game.Control.Random.step(game, control);
            default:
                console.log("undefined control type: %o", control.type);
            }
        }
    },
    Object: {
        step: async (game, object) => {
            object.control = await Game.Control.step(game, object.control);

            switch (object.type) {
            case "teiri":
                return await Game.Object.Teiri.step(game, object);
            default:
                console.log("undefined object type: %o", object.type);
            }
        },
        Teiri: {
            create: async (x, y, control) => {
                const sprite = new PIXI.extras.AnimatedSprite([
                    TEXTURES["hijack/pixelart/teiri/walk/front/0.png"],
                    TEXTURES["hijack/pixelart/teiri/walk/front/1.png"],
                    TEXTURES["hijack/pixelart/teiri/walk/front/2.png"],
                    TEXTURES["hijack/pixelart/teiri/walk/front/3.png"]
                ]);
                sprite.x = x;
                sprite.y = y;
                sprite.animationSpeed = 1/8;

                return {
                    type: "teiri",
                    control: control,
                    sprite: sprite,
                    direction: "front",
                    count: 0,
                    x: x,
                    y: y
                };
            },
            step: async (game, object) => {
                const input = object.control.input;

                if (input.y < -0.25) {
                    if (object.direction !== "back") {
                        object.sprite.textures = [
                            TEXTURES["hijack/pixelart/teiri/walk/back/0.png"],
                            TEXTURES["hijack/pixelart/teiri/walk/back/1.png"],
                            TEXTURES["hijack/pixelart/teiri/walk/back/2.png"],
                            TEXTURES["hijack/pixelart/teiri/walk/back/3.png"]
                        ];
                        object.sprite.play()
                        object.direction = "back";
                        object.count = 0;
                    }
                } else if (input.y > 0.25) {
                    if (object.direction !== "front") {
                        object.sprite.textures = [
                            TEXTURES["hijack/pixelart/teiri/walk/front/0.png"],
                            TEXTURES["hijack/pixelart/teiri/walk/front/1.png"],
                            TEXTURES["hijack/pixelart/teiri/walk/front/2.png"],
                            TEXTURES["hijack/pixelart/teiri/walk/front/3.png"]
                        ];
                        object.sprite.play()
                        object.direction = "front";
                        object.count = 0;
                    }
                } else if (input.x < -0.25) {
                    if (object.direction !== "left") {
                        object.sprite.textures = [
                            TEXTURES["hijack/pixelart/teiri/walk/left/0.png"],
                            TEXTURES["hijack/pixelart/teiri/walk/left/1.png"],
                            TEXTURES["hijack/pixelart/teiri/walk/left/2.png"],
                            TEXTURES["hijack/pixelart/teiri/walk/left/3.png"]
                        ];
                        object.sprite.play()
                        object.direction = "left";
                        object.count = 0;
                    }
                } else if (input.x > 0.25) {
                    if (object.direction !== "right") {
                        object.sprite.textures = [
                            TEXTURES["hijack/pixelart/teiri/walk/right/0.png"],
                            TEXTURES["hijack/pixelart/teiri/walk/right/1.png"],
                            TEXTURES["hijack/pixelart/teiri/walk/right/2.png"],
                            TEXTURES["hijack/pixelart/teiri/walk/right/3.png"]
                        ];
                        object.sprite.play()
                        object.direction = "right";
                        object.count = 0;
                    }
                }

                if (object.count % 8 === 0) {
                    if (input.x < -0.25)
                        object.sprite.x -= 8;
                    if (input.x > 0.25)
                        object.sprite.x += 8;
                    if (input.y < -0.25)
                        object.sprite.y -= 8;
                    if (input.y > 0.25)
                        object.sprite.y += 8;
                }

                if (input.x < -0.25 || input.x > 0.25 || input.y < -0.25 || input.y > 0.25)
                    ++object.count;
            }
        }
    }
};

function assets_load() {
    return new Promise(resolve => {
        PIXI.loader.add(ASSETS).load(() => {
            for (var i = 0; i < 4; ++i) {
                const name = i === 0 ? "left" : i === 1 ? "back" : i === 2 ? "right" : "front";

                for (var j = 0; j < 4; ++j) {
                    const path = "hijack/pixelart/teiri/walk/" + name + "/" + j + ".png";

                    TEXTURES[path] = new PIXI.Texture(PIXI.loader.resources["hijack/pixelart/teiri/walk.png"].texture, new PIXI.Rectangle(j * 16, i * 16, 16, 16));
                }
            }
            resolve();
        });
    });
}

function font_load() {
    return new Promise(resolve => {
        WebFont.load({
            custom: {
                families: ["IBM BIOS", "Misaki Gothic"]
            },
            active: () => {
                resolve();
            }
        });
    });
}
