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
            INPUT.x = -1;
            return e.preventDefault();
        case "p":
            INPUT.y = -1;
            return e.preventDefault();
        case "f":
            INPUT.x = 1;
            return e.preventDefault();
        case "n":
            INPUT.y = 1;
            return e.preventDefault();
        };
    } else {
        switch (e.key) {
        case "h":
        case "a":
        case "Left":
        case "ArrowLeft":
            INPUT.x = -1;
            return e.preventDefault();
        case "k":
        case "w":
        case "Up":
        case "ArrowUp":
            INPUT.y = -1;
            return e.preventDefault();
        case "l":
        case "d":
        case "Right":
        case "ArrowRight":
            INPUT.x = 1;
            return e.preventDefault();
        case "j":
        case "s":
        case "Down":
        case "ArrowDown":
            INPUT.y = 1;
            return e.preventDefault();
        };
    }
});

const INPUT_EMPTY = {
    x: 0,
    y: 0
};

const INPUT = {
    x: 0,
    y: 0
};

window.addEventListener("keyup", (e) => {
    if (e.ctrlKey) {
        switch (e.key) {
        case "b":
        case "p":
        case "f":
        case "n":
            INPUT.x = 0;
            INPUT.y = 0;
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
            INPUT.x = 0;
            INPUT.y = 0;
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

        const teiri = await Teiri.create(app);

        return {
            app: app,
            objects: [teiri]
        };
    },
    step: async (game) => {
        const gamepad = navigator.getGamepads()[0];

        if (gamepad) {
            INPUT.x = gamepad.axes[0];
            INPUT.y = gamepad.axes[1];
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
    Object: {
        control: async (game, control) => {
            switch (control.type) {
            case "playable":
                return {
                    type: control.type,
                    state: control.state,
                    input: INPUT
                };
            default:
                return {
                    type: control.type,
                    state: control.state,
                    input: INPUT_EMPTY
                };
            }
        },
        step: async (game, object) => {
            object.control = await Game.Object.control(game, object.control);

            switch (object.type) {
            case "teiri":
                await Teiri.step(game, object);
                break;
            }
        }
    }
};

const Teiri = {
    create: async (app) => {
        const sprite = new PIXI.extras.AnimatedSprite([
            TEXTURES["hijack/pixelart/teiri/walk/front/0.png"],
            TEXTURES["hijack/pixelart/teiri/walk/front/1.png"],
            TEXTURES["hijack/pixelart/teiri/walk/front/2.png"],
            TEXTURES["hijack/pixelart/teiri/walk/front/3.png"]
        ]);
        sprite.animationSpeed = 1/8;
        sprite.play();
        app.stage.addChild(sprite);

        return {
            type: "teiri",
            control: {
                type: "playable",
                state: null,
                input: INPUT_EMPTY
            },
            sprite: sprite,
            direction: "front"
        };
    },
    step: async (game, object) => {
        const input = object.control.input;

        if (input.y < -0.25) {
            if (object.direction !== "back") {
                object.direction = "back";
                object.sprite.textures = [
                    TEXTURES["hijack/pixelart/teiri/walk/back/0.png"],
                    TEXTURES["hijack/pixelart/teiri/walk/back/1.png"],
                    TEXTURES["hijack/pixelart/teiri/walk/back/2.png"],
                    TEXTURES["hijack/pixelart/teiri/walk/back/3.png"]
                ];
                object.sprite.play();
            }
        } else if (input.y > 0.25) {
            if (object.direction !== "front") {
                object.direction = "front";
                object.sprite.textures = [
                    TEXTURES["hijack/pixelart/teiri/walk/front/0.png"],
                    TEXTURES["hijack/pixelart/teiri/walk/front/1.png"],
                    TEXTURES["hijack/pixelart/teiri/walk/front/2.png"],
                    TEXTURES["hijack/pixelart/teiri/walk/front/3.png"]
                ];
                object.sprite.play();
            }
        } else if (input.x < -0.25) {
            if (object.direction !== "left") {
                object.direction = "left";
                object.sprite.textures = [
                    TEXTURES["hijack/pixelart/teiri/walk/left/0.png"],
                    TEXTURES["hijack/pixelart/teiri/walk/left/1.png"],
                    TEXTURES["hijack/pixelart/teiri/walk/left/2.png"],
                    TEXTURES["hijack/pixelart/teiri/walk/left/3.png"]
                ];
                object.sprite.play();
            }
        } else if (input.x > 0.25) {
            if (object.direction !== "right") {
                object.direction = "right";
                object.sprite.textures = [
                    TEXTURES["hijack/pixelart/teiri/walk/right/0.png"],
                    TEXTURES["hijack/pixelart/teiri/walk/right/1.png"],
                    TEXTURES["hijack/pixelart/teiri/walk/right/2.png"],
                    TEXTURES["hijack/pixelart/teiri/walk/right/3.png"]
                ];
                object.sprite.play();
            }
        }

        if (input.x < -0.25)
            object.sprite.x -= 1;
        if (input.x > 0.25)
            object.sprite.x += 1;
        if (input.y < -0.25)
            object.sprite.y -= 1;
        if (input.y > 0.25)
            object.sprite.y += 1;
    }
}

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
