window.addEventListener("load", () => {
    game_new().then(game => {
        requestAnimationFrame(function step () {
            game_step(game).then(next_game => {
                game = next_game;
                requestAnimationFrame(step);
            });
        });
    });
});

window.addEventListener("resize", () => {
    game_resize();
});

const ASSETS = [
    "hijack/pixelart/teiri/walk/front/0.png",
    "hijack/pixelart/teiri/walk/front/1.png",
    "hijack/pixelart/teiri/walk/front/2.png",
    "hijack/pixelart/teiri/walk/front/3.png",
    "hijack/pixelart/teiri/walk/back/0.png",
    "hijack/pixelart/teiri/walk/back/1.png",
    "hijack/pixelart/teiri/walk/back/2.png",
    "hijack/pixelart/teiri/walk/back/3.png",
    "hijack/pixelart/teiri/walk/right/0.png",
    "hijack/pixelart/teiri/walk/right/1.png",
    "hijack/pixelart/teiri/walk/right/2.png",
    "hijack/pixelart/teiri/walk/right/3.png",
    "hijack/pixelart/teiri/walk/left/0.png",
    "hijack/pixelart/teiri/walk/left/1.png",
    "hijack/pixelart/teiri/walk/left/2.png",
    "hijack/pixelart/teiri/walk/left/3.png"
];

async function game_new() {
    PIXI.settings.SCALE_MODE = PIXI.SCALE_MODES.NEAREST;

    await font_load();
    await assets_load();

    game_resize();

    const game = {}

    const app = new PIXI.Application({autoStart: false, width: 1920, height: 1280});
    app.stage.scale.set(8, 8);
    app.renderer.backgroundColor = 0xC0C0C0;

    const sprite = new PIXI.extras.AnimatedSprite([
        PIXI.loader.resources["hijack/pixelart/teiri/walk/front/0.png"].texture,
        PIXI.loader.resources["hijack/pixelart/teiri/walk/front/1.png"].texture,
        PIXI.loader.resources["hijack/pixelart/teiri/walk/front/2.png"].texture,
        PIXI.loader.resources["hijack/pixelart/teiri/walk/front/3.png"].texture
    ]);
    sprite.onFrameChange = () => {
        if (game.gamepad) {
            const gamepadX = game.gamepad.axes[0];
            const gamepadY = game.gamepad.axes[1];

            if (gamepadX < -0.25)
                sprite.x -= 8;
            if (gamepadX > 0.25)
                sprite.x += 8;
            if (gamepadY < -0.25)
                sprite.y -= 8;
            if (gamepadY > 0.25)
                sprite.y += 8;
        }
    }
    sprite.animationSpeed = 1/6;
    sprite.play();
    app.stage.addChild(sprite);

    document.getElementById("game").appendChild(app.view);

    game.app = app;
    game.gamepad = navigator.getGamepads()[0];
    game.heroine = {
        sprite: sprite,
        direction: "front"
    }

    return game;
}

async function game_step(game) {
    game.gamepad = navigator.getGamepads()[0];

    if (game.gamepad) {
        const gamepadX = game.gamepad.axes[0];
        const gamepadY = game.gamepad.axes[1];

        if (gamepadY < -0.25) {
            if (game.heroine.direction !== "back") {
                game.heroine.direction = "back";
                game.heroine.sprite.textures = [
                    PIXI.loader.resources["hijack/pixelart/teiri/walk/back/0.png"].texture,
                    PIXI.loader.resources["hijack/pixelart/teiri/walk/back/1.png"].texture,
                    PIXI.loader.resources["hijack/pixelart/teiri/walk/back/2.png"].texture,
                    PIXI.loader.resources["hijack/pixelart/teiri/walk/back/3.png"].texture
                ];
                game.heroine.sprite.play();
            }
        } else if (gamepadY > 0.25) {
            if (game.heroine.direction !== "front") {
                game.heroine.direction = "front";
                game.heroine.sprite.textures = [
                    PIXI.loader.resources["hijack/pixelart/teiri/walk/front/0.png"].texture,
                    PIXI.loader.resources["hijack/pixelart/teiri/walk/front/1.png"].texture,
                    PIXI.loader.resources["hijack/pixelart/teiri/walk/front/2.png"].texture,
                    PIXI.loader.resources["hijack/pixelart/teiri/walk/front/3.png"].texture
                ];
                game.heroine.sprite.play();
            }
        } else if (gamepadX < -0.25) {
            if (game.heroine.direction !== "left") {
                game.heroine.direction = "left";
                game.heroine.sprite.textures = [
                    PIXI.loader.resources["hijack/pixelart/teiri/walk/left/0.png"].texture,
                    PIXI.loader.resources["hijack/pixelart/teiri/walk/left/1.png"].texture,
                    PIXI.loader.resources["hijack/pixelart/teiri/walk/left/2.png"].texture,
                    PIXI.loader.resources["hijack/pixelart/teiri/walk/left/3.png"].texture
                ];
                game.heroine.sprite.play();
            }
        } else if (gamepadX > 0.25) {
            if (game.heroine.direction !== "right") {
                game.heroine.direction = "right";
                game.heroine.sprite.textures = [
                    PIXI.loader.resources["hijack/pixelart/teiri/walk/right/0.png"].texture,
                    PIXI.loader.resources["hijack/pixelart/teiri/walk/right/1.png"].texture,
                    PIXI.loader.resources["hijack/pixelart/teiri/walk/right/2.png"].texture,
                    PIXI.loader.resources["hijack/pixelart/teiri/walk/right/3.png"].texture
                ];
                game.heroine.sprite.play();
            }
        }
    }

    game.app.renderer.render(game.app.stage);
    return game;
}

function game_resize() {
    if (document.body.clientWidth < 480)
        document.getElementById("game").setAttribute("class", "single");
    else if (document.body.clientWidth < 960)
        document.getElementById("game").setAttribute("class", "double");
    else
        document.getElementById("game").setAttribute("class", "quadruple");
}

function assets_load() {
    return new Promise(resolve => {
        PIXI.loader.add(ASSETS).load(() => {
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
