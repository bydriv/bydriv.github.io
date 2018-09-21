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

    await Promise.all([font_load(), assets_load()]);

    game_resize();

    const game = {}

    game.input = {
        x: 0,
        y: 0
    };

    window.addEventListener("keydown", (e) => {
        if (e.ctrlKey) {
            switch (e.key) {
            case "b":
                game.input.x = -1;
                return e.preventDefault();
            case "f":
                game.input.x = 1;
                return e.preventDefault();
            case "p":
                game.input.y = -1;
                return e.preventDefault();
            case "n":
                game.input.y = 1;
                return e.preventDefault();
            };
        } else {
            switch (e.key) {
            case "h":
            case "a":
            case "Left":
            case "ArrowLeft":
                game.input.x = -1;
                return e.preventDefault();
            case "l":
            case "d":
            case "Right":
            case "ArrowRight":
                game.input.x = 1;
                return e.preventDefault();
            case "k":
            case "w":
            case "Up":
            case "ArrowUp":
                game.input.y = -1;
                return e.preventDefault();
            case "j":
            case "s":
            case "Down":
            case "ArrowDown":
                game.input.y = 1;
                return e.preventDefault();
            };
        }
    });

    window.addEventListener("keyup", (e) => {
        if (e.ctrlKey) {
            switch (e.key) {
            case "b":
            case "f":
            case "p":
            case "n":
                game.input.x = 0;
                game.input.y = 0;
                return e.preventDefault();
            };
        } else {
            switch (e.key) {
            case "h":
            case "l":
            case "k":
            case "j":
            case "a":
            case "d":
            case "w":
            case "s":
            case "Left":
            case "Right":
            case "Up":
            case "Down":
            case "ArrowLeft":
            case "ArrowRight":
            case "ArrowUp":
            case "ArrowDown":
                game.input.x = 0;
                game.input.y = 0;
                return e.preventDefault();
            };
        }
    });

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
        if (game.input.x < -0.25)
            sprite.x -= 8;
        if (game.input.x > 0.25)
            sprite.x += 8;
        if (game.input.y < -0.25)
            sprite.y -= 8;
        if (game.input.y > 0.25)
            sprite.y += 8;
    }
    sprite.animationSpeed = 1/8;
    sprite.play();
    app.stage.addChild(sprite);

    document.getElementById("game").appendChild(app.view);

    game.app = app;
    game.heroine = {
        sprite: sprite,
        direction: "front"
    }

    return game;
}

async function game_step(game) {
    const gamepad = navigator.getGamepads()[0];

    if (gamepad) {
        game.input.x = gamepad.axes[0];
        game.input.y = gamepad.axes[1];
    }

    if (game.input.y < -0.25) {
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
    } else if (game.input.y > 0.25) {
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
    } else if (game.input.x < -0.25) {
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
    } else if (game.input.x > 0.25) {
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
