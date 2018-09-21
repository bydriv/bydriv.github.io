window.addEventListener("load", () => {
    game_new().then(game => {
        requestAnimationFrame(function step () {
            game_step(game).then(() => {
                requestAnimationFrame(step);
            });
        });
    });
});

window.addEventListener("resize", () => {
    game_resize();
});

const ASSETS = [
    "hijack/pixelart/teiri/walk.png"
];

const TEXTURES = {};

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
            case "p":
                game.input.y = -1;
                return e.preventDefault();
            case "f":
                game.input.x = 1;
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
            case "k":
            case "w":
            case "Up":
            case "ArrowUp":
                game.input.y = -1;
                return e.preventDefault();
            case "l":
            case "d":
            case "Right":
            case "ArrowRight":
                game.input.x = 1;
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
            case "p":
            case "f":
            case "n":
                game.input.x = 0;
                game.input.y = 0;
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
        TEXTURES["hijack/pixelart/teiri/walk/front/0.png"],
        TEXTURES["hijack/pixelart/teiri/walk/front/1.png"],
        TEXTURES["hijack/pixelart/teiri/walk/front/2.png"],
        TEXTURES["hijack/pixelart/teiri/walk/front/3.png"]
    ]);
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
                TEXTURES["hijack/pixelart/teiri/walk/back/0.png"],
                TEXTURES["hijack/pixelart/teiri/walk/back/1.png"],
                TEXTURES["hijack/pixelart/teiri/walk/back/2.png"],
                TEXTURES["hijack/pixelart/teiri/walk/back/3.png"]
            ];
            game.heroine.sprite.play();
        }
    } else if (game.input.y > 0.25) {
        if (game.heroine.direction !== "front") {
            game.heroine.direction = "front";
            game.heroine.sprite.textures = [
                TEXTURES["hijack/pixelart/teiri/walk/front/0.png"],
                TEXTURES["hijack/pixelart/teiri/walk/front/1.png"],
                TEXTURES["hijack/pixelart/teiri/walk/front/2.png"],
                TEXTURES["hijack/pixelart/teiri/walk/front/3.png"]
            ];
            game.heroine.sprite.play();
        }
    } else if (game.input.x < -0.25) {
        if (game.heroine.direction !== "left") {
            game.heroine.direction = "left";
            game.heroine.sprite.textures = [
                TEXTURES["hijack/pixelart/teiri/walk/left/0.png"],
                TEXTURES["hijack/pixelart/teiri/walk/left/1.png"],
                TEXTURES["hijack/pixelart/teiri/walk/left/2.png"],
                TEXTURES["hijack/pixelart/teiri/walk/left/3.png"]
            ];
            game.heroine.sprite.play();
        }
    } else if (game.input.x > 0.25) {
        if (game.heroine.direction !== "right") {
            game.heroine.direction = "right";
            game.heroine.sprite.textures = [
                TEXTURES["hijack/pixelart/teiri/walk/right/0.png"],
                TEXTURES["hijack/pixelart/teiri/walk/right/1.png"],
                TEXTURES["hijack/pixelart/teiri/walk/right/2.png"],
                TEXTURES["hijack/pixelart/teiri/walk/right/3.png"]
            ];
            game.heroine.sprite.play();
        }
    }

    if (game.input.x < -0.25)
        game.heroine.sprite.x -= 1;
    if (game.input.x > 0.25)
        game.heroine.sprite.x += 1;
    if (game.input.y < -0.25)
        game.heroine.sprite.y -= 1;
    if (game.input.y > 0.25)
        game.heroine.sprite.y += 1;

    game.app.renderer.render(game.app.stage);
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
            for (var i = 0; i < 4; ++i) {
                const name = i === 0 ? "left" : i === 1 ? "back" : i === 2 ? "right" : "front";

                for (var j = 0; j < 4; ++j) {
                    const path = "hijack/pixelart/teiri/walk/" + name + "/" + j + ".png";

                    TEXTURES[path] = new PIXI.Texture(PIXI.loader.resources["hijack/pixelart/teiri/walk.png"].texture, new PIXI.Rectangle(j * 16, i * 16, 16, 16));
                    //TEXTURES[path].orig = new PIXI.Rectangle(0, 0, 64, 64);
                    //TEXTURES[path].trim = ;
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
