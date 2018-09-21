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

async function game_new() {
    PIXI.settings.SCALE_MODE = PIXI.SCALE_MODES.NEAREST;

    await Promise.all([font_load(), assets_load()]);

    game_resize();

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

    return {
        app: app,
        objects: [
            {
                type: "teiri",
                sprite: sprite,
                direction: "front"
            }
        ]
    };
}

async function object_step(object) {
    switch (object.type) {
    case "teiri":
        if (INPUT.y < -0.25) {
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
        } else if (INPUT.y > 0.25) {
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
        } else if (INPUT.x < -0.25) {
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
        } else if (INPUT.x > 0.25) {
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

        if (INPUT.x < -0.25)
            object.sprite.x -= 1;
        if (INPUT.x > 0.25)
            object.sprite.x += 1;
        if (INPUT.y < -0.25)
            object.sprite.y -= 1;
        if (INPUT.y > 0.25)
            object.sprite.y += 1;
        break;
    }
}

async function game_step(game) {
    const gamepad = navigator.getGamepads()[0];

    if (gamepad) {
        INPUT.x = gamepad.axes[0];
        INPUT.y = gamepad.axes[1];
    }

    for (var i = 0; i < game.objects.length; ++i)
        await object_step(game.objects[i]);

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
