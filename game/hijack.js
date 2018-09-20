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
    "hijack/pixelart/teiri/walk/back/3.png"
];

async function game_new() {
    PIXI.settings.SCALE_MODE = PIXI.SCALE_MODES.NEAREST;

    await font_load();
    await assets_load();

    game_resize();

    const app = new PIXI.Application({autoStart: false, width: 1920, height: 1280});
    app.stage.scale.set(8, 8);
    app.renderer.backgroundColor = 0xC0C0C0;

    const sprite1 = new PIXI.extras.AnimatedSprite([
        PIXI.loader.resources["hijack/pixelart/teiri/walk/front/0.png"].texture,
        PIXI.loader.resources["hijack/pixelart/teiri/walk/front/1.png"].texture,
        PIXI.loader.resources["hijack/pixelart/teiri/walk/front/2.png"].texture,
        PIXI.loader.resources["hijack/pixelart/teiri/walk/front/3.png"].texture
    ]);
    sprite1.animationSpeed = 1/10;
    sprite1.play();
    app.stage.addChild(sprite1);

    const sprite2 = new PIXI.extras.AnimatedSprite([
        PIXI.loader.resources["hijack/pixelart/teiri/walk/back/0.png"].texture,
        PIXI.loader.resources["hijack/pixelart/teiri/walk/back/1.png"].texture,
        PIXI.loader.resources["hijack/pixelart/teiri/walk/back/2.png"].texture,
        PIXI.loader.resources["hijack/pixelart/teiri/walk/back/3.png"].texture
    ]);
    sprite2.x = 16;
    sprite2.animationSpeed = 1/10;
    sprite2.play();
    app.stage.addChild(sprite2);

    document.getElementById("game").appendChild(app.view);

    return {
        app: app
    };
}

async function game_step(game) {
    const app = game.app;
    app.renderer.render(app.stage);
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
