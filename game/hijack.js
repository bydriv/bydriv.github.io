window.addEventListener("load", function () {
    game_new(function (game) {
        requestAnimationFrame(function step () {
            game_step(game, function (next_game) {
                game = next_game;
                requestAnimationFrame(step);
            });
        });
    });
});

const ASSETS = [
    "hijack/pixelart/teiri/walk/front/0.png",
    "hijack/pixelart/teiri/walk/front/1.png",
    "hijack/pixelart/teiri/walk/front/2.png",
    "hijack/pixelart/teiri/walk/front/3.png"
];

function game_new(k) {
    WebFont.load({
        custom: {
            families: ["IBM BIOS", "Misaki Gothic"]
        },
        active: function () {

            PIXI.settings.SCALE_MODE = PIXI.SCALE_MODES.NEAREST;

            PIXI.loader.add(ASSETS).load(function () {
                const app = new PIXI.Application({autoStart: false, width: 1920, height: 1280});
                app.stage.scale.set(8, 8);
                app.renderer.backgroundColor = 0xC0C0C0;

                const sprite = new PIXI.extras.AnimatedSprite([
                    PIXI.loader.resources["hijack/pixelart/teiri/walk/front/0.png"].texture,
                    PIXI.loader.resources["hijack/pixelart/teiri/walk/front/1.png"].texture,
                    PIXI.loader.resources["hijack/pixelart/teiri/walk/front/2.png"].texture,
                    PIXI.loader.resources["hijack/pixelart/teiri/walk/front/3.png"].texture
                ]);
                sprite.animationSpeed = 1/10;
                sprite.play();
                app.stage.addChild(sprite);

                document.getElementById("game").appendChild(app.view);

                k({
                    app: app
                });
            });
        }
    });
}

function game_step(game, k) {
    const app = game.app;

    if (document.body.clientWidth < 480)
        document.getElementById("game").setAttribute("class", "single");
    else if (document.body.clientWidth < 960)
        document.getElementById("game").setAttribute("class", "double");
    else
        document.getElementById("game").setAttribute("class", "quadruple");

    app.renderer.render(app.stage);

    k(game);
}
