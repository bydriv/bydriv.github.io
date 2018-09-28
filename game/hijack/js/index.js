// signature GAME = sig
//   type game
//   type attack = {
//     x : int,
//     y : int,
//     width : int,
//     height : int,
//     damage : int
//   }
//   val create : unit -> game promise
//   val step : game -> game promise
//   val resize : unit -> unit
// end

// structure Game : GAME

import * as Asset from "./asset.js";
import * as Control from "./control.js";
import * as Font from "./font.js";
import * as Gensym from "./gensym.js";
import * as Input from "./input.js";
import * as Object from "./object.js";
import * as Team from "./team.js";
import * as View from "./view.js";

export {Asset, Control, Font, Input, Object, Team, View};

const WIDTH = 240;
const HEIGHT = 160;
const SCALE = 8;

var currentScale = SCALE;

export async function create() {
    PIXI.settings.SCALE_MODE = PIXI.SCALE_MODES.NEAREST;

    await Promise.all([Font.load(), Asset.load()]);

    resize();

    const app = new PIXI.Application({autoStart: false, width: WIDTH * SCALE, height: HEIGHT * SCALE});
    app.stage.scale.set(SCALE, SCALE);
    app.renderer.backgroundColor = 0xC0C0C0;

    document.getElementById("game").appendChild(app.view);

    const objects = [];
    const views = new Map();
    const hits = new Map();

    for (var i = 0; i < Asset.MAPS.get("hijack/map/test.json").objects.length; ++i) {
        const object = await Object.create(Asset.MAPS.get("hijack/map/test.json").objects[i]);
        const view = await View.create(object);
        await View.setup(app.stage, view);
        objects.push(object);
        views.set(object.id, view);
        hits.set(object.id, new Map());
    }

    const window = new PIXI.Graphics();
    window.x = 0;
    window.y = 0;
    window.width = WIDTH;
    window.height = HEIGHT;
    window.beginFill(0x000000, 0.5);
    window.drawRect(0, 0, WIDTH, HEIGHT);
    window.renderable = false;
    app.stage.addChild(window);

    var touches = [];

    app.view.addEventListener("touchstart", e => {
        touches = e.touches;
    });
    app.view.addEventListener("touchend", e => {
        if (touches.length === 1 && Input.PLAYER.x === 0 && Input.PLAYER.y === 0) {
            Input.PLAYER.buttons[0] = true;
            requestAnimationFrame(() => {
                requestAnimationFrame(() => {
                    requestAnimationFrame(() => {
                        requestAnimationFrame(() => {
                            requestAnimationFrame(() => {
                                requestAnimationFrame(() => {
                                    requestAnimationFrame(() => {
                                        requestAnimationFrame(() => {
                                            Input.PLAYER.buttons[0] = false;
                                        });
                                    });
                                });
                            });
                        });
                    });
                });
            });
        } else if (touches.length === 2) {
            Input.PLAYER.buttons[1] = true;
            requestAnimationFrame(() => {
                Input.PLAYER.buttons[1] = false;
            });
            Input.PLAYER.buttons[2] = true;
            requestAnimationFrame(() => {
                Input.PLAYER.buttons[2] = false;
            });
        } else {
            Input.PLAYER.x = 0;
            Input.PLAYER.y = 0;
        }
    });
    app.view.addEventListener("touchcancel", e => {
    });
    app.view.addEventListener("touchmove", e => {
        if (e.touches.length === 1) {
            Input.PLAYER.x = (e.touches[0].clientX - app.view.getBoundingClientRect().x) / currentScale / WIDTH * 4 - 1;
            Input.PLAYER.y = (e.touches[0].clientY - app.view.getBoundingClientRect().y) / currentScale / HEIGHT * 4 - 1;
        }
    });

    return {
        app: app,
        map: Asset.MAPS.get("hijack/map/test.json"),
        views: views,
        objects: objects,
        attacks: [],
        hits: hits,
        dialog: {
            window: window,
            log: []
        }
    };
}

export async function step(game) {
    const gamepad = navigator.getGamepads()[0];

    if (gamepad) {
        Input.PLAYER.x = gamepad.axes[0];
        Input.PLAYER.y = gamepad.axes[1];
        Input.PLAYER.buttons = [gamepad.buttons[0].pressed, gamepad.buttons[1].pressed, gamepad.buttons[2].pressed, gamepad.buttons[3].pressed];
    }

    for (var i = 0; i < game.objects.length; ++i)
        game.objects[i] = await Object.step(game, game.objects[i]);

    for (var i = 0; i < game.objects.length; ++i)
        for (var j = 0; j < game.attacks.length; ++j)
            if (!(game.hits.get(game.objects[i].id).has(game.attacks[j].id)))
                if (Object.collision(game.objects[i], game.attacks[j])) {
                    game.hits.get(game.objects[i].id).set(game.attacks[j].id, true);
                    await Object.onAttack(game, game.objects[i], game.attacks[j]);
                }

    for (var i = 0; i < game.objects.length; ++i)
        if (game.objects[i].exiled) {
            await View.teardown(game.app.stage, game.views.get(game.objects[i].id));
            game.views.delete(game.objects[i].id);
            game.hits.delete(game.objects[i].id);
        }

    game.objects = game.objects.filter(object => !object.exiled);

    game.attacks = [];

    for (var i = 0; i < game.objects.length; ++i)
        await View.update(game.objects[i], game.views.get(game.objects[i].id));

    game.dialog.window.renderable = false;

    for (var i = 0; i < game.objects.length; ++i)
        if (game.map.lock === game.objects[i].id) {
            const object = game.objects[i];
            const centralX = (object.x + object.width / 2) * SCALE;
            const centralY = (object.y + object.height / 2) * SCALE;
            game.app.stage.x = WIDTH * SCALE / 2 - centralX;
            game.app.stage.y = HEIGHT * SCALE / 2 - centralY;

            if (object.hijack) {
                game.dialog.window.renderable = true;

                var i = 0;
                for (; i < game.dialog.log.length; ++i)
                    for (var j = 0; j < game.dialog.log[i].length; ++j)
                        game.dialog.window.addChild(game.dialog.log[i][j]);

                const enStyle = new PIXI.TextStyle({
                    lineHeight: 12,
                    textBaseline: "top",
                    fontFamily: "Misaki Gothic",
                    fontSize: 8,
                    fill: "white"
                });

                if (object.hijack.target && game.objects.find(o => o.id == object.hijack.target)) {
                    const hijackText = new PIXI.Text("$ hijack ", enStyle);
                    const idText = new PIXI.Text(object.hijack.target, enStyle);
                    hijackText.x = 0;
                    hijackText.y = i * 8 - 4;
                    idText.x = "$ hijack ".length * 4;
                    idText.y = i * 8  - 4;
                    game.dialog.window.addChild(hijackText);
                    game.dialog.window.addChild(idText);

                    const target = game.objects.find(o => o.id == object.hijack.target);
                    const hijackingText = new PIXI.Text("Hijacking ", enStyle);
                    const percentageText = new PIXI.Text(("  " + Math.floor(object.hijack.count / target.security * 100)).slice(-3) + "%", enStyle);
                    const progressText = new PIXI.Text("[" + ("=".repeat(Math.floor(object.hijack.count / target.security * 100 / 5)) + " ".repeat(20)).slice(0, 20) + "]", enStyle);
                    hijackingText.x = 0;
                    hijackingText.y = i * 8 + 8 - 4;
                    percentageText.x = 34 * 4;
                    percentageText.y = i * 8 + 8 - 4;
                    progressText.x = 38 * 4;
                    progressText.y = i * 8 + 8 - 4;
                    game.dialog.window.addChild(hijackingText);
                    game.dialog.window.addChild(percentageText);
                    game.dialog.window.addChild(progressText);

                    if (object.control.input.buttons[0]) {
                        if (object.hijack.count < target.security) {
                        } else {
                            if (game.dialog.log.length < 19) {
                                game.dialog.log.push([hijackText, idText]);
                                game.dialog.log.push([hijackingText, percentageText, progressText]);
                            } else if (game.dialog.log.length < 20) {
                                game.dialog.log.shift();
                                game.dialog.log.push([hijackText, idText]);
                                game.dialog.log.push([hijackingText, percentageText, progressText]);

                                i = 0;
                                for (; i < game.dialog.log.length; ++i)
                                    for (var j = 0; j < game.dialog.log[i].length; ++j)
                                        game.dialog.log[i][j].y -= 8;
                            } else {
                                game.dialog.log.shift();
                                game.dialog.log.shift();
                                game.dialog.log.push([hijackText, idText]);
                                game.dialog.log.push([hijackingText, percentageText, progressText]);

                                i = 0;
                                for (; i < game.dialog.log.length; ++i)
                                    for (var j = 0; j < game.dialog.log[i].length; ++j)
                                        game.dialog.log[i][j].y -= 16;
                            }
                        }
                    }
                }
            }

            break;
        }

    game.dialog.window.x = -game.app.stage.x / SCALE;
    game.dialog.window.y = -game.app.stage.y / SCALE;

    game.app.renderer.render(game.app.stage);

    game.dialog.window.removeChildren();

    return game;
}

export function resize() {
    if (document.body.clientWidth < 480) {
        currentScale = 2;
        document.getElementById("game").setAttribute("class", "single");
    } else if (document.body.clientWidth < 960) {
        currentScale = 4;
        document.getElementById("game").setAttribute("class", "double");
    } else {
        currentScale = 8;
        document.getElementById("game").setAttribute("class", "quadruple");
    }
}
