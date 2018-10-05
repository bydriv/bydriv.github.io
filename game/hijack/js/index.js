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
    app.renderer.backgroundColor = 0x404040;

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

    const activeRect = {
        x: Asset.MAPS.get("hijack/map/test.json").x,
        y: Asset.MAPS.get("hijack/map/test.json").y,
        width: Asset.MAPS.get("hijack/map/test.json").width,
        height: Asset.MAPS.get("hijack/map/test.json").height
    };

    for (var i = 0; i < objects.length; ++i)
        if (Asset.MAPS.get("hijack/map/test.json").lock === objects[i].id) {
            const object = objects[i];

            const centralX = (object.x + object.width / 2);
            const centralY = (object.y + object.height / 2);
            activeRect.x = centralX - WIDTH / 2;
            activeRect.y = centralY - HEIGHT / 2;
            activeRect.width = WIDTH;
            activeRect.height = HEIGHT;

            break;
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

    const textStyle = new PIXI.TextStyle({
        lineHeight: 13,
        textBaseline: "top",
        fontFamily: "Misaki Gothic",
        fontSize: 8,
        fill: 0x00FF00
    });

    const cursorText1 = new PIXI.Text("┏━━┓", textStyle);
    const cursorText2 = new PIXI.Text("┃    ┃", textStyle);
    const cursorText3 = new PIXI.Text("┃    ┃", textStyle);
    const cursorText4 = new PIXI.Text("┗━━┛", textStyle);

    const log = [];

    for (var i = 0; i < 20; ++i) {
        const hijackingText = new PIXI.Text("", textStyle);
        const percentageText = new PIXI.Text("", textStyle);
        const progressText = new PIXI.Text("", textStyle);
        hijackingText.x = 0;
        hijackingText.y = i * 8 - 4;
        percentageText.x = 23 * 4;
        percentageText.y = i * 8 - 4;
        progressText.x = 27 * 4;
        progressText.y = i * 8 - 4;
        log.push({
            hijackingText: hijackingText,
            percentageText: percentageText,
            progressText: progressText
        });
    }

    const clearTextStyle = new PIXI.TextStyle({
        fontFamily: "IBM BIOS",
        fontSize: 16,
        fill: 0x00FF00
    });

    const gameoverTextStyle = new PIXI.TextStyle({
        fontFamily: "IBM BIOS",
        fontSize: 16,
        fill: 0xFF0000
    });

    const clearText = new PIXI.Text("ALL CLEAR!", clearTextStyle);
    clearText.x = 120 - "ALL CLEAR!".length * 16 / 2;
    clearText.y = 80 - 16 / 2 + 2;
    const gameoverText = new PIXI.Text("GAMEOVER", gameoverTextStyle);
    gameoverText.x = 120 - "GAMEOVER".length * 16 / 2;
    gameoverText.y = 80 - 16 / 2 + 2;

    var touches = [];

    app.view.addEventListener("touchstart", e => {
        touches = e.touches;
    });
    app.view.addEventListener("touchend", e => {
        if (touches.length === 1 && Input.PLAYER.x === 0 && Input.PLAYER.y === 0) {
            Input.PLAYER.buttons[0] = true;
            requestAnimationFrame(() => {
                Input.PLAYER.buttons[0] = false;
            });
        } else if (touches.length === 2) {
            Input.PLAYER.buttons[1] = true;
            requestAnimationFrame(() => {
                Input.PLAYER.buttons[1] = false;
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

    const left = Math.max(activeRect.x, Asset.MAPS.get("hijack/map/test.json").x);
    const top = Math.max(activeRect.y, Asset.MAPS.get("hijack/map/test.json").y);
    const right = Math.min(activeRect.x + activeRect.width, Asset.MAPS.get("hijack/map/test.json").x + Asset.MAPS.get("hijack/map/test.json").width);
    const bottom = Math.min(activeRect.y + activeRect.height, Asset.MAPS.get("hijack/map/test.json").y + Asset.MAPS.get("hijack/map/test.json").height);

    return {
        app: app,
        map: Asset.MAPS.get("hijack/map/test.json"),
        x: left,
        y: top,
        width: right - left,
        height: bottom - top,
        views: views,
        allObjects: objects,
        activeRect: activeRect,
        objects: activeObjects(activeRect, objects),
        hijacks: [],
        attacks: [],
        hits: hits,
        dialog: {
            window: window,
            textStyle: textStyle,
            cursorText1: cursorText1,
            cursorText2: cursorText2,
            cursorText3: cursorText3,
            cursorText4: cursorText4,
            log: log,
            clearText: clearText,
            gameoverText: gameoverText
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

    Promise.all(game.objects.map(object => Object.step(game, object)));

    for (var i = 0; i < game.objects.length; ++i) {
        for (var j = 0; j < game.hijacks.length; ++j)
            if (Object.collision(game.objects[i], game.hijacks[j]))
                await Object.onHijack(game, game.objects[i], game.hijacks[j]);

        for (var j = 0; j < game.attacks.length; ++j)
            if (!(game.hits.get(game.objects[i].id).has(game.attacks[j].id)))
                if (Object.collision(game.objects[i], game.attacks[j])) {
                    game.hits.get(game.objects[i].id).set(game.attacks[j].id, true);
                    await Object.onAttack(game, game.objects[i], game.attacks[j]);
                }

        if (game.objects[i].exiled) {
            game.allObjects = game.allObjects.filter(object => object.id !== game.objects[i].id);
            await View.teardown(game.app.stage, game.views.get(game.objects[i].id));
            game.views.delete(game.objects[i].id);
            game.hits.delete(game.objects[i].id);
            game.objects.splice(i, 1);
            --i;
        } else {
            await View.update(game.objects[i], game.views.get(game.objects[i].id));
        }
    }

    game.hijacks = [];
    game.attacks = [];

    game.dialog.window.renderable = false;

    for (var i = 0; i < game.objects.length; ++i)
        if (game.map.lock === game.objects[i].id) {
            const object = game.objects[i];

            if (!object.hijack) {
                const centralX = (object.x + object.width / 2) * SCALE;
                const centralY = (object.y + object.height / 2) * SCALE;
                game.app.stage.x = WIDTH * SCALE / 2 - centralX;
                game.app.stage.y = HEIGHT * SCALE / 2 - centralY;

                game.activeRect.x = centralX / SCALE - WIDTH / 2;
                game.activeRect.y = centralY / SCALE - HEIGHT / 2;
                game.activeRect.width = WIDTH;
                game.activeRect.height = HEIGHT;

                const left = Math.max(game.activeRect.x, Asset.MAPS.get("hijack/map/test.json").x);
                const top = Math.max(game.activeRect.y, Asset.MAPS.get("hijack/map/test.json").y);
                const right = Math.min(game.activeRect.x + game.activeRect.width, Asset.MAPS.get("hijack/map/test.json").x + Asset.MAPS.get("hijack/map/test.json").width);
                const bottom = Math.min(game.activeRect.y + game.activeRect.height, Asset.MAPS.get("hijack/map/test.json").y + Asset.MAPS.get("hijack/map/test.json").height);

                game.x = left;
                game.y = top;
                game.width = right - left;
                game.height = bottom - top;
            } else {
                const centralX = (object.hijack.x + object.hijack.width / 2) * SCALE;
                const centralY = (object.hijack.y + object.hijack.height / 2) * SCALE;
                game.app.stage.x = WIDTH * SCALE / 2 - centralX;
                game.app.stage.y = HEIGHT * SCALE / 2 - centralY;

                game.activeRect.x = centralX / SCALE - WIDTH / 2;
                game.activeRect.y = centralY / SCALE - HEIGHT / 2;
                game.activeRect.width = WIDTH;
                game.activeRect.height = HEIGHT;

                const left = Math.max(game.activeRect.x, Asset.MAPS.get("hijack/map/test.json").x);
                const top = Math.max(game.activeRect.y, Asset.MAPS.get("hijack/map/test.json").y);
                const right = Math.min(game.activeRect.x + game.activeRect.width, Asset.MAPS.get("hijack/map/test.json").x + Asset.MAPS.get("hijack/map/test.json").width);
                const bottom = Math.min(game.activeRect.y + game.activeRect.height, Asset.MAPS.get("hijack/map/test.json").y + Asset.MAPS.get("hijack/map/test.json").height);

                game.x = left;
                game.y = top;
                game.width = right - left;
                game.height = bottom - top;
            }

            if (object.hijack) {
                game.dialog.window.renderable = true;

                const targets = game.objects.filter(o => o.hijackable && Team.enemy(o, object) && Object.collision(o, object.hijack));

                game.dialog.textStyle.fill = targets.length === 0 ? 0x008000 : 0x00FF00;

                game.dialog.cursorText1.x = object.hijack.x + game.app.stage.x / SCALE;
                game.dialog.cursorText1.y = object.hijack.y + game.app.stage.y / SCALE - 4;
                game.dialog.cursorText2.x = object.hijack.x + game.app.stage.x / SCALE;
                game.dialog.cursorText2.y = object.hijack.y + game.app.stage.y / SCALE + 8 - 4;
                game.dialog.cursorText3.x = object.hijack.x + game.app.stage.x / SCALE;
                game.dialog.cursorText3.y = object.hijack.y + game.app.stage.y / SCALE + 16 - 4;
                game.dialog.cursorText4.x = object.hijack.x + game.app.stage.x / SCALE;
                game.dialog.cursorText4.y = object.hijack.y + game.app.stage.y / SCALE + 24 - 4;
                game.dialog.window.addChild(game.dialog.cursorText1);
                game.dialog.window.addChild(game.dialog.cursorText2);
                game.dialog.window.addChild(game.dialog.cursorText3);
                game.dialog.window.addChild(game.dialog.cursorText4);

                for (var i = 0; i < targets.length; ++i) {
                    const target = targets[i];
                    game.dialog.log[i].hijackingText.text = "[" + (i + 1) + " of " + targets.length +  "] Hijacking";
                    game.dialog.log[i].percentageText.text = ("  " + Math.floor((60 - target.security) / 60 * 100)).slice(-3) + "%";
                    game.dialog.log[i].progressText.text = "[" + ("=".repeat(Math.floor((60 - target.security) / 60 * 100 / 5)) + " ".repeat(20)).slice(0, 20) + "] eta " + ("   " + Math.floor(target.security / 60 * 1000)).slice(-4) + "ms";
                    game.dialog.window.addChild(game.dialog.log[i].hijackingText);
                    game.dialog.window.addChild(game.dialog.log[i].percentageText);
                    game.dialog.window.addChild(game.dialog.log[i].progressText);
                }
            }

            break;
        }

    game.dialog.window.x = -game.app.stage.x / SCALE;
    game.dialog.window.y = -game.app.stage.y / SCALE;

    if (await gameover(game)) {
        game.dialog.window.renderable = true;
        game.dialog.window.addChild(game.dialog.gameoverText);
    } else if (await clear(game)) {
        game.dialog.window.renderable = true;
        game.dialog.window.addChild(game.dialog.clearText);
    }

    game.app.renderer.render(game.app.stage);

    game.dialog.window.removeChildren();

    game.objects = activeObjects(game.activeRect, game.allObjects);

    return game;
}

async function clear(game) {
    return !game.allObjects.some(object => object.team === "enemy" && object.type !== "shot" && object.hijackedByTeam !== "player");
}

async function gameover(game) {
    return !game.allObjects.some(object => object.team === "player");
}

function activeObjects(activeRect, objects) {
    return objects.filter(object => object.team === "player" || Object.collision(activeRect, object));
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
