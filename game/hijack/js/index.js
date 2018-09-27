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

    return {
        app: app,
        map: Asset.MAPS.get("hijack/map/test.json"),
        views: views,
        objects: objects,
        attacks: [],
        hits: hits
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

    for (var i = 0; i < game.objects.length; ++i)
        if (game.map.lock === game.objects[i].id) {
            const centralX = (game.objects[i].x + game.objects[i].width / 2) * SCALE;
            const centralY = (game.objects[i].y + game.objects[i].height / 2) * SCALE;
            game.app.stage.x = WIDTH * SCALE / 2 - centralX;
            game.app.stage.y = HEIGHT * SCALE / 2 - centralY;
            break;
        }

    game.app.renderer.render(game.app.stage);

    return game;
}

export function resize() {
    if (document.body.clientWidth < 480)
        document.getElementById("game").setAttribute("class", "single");
    else if (document.body.clientWidth < 960)
        document.getElementById("game").setAttribute("class", "double");
    else
        document.getElementById("game").setAttribute("class", "quadruple");
}
