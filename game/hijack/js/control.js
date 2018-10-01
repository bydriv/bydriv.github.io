// signature CONTROL = sig
//   type control
//   type control_scheme
//   val create : (Object : OBJECT) => Object.object * control_scheme -> control proimise
//   val step : (Object : OBJECT) => Game.game * Object.object * control -> control promise
// end

import * as Input from "./input.js";
import * as Object from "./object.js";
import * as Team from "./team.js";

const INSTANCES = new Map();

export async function create(object, control) {
    if (INSTANCES.has(control.type))
        return INSTANCES.get(control.type).create(object, control);
    else
        console.error("undefined control type: %o", control.type);
}

export async function step(game, object, control) {
    if (INSTANCES.has(control.type))
        return INSTANCES.get(control.type).step(game, object, control);
    else
        console.error("undefined control type: %o", control.type);
}

export const Playable = {
    create: async (object, control) => {
        return {
            type: "playable",
            input: Input.PLAYER
        };
    },
    step: async (game, object, control) => {
        return control;
    }
};
INSTANCES.set("playable", Playable);

export const Random = {
    create: async (object, control) => {
        return {
            type: "random",
            input: {
                x: (Math.random() * 2 - 1) * control.bias,
                y: (Math.random() * 2 - 1) * control.bias,
                buttons: [false, false, false, false]
            },
            bias: control.bias
        };
    },
    step: async (game, object, control) => {
        return Random.create(object, control);
    }
};
INSTANCES.set("random", Random);

export const Wave = {
    create: async (object, control) => {
        const i = Math.floor(Math.random() *  Math.floor(Math.PI / control.incr));
        const j = i + Math.round(Math.random()) *  Math.floor(Math.PI / control.incr);

        return {
            type: "wave",
            input: {
                x: Math.cos(i * control.incr) * control.bias,
                y: Math.sin(j * control.incr) * control.bias,
                buttons: [false, false, false, false]
            },
            i: i,
            j: j,
            incr: control.incr,
            bias: control.bias
        };
    },
    step: async (game, object, control) => {
        return {
            type: "wave",
            input: {
                x: Math.cos(control.i * control.incr) * control.bias,
                y: Math.sin(control.j * control.incr) * control.bias,
                buttons: [false, false, false, false]
            },
            i: control.i % Math.floor(Math.PI / control.incr) === Math.floor(Math.PI / control.incr / 2) ? control.i + Math.round(Math.random()) * Math.floor(Math.PI / control.incr) + 1 : control.i + 1,
            j: control.j % Math.floor(Math.PI / control.incr) === 0 ? control.j + Math.round(Math.random()) * Math.floor(Math.PI / control.incr) + 1 : control.j + 1,
            incr: control.incr,
            bias: control.bias
        };
    }
};
INSTANCES.set("wave", Wave);

export const TypeA = {
    create: async (object, control) => {
        const wave = await Wave.create(object, control);

        return {
            type: "typeA",
            input: wave.input,
            wave: wave
        };
    },
    step: async (game, object, control) => {
        for (var i = 0; i < game.objects.length; ++i)
            if (Team.enemy(object, game.objects[i]))
                for (var j = 0; j < object.buttons.length; ++j)
                    if (object.buttons[j] && (object.buttons[j].type === "attack" || object.buttons[j].type === "shot")) {
                        const attack = {
                            x: object.x + object.buttons[j][object.direction].x,
                            y: object.y + object.buttons[j][object.direction].y,
                            width: object.buttons[j][object.direction].width,
                            height: object.buttons[j][object.direction].height
                        };

                        if (Object.collision(game.objects[i], attack))
                            return {
                                type: "typeA",
                                input: {
                                    x: 0,
                                    y: 0,
                                    buttons: [j === 0, j === 1, j === 2, j === 3]
                                },
                                wave: control.wave
                            };
                    }

        const sight = {
            x: object.x + object.sight[object.direction].x,
            y: object.y + object.sight[object.direction].y,
            width: object.sight[object.direction].width,
            height: object.sight[object.direction].height
        };

        for (var i = 0; i < game.objects.length; ++i)
            if (Team.enemy(object, game.objects[i]))
                if (Object.collision(game.objects[i], sight)) {
                    const x = game.objects[i].x - object.x;
                    const y = game.objects[i].y - object.y;

                    return {
                        type: "typeA",
                        input: {
                            x: x / Math.max(Math.abs(x), Math.abs(y), 1),
                            y: y / Math.max(Math.abs(x), Math.abs(y), 1),
                            buttons: [false, false, false, false]
                        },
                        wave: control.wave
                    };
                }

        const wave = await Wave.step(game, object, control.wave);

        return {
            type: "typeA",
            input: wave.input,
            wave: wave
        };
    }
};
INSTANCES.set("typeA", TypeA);
