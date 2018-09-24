import * as Input from "./input.js";
import * as Object from "./object.js";
import * as Team from "./team.js";

export async function create(object, control) {
    switch (control.type) {
    case "playable":
        return Playable.create(object, control);
    case "random":
        return Random.create(object, control);
    case "wave":
        return Wave.create(object, control);
    case "typeA":
        return TypeA.create(object, control);
    default:
        console.log("undefined object type: %o", control.type);
    }
}

export async function step(game, object, control) {
    switch (control.type) {
    case "playable":
        return Playable.step(game, object, control);
    case "random":
        return Random.step(game, object, control);
    case "wave":
        return Wave.step(game, object, control);
    case "typeA":
        return TypeA.step(game, object, control);
    default:
        console.log("undefined control type: %o", control.type);
    }
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
        const sight = {
            width: object.width * 3,
            height: object.height * 3
        };

        const attack = {
        };

        switch (object.direction) {
        case "left":
            sight.x = object.x - object.width * 2;
            sight.y = object.y - object.height;
            attack.x = object.x - object.width / 2;
            attack.y = object.y;
            attack.width = object.width / 2;
            attack.height = object.height;
            break;
        case "back":
            sight.x = object.x - object.width;
            sight.y = object.y - object.height * 2;
            attack.x = object.x;
            attack.y = object.y - object.height / 2;
            attack.width = object.width;
            attack.height = object.height / 2;
            break;
        case "right":
            sight.x = object.x;
            sight.y = object.y - object.height;
            attack.x = object.x + object.width + object.width / 2;
            attack.y = object.y;
            attack.width = object.width / 2;
            attack.height = object.height;
            break;
        case "front":
            sight.x = object.x - object.width;
            sight.y = object.y;
            attack.x = object.x;
            attack.y = object.y + object.height + object.height / 2;
            attack.width = object.width;
            attack.height = object.height / 2;
            break;
        }

        for (var i = 0; i < game.objects.length; ++i)
            if (Team.enemy(object.team, game.objects[i].team))
                if (Object.collision(game.objects[i], attack))
                    if (game.objects[i].shield) // TODO
                    return {
                        type: "typeA",
                        input: {
                            x: 0,
                            y: 0,
                            buttons: [true, false, false, false]
                        },
                        wave: control.wave
                    };

        for (var i = 0; i < game.objects.length; ++i)
            if (Team.enemy(object.team, game.objects[i].team))
                if (game.objects[i].shield) // TODO
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
