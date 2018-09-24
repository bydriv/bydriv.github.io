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
        const attack = {
            width: object.width,
            height: object.height
        };

        switch (object.direction) {
        case "left":
            attack.x = object.x - object.width;
            attack.y = object.y;
            break;
        case "back":
            attack.x = object.x;
            attack.y = object.y - object.height;
            break;
        case "right":
            attack.x = object.x + object.width;
            attack.y = object.y;
            break;
        case "front":
            attack.x = object.x;
            attack.y = object.y + object.height;
            break;
        }

        for (var i = 0; i < game.objects.length; ++i)
            if (Team.enemy(object.team, game.objects[i].team))
                if (Object.collision(game.objects[i], attack))
                    return {
                        type: "typeA",
                        input: {
                            x: 0,
                            y: 0,
                            buttons: [true, false, false, false]
                        },
                        wave: control.wave
                    };

        const wave = await Wave.step(game, object, control.wave);

        return {
            type: "typeA",
            input: wave.input,
            wave: wave
        };
    }
};
