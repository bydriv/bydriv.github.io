import * as Input from "./input.js";

export async function create(object, control) {
    switch (control.type) {
    case "playable":
        return Playable.create(object, control);
    case "random":
        return Random.create(object, control);
    case "wave":
        return Wave.create(object, control);
    default:
        console.log("undefined object type: %o", control.type);
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
        const i = Math.floor(Math.random() * (Math.PI / control.incr));
        const j = i + Math.round(Math.random()) * (Math.PI / control.incr);

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

export async function step(game, object, control) {
    switch (control.type) {
    case "playable":
        return Playable.step(game, object, control);
    case "random":
        return Random.step(game, object, control);
    case "wave":
        return Wave.step(game, object, control);
    default:
        console.log("undefined control type: %o", control.type);
    }
}
