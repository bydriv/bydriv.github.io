import * as Input from "./input.js";

export async function create(control) {
    switch (control.type) {
    case "playable":
        return Playable.create(control);
    case "random":
        return Random.create(control);
    case "wave":
        return Wave.create(control);
    default:
        console.log("undefined object type: %o", control.type);
    }
}

export const Playable = {
    create: async (control) => {
        return {
            type: "playable",
            input: Input.PLAYER
        };
    },
    step: async (game, control) => {
        return control;
    }
};

export const Random = {
    create: async (control) => {
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
    step: async (game, control) => {
        return Random.create(control);
    }
};

export const Wave = {
    create: async (control) => {
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
    step: async (game, control) => {
        return {
            type: "wave",
            input: {
                x: Math.cos(control.i * control.incr) * control.bias,
                y: Math.sin(control.j * control.incr) * control.bias,
                buttons: [false, false, false, false]
            },
            i: control.i % (Math.PI / control.incr) === (Math.PI / control.incr / 2) ? control.i + Math.round(Math.random()) * (Math.PI / control.incr) + 1 : control.i + 1,
            j: control.j % (Math.PI / control.incr) === 0 ? control.j + Math.round(Math.random()) * (Math.PI / control.
incr) + 1 : control.j + 1,
            incr: control.incr,
            bias: control.bias
        };
    }
};

export async function step(game, control) {
    switch (control.type) {
    case "playable":
        return Playable.step(game, control);
    case "random":
        return Random.step(game, control);
    case "wave":
        return Wave.step(game, control);
    default:
        console.log("undefined control type: %o", control.type);
    }
}
