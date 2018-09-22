import * as Input from "./input.js";

export const Playable = {
    create: async () => {
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
    create: async (bias) => {
        return {
            type: "random",
            input: {
                x: (Math.random() * 2 - 1) * bias,
                y: (Math.random() * 2 - 1) * bias,
                buttons: [false, false, false, false]
            },
            bias: bias
        };
    },
    step: async (game, control) => {
        return Random.create(control.bias);
    }
};

export const Wave = {
    create: async (incr, bias) => {
        const i = Math.random() * (Math.PI / incr);
        const j = i + Math.round(Math.random()) * (Math.PI / incr);

        return {
            type: "wave",
            input: {
                x: Math.cos(i * incr) * bias,
                y: Math.sin(j * incr) * bias,
                buttons: [false, false, false, false]
            },
            i: i,
            j: j,
            incr: incr,
            bias: bias
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
            i: control.i % (Math.PI / control.incr) === 0 ? control.i + Math.round(Math.random()) * (Math.PI / control.incr) + 1 : control.i + 1,
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
