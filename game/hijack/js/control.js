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

export async function step(game, control) {
    switch (control.type) {
    case "playable":
        return Playable.step(game, control);
    case "random":
        return Random.step(game, control);
    default:
        console.log("undefined control type: %o", control.type);
    }
}
