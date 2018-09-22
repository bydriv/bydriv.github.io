import * as Input from "./input.js";

export const Playable = {
    create: async () => {
        return {
            type: "playable",
            state: null,
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
            state: bias,
            input: {
                x: (Math.random() * 2 - 1) * bias,
                y: (Math.random() * 2 - 1) * bias
            }
        };
    },
    step: async (game, control) => {
        return await Random.create(control.state);
    }
};

export async function step(game, control) {
    switch (control.type) {
    case "playable":
        return await Playable.step(game, control);
    case "random":
        return await Random.step(game, control);
    default:
        console.log("undefined control type: %o", control.type);
    }
}
