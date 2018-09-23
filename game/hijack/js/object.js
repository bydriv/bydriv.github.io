import * as Asset from "./asset.js";
import * as Control from "./control.js";

export async function create(object) {
    switch (object.type) {
    case "teiri":
        return Teiri.create(object);
    default:
        console.log("undefined object type: %o", object.type);
    }
}

export async function createSprite(object) {
    switch (object.type) {
    case "teiri":
        return Teiri.createSprite(object);
    default:
        console.log("undefined object type: %o", object.type);
    }
}

export async function step(game, object) {
    object.control = await Control.step(game, object.control);

    switch (object.type) {
    case "teiri":
        return Teiri.step(game, object);
    default:
        console.log("undefined object type: %o", object.type);
    }
};

export const Teiri = {
    create: async (object) => {
        return {
            type: "teiri",
            id: object.id,
            control: await Control.create(object.control),
            pose: object.pose,
            direction: object.direction,
            count: 0,
            x: object.x,
            y: object.y
        };
    },
    createSprite: async (object) => {
        const sprite = new PIXI.extras.AnimatedSprite([
            Asset.TEXTURES["hijack/pixelart/teiri/walk/front/0.png"],
            Asset.TEXTURES["hijack/pixelart/teiri/walk/front/1.png"],
            Asset.TEXTURES["hijack/pixelart/teiri/walk/front/2.png"],
            Asset.TEXTURES["hijack/pixelart/teiri/walk/front/3.png"]
        ]);
        sprite.x = object.x;
        sprite.y = object.y;
        sprite.animationSpeed = 1/8;

        return sprite;
    },
    step: async (game, object) => {
        switch (object.pose) {
        case "walk":
            const input = object.control.input;

            if (input.buttons[0]) {
                game.sprites[object.id].textures = [
                    Asset.TEXTURES["hijack/pixelart/teiri/truncheon/" + object.direction + "/0.png"],
                    Asset.TEXTURES["hijack/pixelart/teiri/truncheon/" + object.direction + "/1.png"],
                    Asset.TEXTURES["hijack/pixelart/teiri/truncheon/" + object.direction + "/2.png"],
                    Asset.TEXTURES["hijack/pixelart/teiri/truncheon/" + object.direction + "/3.png"]
                ];
                game.sprites[object.id].x = object.x - 8;
                game.sprites[object.id].y = object.y - 8;
                game.sprites[object.id].animationSpeed = 1/3;
                game.sprites[object.id].play()
                object.pose = "truncheon";
                object.count = 0;
                return Teiri.step(game, object);
            }

            if (input.y < -0.25) {
                if (object.direction !== "back") {
                    game.sprites[object.id].textures = [
                        Asset.TEXTURES["hijack/pixelart/teiri/walk/back/0.png"],
                        Asset.TEXTURES["hijack/pixelart/teiri/walk/back/1.png"],
                        Asset.TEXTURES["hijack/pixelart/teiri/walk/back/2.png"],
                        Asset.TEXTURES["hijack/pixelart/teiri/walk/back/3.png"]
                    ];
                    game.sprites[object.id].play()
                    object.direction = "back";
                    object.count = 0;
                }
            } else if (input.y > 0.25) {
                if (object.direction !== "front") {
                    game.sprites[object.id].textures = [
                        Asset.TEXTURES["hijack/pixelart/teiri/walk/front/0.png"],
                        Asset.TEXTURES["hijack/pixelart/teiri/walk/front/1.png"],
                        Asset.TEXTURES["hijack/pixelart/teiri/walk/front/2.png"],
                        Asset.TEXTURES["hijack/pixelart/teiri/walk/front/3.png"]
                    ];
                    game.sprites[object.id].play()
                    object.direction = "front";
                    object.count = 0;
                }
            } else if (input.x < -0.25) {
                if (object.direction !== "left") {
                    game.sprites[object.id].textures = [
                        Asset.TEXTURES["hijack/pixelart/teiri/walk/left/0.png"],
                        Asset.TEXTURES["hijack/pixelart/teiri/walk/left/1.png"],
                        Asset.TEXTURES["hijack/pixelart/teiri/walk/left/2.png"],
                        Asset.TEXTURES["hijack/pixelart/teiri/walk/left/3.png"]
                    ];
                    game.sprites[object.id].play()
                    object.direction = "left";
                    object.count = 0;
                }
            } else if (input.x > 0.25) {
                if (object.direction !== "right") {
                    game.sprites[object.id].textures = [
                        Asset.TEXTURES["hijack/pixelart/teiri/walk/right/0.png"],
                        Asset.TEXTURES["hijack/pixelart/teiri/walk/right/1.png"],
                        Asset.TEXTURES["hijack/pixelart/teiri/walk/right/2.png"],
                        Asset.TEXTURES["hijack/pixelart/teiri/walk/right/3.png"]
                    ];
                    game.sprites[object.id].play()
                    object.direction = "right";
                    object.count = 0;
                }
            }

            if (object.count % 1 === 0) {
                if (input.x < -0.25)
                    object.x -= 1;
                if (input.x > 0.25)
                    object.x += 1;
                if (input.y < -0.25)
                    object.y -= 1;
                if (input.y > 0.25)
                    object.y += 1;
                game.sprites[object.id].x = object.x;
                game.sprites[object.id].y = object.y;
            }

            if (input.x < -0.25 || input.x > 0.25 || input.y < -0.25 || input.y > 0.25)
                ++object.count;

            break;
        case "truncheon":
            if (object.count < 12) {
                ++object.count;
            } else {
                game.sprites[object.id].textures = [
                    Asset.TEXTURES["hijack/pixelart/teiri/walk/" + object.direction + "/0.png"],
                    Asset.TEXTURES["hijack/pixelart/teiri/walk/" + object.direction + "/1.png"],
                    Asset.TEXTURES["hijack/pixelart/teiri/walk/" + object.direction + "/2.png"],
                    Asset.TEXTURES["hijack/pixelart/teiri/walk/" + object.direction + "/3.png"]
                ];
                game.sprites[object.id].animationSpeed = 1/8;
                game.sprites[object.id].play();
                object.pose = "walk";
                object.count = 0;
                Teiri.step(game, object);
            }
        }
    }
};
