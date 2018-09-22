import * as Asset from "./asset.js";
import * as Control from "./control.js";

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
    create: async (x, y, control) => {
        const sprite = new PIXI.extras.AnimatedSprite([
            Asset.TEXTURES["hijack/pixelart/teiri/walk/front/0.png"],
            Asset.TEXTURES["hijack/pixelart/teiri/walk/front/1.png"],
            Asset.TEXTURES["hijack/pixelart/teiri/walk/front/2.png"],
            Asset.TEXTURES["hijack/pixelart/teiri/walk/front/3.png"]
        ]);
        sprite.x = x;
        sprite.y = y;
        sprite.animationSpeed = 1/8;

        return {
            type: "teiri",
            control: control,
            sprite: sprite,
            pose: "walk",
            direction: "front",
            count: 0,
            x: x,
            y: y
        };
    },
    step: async (game, object) => {
        switch (object.pose) {
        case "walk":
            const input = object.control.input;

            if (input.buttons[0]) {
                object.sprite.textures = [
                    Asset.TEXTURES["hijack/pixelart/teiri/truncheon/" + object.direction + "/0.png"],
                    Asset.TEXTURES["hijack/pixelart/teiri/truncheon/" + object.direction + "/1.png"],
                    Asset.TEXTURES["hijack/pixelart/teiri/truncheon/" + object.direction + "/2.png"],
                    Asset.TEXTURES["hijack/pixelart/teiri/truncheon/" + object.direction + "/3.png"]
                ];
                object.sprite.x = object.x - 8;
                object.sprite.y = object.y - 8;
                object.sprite.animationSpeed = 1/3;
                object.sprite.play()
                object.pose = "truncheon";
                object.count = 0;
                Teiri.step(game, object);
                break;
            }

            if (input.y < -0.25) {
                if (object.direction !== "back") {
                    object.sprite.textures = [
                        Asset.TEXTURES["hijack/pixelart/teiri/walk/back/0.png"],
                        Asset.TEXTURES["hijack/pixelart/teiri/walk/back/1.png"],
                        Asset.TEXTURES["hijack/pixelart/teiri/walk/back/2.png"],
                        Asset.TEXTURES["hijack/pixelart/teiri/walk/back/3.png"]
                    ];
                    object.sprite.play()
                    object.direction = "back";
                    object.count = 0;
                }
            } else if (input.y > 0.25) {
                if (object.direction !== "front") {
                    object.sprite.textures = [
                        Asset.TEXTURES["hijack/pixelart/teiri/walk/front/0.png"],
                        Asset.TEXTURES["hijack/pixelart/teiri/walk/front/1.png"],
                        Asset.TEXTURES["hijack/pixelart/teiri/walk/front/2.png"],
                        Asset.TEXTURES["hijack/pixelart/teiri/walk/front/3.png"]
                    ];
                    object.sprite.play()
                    object.direction = "front";
                    object.count = 0;
                }
            } else if (input.x < -0.25) {
                if (object.direction !== "left") {
                    object.sprite.textures = [
                        Asset.TEXTURES["hijack/pixelart/teiri/walk/left/0.png"],
                        Asset.TEXTURES["hijack/pixelart/teiri/walk/left/1.png"],
                        Asset.TEXTURES["hijack/pixelart/teiri/walk/left/2.png"],
                        Asset.TEXTURES["hijack/pixelart/teiri/walk/left/3.png"]
                    ];
                    object.sprite.play()
                    object.direction = "left";
                    object.count = 0;
                }
            } else if (input.x > 0.25) {
                if (object.direction !== "right") {
                    object.sprite.textures = [
                        Asset.TEXTURES["hijack/pixelart/teiri/walk/right/0.png"],
                        Asset.TEXTURES["hijack/pixelart/teiri/walk/right/1.png"],
                        Asset.TEXTURES["hijack/pixelart/teiri/walk/right/2.png"],
                        Asset.TEXTURES["hijack/pixelart/teiri/walk/right/3.png"]
                    ];
                    object.sprite.play()
                    object.direction = "right";
                    object.count = 0;
                }
            }

            if (object.count % 8 === 0) {
                if (input.x < -0.25)
                    object.x -= 8;
                if (input.x > 0.25)
                    object.x += 8;
                if (input.y < -0.25)
                    object.y -= 8;
                if (input.y > 0.25)
                    object.y += 8;
                object.sprite.x = object.x;
                object.sprite.y = object.y;
            }

            if (input.x < -0.25 || input.x > 0.25 || input.y < -0.25 || input.y > 0.25)
                ++object.count;

            break;
        case "truncheon":
            if (object.count < 12) {
                ++object.count;
            } else {
                object.sprite.textures = [
                    Asset.TEXTURES["hijack/pixelart/teiri/walk/" + object.direction + "/0.png"],
                    Asset.TEXTURES["hijack/pixelart/teiri/walk/" + object.direction + "/1.png"],
                    Asset.TEXTURES["hijack/pixelart/teiri/walk/" + object.direction + "/2.png"],
                    Asset.TEXTURES["hijack/pixelart/teiri/walk/" + object.direction + "/3.png"]
                ];
                object.sprite.animationSpeed = 1/8;
                object.sprite.play()
                object.pose = "walk";
                object.count = 0;
                Teiri.step(game, object);
            }
        }
    }
};
