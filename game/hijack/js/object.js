import * as Asset from "./asset.js";
import * as Control from "./control.js";

export async function create(object) {
    switch (object.type) {
    case "teiri":
        return Teiri.create(object);
    case "silver":
        return Silver.create(object);
    case "gray":
        return Gray.create(object);
    default:
        console.log("undefined object type: %o", object.type);
    }
}

export async function setup(app, object) {
    switch (object.type) {
    case "teiri":
        return Teiri.setup(app, object);
    case "silver":
        return Silver.setup(app, object);
    case "gray":
        return Gray.setup(app, object);
    default:
        console.log("undefined object type: %o", object.type);
    }
}

export async function step(game, object) {
    switch (object.type) {
    case "teiri":
        return Teiri.step(game, object);
    case "silver":
        return Silver.step(game, object);
    case "gray":
        return Gray.step(game, object);
    default:
        console.log("undefined object type: %o", object.type);
    }
};

export const Teiri = {
    create: async (object) => {
        return {
            type: "teiri",
            id: object.id || Symbol(),
            x: object.x,
            y: object.y,
            width: 16,
            height: 16,
            control: await Control.create(object.control),
            pose: object.pose,
            direction: object.direction,
            count: 0
        };
    },
    setup: async (app, object) => {
        const sprite = new PIXI.extras.AnimatedSprite([
            Asset.TEXTURES["hijack/pixelart/teiri/walk/front/0.png"],
            Asset.TEXTURES["hijack/pixelart/teiri/walk/front/1.png"],
            Asset.TEXTURES["hijack/pixelart/teiri/walk/front/2.png"],
            Asset.TEXTURES["hijack/pixelart/teiri/walk/front/3.png"]
        ]);
        sprite.x = object.x;
        sprite.y = object.y;
        sprite.animationSpeed = 1/8;
        sprite.play();
        app.stage.addChild(sprite);

        return sprite;
    },
    step: async (game, object) => {
        object.control = await Control.step(game, object.control);

        switch (object.pose) {
        case "walk":
            const input = object.control.input;

            if (input.buttons[0]) {
                game.states[object.id].textures = [
                    Asset.TEXTURES["hijack/pixelart/teiri/truncheon/" + object.direction + "/0.png"],
                    Asset.TEXTURES["hijack/pixelart/teiri/truncheon/" + object.direction + "/1.png"],
                    Asset.TEXTURES["hijack/pixelart/teiri/truncheon/" + object.direction + "/2.png"],
                    Asset.TEXTURES["hijack/pixelart/teiri/truncheon/" + object.direction + "/3.png"]
                ];
                game.states[object.id].x = object.x - 8;
                game.states[object.id].y = object.y - 8;
                game.states[object.id].animationSpeed = 1/3;
                game.states[object.id].play()
                object.pose = "truncheon";
                object.count = 0;
                return Teiri.step(game, object);
            }

            if (input.y < -0.25) {
                if (object.direction !== "back") {
                    game.states[object.id].textures = [
                        Asset.TEXTURES["hijack/pixelart/teiri/walk/back/0.png"],
                        Asset.TEXTURES["hijack/pixelart/teiri/walk/back/1.png"],
                        Asset.TEXTURES["hijack/pixelart/teiri/walk/back/2.png"],
                        Asset.TEXTURES["hijack/pixelart/teiri/walk/back/3.png"]
                    ];
                    game.states[object.id].play()
                    object.direction = "back";
                    object.count = 0;
                }
            } else if (input.y > 0.25) {
                if (object.direction !== "front") {
                    game.states[object.id].textures = [
                        Asset.TEXTURES["hijack/pixelart/teiri/walk/front/0.png"],
                        Asset.TEXTURES["hijack/pixelart/teiri/walk/front/1.png"],
                        Asset.TEXTURES["hijack/pixelart/teiri/walk/front/2.png"],
                        Asset.TEXTURES["hijack/pixelart/teiri/walk/front/3.png"]
                    ];
                    game.states[object.id].play()
                    object.direction = "front";
                    object.count = 0;
                }
            } else if (input.x < -0.25) {
                if (object.direction !== "left") {
                    game.states[object.id].textures = [
                        Asset.TEXTURES["hijack/pixelart/teiri/walk/left/0.png"],
                        Asset.TEXTURES["hijack/pixelart/teiri/walk/left/1.png"],
                        Asset.TEXTURES["hijack/pixelart/teiri/walk/left/2.png"],
                        Asset.TEXTURES["hijack/pixelart/teiri/walk/left/3.png"]
                    ];
                    game.states[object.id].play()
                    object.direction = "left";
                    object.count = 0;
                }
            } else if (input.x > 0.25) {
                if (object.direction !== "right") {
                    game.states[object.id].textures = [
                        Asset.TEXTURES["hijack/pixelart/teiri/walk/right/0.png"],
                        Asset.TEXTURES["hijack/pixelart/teiri/walk/right/1.png"],
                        Asset.TEXTURES["hijack/pixelart/teiri/walk/right/2.png"],
                        Asset.TEXTURES["hijack/pixelart/teiri/walk/right/3.png"]
                    ];
                    game.states[object.id].play()
                    object.direction = "right";
                    object.count = 0;
                }
            }

            if (object.count % 1 === 0) {
                if (input.x < -0.25)
                    moveLeft(game, object, 1);
                if (input.x > 0.25)
                    moveRight(game, object, 1);
                if (input.y < -0.25)
                    moveUp(game, object, 1);
                if (input.y > 0.25)
                    moveDown(game, object, 1);
                game.states[object.id].x = object.x;
                game.states[object.id].y = object.y;
            }

            if (input.x < -0.25 || input.x > 0.25 || input.y < -0.25 || input.y > 0.25)
                ++object.count;

            break;
        case "truncheon":
            if (object.count < 12) {
                ++object.count;
            } else {
                game.states[object.id].textures = [
                    Asset.TEXTURES["hijack/pixelart/teiri/walk/" + object.direction + "/0.png"],
                    Asset.TEXTURES["hijack/pixelart/teiri/walk/" + object.direction + "/1.png"],
                    Asset.TEXTURES["hijack/pixelart/teiri/walk/" + object.direction + "/2.png"],
                    Asset.TEXTURES["hijack/pixelart/teiri/walk/" + object.direction + "/3.png"]
                ];
                game.states[object.id].animationSpeed = 1/8;
                game.states[object.id].play();
                object.pose = "walk";
                object.count = 0;
                Teiri.step(game, object);
            }
        }
    }
};

export const Silver = {
    create: async (object) => {
        return {
            type: "silver",
            id: object.id || Symbol(),
            x: object.x,
            y: object.y,
            width: 16,
            height: 16
        };
    },
    setup: async (app, object) => {
        const sprite = new PIXI.Sprite(Asset.TEXTURES["hijack/pixelart/maptip/silver.png"]);
        sprite.x = object.x;
        sprite.y = object.y;
        app.stage.addChild(sprite);
        return sprite;
    },
    step: async (game, object) => {}
};

export const Gray = {
    create: async (object) => {
        return {
            type: "gray",
            id: object.id || Symbol(),
            x: object.x,
            y: object.y,
            width: 16,
            height: 16
        };
    },
    setup: async (app, object) => {
        const sprite = new PIXI.Sprite(Asset.TEXTURES["hijack/pixelart/maptip/gray.png"]);
        sprite.x = object.x;
        sprite.y = object.y;
        app.stage.addChild(sprite);
        return sprite;
    },
    step: async (game, object) => {}
};

function collision(object1, object2) {
    const left = Math.max(object1.x, object2.x);
    const top = Math.max(object1.y, object2.y);
    const right = Math.min(object1.x + object1.width, object2.x + object2.height);
    const bottom = Math.min(object1.y + object1.height, object2.y + object2.height);
    const width = right - left;
    const height = bottom - top;
    return width > 0 && height > 0;
}

function moveLeft(game, object, n) {
    for (var i = 0; i < n; ++i) {
        object.x -= 1;

        for (var j = 0; j < game.objects.length; ++j)
            if (object.id !== game.objects[j].id && collision(object, game.objects[j])) {
                object.x += 1;
                return;
            }
    }
}

function moveRight(game, object, n) {
    for (var i = 0; i < n; ++i) {
        object.x += 1;

        for (var j = 0; j < game.objects.length; ++j)
            if (object.id !== game.objects[j].id && collision(object, game.objects[j])) {
                object.x -= 1;
                return;
            }
    }
}

function moveUp(game, object, n) {
    for (var i = 0; i < n; ++i) {
        object.y -= 1;

        for (var j = 0; j < game.objects.length; ++j)
            if (object.id !== game.objects[j].id && collision(object, game.objects[j])) {
                object.y += 1;
                return;
            }
    }
}

function moveDown(game, object, n) {
    for (var i = 0; i < n; ++i) {
        object.y += 1;

        for (var j = 0; j < game.objects.length; ++j)
            if (object.id !== game.objects[j].id && collision(object, game.objects[j])) {
                object.y -= 1;
                return;
            }
    }
}
