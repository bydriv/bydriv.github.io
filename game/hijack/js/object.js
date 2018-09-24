import * as Asset from "./asset.js";
import * as Control from "./control.js";
import * as Team from "./team.js";

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

export async function teardown(app, object, state) {
    switch (object.type) {
    case "teiri":
        return Teiri.teardown(app, object, state);
    case "silver":
        return Silver.teardown(app, object, state);
    case "gray":
        return Gray.teardown(app, object, state);
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

export async function onAttack(game, object, attack) {
    switch (object.type) {
    case "teiri":
        return Teiri.onAttack(game, object, attack);
    case "silver":
        return Silver.onAttack(game, object, attack);
    case "gray":
        return Gray.onAttack(game, object, attack);
    default:
        console.log("undefined object type: %o", object.type);
    }
}

export const Teiri = {
    create: async (object) => {
        return {
            type: "teiri",
            id: object.id || Symbol(),
            x: object.x,
            y: object.y,
            width: 16,
            height: 16,
            team: object.team,
            control: await Control.create(object, object.control),
            pose: object.pose,
            direction: object.direction,
            shield: 16,
            count: 0,
            attack: null
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

        const shield = new PIXI.Graphics();
        shield.x = object.x;
        shield.y = object.y - 2;
        shield.lineStyle(1, Team.color(object.team).fg).moveTo(0, 0).lineTo(object.shield, 0);
        shield.lineStyle(1, Team.color(object.team).bg).moveTo(object.shield, 0).lineTo(16, 0);
        app.stage.addChild(shield);

        return {
            sprite: sprite,
            shield: shield
        };
    },
    teardown: async (app, object, state) => {
        app.stage.removeChild(state.sprite);
        app.stage.removeChild(state.shield);
    },
    step: async (game, object) => {
        object.control = await Control.step(game, object, object.control);

        switch (object.pose) {
        case "walk":
            const input = object.control.input;

            if (input.buttons[0]) {
                game.states[object.id].sprite.textures = [
                    Asset.TEXTURES["hijack/pixelart/teiri/truncheon/" + object.direction + "/0.png"],
                    Asset.TEXTURES["hijack/pixelart/teiri/truncheon/" + object.direction + "/1.png"],
                    Asset.TEXTURES["hijack/pixelart/teiri/truncheon/" + object.direction + "/2.png"],
                    Asset.TEXTURES["hijack/pixelart/teiri/truncheon/" + object.direction + "/3.png"]
                ];
                game.states[object.id].sprite.x = object.x - 8;
                game.states[object.id].sprite.y = object.y - 8;
                game.states[object.id].sprite.animationSpeed = 1/3;
                game.states[object.id].sprite.play()
                game.states[object.id].shield.x = object.x;
                game.states[object.id].shield.y = object.y - 2;
                object.pose = "truncheon";
                object.count = 0;
                return Teiri.step(game, object);
            }

            if (input.y < -0.25) {
                if (object.direction !== "back") {
                    game.states[object.id].sprite.textures = [
                        Asset.TEXTURES["hijack/pixelart/teiri/walk/back/0.png"],
                        Asset.TEXTURES["hijack/pixelart/teiri/walk/back/1.png"],
                        Asset.TEXTURES["hijack/pixelart/teiri/walk/back/2.png"],
                        Asset.TEXTURES["hijack/pixelart/teiri/walk/back/3.png"]
                    ];
                    game.states[object.id].sprite.play()
                    object.direction = "back";
                    object.count = 0;
                }
            } else if (input.y > 0.25) {
                if (object.direction !== "front") {
                    game.states[object.id].sprite.textures = [
                        Asset.TEXTURES["hijack/pixelart/teiri/walk/front/0.png"],
                        Asset.TEXTURES["hijack/pixelart/teiri/walk/front/1.png"],
                        Asset.TEXTURES["hijack/pixelart/teiri/walk/front/2.png"],
                        Asset.TEXTURES["hijack/pixelart/teiri/walk/front/3.png"]
                    ];
                    game.states[object.id].sprite.play()
                    object.direction = "front";
                    object.count = 0;
                }
            } else if (input.x < -0.25) {
                if (object.direction !== "left") {
                    game.states[object.id].sprite.textures = [
                        Asset.TEXTURES["hijack/pixelart/teiri/walk/left/0.png"],
                        Asset.TEXTURES["hijack/pixelart/teiri/walk/left/1.png"],
                        Asset.TEXTURES["hijack/pixelart/teiri/walk/left/2.png"],
                        Asset.TEXTURES["hijack/pixelart/teiri/walk/left/3.png"]
                    ];
                    game.states[object.id].sprite.play()
                    object.direction = "left";
                    object.count = 0;
                }
            } else if (input.x > 0.25) {
                if (object.direction !== "right") {
                    game.states[object.id].sprite.textures = [
                        Asset.TEXTURES["hijack/pixelart/teiri/walk/right/0.png"],
                        Asset.TEXTURES["hijack/pixelart/teiri/walk/right/1.png"],
                        Asset.TEXTURES["hijack/pixelart/teiri/walk/right/2.png"],
                        Asset.TEXTURES["hijack/pixelart/teiri/walk/right/3.png"]
                    ];
                    game.states[object.id].sprite.play()
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
                game.states[object.id].sprite.x = object.x;
                game.states[object.id].sprite.y = object.y;
                game.states[object.id].shield.x = object.x;
                game.states[object.id].shield.y = object.y - 2;
            }

            if (input.x < -0.25 || input.x > 0.25 || input.y < -0.25 || input.y > 0.25)
                ++object.count;

            return object;
        case "truncheon":
            if (!object.attack) {
                switch (object.direction) {
                case "left":
                    object.attack = { id: Symbol(), x: object.x - 8, y: object.y, width: 8, height: 16 };
                    break;
                case "back":
                    object.attack = { id: Symbol(), x: object.x, y: object.y - 8, width: 16, height: 8 };
                    break;
                case "right":
                    object.attack = { id: Symbol(), x: object.x + 16, y: object.y, width: 8, height: 16 };
                    break;
                case "front":
                    object.attack = { id: Symbol(), x: object.x, y: object.y + 16, width: 16, height: 8 };
                    break;
                }
            }

            if (object.count < 6) {
                ++object.count;
                return object;
            } else if (object.count < 12) {
                game.attacks.push(object.attack);
                ++object.count;
                return object;
            } else {
                game.states[object.id].sprite.textures = [
                    Asset.TEXTURES["hijack/pixelart/teiri/walk/" + object.direction + "/0.png"],
                    Asset.TEXTURES["hijack/pixelart/teiri/walk/" + object.direction + "/1.png"],
                    Asset.TEXTURES["hijack/pixelart/teiri/walk/" + object.direction + "/2.png"],
                    Asset.TEXTURES["hijack/pixelart/teiri/walk/" + object.direction + "/3.png"]
                ];
                game.states[object.id].sprite.animationSpeed = 1/8;
                game.states[object.id].sprite.play();
                object.pose = "walk";
                object.count = 0;
                object.attack = null;
                return Teiri.step(game, object);
            }
        }
    },
    onAttack: async (game, object, attack) => {
        if (object.shield > 0) {
            object.shield -= 1;
            game.states[object.id].shield.clear();
            game.states[object.id].shield.lineStyle(1, Team.color(object.team).fg).moveTo(0, 0).lineTo(object.shield, 0);
            game.states[object.id].shield.lineStyle(1, Team.color(object.team).bg).moveTo(object.shield, 0).lineTo(16, 0);
        } else {
            await Teiri.teardown(game.app, object, game.states[object.id]);
            game.objects = game.objects.filter(o => o.id !== object.id);
            delete game.states[object.id];
            delete game.hits[object.id];
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
            height: 16,
            team: object.team
        };
    },
    setup: async (app, object) => {
        const sprite = new PIXI.Sprite(Asset.TEXTURES["hijack/pixelart/maptip/silver.png"]);
        sprite.x = object.x;
        sprite.y = object.y;
        app.stage.addChild(sprite);
        return sprite;
    },
    teardown: async (game, object) => {},
    step: async (game, object) => object,
    onAttack: async (game, object, attack) => {}
};

export const Gray = {
    create: async (object) => {
        return {
            type: "gray",
            id: object.id || Symbol(),
            x: object.x,
            y: object.y,
            width: 16,
            height: 16,
            team: object.team
        };
    },
    setup: async (app, object) => {
        const sprite = new PIXI.Sprite(Asset.TEXTURES["hijack/pixelart/maptip/gray.png"]);
        sprite.x = object.x;
        sprite.y = object.y;
        app.stage.addChild(sprite);
        return sprite;
    },
    teardown: async (game, object) => {},
    step: async (game, object) => object,
    onAttack: async (game, object, attack) => {}
};

export function collision(object1, object2) {
    const left = Math.max(object1.x, object2.x);
    const top = Math.max(object1.y, object2.y);
    const right = Math.min(object1.x + object1.width, object2.x + object2.width);
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
