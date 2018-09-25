// signature OBJECT = sig
//   type object_scheme
//   type object
//   val create : object_scheme -> object promise
//   val step : Game.game * object -> object promise
//   val onAttack : Game.game * object * Game.attack -> unit promise
// end

// structure Teiri : OBJECT
// structure Silver : OBJECT
// structure Gray : OBJECT

import * as Control from "./control.js";
import * as View from "./view.js";

const INSTANCES = new Map();

export async function create(object) {
    if (INSTANCES.has(object.type))
        return INSTANCES.get(object.type).create(object);
    else
        console.error("undefined object type: %o", object.type);
}

export async function step(game, object) {
    if (INSTANCES.has(object.type))
        return INSTANCES.get(object.type).step(game, object);
    else
        console.error("undefined object type: %o", object.type);
};

export async function onAttack(game, object, attack) {
    if (INSTANCES.has(object.type))
        return INSTANCES.get(object.type).onAttack(game, object, attack);
    else
        console.error("undefined object type: %o", object.type);
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
    step: async (game, object) => {
        object.control = await Control.step(game, object, object.control);

        switch (object.pose) {
        case "walk":
            const input = object.control.input;

            if (button(object, "truncheon", 0)) {
                object.count = 0;
                return object;
            }

            if (isTurned(turn(object))) {
                object.count = 0;
                return object;
            }

            move(game, object, 1, 1);

            ++object.count;

            return object;
        case "truncheon":
            if (!object.attack) {
                switch (object.direction) {
                case "left":
                    object.attack = { id: Symbol(), x: object.x - 8, y: object.y, width: 8, height: 16, damage: 1 };
                    break;
                case "back":
                    object.attack = { id: Symbol(), x: object.x, y: object.y - 8, width: 16, height: 8, damage: 1 };
                    break;
                case "right":
                    object.attack = { id: Symbol(), x: object.x + 16, y: object.y, width: 8, height: 16, damage: 1 };
                    break;
                case "front":
                    object.attack = { id: Symbol(), x: object.x, y: object.y + 16, width: 16, height: 8, damage: 1 };
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
                object.pose = "walk";
                object.count = 0;
                object.attack = null;
                return object;
            }
        }
    },
    onAttack: async (game, object, attack) => {
        object.shield -= Math.min(attack.damage, object.shield);

        if (object.shield === 0) {
            await View.teardown(game.app.stage, game.views.get(object.id));
            game.objects = game.objects.filter(o => o.id !== object.id);
            game.views.delete(object.id);
            game.hits.delete(object.id);
        }
    }
};
INSTANCES.set("teiri", Teiri);

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
    step: async (game, object) => object,
    onAttack: async (game, object, attack) => {}
};
INSTANCES.set("silver", Silver);

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
    step: async (game, object) => object,
    onAttack: async (game, object, attack) => {}
};
INSTANCES.set("gray", Gray);

export function collision(object1, object2) {
    const left = Math.max(object1.x, object2.x);
    const top = Math.max(object1.y, object2.y);
    const right = Math.min(object1.x + object1.width, object2.x + object2.width);
    const bottom = Math.min(object1.y + object1.height, object2.y + object2.height);
    const width = right - left;
    const height = bottom - top;
    return width > 0 && height > 0;
}

function turn(object) {
    const input = object.control.input;

    if (input.y < -0.25) {
        const direction = object.direction;

        if (direction !== "back") {
            object.direction = "back";
            return [direction, "back"];
        }
    } else if (input.y > 0.25) {
        const direction = object.direction;

        if (direction !== "front") {
            object.direction = "front";
            return [direction, "front"];
        }
    } else if (input.x < -0.25) {
        const direction = object.direction;

        if (direction !== "left") {
            object.direction = "left";
            return [direction, "left"];
        }
    } else if (input.x > 0.25) {
        const direction = object.direction;

        if (object.direction !== "right") {
            object.direction = "right";
            return [direction, "right"];
        }
    }

    return [object.direction, object.direction];
}

function isTurned(pair) {
    return pair[0] !== pair[1];
}

function move(game, object, pixels, perFrames) {
    const input = object.control.input;

    if (object.count % perFrames === 0) {
        if (input.x < -0.25)
            return moveLeft(game, object, pixels);
        if (input.x > 0.25)
            return moveRight(game, object, pixels);
        if (input.y < -0.25)
            return moveUp(game, object, pixels);
        if (input.y > 0.25)
            return moveDown(game, object, pixels);
    }

    return 0;
}

function moveLeft(game, object, n) {
    for (var i = 0; i < n; ++i) {
        object.x -= 1;

        for (var j = 0; j < game.objects.length; ++j)
            if (object.id !== game.objects[j].id && collision(object, game.objects[j])) {
                object.x += 1;
                return i;
            }
    }

    return n;
}

function moveRight(game, object, n) {
    for (var i = 0; i < n; ++i) {
        object.x += 1;

        for (var j = 0; j < game.objects.length; ++j)
            if (object.id !== game.objects[j].id && collision(object, game.objects[j])) {
                object.x -= 1;
                return i;
            }
    }

    return n;
}

function moveUp(game, object, n) {
    for (var i = 0; i < n; ++i) {
        object.y -= 1;

        for (var j = 0; j < game.objects.length; ++j)
            if (object.id !== game.objects[j].id && collision(object, game.objects[j])) {
                object.y += 1;
                return i;
            }
    }

    return n;
}

function moveDown(game, object, n) {
    for (var i = 0; i < n; ++i) {
        object.y += 1;

        for (var j = 0; j < game.objects.length; ++j)
            if (object.id !== game.objects[j].id && collision(object, game.objects[j])) {
                object.y -= 1;
                return i;
            }
    }

    return n;
}

function button(object, pose, n) {
    if (object.control.input.buttons[n])
        object.pose = pose;

    return object.control.input.buttons[n];
}
