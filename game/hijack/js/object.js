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
        console.log("undefined object type: %o", object.type);
}

export async function step(game, object) {
    if (INSTANCES.has(object.type))
        return INSTANCES.get(object.type).step(game, object);
    else
        console.log("undefined object type: %o", object.type);
};

export async function onAttack(game, object, attack) {
    if (INSTANCES.has(object.type))
        return INSTANCES.get(object.type).onAttack(game, object, attack);
    else
        console.log("undefined object type: %o", object.type);
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

            if (input.buttons[0]) {
                object.pose = "truncheon";
                object.count = 0;
                return object;
            }

            if (input.y < -0.25) {
                if (object.direction !== "back") {
                    object.direction = "back";
                    object.count = 0;
                    return object;
                }
            } else if (input.y > 0.25) {
                if (object.direction !== "front") {
                    object.direction = "front";
                    object.count = 0;
                    return object;
                }
            } else if (input.x < -0.25) {
                if (object.direction !== "left") {
                    object.direction = "left";
                    object.count = 0;
                    return object;
                }
            } else if (input.x > 0.25) {
                if (object.direction !== "right") {
                    object.direction = "right";
                    object.count = 0;
                    return object;
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
            }

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
