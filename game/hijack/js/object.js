// signature OBJECT = sig
//   type object_scheme
//   type object
//   val create : object_scheme -> object promise
//   val step : Game.game * object -> object promise
//   val onAttack : Game.game * object * Game.attack -> unit promise
// end

// structure Teiri : OBJECT

import * as Control from "./control.js";
import * as Gensym from "./gensym.js";
import * as Team from "./team.js";
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

export async function onHijack(game, object, hijack) {
    if (INSTANCES.has(object.type))
        return INSTANCES.get(object.type).onHijack(game, object, hijack);
    else
        console.error("undefined object type: %o", object.type);
}
;

export async function onAttack(game, object, attack) {
    if (INSTANCES.has(object.type))
        return INSTANCES.get(object.type).onAttack(game, object, attack);
    else
        console.error("undefined object type: %o", object.type);
};

export const Creature = {
    create: async (object) => {
        return {
            type: object.type,
            id: object.id || "0x" + Gensym.gensym().toString(16).toUpperCase(),
            x: object.x,
            y: object.y,
            width: object.width,
            height: object.height,
            team: object.team,
            control: await Control.create(object, object.control),
            pose: object.pose,
            direction: object.direction,
            shield: object.shield,
            count: 0,
            hijack: null,
            attack: null,
            sight: object.sight,
            buttons: object.buttons,
            move: object.move,
            hijackable: object.hijackable,
            security: object.security,
            acceptedIds: [],
            rejectedIds: []
        };
    },
    step: async (game, object) => {
        object.control = await Control.step(game, object, object.control);

        switch (object.pose) {
        case "default":
            const input = object.control.input;

            if (object.count >= 8 && object.buttons[0] && button(object, "button0", 0)) {
                object.count = 0;
                return object;
            }

            if (object.count >= 8 && object.buttons[1] && button(object, "button1", 1)) {
                object.count = 0;
                return object;
            }

            if (object.count >= 8 && object.buttons[2] && button(object, "button2", 2)) {
                object.count = 0;
                return object;
            }

            if (object.count >= 8 && object.buttons[3] && button(object, "button3", 3)) {
                object.count = 0;
                return object;
            }

            if (isTurned(turn(object))) {
                object.count = 0;
                return object;
            }

            move(game, object, object.move.pixels, object.move.perFrames);

            ++object.count;
            return object;
        default:
            switch (object.pose) {
            case "button0":
            case "button1":
            case "button2":
            case "button3":
                const i = object.pose === "button0" ? 0 : object.pose === "button1" ? 1 : object.pose === "button2" ? 2 : 3;

                switch (object.buttons[i].type) {
                case "hijack":
                    if (hijack(game, object)) {
                        object.pose = "default";
                        object.count = 0;
                        return object;
                    }
                    break;
                case "attack":
                    if (attack(game, object, object.buttons[i].startup, object.buttons[i].active, object.buttons[i].recovery, object.buttons[i].left, object.buttons[i].back, object.buttons[i].right, object.buttons[i].front)) {
                        object.pose = "default";
                        object.count = 0;
                        return object;
                    }
                    break;
                case "shot":
                    if (await shot(game, object, object.buttons[i].startup, object.buttons[i].recovery, object.buttons[i].offset[object.direction])) {
                        object.pose = "default";
                        object.count = 0;
                        return object;
                    }
                    break;
                default:
                    console.error("undefined object button type: %o", object.buttons[i].type);
                }

                ++object.count;
                return object;
            default:
                console.error("undefined object pose type: %o", object.pose);
            }
        }
    },
    onHijack: async (game, object, hijack) => {
        if (!object.hijackable)
            return;

        const sourceObject = game.objects.find(o => o.id === hijack.source);
        if (!sourceObject || !Team.enemy(sourceObject, object))
            return;

        object.security -= Math.min(hijack.damage, object.security);

        if (object.security === 0)
            if (!object.acceptedIds.some(id => id === hijack.source))
                object.acceptedIds.push(hijack.source);
    },
    onAttack: async (game, object, attack) => {
        const sourceObject = game.objects.find(o => o.id === attack.source);
        if (!sourceObject || !Team.enemy(sourceObject, object))
            return;

        object.shield -= Math.min(attack.damage, object.shield);

        if (!object.rejectedIds.some(id => id === attack.source))
            object.rejectedIds.push(attack.source);

        if (object.shield === 0)
            object.exiled = true;
    }
};
INSTANCES.set("creature", Creature);

export const Teiri = {
    create: async (object) => {
        return Creature.create({
            type: object.type,
            id: object.id,
            x: object.x,
            y: object.y,
            width: 16,
            height: 16,
            team: object.team,
            control: object.control,
            pose: object.pose,
            direction: object.direction,
            shield: 16,
            sight: {
                left: { x: -32, y: -16, width: 48, height: 48 },
                back: { x: -16, y: -32, width: 48, height: 48 },
                right: { x: 0, y: -16, width: 48, height: 48 },
                front: { x: -16, y: 0, width: 48, height: 48 }
            },
            buttons: [
                {
                    type: "attack",
                    startup: 6,
                    active: 6,
                    recovery: 0,
                    left: { x: -8, y: 0, width: 8, height: 16, damage: 1 },
                    back: { x: 0, y: -8, width: 16, height: 8, damage: 1 },
                    right: { x: 16, y: 0, width: 8, height: 16, damage: 1 },
                    front: { x: 0, y: 16, width: 16, height: 8, damage: 1 }
                },
                {
                    type: "hijack"
                },
                null,
                null
            ],
            move: {
                pixels: 1,
                perFrames: 1
            },
            hijackable: false,
            security: 0
        });
    },
    step: async (game, object) => Creature.step(game, object),
    onHijack: async (game, object, hijack) => Creature.onHijack(game, object, hijack),
    onAttack: async (game, object, attack) => Creature.onAttack(game, object, attack)
};
INSTANCES.set("teiri", Teiri);

export const SecurityDrone = {
    create: async (object) => {
        return Creature.create({
            type: object.type,
            id: object.id,
            x: object.x,
            y: object.y,
            width: 16,
            height: 16,
            team: object.team,
            control: object.control,
            pose: object.pose,
            direction: object.direction,
            shield: 1,
            sight: {
                left: { x: -32, y: -16, width: 48, height: 48 },
                back: { x: -16, y: -32, width: 48, height: 48 },
                right: { x: 0, y: -16, width: 48, height: 48 },
                front: { x: -16, y: 0, width: 48, height: 48 }
            },
            buttons: [
                {
                    type: "shot",
                    startup: 10,
                    recovery: 10,
                    left: { x: -240, y: 8, width: 240, height: 2 },
                    back: { x: 7, y: -240, width: 2, height: 240 },
                    right: { x: 16, y: 8, width: 240, height: 2 },
                    front: { x: 7, y: 16, width: 2, height: 240 },
                    offset: {
                        left: { x: -4, y: 8 },
                        back: { x: 7, y: -4 },
                        right: { x: 16, y: 8 },
                        front: { x: 7, y: 16 }
                    }
                },
                null,
                null,
                null
            ],
            move: {
                pixels: 2,
                perFrames: 1
            },
            hijackable: true,
            security: 60
        });
    },
    step: async (game, object) => Creature.step(game, object),
    onHijack: async (game, object, hijack) => Creature.onHijack(game, object, hijack),
    onAttack: async (game, object, attack) => Creature.onAttack(game, object, attack)
};
INSTANCES.set("security-drone", SecurityDrone);

export const Permanent = {
    create: async (object) => {
        return {
            type: object.type,
            id: object.id || "0x" + Gensym.gensym().toString(16).toUpperCase(),
            x: object.x,
            y: object.y,
            width: object.width,
            height: object.height,
            team: object.team,
            count: 0
        };
    },
    step: async (game, object) => {
        ++object.count;
        return object;
    },
    onHijack: async (game, object, hijack) => {},
    onAttack: async (game, object, attack) => {}
};
INSTANCES.set("permanent", Permanent);

export const Gray = {
    create: async (object) => {
        return Permanent.create({
            type: object.type,
            id: object.id,
            x: object.x,
            y: object.y,
            width: 16,
            height: 16,
            team: "neutral"
        });
    },
    step: async (game, object) => Permanent.step(game, object),
    onHijack: async (game, object, hijack) => Permanent.onHijack(game, object. hijack),
    onAttack: async (game, object, attack) => Permanent.onAttack(game, object. attack)
};
INSTANCES.set("gray", Gray);

export const GrayDoor = {
    create: async (object) => {
        return Permanent.create({
            type: object.type,
            id: object.id,
            x: object.x,
            y: object.y,
            width: 16,
            height: 16,
            team: "neutral"
        });
    },
    step: async (game, object) => Permanent.step(game, object),
    onHijack: async (game, object, hijack) => Permanent.onHijack(game, object. hijack),
    onAttack: async (game, object, attack) => Permanent.onAttack(game, object. attack)
};
INSTANCES.set("gray-door", GrayDoor);

export const StoneWall = {
    create: async (object) => {
        return Permanent.create({
            type: object.type,
            id: object.id,
            x: object.x,
            y: object.y,
            width: 16,
            height: 16,
            team: "neutral"
        });
    },
    step: async (game, object) => Permanent.step(game, object),
    onHijack: async (game, object, hijack) => Permanent.onHijack(game, object. hijack),
    onAttack: async (game, object, attack) => Permanent.onAttack(game, object. attack)
};
INSTANCES.set("stone-wall", StoneWall);

export const StoneTile = {
    create: async (object) => {
        return Permanent.create({
            type: object.type,
            id: object.id,
            x: object.x,
            y: object.y,
            width: 0,
            height: 0,
            team: "neutral"
        });
    },
    step: async (game, object) => Permanent.step(game, object),
    onHijack: async (game, object, hijack) => Permanent.onHijack(game, object. hijack),
    onAttack: async (game, object, attack) => Permanent.onAttack(game, object. attack)
};
INSTANCES.set("stone-tile", StoneTile);

export const Exit = {
    create: async (object) => {
        return Permanent.create({
            type: object.type,
            id: object.id,
            x: object.x,
            y: object.y,
            width: 16,
            height: 16,
            team: "neutral"
        });
    },
    step: async (game, object) => Permanent.step(game, object),
    onHijack: async (game, object, hijack) => Permanent.onHijack(game, object. hijack),
    onAttack: async (game, object, attack) => Permanent.onAttack(game, object. attack)
};
INSTANCES.set("exit", Exit);

export const Water = {
    create: async (object) => {
        return Permanent.create({
            type: object.type,
            id: object.id,
            x: object.x,
            y: object.y,
            width: 16,
            height: 16,
            team: "neutral"
        });
    },
    step: async (game, object) => Permanent.step(game, object),
    onHijack: async (game, object, hijack) => Permanent.onHijack(game, object. hijack),
    onAttack: async (game, object, attack) => Permanent.onAttack(game, object. attack)
};
INSTANCES.set("water", Water);

export const WaterMid = {
    create: async (object) => {
        return Permanent.create({
            type: object.type,
            id: object.id,
            x: object.x,
            y: object.y,
            width: 16,
            height: 16,
            team: "neutral"
        });
    },
    step: async (game, object) => Permanent.step(game, object),
    onHijack: async (game, object, hijack) => Permanent.onHijack(game, object. hijack),
    onAttack: async (game, object, attack) => Permanent.onAttack(game, object. attack)
};
INSTANCES.set("water-mid", WaterMid);

export const Shot = {
    create: async (object) => {
        return {
            type: object.type,
            id: object.id || "0x" + Gensym.gensym().toString(16).toUpperCase(),
            x: object.x,
            y: object.y,
            width: 0,
            height: 0,
            team: object.team,
            direction: object.direction,
            count: 0,
            lifetime: 120,
            source: object.source
        };
    },
    step: async (game, object) => {
        if (object.count < object.lifetime) {
            switch (object.direction) {
            case "left":
                object.x -= 2;
                break;
            case "back":
                object.y -= 2;
                break;
            case "right":
                object.x += 2;
                break;
            case "front":
                object.y += 2;
                break;
            default:
                console.error("undefined object direction: %o", object.direction);
            }

            game.attacks.push({ id: object.id, x: object.x, y: object.y, width: 2, height: 2, damage: 1, source: object.source });

            ++object.count;
            return object;
        } else {
            object.exiled = true;
            return object;
        }
    },
    onHijack: async (game, object, hijack) => {},
    onAttack: async (game, object, attack) => {}
};
INSTANCES.set("shot", Shot);

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

    var i = 0;
    var j = 0;

    if (object.count % perFrames === 0) {
        if (input.x < -0.25)
            i = moveLeft(game, object, pixels);
        if (input.x > 0.25)
            i = moveRight(game, object, pixels);
        if (input.y < -0.25)
            j = moveUp(game, object, pixels);
        if (input.y > 0.25)
            j = moveDown(game, object, pixels);
    }

    return [i, j];
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

function hijack(game, object) {
    if (object.count >= 8 && object.control.input.buttons[1]) {
        object.hijack = null;
        return true;
    }

    if (!object.hijack) {
        object.hijack = {
            x: (object.x + (object.direction === "left" ? -32 : object.direction === "right" ? 16 : -8)),
            y: (object.y + (object.direction === "back" ? -32 : object.direction === "front" ? 16 : -8)),
            width: 32,
            height: 32,
            damage: 1,
            source: object.id
        };
    }

    const targets = game.objects.filter(o => o.hijackable && Team.enemy(o, object) && collision(o, object.hijack));

    if (targets.length === 0) {
        if (object.count % 2 === 0) {
            if (object.control.input.x < -0.25 && Math.abs(object.x + object.width - (object.hijack.x + object.hijack.width / 2 - 8)) <= 120)
                object.hijack.x -= 8;
            if (object.control.input.x > 0.25 && Math.abs(object.x - (object.hijack.x + object.hijack.width / 2 + 8)) <= 120)
                object.hijack.x += 8;
            if (object.control.input.y < -0.25 && Math.abs(object.y + object.height - (object.hijack.y + object.hijack.height / 2 - 8)) <= 80)
                object.hijack.y -= 8;
            if (object.control.input.y > 0.25 && Math.abs(object.y - (object.hijack.y + object.hijack.height / 2 + 8)) <= 80)
                object.hijack.y += 8;
        }
    } else {
        const left = Math.min.apply(null, targets.map(target => target.x));
        const top = Math.min.apply(null, targets.map(target => target.y));
        const right = Math.max.apply(null, targets.map(target => target.x + target.width));
        const bottom = Math.max.apply(null, targets.map(target => target.y + target.height));
        const width = right - left;
        const height = bottom - top;
        const x = left + width / 2 - object.hijack.width / 2;
        const y = top + height / 2 - object.hijack.height / 2;

        /*if (object.count % 8 === 0 && object.hijack.x === x && object.hijack.y === y) {
            if (object.control.input.x < -0.25 && Math.abs(object.x - (object.hijack.x - 8)) <= 112)
                object.hijack.x += (left - object.hijack.x - 8) & 0xFFFFFFF8;
            if (object.control.input.x > 0.25 && Math.abs(object.x - (object.hijack.x + object.hijack.width + 8)) <= 120)
                object.hijack.x += (right - object.hijack.x + 8) & 0xFFFFFFF8;
            if (object.control.input.y < -0.25 && Math.abs(object.y - (object.hijack.y - 8)) <= 72)
                object.hijack.y += (top - object.hijack.y - 8) & 0xFFFFFFF8;
            if (object.control.input.y > 0.25 && Math.abs(object.y - (object.hijack.y + object.hijack.height + 8)) <= 88)
                object.hijack.y += (bottom - object.hijack.x + 8) & 0xFFFFFFF8;
        } else {
        */
            object.hijack.x = x;
            object.hijack.y = y;
        //}
    }

    const x = (object.hijack.x + object.hijack.width / 2) - (object.x + object.width / 2);
    const y = (object.hijack.y + object.hijack.height / 2) - (object.y + object.height / 2);

    if (y < 0) {
        object.direction = "back";
    } else if (x < 0) {
        object.direction = "left";
    } else if (x > 0) {
        object.direction = "right";
    } else if (y > 0) {
        object.direction = "front";
    }

    game.hijacks.push(object.hijack);

    return false;
}

function attack(game, object, startup, active, recovery, leftAttack, backAttack, rightAttack, frontAttack) {
    if (!object.attack) {
        switch (object.direction) {
        case "left":
            object.attack = { id: "0x" + Gensym.gensym().toString(16).toUpperCase(), x: object.x + leftAttack.x, y: object.y + leftAttack.y, width: leftAttack.width, height: leftAttack.height, damage: leftAttack.damage, source: object.id };
            break;
        case "back":
            object.attack = { id: "0x" + Gensym.gensym().toString(16).toUpperCase(), x: object.x + backAttack.x, y: object.y + backAttack.y, width: backAttack.width, height: backAttack.height, damage: backAttack.damage, source: object.id };
            break;
        case "right":
            object.attack = { id: "0x" + Gensym.gensym().toString(16).toUpperCase(), x: object.x + rightAttack.x, y: object.y + rightAttack.y, width: rightAttack.width, height: rightAttack.height, damage: rightAttack.damage, source: object.id };
            break;
        case "front":
            object.attack = { id: "0x" + Gensym.gensym().toString(16).toUpperCase(), x: object.x + frontAttack.x, y: object.y + frontAttack.y, width: frontAttack.width, height: frontAttack.height, damage: frontAttack.damage, source: object.id };
            break;
        }
    }

    if (object.count < startup) {
        return false;
    } else if (object.count < startup + active) {
        game.attacks.push(object.attack);
        return false;
    } else if (object.count < startup + active + recovery) {
        object.attack = null;
        return false;
    } else {
        object.attack = null;
        return true;
    }
}

async function shot(game, object, startup, recovery, offset) {
    if (object.count < startup) {
        return false;
    } else if (object.count === startup) {
        const shot = await Shot.create({ type: "shot", direction: object.direction, team: object.team, x: object.x + offset.x, y: object.y + offset.y, source: object.id });
        const view = await View.create(shot);
        await View.setup(game.app.stage, view);
        game.objects.push(shot);
        game.views.set(shot.id, view);
        game.hits.set(shot.id, new Map());
        return false;
    } else if (object.count < startup + recovery) {
        return false;
    } else {
        return true;
    }
}
