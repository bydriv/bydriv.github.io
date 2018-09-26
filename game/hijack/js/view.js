// signature VIEW = sig
//   type view
//   structure Object : OBJECT
//   val create : Object.object -> view promise
//   val update : Object.object * view -> unit promise
//   val setup : PIXI.container * view -> unit promise
//   val teardown : PIXI.container * view -> unit promise
// end

// structure Teiri : VIEW where structure Object = Object.Teiri
// structure Silver : VIEW where structure Object = Object.Silver
// structure Gray : VIEW where structure Object = Object.Gray

import * as Asset from "./asset.js";
import * as Team from "./team.js";

const INSTANCES = new Map();

export async function create(object) {
    if (INSTANCES.has(object.type))
        return INSTANCES.get(object.type).create(object);
    else
        console.error("undefined object type: %o", object.type);
}

export async function update(object, view) {
    if (INSTANCES.has(view.type))
        return INSTANCES.get(view.type).update(object, view);
    else
        console.error("undefined view type: %o", view.type);
}

export async function setup(container, view) {
    if (INSTANCES.has(view.type))
        return INSTANCES.get(view.type).setup(container, view);
    else
        console.error("undefined view type: %o", view.type);
}

export async function teardown(container, view) {
    if (INSTANCES.has(view.type))
        return INSTANCES.get(view.type).teardown(container, view);
    else
        console.error("undefined view type: %o", view.type);
}

export const Teiri = {
    create: async (object) => {
        const sprite = new PIXI.Sprite(Asset.TEXTURES.get("hijack/pixelart/teiri/walk/front/0.png"));
        sprite.x = object.x;
        sprite.y = object.y;

        const shield = new PIXI.Graphics();
        shield.x = object.x;
        shield.y = object.y - 2;
        shield.lineStyle(1, Team.color(object.team).fg).moveTo(0, 0).lineTo(object.shield, 0);
        shield.lineStyle(1, Team.color(object.team).bg).moveTo(object.shield, 0).lineTo(16, 0);

        return {
            type: "teiri",
            sprite: sprite,
            shield: shield
        };
    },
    update: async (object, view) => {
        view.shield.clear();
        view.shield.lineStyle(1, Team.color(object.team).fg).moveTo(0, 0).lineTo(object.shield, 0);
        view.shield.lineStyle(1, Team.color(object.team).bg).moveTo(object.shield, 0).lineTo(16, 0);

        switch (object.pose) {
        case "default":
            if (object.count % 8 === 0)
                view.sprite.texture = Asset.TEXTURES.get("hijack/pixelart/teiri/walk/" + object.direction + "/" + object.count / 8 % 4 + ".png");

            view.sprite.x = object.x;
            view.sprite.y = object.y;
            view.shield.x = object.x;
            view.shield.y = object.y - 2;

            return;
        case "button0":
            if (object.count % 3 === 0)
                view.sprite.texture = Asset.TEXTURES.get("hijack/pixelart/teiri/truncheon/" + object.direction + "/" + object.count / 3 % 4 + ".png");

            view.sprite.x = object.x - 8;
            view.sprite.y = object.y - 8;
            view.shield.x = object.x;
            view.shield.y = object.y - 2;

            return;
        default:
            console.error("undefined object pose: %o", object.pose);
        }
    },
    setup: async (container, view) => {
        container.addChild(view.sprite);
        container.addChild(view.shield);
    },
    teardown: async (container, view) => {
        container.removeChild(view.sprite);
        container.removeChild(view.shield);
    }
};
INSTANCES.set("teiri", Teiri);

export const SecurityDrone = {
    create: async (object) => {
        const sprite = new PIXI.Sprite(Asset.TEXTURES.get("hijack/pixelart/security-drone/front/0.png"));
        sprite.x = object.x;
        sprite.y = object.y;

        const shield = new PIXI.Graphics();
        shield.x = object.x;
        shield.y = object.y - 2;
        shield.lineStyle(1, Team.color(object.team).fg).moveTo(0, 0).lineTo(object.shield * 16, 0);
        shield.lineStyle(1, Team.color(object.team).bg).moveTo(object.shield * 16, 0).lineTo(16, 0);

        return {
            type: "security-drone",
            sprite: sprite,
            shield: shield
        };
    },
    update: async (object, view) => {
        view.shield.clear();
        view.shield.lineStyle(1, Team.color(object.team).fg).moveTo(0, 0).lineTo(object.shield * 16, 0);
        view.shield.lineStyle(1, Team.color(object.team).bg).moveTo(object.shield * 16, 0).lineTo(16, 0);

        switch (object.pose) {
        case "default":
            if (object.count % 1 === 0)
                view.sprite.texture = Asset.TEXTURES.get("hijack/pixelart/security-drone/" + object.direction + "/" + object.count / 1 % 4 + ".png");

            view.sprite.x = object.x;
            view.sprite.y = object.y;
            view.shield.x = object.x;
            view.shield.y = object.y - 2;

            return;
        default:
            console.error("undefined object pose: %o", object.pose);
        }
    },
    setup: async (container, view) => {
        container.addChild(view.sprite);
        container.addChild(view.shield);
    },
    teardown: async (container, view) => {
        container.removeChild(view.sprite);
        container.removeChild(view.shield);
    }
};
INSTANCES.set("security-drone", SecurityDrone);

export const Silver = {
    create: async (object) => {
        const sprite = new PIXI.Sprite(Asset.TEXTURES.get("hijack/pixelart/maptip/silver.png"));
        sprite.x = object.x;
        sprite.y = object.y;
        return {
            type: "silver",
            sprite: sprite
        };
    },
    update: async (object, view) => {},
    setup: async (container, view) => {
        container.addChild(view.sprite);
    },
    teardown: async (container, view) => {
        container.removeChild(view.sprite);
    }
};
INSTANCES.set("silver", Silver);

export const Gray = {
    create: async (object) => {
        const sprite = new PIXI.Sprite(Asset.TEXTURES.get("hijack/pixelart/maptip/gray.png"));
        sprite.x = object.x;
        sprite.y = object.y;
        return {
            type: "gray",
            sprite: sprite
        };
    },
    update: async (object, view) => {},
    setup: async (container, view) => {
        container.addChild(view.sprite);
    },
    teardown: async (container, view) => {
        container.removeChild(view.sprite);
    }
};
INSTANCES.set("gray", Gray);
