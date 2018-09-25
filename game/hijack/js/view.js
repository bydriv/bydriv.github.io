import * as Asset from "./asset.js";
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

export async function update(object, view) {
    switch (view.type) {
    case "teiri":
        return Teiri.update(object, view);
    case "silver":
        return Silver.update(object, view);
    case "gray":
        return Gray.update(object, view);
    default:
        console.log("undefined object type: %o", view.type);
    }
}

export async function setup(container, view) {
    switch (view.type) {
    case "teiri":
        return Teiri.setup(container, view);
    case "silver":
        return Silver.setup(container, view);
    case "gray":
        return Gray.setup(container, view);
    default:
        console.log("undefined object type: %o", view.type);
    }
}

export async function teardown(container, view) {
    switch (view.type) {
    case "teiri":
        return Teiri.teardown(container, view);
    case "silver":
        return Silver.teardown(container, view);
    case "gray":
        return Gray.teardown(container, view);
    default:
        console.log("undefined object type: %o", view.type);
    }
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
        case "walk":
            if (object.count % 8 === 0)
                view.sprite.texture = Asset.TEXTURES.get("hijack/pixelart/teiri/walk/" + object.direction + "/" + object.count / 8 % 4 + ".png");

            view.sprite.x = object.x;
            view.sprite.y = object.y;
            view.shield.x = object.x;
            view.shield.y = object.y - 2;

            return;
        case "truncheon":
            if (object.count % 3 === 0)
                view.sprite.texture = Asset.TEXTURES.get("hijack/pixelart/teiri/truncheon/" + object.direction + "/" + object.count / 3 % 4 + ".png");

            view.sprite.x = object.x - 8;
            view.sprite.y = object.y - 8;
            view.shield.x = object.x;
            view.shield.y = object.y - 2;

            return;
        default:
            console.log("undefined object pose: %o", object.pose);
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
