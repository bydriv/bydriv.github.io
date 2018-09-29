export const ASSETS = [
    "hijack/pixelart/teiri/walk.png",
    "hijack/pixelart/teiri/truncheon.png",
    "hijack/pixelart/teiri/hijack.png",
    "hijack/pixelart/security-drone.png",
    "hijack/pixelart/common/shot.png",
    "hijack/pixelart/maptip/gray.png",
    "hijack/pixelart/maptip/gray-door.png",
    "hijack/pixelart/maptip/stone-wall.png",
    "hijack/pixelart/maptip/stone-tile.png",
    "hijack/pixelart/maptip/exit.png",
    "hijack/map/test.json"
];

export const MAPS = new Map();
export const TEXTURES = new Map();

export function load() {
    return new Promise(resolve => {
        PIXI.loader.add(ASSETS).load(() => {
            MAPS.set("hijack/map/test.json", PIXI.loader.resources["hijack/map/test.json"].data);
            TEXTURES.set("hijack/pixelart/maptip/gray.png", PIXI.loader.resources["hijack/pixelart/maptip/gray.png"].texture);
            TEXTURES.set("hijack/pixelart/maptip/gray-door.png", PIXI.loader.resources["hijack/pixelart/maptip/gray-door.png"].texture);
            TEXTURES.set("hijack/pixelart/maptip/stone-wall.png", PIXI.loader.resources["hijack/pixelart/maptip/stone-wall.png"].texture);
            TEXTURES.set("hijack/pixelart/maptip/stone-tile.png", PIXI.loader.resources["hijack/pixelart/maptip/stone-tile.png"].texture);
            TEXTURES.set("hijack/pixelart/maptip/exit.png", PIXI.loader.resources["hijack/pixelart/maptip/exit.png"].texture);
            for (var i = 0; i < 4; ++i) {
                const name = i === 0 ? "left" : i === 1 ? "back" : i === 2 ? "right" : "front";

                for (var j = 0; j < 4; ++j) {
                    TEXTURES.set("hijack/pixelart/teiri/walk/" + name + "/" + j + ".png", new PIXI.Texture(PIXI.loader.resources["hijack/pixelart/teiri/walk.png"].texture, new PIXI.Rectangle(j * 16, i * 16, 16, 16)));
                    TEXTURES.set("hijack/pixelart/teiri/truncheon/" + name + "/" + j + ".png", new PIXI.Texture(PIXI.loader.resources["hijack/pixelart/teiri/truncheon.png"].texture, new PIXI.Rectangle(j * 32, i * 32, 32, 32)));
                    TEXTURES.set("hijack/pixelart/teiri/hijack/" + name + "/" + j + ".png", new PIXI.Texture(PIXI.loader.resources["hijack/pixelart/teiri/hijack.png"].texture, new PIXI.Rectangle(j * 32, i * 32, 32, 32)));
                    TEXTURES.set("hijack/pixelart/security-drone/" + name + "/" + j + ".png", new PIXI.Texture(PIXI.loader.resources["hijack/pixelart/security-drone.png"].texture, new PIXI.Rectangle(j * 16, i * 16, 16, 16)));
                }
            }
            TEXTURES.set("hijack/pixelart/common/shot.png", PIXI.loader.resources["hijack/pixelart/common/shot.png"].texture);
            resolve();
        });
    });
}
