export const ASSETS = [
    "hijack/pixelart/teiri/walk.png",
    "hijack/pixelart/teiri/truncheon.png",
    "hijack/pixelart/maptip/silver.png",
    "hijack/pixelart/maptip/gray.png",
    "hijack/map/test.json"
];

export const MAPS = {};
export const TEXTURES = {};

export function load() {
    return new Promise(resolve => {
        PIXI.loader.add(ASSETS).load(() => {
            MAPS["hijack/map/test.json"] = PIXI.loader.resources["hijack/map/test.json"].data;
            TEXTURES["hijack/pixelart/maptip/silver.png"] = PIXI.loader.resources["hijack/pixelart/maptip/silver.png"].texture;
            TEXTURES["hijack/pixelart/maptip/gray.png"] = PIXI.loader.resources["hijack/pixelart/maptip/gray.png"].texture;
            for (var i = 0; i < 4; ++i) {
                const name = i === 0 ? "left" : i === 1 ? "back" : i === 2 ? "right" : "front";

                for (var j = 0; j < 4; ++j) {
                    TEXTURES["hijack/pixelart/teiri/walk/" + name + "/" + j + ".png"] = new PIXI.Texture(PIXI.loader.resources["hijack/pixelart/teiri/walk.png"].texture, new PIXI.Rectangle(j * 16, i * 16, 16, 16));
                    TEXTURES["hijack/pixelart/teiri/truncheon/" + name + "/" + j + ".png"] = new PIXI.Texture(PIXI.loader.resources["hijack/pixelart/teiri/truncheon.png"].texture, new PIXI.Rectangle(j * 32, i * 32, 32, 32));
                }
            }
            resolve();
        });
    });
}
