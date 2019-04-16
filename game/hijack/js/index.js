/******************************************************************************/
/*                             UTILITY  FUNCTIONS                             */
/******************************************************************************/
function loadImage(src) {
    return new Promise(function (resolve, reject) {
        const img = new Image();
        img.onload = function () {
            resolve(img);
        };
        img.onerror = function (e) {
            reject(e);
        };
        img.src = src;
    });
}

async function initSystem(config) {
    const stats = new Stats();
    stats.showPanel(0); // 0: fps
    document.body.appendChild(stats.dom);

    const assets = new Map();
    const promises = config.assets.map(async function (asset) {
        const src = asset[0];

        const img = await loadImage(src);

        return Promise.all(asset[1].map(async function (assetSpec) {
            const name = assetSpec[0];
            const sx = assetSpec[1][0];
            const sy = assetSpec[1][1];
            const sw = assetSpec[1][2];
            const sh = assetSpec[1][3];

            const canvas = document.createElement("canvas");
            canvas.width = sw;
            canvas.height = sh;

            const context = canvas.getContext("2d");
            context.drawImage(img, sx, sy, sw, sh, 0, 0, sw, sh);

            const img_ = await loadImage(canvas.toDataURL());

            assets.set(name, {
                img: img_
            });
        }));
    });

    await Promise.all(promises);

    const offscreenCanvas = document.createElement("canvas");
    const onscreenCanvas = document.createElement("canvas");
    offscreenCanvas.width = config.width;
    offscreenCanvas.height = config.height;
    onscreenCanvas.width = config.width * config.scale;
    onscreenCanvas.height = config.height * config.scale;
    document.getElementById("game").appendChild(onscreenCanvas);

    const offscreenContext = offscreenCanvas.getContext("2d");

    const onscreenContext = onscreenCanvas.getContext("2d");
    onscreenContext.imageSmoothingEnabled = false;

    return {
        stats: stats,
        offscreen: {
            canvas: offscreenCanvas,
            context: offscreenContext
        },
        onscreen: {
            canvas: onscreenCanvas,
            context: onscreenContext
        },
        assets: assets
    };
}

function drawView(Game, canvas, context, assets, views, i, x, y) {
    if (Game.view_is_image(i, views)) {
        const dx = Game.view_image_x(i, views);
        const dy = Game.view_image_y(i, views);
        const name = Game.view_image_name(i, views);

        if (!assets.has(name)) {
            console.warn("unrecognized view name: %o", name);
            return;
        }

        const sprite = assets.get(name);
        context.drawImage(sprite.img, dx + x, dy + y);
    } else {
        console.warn("unrecognized view: %o", i);
    }
}

/******************************************************************************/
/*                                  WASM FFI                                  */
/******************************************************************************/
function inputs_length(inputs) {
    return inputs.length;
}

function input_x(i, inputs) {
    return i < inputs.length ? inputs[i].x : null;
}

function input_y(i, inputs) {
    return i < inputs.length ? inputs[i].y : null;
}

function input_button(i, j, inputs) {
    return (i < inputs.length && j < inputs[i].buttons.length)
        ? inputs[i].buttons[j]
        : null;
}

window.addEventListener("load", async function() {
    /**************************************************************************/
    /*                          GAME  CONFIGURATIONS                          */
    /**************************************************************************/
    const response = await fetch("./json/config.json", {cache: "no-cache"});
    const config = await response.json();

    /**************************************************************************/
    /*                               INIT  WASM                               */
    /**************************************************************************/
    await wasm_bindgen("./wasm/hijackjs_bg.wasm");

    const Game = wasm_bindgen;

    const keyboardInput = {
        x: 0,
        y: 0,
        buttons: [false, false, false, false, false, false, false, false, false, false]
    };

    window.addEventListener("keydown", e => {
        if (e.ctrlKey) {
            switch (e.key) {
            case "b":
                keyboardInput.x = -1;
                return e.preventDefault();
            case "p":
                keyboardInput.y = -1;
                return e.preventDefault();
            case "f":
                keyboardInput.x = 1;
                return e.preventDefault();
            case "n":
                keyboardInput.y = 1;
                return e.preventDefault();
            };
        } else {
            switch (e.key) {
            case "z":
                keyboardInput.buttons[0] = true;
                return e.preventDefault();
            case "x":
                keyboardInput.buttons[1] = true;
                return e.preventDefault();
            case "c":
                keyboardInput.buttons[2] = true;
                return e.preventDefault();
            case "a":
                keyboardInput.buttons[3] = true;
                return e.preventDefault();
            case "s":
                keyboardInput.buttons[4] = true;
                return e.preventDefault();
            case "d":
                keyboardInput.buttons[5] = true;
                return e.preventDefault();
            case "q":
                keyboardInput.buttons[6] = true;
                return e.preventDefault();
            case "w":
                keyboardInput.buttons[7] = true;
                return e.preventDefault();
            case "h":
            case "Left":
            case "ArrowLeft":
                keyboardInput.x = -1;
                return e.preventDefault();
            case "k":
            case "Up":
            case "ArrowUp":
                keyboardInput.y = -1;
                return e.preventDefault();
            case "l":
            case "Right":
            case "ArrowRight":
                keyboardInput.x = 1;
                return e.preventDefault();
            case "j":
            case "Down":
            case "ArrowDown":
                keyboardInput.y = 1;
                return e.preventDefault();
            };
        }
    });

    window.addEventListener("keyup", e => {
        if (e.ctrlKey) {
            switch (e.key) {
            case "b":
            case "p":
            case "f":
            case "n":
                keyboardInput.x = 0;
                keyboardInput.y = 0;
                return e.preventDefault();
            };
        } else {
            switch (e.key) {
            case "z":
            case "x":
            case "c":
            case "v":
            case "h":
            case "k":
            case "l":
            case "j":
            case "a":
            case "w":
            case "d":
            case "s":
            case "Left":
            case "Up":
            case "Right":
            case "Down":
            case "ArrowLeft":
            case "ArrowUp":
            case "ArrowRight":
            case "ArrowDown":
                keyboardInput.x = 0;
                keyboardInput.y = 0;
                keyboardInput.buttons[0] = false;
                keyboardInput.buttons[1] = false;
                keyboardInput.buttons[2] = false;
                keyboardInput.buttons[3] = false;
                keyboardInput.buttons[4] = false;
                keyboardInput.buttons[5] = false;
                keyboardInput.buttons[6] = false;
                keyboardInput.buttons[7] = false;
                keyboardInput.buttons[8] = false;
                keyboardInput.buttons[9] = false;
                return e.preventDefault();
            };
        }
    });

    const ctrl = document.getElementById("ctrl");

    window.addEventListener("click", e => {
        const rect = ctrl.getBoundingClientRect();
        const x = (e.clientX - rect.left) / config.scale;
        const y = (e.clientY - rect.top) / config.scale;

        if (0 <= x && x < config.width && 0 <= y && y < config.height)
            return e.preventDefault();
    });

    window.addEventListener("dblclick", e => {
        const rect = ctrl.getBoundingClientRect();
        const x = (e.clientX - rect.left) / config.scale;
        const y = (e.clientY - rect.top) / config.scale;

        if (0 <= x && x < config.width && 0 <= y && y < config.height)
            return e.preventDefault();
    });

    window.addEventListener("contextmenu", e => {
        const rect = ctrl.getBoundingClientRect();
        const x = (e.clientX - rect.left) / config.scale;
        const y = (e.clientY - rect.top) / config.scale;

        if (0 <= x && x < config.width && 0 <= y && y < config.height)
            return e.preventDefault();
    });

    window.addEventListener("mousedown", e => {
        const rect = ctrl.getBoundingClientRect();
        const x = (e.clientX - rect.left) / config.scale;
        const y = (e.clientY - rect.top) / config.scale;

        if (0 <= x && x < config.width && 0 <= y && y < config.height)
            return e.preventDefault();
    });

    window.addEventListener("mouseup", e => {
        const rect = ctrl.getBoundingClientRect();
        const x = (e.clientX - rect.left) / config.scale;
        const y = (e.clientY - rect.top) / config.scale;

        if (0 <= x && x < config.width && 0 <= y && y < config.height)
            return e.preventDefault();
    });

    window.addEventListener("mouseover", e => {
        const rect = ctrl.getBoundingClientRect();
        const x = (e.clientX - rect.left) / config.scale;
        const y = (e.clientY - rect.top) / config.scale;

        if (0 <= x && x < config.width && 0 <= y && y < config.height)
            return e.preventDefault();
    });

    window.addEventListener("mouseenter", e => {
        const rect = ctrl.getBoundingClientRect();
        const x = (e.clientX - rect.left) / config.scale;
        const y = (e.clientY - rect.top) / config.scale;

        if (0 <= x && x < config.width && 0 <= y && y < config.height)
            return e.preventDefault();
    });

    window.addEventListener("mouseout", e => {
        const rect = ctrl.getBoundingClientRect();
        const x = (e.clientX - rect.left) / config.scale;
        const y = (e.clientY - rect.top) / config.scale;

        if (0 <= x && x < config.width && 0 <= y && y < config.height)
            return e.preventDefault();
    });

    window.addEventListener("mouseleave", e => {
        const rect = ctrl.getBoundingClientRect();
        const x = (e.clientX - rect.left) / config.scale;
        const y = (e.clientY - rect.top) / config.scale;

        if (0 <= x && x < config.width && 0 <= y && y < config.height)
            return e.preventDefault();
    });

    window.addEventListener("mousemove", e => {
        const rect = ctrl.getBoundingClientRect();
        const x = (e.clientX - rect.left) / config.scale;
        const y = (e.clientY - rect.top) / config.scale;

        if (0 <= x && x < config.width && 0 <= y && y < config.height)
            return e.preventDefault();
    });

    window.addEventListener("wheel", e => {
        const rect = ctrl.getBoundingClientRect();
        const x = (e.clientX - rect.left) / config.scale;
        const y = (e.clientY - rect.top) / config.scale;

        if (0 <= x && x < config.width && 0 <= y && y < config.height)
            return e.preventDefault();
    });

    window.addEventListener("touchstart", e => {
        const rect = ctrl.getBoundingClientRect();
        const xs = [];
        const ys = [];

        for (var i = 0; i < e.touches.length; ++i) {
            const touch = e.touches[i];
            const x = (touch.clientX - rect.left) / config.scale;
            const y = (touch.clientY - rect.top) / config.scale;
            xs.push(x);
            ys.push(y);

            if (16 <= x && x < 48 && 160 <= y && y < 192) {
                keyboardInput.x = -1;
            } else if (80 <= x && x < 112 && 160 <= y && y < 192) {
                keyboardInput.x = 1;
            } else if (48 <= x && x < 80 && 128 <= y && y < 160) {
                keyboardInput.y = -1;
            } else if (48 <= x && x < 80 && 192 <= y && y < 224) {
                keyboardInput.y = 1;
            } else if (208 <= x && x < 240 && 160 <= y && y < 192) {
                keyboardInput.buttons[2] = true;
            } else if (272 <= x && x < 304 && 160 <= y && y < 192) {
                keyboardInput.buttons[1] = true;
            } else if (240 <= x && x < 272 && 128 <= y && y < 160) {
                keyboardInput.buttons[3] = true;
            } else if (240 <= x && x < 272 && 192 <= y && y < 224) {
                keyboardInput.buttons[0] = true;
            } else if (0 <= x && x < 64 && 0 <= y && y < 32) {
                keyboardInput.buttons[4] = true;
            } else if (256 <= x && x < 320 && 0 <= y && y < 32) {
                keyboardInput.buttons[5] = true;
            } else if (112 <= x && x < 144 && 224 <= y && y < 240) {
                keyboardInput.buttons[6] = true;
            } else if (176 <= x && x < 208 && 224 <= y && y < 240) {
                keyboardInput.buttons[7] = true;
            }

        }

        if (0 <= Math.min(...xs) && Math.max(...xs) < config.width && 0 <= Math.min(...ys) && Math.max(...ys) < config.height)
            return e.preventDefault();
    });

    window.addEventListener("touchend", e => {
        keyboardInput.x = 0;
        keyboardInput.y = 0;
        keyboardInput.buttons[0] = false;
        keyboardInput.buttons[1] = false;
        keyboardInput.buttons[2] = false;
        keyboardInput.buttons[3] = false;
        keyboardInput.buttons[4] = false;
        keyboardInput.buttons[5] = false;
        keyboardInput.buttons[6] = false;
        keyboardInput.buttons[7] = false;
        keyboardInput.buttons[8] = false;
        keyboardInput.buttons[9] = false;

        const rect = ctrl.getBoundingClientRect();
        const x = (e.clientX - rect.left) / config.scale;
        const y = (e.clientY - rect.top) / config.scale;

        if (0 <= x && x < config.width && 0 <= y && y < config.height)
            return e.preventDefault();
    });

    window.addEventListener("touchcancel", e => {
        const rect = ctrl.getBoundingClientRect();
        const x = (e.clientX - rect.left) / config.scale;
        const y = (e.clientY - rect.top) / config.scale;

        if (0 <= x && x < config.width && 0 <= y && y < config.height)
            return e.preventDefault();
    });

    window.addEventListener("touchmove", e => {
        const rect = ctrl.getBoundingClientRect();
        const x = (e.clientX - rect.left) / config.scale;
        const y = (e.clientY - rect.top) / config.scale;

        if (0 <= x && x < config.width && 0 <= y && y < config.height)
            return e.preventDefault();
    });

    const {stats, offscreen, onscreen, assets} = await initSystem(config);
    const CACHE_OFFSET = 64;

    var game = Game.new_();
    var previousOffsetX = 0;
    var previousOffsetY = 0;
    var previousViewMap = new Map();
    const caches = new Map();

    const mode = document.getElementById("mode");
    var previousTimestamp = 0;

    var recording = false;
    var recorder = null;

    requestAnimationFrame(function step(timestamp) {
        stats.begin();

        if (!recording && mode["record-mode"].checked) {
            const canvasStream = onscreen.canvas.captureStream();
            recorder = new MediaRecorder(canvasStream);
            const chunks = [];

            recorder.addEventListener("dataavailable", function (e) {
                chunks.push(e.data);
            });

            recorder.addEventListener("stop", function () {
              const blob = new Blob(chunks, {"type": "video/mpeg"});
              const url = URL.createObjectURL(blob);
              const video = document.createElement("video");
              video.src = url;
              video.controls = true;
              const a = document.createElement("a");
              const text = document.createTextNode("download");
              a.href = url;
              a.appendChild(text);
              document.querySelector("#downloads").appendChild(video);
              document.querySelector("#downloads").appendChild(a);
              recorder = null;
            });

            recorder.start();
            recording = true;
        }

        if (recording && !mode["record-mode"].checked) {
            recorder.stop();
            recording = false;
        }

        var fpsMode = 60;

        if (mode["fps-mode"].value === "30") {
            fpsMode = 30;
        }

        if (previousTimestamp === 0)
            previousTimestamp = timestamp;

        const elapsed = timestamp - previousTimestamp;

        if (fpsMode === 30 && elapsed < 17) {
            requestAnimationFrame(step);
            return;
        }

        const inputs = Array.from(navigator.getGamepads()).map(function(gamepad, i) {
            const input = {
                x: i === 0 ? keyboardInput.x : 0,
                y: i === 0 ? keyboardInput.y : 0,
                buttons:
                i === 0
                    ? keyboardInput.buttons.concat()
                    : [false, false, false, false, false, false, false, false, false, false]
            };

            if (gamepad) {
                input.x = gamepad.axes[0];
                input.y = gamepad.axes[1];
                input.buttons[0] = gamepad.buttons[0].pressed;
                input.buttons[1] = gamepad.buttons[1].pressed;
                input.buttons[2] = gamepad.buttons[2].pressed;
                input.buttons[3] = gamepad.buttons[3].pressed;
                input.buttons[4] = gamepad.buttons[4].pressed;
                input.buttons[5] = gamepad.buttons[5].pressed;
                input.buttons[6] = gamepad.buttons[6].pressed;
                input.buttons[7] = gamepad.buttons[7].pressed;
                input.buttons[8] = gamepad.buttons[8].pressed;
                input.buttons[9] = gamepad.buttons[9].pressed;
            }

            return input;
        });

        if (inputs.length === 0)
            inputs.push({
                x: keyboardInput.x,
                y: keyboardInput.y,
                buttons: keyboardInput.buttons.concat()
            });

        const next_game = Game.step(fpsMode, inputs, game);
        game.free();
        game = next_game;

        const viewMap = Game.view_map(game);
        const offsetX = Game.view_map_x(viewMap);
        const offsetY = Game.view_map_y(viewMap);

        offscreen.context.fillRect(0, 0, offscreen.canvas.width, offscreen.canvas.height);

        for (var i = 0; i < Game.view_map_length(viewMap); ++i) {
            const z = Game.view_map_z(i, viewMap);
            const views = Game.view_map_views(i, viewMap);
            const refresh = !previousViewMap.has(z) || !Game.views_eq(views, previousViewMap.get(z)) || Math.floor(offsetX / CACHE_OFFSET) !== Math.floor(previousOffsetX / CACHE_OFFSET) || Math.floor(offsetY / CACHE_OFFSET) !== Math.floor(previousOffsetY / CACHE_OFFSET);

            if (previousViewMap.has(z)) {
                previousViewMap.get(z).free();
                previousViewMap.delete(z);
            }

            previousViewMap.set(z, views);

            if (!refresh) {
                if (!caches.has(z)) {
                    const cacheCanvas = document.createElement("canvas");
                    cacheCanvas.width = offscreen.canvas.width + CACHE_OFFSET * 2;
                    cacheCanvas.height = offscreen.canvas.height + CACHE_OFFSET * 2;

                    const cacheContext = cacheCanvas.getContext("2d");

                    const cache = {
                        canvas: cacheCanvas,
                        context: cacheContext
                    };

                    caches.set(z, cache);

                    cache.context.clearRect(0, 0, cache.canvas.width, cache.canvas.height);

                    for (var j = 0; j < Game.views_length(views); ++j) {
                        drawView(Game, cache.canvas, cache.context, assets, views, j, -offsetX + (offsetX - Math.floor(offsetX / CACHE_OFFSET) * CACHE_OFFSET), -offsetY + (offsetY - Math.floor(offsetY / CACHE_OFFSET) * CACHE_OFFSET));
                        drawView(Game, offscreen.canvas, offscreen.context, assets, views, j, -offsetX, -offsetY);
                    }
                }

                const cache = caches.get(z);

                offscreen.context.drawImage(cache.canvas, -(offsetX - Math.floor(offsetX / CACHE_OFFSET) * CACHE_OFFSET), -(offsetY - Math.floor(offsetY / CACHE_OFFSET) * CACHE_OFFSET));

                continue;
            }

            caches.delete(z);

            for (var j = 0; j < Game.views_length(views); ++j)
                drawView(Game, offscreen.canvas, offscreen.context, assets, views, j, -offsetX, -offsetY);
        }

        viewMap.free();

        onscreen.context.drawImage(
            offscreen.canvas,
            0,
            0,
            offscreen.canvas.width,
            offscreen.canvas.height,
            0,
            0,
            onscreen.canvas.width,
            onscreen.canvas.height
        );

        previousOffsetX = offsetX;
        previousOffsetY = offsetY;

        previousTimestamp = timestamp;

        stats.end();

        requestAnimationFrame(step);
    });
});
