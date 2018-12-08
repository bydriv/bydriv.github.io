window.addEventListener("load", function() {
    /**************************************************************************/
    /*                          GAME  CONFIGURATIONS                          */
    /**************************************************************************/
    const SCALE = 2;
    const WIDTH = 640 / SCALE;
    const HEIGHT = 480 / SCALE;

    const ASSETS = [
      ["pixelart/teiri/walk.png", [
        ["pixelart/teiri/walk/left/0.png", [0, 0, 16, 16]],
        ["pixelart/teiri/walk/left/1.png", [16, 0, 16, 16]],
        ["pixelart/teiri/walk/left/2.png", [32, 0, 16, 16]],
        ["pixelart/teiri/walk/left/3.png", [48, 0, 16, 16]],
        ["pixelart/teiri/walk/back/0.png", [0, 16, 16, 16]],
        ["pixelart/teiri/walk/back/1.png", [16, 16, 16, 16]],
        ["pixelart/teiri/walk/back/2.png", [32, 16, 16, 16]],
        ["pixelart/teiri/walk/back/3.png", [48, 16, 16, 16]],
        ["pixelart/teiri/walk/right/0.png", [0, 32, 16, 16]],
        ["pixelart/teiri/walk/right/1.png", [16, 32, 16, 16]],
        ["pixelart/teiri/walk/right/2.png", [32, 32, 16, 16]],
        ["pixelart/teiri/walk/right/3.png", [48, 32, 16, 16]],
        ["pixelart/teiri/walk/front/0.png", [0, 48, 16, 16]],
        ["pixelart/teiri/walk/front/1.png", [16, 48, 16, 16]],
        ["pixelart/teiri/walk/front/2.png", [32, 48, 16, 16]],
        ["pixelart/teiri/walk/front/3.png", [48, 48, 16, 16]]
      ]],
      ["pixelart/teiri/hijack.png", [
        ["pixelart/teiri/hijack/left/0.png", [0, 0, 32, 32]],
        ["pixelart/teiri/hijack/left/1.png", [32, 0, 32, 32]],
        ["pixelart/teiri/hijack/left/2.png", [64, 0, 32, 32]],
        ["pixelart/teiri/hijack/left/3.png", [96, 0, 32, 32]],
        ["pixelart/teiri/hijack/back/0.png", [0, 32, 32, 32]],
        ["pixelart/teiri/hijack/back/1.png", [32, 32, 32, 32]],
        ["pixelart/teiri/hijack/back/2.png", [64, 32, 32, 32]],
        ["pixelart/teiri/hijack/back/3.png", [96, 32, 32, 32]],
        ["pixelart/teiri/hijack/right/0.png", [0, 64, 32, 32]],
        ["pixelart/teiri/hijack/right/1.png", [32, 64, 32, 32]],
        ["pixelart/teiri/hijack/right/2.png", [64, 64, 32, 32]],
        ["pixelart/teiri/hijack/right/3.png", [96, 64, 32, 32]],
        ["pixelart/teiri/hijack/front/0.png", [0, 96, 32, 32]],
        ["pixelart/teiri/hijack/front/1.png", [32, 96, 32, 32]],
        ["pixelart/teiri/hijack/front/2.png", [64, 96, 32, 32]],
        ["pixelart/teiri/hijack/front/3.png", [96, 96, 32, 32]]
      ]],
      ["pixelart/maptip/archimedes.png", [
        ["pixelart/maptip/archimedes/0-0.png", [0, 0, 16, 16]],
        ["pixelart/maptip/archimedes/1-0.png", [0, 0, 16, 16]],
        ["pixelart/maptip/archimedes/0-1.png", [0, 0, 16, 16]],
        ["pixelart/maptip/archimedes/1-1.png", [0, 0, 16, 16]]
      ]]
    ];

    wasm_bindgen("./wasm/hijack_bg.wasm").then(function () {
        const Game = wasm_bindgen;

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

        function initSystem() {
          const assets = new Map();
          const promises = ASSETS.map(function (asset) {
            const src = asset[0];

            return loadImage(src).then(function (img) {
              for (var i = 0; i < asset[1].length; ++i) {
                const name = asset[1][i][0];
                const sx = asset[1][i][1][0];
                const sy = asset[1][i][1][1];
                const sw = asset[1][i][1][2];
                const sh = asset[1][i][1][3];

                assets.set(name, {
                  img: img,
                  sx: sx,
                  sy: sy,
                  sw: sw,
                  sh: sh
                });
              }

              return Promise.resolve();
            });
          });

          return Promise.all(promises).then(function () {
            const offscreenCanvas = document.createElement("canvas");
            const onscreenCanvas = document.createElement("canvas");
            offscreenCanvas.width = WIDTH;
            offscreenCanvas.height = HEIGHT;
            onscreenCanvas.width = WIDTH * SCALE;
            onscreenCanvas.height = HEIGHT * SCALE;
            document.getElementById("game").appendChild(onscreenCanvas);

            const offscreenContext = offscreenCanvas.getContext("2d");

            const onscreenContext = onscreenCanvas.getContext("2d");
            onscreenContext.imageSmoothingEnabled = false;

            return Promise.resolve({
              offscreen: {
                canvas: offscreenCanvas,
                context: offscreenContext
              },
              onscreen: {
                canvas: onscreenCanvas,
                context: onscreenContext
              },
              assets: assets
            });
          });
        }

        function drawView(canvas, context, assets, views, i) {
            if (Game.view_is_image(i, views)) {
              const dx = Game.view_image_x(i, views);
              const dy = Game.view_image_y(i, views);
              const src = Game.view_image_name(i, views);
              const sprite = assets.get(src);
              context.drawImage(
                sprite.img,
                sprite.sx,
                sprite.sy,
                sprite.sw,
                sprite.sh,
                dx,
                dy,
                sprite.sw,
                sprite.sh
              );
            } else {
                console.warn("unrecognized view: %o", view);
            }
        }

        const keyboardInput = {
            x: 0,
            y: 0,
            buttons: [false, false, false, false, false, false, false, false]
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
                case "v":
                    keyboardInput.buttons[3] = true;
                    return e.preventDefault();
                case "h":
                case "a":
                case "Left":
                case "ArrowLeft":
                    keyboardInput.x = -1;
                    return e.preventDefault();
                case "k":
                case "w":
                case "Up":
                case "ArrowUp":
                    keyboardInput.y = -1;
                    return e.preventDefault();
                case "l":
                case "d":
                case "Right":
                case "ArrowRight":
                    keyboardInput.x = 1;
                    return e.preventDefault();
                case "j":
                case "s":
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
                    return e.preventDefault();
                };
            }
        });

        initSystem().then(function(ret) {
          const offscreen = ret.offscreen;
          const onscreen = ret.onscreen;
          const assets = ret.assets;

          var game = Game.new_();

          requestAnimationFrame(function step() {
            const views = Game.views(game);

            offscreen.context.fillRect(0, 0, offscreen.canvas.width, offscreen.canvas.height);

            for (var i = 0; i < Game.views_length(views); ++i)
              drawView(offscreen.canvas, offscreen.context, assets, views, i);

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

            const inputs = navigator.getGamepads().map(function(gamepad, i) {
              const input = {
                x: i === 0 ? keyboardInput.x : 0,
                y: i === 0 ? keyboardInput.y : 0,
                buttons:
                  i === 0
                    ? keyboardInput.buttons.concat()
                    : [false, false, false, false, false, false, false, false]
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
              inputs[0] = {
                x: keyboardInput.x,
                y: keyboardInput.y,
                buttons: keyboardInput.buttons.concat()
              };

            game = Game.step(inputs, game);
            requestAnimationFrame(step);
          });
        });
    }).catch(function (e) {
        console.error(e);
    });
});
