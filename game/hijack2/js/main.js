const Game = require("./hijack.js");

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
  const promises = Game.assets.map(function (asset) {
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
    offscreenCanvas.width = Game.width;
    offscreenCanvas.height = Game.height;
    onscreenCanvas.width = Game.width * Game.scale;
    onscreenCanvas.height = Game.height * Game.scale;
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

function drawView(canvas, context, assets, view) {
  switch (view[0]) {
  case -795439301 /* `Image */:
    const dx = view[1][0];
    const dy = view[1][1];
    const src = view[1][2];
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
    break;
  default:
    console.warn("unrecognized view: %o", view);
    break;
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

window.addEventListener("load", function() {
  initSystem().then(function(ret) {
    const offscreen = ret.offscreen;
    const onscreen = ret.onscreen;
    const assets = ret.assets;

    Game.intro().then(function(game) {
      requestAnimationFrame(function step() {
        Game.views(game).then(function (views) {
          offscreen.context.fillRect(0, 0, offscreen.canvas.width, offscreen.canvas.height);

          for (var i = 0; i < views.length; ++i)
            drawView(offscreen.canvas, offscreen.context, assets, views[i]);

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

          Game.step(inputs, game).then(function(next_game) {
            game = next_game;
            requestAnimationFrame(step);
          });
        });
      });
    });
  });
});
