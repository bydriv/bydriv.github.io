const Game = require("./hijack.js");

function initSystem(k) {
  PIXI.settings.SCALE_MODE = PIXI.SCALE_MODES.NEAREST;

  const assets = new Map();

  PIXI.loader.add(Game.assets.map(function(asset) { return asset[0]; })).load(function() {
    for (var i = 0; i < Game.assets.length; ++i) {
      const asset = Game.assets[i];
      const src = asset[0];

      for (var j = 0; j < asset[1].length; ++j) {
        const name = asset[1][j][0];
        const sx = asset[1][j][1][0];
        const sy = asset[1][j][1][1];
        const sw = asset[1][j][1][2];
        const sh = asset[1][j][1][3];

        assets.set(name, new PIXI.Texture(PIXI.loader.resources[src].texture, new PIXI.Rectangle(sx, sy, sw, sh)));
      }
    }

    const app = new PIXI.Application({autoStart: false, width: Game.width * Game.scale, height: Game.height * Game.scale});
    document.getElementById("game").appendChild(app.view);
    app.stage.scale.set(Game.scale, Game.scale);
    app.renderer.backgroundColor = 0x404040;

    k(app, assets);
  });
}

var FREE_SPRITES = [];
var USED_SPRITES = [];

function getSprite(texture, x, y) {
  if (FREE_SPRITES.length === 0) {
    const sprite = new PIXI.Sprite(texture, x, y);
    USED_SPRITES.push(sprite);

    return sprite;
  } else {
    const sprite = FREE_SPRITES.pop();
    USED_SPRITES.push(sprite);

    sprite.texture = texture;
    sprite.x = x;
    sprite.y = y;

    return sprite;
  }
}

function drawView(app, assets, view) {
  switch (view[0]) {
  case -795439301 /* `Image */:
    const dx = view[1][0];
    const dy = view[1][1];
    const src = view[1][2];
    const sprite = getSprite(assets.get(src), dx, dy);
    app.stage.addChild(sprite);
    break;
  default:
    console.warn("unrecognized view: %o", view);
    break;
  }
}

window.addEventListener("load", function() {
  initSystem(function(app, assets) {
    Game.intro().then(function(game) {
      requestAnimationFrame(function step() {
        Game.views(game).then(function (views) {
          for (var i = 0; i < views.length; ++i)
            drawView(app, assets, views[i]);

          app.renderer.render(app.stage);
          app.stage.removeChildren();

          FREE_SPRITES = FREE_SPRITES.concat(USED_SPRITES);
          USED_SPRITES = [];

          const inputs = navigator.getGamepads().map(function(gamepad) {
            const input = {
              x: 0,
              y: 0,
              buttons: [false, false, false, false, false, false, false, false]
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
                input.buttons[4] = gamepad.buttons[6].pressed;
                input.buttons[5] = gamepad.buttons[7].pressed;
            }

            return input;
          });

          Game.step(inputs, game).then(function(next_game) {
            game = next_game;
            requestAnimationFrame(step);
          });
        });
      });
    });
  });
});
