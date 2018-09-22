import * as Asset from "./asset.js";
import * as Control from "./control.js";
import * as Font from "./font.js";
import * as Input from "./input.js";
import * as Object from "./object.js";

export {Asset, Control, Font, Input, Object};

export async function create() {
    PIXI.settings.SCALE_MODE = PIXI.SCALE_MODES.NEAREST;

    await Promise.all([Font.load(), Asset.load()]);

    resize();

    const app = new PIXI.Application({autoStart: false, width: 1920, height: 1280});
    app.stage.scale.set(8, 8);
    app.renderer.backgroundColor = 0xC0C0C0;

    document.getElementById("game").appendChild(app.view);

    const teiri = await Object.Teiri.create(0, 0, await Control.Playable.create());
    const teiri2 = await Object.Teiri.create(60, 40, await Control.Random.create(0.252));
    const teiri3 = await Object.Teiri.create(120, 40, await Control.Random.create(0.252));
    const teiri4 = await Object.Teiri.create(60, 80, await Control.Random.create(0.252));
    const teiri5 = await Object.Teiri.create(120, 80, await Control.Random.create(0.252));
    teiri.sprite.play();
    app.stage.addChild(teiri.sprite);
    teiri2.sprite.play();
    app.stage.addChild(teiri2.sprite);
    teiri3.sprite.play();
    app.stage.addChild(teiri3.sprite);
    teiri4.sprite.play();
    app.stage.addChild(teiri4.sprite);
    teiri5.sprite.play();
    app.stage.addChild(teiri5.sprite);

    return {
        app: app,
        objects: [teiri, teiri2, teiri3, teiri4, teiri5]
    };
}

export async function step(game) {
    const gamepad = navigator.getGamepads()[0];

    if (gamepad) {
        Input.PLAYER.x = gamepad.axes[0];
        Input.PLAYER.y = gamepad.axes[1];
        Input.PLAYER.buttons = [gamepad.buttons[0].pressed, gamepad.buttons[1].pressed, gamepad.buttons[2].pressed, gamepad.buttons[3].pressed];
    }

    for (var i = 0; i < game.objects.length; ++i)
        await Object.step(game, game.objects[i]);

    game.app.renderer.render(game.app.stage);

    return game;
}

export async function resize() {
    if (document.body.clientWidth < 480)
        document.getElementById("game").setAttribute("class", "single");
    else if (document.body.clientWidth < 960)
        document.getElementById("game").setAttribute("class", "double");
    else
        document.getElementById("game").setAttribute("class", "quadruple");
}
