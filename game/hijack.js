import * as Game from "./hijack/js/index.js";
import * as Input from "./hijack/js/input.js";

window.addEventListener("load", () => {
    Game.create().then(game => {
        requestAnimationFrame(function step () {
            Game.step(game).then(next_game => {
                game = next_game;
                requestAnimationFrame(step);
            });
        });
    });
});

window.addEventListener("resize", () => {
    Game.resize();
});

window.addEventListener("keydown", (e) => {
    if (e.ctrlKey) {
        switch (e.key) {
        case "b":
            Input.PLAYER.x = -1;
            return e.preventDefault();
        case "p":
            Input.PLAYER.y = -1;
            return e.preventDefault();
        case "f":
            Input.PLAYER.x = 1;
            return e.preventDefault();
        case "n":
            Input.PLAYER.y = 1;
            return e.preventDefault();
        };
    } else {
        switch (e.key) {
        case "z":
            Input.PLAYER.buttons[0] = true;
            return e.preventDefault();
        case "x":
            Input.PLAYER.buttons[1] = true;
            return e.preventDefault();
        case "c":
            Input.PLAYER.buttons[2] = true;
            return e.preventDefault();
        case "v":
            Input.PLAYER.buttons[3] = true;
            return e.preventDefault();
        case "h":
        case "a":
        case "Left":
        case "ArrowLeft":
            Input.PLAYER.x = -1;
            return e.preventDefault();
        case "k":
        case "w":
        case "Up":
        case "ArrowUp":
            Input.PLAYER.y = -1;
            return e.preventDefault();
        case "l":
        case "d":
        case "Right":
        case "ArrowRight":
            Input.PLAYER.x = 1;
            return e.preventDefault();
        case "j":
        case "s":
        case "Down":
        case "ArrowDown":
            Input.PLAYER.y = 1;
            return e.preventDefault();
        };
    }
});

window.addEventListener("keyup", (e) => {
    if (e.ctrlKey) {
        switch (e.key) {
        case "b":
        case "p":
        case "f":
        case "n":
            Input.PLAYER.x = 0;
            Input.PLAYER.y = 0;
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
            Input.PLAYER.x = 0;
            Input.PLAYER.y = 0;
            Input.PLAYER.buttons[0] = false;
            Input.PLAYER.buttons[1] = false;
            Input.PLAYER.buttons[2] = false;
            Input.PLAYER.buttons[3] = false;
            return e.preventDefault();
        };
    }
});
