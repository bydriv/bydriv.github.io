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

var HIJACK_MODE_TITLE = "title";
var HIJACK_MODE_CHARACTER_SELECTION = "characterSelection";
var HIJACK_MODE_GAME = "game";
var HIJACK_MODE_RESULT = "result";

function newHijack(config, onscreenCanvas, offscreenCanvas) {
    return {
        mode: HIJACK_MODE_TITLE,
        config: config,
        onscreenCanvas: onscreenCanvas,
        offscreenCanvas: offscreenCanvas
    };
};

function stepHijack(state, input) {
    switch (state.mode) {
    case HIJACK_MODE_TITLE:
        return stepHijackModeTitle(state, input);
    case HIJACK_MODE_CHARACTER_SELECTION:
        return stepHijackModeCharacterSelection(state, input);
    case HIJACK_MODE_GAME:
        return stepHijackModeGame(state, input);
    case HIJACK_MODE_RESULT:
        return stepHijackModeResult(state, input);
    }
}

function viewHijack(state) {
    var offscreenContext = state.offscreenCanvas.getContext("2d");
    offscreenContext.fillRect(0, 0, state.offscreenCanvas.width, state.offscreenCanvas.height);

    var views = [];

    switch (state.mode) {
    case "title":
        views = viewHijackModeTitle(state);
        break;
    case "characterSelection":
        views = viewHijackModeCharacterSelection(state);
        break;
    case "game":
        views = viewHijackModeGame(state);
        break;
    case "result":
        views = viewHijackModeResult(state);
        break;
    }

    for (var i = 0; i < views.length; ++i) {
        var view = views[i];

        offscreenContext.drawImage(view.img, view.sx, view.sy, view.sw, view.sh, view.dx, view.dy, view.dw, view.dh);
    }

    var onscreenContext = state.onscreenCanvas.getContext("2d");
    onscreenContext.drawImage(state.offscreenCanvas, 0, 0);
}

function stepHijackModeTitle(state, input) {
    if (input.some(function (pad) { return pad.buttons.some(function (button) { return button.pressed; }); })) {
        state.mode = HIJACK_MODE_CHARACTER_SELECTION;

        state.selection0 =
            {
                x: 0,
                y: 0
            };

        state.selection1 =
            {
                x: 0,
                y: 0
            };
    }

    return state;
}

function stepHijackModeCharacterSelection(state, input) {
    if (input.some(function (pad) { return pad.buttons.some(function (button) { return button.pressed; }); })) {
        state.mode = HIJACK_MODE_GAME;

        var character0 = state.config.characters[0];
        var character1 = state.config.characters[0];

        state.character0 = {
            i: 0,
            x: 0,
            y: 480 - character0.animations.neutral_right.height * 2,
            pose: "neutral",
            direction: "right",
            character: character0
        };

        state.character1 = {
            i: 0,
            x: 640 - character0.animations.neutral_left.width * 2,
            y: 480 - character0.animations.neutral_left.height * 2,
            pose: "neutral",
            direction: "left",
            character: character1
        };
    }

    return state;
}

function stepHijackModeGame(state, input) {
    switch (input.length) {
    case 0:
        break;
    case 1:
        stepCharacter(0, state.character0);
        break;
    default:
        stepCharacter(0, state.character0);
        stepCharacter(1, state.character1);
        break;
    }

    return state;

    function stepCharacter(i, character) {
        var x0 = input[i].axes[0];
        var y0 = input[i].axes[1];
        var button0 = input[i].buttons[0].pressed;

        switch (character.pose) {
        case "weak":
            var action;

            switch (character.direction) {
            case "left":
                action = character.character.actions.weak_left;
                break;
            case "right":
                action = character.character.actions.weak_right;
                break;
            }

            if (character.i < action.startup)
                ++character.i;
            else if (character.i < action.startup + action.active)
                ++character.i;
            else if (character.i < action.startup + action.active + action.recovery)
                ++character.i;
            else {
                character.pose = "neutral";
                character.i = 0;
            }
        default:
            if (button0) {
                character.i = 0;
                character.pose = "weak";
                return;
            }

            if (x0 < -0.5) {
                if (character.pose === "run")
                    ++character.i;
                else
                    character.i = 0;
                character.pose = "run";
                character.direction = "left";
                if (character.i % character.character.actions.run_left.frames_per_move === 0)
                    character.x += character.character.actions.run_left.move * 2;
            } else if (x0 > 0.5) {
                if (character.pose === "run")
                    ++character.i;
                else
                    character.i = 0;
                character.pose = "run";
                character.direction = "right";
                if (character.i % character.character.actions.run_right.frames_per_move === 0)
                    character.x += character.character.actions.run_right.move * 2;
            } else {
                if (character.pose === "neutral")
                    ++character.i;
                else
                    character.i = 0;
                character.pose = "neutral";
            }
        }
    }
}

function stepHijackModeResult(state, input) {
    return state;
}

function viewHijackModeTitle(state) {
    var views = [{
        sx: 0,
        sy: 0,
        sw: state.config.logo.width,
        sh: state.config.logo.height,
        dx: (640 - state.config.logo.width * 2) / 2,
        dy: (480 - state.config.logo.height * 2) / 2,
        dw: state.config.logo.width * 2,
        dh: state.config.logo.height * 2,
        img: state.config.logo
    }];

    return views;
}

function viewHijackModeCharacterSelection(state) {
    var views = [];

    return views;
}

function viewHijackModeGame(state) {
    var views = [];

    viewCharacter(state.character0);
    viewCharacter(state.character1);

    return views;

    function viewCharacter(character) {
        var id = character.pose + "_" + character.direction;
        var action = character.character.actions[id];
        var animation = action.animation;

        var m = Math.floor(animation.sprite_sheet.width / animation.width);

        views.push({
            sx: Math.floor(character.i / animation.frames_per_sprite % m) * animation.width,
            sy: 0,
            sw: animation.width,
            sh: animation.height,
            dx: character.x,
            dy: character.y,
            dw: animation.width * 2,
            dh: animation.height * 2,
            img: animation.sprite_sheet
        });
    }
}

function viewHijackModeResult(state) {
    return [];
}

function loadConfig(src) {
    return new Promise(function (resolve, reject) {
        fetch(src, {cache: "no-cache"}).then(function (config) { return config.json(); }).then(function (config) {
            if (typeof config.logo !== "string") {
                console.error("config.logo isn't a string: %o", i, config.logo);
                reject();
            }

            loadImage(config.logo).then(function (logo) {
                if (!Array.isArray(config.characters)) {
                    console.error("config.characters isn't an array: %o", config.characters);
                    return;
                }

                Promise.all(config.characters.map(function (path, i) {
                    if (typeof path !== "string") {
                        console.error("config.characters[%o] isn't a string: %o", i, config.characters);
                        reject();
                    }

                    return fetch(path, {cache: "no-cache"}).then(function (character) { return character.json(); });
                })).then(function (characters) {
                    var promises0 = [];

                    for (var i = 0; i < characters.length; ++i) {
                        var character = characters[i];

                        if (Object.prototype.toString.call(character.actions) !== "[object Object]") {
                            console.error("character.actions isn't an onject: %o", character.actions);
                            reject();
                        }

                        if (Object.prototype.toString.call(character.animations) !== "[object Object]") {
                            console.error("character.actions isn't an onject: %o", character.animations);
                            reject();
                        }

                        var promises1 = [];

                        for (var prop in character.animations) {
                            var animation = character.animations[prop];

                            if (animation == null) {
                                console.error("character.animations[%o] is %o", prop, character.animations[prop]);
                            }

                            if (typeof animation.width !== "number") {
                                console.error("animation.width isn't a number: %o", animation.width);
                                reject();
                            }

                            if (typeof animation.height !== "number") {
                                console.error("animation.width isn't a number: %o", animation.height);
                                reject();
                            }

                            if (typeof animation.frames_per_sprite !== "number") {
                                console.error("animation.width isn't a number: %o", animation.frames_per_sprite);
                                reject();
                            }

                            if (typeof animation.sprite_sheet !== "string") {
                                console.error("animation.sprite_sheet isn't a string: %o", animation.sprite_sheet);
                                reject();
                            }

                            (function (id, animation) {
                                promises1.push(loadImage(animation.sprite_sheet).then(function (sprite_sheet) {
                                    return {
                                        id: id,
                                        width: animation.width,
                                        height: animation.height,
                                        frames_per_sprite: animation.frames_per_sprite,
                                        sprite_sheet: sprite_sheet
                                    };
                                }));
                            }(prop, animation));
                        }

                        promises0.push(Promise.all(promises1).then(function (_animations) {
                            var animations = {};

                            for (var j = 0; j < _animations.length; ++j)
                                animations[_animations[j].id] = _animations[j];

                            var actions = {};

                            for (var prop in character.actions) {
                                var action = character.actions[prop];

                                if (action == null) {
                                    console.error("character.actions[%o] is %o", prop, character.actions[prop]);
                                }

                                if (animations[action.animation] == null) {
                                    console.error("animations[%o] is %o", action.animation, animations[action.animation]);
                                    reject();
                                }

                                var animation = animations[action.animation];

                                action = JSON.parse(JSON.stringify(action));
                                action.id = prop;
                                action.animation = animation;

                                actions[prop] = action;
                            }

                            return {
                                actions: actions,
                                animations: animations
                            };
                        }));
                    }

                    Promise.all(promises0).then(function (characters) {
                        resolve({
                            logo: logo,
                            characters: characters
                        });
                    });
                });
            });
        });
    });
}

window.addEventListener("load", function () {
    loadConfig("config.json").then(function (config) {
        var mode = document.getElementById("mode");

        var recording = false;
        var recorder = null;

        var onscreenCanvas = document.getElementById("hijack");
        var offscreenCanvas = document.createElement("canvas");
        offscreenCanvas.width = onscreenCanvas.width;
        offscreenCanvas.height = onscreenCanvas.height;
        var offscreenContext = offscreenCanvas.getContext("2d");
        offscreenContext.imageSmoothingEnabled = false;

        var state = newHijack(config, onscreenCanvas, offscreenCanvas);
        viewHijack(state);

        requestAnimationFrame(function step() {
            if (!recording && mode["record-mode"].checked) {
                var canvasStream = onscreenCanvas.captureStream();
                recorder = new MediaRecorder(canvasStream);
                var chunks = [];

                recorder.addEventListener("dataavailable", function (e) {
                    chunks.push(e.data);
                });

                recorder.addEventListener("stop", function () {
                    var blob = new Blob(chunks, {"type": "video/mpeg"});
                    var url = URL.createObjectURL(blob);
                    var video = document.createElement("video");
                    video.src = url;
                    video.controls = true;
                    var a = document.createElement("a");
                    var text = document.createTextNode("download");
                    a.href = url;
                    a.appendChild(text);

                    var w = document.createElement("div");
                    var header = document.createElement("div");
                    var content = document.createElement("div");
                    w.setAttribute("class", "window");
                    header.setAttribute("class", "header");
                    content.setAttribute("class", "content");
                    header.appendChild(document.createTextNode(url));
                    content.appendChild(video);
                    content.appendChild(a);
                    w.appendChild(header);
                    w.appendChild(content);

                    document.querySelector("body").appendChild(w);

                    recorder = null;
                });

                recorder.start();
                recording = true;
            }

            if (recording && !mode["record-mode"].checked) {
                recorder.stop();
                recording = false;
            }

            var input = Array.from(navigator.getGamepads()).filter(function (pad) { return pad != null; });
            state = stepHijack(state, input);
            viewHijack(state);
            requestAnimationFrame(step);
        });
    });
});
