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

function newHijack(images) {
    return {
        mode: HIJACK_MODE_TITLE,
        images: images
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

function viewHijack(state, onscreenCanvas, offscreenCanvas) {
    var offscreenContext = offscreenCanvas.getContext("2d");
    offscreenContext.imageSmoothingEnabled = false;
    offscreenContext.fillRect(0, 0, offscreenCanvas.width, offscreenCanvas.height);

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

    var onscreenContext = onscreenCanvas.getContext("2d");
    onscreenContext.drawImage(offscreenCanvas, 0, 0);
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

        state.character0 =
            {
                i: 0,
                x: 0,
                y: 480 - 192,
                pose: "neutral",
                direction: "right",
                images: {
                    neutral_left: state.images[1],
                    neutral_right: state.images[2],
                    run_left: state.images[3],
                    run_right: state.images[4]
                }
            };

        state.character1 =
            {
                i: 0,
                x: 640 - 192,
                y: 480 - 192,
                pose: "neutral",
                direction: "left",
                images: {
                    neutral_left: state.images[1],
                    neutral_right: state.images[2],
                    run_left: state.images[3],
                    run_right: state.images[4]
                }
            };
    }

    return state;
}

function stepHijackModeGame(state, input) {
    switch (input.length) {
    case 0:
        break;
    case 1:
    default:
        var x0 = input[0].axes[0];
        var y0 = input[0].axes[1];

        if (x0 < -0.5) {
            if (state.character0.pose === "run")
                ++state.character0.i;
            else
                state.character0.i = 0;
            state.character0.pose = "run";
            state.character0.direction = "left";
            if (state.character0.i % 4 === 0)
                state.character0.x -= 36;
        } else if (x0 > 0.5) {
            if (state.character0.pose === "run")
                ++state.character0.i;
            else
                state.character0.i = 0;
            state.character0.pose = "run";
            state.character0.direction = "right";
            if (state.character0.i % 4 === 0)
                state.character0.x += 36;
        } else {
            if (state.character0.pose === "neutral")
                ++state.character0.i;
            else
                state.character0.i = 0;
            state.character0.pose = "neutral";
        }
    }

    return state;
}

function stepHijackModeResult(state, input) {
    return state;
}

function viewHijackModeTitle(state) {
    var views = [{
        sx: 0,
        sy: 0,
        sw: state.images[0].width,
        sh: state.images[0].height,
        dx: (640 - state.images[0].width * 2) / 2,
        dy: (480 - state.images[0].height * 2) / 2,
        dw: state.images[0].width * 2,
        dh: state.images[0].height * 2,
        img: state.images[0]
    }];

    return views;
}

function viewHijackModeCharacterSelection(state) {
    var views = [];

    return views;
}

function viewHijackModeGame(state) {
    var views = [];

    switch (state.character0.pose) {
    case "neutral":
        switch (state.character0.direction) {
        case "left":
            views.push({
                sx: Math.floor(state.character0.i / 5 % 8) * 96,
                sy: 0,
                sw: 96,
                sh: 96,
                dx: state.character0.x,
                dy: state.character0.y,
                dw: 192,
                dh: 192,
                img: state.character0.images.neutral_left
            });
            break;
        case "right":
            views.push({
                sx: Math.floor(state.character0.i / 5 % 8) * 96,
                sy: 0,
                sw: 96,
                sh: 96,
                dx: state.character0.x,
                dy: state.character0.y,
                dw: 192,
                dh: 192,
                img: state.character0.images.neutral_right
            });
            break;
        }
        break;
    case "run":
        switch (state.character0.direction) {
        case "left":
            views.push({
                sx: Math.floor(state.character0.i / 4 % 8) * 96,
                sy: 0,
                sw: 96,
                sh: 96,
                dx: state.character0.x,
                dy: state.character0.y,
                dw: 192,
                dh: 192,
                scale: 2,
                img: state.character0.images.run_left
            });
            break;
        case "right":
            views.push({
                sx: Math.floor(state.character0.i / 4 % 8) * 96,
                sy: 0,
                sw: 96,
                sh: 96,
                dx: state.character0.x,
                dy: state.character0.y,
                dw: 192,
                dh: 192,
                scale: 2,
                img: state.character0.images.run_right
            });
            break;
        }
        break;
    }

    return views;
}

function viewHijackModeResult(state) {
    return [];
}

window.addEventListener("load", function () {
    Promise.all([
        loadImage("pixelart/system/logo.png"),
        loadImage("pixelart/teiri/neutral_left.png"),
        loadImage("pixelart/teiri/neutral_right.png"),
        loadImage("pixelart/teiri/run_left.png"),
        loadImage("pixelart/teiri/run_right.png")
    ]).then(function (images) {
        var state = newHijack(images);
        var onscreenCanvas = document.getElementById("hijack");
        var offscreenCanvas = document.createElement("canvas");
        offscreenCanvas.width = onscreenCanvas.width;
        offscreenCanvas.height = onscreenCanvas.height;
        viewHijack(state, onscreenCanvas, offscreenCanvas);

        requestAnimationFrame(function step() {
            var input = Array.from(navigator.getGamepads()).filter(function (pad) { return pad != null; });
            state = stepHijack(state, input);
            viewHijack(state, onscreenCanvas, offscreenCanvas);
            requestAnimationFrame(step);
        });
    });
});
