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
        images: images,
        views: [{
            sx: 0,
            sy: 0,
            dx: (640 - images[0].width) / 2,
            dy: (480 - images[0].height) / 2,
            width: images[0].width,
            height: images[0].height,
            img: images[0]
        }]
    };
};

function stepHijack(state) {
    switch (state.mode) {
    case HIJACK_MODE_TITLE:
        return stepHijackModeTitle(state);
    case HIJACK_MODE_CHARACTER_SELECTION:
        return stepHijackCharacterSelection(state);
    case HIJACK_MODE_GAME:
        return stepHijackModeGame(state);
    case HIJACK_MODE_RESULT:
        return stepHijackModeResult(state);
    }
}

function viewHijack(state, onscreenCanvas, offscreenCanvas) {
    var offscreenContext = offscreenCanvas.getContext("2d");

    offscreenContext.fillRect(0, 0, offscreenCanvas.width, offscreenCanvas.height);

    for (var i = 0; i < state.views.length; ++i) {
        var view = state.views[i];
        offscreenContext.drawImage(view.img, view.sx, view.sy, view.width, view.height, view.dx, view.dy, view.width, view.height);
    }

    var onscreenContext = onscreenCanvas.getContext("2d");
    onscreenContext.drawImage(offscreenCanvas, 0, 0);
}

function stepHijackModeTitle(state) {
    return state;
}

function stepHijackModeCharacterSelection(state) {
    return state;
}

function stepHijackModeGame(state) {
    return state;
}

function stepHijackModeResult(state) {
    return state;
}

window.addEventListener("load", function () {
    Promise.all([loadImage("pixelart/system/logo.png")]).then(function (images) {
        var state = newHijack(images);
        var onscreenCanvas = document.getElementById("hijack");
        var offscreenCanvas = document.createElement("canvas");
        offscreenCanvas.width = onscreenCanvas.width;
        offscreenCanvas.height = onscreenCanvas.height;
        viewHijack(state, onscreenCanvas, offscreenCanvas);

        requestAnimationFrame(function step() {
            state = stepHijack(state);
            viewHijack(state, onscreenCanvas, offscreenCanvas);
            requestAnimationFrame(step);
        });
    });
});
