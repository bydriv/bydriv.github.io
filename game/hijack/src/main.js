var HIJACK_MODE_TITLE = "title";
var HIJACK_MODE_CHARACTER_SELECTION = "characterSelection";
var HIJACK_MODE_GAME = "game";
var HIJACK_MODE_RESULT = "result";
var HIJACK_FLOOR_HEIGHT = 16;

function newHijack(config, onscreenCanvas, offscreenCanvas) {
    return {
        i0: 0,
        i1: 0,
        waitLeftStick: [{}, {}],
        waitButton: [[], []],
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
    switch (state.mode) {
    case HIJACK_MODE_TITLE:
        return viewHijackModeTitle(state);
    case HIJACK_MODE_CHARACTER_SELECTION:
        return viewHijackModeCharacterSelection(state);
    case HIJACK_MODE_GAME:
        return viewHijackModeGame(state);
    case HIJACK_MODE_RESULT:
        return viewHijackModeResult(state);
    }
}

function drawViews(canvas, context, views) {
    context.fillRect(0, 0, canvas.width, canvas.height);

    for (var i = 0; i < views.length; ++i) {
        var view = views[i];

        switch (view.type) {
        case "image":
            context.drawImage(view.img, view.sx, view.sy, view.sw, view.sh, view.dx, view.dy, view.dw, view.dh);
            break;
        case "rect":
            context.save();
            context.fillStyle = view.color;
            context.fillRect(view.x, view.y, view.width, view.height);
            context.restore();
            break;
        case "text":
            context.save();
            context.fillStyle = view.color;
            context.font = view.fontSize.toString() + "px " + view.fontFamily;
            context.fillText(view.message, view.x, view.y, view.width);
            context.restore();
            break;
        }
    }
}

function getHijackParameterX(position) {
    return position.x * position.scale;
}

function getHijackParameterY(position) {
    return position.y * position.scale;
}

function getHijackParameterWidth(rectangle) {
    return rectangle.width * rectangle.scale;
}

function getHijackParameterHeight(rectangle) {
    return rectangle.height * rectangle.scale;
}

window.addEventListener("load", function () {
    return loadConfig("config.json").then(function (config) {
        var mode = document.getElementById("mode");

        var recording = false;
        var recorder = null;

        var onscreenCanvas = document.getElementById("hijack");
        var offscreenCanvas = document.createElement("canvas");
        var offscreenContext = offscreenCanvas.getContext("2d");
        var onscreenContext = onscreenCanvas.getContext("2d");
        offscreenCanvas.width = onscreenCanvas.width;
        offscreenCanvas.height = onscreenCanvas.height;
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

            var input = getInput(Array.from(navigator.getGamepads()).filter(function (pad) { return pad != null; }));
            state = stepHijack(state, input);
            var views = viewHijack(state);
            drawViews(offscreenCanvas, offscreenContext, views);
            onscreenContext.drawImage(offscreenCanvas, 0, 0);
            requestAnimationFrame(step);
        });
    });
});
