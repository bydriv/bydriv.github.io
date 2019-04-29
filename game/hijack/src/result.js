function stepHijackModeResult(state, input) {
    var pad0 = input[0];
    var pad1 = input[1];
    var pad0IsPressed = state.i0 >= HIJACK_BUTTON_WAIT && pad0.buttons.some(function (button) { return button.pressed; });
    var pad1IsPressed = state.i1 >= HIJACK_BUTTON_WAIT && pad1.buttons.some(function (button) { return button.pressed; });

    if (pad0IsPressed || pad1IsPressed) {
        state.i0 = 0;
        state.i1 = 0;

        state.mode = HIJACK_MODE_CHARACTER_SELECTION;
    } else {
        stepHijackModeGame1(state, []);
        ++state.i0;
        ++state.i1;
    }

    return state;
}

function viewHijackModeResult(state) {
    var views = viewHijackModeGame(state);

    views.push({
        type: "rect",
        x: 0,
        y: 0,
        width: state.config.width,
        height: state.config.height,
        color: "rgba(0, 0, 0, 0.5)"
    });

    if (state.character0.odds >= 100) {
        var character0 = state.selection0.character;
        var portrait0 = state.selection0.character.portrait_right;

        views.push({
            type: "image",
            sx: 0,
            sy: 0,
            sw: portrait0.width,
            sh: portrait0.height,
            dx: 0,
            dy: 0,
            dw: portrait0.width,
            dh: portrait0.height,
            img: portrait0
        });

        views.push({
            type: "text",
            x: 192,
            y: 320,
            width: 256,
            height: 160,
            color: "rgba(255, 255, 255, 1)",
            fontSize: 16,
            fontFamily: "Junicode",
            message: character0.result_messages[Math.floor(Math.random() * character0.result_messages.length)]
        });
    } else if (state.character1.odds >= 100) {
        var character1 = state.selection1.character;
        var portrait1 = state.selection1.character.portrait_left;

        views.push({
            type: "image",
            sx: 0,
            sy: 0,
            sw: portrait1.width,
            sh: portrait1.height,
            dx: state.config.width - portrait1.width,
            dy: 0,
            dw: portrait1.width,
            dh: portrait1.height,
            img: portrait1
        });

        views.push({
            type: "text",
            x: 192,
            y: 320,
            width: 256,
            height: 160,
            color: "rgba(255, 255, 255, 1)",
            fontSize: 16,
            fontFamily: "Junicode",
            message: character1.result_messages[Math.floor(Math.random() * character1.result_messages.length)]
        });
    }

    return views;
}
