function stepHijackModeTitle(state, input) {
    var pad0 = input[0];
    var pad1 = input[1];
    var pad0IsPressed = pad0 && state.i0 >= HIJACK_BUTTON_WAIT && pad0.buttons.some(function (button) { return button.pressed; });
    var pad1IsPressed = pad1 && state.i1 >= HIJACK_BUTTON_WAIT && pad1.buttons.some(function (button) { return button.pressed; });

    if (pad0IsPressed || pad1IsPressed) {
        state.i0 = 0;
        state.i1 = 0;

        state.mode = HIJACK_MODE_CHARACTER_SELECTION;

        state.selection0 =
            {
                x: 0,
                y: 0,
                character: state.config.characters[0]
            };

        state.selection1 =
            {
                x: 0,
                y: 0,
                character: state.config.characters[0]
            };
    } else {
        ++state.i0;
        ++state.i1;
    }

    return state;
}

function viewHijackModeTitle(state) {
    var views = [{
        type: "image",
        sx: 0,
        sy: 0,
        sw: state.config.logo.width,
        sh: state.config.logo.height,
        dx: (getHijackParameterWidth(state.config) - state.config.logo.width * state.config.logo.scale) / 2,
        dy: (getHijackParameterHeight(state.config) - state.config.logo.height * state.config.logo.scale) / 2,
        dw: state.config.logo.width * state.config.logo.scale,
        dh: state.config.logo.height * state.config.logo.scale,
        img: state.config.logo.image
    }];

    return views;
}
