function stepHijackModeTitle(state, input) {
    var called = false;

    whenAnyButton(state, input, 0, function () {
        called = true;
    });

    whenAnyButton(state, input, 1, function () {
        called = true;
    });

    if (called) {
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
