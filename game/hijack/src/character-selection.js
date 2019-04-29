function stepHijackModeCharacterSelection(state, input) {
    var pad0 = input[0];
    var pad1 = input[1];
    var pad0IsPressed = pad0 && state.i0 >= HIJACK_BUTTON_WAIT && pad0.buttons.some(function (button) { return button.pressed; });
    var pad1IsPressed = pad1 && state.i1 >= HIJACK_BUTTON_WAIT && pad1.buttons.some(function (button) { return button.pressed; });

    if (pad0IsPressed || pad1IsPressed) {
        state.i0 = 0;
        state.i1 = 0;

        state.mode = HIJACK_MODE_GAME;

        var character0 = state.selection0.character;
        var character1 = state.selection1.character;
        var stage = state.config.stages[0];

        state.x = 0;
        state.y = 0;

        state.character0 = {
            i: 0,
            id: 0,
            v: {
                x: 0,
                y: 0
            },
            odds: 50,
            x: -getHijackParameterX(character0.actions.neutral_right),
            y: getHijackParameterHeight(state.config) - getHijackParameterY(character0.actions.neutral_right) - getHijackParameterHeight(character0.actions.neutral_right) - HIJACK_FLOOR_HEIGHT,
            pose: "neutral",
            direction: "right",
            ate: new Set(),
            character: character0
        };

        state.character1 = {
            i: 0,
            id: 0,
            v: {
                x: 0,
                y: 0
            },
            odds: 50,
            x: getHijackParameterWidth(state.config) - getHijackParameterX(character0.actions.neutral_left) - getHijackParameterWidth(character0.actions.neutral_left),
            y: getHijackParameterHeight(state.config) - getHijackParameterY(character0.actions.neutral_left) - getHijackParameterHeight(character0.actions.neutral_left) - HIJACK_FLOOR_HEIGHT,
            pose: "neutral",
            direction: "left",
            ate: new Set(),
            character: character1
        };

        state.stage = {
            stage: stage
        };
    } else {
        var x0 = pad0 && state.i0 >= HIJACK_BUTTON_WAIT ? pad0.axes[0] : 0;
        var y0 = pad0 && state.i0 >= HIJACK_BUTTON_WAIT ? pad0.axes[1] : 0;
        var x1 = pad1 && state.i1 >= HIJACK_BUTTON_WAIT ? pad1.axes[0] : 0;
        var y1 = pad1 && state.i1 >= HIJACK_BUTTON_WAIT ? pad1.axes[1] : 0;

        if (x0 < -0.5) {
            var i = state.selection0.y * 4 + (state.selection0.x - 1);
            if (0 <= state.selection0.x - 1 && state.selection0.x - 1 < 4 && 0 <= i && i < state.config.characters.length) {
                --state.selection0.x;
            }
            state.i0 = -1;
        } else if (x0 > 0.5) {
            var i = state.selection0.y * 4 + (state.selection0.x + 1);
            if (0 <= state.selection0.x + 1 && state.selection0.x + 1 < 4 && 0 <= i && i < state.config.characters.length) {
                ++state.selection0.x;
            }
            state.i0 = -1;
        }

        if (y0 < -0.5) {
            var i = (state.selection0.y - 1) * 4 + state.selection0.x;
            if (0 <= state.selection0.y - 1 && state.selection0.y - 1 < 4 && 0 <= i && i < state.config.characters.length) {
                --state.selection0.y;
            }
            state.i0 = -1;
        } else if (y0 > 0.5) {
            var i = (state.selection0.y + 1) * 4 + state.selection0.x;
            if (0 <= state.selection0.y + 1 && state.selection0.y + 1 < 4 && 0 <= i && i < state.config.characters.length) {
                ++state.selection0.y;
            }
            state.i0 = -1;
        }

        if (x1 < -0.5) {
            var i = state.selection1.y * 4 + (state.selection1.x - 1);
            if (0 <= state.selection1.x - 1 && state.selection1.x - 1 < 4 && 0 <= i && i < state.config.characters.length) {
                --state.selection1.x;
            }
            state.i1 = -1;
        } else if (x1 > 0.5) {
            var i = state.selection1.y * 4 + (state.selection1.x + 1);
            if (0 <= state.selection1.x + 1 && state.selection1.x + 1 < 4 && 0 <= i && 0 <= i && i < state.config.characters.length) {
                ++state.selection1.x;
            }
            state.i1 = -1;
        }

        if (y1 < -0.5) {
            var i = (state.selection1.y - 1) * 4 + state.selection1.x;
            if (0 <= state.selection1.y - 1 && state.selection1.y - 1 < 4 && 0 <= i && 0 <= i && i < state.config.characters.length) {
                --state.selection1.y;
            }
            state.i1 = -1;
        } else if (y1 > 0.5) {
            var i = (state.selection1.y + 1) * 4 + state.selection1.x;
            if (0 <= state.selection1.y + 1 && state.selection1.y + 1 < 4 && 0 <= i && 0 <= i && i < state.config.characters.length) {
                ++state.selection1.y;
            }
            state.i1 = -1;
        }

        state.selection0.character = state.config.characters[state.selection0.y * 4 + state.selection0.x];
        state.selection1.character = state.config.characters[state.selection1.y * 4 + state.selection1.x];

        ++state.i0;
        ++state.i1;
    }

    return state;
}

function viewHijackModeCharacterSelection(state) {
    var views = [];

    for (var i = 0; i < state.config.characters.length; ++i) {
        var character = state.config.characters[i];
        var x = i % 4;
        var y = Math.floor(i / 4);

        views.push({
            type: "image",
            sx: 0,
            sy: 0,
            sw: 64,
            sh: 64,
            dx: 192 + x * 64,
            dy: 96 + y * 64,
            dw: 64,
            dh: 64,
            img: character.icon
        });

        if (x === state.selection0.x && y === state.selection0.y && x === state.selection1.x && y === state.selection1.y) {
            views.push({
                type: "image",
                sx: 0,
                sy: 0,
                sw: 64,
                sh: 64,
                dx: 192 + x * 64,
                dy: 96 + y * 64,
                dw: 64,
                dh: 64,
                img: state.config.icon_border2.image
            });
        } else if (x === state.selection0.x && y === state.selection0.y) {
            views.push({
                type: "image",
                sx: 0,
                sy: 0,
                sw: 64,
                sh: 64,
                dx: 192 + x * 64,
                dy: 96 + y * 64,
                dw: 64,
                dh: 64,
                img: state.config.icon_border0.image
            });
        } else if (x === state.selection1.x && y === state.selection1.y) {
            views.push({
                type: "image",
                sx: 0,
                sy: 0,
                sw: 64,
                sh: 64,
                dx: 192 + x * 64,
                dy: 96 + y * 64,
                dw: 64,
                dh: 64,
                img: state.config.icon_border1.image
            });
        } else {
            views.push({
                type: "image",
                sx: 0,
                sy: 0,
                sw: 64,
                sh: 64,
                dx: 192 + x * 64,
                dy: 96 + y * 64,
                dw: 64,
                dh: 64,
                img: state.config.icon_border.image
            });
        }
    }

    var portrait0 = state.selection0.character.portrait_right;
    var portrait1 = state.selection1.character.portrait_left;

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

    return views;
}
