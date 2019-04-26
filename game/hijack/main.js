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
var HIJACK_FLOOR_HEIGHT = 16;
var HIJACK_BUTTON_WAIT = 10;

function newHijack(config, onscreenCanvas, offscreenCanvas) {
    return {
        i0: 0,
        i1: 0,
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

        switch (view.type) {
        case "image":
            offscreenContext.drawImage(view.img, view.sx, view.sy, view.sw, view.sh, view.dx, view.dy, view.dw, view.dh);
            break;
        case "rect":
            var fillStyle = offscreenContext.fillStyle;
            offscreenContext.fillStyle = view.color;
            offscreenContext.fillRect(view.x, view.y, view.width, view.height);
            offscreenContext.fillStyle = fillStyle;
        }
    }

    var onscreenContext = state.onscreenCanvas.getContext("2d");
    onscreenContext.drawImage(state.offscreenCanvas, 0, 0);
}

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

const HIJACK_INPUT_EMPTY = {
    axes: [0, 0],
    buttons: [
        {pressed: false},
        {pressed: false},
        {pressed: false},
        {pressed: false},
        {pressed: false},
        {pressed: false}
    ]
};

function stepHijackModeGame(state, input) {
    if (state.character0.odds >= 100 || state.character1.odds >= 100) {
        state.i0 = 0;
        state.i1 = 0;
        state.mode = HIJACK_MODE_RESULT;
        return state;
    }

    var attacks0 = [];
    var attacks1 = [];
    var grabs0 = [];
    var grabs1 = [];

    input =
        input.length === 0
        ? [HIJACK_INPUT_EMPTY, HIJACK_INPUT_EMPTY]
        : input.length === 1
        ?  [input[0], HIJACK_INPUT_EMPTY]
        : input;

    stepCharacterCalc(0, state.character0, attacks0, grabs0, state.character1, attacks1, grabs1);
    stepCharacterCalc(1, state.character1, attacks1, grabs1, state.character0, attacks0, grabs0);

    if (state.i0 >= HIJACK_BUTTON_WAIT && state.i1 >= HIJACK_BUTTON_WAIT) {
        stepCharacterControl(0, state.character0, attacks0, grabs0, state.character1, attacks1, grabs1);
        stepCharacterControl(1, state.character1, attacks1, grabs1, state.character0, attacks0, grabs0);
    } else {
        ++state.i0;
        ++state.i1;
    }

    stepCharacterApply(0, state.character0, attacks0, grabs0, state.character1, attacks1, grabs1);
    stepCharacterApply(1, state.character1, attacks1, grabs1, state.character0, attacks0, grabs0);

    var action0 = state.character0.character.actions[state.character0.pose + "_" + state.character0.direction];
    var action1 = state.character1.character.actions[state.character1.pose + "_" + state.character1.direction];

    var left = Math.min(state.character0.x + getHijackParameterX(action0), state.character1.x + getHijackParameterX(action1));
    var right = Math.max(state.character0.x + getHijackParameterX(action0) + getHijackParameterWidth(action0), state.character1.x + getHijackParameterX(action1) + getHijackParameterWidth(action1));

    if (left < state.x) {
        state.x = left;
    }

    if (right > state.x + state.config.width) {
        state.x = right - state.config.width;
    }

    return state;

    function stepCharacterControl(i, character, characterAttacks, characterGrabs, enemy, enemyAttacks, enemyGrabs) {
        var x0 = input[i].axes[0];
        var y0 = input[i].axes[1];
        var button0 = input[i].buttons[0].pressed;
        var button1 = input[i].buttons[1].pressed;
        var button2 = input[i].buttons[2].pressed;
        var button3 = input[i].buttons[3].pressed;
        var button4 = input[i].buttons[4].pressed;
        var button5 = input[i].buttons[5].pressed;
        var action = character.character.actions[character.pose + "_" + character.direction];

        if (enemyAttacks.length > 0) {
            for (var i = 0; i < enemyAttacks.length; ++i) {
                if (!character.ate.has(enemyAttacks[i].id)) {
                    character.i = 0;
                    ++character.id;
                    character.pose = "be_attacked";
                    return;
                }
            }
        }

        if (characterGrabs.length > 0 && enemyGrabs.length === 0) {
            character.pose = "grabbed";
            return;
        }

        if (enemyGrabs.length > 0 && characterGrabs.length === 0) {
            character.pose = "be_grabbed";
            return;
        }

        switch (character.pose) {
        case "weak":
        case "strong":
            if (character.i < action.startup)
                ++character.i;
            else if (character.i < action.startup + action.active)
                ++character.i;
            else if (character.i < action.startup + action.active + action.recovery)
                ++character.i;
            else {
                character.i = 0;
                ++character.id;
                character.pose = "neutral";
            }
            break;
        case "short_jump":
            var n = Math.floor(action.animation.sprite_sheet.width / action.animation.width);

            if (character.i < n * action.animation.frames_per_sprite)
                ++character.i;
            else {
                character.i = 0;
                ++character.id;
                character.pose = "neutral";
            }
            break;
        case "full_jump":
            var n = Math.floor(action.animation.sprite_sheet.width / action.animation.width);

            if (character.i < n * action.animation.frames_per_sprite)
                ++character.i;
            else {
                character.i = 0;
                ++character.id;
                character.pose = "neutral";
            }
            break;
        case "fall":
            if (x0 < -0.5) {
                character.direction = "left";
            } else if (x0 > 0.5) {
                character.direction = "right";
            }

            ++character.i;
            break;
        case "shield":
            if (character.i < action.startup) {
                ++character.i;
                character.j = 0;
            } else if (button4) {
                ++character.i;
                character.j = 0;
            } else if (character.j < action.recovery) {
                ++character.i;
                ++character.j;
            } else {
                character.i = 0;
                character.j = 0;
                ++character.id;
                character.pose = "neutral";
            }

            break;
        case "grab":
            if (character.i < action.startup)
                ++character.i;
            else if (character.i < action.startup + action.active)
                ++character.i;
            else if (character.i < action.startup + action.active + action.recovery)
                ++character.i;
            else {
                character.i = 0;
                ++character.id;
                character.pose = "neutral";
            }

            break;
        case "be_attacked":
            // TODO
            var n = Math.floor(action.animation.sprite_sheet.width / action.animation.width);

            if (character.i < n * action.animation.frames_per_sprite)
                ++character.i;
            else {
                character.i = 0;
                ++character.id;
                character.pose = "neutral";
            }
            break;
        case "grabbed":
        case "be_grabbed":
            // TODO
            var n = Math.floor(action.animation.sprite_sheet.width / action.animation.width);

            if (character.i < n * action.animation.frames_per_sprite)
                ++character.i;
            else {
                character.i = 0;
                ++character.id;
                character.pose = "neutral";
            }
            break;
        default:
            if (button0) {
                character.i = 0;
                ++character.id;

                if (x0 < -0.5) {
                    character.pose = "weak";
                    character.direction = "left";
                } else if (x0 > 0.5) {
                    character.pose = "weak";
                    character.direction = "right";
                } else {
                    character.pose = "weak";
                }

                break;
            }

            if (button1) {
                character.i = 0;
                ++character.id;

                if (x0 < -0.5) {
                    character.pose = "strong";
                    character.direction = "left";
                } else if (x0 > 0.5) {
                    character.pose = "strong";
                    character.direction = "right";
                } else {
                    character.pose = "strong";
                }

                break;
            }

            if (button2) {
                character.i = 0;
                ++character.id;

                if (x0 < -0.5) {
                    character.pose = "short_jump";
                    character.direction = "left";
                } else if (x0 > 0.5) {
                    character.pose = "short_jump";
                    character.direction = "right";
                } else {
                    character.pose = "short_jump";
                }

                break;
            }

            if (button3) {
                character.i = 0;
                ++character.id;
                if (x0 < -0.5) {
                    character.pose = "full_jump";
                    character.direction = "left";
                } else if (x0 > 0.5) {
                    character.pose = "full_jump";
                    character.direction = "right";
                } else {
                    character.pose = "full_jump";
                }

                break;
            }

            if (button4) {
                character.i = 0;
                ++character.id;

                if (x0 < -0.5) {
                    character.pose = "shield";
                    character.direction = "left";
                } else if (x0 > 0.5) {
                    character.pose = "shield";
                    character.direction = "right";
                } else {
                    character.pose = "shield";
                }

                break;
            }

            if (button5) {
                character.i = 0;
                ++character.id;

                if (x0 < -0.5) {
                    character.pose = "grab";
                    character.direction = "left";
                } else if (x0 > 0.5) {
                    character.pose = "grab";
                    character.direction = "right";
                } else {
                    character.pose = "grab";
                }

                break;
            }

            if (x0 < -0.5) {
                if (character.pose === "run")
                    ++character.i;
                else {
                    character.i = 0;
                    ++character.id;
                }
                character.pose = "run";
                character.direction = "left";
            } else if (x0 > 0.5) {
                if (character.pose === "run")
                    ++character.i;
                else {
                    character.i = 0;
                    ++character.id;
                }
                character.pose = "run";
                character.direction = "right";
            } else {
                if (character.pose === "neutral")
                    ++character.i;
                else {
                    character.i = 0;
                    ++character.id;
                }
                character.pose = "neutral";
            }
        }

        if (character.y + getHijackParameterY(action) + getHijackParameterHeight(action) < getHijackParameterHeight(state.config) - HIJACK_FLOOR_HEIGHT) {
            if (character.pose === "fall") {
            } else if (character.pose === "short_jump") {
            } else if (character.pose === "full_jump") {
            } else {
                character.i = 0;
                ++character.id;
                character.pose = "fall";
            }

            if (x0 < -0.5) {
                character.v.x -= character.character.dexterity;
            } else if (x0 > 0.5) {
                character.v.x += character.character.dexterity;
            }

            if (y0 > 0.5) {
                character.v.y += character.character.dexterity;
            }

            character.v.y += character.character.gravity;

            if (character.v.x < 0) {
                character.v.x += character.character.resistance;
                character.v.x = Math.min(0, character.v.x);
            } else if (character.v.x > 0) {
                character.v.x -= character.character.resistance;
                character.v.x = Math.max(0, character.v.x);
            }
        } else if (character.pose === "be_attacked") {
            character.v.y += character.character.gravity;

            if (character.v.x < 0) {
                character.v.x += character.character.resistance;
                character.v.x = Math.min(0, character.v.x);
            } else if (character.v.x > 0) {
                character.v.x -= character.character.resistance;
                character.v.x = Math.max(0, character.v.x);
            }
        } else if (character.pose === "fall") {
            character.i = 0;
            ++character.id;
            character.pose = "neutral";
        }
    }

    function stepCharacterCalc(i, character, characterAttacks, characterGrabs, enemy, enemyAttacks, enemyGrabs) {
        var characterAction = character.character.actions[character.pose + "_" + character.direction];
        var enemyAction = enemy.character.actions[enemy.pose + "_" + enemy.direction];

        var characterRectangle = {
            x: character.x + getHijackParameterX(characterAction),
            y: character.y + getHijackParameterY(characterAction),
            width: getHijackParameterWidth(characterAction),
            height: getHijackParameterHeight(characterAction)
        };

        var enemyRectangle = {
            x: enemy.x + getHijackParameterX(enemyAction),
            y: enemy.y + getHijackParameterY(enemyAction),
            width: getHijackParameterWidth(enemyAction),
            height: getHijackParameterHeight(enemyAction)
        };

        switch (character.pose) {
        case "weak":
        case "strong":
            var characterAttack = {
                x: character.x + getHijackParameterX(characterAction.attack),
                y: character.y + getHijackParameterY(characterAction.attack),
                width: getHijackParameterWidth(characterAction.attack),
                height: getHijackParameterHeight(characterAction.attack),
                damage: characterAction.attack.damage,
                v: characterAction.attack.v
            };

            if (character.i <= characterAction.startup && character.i < characterAction.startup + characterAction.active && collision(characterAttack, enemyRectangle))
                characterAttacks.push({
                    id: character.id,
                    x: character.x + getHijackParameterX(characterAttack),
                    y: character.y + getHijackParameterY(characterAttack),
                    width: getHijackParameterWidth(characterAttack),
                    height: getHijackParameterHeight(characterAttack),
                    damage: characterAttack.damage,
                    v: characterAttack.v
                });
            break;
        case "grab":
            var characterGrab = {
                x: character.x + getHijackParameterX(characterAction.grab),
                y: character.y + getHijackParameterY(characterAction.grab),
                width: getHijackParameterWidth(characterAction.grab),
                height: getHijackParameterHeight(characterAction.grab)
            };

            if (character.i <= characterAction.startup && character.i < characterAction.startup + characterAction.active && collision(characterGrab, enemyRectangle))
                characterGrabs.push({
                    id: character.id,
                    x: character.x + getHijackParameterX(characterGrab),
                    y: character.y + getHijackParameterY(characterGrab),
                    width: getHijackParameterWidth(characterGrab),
                    height: getHijackParameterHeight(characterGrab)
                });
            break;
        }
    }

    function stepCharacterApply(i, character, characterAttacks, characterGrabs, enemy, enemyAttacks, enemyGrabs) {
        var x0 = input[i].axes[0];
        var y0 = input[i].axes[1];
        var action = character.character.actions[character.pose + "_" + character.direction];
        var enemyAction = enemy.character.actions[enemy.pose + "_" + enemy.direction];

        if (enemyAttacks.length > 0) {
            for (var i = 0; i < enemyAttacks.length; ++i) {
                if (!character.ate.has(enemyAttacks[i].id)) {
                    character.odds -= enemyAttacks[i].damage;
                    enemy.odds += enemyAttacks[i].damage;
                    character.v.x += enemyAttacks[i].v.x;
                    character.v.y += enemyAttacks[i].v.y;
                    character.ate.add(enemyAttacks[i].id);
                }
            }
        }

        switch (character.pose) {
        case "weak":
        case "strong":
            break;
        case "short_jump":
        case "full_jump":
            if (character.i === 0) {
                var v = {
                    x: getHijackParameterX(action.move),
                    y: getHijackParameterY(action.move)
                };

                if (x0 < -0.5) {
                    v.x += character.character.actions.run_left.move.x;
                    v.y += character.character.actions.run_left.move.y;
                } else if (x0 > 0.5) {
                    v.x += character.character.actions.run_right.move.x;
                    v.y += character.character.actions.run_right.move.y;
                }

                character.v.x += v.x;
                character.v.y += v.y;
            }
            break;
        case "fall":
            break;
        case "shield":
            break;
        case "grab":
            break;
        case "grabbed":
        case "be_grabbed":
            // TODO
            break;
        case "neutral":
            character.v = {
                x: 0,
                y: 0
            };
            break;
        case "run":
            if (character.i === 0) {
                var v = {
                    x: getHijackParameterX(action.move),
                    y: getHijackParameterY(action.move)
                };

                character.v.x += v.x;
                character.v.y += v.y;
            }
            break;
        default:
        }

        if (character.i % action.animation.frames_per_sprite === 0) {
            character.x += Math.round(character.v.x);
            character.y += Math.round(character.v.y);
        }

        var left = enemy.x + getHijackParameterX(enemyAction) + getHijackParameterWidth(enemyAction) - getHijackParameterWidth(state.config) - getHijackParameterX(action);
        var right = enemy.x + getHijackParameterX(enemyAction) + getHijackParameterWidth(state.config) - getHijackParameterX(action) - getHijackParameterWidth(action);

        character.x = Math.min(Math.max(left, character.x), right);
        character.y = Math.min(Math.max(-getHijackParameterY(action), character.y), getHijackParameterHeight(state.config) - getHijackParameterY(action) - getHijackParameterHeight(action) - HIJACK_FLOOR_HEIGHT);
    }
}

function stepHijackModeResult(state, input) {
    if (input.some(function (pad) { return pad.buttons.some(function (button) { return button.pressed; }); })) {
        state.mode = HIJACK_MODE_CHARACTER_SELECTION;
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
        dx: (getHijackParameterWidth(state.config) - state.config.logo.width * state.config.logo_scale) / 2,
        dy: (getHijackParameterHeight(state.config) - state.config.logo.height * state.config.logo_scale) / 2,
        dw: state.config.logo.width * state.config.logo_scale,
        dh: state.config.logo.height * state.config.logo_scale,
        img: state.config.logo
    }];

    return views;
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
                img: state.config.icon_border2
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
                img: state.config.icon_border0
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
                img: state.config.icon_border1
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
                img: state.config.icon_border
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

function viewHijackModeGame(state) {
    var views = [];

    viewStagePerspective(state.stage);
    viewStageBackground(state.stage);
    viewStageFloor(state.stage);
    viewStageForeground(state.stage);
    viewCharacter(state.character0);
    viewCharacter(state.character1);
    viewOdds(state.character0.odds, state.character1.odds);

    return views;

    function viewStagePerspective(stage) {
        var x = Math.floor((stage.stage.perspective.width - state.config.width) / 2);
        x += Math.round(state.x / 64);

        var dx = 0;

        while (dx < state.config.width) {
            var sx = (x < 0 ? Math.abs(stage.stage.perspective.width + x) : x) % stage.stage.perspective.width;
            var sw = stage.stage.perspective.width - sx;
            var dw = sw;

            views.push({
                type: "image",
                sx: sx,
                sy: 0,
                sw: sw,
                sh: state.config.height,
                dx: dx,
                dy: 0,
                dw: dw,
                dh: state.config.height,
                img: stage.stage.perspective.sprite_sheet
            });

            x += dw;
            dx += dw;
        }
    }

    function viewStageBackground(stage) {
        var x = Math.floor((stage.stage.background.width - state.config.width) / 2);
        x += Math.round(state.x / 8);

        var dx = 0;

        while (dx < state.config.width) {
            var sx = (x < 0 ? Math.abs(stage.stage.background.width + x) : x) % stage.stage.background.width;
            var sw = stage.stage.background.width - sx;
            var dw = sw;

            views.push({
                type: "image",
                sx: sx,
                sy: 0,
                sw: sw,
                sh: state.config.height,
                dx: dx,
                dy: 0,
                dw: dw,
                dh: state.config.height,
                img: stage.stage.background.sprite_sheet
            });

            x += dw;
            dx += dw;
        }
    }

    function viewStageFloor(stage) {
        var x = Math.floor((stage.stage.floor.width - state.config.width) / 2);
        x += state.x;

        var dx = -state.config.width;

        while (dx < state.config.width * 2) {
            var sx = (x < 0 ? Math.abs(stage.stage.floor.width + x) : x) % stage.stage.floor.width;
            var sw = stage.stage.floor.width - sx;

            var shift1 = dx - (state.config.width / 2);
            var shift2 = dx + sw - (state.config.width / 2);
            var theta = Math.atan2(state.config.height, shift1);
            var phi = Math.atan2(state.config.height, shift2);

            var n = stage.stage.floor.height;
            var h = Math.floor(stage.stage.floor.height / n);

            for (var i = 0; i < n; ++i) {
                var y = Math.floor(h * i);
                var x1 = (state.config.width / 2) - (h * i) / Math.tan(theta);
                var dw = (state.config.width / 2) - (h * i) / Math.tan(phi) - x1;

                views.push({
                    type: "image",
                    sx: sx,
                    sy: y,
                    sw: sw,
                    sh: h,
                    dx: x1,
                    dy: y,
                    dw: dw,
                    dh: h,
                    img: stage.stage.floor.sprite_sheet
                });
            }

            x += sw;
            dx += sw;
        }
    }

    function viewStageForeground(stage) {
    }

    function viewCharacter(character) {
        var id = character.pose + "_" + character.direction;
        var action = character.character.actions[id];
        var animation = action.animation;

        var n = Math.floor(animation.sprite_sheet.width / animation.width);

        views.push({
            type: "image",
            sx: Math.floor(character.i / animation.frames_per_sprite % n) * animation.width,
            sy: 0,
            sw: animation.width,
            sh: animation.height,
            dx: character.x + getHijackParameterX(animation) - state.x,
            dy: character.y + getHijackParameterY(animation) - state.y,
            dw: getHijackParameterWidth(animation),
            dh: getHijackParameterHeight(animation),
            img: animation.sprite_sheet
        });
    }

    function viewOdds(odds0, odds1) {
        var left = 10;

        views.push({
            type: "rect",
            x: 10,
            y: 10,
            width: odds0 * 6,
            height: 16,
            color: "rgba(255, 255, 255, 1)"
        });

        views.push({
            type: "rect",
            x: odds0 * 6 + 30,
            y: 10,
            width: odds1 * 6,
            height: 16,
            color: "rgba(255, 255, 255, 1)"
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

            if (typeof config.logo_scale !== "number") {
                console.error("config.logo_scale isn't a number: %o", config.logo_scale);
                reject();
            }

            if (typeof config.icon_border !== "string") {
                console.error("config.icon_border isn't a string: %o", i, config.icon_border);
                reject();
            }

            if (typeof config.icon_border0 !== "string") {
                console.error("config.icon_border0 isn't a string: %o", i, config.icon_border0);
                reject();
            }

            if (typeof config.icon_border1 !== "string") {
                console.error("config.icon_border1 isn't a string: %o", i, config.icon_border1);
                reject();
            }

            if (typeof config.icon_border2 !== "string") {
                console.error("config.icon_border2 isn't a string: %o", i, config.icon_border2);
                reject();
            }

            if (typeof config.width !== "number") {
                console.error("config.width isn't a number: %o", config.width);
                reject();
            }

            if (typeof config.height !== "number") {
                console.error("config.height isn't a number: %o", config.height);
                reject();
            }

            if (typeof config.scale !== "number") {
                console.error("config.scale isn't a number: %o", config.scale);
                reject();
            }

            Promise.all([
                loadImage(config.logo),
                loadImage(config.icon_border),
                loadImage(config.icon_border0),
                loadImage(config.icon_border1),
                loadImage(config.icon_border2)
            ]).then(function (xs) {
                var logo = xs[0];
                var icon_border = xs[1];
                var icon_border0 = xs[2];
                var icon_border1 = xs[3];
                var icon_border2 = xs[4];

                if (!Array.isArray(config.characters)) {
                    console.error("config.characters isn't an array: %o", config.characters);
                    return;
                }

                Promise.all([
                    Promise.all(config.characters.map(function (path, i) {
                        if (typeof path !== "string") {
                            console.error("config.characters[%o] isn't a string: %o", i, config.characters);
                            reject();
                        }

                        return fetch(path, {cache: "no-cache"}).then(function (character) { return character.json(); });
                    })),
                    Promise.all(config.stages.map(function (path, i) {
                        if (typeof path !== "string") {
                            console.error("config.stage[%o] isn't a string: %o", i, config.stages);
                            reject();
                        }

                        return fetch(path, {cache: "no-cache"}).then(function (stage) { return stage.json(); });
                    }))
                ]).then(function (xs) {
                    var characters = xs[0];
                    var stages = xs[1];
                    var promises0 = [];
                    var promises2 = [];

                    for (var i = 0; i < stages.length; ++i) {
                        var stage = stages[i];

                        var keys = ["foreground", "floor", "background", "perspective"];

                        for (var j = 0; j < keys.length; ++j) {
                            var key = keys[j];

                            if (typeof stage[key].width !== "number") {
                                console.error("stage[%o].width isn't a number: %o", key, stage.width);
                                reject();
                            }

                            if (typeof stage[key].height !== "number") {
                                console.error("stage[%o].height isn't a number: %o", key, stage.height);
                                reject();
                            }

                            if (typeof stage[key].frames_per_sprite !== "number") {
                                console.error("stage[%o].frames_per_sprite isn't a number: %o", key, stage.frames_per_sprite);
                                reject();
                            }

                            if (typeof stage[key].sprite_sheet !== "string") {
                                console.error("stage[%o].sprite_sheet isn't a string: %o", key, stage.sprite_sheet);
                                reject();
                            }
                        }

                        promises2.push(Promise.all([
                            loadImage(stage.foreground.sprite_sheet),
                            loadImage(stage.floor.sprite_sheet),
                            loadImage(stage.background.sprite_sheet),
                            loadImage(stage.perspective.sprite_sheet)
                        ]).then(function (sprite_sheets) {
                            return {
                                foreground: {
                                    width: stage.foreground.width,
                                    height: stage.foreground.height,
                                    frames_per_sprite: stage.foreground.frames_per_sprite,
                                    sprite_sheet: sprite_sheets[0]
                                },
                                floor: {
                                    width: stage.floor.width,
                                    height: stage.floor.height,
                                    frames_per_sprite: stage.floor.frames_per_sprite,
                                    sprite_sheet: sprite_sheets[1]
                                },
                                background: {
                                    width: stage.background.width,
                                    height: stage.background.height,
                                    frames_per_sprite: stage.background.frames_per_sprite,
                                    sprite_sheet: sprite_sheets[2]
                                },
                                perspective: {
                                    width: stage.perspective.width,
                                    height: stage.perspective.height,
                                    frames_per_sprite: stage.perspective.frames_per_sprite,
                                    sprite_sheet: sprite_sheets[3]
                                }
                            };
                        }));
                    }

                    for (var i = 0; i < characters.length; ++i) {
                        var character = characters[i];

                        if (typeof character.gravity !== "number") {
                            console.error("character.gravity isn't a number: %o", character.gravity);
                            reject();
                        }

                        if (typeof character.resistance !== "number") {
                            console.error("character.resistance isn't a number: %o", character.resistance);
                            reject();
                        }

                        if (typeof character.dexterity !== "number") {
                            console.error("character.dexterity isn't a number: %o", character.dexterity);
                            reject();
                        }

                        if (typeof character.icon !== "string") {
                            console.error("character.icon isn't a string: %o", character.icon);
                            reject();
                        }

                        if (typeof character.portrait_left !== "string") {
                            console.error("character.portrait_left isn't a string: %o", character.portrait_left);
                            reject();
                        }

                        if (typeof character.portrait_right !== "string") {
                            console.error("character.portrait_right isn't a string: %o", character.portrait_right);
                            reject();
                        }

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

                            if (typeof animation.x !== "number") {
                                console.error("animation.x isn't a number: %o", animation.x);
                                reject();
                            }

                            if (typeof animation.y !== "number") {
                                console.error("animation.y isn't a number: %o", animation.y);
                                reject();
                            }

                            if (typeof animation.width !== "number") {
                                console.error("animation.width isn't a number: %o", animation.width);
                                reject();
                            }

                            if (typeof animation.height !== "number") {
                                console.error("animation.height isn't a number: %o", animation.height);
                                reject();
                            }

                            if (typeof animation.scale !== "number") {
                                console.error("animation.scale isn't a number: %o", animation.scale);
                                reject();
                            }

                            if (typeof animation.frames_per_sprite !== "number") {
                                console.error("animation.frames_per_sprite isn't a number: %o", animation.frames_per_sprite);
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
                                        x: animation.x,
                                        y: animation.y,
                                        width: animation.width,
                                        height: animation.height,
                                        scale: animation.scale,
                                        frames_per_sprite: animation.frames_per_sprite,
                                        sprite_sheet: sprite_sheet
                                    };
                                }));
                            }(prop, animation));
                        }

                        promises0.push(Promise.all([loadImage(character.icon), loadImage(character.portrait_left), loadImage(character.portrait_right), Promise.all(promises1)]).then(function (xs) {
                            var icon = xs[0];
                            var portrait_left = xs[1];
                            var portrait_right = xs[2];
                            var _animations = xs[3];
                            var animations = {};

                            for (var j = 0; j < _animations.length; ++j)
                                animations[_animations[j].id] = _animations[j];

                            var actions = {};

                            for (var prop in character.actions) {
                                var action = character.actions[prop];

                                if (action == null) {
                                    console.error("character.actions[%o] is %o", prop, character.actions[prop]);
                                }

                                if (typeof action.x !== "number") {
                                    console.error("action.x isn't a number: %o", action.x);
                                    reject();
                                }

                                if (typeof action.y !== "number") {
                                    console.error("action.y isn't a number: %o", action.y);
                                    reject();
                                }

                                if (typeof action.width !== "number") {
                                    console.error("action.width isn't a number: %o", action.width);
                                    reject();
                                }

                                if (typeof action.height !== "number") {
                                    console.error("action.height isn't a number: %o", action.height);
                                    reject();
                                }

                                if (typeof action.animation !== "string") {
                                    console.error("action.animation isn't a string: %o", action.animation);
                                    reject();
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
                                gravity: character.gravity,
                                resistance: character.resistance,
                                dexterity: character.dexterity,
                                icon: icon,
                                portrait_left: portrait_left,
                                portrait_right: portrait_right,
                                actions: actions,
                                animations: animations
                            };
                        }));
                    }

                    Promise.all([Promise.all(promises0), Promise.all(promises2)]).then(function (xs) {
                        var characters = xs[0];
                        var stages = xs[1];

                        resolve({
                            logo: logo,
                            icon_border: icon_border,
                            icon_border0: icon_border0,
                            icon_border1: icon_border1,
                            icon_border2: icon_border2,
                            logo_scale: config.logo_scale,
                            width: config.width,
                            height: config.height,
                            scale: config.scale,
                            characters: characters,
                            stages
                        });
                    });
                });
            });
        });
    });
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

function collision(r0, r1) {
    var left = Math.max(r0.x, r1.x);
    var top = Math.max(r0.y, r1.y);
    var right = Math.min(r0.x + r0.width, r1.x + r1.width);
    var bottom = Math.min(r0.y + r0.height, r1.y + r1.height);
    var width = right - left;
    var height = bottom - top;
    return width > 0 && height > 0;
}

function vec2norm(v) {
    return Math.sqrt(v.x * v.x + v.y * v.y);
}

function vec2normalize(v) {
    var norm = vec2norm(v);

    return {
        x: v.x / norm,
        y: v.y / norm
    };
}
