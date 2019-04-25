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
    }

    return state;
}

function stepHijackModeGame(state, input) {
    if (state.character0.odds >= 100 || state.character1.odds >= 100) {
        state.mode = HIJACK_MODE_RESULT;
        return state;
    }

    var attacks0 = [];
    var attacks1 = [];
    var grabs0 = [];
    var grabs1 = [];

    switch (input.length) {
    case 0:
        break;
    case 1:
        stepCharacterCalc(0, state.character0, attacks0, grabs0, state.character1, attacks1, grabs1);
        stepCharacterControl(0, state.character0, attacks0, grabs0, state.character1, attacks1, grabs1);
        stepCharacterApply(0, state.character0, attacks0, grabs0, state.character1, attacks1, grabs1);
        break;
    default:
        stepCharacterCalc(0, state.character0, attacks0, grabs0, state.character1, attacks1, grabs1);
        stepCharacterCalc(1, state.character1, attacks1, grabs1, state.character0, attacks0, grabs0);
        stepCharacterControl(0, state.character0, attacks0, grabs0, state.character1, attacks1, grabs1);
        stepCharacterControl(1, state.character1, attacks1, grabs1, state.character0, attacks0, grabs0);
        stepCharacterApply(0, state.character0, attacks0, grabs0, state.character1, attacks1, grabs1);
        stepCharacterApply(1, state.character1, attacks1, grabs1, state.character0, attacks0, grabs0);
        break;
    }

    var action0 = state.character0.character.actions[state.character0.pose + "_" + state.character0.direction];
    var action1 = state.character1.character.actions[state.character1.pose + "_" + state.character1.direction];

    var left = Math.min(state.character0.x + getHijackParameterX(action0), state.character1.x + getHijackParameterX(action1));
    var right = Math.max(state.character0.x + getHijackParameterX(action0) + getHijackParameterWidth(action0), state.character1.x + getHijackParameterX(action1) + getHijackParameterWidth(action1));

    if (left > state.x) {
        state.x = left;
    }

    if (right < state.x + state.config.width) {
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

            loadImage(config.logo).then(function (logo) {
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
