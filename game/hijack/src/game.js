var HIJACK_INPUT_EMPTY = {
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

    return stepHijackModeGame1(state, input);
}

function stepHijackModeGame1(state, input) {
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
            var n = Math.floor(character.character.animations[action.animation].sprite_sheet.width / character.character.animations[action.animation].width);
            if (character.i < n * character.character.animations[action.animation].frames_per_sprite)
                ++character.i;
            /*if (character.i < action.startup)
                ++character.i;
            else if (character.i < action.startup + action.active)
                ++character.i;
            else if (character.i < action.startup + action.active + action.recovery)
                ++character.i;*/
            else {
                character.i = 0;
                ++character.id;
                character.pose = "neutral";
            }
            break;
        case "short_jump":
            var n = Math.floor(character.character.animations[action.animation].sprite_sheet.width / character.character.animations[action.animation].width);

            if (character.i < n * character.character.animations[action.animation].frames_per_sprite)
                ++character.i;
            else {
                character.i = 0;
                ++character.id;
                character.pose = "neutral";
            }
            break;
        case "full_jump":
            var n = Math.floor(character.character.animations[action.animation].sprite_sheet.width / character.character.animations[action.animation].width);

            if (character.i < n * character.character.animations[action.animation].frames_per_sprite)
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
            var n = Math.floor(character.character.animations[action.animation].sprite_sheet.width / character.character.animations[action.animation].width);

            if (character.i < n * character.character.animations[action.animation].frames_per_sprite)
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
            var n = Math.floor(character.character.animations[action.animation].sprite_sheet.width / character.character.animations[action.animation].width);

            if (character.i < n * character.character.animations[action.animation].frames_per_sprite)
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
                id: character.id,
                x: character.x + getHijackParameterX(characterAction.attack),
                y: character.y + getHijackParameterY(characterAction.attack),
                width: getHijackParameterWidth(characterAction.attack),
                height: getHijackParameterHeight(characterAction.attack),
                damage: characterAction.attack.damage,
                v: characterAction.attack.v
            };

            if (character.i >= characterAction.startup && character.i < characterAction.startup + characterAction.active && collision(characterAttack, enemyRectangle))
                characterAttacks.push(characterAttack);
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

        if (character.i % character.character.animations[action.animation].frames_per_sprite === 0) {
            if (character.pose !== "strong") // TODO: ad-hoc
            {
                character.x += Math.round(character.v.x);
                character.y += Math.round(character.v.y);
            }
        }

        var left = enemy.x + getHijackParameterX(enemyAction) + getHijackParameterWidth(enemyAction) - getHijackParameterWidth(state.config) - getHijackParameterX(action);
        var right = enemy.x + getHijackParameterX(enemyAction) + getHijackParameterWidth(state.config) - getHijackParameterX(action) - getHijackParameterWidth(action);

        character.x = Math.min(Math.max(left, character.x), right);
        character.y = Math.min(Math.max(-getHijackParameterY(action), character.y), getHijackParameterHeight(state.config) - getHijackParameterY(action) - getHijackParameterHeight(action) - HIJACK_FLOOR_HEIGHT);
    }
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
        var animation = character.character.animations[action.animation];

        var n = Math.floor(animation.sprite_sheet.width / animation.width);

        views.push({
            type: "image",
            sx: Math.floor(character.i / animation.frames_per_sprite) % n * animation.width,
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
