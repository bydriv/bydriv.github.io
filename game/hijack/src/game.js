function onGround(state, player) {
    var playerAction = player.character.actions[player.pose + "_" + player.direction];

    return !(player.y + getHijackParameterY(playerAction) + getHijackParameterHeight(playerAction) < getHijackParameterHeight(state.config) - HIJACK_FLOOR_HEIGHT);
}

function poseAttacked(player) {
    if (-32 <= player.v.x && player.v.x <= 32) {
        if (-32 <= player.v.y && player.v.y <= 32) {
            return "be_attacked";
        } else {
            return "be_attacked_bottom";
        }
    } else {
        return "be_attacked_top";
    }
}

function judgeHijackModeGame(state) {
    return state.player0.odds >= 100 || state.player1.odds >= 100;
}

function effectHijackModeGame(state, input) {
    var effect0 = effectHijackModeGamePlayer(state, input, 0, state.player0, state.player1);
    var effect1 = effectHijackModeGamePlayer(state, input, 1, state.player1, state.player0);
    return [effect0, effect1];
}

function effectHijackModeGamePlayer(state, input, i, player, opponent) {
    var effect = [];

    var pad = getPad(state, input, i);

    var playerAction = player.character.actions[player.pose + "_" + player.direction];
    var opponentAction = opponent.character.actions[opponent.pose + "_" + opponent.direction];

    var animation = player.character.animations[playerAction.animation];
    var sprite_count = Math.floor(animation.sprite_sheet.width / animation.width);
    var total_frames = sprite_count * animation.frames_per_sprite;

    switch (player.pose) {
    case "neutral":
        if (opponent.x < player.x) {
            effect.push({
                type: "direction",
                direction: "left"
            });
        } else if (opponent.x > player.x) {
            effect.push({
                type: "direction",
                direction: "right"
            });
        }

        run();
        jump();
        groundAttack();
        grab();
        shield();

        break;
    case "run":
        if (-0.5 <= input[i].axes[0] && input[i].axes[0] <= 0.5) {
            neutral();
        }

        jump();
        dashAttack();

        break;
    case "hop":
    case "jump":
        if (state.i - player.i < total_frames) {
            airAttack();
            break;
        }

        neutral();

        break;
    case "fall":
        airAttack();
        break;
    case "light_ground_attack":
    case "medium_ground_attack":
    case "hard_ground_attack":
    case "light_air_attack":
    case "medium_air_attack":
    case "hard_air_attack":
        if (state.i - player.i < total_frames) {
            var j = state.i - player.i;

            if (j < playerAction.startup) {
                /* pass */
            } else if (j < playerAction.startup + playerAction.active) {
                effect.push({
                    type: "attack",
                    attack: {
                        id: player.id,
                        x: player.x + getHijackParameterX(playerAction.attack),
                        y: player.y + getHijackParameterY(playerAction.attack),
                        width: getHijackParameterWidth(playerAction.attack),
                        height: getHijackParameterHeight(playerAction.attack),
                        damage: playerAction.attack.damage,
                        v: playerAction.attack.v
                    }
                });
            } else if (j < playerAction.startup + playerAction.active + playerAction.recovery) {
                /* pass */
            }

            break;
        }

        effect.push({
            type: "id"
        });

        neutral();

        break;
    case "ground_grab":
        if (state.i - player.i < total_frames) {
            var j = state.i - player.i;

            if (j < playerAction.startup) {
                /* pass */
            } else if (j < playerAction.startup + playerAction.active) {
                effect.push({
                    type: "grab",
                    grab: {
                        id: player.id,
                        x: player.x + getHijackParameterX(playerAction.grab),
                        y: player.y + getHijackParameterY(playerAction.grab),
                        width: getHijackParameterWidth(playerAction.grab),
                        height: getHijackParameterHeight(playerAction.grab),
                        damage: playerAction.grab.damage,
                        v: playerAction.grab.v
                    }
                });

            } else if (j < playerAction.startup + playerAction.active + playerAction.recovery) {
                throw_();

                /* pass */
            }

            break;
        }

        throw_();

        effect.push({
            type: "id"
        });

        neutral();

        break;
    case "light_ground_throw":
    case "medium_ground_throw":
    case "hard_ground_throw":
        if (state.i - player.i < total_frames) {
            effect.push({
                type: "throw",
                "throw": {
                    id: player.id,
                    damage: playerAction["throw"].damage,
                    v: playerAction["throw"].v
                }
            });

            break;
        }

        effect.push({
            type: "id"
        });

        neutral();

        break;
    case "shield":
        var j = state.i - player.i;

        if (j < playerAction.startup) {
            effect.push({
                type: "shield",
                shield: false
            });
        } else if (input[i].buttons[4].pressed) {
            effect.push({
                type: "shield",
                shield: true,
                startup: playerAction.startup
            });
        } else if (j < playerAction.startup + playerAction.recovery) {
            effect.push({
                type: "shield",
                shield: false
            });
        } else {
            neutral();
        }

        break;
    case "be_attacked":
    case "be_attacked_top":
    case "be_attacked_bottom":
    case "be_grabbed":
    case "be_knockdown":
        if (opponent.x < player.x) {
            effect.push({
                type: "direction",
                direction: "left"
            });
        } else if (opponent.x > player.x) {
            effect.push({
                type: "direction",
                direction: "right"
            });
        }

        if (state.i - player.i < playerAction.recovery) {
            break;
        }

        effect.push({
            type: "pose",
            pose: "neutral"
        });

        break;
    case "be_knockout":
        break;
    }

    return effect;

    function neutral() {
        effect.push({
            type: "pose",
            pose: "neutral"
        });

        effect.push({
            type: "reset_vector",
            v: {
                x: 0,
                y: 0
            }
        });
    }

    function run() {
        if (pad.axes[0] < -0.5) {
            effect.push({
                type: "pose",
                pose: "run"
            });

            effect.push({
                type: "direction",
                direction: "left"
            });

            effect.push({
                type: "add_vector",
                v: {
                    x: getHijackParameterX(player.character.actions.run_left.move),
                    y: getHijackParameterY(player.character.actions.run_left.move)
                }
            });
        } else if (pad.axes[0] > 0.5) {
            effect.push({
                type: "pose",
                pose: "run"
            });

            effect.push({
                type: "direction",
                direction: "right"
            });

            effect.push({
                type: "add_vector",
                v: {
                    x: getHijackParameterX(player.character.actions.run_right.move),
                    y: getHijackParameterY(player.character.actions.run_right.move)
                }
            });
        }
    }

    function jump() {
        if (pad.buttons[3].pressed) {
            if (input[i].axes[1] < -0.5) {
                effect.push({
                    type: "pose",
                    pose: "jump"
                });

                effect.push({
                    type: "add_vector",
                    v: {
                        x: getHijackParameterX(player.character.actions["jump_" + player.direction].move),
                        y: getHijackParameterY(player.character.actions["jump_" + player.direction].move)
                    }
                });
            } else {
                effect.push({
                    type: "pose",
                    pose: "hop"
                });

                effect.push({
                    type: "add_vector",
                    v: {
                        x: getHijackParameterX(player.character.actions["hop_" + player.direction].move),
                        y: getHijackParameterY(player.character.actions["hop_" + player.direction].move)
                    }
                });
            }

            if (input[i].axes[0] < -0.5 && -0.5 <= pad.axes[0] && pad.axes[0] <= 0.5) {
                effect.push({
                    type: "add_vector",
                    v: {
                        x: getHijackParameterX(player.character.actions.run_left.move),
                        y: getHijackParameterY(player.character.actions.run_left.move)
                    }
                });
            } else if (input[i].axes[0] > 0.5 && -0.5 <= pad.axes[0] && pad.axes[0] <= 0.5) {
                effect.push({
                    type: "add_vector",
                    v: {
                        x: getHijackParameterX(player.character.actions.run_right.move),
                        y: getHijackParameterY(player.character.actions.run_right.move)
                    }
                });
            }
        }
    }

    function groundAttack() {
        if (pad.buttons[0].pressed) {
            effect.push({
                type: "pose",
                pose: "light_ground_attack"
            });
        } else if (pad.buttons[1].pressed) {
            effect.push({
                type: "pose",
                pose: "medium_ground_attack"
            });
        } else if (pad.buttons[2].pressed) {
            effect.push({
                type: "pose",
                pose: "hard_ground_attack"
            });
        }
    }

    function dashAttack() {
        if (pad.buttons[0].pressed) {
            effect.push({
                type: "pose",
                pose: "light_ground_attack"
            });

            effect.push({
                type: "reset_vector",
                v: {
                    x: 0,
                    y: 0
                }
            });
        } else if (pad.buttons[1].pressed) {
            effect.push({
                type: "pose",
                pose: "medium_ground_attack"
            });

            effect.push({
                type: "reset_vector",
                v: {
                    x: 0,
                    y: 0
                }
            });
        } else if (pad.buttons[2].pressed) {
            effect.push({
                type: "pose",
                pose: "hard_ground_attack"
            });

            effect.push({
                type: "reset_vector",
                v: {
                    x: 0,
                    y: 0
                }
            });
        }
    }

    function airAttack() {
        if (pad.buttons[0].pressed) {
            effect.push({
                type: "pose",
                pose: "light_air_attack"
            });
        } else if (pad.buttons[1].pressed) {
            effect.push({
                type: "pose",
                pose: "medium_air_attack"
            });
        } else if (pad.buttons[2].pressed) {
            effect.push({
                type: "pose",
                pose: "hard_air_attack"
            });
        }
    }

    function shield() {
        if (pad.buttons[4].pressed) {
            effect.push({
                type: "pose",
                pose: "shield"
            });
        }
    }

    function grab() {
        if (pad.buttons[5].pressed) {
            effect.push({
                type: "pose",
                pose: "ground_grab"
            });
        }
    }

    function throw_() {
        if (input[i].buttons[0].pressed) {
            effect.push({
                type: "pose",
                pose: "light_ground_throw"
            });
            return true;
        } else if (input[i].buttons[1].pressed) {
            effect.push({
                type: "pose",
                pose: "medium_ground_throw"
            });
            return true;
        } else if (input[i].buttons[2].pressed) {
            effect.push({
                type: "pose",
                pose: "hard_ground_throw"
            });
            return true;
        }
        return false;
    }
}

function resolveHijackModeGame(state, effect) {
    resolveHijackModeGamePlayer(state, effect[0], effect[1], 0, state.player0, state.player1);
    resolveHijackModeGamePlayer(state, effect[1], effect[0], 1, state.player1, state.player0);

    var action0 = state.player0.character.actions[state.player0.pose + "_" + state.player0.direction];
    var action1 = state.player1.character.actions[state.player1.pose + "_" + state.player1.direction];
    var animation0 = state.player0.character.animations[action0.animation];
    var animation1 = state.player1.character.animations[action1.animation];

    if ((state.i - state.player0.i) % animation0.frames_per_sprite === 0 || (state.i - state.player1.i) % animation1.frames_per_sprite === 0) {
        var left = Math.min(state.player0.x + getHijackParameterX(action0), state.player1.x + getHijackParameterX(action1));
        var right = Math.max(state.player0.x + getHijackParameterX(action0) + getHijackParameterWidth(action0), state.player1.x + getHijackParameterX(action1) + getHijackParameterWidth(action1));

        if (left < state.x) {
            state.x = left;
        }

        if (right > state.x + state.config.width) {
            state.x = right - state.config.width;
        }
    }
}

function resolveHijackModeGamePlayer(state, playerEffect, opponentEffect, i, player, opponent) {
    var playerAction = player.character.actions[player.pose + "_" + player.direction];
    var opponentAction = opponent.character.actions[opponent.pose + "_" + opponent.direction];
    var playerAnimation = player.character.animations[playerAction.animation];
    var opponentAnimation = opponent.character.animations[opponentAction.animation];

    player.shield = false;

    for (var i = 0; i < playerEffect.length; ++i) {
        var eff = playerEffect[i];

        switch (eff.type) {
        case "id":
            ++player.id;
            break;
        case "pose":
            if (player.pose !== eff.pose) {
                player.i = state.i;
                player.pose = eff.pose;
            }
            break;
        case "direction":
            player.direction = eff.direction;
            break;
        case "attack":
            break;
        case "grab":
            break;
        case "throw":
            break;
        case "shield":
            player.shield = eff.shield;
            if (eff.shield) {
                player.i = state.i - eff.startup;
            }
            break;
        case "reset_vector":
            player.v = eff.v;
            break;
        case "add_vector":
            player.v.x += eff.v.x;
            player.v.y += eff.v.y;
            break;
        }
    }

    for (var i = 0; i < opponentEffect.length; ++i) {
        var eff = opponentEffect[i];

        switch (eff.type) {
        case "id":
            break;
        case "pose":
            break;
        case "direction":
            break;
        case "attack":
            var r = {
                x: player.x + getHijackParameterX(playerAction),
                y: player.y + getHijackParameterY(playerAction),
                width: getHijackParameterWidth(playerAction),
                height: getHijackParameterHeight(playerAction)
            };

            if (!player.ateAttack.has(eff.attack.id) && collision(eff.attack, r) && !player.shield) {
                player.ateAttack.add(eff.attack.id);
                player.i = state.i;
                player.v.x += eff.attack.v.x;
                player.v.y += eff.attack.v.y;
                player.pose = poseAttacked(player);
                player.odds -= eff.attack.damage;
                player.odds = Math.max(0, player.odds);
                opponent.odds += eff.attack.damage;
                opponent.odds = Math.min(100, opponent.odds);
            }

            break;
        case "grab":
            var r = {
                x: player.x + getHijackParameterX(playerAction),
                y: player.y + getHijackParameterY(playerAction),
                width: getHijackParameterWidth(playerAction),
                height: getHijackParameterHeight(playerAction)
            };

            if (!player.ateGrab.has(eff.grab.id) && collision(eff.grab, r)) {
                player.ateGrab.add(eff.grab.id);
                player.i = state.i;
                player.pose = "be_grabbed";
                player.v.x += eff.grab.v.x;
                player.v.y += eff.grab.v.y;
                player.odds -= eff.grab.damage;
                player.odds = Math.max(0, player.odds);
                opponent.odds += eff.grab.damage;
                opponent.odds = Math.min(100, opponent.odds);
            }

            break;
        case "throw":
            if (!player.ateThrow.has(eff["throw"].id) && player.pose === "be_grabbed") {
                player.ateThrow.add(eff["throw"].id);
                player.i = state.i;
                player.v.x += eff["throw"].v.x;
                player.v.y += eff["throw"].v.y;
                player.pose = poseAttacked(player);
                player.odds -= eff["throw"].damage;
                player.odds = Math.max(0, player.odds);
                opponent.odds += eff["throw"].damage;
                opponent.odds = Math.min(100, opponent.odds);
            }

            break;
        case "shield":
            break;
        case "reset_vector":
            break;
        case "add_vector":
            break;
        }
    }

    playerAction = player.character.actions[player.pose + "_" + player.direction];
    opponentAction = opponent.character.actions[opponent.pose + "_" + opponent.direction];
    playerAnimation = player.character.animations[playerAction.animation];
    opponentAnimation = opponent.character.animations[opponentAction.animation];

    if ((state.i - player.i) % playerAnimation.frames_per_sprite === 0) {
        var left = opponent.x + getHijackParameterX(opponentAction) + getHijackParameterWidth(opponentAction) - getHijackParameterWidth(state.config) - getHijackParameterX(playerAction);
        var right = opponent.x + getHijackParameterX(opponentAction) + getHijackParameterWidth(state.config) - getHijackParameterX(playerAction) - getHijackParameterWidth(playerAction);

        player.x += player.v.x;
        player.y += player.v.y;

        player.x = Math.min(Math.max(left, player.x), right);
        player.y = Math.min(Math.max(-getHijackParameterY(playerAction), player.y), getHijackParameterHeight(state.config) - getHijackParameterY(playerAction) - getHijackParameterHeight(playerAction) - HIJACK_FLOOR_HEIGHT);
    }

    if (!onGround(state, player)) {
        if (player.pose !== "hop" && player.pose !== "jump" && player.pose !== "light_air_attack" && player.pose !== "medium_air_attack" && player.pose !== "hard_air_attack" && player.pose !== "be_attacked" && player.pose !== "be_attacked_top" && player.pose !== "be_attacked_bottom")
            player.pose = "fall";
        player.v.y += player.character.gravity;

        if (player.v.x < 0) {
            player.v.x += player.character.resistance;
            player.v.x = Math.min(0, player.v.x);
        } else if (player.v.x > 0) {
            player.v.x -= player.character.resistance;
            player.v.x = Math.max(0, player.v.x);
        }
    } else {
        if (player.pose === "be_attacked" || player.pose === "be_attacked_top" || player.pose === "be_attacked_bottom") {
            if (player.v.x < 0) {
                player.v.x += player.character.resistance;
                player.v.x = Math.min(0, player.v.x);
            } else if (player.v.x > 0) {
                player.v.x -= player.character.resistance;
                player.v.x = Math.max(0, player.v.x);
            }
        }

        if (player.pose === "fall") {
            player.pose = "neutral";
        }

        if (player.pose === "neutral") {
            player.v = {
                x: 0,
                y: 0
            };
        }
    }

    return state;
}

function inputCP(state, player, opponent) {
    if (player.inputCpStrategy == null)
        player.inputCpStrategy = "attack";

    var axes = [0, 0];
    var buttons = [
        {pressed: false},
        {pressed: false},
        {pressed: false},
        {pressed: false},
        {pressed: false},
        {pressed: false}
    ];

    if (player.pose === "be_attacked" || player.pose === "be_attacked_top" || player.pose === "be_attacked_bottom" || player.pose === "be_grabbed")
        player.inputCpStrategy = "shield";

    switch (player.inputCpStrategy) {
    case "attack":
        if (player.x < opponent.x - 256)
            axes[0] = 1;
        else if (player.x > opponent.x + 256)
            axes[0] = -1;
        else {
            var n = Math.floor(Math.random() * 3);

            switch (n) {
            case 0:
                if (player.x < opponent.x - 192)
                    buttons[2].pressed = true;
                else if (player.x > opponent.x + 192)
                    buttons[2].pressed = true;
                else if (player.x < opponent.x)
                    axes[0] = 1;
                else
                    axes[0] = -1;
                break;
            case 1:
                if (player.x < opponent.x - 128)
                    axes[0] = 1;
                else if (player.x > opponent.x + 128)
                    axes[0] = -1;
                else {
                    buttons[5].pressed = true;
                    if (opponent.pose === "be_grabbed")
                        buttons[Math.floor(Math.random() * 2) + 1].pressed = true;
                }
                break;
            default:
                break;
            }
        }

        var n = Math.floor(Math.random() * 30);

        switch (n) {
        case 0:
            buttons[3].pressed = true;
            break;
        default:
            break;
        }

        break;
    case "shield":
        var n = Math.floor(Math.random() * 30);

        switch (n) {
        case 0:
            player.inputCpStrategy = "attack";
            break;
        default:
            buttons[4].pressed = true;
            break;
        }

        break;
    }

    return {
        axes: axes,
        buttons: buttons
    };
}

function stepHijackModeGameTest(state, input) {
    //input[0] = inputCP(state, state.player0, state.player1);
    input[1] = inputCP(state, state.player1, state.player0);
    return stepHijackModeGame(state, input);
}

function stepHijackModeGame(state, input) {
    if (judgeHijackModeGame(state))
        state.mode = HIJACK_MODE_RESULT;

    var effect = effectHijackModeGame(state, input);

    resolveHijackModeGame(state, effect);

    return state;
}

function viewHijackModeGame(state) {
    var views = [];

    viewStagePerspective(state.stage);
    viewStageBackground(state.stage);
    viewStageFloor(state.stage);
    viewStageForeground(state.stage);
    viewCharacter(state.player0);
    viewCharacter(state.player1);
    viewOdds(state.player0.odds, state.player1.odds);

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
            sx: Math.floor((state.i - character.i) / animation.frames_per_sprite) % n * animation.width,
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
