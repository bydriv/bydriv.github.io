function judgeHijackModeGame(state) {
    return state.player0.odds >= 100 || state.player1.odds >= 100;
}

function effectHijackModeGame(state, input) {
    var effect0 = effectHijackModeGamePlayer(state, input, 0, state.player0, state.player1);
    var effect1 = effectHijackModeGamePlayer(state, input, 1, state.player1, state.player0);
    return effect0.concat(effect1);
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
        if (pad.axes[0] < -0.5) {
            effect.push({
                type: "i",
                player: player
            });

            effect.push({
                type: "pose",
                player: player,
                pose: "run"
            });

            effect.push({
                type: "direction",
                player: player,
                direction: "left"
            });

            effect.push({
                type: "add_vector",
                player: player,
                v: {
                    x: getHijackParameterX(player.character.actions.run_left.move),
                    y: getHijackParameterY(player.character.actions.run_left.move)
                }
            });
        } else if (pad.axes[0] > 0.5) {
            effect.push({
                type: "i",
                player: player
            });

            effect.push({
                type: "pose",
                player: player,
                pose: "run"
            });

            effect.push({
                type: "direction",
                player: player,
                direction: "right"
            });

            effect.push({
                type: "add_vector",
                player: player,
                v: {
                    x: getHijackParameterX(player.character.actions.run_right.move),
                    y: getHijackParameterY(player.character.actions.run_right.move)
                }
            });
        } else if (pad.buttons[0].pressed) {
            effect.push({
                type: "i",
                player: player
            });

            effect.push({
                type: "pose",
                player: player,
                //TODO
                //pose: "light_ground_attack",
                pose: "hard_ground_attack"
            });
        } else if (pad.buttons[1].pressed) {
            effect.push({
                type: "i",
                player: player
            });

            effect.push({
                type: "pose",
                player: player,
                //TODO
                //pose: "medium_ground_attack"
                pose: "hard_ground_attack"
            });
        } else if (pad.buttons[2].pressed) {
            effect.push({
                type: "i",
                player: player
            });

            effect.push({
                type: "pose",
                player: player,
                pose: "hard_ground_attack"
            });
        } else if (pad.buttons[3].pressed) {
            effect.push({
                type: "i",
                player: player
            });

            if (input[i].axes[1] < -0.5) {
                effect.push({
                    type: "pose",
                    player: player,
                    pose: "jump"
                });

                effect.push({
                    type: "add_vector",
                    player: player,
                    v: {
                        x: getHijackParameterX(player.character.actions["jump_" + player.direction].move),
                        y: getHijackParameterY(player.character.actions["jump_" + player.direction].move)
                    }
                });
            } else {
                effect.push({
                    type: "pose",
                    player: player,
                    pose: "hop"
                });

                effect.push({
                    type: "add_vector",
                    player: player,
                    v: {
                        x: getHijackParameterX(player.character.actions["hop_" + player.direction].move),
                        y: getHijackParameterY(player.character.actions["hop_" + player.direction].move)
                    }
                });
            }

            if (input[i].axes[0] < -0.5) {
                effect.push({
                    type: "add_vector",
                    player: player,
                    v: {
                        x: getHijackParameterX(player.character.actions.run_left.move),
                        y: getHijackParameterY(player.character.actions.run_left.move)
                    }
                });
            } else if (input[i].axes[0] > 0.5) {
                effect.push({
                    type: "add_vector",
                    player: player,
                    v: {
                        x: getHijackParameterX(player.character.actions.run_right.move),
                        y: getHijackParameterY(player.character.actions.run_right.move)
                    }
                });
            }
        }

        break;
    case "run":
        if (-0.5 <= input[i].axes[0] && input[i].axes[0] <= 0.5) {
            effect.push({
                type: "pose",
                player: player,
                pose: "neutral"
            });

            effect.push({
                type: "reset_vector",
                player: player,
                v: {
                    x: 0,
                    y: 0
                }
            });
        } else if (pad.buttons[3].pressed) {
            effect.push({
                type: "i",
                player: player
            });

            if (input[i].axes[1] < -0.5) {
                effect.push({
                    type: "pose",
                    player: player,
                    pose: "jump"
                });

                effect.push({
                    type: "add_vector",
                    player: player,
                    v: {
                        x: getHijackParameterX(player.character.actions["jump_" + player.direction].move),
                        y: getHijackParameterY(player.character.actions["jump_" + player.direction].move)
                    }
                });
            } else {
                effect.push({
                    type: "pose",
                    player: player,
                    pose: "hop"
                });

                effect.push({
                    type: "add_vector",
                    player: player,
                    v: {
                        x: getHijackParameterX(player.character.actions["hop_" + player.direction].move),
                        y: getHijackParameterY(player.character.actions["hop_" + player.direction].move)
                    }
                });
            }

            if (input[i].axes[0] < -0.5) {
                effect.push({
                    type: "add_vector",
                    player: player,
                    v: {
                        x: getHijackParameterX(player.character.actions.run_left.move),
                        y: getHijackParameterY(player.character.actions.run_left.move)
                    }
                });
            } else if (input[i].axes[0] > 0.5) {
                effect.push({
                    type: "add_vector",
                    player: player,
                    v: {
                        x: getHijackParameterX(player.character.actions.run_right.move),
                        y: getHijackParameterY(player.character.actions.run_right.move)
                    }
                });
            }
        }

        break;
    case "hop":
    case "jump":
        if (state.i - player.i < total_frames)
            break;

        effect.push({
            type: "pose",
            player: player,
            pose: "neutral"
        });

        effect.push({
            type: "reset_vector",
            player: player,
            v: {
                x: 0,
                y: 0
            }
        });

        break;
    case "light_ground_attack":
    case "medium_ground_attack":
    case "hard_ground_attack":
        if (state.i - player.i < total_frames) {
            var i = state.i - player.i;

            if (i < playerAction.startup) {
                /* pass */
            } else if (i < playerAction.startup + playerAction.active) {
                effect.push({
                    type: "attack",
                    player: player,
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
            } else if (i < playerAction.startup + playerAction.active + playerAction.recovery) {
                /* pass */
            }

            break;
        }

        effect.push({
            type: "id",
            player: player
        });

        effect.push({
            type: "pose",
            player: player,
            pose: "neutral"
        });

        effect.push({
            type: "reset_vector",
            player: player,
            v: {
                x: 0,
                y: 0
            }
        });

        break;
    case "light_air_attack":
        break;
    case "medium_air_attack":
        break;
    case "hard_air_attack":
        break;
    case "grab":
        break;
    case "grabbed":
        break;
    case "shield":
        break;
    case "be_attacked":
        break;
    case "be_attacked_top":
        break;
    case "be_attacked_bottom":
        break;
    case "be_grabbed":
        break;
    case "be_knockdown":
        break;
    case "be_knockout":
        break;
    }

    return effect;
}

function resolveHijackModeGame(state, effect) {
    var action0 = state.player0.character.actions[state.player0.pose + "_" + state.player0.direction];
    var action1 = state.player1.character.actions[state.player1.pose + "_" + state.player1.direction];
    var animation0 = state.player0.character.animations[action0.animation];
    var animation1 = state.player1.character.animations[action1.animation];

    for (var i = 0; i < effect.length; ++i) {
        var eff = effect[i];

        switch (eff.type) {
        case "i":
            eff.player.i = state.i;
            break;
        case "id":
            ++eff.player.id;
            break;
        case "pose":
            eff.player.pose = eff.pose;
            break;
        case "direction":
            eff.player.direction = eff.direction;
            break;
        case "attack":
            if (eff.player === state.player0) {
                var r = {
                    x: state.player1.x + getHijackParameterX(action1),
                    y: state.player1.y + getHijackParameterX(action1),
                    width: getHijackParameterWidth(action1),
                    height: getHijackParameterHeight(action1)
                };

                if (!state.player1.ate.has(eff.attack.id) && collision(eff.attack, r)) {
                    state.player1.ate.add(eff.attack.id);
                    state.player1.v.x += eff.attack.v.x;
                    state.player1.v.y += eff.attack.v.y;
                    state.player1.odds -= eff.attack.damage;
                    state.player1.odds = Math.max(0, state.player1.odds);
                    state.player0.odds += eff.attack.damage;
                    state.player0.odds = Math.min(100, state.player0.odds);
                }
            } else if (eff.player === state.player1) {
                var r = {
                    x: state.player0.x + getHijackParameterX(action1),
                    y: state.player0.y + getHijackParameterX(action1),
                    width: getHijackParameterWidth(action0),
                    height: getHijackParameterHeight(action0)
                };

                if (!state.player0.ate.has(eff.attack.id) && collision(eff.attack, r)) {
                    state.player0.ate.add(eff.attack.id);
                    state.player0.v.x += eff.attack.v.x;
                    state.player0.v.y += eff.attack.v.y;
                    state.player0.odds -= eff.attack.damage;
                    state.player0.odds = Math.max(0, state.player0.odds);
                    state.player1.odds += eff.attack.damage;
                    state.player1.odds = Math.min(100, state.player1.odds);
                }
            }

            break;
        case "reset_vector":
            eff.player.v = eff.v;
            break;
        case "add_vector":
            eff.player.v.x += eff.v.x;
            eff.player.v.y += eff.v.y;
            break;
        }
    }

    action0 = state.player0.character.actions[state.player0.pose + "_" + state.player0.direction];
    action1 = state.player1.character.actions[state.player1.pose + "_" + state.player1.direction];
    animation0 = state.player0.character.animations[action0.animation];
    animation1 = state.player1.character.animations[action1.animation];

    if (state.i % animation0.frames_per_sprite === 0) {
        var left0 = state.player1.x + getHijackParameterX(action1) + getHijackParameterWidth(action1) - getHijackParameterWidth(state.config) - getHijackParameterX(action0);
        var right0 = state.player1.x + getHijackParameterX(action1) + getHijackParameterWidth(state.config) - getHijackParameterX(action0) - getHijackParameterWidth(action0);

        state.player0.x += state.player0.v.x;
        state.player0.y += state.player0.v.y;

        state.player0.x = Math.min(Math.max(left0, state.player0.x), right0);
        state.player0.y = Math.min(Math.max(-getHijackParameterY(action0), state.player0.y), getHijackParameterHeight(state.config) - getHijackParameterY(action0) - getHijackParameterHeight(action0) - HIJACK_FLOOR_HEIGHT);
    }

    if (state.i % animation1.frames_per_sprite === 0) {
        var left1 = state.player0.x + getHijackParameterX(action0) + getHijackParameterWidth(action0) - getHijackParameterWidth(state.config) - getHijackParameterX(action1);
        var right1 = state.player0.x + getHijackParameterX(action0) + getHijackParameterWidth(state.config) - getHijackParameterX(action1) - getHijackParameterWidth(action1);

        state.player1.x += state.player1.v.x;
        state.player1.y += state.player1.v.y;

        state.player1.x = Math.min(Math.max(left1, state.player1.x), right1);
        state.player1.y = Math.min(Math.max(-getHijackParameterY(action1), state.player1.y), getHijackParameterHeight(state.config) - getHijackParameterY(action1) - getHijackParameterHeight(action1) - HIJACK_FLOOR_HEIGHT);
    }

    if (state.i % animation0.frames_per_sprite === 0 || state.i % animation1.frames_per_sprite === 0) {
        var left = Math.min(state.player0.x + getHijackParameterX(action0), state.player1.x + getHijackParameterX(action1));
        var right = Math.max(state.player0.x + getHijackParameterX(action0) + getHijackParameterWidth(action0), state.player1.x + getHijackParameterX(action1) + getHijackParameterWidth(action1));

        if (left < state.x) {
            state.x = left;
        }

        if (right > state.x + state.config.width) {
            state.x = right - state.config.width;
        }
    }

    if (state.player0.y + getHijackParameterY(action0) + getHijackParameterHeight(action0) < getHijackParameterHeight(state.config) - HIJACK_FLOOR_HEIGHT) {
        if (state.player0.pose !== "hop" && state.player0.pose !== "jump")
            state.player0.pose = "fall";
        state.player0.v.y += state.player0.character.gravity;

        if (state.player0.v.x < 0) {
            state.player0.v.x += state.player0.character.resistance;
            state.player0.v.x = Math.min(0, state.player0.v.x);
        } else if (state.player0.v.x < 0) {
            state.player0.v.x -= state.player0.character.resistance;
            state.player0.v.x = Math.max(0, state.player0.v.x);
        }
    } else {
        if (state.player0.pose === "fall") {
            state.player0.pose = "neutral";
            state.player0.v = {
                x: 0,
                y: 0
            };
        }
    }

    if (state.player1.y + getHijackParameterY(action1) + getHijackParameterHeight(action1) < getHijackParameterHeight(state.config) - HIJACK_FLOOR_HEIGHT) {
        if (state.player1.pose !== "hop" && state.player1.pose !== "jump")
            state.player1.pose = "fall";
        state.player1.v.y += state.player1.character.gravity;

        if (state.player1.v.x < 0) {
            state.player1.v.x += state.player1.character.resistance;
            state.player1.v.x = Math.min(0, state.player1.v.x);
        } else if (state.player1.v.x < 0) {
            state.player1.v.x -= state.player1.character.resistance;
            state.player1.v.x = Math.max(0, state.player1.v.x);
        }
    } else {
        if (state.player1.pose === "fall") {
            state.player1.pose = "neutral";
            state.player1.v = {
                x: 0,
                y: 0
            };
        }
    }

    return state;
}

function stepHijackModeGame(state, input) {
    if (judgeHijackModeGame(state))
        state.mode = HIJACK_MODE_RESULT;

    var effect = effectHijackModeGame(state, input);

    resolveHijackModeGame(state, effect);

    return state;
}

/*
function stepHijackModeGame(state, input) {
    if (state.character0.odds >= 100 || state.character1.odds >= 100) {
        state.mode = HIJACK_MODE_RESULT;
    }

    var attacks0 = [];
    var attacks1 = [];
    var grabs0 = [];
    var grabs1 = [];

    stepCharacterCalc(0, state.character0, attacks0, grabs0, state.character1, attacks1, grabs1);
    stepCharacterCalc(1, state.character1, attacks1, grabs1, state.character0, attacks0, grabs0);

    stepCharacterControl(0, state.character0, attacks0, grabs0, state.character1, attacks1, grabs1);
    stepCharacterControl(1, state.character1, attacks1, grabs1, state.character0, attacks0, grabs0);

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
                ++character.i;* /
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
*/

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
