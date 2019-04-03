window.addEventListener("load", function onLoad() {
    var dom = document.getElementById("liljax");
    var input = liljaxInput(dom);
    var output = liljaxOutput(dom);
    var state = newLiljax(input, output);
    state.process = createProcess(state, "sh", {type: "unit", value: {}});

    input.addEventListener("input", function onInput() {
        state.input = input.value;
    });

    requestAnimationFrame(function step() {
        input.focus();

        if (state.process != null && state.process.type == "running") {
            var item = state.process.value.cont();

            switch (item.type) {
            case "nothing":
                break;
            default:
                var line = document.getElementById("liljax-output-" + item.id.toString());
                if (line != null) {
                    line.innerText = item.value.value;
                    break;
                }
                line = document.createElement("div");
                line.setAttribute("class", item.type);
                line.setAttribute("id", "liljax-output-" + item.id.toString());
                line.innerText = item.value.value;
                output.appendChild(line);
                break;
            }

            state.process = item.process;
        } else if (state.stack.length > 0) {
            state.process = state.stack.pop();
        }

        requestAnimationFrame(step);
    });
});

function gensym() {
    return Math.floor(Math.random() * 0x100000000);
}

function liljaxInput(dom) {
    var input = document.createElement("textarea");
    input.setAttribute("class", "input");
    dom.appendChild(input);
    return input;
}

function liljaxOutput(dom) {
    var output = document.createElement("div");
    output.setAttribute("class", "output");
    dom.appendChild(output);
    return output;
}

function newLiljax(input, output) {
    var programs = new Map;
    programs.set("sh", liljaxProgramSh);
    programs.set("yes", liljaxProgramYes);
    programs.set("puts", liljaxProgramPuts);
    programs.set("info", liljaxProgramInfo);
    programs.set("warn", liljaxProgramWarn);
    programs.set("error", liljaxProgramError);

    return {
        programs: programs,
        input: "",
        stack: [],
        process: null,
        dom: {
            input: input,
            output: output
        }
    };
}

function liljaxClearInput(state) {
    state.dom.input.value = "";
    state.input = "";
}

function eqType(T, U) {
    if (typeof T === "string" && typeof U === "string") {
        return T === U;
    } else if (typeof T === "string") {
        return false;
    } else if (typeof U === "string") {
        return false;
    } else if (T.length !== U.length) {
        return false;
    } else {
        for (var i = 0; i < T.length /* === U.length */; ++i)
            if (!eqType(T[i], U[i]))
                return false;
        return true;
    }
}

function showType(T) {
    if (typeof T === "string") {
        return T;
    } else if (T[0] === "function") {
        return showTypeArr(T[1]) + " -> " + showType(T[2]);
    }
}

function showTypeArr(T) {
    if (typeof T === "string") {
        return T;
    } else if (T[0] === "function") {
        return "(" + showTypeArr(T[1]) + " -> " + showType(T[2]) + ")";
    }
}

function validateType(M, T) {
    return eqType(M.type, T);
}

function failWith(message) {
    return {
        type: "failure",
        message: message
    };
}

function executeCommand(state, command) {
    var cmd = liljaxProgramShLexToArray(command).filter(function (token) { return token.type !== "space"; });

    if (cmd.length === 0)
        return {
            type: "success",
            value: {
                type: "int",
                value: 0
            }
        };

    var T = liljaxProgramShInfer(cmd[0]);

    if (cmd.length === 1) {
        return createProcess(state, "puts", {type: "string", value: cmd[0].value + " : " + showType(T) });
    }

    var U = "int";

    for (var i = 1; i < cmd.length; ++i)
        U = ["function", liljaxProgramShInfer(cmd[cmd.length - i]), U];

    if (!eqType(T, U))
        return createProcess(state, "error", {type: "string", value: "expected: " + showType(U) + "; actual: " + showType(T) });

    var values = cmd.map(function (token) { return liljaxProgramShEval(token); });

    return values[0].value({state: state}, values[1]);
}

function createProcess(state, progName, args) {
    if (typeof progName !== "string")
        return failWith("failure");

    if (!state.programs.has(progName))
        return failWith("no such program `" + progName + "'");

    var program = state.programs.get(progName)

    if (!eqType(program.type[0], "function"))
        return failWith("failure");

    if (!eqType(program.type[1], args.type))
        return failWith("failure");

    if (!eqType(program.type[2], "int"))
        return failWith("failure");

    return program.value({state: state}, args);
}

function isSpace(c) {
    switch (c) {
    case '\r': case '\n': case '\f': case '\v': case '\t': case ' ':
        return true;
    default:
        return false;
    }
}

function isSmall(c) {
    switch (c) {
    case 'a': case 'b': case 'c': case 'd': case 'e': case 'f': case 'g':
    case 'h': case 'i': case 'j': case 'k': case 'l': case 'm': case 'n':
    case 'o': case 'p': case 'q': case 'r': case 's': case 't': case 'u':
    case 'v': case 'w': case 'x': case 'y': case 'z':
        return true;
    default:
        return false;
    }
}

function isDigit(c) {
    switch (c) {
    case '0': case '1': case '2': case '3': case '4':
    case '5': case '6': case '7': case '8': case '9':
        return true;
    default:
        return false;
    }
}

function liljaxProgramShLex(s) {
    if (s.length === 0) {
        return null;
    } else {
        if (isSpace(s[0])) {
            var i = 1;

            while (i < s.length && isSpace(s[i]))
                ++i;

            return {
                token: {
                    type: "space",
                    value: s.slice(0, i),
                },
                rest: s.slice(i)
            };
        }

        if (isSmall(s[0])) {
            var i = 1;

            while (i < s.length && (isSmall(s[i]) || isDigit(s[i]) || s[i] === '-'))
                ++i;

            return {
                token: {
                    type: "id",
                    value: s.slice(0, i),
                },
                rest: s.slice(i)
            };
        }

        if (isDigit(s[0])) {
            var i = 1;

            while (i < s.length && isDigit(s[i]))
                ++i;

            return {
                token: {
                    type: "int",
                    value: s.slice(0, i),
                },
                rest: s.slice(i)
            };
        }

        if (s[0] === '"') {
            var i = 1;

            while (i < s.length && s[i] !== '"') {
                if (s[i] === '\\' && i + 1 < s.length)
                    i += 2;
                else
                    ++i;
            }

            if (i < s.length)
                ++i;

            return {
                token: {
                    type: "string",
                    value: s.slice(0, i),
                },
                rest: s.slice(i)
            };
        }

        if (s[0] === '(' && s[1] === ')') {
            return {
                token: {
                    type: "unit",
                    value: s.slice(0, 2),
                },
                rest: s.slice(2)
            };
        }
    }
}

function liljaxProgramShLexToArray(s) {
    var tokens = [];
    var result = null;

    while (result = liljaxProgramShLex(s)) {
        tokens.push(result.token);
        s = result.rest;
    }

    return tokens;
}

function liljaxProgramShInfer(token) {
    switch (token.type) {
    case "id":
        switch (token.value) {
        case "sh":
            return ["function", "unit", "int"];
        case "yes":
            return ["function", "string", "int"];
        case "puts":
            return ["function", "string", "int"];
        case "info":
            return ["function", "string", "int"];
        case "warn":
            return ["function", "string", "int"];
        case "error":
            return ["function", "string", "int"];
        default:
            return "bottom";
        }
    case "int":
        return "int";
    case "string":
        return "string";
    case "unit":
        return "unit";
    }
}

function liljaxProgramShEval(token) {
    var T = liljaxProgramShInfer(token);

    switch (token.type) {
    case "id":
        switch (token.value) {
        case "sh":
            return liljaxProgramSh;
        case "yes":
            return liljaxProgramYes;
        case "puts":
            return liljaxProgramPuts;
        case "info":
            return liljaxProgramInfo;
        case "warn":
            return liljaxProgramWarn;
        case "error":
            return liljaxProgramError;
        default:
            return {
                type: "bottom",
                value: token.value
            };
        }
    case "int": case "string":
        return {
            type: T,
            value: eval(token.value)
        };
    case "unit":
        return {
            type: "unit",
            value: {}
        };
    }
}

var liljaxProgramSh = {
    type: ["function", "unit", "int"],
    value: function yes(metadata, args) {
        var process = {
            id: gensym()
        };

        return {
            type: "running",
            value: {
                cont: function cont() {
                    var i = metadata.state.input.indexOf("\n");

                    if (i < 0)
                        return {
                            type: "puts",
                            id: process.id,
                            value: {
                                type: "string",
                                value: "$ " + metadata.state.input
                            },
                            process: {
                                type: "running",
                                value: {
                                    cont: cont
                                }
                            }
                        };

                    process.id = gensym();
                    metadata.state.stack.push(metadata.state.process);

                    var command = metadata.state.input.slice(0, i);
                    liljaxClearInput(metadata.state);

                    return {
                        type: "nothing",
                        process: executeCommand(metadata.state, command)
                    };
                }
            }
        };
    }
};

var liljaxProgramYes = {
    type: ["function", "string", "int"],
    value: function yes(metadata, args) {
        return {
            type: "running",
            value: {
                cont: function cont() {
                    return {
                        type: "puts",
                        id: gensym(),
                        value: args,
                        process: {
                            type: "running",
                            value: {
                                cont: cont
                            }
                        }
                    };
                }
            }
        };
    }
};

var liljaxProgramPuts = {
    type: ["function", "string", "int"],
    value: function puts(metadata, args) {
        return {
            type: "running",
            value: {
                cont: function cont() {
                    return {
                        type: "puts",
                        id: gensym(),
                        value: args,
                        process: {
                            type: "success",
                            value: {
                                type: "int",
                                value: 0
                            }
                        }
                    };
                }
            }
        };
    }
};

var liljaxProgramInfo = {
    type: ["function", "string", "int"],
    value: function info(metadata, args) {
        return {
            type: "running",
            value: {
                cont: function cont() {
                    return {
                        type: "info",
                        id: gensym(),
                        value: args,
                        process: {
                            type: "success",
                            value: {
                                type: "int",
                                value: 0
                            }
                        }
                    };
                }
            }
        };
    }
};

var liljaxProgramWarn = {
    type: ["function", "string", "int"],
    value: function warn(metadata, args) {
        return {
            type: "running",
            value: {
                cont: function cont() {
                    return {
                        type: "warn",
                        id: gensym(),
                        value: args,
                        process: {
                            type: "success",
                            value: {
                                type: "int",
                                value: 0
                            }
                        }
                    };
                }
            }
        };
    }
};

var liljaxProgramError = {
    type: ["function", "string", "int"],
    value: function error(metadata, args) {
        return {
            type: "running",
            value: {
                cont: function cont() {
                    return {
                        type: "error",
                        id: gensym(),
                        value: args,
                        process: {
                            type: "success",
                            value: {
                                type: "int",
                                value: 1
                            }
                        }
                    };
                }
            }
        };
    }
};
