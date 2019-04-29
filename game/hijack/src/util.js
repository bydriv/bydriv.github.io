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

function loadConfig(src) {
    function f(_config) {
        switch (Object.prototype.toString.call(_config)) {
        case "[object Null]":
        case "[object Boolean]":
        case "[object Number]":
            return Promise.resolve(_config);
        case "[object String]":
            if (_config.endsWith(".json"))
                return loadConfig(_config);
            else if (_config.endsWith(".png"))
                return loadImage(_config);
            else
                return Promise.resolve(_config);
        case "[object Array]":
            return Promise.all(_config.map(function (_cfg) {
                return f(_cfg);
            }));
        case "[object Object]":
            return Promise.all(Object.entries(_config).map(function (arg) {
                return f(arg[1]).then(function (cfg) {
                    return [arg[0], cfg];
                });
            })).then(function (entries) {
                return Object.fromEntries(entries);
            });
        }
    }

    return fetch(src, {cache: "no-cache"}).then(function (response) {
        return response.json();
    }).then(function (_config) {
        return f(_config);
    });
}

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

function getInput(input) {
    switch (input.length) {
    case 0:
        return [HIJACK_INPUT_EMPTY, HIJACK_INPUT_EMPTY];
    case 1:
        return [input[0], HIJACK_INPUT_EMPTY];
    default:
        return input;
    }
}
