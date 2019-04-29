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
