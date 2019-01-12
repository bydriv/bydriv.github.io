(function() {
    var wasm;
    const __exports = {};


    const heap = new Array(32);

    heap.fill(undefined);

    heap.push(undefined, null, true, false);

function getObject(idx) { return heap[idx]; }

__exports.__wbg_inputslength_c48062f37cfc16c5 = function(arg0) {
    return inputs_length(getObject(arg0));
};

function isLikeNone(x) {
    return x === undefined || x === null;
}

let cachegetUint32Memory = null;
function getUint32Memory() {
    if (cachegetUint32Memory === null || cachegetUint32Memory.buffer !== wasm.memory.buffer) {
        cachegetUint32Memory = new Uint32Array(wasm.memory.buffer);
    }
    return cachegetUint32Memory;
}

let cachegetFloat32Memory = null;
function getFloat32Memory() {
    if (cachegetFloat32Memory === null || cachegetFloat32Memory.buffer !== wasm.memory.buffer) {
        cachegetFloat32Memory = new Float32Array(wasm.memory.buffer);
    }
    return cachegetFloat32Memory;
}

__exports.__wbg_inputx_0e0646c6d86e002e = function(ret, arg0, arg1) {

    const val = input_x(arg0, getObject(arg1));
    getUint32Memory()[ret / 4] = !isLikeNone(val);
    getFloat32Memory()[ret / 4 + 1] = isLikeNone(val) ? 0 : val;

};

__exports.__wbg_inputy_40578a6b3f3a52d5 = function(ret, arg0, arg1) {

    const val = input_y(arg0, getObject(arg1));
    getUint32Memory()[ret / 4] = !isLikeNone(val);
    getFloat32Memory()[ret / 4 + 1] = isLikeNone(val) ? 0 : val;

};

__exports.__wbg_inputbutton_15839f0335a14ddc = function(arg0, arg1, arg2) {

    const val = input_button(arg0, arg1, getObject(arg2));
    return isLikeNone(val) ? 0xFFFFFF : val ? 1 : 0;

};
/**
* @param {ViewMap} arg0
* @returns {number}
*/
__exports.view_map_length = function(arg0) {
    return wasm.view_map_length(arg0.ptr);
};

/**
* @param {ViewMap} arg0
* @returns {number}
*/
__exports.view_map_x = function(arg0) {
    return wasm.view_map_x(arg0.ptr);
};

/**
* @param {ViewMap} arg0
* @returns {number}
*/
__exports.view_map_y = function(arg0) {
    return wasm.view_map_y(arg0.ptr);
};

/**
* @param {number} arg0
* @param {ViewMap} arg1
* @returns {number}
*/
__exports.view_map_z = function(arg0, arg1) {
    return wasm.view_map_z(arg0, arg1.ptr);
};

/**
* @param {number} arg0
* @param {ViewMap} arg1
* @returns {Views}
*/
__exports.view_map_views = function(arg0, arg1) {
    return Views.__wrap(wasm.view_map_views(arg0, arg1.ptr));
};

/**
* @param {Views} arg0
* @returns {number}
*/
__exports.views_length = function(arg0) {
    return wasm.views_length(arg0.ptr);
};

/**
* @param {Views} arg0
* @param {Views} arg1
* @returns {boolean}
*/
__exports.views_eq = function(arg0, arg1) {
    return (wasm.views_eq(arg0.ptr, arg1.ptr)) !== 0;
};

/**
* @param {number} arg0
* @param {Views} arg1
* @returns {boolean}
*/
__exports.view_is_image = function(arg0, arg1) {
    return (wasm.view_is_image(arg0, arg1.ptr)) !== 0;
};

let cachedTextDecoder = new TextDecoder('utf-8');

let cachegetUint8Memory = null;
function getUint8Memory() {
    if (cachegetUint8Memory === null || cachegetUint8Memory.buffer !== wasm.memory.buffer) {
        cachegetUint8Memory = new Uint8Array(wasm.memory.buffer);
    }
    return cachegetUint8Memory;
}

function getStringFromWasm(ptr, len) {
    return cachedTextDecoder.decode(getUint8Memory().subarray(ptr, ptr + len));
}

let cachedGlobalArgumentPtr = null;
function globalArgumentPtr() {
    if (cachedGlobalArgumentPtr === null) {
        cachedGlobalArgumentPtr = wasm.__wbindgen_global_argument_ptr();
    }
    return cachedGlobalArgumentPtr;
}
/**
* @param {number} arg0
* @param {Views} arg1
* @returns {string}
*/
__exports.view_image_name = function(arg0, arg1) {
    const retptr = globalArgumentPtr();
    wasm.view_image_name(retptr, arg0, arg1.ptr);
    const mem = getUint32Memory();
    const rustptr = mem[retptr / 4];
    const rustlen = mem[retptr / 4 + 1];
    if (rustptr === 0) return;
    const realRet = getStringFromWasm(rustptr, rustlen).slice();
    wasm.__wbindgen_free(rustptr, rustlen * 1);
    return realRet;

};

let cachegetInt32Memory = null;
function getInt32Memory() {
    if (cachegetInt32Memory === null || cachegetInt32Memory.buffer !== wasm.memory.buffer) {
        cachegetInt32Memory = new Int32Array(wasm.memory.buffer);
    }
    return cachegetInt32Memory;
}
/**
* @param {number} arg0
* @param {Views} arg1
* @returns {number | undefined}
*/
__exports.view_image_x = function(arg0, arg1) {
    const retptr = globalArgumentPtr();

    wasm.view_image_x(retptr, arg0, arg1.ptr);
    const present = getUint32Memory()[retptr / 4];
    const value = getInt32Memory()[retptr / 4 + 1];
    return present === 0 ? undefined : value;

};

/**
* @param {number} arg0
* @param {Views} arg1
* @returns {number | undefined}
*/
__exports.view_image_y = function(arg0, arg1) {
    const retptr = globalArgumentPtr();

    wasm.view_image_y(retptr, arg0, arg1.ptr);
    const present = getUint32Memory()[retptr / 4];
    const value = getInt32Memory()[retptr / 4 + 1];
    return present === 0 ? undefined : value;

};

/**
* @param {number} arg0
* @param {Views} arg1
* @returns {number | undefined}
*/
__exports.view_image_z = function(arg0, arg1) {
    const retptr = globalArgumentPtr();

    wasm.view_image_z(retptr, arg0, arg1.ptr);
    const present = getUint32Memory()[retptr / 4];
    const value = getInt32Memory()[retptr / 4 + 1];
    return present === 0 ? undefined : value;

};

/**
* @returns {Game}
*/
__exports.new_ = function() {
    return Game.__wrap(wasm.new_());
};

let heap_next = heap.length;

function addHeapObject(obj) {
    if (heap_next === heap.length) heap.push(heap.length + 1);
    const idx = heap_next;
    heap_next = heap[idx];

    heap[idx] = obj;
    return idx;
}
/**
* @param {number} arg0
* @param {any} arg1
* @param {Game} arg2
* @returns {Game}
*/
__exports.step = function(arg0, arg1, arg2) {
    return Game.__wrap(wasm.step(arg0, addHeapObject(arg1), arg2.ptr));
};

/**
* @param {Game} arg0
* @returns {ViewMap}
*/
__exports.view_map = function(arg0) {
    return ViewMap.__wrap(wasm.view_map(arg0.ptr));
};

function freeViews(ptr) {

    wasm.__wbg_views_free(ptr);
}
/**
*/
class Views {

    static __wrap(ptr) {
        const obj = Object.create(Views.prototype);
        obj.ptr = ptr;

        return obj;
    }

    free() {
        const ptr = this.ptr;
        this.ptr = 0;
        freeViews(ptr);
    }

}
__exports.Views = Views;

function freeGame(ptr) {

    wasm.__wbg_game_free(ptr);
}
/**
*/
class Game {

    static __wrap(ptr) {
        const obj = Object.create(Game.prototype);
        obj.ptr = ptr;

        return obj;
    }

    free() {
        const ptr = this.ptr;
        this.ptr = 0;
        freeGame(ptr);
    }

}
__exports.Game = Game;

function freeViewMap(ptr) {

    wasm.__wbg_viewmap_free(ptr);
}
/**
*/
class ViewMap {

    static __wrap(ptr) {
        const obj = Object.create(ViewMap.prototype);
        obj.ptr = ptr;

        return obj;
    }

    free() {
        const ptr = this.ptr;
        this.ptr = 0;
        freeViewMap(ptr);
    }

}
__exports.ViewMap = ViewMap;

function dropObject(idx) {
    if (idx < 36) return;
    heap[idx] = heap_next;
    heap_next = idx;
}

__exports.__wbindgen_object_drop_ref = function(i) { dropObject(i); };

__exports.__wbindgen_throw = function(ptr, len) {
    throw new Error(getStringFromWasm(ptr, len));
};

function init(path_or_module) {
    let instantiation;
    const imports = { './hijackjs': __exports };
    if (path_or_module instanceof WebAssembly.Module) {
        instantiation = WebAssembly.instantiate(path_or_module, imports)
        .then(instance => {
        return { instance, module: path_or_module }
    });
} else {
    const data = fetch(path_or_module);
    if (typeof WebAssembly.instantiateStreaming === 'function') {
        instantiation = WebAssembly.instantiateStreaming(data, imports);
    } else {
        instantiation = data
        .then(response => response.arrayBuffer())
        .then(buffer => WebAssembly.instantiate(buffer, imports));
    }
}
return instantiation.then(({instance}) => {
    wasm = init.wasm = instance.exports;

});
};
self.wasm_bindgen = Object.assign(init, __exports);
})();
