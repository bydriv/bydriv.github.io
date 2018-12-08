(function() {
    var wasm;
    const __exports = {};
    /**
    * @param {Views} arg0
    * @returns {number}
    */
    __exports.views_length = function(arg0) {
        return wasm.views_length(arg0.ptr);
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

    let cachegetUint32Memory = null;
    function getUint32Memory() {
        if (cachegetUint32Memory === null || cachegetUint32Memory.buffer !== wasm.memory.buffer) {
            cachegetUint32Memory = new Uint32Array(wasm.memory.buffer);
        }
        return cachegetUint32Memory;
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
    * @returns {Game}
    */
    __exports.intro = function() {
        return Game.__wrap(wasm.intro());
    };

    const heap = new Array(32);

    heap.fill(undefined);

    heap.push(undefined, null, true, false);

    let stack_pointer = 32;

    function addBorrowedObject(obj) {
        if (stack_pointer == 1) throw new Error('out of js stack');
        heap[--stack_pointer] = obj;
        return stack_pointer;
    }
    /**
    * @param {any} arg0
    * @param {Game} arg1
    * @returns {Game}
    */
    __exports.step = function(arg0, arg1) {
        try {
            return Game.__wrap(wasm.step(addBorrowedObject(arg0), arg1.ptr));

        } finally {
            heap[stack_pointer++] = undefined;

        }

    };

    /**
    * @param {Game} arg0
    * @returns {Views}
    */
    __exports.views = function(arg0) {
        return Views.__wrap(wasm.views(arg0.ptr));
    };

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

    __exports.__wbindgen_throw = function(ptr, len) {
        throw new Error(getStringFromWasm(ptr, len));
    };

    function init(path_or_module) {
        let instantiation;
        const imports = { './hijack': __exports };
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