(function(f){if(typeof exports==="object"&&typeof module!=="undefined"){module.exports=f()}else if(typeof define==="function"&&define.amd){define([],f)}else{var g;if(typeof window!=="undefined"){g=window}else if(typeof global!=="undefined"){g=global}else if(typeof self!=="undefined"){g=self}else{g=this}g.ShellState = f()}})(function(){var define,module,exports;return (function(){function r(e,n,t){function o(i,f){if(!n[i]){if(!e[i]){var c="function"==typeof require&&require;if(!f&&c)return c(i,!0);if(u)return u(i,!0);var a=new Error("Cannot find module '"+i+"'");throw a.code="MODULE_NOT_FOUND",a}var p=n[i]={exports:{}};e[i][0].call(p.exports,function(r){var n=e[i][1][r];return o(n||r)},p,p.exports,r,e,n,t)}return n[i].exports}for(var u="function"==typeof require&&require,i=0;i<t.length;i++)o(t[i]);return o}return r})()({1:[function(require,module,exports){
"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
/*
 * Surface のアニメーション状態およびレイヤ状態を表すモデル
 */
class SurfaceModel {
    constructor(scopeId, surfaceId) {
        this.scopeId = scopeId;
        this.surfaceId = surfaceId;
        this.renderingTree = new SurfaceRenderingTree(surfaceId);
        this.serikos = {};
        this.talkCount = 0;
        this.destructed = false;
        this.move = { x: 0, y: 0 };
    }
}
exports.SurfaceModel = SurfaceModel;
class Seriko {
    constructor(patternID = -1) {
        this.patternID = patternID;
        this.paused = false;
        this.exclusive = false;
        this.canceled = false;
        this.finished = false;
    }
}
exports.Seriko = Seriko;
/*
 * 現在のサーフェスのレイヤ状態を一意に表すレンダリングツリー
 */
class SurfaceRenderingTree {
    constructor(surface) {
        this.base = surface;
        this.foregrounds = [];
        this.backgrounds = [];
        this.collisions = [];
    }
}
exports.SurfaceRenderingTree = SurfaceRenderingTree;
class SurfaceRenderingLayer {
    constructor(type, surface, x, y) {
        this.type = type;
        this.surface = surface;
        this.x = x;
        this.y = y;
    }
}
exports.SurfaceRenderingLayer = SurfaceRenderingLayer;

},{}],2:[function(require,module,exports){
"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const events_1 = require("events");
const SurfaceState_1 = require("./SurfaceState");
const SurfaceModel_1 = require("../Model/SurfaceModel");
class ShellState extends events_1.EventEmitter {
    constructor(shell) {
        super();
        this.shell = shell;
        this.config = shell.config;
    }
    destructor() {
        this.removeAllListeners("render");
    }
    createSurfaceState(scopeId, surfaceId, rndr) {
        const state = new SurfaceState_1.SurfaceState(new SurfaceModel_1.SurfaceModel(scopeId, surfaceId), this.shell, rndr);
        const render = () => state.render();
        this.on("render", render);
        state.addListener("destructing", () => { this.removeListener("render", render); });
        return state;
    }
    showRegion() {
        const { shell } = this;
        const { config } = shell;
        config.enableRegion = true;
        this.emit("render");
    }
    hideRegion() {
        const { shell } = this;
        const { config } = shell;
        config.enableRegion = false;
        this.emit("render");
    }
    bind(a, b) {
        const { shell } = this;
        const { config } = shell;
        bind_value(config, a, b, true);
        this.emit("render");
    }
    unbind(a, b) {
        const { shell } = this;
        const { config } = shell;
        bind_value(config, a, b, false);
        this.emit("render");
    }
}
exports.ShellState = ShellState;
// 着せ替えオンオフ
function bind_value(config, a, b, flag) {
    const { bindgroup, char } = config;
    if (typeof a === "number" && typeof b === "number") {
        const scopeId = a;
        const bindgroupId = b;
        if (bindgroup[scopeId] == null) {
            console.warn("ShellState#bind_value: bindgroup", "scopeId:", scopeId, "bindgroupId:", bindgroupId, "is not defined");
            return;
        }
        bindgroup[scopeId][bindgroupId] = flag;
        return;
    }
    if (typeof a === "string" && typeof b === "string") {
        const _category = a;
        const _parts = b;
        char.forEach((char, scopeId) => {
            char.bindgroup.forEach((bindgroup, bindgroupId) => {
                const { category, parts } = bindgroup.name;
                if (_category === category && _parts === parts) {
                    bind_value(config, scopeId, bindgroupId, flag);
                }
            });
        });
    }
    console.error("ShellState#bind_value:", "TypeError:", a, b);
    return;
}

},{"../Model/SurfaceModel":1,"./SurfaceState":3,"events":7}],3:[function(require,module,exports){
"use strict";
/*
 * Surface 状態モデルを更新する副作用関数群
 */
Object.defineProperty(exports, "__esModule", { value: true });
const Util = require("../Util");
const SurfaceDefinitionTree_1 = require("ikagaka-shell-loader/lib/Model/SurfaceDefinitionTree");
const SurfaceModel_1 = require("../Model/SurfaceModel");
const events_1 = require("events");
class SurfaceState extends events_1.EventEmitter {
    // アニメーション終了時に呼び出す手はずになっているプロミス値への継続
    // 本来は surface モデルに入れるべきだがクロージャを表現できないので
    constructor(surface, shell, rndr) {
        super();
        this.surface = surface;
        this.shell = shell;
        this.rndr = rndr;
        this.continuations = {};
        this.debug = false;
        this.surfaceNode = this.shell.surfaceDefTree.surfaces[surface.surfaceId];
        this.surfaceNode.animations.forEach((anim, animId) => {
            if (anim != null) {
                this.initSeriko(animId);
            }
        });
        this.constructRenderingTree();
        this.render(); // ~~debugの設定を待つため初回 render はしない~~
    }
    destructor() {
        this.emit("destructing");
        this.surface.destructed = true;
        this.endAll();
    }
    render() {
        const { scopeId, surfaceId } = this.surface;
        this.debug && console.time("render(" + scopeId + "," + surfaceId + ")");
        this.constructRenderingTree();
        // 実 DOM の canvas へ反映
        if (this.rndr instanceof Function) {
            return this.rndr(scopeId, surfaceId, this.surface.renderingTree).then(() => {
                this.debug && console.timeEnd("render(" + scopeId + "," + surfaceId + ")");
            });
        }
        else {
            return Promise.reject("renderer have not been attached yet");
        }
    }
    initSeriko(animId) {
        // レイヤの初期化、コンストラクタからのみ呼ばれるべき
        const { surfaceNode } = this;
        const { config } = this.shell;
        const { surfaceId, scopeId } = this.surface;
        if (surfaceNode.animations[animId] == null) {
            console.warn("SurfaceState#initLayer: animationID", animId, "is not defined in ", surfaceId, surfaceNode);
            return;
        }
        const anim = surfaceNode.animations[animId];
        const { intervals, patterns, options, collisions } = anim;
        if (intervals.some(([interval, args]) => "bind" === interval)) {
            // このanimIDは着せ替え機能付きレイヤ
            if (config.isBind(scopeId, animId)) {
                // 現在有効な bind なら
                if (intervals.length > 1) {
                    // [[bind, []]].length === 1
                    // bind+hogeは着せ替え付随アニメーション。
                    // 現在のレイヤにSERIKOレイヤを追加
                    // インターバルタイマの登録
                    this.begin(animId);
                    return;
                }
                // interval,bind
                return;
            }
            // 現在有効な bind でないなら
            // 現在の合成レイヤの着せ替えレイヤを非表示設定
            // bind+sometimsなどを殺す
            this.end(animId);
            return;
        }
        // 着せ替え機能なしレイヤ = 全てSERIKOレイヤ
        // 現在のレイヤにSERIKOレイヤを追加
        this.begin(animId);
    }
    updateBind() {
        const { surface, surfaceNode } = this;
        const animations = surfaceNode.animations;
        animations.forEach(({ intervals }, animId) => {
            if (intervals.some(([interval, args]) => "bind" === interval)) {
                // bind+ を発動
                this.initSeriko(animId);
            }
        });
        this.constructRenderingTree();
        return this.render().then(() => { });
    }
    // アニメーションタイミングループの開始要請
    begin(animId) {
        const { surfaceNode, config } = this;
        const { serikos, scopeId } = this.surface;
        const { intervals, patterns, options, collisions } = surfaceNode.animations[animId];
        if (intervals.some(([interval]) => interval === "bind")) {
            if (!config.isBind(scopeId, animId)) {
                return;
            }
        }
        // SERIKO Layer の状態を変更
        serikos[animId] = new SurfaceModel_1.Seriko();
        intervals.forEach(([interval, args]) => {
            // インターバルタイマの登録
            this.setIntervalTimer(animId, interval, args);
        });
    }
    // アニメーションタイミングループのintervalタイマの停止
    end(animId) {
        const { serikos } = this.surface;
        // SERIKO Layer の状態を変更
        delete serikos[animId];
    }
    // すべての自発的アニメーション再生の停止
    endAll() {
        const { serikos } = this.surface;
        Object.keys(serikos).forEach((animId) => {
            this.end(Number(animId));
        });
    }
    setIntervalTimer(animId, interval, args) {
        // setTimeoutする、beginからのみ呼ばれてほしい
        const serikos = this.surface.serikos;
        if (!(serikos[animId] instanceof SurfaceModel_1.Seriko)) {
            console.warn("SurfaceState#setTimer: animId", animId, "is not SerikoLayer");
            return;
        }
        const fn = (nextTick) => {
            // nextTick は アニメーション終わってから呼ぶともういっぺん random や always されるもの
            if (!(serikos[animId] instanceof SurfaceModel_1.Seriko)) {
                // nextTick 呼ばないのでintervalを終了する
                return;
            }
            this.play(animId)
                .catch((err) => console.info("animation canceled", err))
                .then(() => { nextTick(); });
        };
        // アニメーション描画タイミングの登録
        switch (interval) {
            // nextTickを呼ぶともう一回random
            case "always":
                Util.always(fn);
                return;
            case "runonce":
                setTimeout(() => this.play(animId));
                return;
            case "never": return;
            case "yen-e": return;
            case "talk": return;
            case "sometimes":
                Util.random(fn, 2);
                return;
            case "rarely":
                Util.random(fn, 4);
                return;
            default:
                const n = isFinite(args[0]) ? args[0]
                    : (console.warn("Surface#setIntervalTimer: failback to", 4, "from", args[0], interval, animId)
                        , 4);
                if (interval === "random") {
                    Util.random(fn, n);
                    return;
                }
                if (interval === "periodic") {
                    Util.periodic(fn, n);
                    return;
                }
        }
        console.warn("SurfaceState#setIntervalTimer: unkown interval:", interval, animId);
        return;
    }
    // アニメーション再生
    play(animId) {
        const { surfaceNode, debug, surface, config } = this;
        const { serikos, destructed, scopeId, surfaceId } = surface;
        const { animations } = surfaceNode;
        if (!(animations[animId] instanceof SurfaceDefinitionTree_1.SurfaceAnimation)) {
            // そんなアニメーションはない
            console.warn("SurfaceState#play: animation " + animId + " is not defined");
            return Promise.reject("SurfaceState#play: animation " + animId + " is not defined");
        }
        const anim = animations[animId];
        const { intervals, patterns, options, collisions } = anim;
        if (intervals.some(([interval]) => interval === "bind")) {
            if (!config.isBind(scopeId, animId)) {
                // その bind+ は現在の着せ替え設定では無効だ
                console.warn("SurfaceState#play: this animation is turned off in current bindgroup state");
                return Promise.reject("SurfaceState#play: this animation is turned off in current bindgroup state");
            }
        }
        if (destructed) {
            // 既に破棄されたサーフェスなのでアニメーション再生とかありえん
            return Promise.reject("SurfaceState#play: destructed");
        }
        if (!(serikos[animId] instanceof SurfaceModel_1.Seriko)) {
            // SERIKO Layer の状態を初期化
            serikos[animId] = new SurfaceModel_1.Seriko();
        }
        let seriko = serikos[animId];
        if (seriko.patternID >= 0 || seriko.paused) {
            // 既に再生中、ポーズ中ならば再生停止して最初からどうぞ
            seriko.canceled = true; // this.step に渡している Seriko への参照はキャンセル
            seriko = serikos[animId] = new SurfaceModel_1.Seriko(); // 新しい値を設定
        }
        anim.getExclusives().map((exAnimId) => {
            // exclusive指定を反映
            if (serikos[exAnimId] instanceof SurfaceModel_1.Seriko) {
                serikos[exAnimId].exclusive = true;
            }
        });
        debug && console.group("(" + [scopeId, surfaceId, animId].join(",") + ")");
        debug && console.info("animation start", animId, anim);
        return new Promise((resolve, reject) => {
            // pause から resume した後に帰るべき場所への継続を取り出す
            this.continuations[animId] = { resolve, reject };
            this.step(animId, seriko);
        }).catch(console.info.bind(console, "animation")).then(() => {
            debug && console.info("animation finish", animId);
            debug && console.groupEnd();
        });
    }
    step(animId, seriko) {
        if (this.continuations[animId] == null) {
            console.warn("animation", animId, "is not prepared", seriko);
        }
        const { surface, debug, surfaceNode } = this;
        const { serikos, destructed, scopeId, surfaceId } = surface;
        const { resolve, reject } = this.continuations[animId];
        const anim = surfaceNode.animations[animId];
        // patternをすすめる
        // exclusive中のやつら探す
        const exclusives = Object.keys(serikos)
            .filter((id) => !(serikos[id] instanceof SurfaceModel_1.Seriko))
            .filter((id) => serikos[id].exclusive);
        if (exclusives.length > 0) {
            // exclusiveが存在
            if (exclusives.every((id) => Number(id) !== animId)) {
                // exclusives の中に自分は含まれない＝排他されてしまう
                seriko.canceled = true;
            }
        }
        if (seriko.canceled) {
            // キャンセルされたので reject
            if (reject instanceof Function) {
                reject("SurfaceState#step: canceled.");
            }
            return;
        }
        if (seriko.paused) {
            // 次にplayが呼び出されるまで何もしない 
            return;
        }
        // patternID は現在表示中のパタン
        // patternID === -1 は +1 され 0 になり wait ミリ秒間待ってから patternID === 0 を表示するとの意思表明
        // patternID+1 はこれから表示
        seriko.patternID++;
        if (anim.patterns[seriko.patternID] == null) {
            // このステップで次に表示すべきなにかがない＝このアニメは終了
            seriko.finished = true;
        }
        if (seriko.finished) {
            // 初期化
            serikos[animId] = new SurfaceModel_1.Seriko();
            delete this.continuations[animId];
            this.render().then(() => {
                // 最終状態を描画してから終了
                if (resolve instanceof Function) {
                    resolve();
                }
            });
            return;
        }
        const { wait, type, x, y, animation_ids } = anim.patterns[seriko.patternID];
        let _surface = anim.patterns[seriko.patternID].surface;
        const _wait = Util.randomRange(wait[0], wait[1]);
        switch (type) {
            // 付随再生であってこのアニメの再生終了は待たない・・・はず？
            case "start":
                this.play(animation_ids[0]);
                return;
            case "stop":
                this.stop(animation_ids[0]);
                return;
            case "alternativestart":
                this.play(Util.choice(animation_ids));
                return;
            case "alternativestop":
                this.stop(Util.choice(animation_ids));
                return;
            case "move":
                surface.move.x = x;
                surface.move.y = y;
                debug && console.time("move(" + scopeId + "," + surfaceId + ")");
                // 動き終わるのを待つ
                new Promise((resolve) => setTimeout(resolve, _wait))
                    .catch(console.warn.bind(console)) // 何らかの理由で move がキャンセルされようが続行
                    .then(() => {
                    debug && console.timeEnd("move(" + scopeId + "," + surfaceId + ")");
                    this.emit("onMove", { type: "onMove", scopeId, surfaceId });
                    // 次のパターン処理へ
                    this.step(animId, seriko);
                });
                return;
        }
        // waitだけ待ってからレンダリング
        debug && console.time("waiting(" + [scopeId, surfaceId, animId].join(",") + "): " + _wait + "ms");
        setTimeout(() => {
            debug && console.timeEnd("waiting(" + [scopeId, surfaceId, animId].join(",") + "): " + _wait + "ms");
            if (_surface < -2) {
                // SERIKO/1.4 ?
                console.warn("SurfaceState#step: pattern surfaceId", animId, seriko.patternID, _surface, "is not defined in SERIKO/1.4, failback to -2");
                _surface = -2;
            }
            if (_surface === -1) {
                // SERIKO/1.4 -1 として表示されいたこのアニメーション終了 
                seriko.finished = true;
                return this.step(animId, seriko);
            }
            if (_surface === -2) {
                // SERIKO/1.4 全アニメーション停止
                Object.keys(serikos).forEach((id) => {
                    if (serikos[id] instanceof SurfaceModel_1.Seriko) {
                        serikos[id].finished = true;
                        this.step(animId, serikos[id]);
                    }
                });
            }
            // 描画
            this.render().then(() => {
                // 次のパターン処理へ
                this.step(animId, seriko);
            });
        }, _wait);
    }
    // 再生中のアニメーションを停止しろ
    stop(animId) {
        const { serikos } = this.surface;
        if (serikos[animId] instanceof SurfaceModel_1.Seriko) {
            // 何らかの理由で停止要請がでたのでつまりキャンセル
            serikos[animId].canceled = true;
        }
    }
    pause(animId) {
        const { serikos } = this.surface;
        if (serikos[animId] instanceof SurfaceModel_1.Seriko) {
            serikos[animId].paused = true;
        }
    }
    resume(animId) {
        const { serikos } = this.surface;
        if (serikos[animId] instanceof SurfaceModel_1.Seriko) {
            serikos[animId].paused = false;
            this.step(animId, serikos[animId]);
        }
    }
    talk() {
        const { surfaceNode } = this;
        const srf = this.surface;
        const { serikos } = this.surface;
        const animations = surfaceNode.animations;
        srf.talkCount++;
        // talkなものでかつtalkCountとtalk,nのmodが0なもの
        const hits = animations.filter((anim, animId) => anim.intervals.some(([interval, args]) => "talk" === interval && srf.talkCount % args[0] === 0));
        hits.forEach((anim, animId) => {
            // そのtalkアニメーションは再生が終了しているか？
            if (serikos[animId] instanceof SurfaceModel_1.Seriko) {
                if (serikos[animId].patternID < 0) {
                    this.play(animId);
                }
            }
        });
    }
    yenE() {
        const anims = this.surfaceNode.animations;
        return Promise.all([
            anims
                .filter((anim, animId) => anim.intervals.some(([interval, args]) => interval === "yen-e"))
                .map((anim, animId) => this.play(animId))
        ]).then(() => { return; });
    }
    constructRenderingTree() {
        // 再帰的にpatternで読んでいるベースサーフェス先のbindまで考慮してレンダリングツリーを構築し反映
        const { surface, debug, shell } = this;
        const { surfaceId, serikos, renderingTree, scopeId } = surface;
        const { config, surfaceDefTree } = shell;
        const { surfaces } = surfaceDefTree;
        surface.renderingTree = layersToTree(surfaces, scopeId, surfaceId, serikos, config);
        debug && console.log("diff(" + scopeId + "," + surfaceId + "): ", Util.diff(renderingTree, surface.renderingTree), surface.renderingTree);
        // レンダリングツリーが更新された！
    }
}
exports.SurfaceState = SurfaceState;
function layersToTree(surfaces, scopeId, n, serikos, config) {
    const { animations, collisions } = surfaces[n];
    const tree = new SurfaceModel_1.SurfaceRenderingTree(n);
    tree.collisions = collisions;
    animations.forEach((anim, animId) => {
        const { patterns, collisions, intervals } = anim;
        const rndLayerSets = [];
        // seriko で表示されているものをレンダリングツリーに追加
        if (serikos[animId] instanceof SurfaceModel_1.Seriko) {
            const { patternID } = serikos[animId];
            if (patterns[patternID] instanceof SurfaceDefinitionTree_1.SurfaceAnimationPattern) {
                // pattern が定義されている seriko layer
                const { type, surface, x, y } = patterns[patternID];
                if (surface > 0) {
                    // 非表示でない
                    if (surfaces[surface] instanceof SurfaceDefinitionTree_1.SurfaceDefinition) {
                        const _tree = recursiveBind(surfaces, surface, serikos, config, collisions);
                        rndLayerSets.push(new SurfaceModel_1.SurfaceRenderingLayer(type, _tree, x, y));
                    }
                    else {
                        // 存在しないサーフェスを参照した
                        console.warn("SurfaceState.layersToTree: surface", n, "is not defined");
                    }
                }
            }
        }
        else if (config.isBind(scopeId, animId) && intervals.some(([interval, args]) => "bind" === interval) && intervals.length === 1) {
            // interval,bind である、 insert のための再帰的処理
            processInsert(patterns, collisions, rndLayerSets);
        }
        if (anim.isBack()) {
            tree.backgrounds.push(rndLayerSets);
        }
        else {
            tree.foregrounds.push(rndLayerSets);
        }
    });
    return tree;
    function processInsert(patterns, collisions, rndLayerSets) {
        // SC.isBind(config, animId) && intervals.some(([interval, args])=> "bind" === interval) && intervals.length === 1
        // なときだけ呼ばれたい
        // TODO: insert の循環参照を防ぐ
        patterns.forEach(({ type, surface, x, y, animation_ids }, patId) => {
            if (type === "insert") {
                // insertの場合は対象のIDをとってくる
                const insertId = animation_ids[0];
                if (!(animations[insertId] instanceof SurfaceDefinitionTree_1.SurfaceAnimation)) {
                    console.warn("SurfaceState.layersToTree", "insert id", animation_ids, "is wrong target.", n, patId);
                    return;
                }
                const { patterns, collisions } = animations[insertId];
                // insertをねじ込む
                processInsert(patterns, collisions, rndLayerSets);
                return;
            }
            if (surface > 0 && surfaces[surface] instanceof SurfaceDefinitionTree_1.SurfaceDefinition) {
                const tree = recursiveBind(surfaces, surface, serikos, config, collisions);
                rndLayerSets.push(new SurfaceModel_1.SurfaceRenderingLayer(type, tree, x, y));
            }
            else {
                // MAYUNA で -1 はありえん
                console.warn("SurfaceState.layersToTree: unexpected surface id ", surface);
            }
        });
    }
    function recursiveBind(surfaces, n, serikos, config, collisions) {
        // この関数は n が surfaces[n] に存在することを必ず確認してから呼ぶこと
        // TODO: bind の循環参照発生するので防ぐこと
        const { animations } = surfaces[n];
        const tree = new SurfaceModel_1.SurfaceRenderingTree(n);
        // animation0.collision0
        tree.collisions = collisions;
        animations.forEach((anim, animId) => {
            const { patterns, intervals, collisions } = anim;
            const rndLayerSets = [];
            if (config.isBind(scopeId, animId) && intervals.some(([interval, args]) => "bind" === interval) && intervals.length === 1) {
                // interval,bind である、 insert のための再帰的処理
                processInsert(patterns, collisions, rndLayerSets);
            }
            if (anim.isBack()) {
                tree.backgrounds.push(rndLayerSets);
            }
            else {
                tree.foregrounds.push(rndLayerSets);
            }
        });
        return tree;
    }
}

},{"../Model/SurfaceModel":1,"../Util":4,"events":7,"ikagaka-shell-loader/lib/Model/SurfaceDefinitionTree":8}],4:[function(require,module,exports){
"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const deep = require("deep-diff");
function diff(lhs, rhs, prefilter, acc) {
    const ret = deep.diff(lhs, rhs, prefilter, acc);
    return ret != null ? ret : [];
}
exports.diff = diff;
// find filename that matches arg "filename" from arg "paths"
// filename: in surface.txt, as ./surface0.png,　surface0.PNG, .\element\element0.PNG ...
function find(paths, filename) {
    filename = filename.split("\\").join("/");
    if (filename.slice(0, 2) === "./")
        filename = filename.slice(2);
    const reg = new RegExp("^" + filename.replace(".", "\.") + "$", "i");
    const hits = paths.filter((key) => reg.test(key));
    return hits;
}
exports.find = find;
// 検索打ち切って高速化
function fastfind(paths, filename) {
    filename = filename.split("\\").join("/");
    if (filename.slice(0, 2) === "./")
        filename = filename.slice(2);
    const reg = new RegExp("^" + filename.replace(".", "\.") + "$", "i");
    for (let i = 0; i < paths.length; i++) {
        if (reg.test(paths[i])) {
            return paths[i];
        }
    }
    return "";
}
exports.fastfind = fastfind;
// random(func, n) means call func 1/n per sec
function random(callback, probability) {
    return setTimeout((() => {
        function nextTick() { random(callback, probability); }
        if (Math.random() < 1 / probability)
            callback(nextTick);
        else
            nextTick();
    }), 1000);
}
exports.random = random;
// cron
function periodic(callback, sec) {
    return setTimeout((() => callback(() => periodic(callback, sec))), sec * 1000);
}
exports.periodic = periodic;
// 非同期ループするだけ
function always(callback) {
    return setTimeout((() => callback(() => always(callback))), 0);
}
exports.always = always;
// min-max 間のランダム値
function randomRange(min, max) {
    return min + Math.floor(Math.random() * (max - min + 1));
}
exports.randomRange = randomRange;
// [1,2,3] -> 1 or 2 or 3 as 33% probability
function choice(arr) {
    return arr[Math.ceil(Math.random() * 100 * (arr.length)) % arr.length];
}
exports.choice = choice;
function has(dir, path) {
    return fastfind(Object.keys(dir), path);
}
exports.has = has;
function get(dir, path) {
    let key = "";
    if ((key = this.has(dir, path)) === "") {
        return Promise.reject("file not find");
    }
    return Promise.resolve(dir[key]);
}
exports.get = get;
// 0 -> sakura
function scope(scopeId) {
    return scopeId === 0 ? "sakura"
        : scopeId === 1 ? "kero"
            : "char" + scopeId;
}
exports.scope = scope;
// sakura -> 0
// parse error -> -1
function unscope(charId) {
    return charId === "sakura" ? 0
        : charId === "kero" ? 1
            : Number((/^char(\d+)/.exec(charId) || ["", "-1"])[1]);
}
exports.unscope = unscope;

},{"deep-diff":6}],5:[function(require,module,exports){
"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.version = require("../package.json").version;
var ShellState_1 = require("./State/ShellState");
exports.ShellState = ShellState_1.ShellState;

},{"../package.json":9,"./State/ShellState":2}],6:[function(require,module,exports){
(function (global){
(function (global, factory) {
	typeof exports === 'object' && typeof module !== 'undefined' ? module.exports = factory() :
	typeof define === 'function' && define.amd ? define(factory) :
	(global.DeepDiff = factory());
}(this, (function () { 'use strict';

var $scope;
var conflict;
var conflictResolution = [];
if (typeof global === 'object' && global) {
  $scope = global;
} else if (typeof window !== 'undefined') {
  $scope = window;
} else {
  $scope = {};
}
conflict = $scope.DeepDiff;
if (conflict) {
  conflictResolution.push(
    function() {
      if ('undefined' !== typeof conflict && $scope.DeepDiff === accumulateDiff) {
        $scope.DeepDiff = conflict;
        conflict = undefined;
      }
    });
}

// nodejs compatible on server side and in the browser.
function inherits(ctor, superCtor) {
  ctor.super_ = superCtor;
  ctor.prototype = Object.create(superCtor.prototype, {
    constructor: {
      value: ctor,
      enumerable: false,
      writable: true,
      configurable: true
    }
  });
}

function Diff(kind, path) {
  Object.defineProperty(this, 'kind', {
    value: kind,
    enumerable: true
  });
  if (path && path.length) {
    Object.defineProperty(this, 'path', {
      value: path,
      enumerable: true
    });
  }
}

function DiffEdit(path, origin, value) {
  DiffEdit.super_.call(this, 'E', path);
  Object.defineProperty(this, 'lhs', {
    value: origin,
    enumerable: true
  });
  Object.defineProperty(this, 'rhs', {
    value: value,
    enumerable: true
  });
}
inherits(DiffEdit, Diff);

function DiffNew(path, value) {
  DiffNew.super_.call(this, 'N', path);
  Object.defineProperty(this, 'rhs', {
    value: value,
    enumerable: true
  });
}
inherits(DiffNew, Diff);

function DiffDeleted(path, value) {
  DiffDeleted.super_.call(this, 'D', path);
  Object.defineProperty(this, 'lhs', {
    value: value,
    enumerable: true
  });
}
inherits(DiffDeleted, Diff);

function DiffArray(path, index, item) {
  DiffArray.super_.call(this, 'A', path);
  Object.defineProperty(this, 'index', {
    value: index,
    enumerable: true
  });
  Object.defineProperty(this, 'item', {
    value: item,
    enumerable: true
  });
}
inherits(DiffArray, Diff);

function arrayRemove(arr, from, to) {
  var rest = arr.slice((to || from) + 1 || arr.length);
  arr.length = from < 0 ? arr.length + from : from;
  arr.push.apply(arr, rest);
  return arr;
}

function realTypeOf(subject) {
  var type = typeof subject;
  if (type !== 'object') {
    return type;
  }

  if (subject === Math) {
    return 'math';
  } else if (subject === null) {
    return 'null';
  } else if (Array.isArray(subject)) {
    return 'array';
  } else if (Object.prototype.toString.call(subject) === '[object Date]') {
    return 'date';
  } else if (typeof subject.toString === 'function' && /^\/.*\//.test(subject.toString())) {
    return 'regexp';
  }
  return 'object';
}

function deepDiff(lhs, rhs, changes, prefilter, path, key, stack) {
  path = path || [];
  stack = stack || [];
  var currentPath = path.slice(0);
  if (typeof key !== 'undefined') {
    if (prefilter) {
      if (typeof(prefilter) === 'function' && prefilter(currentPath, key)) {
        return; } else if (typeof(prefilter) === 'object') {
        if (prefilter.prefilter && prefilter.prefilter(currentPath, key)) {
          return; }
        if (prefilter.normalize) {
          var alt = prefilter.normalize(currentPath, key, lhs, rhs);
          if (alt) {
            lhs = alt[0];
            rhs = alt[1];
          }
        }
      }
    }
    currentPath.push(key);
  }

  // Use string comparison for regexes
  if (realTypeOf(lhs) === 'regexp' && realTypeOf(rhs) === 'regexp') {
    lhs = lhs.toString();
    rhs = rhs.toString();
  }

  var ltype = typeof lhs;
  var rtype = typeof rhs;

  var ldefined = ltype !== 'undefined' || (stack && stack[stack.length - 1].lhs && stack[stack.length - 1].lhs.hasOwnProperty(key));
  var rdefined = rtype !== 'undefined' || (stack && stack[stack.length - 1].rhs && stack[stack.length - 1].rhs.hasOwnProperty(key));

  if (!ldefined && rdefined) {
    changes(new DiffNew(currentPath, rhs));
  } else if (!rdefined && ldefined) {
    changes(new DiffDeleted(currentPath, lhs));
  } else if (realTypeOf(lhs) !== realTypeOf(rhs)) {
    changes(new DiffEdit(currentPath, lhs, rhs));
  } else if (realTypeOf(lhs) === 'date' && (lhs - rhs) !== 0) {
    changes(new DiffEdit(currentPath, lhs, rhs));
  } else if (ltype === 'object' && lhs !== null && rhs !== null) {
    if (!stack.filter(function(x) {
        return x.lhs === lhs; }).length) {
      stack.push({ lhs: lhs, rhs: rhs });
      if (Array.isArray(lhs)) {
        var i, len = lhs.length;
        for (i = 0; i < lhs.length; i++) {
          if (i >= rhs.length) {
            changes(new DiffArray(currentPath, i, new DiffDeleted(undefined, lhs[i])));
          } else {
            deepDiff(lhs[i], rhs[i], changes, prefilter, currentPath, i, stack);
          }
        }
        while (i < rhs.length) {
          changes(new DiffArray(currentPath, i, new DiffNew(undefined, rhs[i++])));
        }
      } else {
        var akeys = Object.keys(lhs);
        var pkeys = Object.keys(rhs);
        akeys.forEach(function(k, i) {
          var other = pkeys.indexOf(k);
          if (other >= 0) {
            deepDiff(lhs[k], rhs[k], changes, prefilter, currentPath, k, stack);
            pkeys = arrayRemove(pkeys, other);
          } else {
            deepDiff(lhs[k], undefined, changes, prefilter, currentPath, k, stack);
          }
        });
        pkeys.forEach(function(k) {
          deepDiff(undefined, rhs[k], changes, prefilter, currentPath, k, stack);
        });
      }
      stack.length = stack.length - 1;
    } else if (lhs !== rhs) {
      // lhs is contains a cycle at this element and it differs from rhs
      changes(new DiffEdit(currentPath, lhs, rhs));
    }
  } else if (lhs !== rhs) {
    if (!(ltype === 'number' && isNaN(lhs) && isNaN(rhs))) {
      changes(new DiffEdit(currentPath, lhs, rhs));
    }
  }
}

function accumulateDiff(lhs, rhs, prefilter, accum) {
  accum = accum || [];
  deepDiff(lhs, rhs,
    function(diff) {
      if (diff) {
        accum.push(diff);
      }
    },
    prefilter);
  return (accum.length) ? accum : undefined;
}

function applyArrayChange(arr, index, change) {
  if (change.path && change.path.length) {
    var it = arr[index],
      i, u = change.path.length - 1;
    for (i = 0; i < u; i++) {
      it = it[change.path[i]];
    }
    switch (change.kind) {
      case 'A':
        applyArrayChange(it[change.path[i]], change.index, change.item);
        break;
      case 'D':
        delete it[change.path[i]];
        break;
      case 'E':
      case 'N':
        it[change.path[i]] = change.rhs;
        break;
    }
  } else {
    switch (change.kind) {
      case 'A':
        applyArrayChange(arr[index], change.index, change.item);
        break;
      case 'D':
        arr = arrayRemove(arr, index);
        break;
      case 'E':
      case 'N':
        arr[index] = change.rhs;
        break;
    }
  }
  return arr;
}

function applyChange(target, source, change) {
  if (target && source && change && change.kind) {
    var it = target,
      i = -1,
      last = change.path ? change.path.length - 1 : 0;
    while (++i < last) {
      if (typeof it[change.path[i]] === 'undefined') {
        it[change.path[i]] = (typeof change.path[i] === 'number') ? [] : {};
      }
      it = it[change.path[i]];
    }
    switch (change.kind) {
      case 'A':
        applyArrayChange(change.path ? it[change.path[i]] : it, change.index, change.item);
        break;
      case 'D':
        delete it[change.path[i]];
        break;
      case 'E':
      case 'N':
        it[change.path[i]] = change.rhs;
        break;
    }
  }
}

function revertArrayChange(arr, index, change) {
  if (change.path && change.path.length) {
    // the structure of the object at the index has changed...
    var it = arr[index],
      i, u = change.path.length - 1;
    for (i = 0; i < u; i++) {
      it = it[change.path[i]];
    }
    switch (change.kind) {
      case 'A':
        revertArrayChange(it[change.path[i]], change.index, change.item);
        break;
      case 'D':
        it[change.path[i]] = change.lhs;
        break;
      case 'E':
        it[change.path[i]] = change.lhs;
        break;
      case 'N':
        delete it[change.path[i]];
        break;
    }
  } else {
    // the array item is different...
    switch (change.kind) {
      case 'A':
        revertArrayChange(arr[index], change.index, change.item);
        break;
      case 'D':
        arr[index] = change.lhs;
        break;
      case 'E':
        arr[index] = change.lhs;
        break;
      case 'N':
        arr = arrayRemove(arr, index);
        break;
    }
  }
  return arr;
}

function revertChange(target, source, change) {
  if (target && source && change && change.kind) {
    var it = target,
      i, u;
    u = change.path.length - 1;
    for (i = 0; i < u; i++) {
      if (typeof it[change.path[i]] === 'undefined') {
        it[change.path[i]] = {};
      }
      it = it[change.path[i]];
    }
    switch (change.kind) {
      case 'A':
        // Array was modified...
        // it will be an array...
        revertArrayChange(it[change.path[i]], change.index, change.item);
        break;
      case 'D':
        // Item was deleted...
        it[change.path[i]] = change.lhs;
        break;
      case 'E':
        // Item was edited...
        it[change.path[i]] = change.lhs;
        break;
      case 'N':
        // Item is new...
        delete it[change.path[i]];
        break;
    }
  }
}

function applyDiff(target, source, filter) {
  if (target && source) {
    var onChange = function(change) {
      if (!filter || filter(target, source, change)) {
        applyChange(target, source, change);
      }
    };
    deepDiff(target, source, onChange);
  }
}

Object.defineProperties(accumulateDiff, {

  diff: {
    value: accumulateDiff,
    enumerable: true
  },
  observableDiff: {
    value: deepDiff,
    enumerable: true
  },
  applyDiff: {
    value: applyDiff,
    enumerable: true
  },
  applyChange: {
    value: applyChange,
    enumerable: true
  },
  revertChange: {
    value: revertChange,
    enumerable: true
  },
  isConflict: {
    value: function() {
      return 'undefined' !== typeof conflict;
    },
    enumerable: true
  },
  noConflict: {
    value: function() {
      if (conflictResolution) {
        conflictResolution.forEach(function(it) {
          it();
        });
        conflictResolution = null;
      }
      return accumulateDiff;
    },
    enumerable: true
  }
});

return accumulateDiff;

})));

}).call(this,typeof global !== "undefined" ? global : typeof self !== "undefined" ? self : typeof window !== "undefined" ? window : {})
},{}],7:[function(require,module,exports){
// Copyright Joyent, Inc. and other Node contributors.
//
// Permission is hereby granted, free of charge, to any person obtaining a
// copy of this software and associated documentation files (the
// "Software"), to deal in the Software without restriction, including
// without limitation the rights to use, copy, modify, merge, publish,
// distribute, sublicense, and/or sell copies of the Software, and to permit
// persons to whom the Software is furnished to do so, subject to the
// following conditions:
//
// The above copyright notice and this permission notice shall be included
// in all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
// OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
// MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN
// NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
// DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
// OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE
// USE OR OTHER DEALINGS IN THE SOFTWARE.

function EventEmitter() {
  this._events = this._events || {};
  this._maxListeners = this._maxListeners || undefined;
}
module.exports = EventEmitter;

// Backwards-compat with node 0.10.x
EventEmitter.EventEmitter = EventEmitter;

EventEmitter.prototype._events = undefined;
EventEmitter.prototype._maxListeners = undefined;

// By default EventEmitters will print a warning if more than 10 listeners are
// added to it. This is a useful default which helps finding memory leaks.
EventEmitter.defaultMaxListeners = 10;

// Obviously not all Emitters should be limited to 10. This function allows
// that to be increased. Set to zero for unlimited.
EventEmitter.prototype.setMaxListeners = function(n) {
  if (!isNumber(n) || n < 0 || isNaN(n))
    throw TypeError('n must be a positive number');
  this._maxListeners = n;
  return this;
};

EventEmitter.prototype.emit = function(type) {
  var er, handler, len, args, i, listeners;

  if (!this._events)
    this._events = {};

  // If there is no 'error' event listener then throw.
  if (type === 'error') {
    if (!this._events.error ||
        (isObject(this._events.error) && !this._events.error.length)) {
      er = arguments[1];
      if (er instanceof Error) {
        throw er; // Unhandled 'error' event
      } else {
        // At least give some kind of context to the user
        var err = new Error('Uncaught, unspecified "error" event. (' + er + ')');
        err.context = er;
        throw err;
      }
    }
  }

  handler = this._events[type];

  if (isUndefined(handler))
    return false;

  if (isFunction(handler)) {
    switch (arguments.length) {
      // fast cases
      case 1:
        handler.call(this);
        break;
      case 2:
        handler.call(this, arguments[1]);
        break;
      case 3:
        handler.call(this, arguments[1], arguments[2]);
        break;
      // slower
      default:
        args = Array.prototype.slice.call(arguments, 1);
        handler.apply(this, args);
    }
  } else if (isObject(handler)) {
    args = Array.prototype.slice.call(arguments, 1);
    listeners = handler.slice();
    len = listeners.length;
    for (i = 0; i < len; i++)
      listeners[i].apply(this, args);
  }

  return true;
};

EventEmitter.prototype.addListener = function(type, listener) {
  var m;

  if (!isFunction(listener))
    throw TypeError('listener must be a function');

  if (!this._events)
    this._events = {};

  // To avoid recursion in the case that type === "newListener"! Before
  // adding it to the listeners, first emit "newListener".
  if (this._events.newListener)
    this.emit('newListener', type,
              isFunction(listener.listener) ?
              listener.listener : listener);

  if (!this._events[type])
    // Optimize the case of one listener. Don't need the extra array object.
    this._events[type] = listener;
  else if (isObject(this._events[type]))
    // If we've already got an array, just append.
    this._events[type].push(listener);
  else
    // Adding the second element, need to change to array.
    this._events[type] = [this._events[type], listener];

  // Check for listener leak
  if (isObject(this._events[type]) && !this._events[type].warned) {
    if (!isUndefined(this._maxListeners)) {
      m = this._maxListeners;
    } else {
      m = EventEmitter.defaultMaxListeners;
    }

    if (m && m > 0 && this._events[type].length > m) {
      this._events[type].warned = true;
      console.error('(node) warning: possible EventEmitter memory ' +
                    'leak detected. %d listeners added. ' +
                    'Use emitter.setMaxListeners() to increase limit.',
                    this._events[type].length);
      if (typeof console.trace === 'function') {
        // not supported in IE 10
        console.trace();
      }
    }
  }

  return this;
};

EventEmitter.prototype.on = EventEmitter.prototype.addListener;

EventEmitter.prototype.once = function(type, listener) {
  if (!isFunction(listener))
    throw TypeError('listener must be a function');

  var fired = false;

  function g() {
    this.removeListener(type, g);

    if (!fired) {
      fired = true;
      listener.apply(this, arguments);
    }
  }

  g.listener = listener;
  this.on(type, g);

  return this;
};

// emits a 'removeListener' event iff the listener was removed
EventEmitter.prototype.removeListener = function(type, listener) {
  var list, position, length, i;

  if (!isFunction(listener))
    throw TypeError('listener must be a function');

  if (!this._events || !this._events[type])
    return this;

  list = this._events[type];
  length = list.length;
  position = -1;

  if (list === listener ||
      (isFunction(list.listener) && list.listener === listener)) {
    delete this._events[type];
    if (this._events.removeListener)
      this.emit('removeListener', type, listener);

  } else if (isObject(list)) {
    for (i = length; i-- > 0;) {
      if (list[i] === listener ||
          (list[i].listener && list[i].listener === listener)) {
        position = i;
        break;
      }
    }

    if (position < 0)
      return this;

    if (list.length === 1) {
      list.length = 0;
      delete this._events[type];
    } else {
      list.splice(position, 1);
    }

    if (this._events.removeListener)
      this.emit('removeListener', type, listener);
  }

  return this;
};

EventEmitter.prototype.removeAllListeners = function(type) {
  var key, listeners;

  if (!this._events)
    return this;

  // not listening for removeListener, no need to emit
  if (!this._events.removeListener) {
    if (arguments.length === 0)
      this._events = {};
    else if (this._events[type])
      delete this._events[type];
    return this;
  }

  // emit removeListener for all listeners on all events
  if (arguments.length === 0) {
    for (key in this._events) {
      if (key === 'removeListener') continue;
      this.removeAllListeners(key);
    }
    this.removeAllListeners('removeListener');
    this._events = {};
    return this;
  }

  listeners = this._events[type];

  if (isFunction(listeners)) {
    this.removeListener(type, listeners);
  } else if (listeners) {
    // LIFO order
    while (listeners.length)
      this.removeListener(type, listeners[listeners.length - 1]);
  }
  delete this._events[type];

  return this;
};

EventEmitter.prototype.listeners = function(type) {
  var ret;
  if (!this._events || !this._events[type])
    ret = [];
  else if (isFunction(this._events[type]))
    ret = [this._events[type]];
  else
    ret = this._events[type].slice();
  return ret;
};

EventEmitter.prototype.listenerCount = function(type) {
  if (this._events) {
    var evlistener = this._events[type];

    if (isFunction(evlistener))
      return 1;
    else if (evlistener)
      return evlistener.length;
  }
  return 0;
};

EventEmitter.listenerCount = function(emitter, type) {
  return emitter.listenerCount(type);
};

function isFunction(arg) {
  return typeof arg === 'function';
}

function isNumber(arg) {
  return typeof arg === 'number';
}

function isObject(arg) {
  return typeof arg === 'object' && arg !== null;
}

function isUndefined(arg) {
  return arg === void 0;
}

},{}],8:[function(require,module,exports){
"use strict";
/*
 * surfaces.txt の内容を構造化したもの
 */
Object.defineProperty(exports, "__esModule", { value: true });
class SurfaceDefinitionTree {
    //regions: { [scopeID: number]: {[regionName: string]: ToolTipElement}; }; // 謎
    constructor(descript = new SurfaceDescript(), surfaces = [], aliases = []) {
        this.descript = descript;
        this.surfaces = surfaces;
        this.aliases = aliases;
    }
}
exports.SurfaceDefinitionTree = SurfaceDefinitionTree;
class SurfaceDescript {
    constructor(collisionSort = "ascend", animationSort = "ascend") {
        this.collisionSort = collisionSort;
        this.animationSort = animationSort;
    }
}
exports.SurfaceDescript = SurfaceDescript;
class SurfaceDefinition {
    constructor(elements = [], collisions = [], animations = [], balloons = { char: [], offsetX: 0, offsetY: 0 }, points = { basepos: { x: null, y: null }
        }) {
        this.elements = elements;
        this.collisions = collisions;
        this.animations = animations;
        this.points = points;
        this.balloons = balloons;
    }
    getRegion(offsetX, offsetY) {
        return getRegion(this.collisions, offsetX, offsetY);
    }
}
exports.SurfaceDefinition = SurfaceDefinition;
class SurfaceElement {
    constructor(type, file, x = 0, y = 0) {
        this.type = type;
        this.file = file;
        this.x = x;
        this.y = y;
    }
}
exports.SurfaceElement = SurfaceElement;
class SurfaceCollision {
    constructor(type, name) {
        this.name = name;
        this.type = type;
    }
}
exports.SurfaceCollision = SurfaceCollision;
class SurfaceCollisionRect extends SurfaceCollision {
    constructor(name, left, top, right, bottom) {
        super("rect", name);
        this.left = left;
        this.top = top;
        this.right = right;
        this.bottom = bottom;
    }
}
exports.SurfaceCollisionRect = SurfaceCollisionRect;
class SurfaceCollisionEllipse extends SurfaceCollision {
    constructor(name, left, top, right, bottom) {
        super("ellipse", name);
        this.left = left;
        this.top = top;
        this.right = right;
        this.bottom = bottom;
    }
}
exports.SurfaceCollisionEllipse = SurfaceCollisionEllipse;
class SurfaceCollisionCircle extends SurfaceCollision {
    constructor(name, centerX, centerY, radius) {
        super("circle", name);
        this.centerX = centerX;
        this.centerY = centerY;
        this.radius = radius;
    }
}
exports.SurfaceCollisionCircle = SurfaceCollisionCircle;
class SurfaceCollisionPolygon extends SurfaceCollision {
    constructor(name, coordinates) {
        super("polygon", name);
        this.coordinates = coordinates;
    }
}
exports.SurfaceCollisionPolygon = SurfaceCollisionPolygon;
class SurfaceAnimation {
    constructor(intervals = [["never", []]], options = [], collisions = [], patterns = []) {
        this.intervals = intervals;
        this.options = options;
        this.collisions = collisions;
        this.patterns = patterns;
    }
    isBack() { return isBack(this); }
    getExclusives() { return getExclusives(this); }
}
exports.SurfaceAnimation = SurfaceAnimation;
class SurfaceAnimationPattern {
    constructor(type = "ovelay", surface = -1, wait = [0, 0], x = 0, y = 0, animation_ids = []) {
        this.type = type;
        this.surface = surface;
        this.wait = wait;
        this.x = x;
        this.y = y;
        this.animation_ids = animation_ids;
    }
}
exports.SurfaceAnimationPattern = SurfaceAnimationPattern;
function isBack(anim) {
    return anim.options.some(([opt, args]) => opt === "background");
}
function getExclusives(anim) {
    return anim.options.filter(([opt, args]) => opt === "exclusive").reduce((l, [opt, args]) => l.concat(args), []);
}
function getRegion(collisions, offsetX, offsetY) {
    // このサーフェスの定義 surfaceNode.collision と canvas と座標を比較して
    // collision設定されていれば name"hoge"
    // basepos 左上からの座標の位置が透明かそうでないか、当たり判定領域か、名前があるかを調べる
    // offsetX: number, offsetY: number は surfaceCanvas.basePosX からの相対座標である必要がある、間違ってもcanvas左上からにしてはいけない 
    const hitCols = collisions.filter((collision, colId) => {
        const { type, name } = collision;
        switch (collision.type) {
            case "rect":
                const { left, top, right, bottom } = collision;
                return (left < offsetX && offsetX < right && top < offsetY && offsetY < bottom) ||
                    (right < offsetX && offsetX < left && bottom < offsetX && offsetX < top);
            case "ellipse":
                const o = collision;
                const width = Math.abs(o.right - o.left);
                const height = Math.abs(o.bottom - o.top);
                return Math.pow((offsetX - (o.left + width / 2)) / (width / 2), 2) +
                    Math.pow((offsetY - (o.top + height / 2)) / (height / 2), 2) < 1;
            case "circle":
                const { radius, centerX, centerY } = collision;
                return Math.pow((offsetX - centerX) / radius, 2) + Math.pow((offsetY - centerY) / radius, 2) < 1;
            case "polygon":
                const { coordinates } = collision;
                const ptC = { x: offsetX, y: offsetY };
                const tuples = coordinates.reduce(((arr, { x, y }, i) => {
                    arr.push([
                        coordinates[i],
                        (!!coordinates[i + 1] ? coordinates[i + 1] : coordinates[0])
                    ]);
                    return arr;
                }), []);
                // TODO: acos使わない奴に変える
                const deg = tuples.reduce(((sum, [ptA, ptB]) => {
                    const vctA = [ptA.x - ptC.x, ptA.y - ptC.y];
                    const vctB = [ptB.x - ptC.x, ptB.y - ptC.y];
                    const dotP = vctA[0] * vctB[0] + vctA[1] * vctB[1];
                    const absA = Math.sqrt(vctA.map((a) => Math.pow(a, 2)).reduce((a, b) => a + b));
                    const absB = Math.sqrt(vctB.map((a) => Math.pow(a, 2)).reduce((a, b) => a + b));
                    const rad = Math.acos(dotP / (absA * absB));
                    return sum + rad;
                }), 0);
                return deg / (2 * Math.PI) >= 1;
            default:
                console.warn("SurfaceTree.getRegion: unkown collision type:", this.surfaceId, colId, name, collision);
                return false;
        }
    });
    if (hitCols.length > 0) {
        return hitCols[hitCols.length - 1].name;
    }
    return "";
}

},{}],9:[function(require,module,exports){
module.exports={
  "name": "ikagaka-shell-state",
  "version": "5.0.0",
  "description": "Ukagaka Shell State Machine",
  "license": "MIT",
  "url": "https://github.com/ikagaka/ikagaka-shell-state.js",
  "keywords": [
    "nar",
    "ikagaka",
    "ukagaka"
  ],
  "scripts": {
    "setup": "npm install -g http-server",
    "init": "npm run update; npm run mkdir; npm run build",
    "update": "npm run reset; npm update",
    "reset": "rm -rf node_modules 2>/dev/null",
    "mkdir": "mkdir dist lib 2>/dev/null",
    "clean": "rm -rf dist/* lib/* 2>/dev/null",
    "build": "npm run clean; tsc -p .;                 browserify lib/index.js      --standalone ShellState -o dist/ikagaka-shell-state.js",
    "test": "npm run clean; tsc -p .; npm run espower; browserify lib/Test/index.js --standalone Test  -o dist/test.js",
    "play": "npm run clean; tsc -p .;                  browserify lib/Test/playground.js --standalone Play  -o dist/playground.js",
    "espower": "for file in `find lib/Test -name *.js`; do espower $file > $file.tmp; mv -f $file.tmp $file; done",
    "check": "tsc -w --noEmit -p ./",
    "lint": "tslint -c ./tslint.json --project ./tsconfig.json --type-check",
    "doc": "typedoc --mode modules --out doc --disableOutputCheck"
  },
  "repository": {
    "type": "git",
    "url": "https://github.com/ikagaka/ikagaka/ikagaka-shell-state.js.git"
  },
  "dependencies": {
    "@types/deep-diff": "0.0.30",
    "deep-diff": "^0.3.6",
    "ikagaka-shell-loader": "github:ikagaka/ikagaka-shell-loader"
  },
  "devDependencies": {
    "jquery": "^3.2.1",
    "@types/node": "0.0.3",
    "@types/power-assert-formatter": "^1.4.28",
    "@types/qunit": "^2.0.31",
    "browserify": "^13.1.0",
    "empower": "^1.2.1",
    "espower-cli": "^1.1.0",
    "narloader": "github:ikagaka/NarLoader#jszip3",
    "typescript": "^2.0.0",
    "typedoc": "^0.5.3",
    "tslint": "^3.13.0-dev.0",
    "power-assert": "^1.4.1",
    "power-assert-formatter": "^1.4.1",
    "qunit-tap": "^1.5.1",
    "qunitjs": "^2.0.1"
  },
  "babel": {
    "presets": [
      "es2015"
    ]
  },
  "bugs": {
    "url": "https://github.com/Ikagaka/ikagaka-shell-state/issues"
  },
  "typings": "./lib/index.d.ts",
  "main": "./lib/index.js",
  "author": "Ikagaka",
  "contributors": [
    "legokichi",
    "narazaka"
  ]
}

},{}]},{},[5])(5)
});
