window.addEventListener("load", function () {
    ShellLoader.load({
        "/shell/master/surface0.png": fetch("/worldview/teiri/shell/master/surface0.png").then(function (res) { return res.arrayBuffer(); }),
        "/shell/master/surface10.png": fetch("/worldview/teiri/shell/master/surface10.png").then(function (res) { return res.arrayBuffer(); }),
        "/shell/master/descript.txt": fetch("/worldview/teiri/shell/master/descript.txt").then(function (res) { return res.arrayBuffer(); }),
        "/shell/master/surfaces.txt": fetch("/worldview/teiri/shell/master/surfaces.txt").then(function (res) { return res.arrayBuffer(); })
    }).then(function (shell) {
        console.log(shell);
        console.log("a1");
        shell.config.enableRegion = true;
        const shellState = new SHS.ShellState(shell);
        console.log("a2");
        const baseCache = new SurfaceBaseRenderer(shell);
        console.log("a3");
        const scopeId = 0;
        const surfaceId = 0;
        return baseCache.getBaseSurfaceSize(surfaceId).then(function (size) {
            console.log("a4");
            const width = size.width;
            const height = size.height;
            const realCanvas = Util.createCanvas(width, height);
            console.log("a5");
            document.body.appendChild(realCanvas);
            console.log("a6");
            const rndr = new SurfacePatternRenderer(baseCache);
            console.log("a7");
            rndr.attachCanvas(new Canvas(realCanvas));
            console.log("a8");
            rndr.offscreen.debug = true;
            console.log("a9");
            const srfState = shellState.createSurfaceState(scopeId, surfaceId, function (_, surfaceId, tree) { rndr.render(surfaceId, tree); return Promise.resolve(); });
            console.log("a10");
            srfState.debug = true;
            console.log(srfState);
            console.log("a11");
            return srfState.render().then(function (res) {
                console.log("a12");
                console.log(res);
                return res;
            });
        });
    });
});
