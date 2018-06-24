window.addEventListener("load", function () {
    var teiri = document.querySelectorAll(".teiri");
    var teiri_petan = document.querySelectorAll(".teiri_petan");

    for (var i = 0; i < teiri.length; ++i) {
        (function () {
        const width = 320;
        const height = 240;
        const renderer = new THREE.WebGLRenderer({
            canvas: teiri[i]
        });
        renderer.setPixelRatio(window.devicePixelRatio);
        renderer.setSize(width, height);
        const scene = new THREE.Scene();
        const camera = new THREE.PerspectiveCamera(45, width / height, 0.1, 10000);
        camera.position.set(0, 3, 10);
        const controls = new THREE.OrbitControls(camera);
        controls.target.set(0, 0, 0);
        controls.update();
        const ambientLight = new THREE.AmbientLight(0xFFFFFF);
        scene.add(ambientLight);
        const loader = new THREE.ColladaLoader();
        var model = null;
        loader.load('/worldview/teiri.dae', (collada) => {
            model = collada.scene;
            scene.add(model);
        });
        tick();
        function tick() {
            renderer.render(scene, camera);

            if (model) {
                model.rotation.z -= 1.0/60;
            }

            requestAnimationFrame(tick);
        }
        })();
    }

    for (var i = 0; i < teiri_petan.length; ++i) {
        (function () {
        const width = 320;
        const height = 240;
        const renderer = new THREE.WebGLRenderer({
            canvas: teiri_petan[i]
        });
        renderer.setPixelRatio(window.devicePixelRatio);
        renderer.setSize(width, height);
        const scene = new THREE.Scene();
        const camera = new THREE.PerspectiveCamera(45, width / height, 0.1, 10000);
        camera.position.set(0, 3, 10);
        const controls = new THREE.OrbitControls(camera);
        controls.target.set(0, 0, 0);
        controls.update();
        const ambientLight = new THREE.AmbientLight(0xFFFFFF);
        scene.add(ambientLight);
        const loader = new THREE.ColladaLoader();
        var model = null;
        loader.load('/worldview/teiri_petan.dae', (collada) => {
            model = collada.scene;
            scene.add(model);
        });
        tick();
        function tick() {
            renderer.render(scene, camera);

            if (model) {
                model.rotation.z -= 1.0/60;
            }

            requestAnimationFrame(tick);
        }
        })();
    }
});
