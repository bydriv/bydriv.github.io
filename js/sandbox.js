window.addEventListener("load", function () {
    const width = 320;
    const height = 240;
    const renderer = new THREE.WebGLRenderer({
        canvas: document.getElementById("teiri-3d")
    });
    renderer.setPixelRatio(window.devicePixelRatio);
    renderer.setSize(width, height);
    const scene = new THREE.Scene();
    const camera = new THREE.PerspectiveCamera(45, width / height, 0.1, 10000);
    camera.position.set(0, 3, 3);
    const controls = new THREE.OrbitControls(camera);
    controls.target.set(0, 3, 0);
    controls.update();
    //const directionalLight = new THREE.DirectionalLight(0xFFFFFF);
    //directionalLight.position.set(0, 0, 1);
    //scene.add(directionalLight);
    //const ambientLight = new THREE.AmbientLight(0x333333);
    const ambientLight = new THREE.AmbientLight(0xFFFFFF);
    scene.add(ambientLight);
    const loader = new THREE.ColladaLoader();
    loader.load('/sandbox/teiri.dae', (collada) => {
        const model = collada.scene;
        //model.applyMatrix(new THREE.Matrix4().makeScale(1, 1, -1));
        scene.add(model);
    });
    tick();
    function tick() {
        renderer.render(scene, camera);
        requestAnimationFrame(tick);
    }
});
