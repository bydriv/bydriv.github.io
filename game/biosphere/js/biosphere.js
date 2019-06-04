

window.addEventListener("DOMContentLoaded", () => {
    const canvas = document.getElementById("biosphere");
    
    new Recorder(canvas);
    // const stats = new Stats();
    // stats.showPanel( 0 ); // 0: fps, 1: ms, 2: mb, 3+: custom
    // document.body.appendChild( stats.domElement );
    
    const offscreenCanvas = canvas.transferControlToOffscreen();
    const worker = new Worker("./js/worker.js");
    worker.postMessage({ action: "init", canvas: offscreenCanvas }, [ offscreenCanvas ]);

    // function recur(){
    //     stats.begin();
        
    //     stats.end();
    //     requestAnimationFrame(recur);
    // }
    // recur();
});

class Recorder{
    constructor(canvas){
        this.recording = false;
        this.recorder = null;
        this.record_mode = document.getElementById("record-mode");
        this.record_mode.addEventListener("change", (ev)=>{
            if (this.record_mode.checked) {
                const canvasStream = canvas;
                this.recorder = new MediaRecorder(canvasStream, {mimeType: "video/webm"});
                const chunks = [];

                this.recorder.addEventListener("dataavailable", function (e) {
                    chunks.push(e.data);
                });

                this.recorder.addEventListener("stop", function () {
                    const blob = new Blob(chunks, {"type": "video/webm"});
                    const url = URL.createObjectURL(blob);
                    const video = document.createElement("video");
                    video.src = url;
                    video.controls = true;
                    const a = document.createElement("a");
                    const text = document.createTextNode("download");
                    a.href = url;
                    a.appendChild(text);
                    document.querySelector("#downloads").appendChild(video);
                    document.querySelector("#downloads").appendChild(a);
                    recorder = null;
                });

                this.recorder.start();
                this.recording = true;
            }else{
                this.recorder.stop();
                this.recording = false;
            }
        });
    }
}
