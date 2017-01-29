var bench = require("./bench.js");
var vultProcess = new bench.vultProcess();
var data = vultProcess.Bench_process_init();

var time = 44100 * 100;

while (time > 0) {
   vultProcess.Bench_process(data);
   time--;
}