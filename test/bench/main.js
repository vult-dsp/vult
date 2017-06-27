var bench = require("./bench.js");
var vultProcess = new bench.vultProcess();
var data = vultProcess.Bench_process_init();

var time = 44100 * 200;

var start = new Date();
while (time > 0) {
   vultProcess.Bench_process(data);
   time--;
}
var end = new Date() - start;
console.info("\n### Js Execution time: %d", end / 1000);