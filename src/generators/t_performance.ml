(*
   The MIT License (MIT)

   Copyright (c) 2014 Leonardo Laguna Ruiz

   Permission is hereby granted, free of charge, to any person obtaining a copy
   of this software and associated documentation files (the "Software"), to deal
   in the Software without restriction, including without limitation the rights
   to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
   copies of the Software, and to permit persons to whom the Software is
   furnished to do so, subject to the following conditions:

   The above copyright notice and this permission notice shall be included in
   all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
   IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
   FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
   AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
   LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
   OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
   THE SOFTWARE.
*)

let time = 20.0

(** Header function *)
let implPre (args : Util.Args.args) : Pla.t =
  let output = Option.value args.output ~default:"output" in
  {%pla|
#include "<#output#s>.h"
#include "vultin.hpp"
#include <time.h>
#include <stdio.h>
|}

let implPost (args : Util.Args.args) : Pla.t =
  let real = if args.real = Fixed then "fx" else "fl" in
  let module_name = match args.files with Util.Args.File s :: _ -> Pparser.Parse.moduleName s | _ -> "Top" in
  {%pla|
int main(void)
{
   <#module_name#s>_process_type data;
   <#module_name#s>_process_type_init(data);
   <#module_name#s>_default(data);
   float time = <#time#f>;
   int samples = 44100 * (int)time;
   clock_t start = clock();
   float ramp = 0.0f;
   volatile float acc = 0.0f;
   while (samples > 0)
   {
      ramp += 0.001f;
      if (ramp > 1.0f)
          ramp = ramp - 1.0f;
      acc += <#module_name#s>_process(data, ramp);
      samples--;
   }
   clock_t diff = clock() - start;
   float sec = (diff * 1000.0f / CLOCKS_PER_SEC) / 1000.0f;
   printf("<#module_name#s>\tC++ <#real#s>\t%f ms/s\n", (sec / time) * 1000.0);
   return 0;
}
|}

let generateC (args : Util.Args.args) : (Pla.t * Pla.t) * (Pla.t * Pla.t) =
  ((implPre args, implPost args), (Pla.unit, Pla.unit))

let luaPost (args : Util.Args.args) =
  let module_name = match args.files with Util.Args.File s :: _ -> Pparser.Parse.moduleName s | _ -> "Top" in
  {%pla|
     -- Performance measurement with LuaJIT detection
     local engine = isLuaJIT and "LuaJIT" or "Lua"

     data = <#module_name#s>_process_type_alloc()
     <#module_name#s>_default(data)
     time = <#time#f>
     samples = 44100 * time

     -- Warm up - run more iterations for LuaJIT to allow proper compilation
     local warmup_iterations = isLuaJIT and 5000 or 1000
     for i = 1, warmup_iterations do
        <#module_name#s>_process(data, 0.0, 0.5, 0.5)
     end

     local start = os.clock()
     local ramp = 0.0
     local acc = 0.0
     while samples > 0 do
        ramp = ramp + 0.001
        if ramp > 1.0 then ramp = ramp - 1.0 end
        acc = acc + <#module_name#s>_process(data, ramp, 0.5, 0.5)
        samples = samples -1
     end
     local finish = (os.clock() - start) * 1000.0
     print(string.format("<#module_name#s>\t%s\t%.2f ms/s", engine, finish / time))
     |}

let generateLua (args : Util.Args.args) = (Pla.unit, luaPost args)

let jsPostWithLabel (args : Util.Args.args) (label : string) =
  let module_name = match args.files with Util.Args.File s :: _ -> Pparser.Parse.moduleName s | _ -> "Top" in
  {%pla|
var data = this.<#module_name#s>_process_type_alloc();
this.<#module_name#s>_default(data);
var time = <#time#f>;
var samples = 44100 * time;

// Warm up - run a few iterations to allow V8 JIT compilation
for (var i = 0; i < 1000; i++) {
  this.<#module_name#s>_process(data, 0.0);
}

var start = process.hrtime.bigint();
while (samples > 0) {
  this.<#module_name#s>_process(data, 0.0);
  samples = samples -1;
}
var finish = Number(process.hrtime.bigint() - start) / 1000000 / time;
console.log(`<#module_name#s>\t<#label#s>\t${finish.toFixed(2)} ms/s`)
|}

let jsPost (args : Util.Args.args) = jsPostWithLabel args "Js"

let jsBunPost (args : Util.Args.args) = jsPostWithLabel args "Bun"

let generateJs (args : Util.Args.args) = (Pla.unit, jsPost args)

let generateJsBun (args : Util.Args.args) = (Pla.unit, jsBunPost args)

let juliaPost (args : Util.Args.args) =
  let module_name = match args.files with Util.Args.File s :: _ -> Pparser.Parse.moduleName s | _ -> "Top" in
  {%pla|
# Performance measurement for <#module_name#s>
function measure_performance()
    # Initialize the process state
    data = <#module_name#s>_process_type_alloc()

    # Setup timing parameters
    time_seconds = <#time#f>
    sample_rate = 44100
    samples = Int(sample_rate * time_seconds)

    # Warm up - run a few iterations to allow Julia JIT compilation
    for i in 1:1000
        result = <#module_name#s>_process(data, 0.0)
    end

    # Actual performance measurement
    start_time = time_ns()
    acc = 0.0
    ramp = 0.0

    for i in 1:samples
        ramp += 0.001
        if ramp > 1.0
            ramp = ramp - 1.0
        end
        acc += <#module_name#s>_process(data, ramp)
    end

    elapsed_ns = time_ns() - start_time
    elapsed_seconds = elapsed_ns / 1_000_000_000
    ms_per_second = (elapsed_seconds / time_seconds) * 1000.0

    println("<#module_name#s>\tJulia\t$(round(ms_per_second, digits=2)) ms/s")

    # Return accumulated value to prevent dead code elimination
    return acc
end

# Run the performance measurement
measure_performance()
|}

let generateJulia (args : Util.Args.args) = (Pla.unit, juliaPost args)

let javaPost (args : Util.Args.args) =
  let module_name = match args.files with Util.Args.File s :: _ -> Pparser.Parse.moduleName s | _ -> "Top" in
  let class_name =
    match args.output with
    | Some output ->
        String.capitalize_ascii (Filename.basename (Filename.remove_extension output))
    | None ->
        "VultCode"
  in
  {%pla|
public class <#module_name#s>Perf {
    public static void main(String[] args) {
        <#class_name#s> vult = new <#class_name#s>();
        <#class_name#s>.<#module_name#s>_process_type data = vult.<#module_name#s>_process_type_alloc();
        vult.<#module_name#s>_default(data);

        double time = <#time#f>;
        int samples = (int)(44100 * time);

        // Warm up - run iterations to allow JVM JIT compilation
        for (int i = 0; i < 10000; i++) {
            vult.<#module_name#s>_process(data, (float)0.0);
        }

        // Actual performance measurement
        long startTime = System.nanoTime();
        double acc = 0.0;
        double ramp = 0.0;

        for (int i = 0; i < samples; i++) {
            ramp += 0.001;
            if (ramp > 1.0) {
                ramp = ramp - 1.0;
            }
            acc += vult.<#module_name#s>_process(data, (float)ramp);
        }

        long endTime = System.nanoTime();
        double elapsedSeconds = (endTime - startTime) / 1_000_000_000.0;
        double msPerSecond = (elapsedSeconds / time) * 1000.0;

        System.out.printf("<#module_name#s>\tJava\t%.2f ms/s\n", msPerSecond);
    }
}
|}

let generateJava (args : Util.Args.args) = (Pla.unit, javaPost args)

let pythonPost (args : Util.Args.args) =
  let module_name = match args.files with Util.Args.File s :: _ -> Pparser.Parse.moduleName s | _ -> "Top" in
  {%pla|
# Performance measurement for <#module_name#s>
import time

def measure_performance():
    # Initialize the process state
    data = <#module_name#s>_process_type_alloc()
    <#module_name#s>_default(data)

    # Setup timing parameters
    time_seconds = <#time#f>
    sample_rate = 44100
    samples = int(sample_rate * time_seconds)

    # Warm up - run a few iterations
    for i in range(1000):
        <#module_name#s>_process(data, 0.0)

    # Actual performance measurement
    start_time = time.perf_counter()
    acc = 0.0
    ramp = 0.0

    for i in range(samples):
        ramp += 0.001
        if ramp > 1.0:
            ramp = ramp - 1.0
        acc += <#module_name#s>_process(data, ramp)

    elapsed_seconds = time.perf_counter() - start_time
    ms_per_second = (elapsed_seconds / time_seconds) * 1000.0

    print(f"<#module_name#s>\tPython\t{ms_per_second:.2f} ms/s")

    # Return accumulated value to prevent dead code elimination
    return acc

# Run the performance measurement
measure_performance()
|}

let generatePython (args : Util.Args.args) = (Pla.unit, pythonPost args)
