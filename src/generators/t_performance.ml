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

let time = 100.0

(** Header function *)
let implPre (args : Util.Args.args) : Pla.t =
  let output = Option.value args.output ~default:"output" in
  {%pla|
#include "<#output#s>.h"
#include "vultin.h"
#include <time.h>
#include <stdio.h>
|}


let implPost (args : Util.Args.args) : Pla.t =
  let real = if args.real = Fixed then "fx" else "fl" in
  let module_name =
    match args.files with
    | Util.Args.File s :: _ -> Pparser.Parse.moduleName s
    | _ -> "Top"
  in
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
  (implPre args, implPost args), (Pla.unit, Pla.unit)


(*
   let mainJs params =
   let output = params.output in
   let module_name = params.module_name in
   [%pla
      {|var code = require("./<#output#s>.js");
        var vultProcess = new code.vultProcess();
        var data = vultProcess.<#module_name#s>_process_init();
        vultProcess.<#module_name#s>_default();

        var time = <#time#f>;
        var samples = 44100 * time;

        var start = new Date();
        while (samples > 0) {
        vultProcess.<#module_name#s>_process(data, 0.0);
        samples--;
        }
        var end = (new Date() - start);
        console.info("<#module_name#s>\tJs\t", end / time, "ms/s");
   |}]
;;

let implJs params code runtime =
   let module_name = params.module_name in
   [%pla
      {|exports.vultProcess = function () {
        <#runtime#>
        this.<#module_name#s>_process_init = null;
        this.<#module_name#s>_default = null;
        <#code#>
        if(this.<#module_name#s>_process_init)  this.context =  this.<#module_name#s>_process_init(); else this.context = {};
        if(this.<#module_name#s>_default)      this.<#module_name#s>_default(this.context);
        this.liveNoteOn        = function(note, velocity, channel) { if(this.<#module_name#s>_noteOn)         this.<#module_name#s>_noteOn(this.context,note,velocity,channel); };
        this.liveNoteOff       = function(note, velocity, channel) { if(this.<#module_name#s>_noteOff)       this.<#module_name#s>_noteOff(this.context,note,velocity,channel); };
        this.liveControlChange = function(note, velocity, channel) { if(this.<#module_name#s>_controlChange) this.<#module_name#s>_controlChange(this.context,note,velocity,channel); };
        this.liveProcess       = function(input)         { if(this.<#module_name#s>_process)       return this.<#module_name#s>_process(this.context,input); else return 0; };
        this.liveDefault       = function() { if(this.<#module_name#s>_default)      return this.<#module_name#s>_default(this.context); };
        }|}]
;;

let getJs (params : params) runtime code : (Pla.t * FileKind.t) list =
   [ mainJs params, FileKind.FullName "main.js"; implJs params code runtime, FileKind.ExtOnly "js" ]
;;
*)

let luaPost (args : Util.Args.args) =
  let module_name =
    match args.files with
    | Util.Args.File s :: _ -> Pparser.Parse.moduleName s
    | _ -> "Top"
  in
  {%pla|
     data = <#module_name#s>_process_type_alloc()
     <#module_name#s>_default(data)
     time = <#time#f>
     samples = 44100 * time
     local start = os.clock()
     while samples > 0 do
        <#module_name#s>_process(data, 0.0)
        samples = samples -1
     end
     local finish = (os.clock() - start) * 1000.0
     print("<#module_name#s>\tLua", finish / time, "ms/s")
     |}


let generateLua (args : Util.Args.args) = Pla.unit, luaPost args
