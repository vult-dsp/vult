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

open Config

(** Header function *)
let mainC (params:params) : Pla.t =
   let output = params.output in
   let module_name = params.module_name in
   let real = if params.real = "fixed" then "fx" else "fl" in
   {pla|
#include "<#output#s>.h"
#include "vultin.h"
#include <time.h>
#include <stdio.h>
int main(void)
{
   <#module_name#s>_process_type data;
   <#module_name#s>_process_init(data);
   <#module_name#s>_default(data);
   float time = <#time#f>;
   int samples = 44100 * (int)time;
   clock_t start = clock(), diff;
   while (samples > 0)
   {
      <#module_name#s>_process(data, 0.0);
      samples--;
   }
   diff = clock() - start;
   float sec = (diff * 1000 / CLOCKS_PER_SEC) / 1000.0;
   printf("<#module_name#s>\tC++ <#real#s>\t%f ms/s\n", (sec / time) * 1000.0);
   return 0;
}
|pla}

let implC params impl_code =
   let output = params.output in
   {pla|
   #include "vultin.h"
   #include "<#output#s>.h"

   <#impl_code#>
   |pla}

let headerC _ header_code =
   {pla|
   #include "math.h"
   #include "stdint.h"
   #include "vultin.h"

   <#header_code#>
|pla}

let getC (params:params) (header_code:Pla.t) (impl_code:Pla.t) : (Pla.t * FileKind.t) list =
   [
      headerC params header_code, FileKind.ExtOnly "h";
      implC params impl_code, FileKind.ExtOnly "cpp";
      mainC params, FileKind.FullName "main.cpp";
   ]


let mainJs params =
   let output = params.output in
   let module_name = params.module_name in
   {pla|var code = require("./<#output#s>.js");
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
   |pla}

let implJs params code runtime =
   let module_name = params.module_name in
   {pla|exports.vultProcess = function () {
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
        }|pla}


let getJs (params:params) runtime code : (Pla.t * FileKind.t) list =
   [
      mainJs params, FileKind.FullName "main.js";
      implJs params code runtime, FileKind.ExtOnly "js"
   ]


let mainLua (params:params) =
   let output = params.output in
   let module_name = params.module_name in
   {pla|
   vult = loadfile("./<#output#s>.lua")()
   data = vult.<#module_name#s>_process_init()
   vult.<#module_name#s>_default(data)
   time = <#time#f>
   samples = 44100 * time
   local start = os.clock()
   while samples > 0 do
      vult.<#module_name#s>_process(data, 0.0)
      samples = samples -1
   end
   local finish = (os.clock() - start) * 1000.0
   print("<#module_name#s>\tLua", finish / time, "ms/s")
   |pla}

let getLua (params:params) default_code : (Pla.t * FileKind.t) list =
   [
      default_code, FileKind.ExtOnly "lua";
      mainLua params, FileKind.FullName "main.lua"
   ]
