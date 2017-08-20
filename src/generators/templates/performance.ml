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


open GenerateParams

(** Header function *)
let main (params:params) : Pla.t =
   let output = params.output in
   let module_name = params.module_name in
   let real = params.real in
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
   float time = 50.0;
   int samples = 44100 * (int)time;
   clock_t start = clock(), diff;
   while (samples > 0)
   {
      <#module_name#s>_process(data, 0.0);
      samples--;
   }
   diff = clock() - start;
   float sec = (diff * 1000 / CLOCKS_PER_SEC) / 1000.0;
   printf("%s\t%s\t%f ms/s\n", "<#module_name#s>", "<#real#s>", (sec / time) * 1000.0);
   return 0;
}
|pla}

let impl params impl_code =
   let output = params.output in
   {pla|
   #include "vultin.h"
   #include "<#output#s>.h"

   <#impl_code#>
   |pla}

let header _ header_code =
   {pla|
   #include "math.h"
   #include "stdint.h"
   #include "vultin.h"

   <#header_code#>
|pla}

let getC (params:params) (header_code:Pla.t) (impl_code:Pla.t) : (Pla.t * filename) list =
   [
      header params header_code, ExtOnly "h";
      impl params impl_code, ExtOnly "cpp";
      main params, FullName "main.cpp";
   ]