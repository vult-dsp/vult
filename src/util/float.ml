(*
The MIT License (MIT)

Copyright (c) 2017 Leonardo Laguna Ruiz

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

let reduce_precision = ref false

let to_string (f:float) =
   if not !reduce_precision then
      string_of_float f
   else
      Printf.sprintf "%.5f" f

let crop (f:float) =
   if not !reduce_precision then f
   else
      let nf = int_of_float (f *. 10000.0) in
      let ff = (float_of_int nf) /. 10000.0 in
      match classify_float ff with
      | FP_normal -> ff
      | FP_subnormal -> 0.0
      | FP_zero -> 0.0
      | _ -> f