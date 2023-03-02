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

let crop (f : float) =
  if !reduce_precision then (
    let ff = f *. 10000000.0 in
    ceil ff /. 10000000.0)
  else
    f


let adapt (f : float) =
  crop
  @@
  match Float.classify_float f with
  | FP_normal -> f
  | FP_subnormal -> f
  | FP_zero -> 0.0
  | FP_infinite -> if f > 0.0 then 3.40282347E+38 else -3.40282347E+38
  | FP_nan -> failwith "nan"


let to_string (f : float) =
  Float.to_string
  @@ crop
  @@
  match Float.classify_float f with
  | FP_normal -> f
  | FP_subnormal -> f
  | FP_zero -> 0.0
  | FP_infinite -> if f > 0.0 then 3.40282347E+38 else -3.40282347E+38
  | FP_nan -> failwith "nan"
