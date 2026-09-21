(*
   The MIT License (MIT)

   Copyright (c) 2014-2024 Leonardo Laguna Ruiz

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

(* Precision of the target language's floating point type. Literals emitted for
   a single precision target must fit in a 'float', otherwise compilers report
   them as out of range. *)
type precision = Single | Double

let max_single = 3.40282347E+38

(* Smallest positive normal single precision value. Anything below it either
   underflows to zero or becomes a denormal, which is both a compiler warning
   and a performance hazard in audio code. *)
let min_normal_single = 1.17549435E-38

let crop (f : float) =
  if !reduce_precision then
    let ff = f *. 1000000.0 in
    floor ff /. 1000000.0
  else f

let fit_single (f : float) =
  if Float.abs f < min_normal_single then 0.0
  else if f > max_single then max_single
  else if f < -.max_single then -.max_single
  else f

let adapt (precision : precision) (f : float) =
  crop
  @@
  match Float.classify_float f with
  | FP_normal | FP_subnormal -> (
    match precision with Single -> fit_single f | Double -> f )
  | FP_zero ->
      0.0
  | FP_infinite ->
      if f > 0.0 then max_single else -.max_single
  | FP_nan ->
      failwith "nan"

let to_string (precision : precision) (f : float) =
  let r = Float.to_string (adapt precision f) in
  if String.ends_with ~suffix:"." r then r ^ "0" else r
