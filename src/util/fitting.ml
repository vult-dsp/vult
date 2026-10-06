(*
   The MIT License (MIT)

   Copyright (c) 2014-2024 Leonardo Laguna Ruiz, Carl Jönsson

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

(* Coefficients (c0, c1) of the line c0 + c1 * x that passes through the two points *)
let lagrange2 ((x0, y0) : float * float) ((x1, y1) : float * float) : float * float =
  let den = x0 -. x1 in
  let c0 = ((x0 *. y1) -. (x1 *. y0)) /. den in
  let c1 = (y0 -. y1) /. den in
  (c0, c1)

(* Coefficients (c0, c1, c2) of the parabola c0 + c1 * x + c2 * x^2 that passes through the three points *)
let lagrange3 ((x1, y1) : float * float) ((x2, y2) : float * float) ((x3, y3) : float * float) : float * float * float =
  let den = (x1 -. x2) *. (x1 -. x3) *. (x2 -. x3) in
  let c0 =
    ((x2 *. (x2 -. x3) *. x3 *. y1) +. (x1 *. x3 *. (x3 -. x1) *. y2) +. (x1 *. (x1 -. x2) *. x2 *. y3)) /. den
  in
  let c1 = ((x3 *. x3 *. (y1 -. y2)) +. (x1 *. x1 *. (y2 -. y3)) +. (x2 *. x2 *. (y3 -. y1))) /. den in
  let c2 = ((x3 *. (y2 -. y1)) +. (x2 *. (y1 -. y3)) +. (x1 *. (y3 -. y2))) /. den in
  (c0, c1, c2)
