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

let lagrange (x_points : float list) (y_points : float list) : float list =
  match x_points, y_points with
  | [ x1; x2; x3 ], [ y1; y2; y3 ] ->
    let den = (x1 -. x2) *. (x1 -. x3) *. (x2 -. x3) in
    let c0 =
      ((x2 *. (x2 -. x3) *. x3 *. y1) +. (x1 *. x3 *. (x3 -. x1) *. y2) +. (x1 *. (x1 -. x2) *. x2 *. y3)) /. den
    in
    let c1 = ((x3 *. x3 *. (y1 -. y2)) +. (x1 *. x1 *. (y2 -. y3)) +. (x2 *. x2 *. (y3 -. y1))) /. den in
    let c2 = ((x3 *. (y2 -. y1)) +. (x2 *. (y1 -. y3)) +. (x1 *. (y3 -. y2))) /. den in
    [ c0; c1; c2 ]
  | [ x0; x1 ], [ y0; y1 ] ->
    let den = x0 -. x1 in
    let c0 = ((x0 *. y1) -. (x1 *. y0)) /. den in
    let c1 = (y0 -. y1) /. den in
    [ c0; c1 ]
  | _ -> failwith "Fitting.lagrange: invalid input"
