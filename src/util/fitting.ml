(*
The MIT License (MIT)

Copyright (c) 2014 Leonardo Laguna Ruiz, Carl Jönsson

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

(* This module contains *)

let set m r c v =
   Array.set (Array.get m (r-1)) (c-1) v

let get m r c =
   Array.get (Array.get m (r-1)) (c-1)

let make rows columns =
   Array.init rows (fun _ -> Array.make columns 0.0)

let inverse3 (m:float array array) =
   let result = make 3 3 in
   let get (r,c) = get m r c in
   let r (r,c) v = set result r c v in
   let a = get (1,1) in
   let b = get (1,2) in
   let c = get (1,3) in
   let d = get (2,1) in
   let e = get (2,2) in
   let f = get (2,3) in
   let g = get (3,1) in
   let h = get (3,2) in
   let i = get (3,3) in
   let det = -. c *. e *. g +. b *. f *. g +. c *. d *. h -. a *. f *. h -. b *. d *. i +. a *. e *. i in
   r(1,1) (( -.f *. h +. e *. i) /. det);
   r(1,2) (( c *. h -. b *. i) /. det);
   r(1,3) (( -.c *. e +. b *. f ) /. det);
   r(2,1) ((  f *. g -. d *. i) /. det);
   r(2,2) (( -.c *. g +. a *. i) /. det);
   r(2,3) (( c *. d -. a *. f) /. det);
   r(3,1) (( -.e *. g +. d *. h) /. det);
   r(3,2) (( b *. g -. a *. h) /. det);
   r(3,3) (( -.b *. d +. a *. e) /. det);
   result

let transposed3 (m:float array array) =
   let result = make 3 3 in
   let get (r,c) = get m r c in
   let r (r,c) v = set result r c v in
   let a = get (1,1) in
   let b = get (1,2) in
   let c = get (1,3) in
   let d = get (2,1) in
   let e = get (2,2) in
   let f = get (2,3) in
   let g = get (3,1) in
   let h = get (3,2) in
   let i = get (3,3) in
   r(1,1) a;
   r(1,2) d;
   r(1,3) g;
   r(2,1) b;
   r(2,2) e;
   r(2,3) h;
   r(3,1) c;
   r(3,2) f;
   r(3,3) i;
   result

let multiply x y =
   let x0 = Array.length x in
   let y0 = Array.length y in
   let y1 = if y0 = 0 then 0 else Array.length y.(0) in
   let z = make x0 y1 in
   for i = 1 to x0 do
      for j = 1 to y1 do
         for k = 1 to y0 do
            set z i j (get z i j +. get x i k *. get y k j)
         done
      done
   done;
   z

(** Performs second order curve fitting of three points *)
let fit (x_points:float list) (y_points:float list) : float list =
   match List.length x_points, List.length y_points with
   | 3, 3 ->
      (* prepare the matrix X *)
      let x =
         Array.init 3 (fun i ->
               let xi = List.nth x_points i in
               Array.init 3 (fun p -> xi ** (float_of_int p)))
      in
      (* prepare vector y *)
      let y = List.map (fun a -> [|a|]) y_points |> Array.of_list in
      (* solve with a = (XT X)^-1 XT y *)
      let xt = transposed3 x in
      let a = multiply (multiply (inverse3 (multiply xt x)) xt) y in
      (* Convert the matrix to vector *)
      let result =
         Array.map (fun a -> Array.to_list a) a
         |> Array.to_list
         |> List.flatten
      in
      result
   | _ -> failwith "Fitting is only implemented for second order"

let lagrange (x_points:float list) (y_points:float list) : float list =
   match x_points, y_points with
   | [x1; x2; x3], [y1; y2; y3] ->
      let den = (x1 -. x2) *. (x1 -. x3) *. (x2 -. x3) in
      let c0 = (x2 *. (x2 -. x3) *. x3 *. y1 +. x1 *. x3 *. (x3 -. x1) *. y2 +. x1 *. (x1 -. x2) *. x2 *. y3) /. den in
      let c1 = (x3 *. x3 *. (y1 -. y2) +. x1 *. x1 *. (y2 -. y3) +. x2 *. x2 *. (y3 -. y1)) /. den in
      let c2 = (x3 *. (y2 -. y1) +. x2 *. (y1 -. y3) +. x1 *. (y3 -. y2)) /. den in
      [c0; c1; c2]
   | _ -> failwith "Fitting.lagrange: invalid input"
