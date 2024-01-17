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
let table : (string, float) Hashtbl.t = Hashtbl.create 0

let getTime label =
  match Hashtbl.find_opt table label with
  | Some t -> t
  | None -> 0.0


let time label f =
  let t1 = Sys.time () in
  let ret = f () in
  let t2 = Sys.time () in
  let current = getTime label in
  let acc = current +. (t2 -. t1) in
  Hashtbl.replace table label acc;
  ret


let show () =
  let elems = Hashtbl.fold (fun key value acc -> (key, value) :: acc) table [] in
  let sorted = List.sort (fun (_, a) (_, b) -> compare b a) elems in
  List.iter (fun (label, value) -> prerr_endline (label ^ ": " ^ string_of_float value)) sorted
