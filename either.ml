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

(** Implementation of Either *)


(** Type *)
type ('a,'b) either =
   | Left of 'a
   | Right of 'b

(** Folds left over a list, and stops if it encounters Left. *)
let eitherFold_left : ('a -> 'b -> ('c,'a) either) -> 'a -> 'b list -> ('c,'a) either =
   fun f val1 yt1 ->
      let rec go : 'a -> 'b list -> ('c,'a) either =
         fun v yt -> match yt with
            | (y::ys) ->
               begin match f v y with
                  | Left _ as failure -> failure
                  | Right success -> go success ys
               end
            | [] -> Right v
      in
      go val1 yt1

let mapRight (f:'a -> 'b) (value:('c,'b) either) =
   match value with
   | Left(v)  -> Left(v)
   | Right(v) -> Right(f v)
