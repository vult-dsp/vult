(*
   The MIT License (MIT)

   Copyright (c) 2014-2026 Leonardo Laguna Ruiz

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

(* Persistent arrays (Baker's rerooting, as described by Conchon and Filliatre in "A Persistent
   Union-Find Data Structure"). The interface is purely functional: [set] returns a new version and
   the previous versions remain valid and unchanged. Internally the most recently used version owns
   a mutable array and every other version is a chain of differences leading to it. Accessing that
   version takes constant time; accessing another one first moves the array to it ("rerooting"),
   which costs the number of differences between both.

   The arrays have no fixed size: the elements that were never set are None. *)

type 'a t = 'a data ref

and 'a data = Array of 'a option array | Diff of int * 'a option * 'a t

let empty () : 'a t = ref (Array [||])

(* Makes the array belong to [t] *)
let rec reroot (t : 'a t) : 'a option array =
  match !t with
  | Array a ->
      a
  | Diff (i, v, next) ->
      let a = reroot next in
      let previous = a.(i) in
      a.(i) <- v ;
      t := Array a ;
      next := Diff (i, previous, t) ;
      a

let get (t : 'a t) (i : int) : 'a option =
  let a = match !t with Array a -> a | Diff _ -> reroot t in
  if i < Array.length a then a.(i) else None

(* An array that holds the elements of [a] and has room for the index [i] *)
let grow (a : 'a option array) (i : int) : 'a option array =
  if i < Array.length a then a
  else
    let bigger = Array.make (max (i + 1) (2 * Array.length a)) None in
    Array.blit a 0 bigger 0 (Array.length a) ;
    bigger

let set (t : 'a t) (i : int) (v : 'a option) : 'a t =
  let a = grow (reroot t) i in
  let previous = a.(i) in
  a.(i) <- v ;
  let result = ref (Array a) in
  t := Diff (i, previous, result) ;
  result
