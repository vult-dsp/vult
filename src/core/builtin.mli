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

(* The functions the language provides. Operators are not here: the parser
   turns them into EOp and EUnOp nodes before any pass sees them. *)
type t =
  | Abs
  | Acos
  | Asin
  | Atan
  | Atan2
  | Ceil
  | Clip
  | Cos
  | Cosh
  | Exp
  | Floor
  | Log
  | Log10
  | Max
  | Min
  | Pow
  | Sin
  | Sinh
  | Sqrt
  | Tan
  | Tanh
  | Bool
  | Fix16
  | Int
  | Int16
  | Real
  | String
  | Eps
  | Pi
  | Irandom
  | Random
  | Samplerate
  | Not
  | Length
  | Size
  | WrapArray
  | ListAppend
  | ListCapacity
  | ListClear
  | ListGet
  | ListInsert
  | ListRemove
  | ListReserve
  | ListSet

(* The name the source language and every backend use for the builtin. *)
val name : t -> string

(* A pure call may be duplicated, removed or reordered: its result depends only
   on its arguments and evaluating it leaves no trace. *)
val is_pure : t -> bool

val is_pure_name : string -> bool

val of_name : string -> t option

val all : t list
