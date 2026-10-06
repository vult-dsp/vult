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
  (* math *)
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
  (* conversions *)
  | Bool
  | Fix16
  | Int
  | Int16
  | Real
  | String
  (* constants and environment *)
  | Eps
  | Pi
  | Irandom
  | Random
  | Samplerate
  (* logic *)
  | Not
  (* arrays and strings *)
  | Length
  | Size
  | WrapArray
  (* lists *)
  | ListAppend
  | ListCapacity
  | ListClear
  | ListGet
  | ListInsert
  | ListRemove
  | ListReserve
  | ListSet

(* The name the source language and every backend use for the builtin. *)
let name = function
  | Abs ->
      "abs"
  | Acos ->
      "acos"
  | Asin ->
      "asin"
  | Atan ->
      "atan"
  | Atan2 ->
      "atan2"
  | Ceil ->
      "ceil"
  | Clip ->
      "clip"
  | Cos ->
      "cos"
  | Cosh ->
      "cosh"
  | Exp ->
      "exp"
  | Floor ->
      "floor"
  | Log ->
      "log"
  | Log10 ->
      "log10"
  | Max ->
      "max"
  | Min ->
      "min"
  | Pow ->
      "pow"
  | Sin ->
      "sin"
  | Sinh ->
      "sinh"
  | Sqrt ->
      "sqrt"
  | Tan ->
      "tan"
  | Tanh ->
      "tanh"
  | Bool ->
      "bool"
  | Fix16 ->
      "fix16"
  | Int ->
      "int"
  | Int16 ->
      "int16"
  | Real ->
      "real"
  | String ->
      "string"
  | Eps ->
      "eps"
  | Pi ->
      "pi"
  | Irandom ->
      "irandom"
  | Random ->
      "random"
  | Samplerate ->
      "samplerate"
  | Not ->
      "not"
  | Length ->
      "length"
  | Size ->
      "size"
  | WrapArray ->
      "wrap_array"
  | ListAppend ->
      "list_append"
  | ListCapacity ->
      "list_capacity"
  | ListClear ->
      "list_clear"
  | ListGet ->
      "list_get"
  | ListInsert ->
      "list_insert"
  | ListRemove ->
      "list_remove"
  | ListReserve ->
      "list_reserve"
  | ListSet ->
      "list_set"

(* A pure call may be duplicated, removed or reordered: its result depends only
   on its arguments and evaluating it leaves no trace. *)
let is_pure = function
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
  | Tanh ->
      true
  | Bool | Fix16 | Int | Int16 | Real | String ->
      true
  | Eps | Pi | Samplerate ->
      true
  | Not | Length | Size ->
      true
  (* Draw from a hidden generator. *)
  | Irandom | Random ->
      false
  (* Read or write storage that other statements can change. *)
  | WrapArray | ListAppend | ListCapacity | ListClear | ListGet | ListInsert | ListRemove | ListReserve | ListSet ->
      false

(* Every builtin. A missing entry makes that builtin unknown to the
   typechecker, which the test suite reports immediately. *)
let all =
  [ Abs
  ; Acos
  ; Asin
  ; Atan
  ; Atan2
  ; Ceil
  ; Clip
  ; Cos
  ; Cosh
  ; Exp
  ; Floor
  ; Log
  ; Log10
  ; Max
  ; Min
  ; Pow
  ; Sin
  ; Sinh
  ; Sqrt
  ; Tan
  ; Tanh
  ; Bool
  ; Fix16
  ; Int
  ; Int16
  ; Real
  ; String
  ; Eps
  ; Pi
  ; Irandom
  ; Random
  ; Samplerate
  ; Not
  ; Length
  ; Size
  ; WrapArray
  ; ListAppend
  ; ListCapacity
  ; ListClear
  ; ListGet
  ; ListInsert
  ; ListRemove
  ; ListReserve
  ; ListSet ]

let by_name = lazy (CCList.fold_left (fun m b -> Util.Maps.Map.add (name b) b m) Util.Maps.Map.empty all)

let of_name path = Util.Maps.Map.find_opt path (Lazy.force by_name)

let is_pure_name path = match of_name path with Some b -> is_pure b | None -> false
