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

open Types
open Lexing

(** Return values of the interpreter *)
type value =
   | VUnit
   | VString  of string
   | VNum     of float
   | VBool    of bool
   | VTuple   of value list

module StringMap = Map.Make(String)

(** Used to define types of functions: builtin and declared by the user *)
type function_body =
   | Builtin  of (value list -> value)
   | Declared of stmt_type parse_exp

(** Environment of the interpreter used to store all bindings and declarations *)
type env =
   {
      val_binds : (value StringMap.t) list;
      mem_binds : value StringMap.t;
      fun_bind  : env StringMap.t;
      fun_decl  : function_body StringMap.t;
   }

type debugger_state =
   {
      code  : stmt_type parse_exp list list;
      stack : string list;
      pc    : int;
      env   : env;
      line  : int;
   }

type breakpoints =
   {
      lines : int list;
   }

let apply_default (f:'a -> 'b) (v:'a option) (def:'b) =
   match v with
   | Some(x) -> f x
   | _ -> def

(** Joins a list of strings *)
let rec joinStrings sep elems =
   match elems with
   | [] -> ""
   | [h] -> h
   | h::t -> h^sep^(joinStrings sep t)

(* Step, Step-in, Step-out *)

let isNextLine state stmt =
   let location = TypesUtil.getExpLocation stmt in
   location.start_pos.pos_lnum > state.line

let isPrevLine state stmt =
   let location = TypesUtil.getExpLocation stmt in
   location.start_pos.pos_lnum < state.line

