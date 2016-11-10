(*
The MIT License (MIT)

Copyright (c) 2016 Leonardo Laguna Ruiz

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

open CLike


type value =
   | VUnit
   | VBool of bool
   | VInt of int
   | VReal of float
   | VString of string
   | VArray of value array
   | VObj of (string, value) Hashtbl.t
   | VFunct of cstmt

let eval_op op e1 e2 =
   match op, e1, e2 with
   | "+", VReal(v1), VReal(v2) -> VReal(v1 +. v2)
   | "+", VInt(v1),  VInt(v2)  -> VInt(v1 + v2)
   | "-", VReal(v1), VReal(v2) -> VReal(v1 -. v2)
   | "-", VInt(v1),  VInt(v2)  -> VInt(v1 - v2)
   | "*", VReal(v1), VReal(v2) -> VReal(v1 *. v2)
   | "*", VInt(v1),  VInt(v2)  -> VInt(v1 * v2)
   | "/", VReal(v1), VReal(v2) -> VReal(v1 /. v2)
   | "/", VInt(v1),  VInt(v2)  -> VInt(v1 / v2)
   | "%", VReal(v1), VReal(v2) -> VReal(mod_float v1  v2)
   | "%", VInt(v1),  VInt(v2)  -> VInt(v1 mod v2)
   | _ -> failwith ("eval_op: invalid operator "^op)

let fold_op op args =
   match args with
   | [] -> failwith ""
   | h::t ->
      List.fold_left (eval_op op) h t

let rec eval_cexp env e =
   match e with
   | CEInt(v)     -> VInt(v)
   | CEFloat(_,v) -> VReal(v)
   | CEBool(v)    -> VBool(v)
   | CEString(s)  -> VString(s)
   | CENewObj     -> VObj(Hashtbl.create 0)
   | CEEmpty      -> VUnit
   | CEArray(elems) ->
      let elems' = List.map (eval_cexp env) elems in
      VArray(Array.of_list elems')
   | CEIf(cond, then_, else_) ->
      begin
         match eval_cexp env cond with
         | VBool(true) -> eval_cexp env then_
         | VBool(false) -> eval_cexp env else_
         | _ -> failwith "eval_cexp: Condition is not boolean"
      end
   | CEOp(op,elems) ->
      let elems' = List.map (eval_cexp env) elems in
      fold_op op elems'
   | CEUnOp(_,_) -> failwith ""

   | _ -> failwith ""

