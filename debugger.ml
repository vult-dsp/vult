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
open TypesUtil
open PrintTypes

(** Return values of the interpreter *)
type value =
   | VUnit
   | VString  of string
   | VNum     of float
   | VBool    of bool
   | VTuple   of value list

type instruction =
   | Value  of value
   | Reg    of named_id
   | Lazy   of instruction list
   | Call   of named_id
   | Obj    of int
   | Lambda of named_id list
   | Store
   | Read
   | If
   | Ret
   | Drop
   | Loop

module StringMap = Map.Make(String)

(** Used to define types of functions: builtin and declared by the user *)
type function_body =
   | Builtin  of (value list -> value)
   | Declared of parse_exp

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
      code  : parse_exp list list;
      stack : string list;
      pc    : int;
      env   : env;
      line  : int;
   }

type breakpoints =
   {
      lines : int list;
   }

(** Joins a list of strings *)
let rec joinStrings sep elems =
   match elems with
   | [] -> ""
   | [h] -> h
   | h::t -> h^sep^(joinStrings sep t)

(** Converts a value to string *)
let rec valueStr (value:value) : string =
   match value with
   | VUnit         -> "()"
   | VNum(v)       -> string_of_float v
   | VString(s)    -> "\""^s^"\""
   | VBool(true)   -> "true"
   | VBool(false)  -> "false"
   | VTuple(elems) ->
      let elems_s = elems
                    |> List.map valueStr
                    |> joinStrings ","
      in "("^elems_s^")"

let append_nl buff s =
   let _ = append buff ": " in
   let _ = append buff s in
   newline buff

let rec printIBuff buff i =
   match i with
   | Value(v)  -> append_nl buff (valueStr v)
   | Reg(name) -> append_nl buff ("$"^namedIdStr name)
   | Store     -> append_nl buff "Store"
   | Read      -> append_nl buff "Read"
   | Call(name)-> append_nl buff ("Call("^(namedIdStr name)^")")
   | Obj(n)    -> append_nl buff ("Obj("^(string_of_int n)^")")
   | Ret       -> append_nl buff "Ret"
   | If        -> append_nl buff "If"
   | Drop      -> append_nl buff "Drop"
   | Lambda(vars) -> append_nl buff ("Lambda("^(joinStrings "," (List.map namedIdStr vars))^")")
   | Loop      -> append_nl buff "Loop"
   | Lazy(il)  ->
      append buff ": [";
      indent buff;
      printIBuffList buff il;
      outdent buff;
      append buff "]";
      newline buff
and printIBuffList buff il =
   printList buff printIBuff "" il

let printInstructions il =
   let buffer = makePrintBuffer () in
   let _ = printIBuffList buffer il in
   print_string (contents buffer)

let rec assemble (i0:instruction list) (exp:parse_exp) =
   match exp with
   | PUnit(_)     -> Value(VUnit)::i0
   | PBool(v,_)   -> Value(VBool(v))::i0
   | PInt(v,_)    -> Value(VNum(float_of_string v))::i0
   | PReal(v,_)   -> Value(VNum(float_of_string v))::i0
   | PId(name)    -> Read::Reg(name)::i0
   | PTuple(el,_) ->
      let n = List.length el in
      let i1 = assembleListExp i0 el in
      Obj(n)::i1
   | PCall(fname,args,_,_) ->
      let i1 = assembleListExp i0 args in
      Call(fname)::i1
   | PSeq(el,_) -> assembleListStmt i0 el
   | PIf(cond,then_,else_,_) ->
      let i1 = assemble i0 cond in
      let then_i = Lazy(assemble [] then_) in
      let else_i = Lazy(assemble [] else_) in
      If::else_i::then_i::i1
   | StmtVal(PId(name),None,_) ->
      Store::Reg(name)::Value(VNum(0.0))::i0
   | StmtVal(PId(name),Some(init),_) ->
      let i1 = assemble i0 init in
      Store::Reg(name)::i1
   | StmtMem(PId(name),None,None,_) ->
      Store::Reg(name)::Value(VNum(0.0))::i0
   | StmtMem(PId(name),Some(init),None,_) ->
      let i1 = assemble i0 init in
      Store::Reg(name)::i1
   | StmtMem(PId(name),_,Some(init),_) ->
      let i1 = assemble i0 init in
      Store::Reg(name)::i1
   | StmtVal(_,_,_) -> failwith "Complex bindings should be simplified"
   | StmtMem(_,_,_,_) -> failwith "Complex bindings should be simplified"
   | StmtBind(PId(name),e2,loc) ->
      let i1 = assemble i0 e2 in
      Store::Reg(name)::i1
   | StmtBind(PUnit(_),e2,loc) ->
      assemble i0 e2
   | StmtBind(_,_,_) -> failwith "Complex bindings should be simplified"
   | StmtReturn(e,_) ->
      let i1 = assemble i0 e in
      Ret::i1
   | StmtBlock(el,_) -> assembleListStmt i0 el
   | StmtIf(cond,then_,Some(else_),_) ->
      let i1 = assemble i0 cond in
      let then_i = Lazy(assemble [] then_) in
      let else_i = Lazy(assemble [] else_) in
      Drop::If::else_i::then_i::i1
   | StmtIf(cond,then_,None,_) ->
      let i1 = assemble i0 cond in
      let then_i = Lazy(assemble [] then_) in
      Drop::If::Value(VUnit)::then_i::i1
   | StmtFun(name,vars,body,_) ->
      let i1 = assemble [] body in
      Store::Reg(name)::Lambda(vars)::Lazy(i1)::i0
   | StmtWhile(cond,body,_) ->
      let cond_i = assemble [] cond in
      let body_i = assemble [] body in
      Loop::Lazy(body_i)::Lazy(cond_i)::i0
   | PGroup(e,_) -> assemble i0 e
   | PUnOp(_,_,_)    -> failwith "No unary operations should remain"
   | PBinOp(_,_,_,_) -> failwith "No binary operations should remain"
   | PEmpty          -> failwith "No empty expressions should remain"
   | StmtEmpty       -> failwith "No empty statements should remain"

and assembleListExp (i0:instruction list) (exp_list:parse_exp list) : instruction list =
   List.fold_left (fun i e -> assemble i e) i0 exp_list
and assembleListStmt (i0:instruction list) (exp_list:parse_exp list) : instruction list =
   List.fold_left (fun i e -> assemble i e) i0 exp_list
   |> List.rev

let debug (results:parser_results) : unit =
      let result = CCError.map
         (fun stmts -> assembleListStmt [] stmts |> printInstructions )
         results.presult
      in
      match result with
      | `Ok(ret) ->
         ()
      | `Error(error) ->
         ()

