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
   | Value  of value            * int
   | Reg    of identifier       * int
   | Lazy   of instruction list * int
   | Call   of identifier       * int
   | Obj    of int              * int
   | Lambda of identifier list  * int
   | Store  of int
   | Read   of int
   | If     of int
   | Ret    of int
   | Drop   of int
   | Loop   of int

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

let append_nl buff line s =
   let _ = append buff (string_of_int line) in
   let _ = append buff ": " in
   let _ = append buff s in
   newline buff

let rec printIBuff buff i =
   match i with
   | Value(v,line)   -> append_nl buff line (valueStr v)
   | Reg(name,line)  -> append_nl buff line ("$"^identifierStr name)
   | Store(line)     -> append_nl buff line "Store"
   | Read(line)      -> append_nl buff line "Read"
   | Call(name,line) -> append_nl buff line ("Call("^(identifierStr name)^")")
   | Obj(n,line)     -> append_nl buff line ("Obj("^(string_of_int n)^")")
   | Ret(line)       -> append_nl buff line "Ret"
   | If(line)        -> append_nl buff line "If"
   | Drop(line)      -> append_nl buff line "Drop"
   | Lambda(vars,line) -> append_nl buff line ("Lambda("^(joinStrings "," (List.map identifierStr vars))^")")
   | Loop(line)      -> append_nl buff line "Loop"
   | Lazy(il,line)   ->
      append buff (string_of_int line);
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


let locationLine (loc:location) : int =
   loc.start_pos.Lexing.pos_lnum
   
let getIdentifier (name:named_id) : identifier =
   match name with
   | SimpleId(id,_)    -> id
   | NamedId(id,_,_,_) -> id

let getNameLocation (name:named_id) : location =
   match name with
   | SimpleId(_,loc) -> loc
   | NamedId(_,_,loc1,loc2) -> mergeLocations loc1 loc2

let rec assemble (i0:instruction list) (exp:parse_exp) =
   match exp with
   | PUnit(loc)   ->
      let line = locationLine loc in
      Value(VUnit,line)::i0
   | PBool(v,loc)   ->
      let line = locationLine loc in
      Value(VBool(v),line)::i0
   | PInt(v,loc)    ->
      let line = locationLine loc in
      Value(VNum(float_of_string v),line)::i0
   | PReal(v,loc)   ->
      let line = locationLine loc in
      Value(VNum(float_of_string v),line)::i0
   | PId(name)    ->
      let loc = getNameLocation name in
      let id = getIdentifier name in
      let line = locationLine loc in
      Read(line)::Reg(id,line)::i0
   | PTuple(el,loc) ->
      let line = locationLine loc in
      let n = List.length el in
      let i1 = assembleListExp i0 el in
      Obj(n,line)::i1
   | PCall(fname,args,loc,_) ->
      let line = locationLine loc in
      let i1 = assembleListExp i0 args in
      let id = getIdentifier fname in
      Call(id,line)::i1
   | PSeq(el,_) -> assembleListStmt i0 el
   | PIf(cond,then_,else_,loc) ->
      let line = locationLine loc in
      let i1 = assemble i0 cond in
      let then_line = getExpLocation then_ |> locationLine in
      let else_line = getExpLocation else_ |> locationLine in
      let then_i = Lazy(assemble [] then_,then_line) in
      let else_i = Lazy(assemble [] else_,else_line) in
      If(line)::else_i::then_i::i1
   | StmtVal(PId(name),None,loc) ->
      let line = locationLine loc in
      let id = getIdentifier name in
      Store(line)::Reg(id,line)::Value(VNum(0.0),line)::i0
   | StmtVal(PId(name),Some(init),loc) ->
      let line = locationLine loc in
      let i1 = assemble i0 init in
      let id = getIdentifier name in
      Store(line)::Reg(id,line)::i1
   | StmtMem(PId(name),None,None,loc) ->
      let line = locationLine loc in
      let id = getIdentifier name in
      Store(line)::Reg(id,line)::Value(VNum(0.0),line)::i0
   | StmtMem(PId(name),Some(init),None,loc) ->
      let line = locationLine loc in
      let i1 = assemble i0 init in
      let id = getIdentifier name in
      Store(line)::Reg(id,line)::i1
   | StmtMem(PId(name),_,Some(init),loc) ->
      let line = locationLine loc in
      let i1 = assemble i0 init in
      let id = getIdentifier name in
      Store(line)::Reg(id,line)::i1
   | StmtVal(_,_,_) -> failwith "Complex bindings should be simplified"
   | StmtMem(_,_,_,_) -> failwith "Complex bindings should be simplified"
   | StmtBind(PId(name),e2,loc) ->
      let line = locationLine loc in
      let i1 = assemble i0 e2 in
      let id = getIdentifier name in
      Store(line)::Reg(id,line)::i1
   | StmtBind(PUnit(_),e2,loc) ->
      assemble i0 e2
   | StmtBind(_,_,_) -> failwith "Complex bindings should be simplified"
   | StmtReturn(e,loc) ->
      let line = locationLine loc in
      let i1 = assemble i0 e in
      Ret(line)::i1
   | StmtBlock(el,loc) ->
      assembleListStmt i0 el
   | StmtIf(cond,then_,Some(else_),loc) ->
      let line = locationLine loc in
      let i1 = assemble i0 cond in
      let then_line = getExpLocation then_ |> locationLine in
      let else_line = getExpLocation else_ |> locationLine in
      let then_i = Lazy(assemble [] then_,then_line) in
      let else_i = Lazy(assemble [] else_,else_line) in
      Drop(line)::If(line)::else_i::then_i::i1
   | StmtIf(cond,then_,None,loc) ->
      let line = locationLine loc in
      let i1 = assemble i0 cond in
      let then_line = getExpLocation then_ |> locationLine in
      let then_i = Lazy(assemble [] then_,then_line) in
      Drop(line)::If(line)::Value(VUnit,line)::then_i::i1
   | StmtFun(name,vars,body,loc) ->
      let line = locationLine loc in
      let i1 = assemble [] body in
      let id = getIdentifier name in
      let vars_id = List.map getIdentifier vars in
      Store(line)::Reg(id,line)::Lambda(vars_id,line)::Lazy(i1,line)::i0
   | StmtWhile(cond,body,loc) ->
      let line = locationLine loc in
      (* This needs to be reversed since is gonna be placed in a Lazy *)
      let cond_i = assemble [] cond |> List.rev in
      let body_i = assemble [] body in
      let cond_line = getExpLocation cond |> locationLine in
      let body_line = getExpLocation body |> locationLine in
      Loop(line)::Lazy(body_i,body_line)::Lazy(cond_i,cond_line)::i0
   | PGroup(e,loc) ->
      assemble i0 e
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

