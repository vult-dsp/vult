
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

(** Transformations and optimizations of the syntax tree *)

open Types


(** Generic type of transformations *)
type ('data,'value) transformation = 'data -> 'value -> 'data * 'value

(** Generic type of expanders *)
type ('data,'value) expander = 'data -> 'value -> 'data * 'value list

(** Makes a chain of transformations. E.g. foo |-> bar will apply first foo then bar. *)
let (|->) : ('data,'value) transformation -> ('data,'value) transformation -> ('data,'value) transformation =
   fun a b ->
   fun state exp ->
      let new_state,new_exp = a state exp in
      b new_state new_exp

(** Pipes a pair (state,value) into transformation functions *)
let (|+>) : ('state * 'value) -> ('state, 'value) transformation -> ('state * 'value) =
   fun (state,value) transformation ->
      transformation state value

(** Traversing state one *)
type state_t1 =
   {
      dummy : int;
      fcall_index : int;
   }
(* ======================= *)

let returnBindsAndDecl ((decls:val_bind list),(binds: stmt list)) (bind:val_bind) =
   match bind with
   | ValBind(name,init,exp) ->
      ValNoBind(name,init)::decls, StmtBind(PId(name),exp)::binds
   | ValNoBind(name,init) -> ValNoBind(name,init)::decls,binds

(** Transforms mem x=0; -> mem x; x=0; *)
let separateBindAndDeclaration : ('data,stmt) expander =
   fun state stmt ->
      match stmt with
      | StmtMem(vlist) ->
         let new_vlist,binds = List.fold_left returnBindsAndDecl ([],[]) vlist in
         let stmts = StmtMem(List.rev new_vlist)::binds  in
         state,stmts
      | StmtVal(vlist) ->
         let new_vlist,binds = List.fold_left returnBindsAndDecl ([],[]) vlist in
         let stmts = StmtVal(List.rev new_vlist)::binds  in
         state,stmts
      | _ -> state,[stmt]

(* ======================= *)

(** Transforms val x,y; -> val x; val y; *)
let makeSingleDeclaration : ('data,stmt) expander =
   fun state stmt ->
      match stmt with
      | StmtVal(vlist) ->
         let stmts = List.map (fun a -> StmtVal([a])) vlist in
         state,stmts
      | StmtMem(vlist) ->
         let stmts = List.map (fun a -> StmtMem([a])) vlist in
         state,stmts
      | _ -> state,[stmt]

(* ======================= *)

(** Adds a default name to all function calls. e.g. foo(x) ->  _inst_0:foo(x) *)
let nameFunctionCalls : ('data,parse_exp) transformation =
   fun state exp ->
      match exp with
      | PCall(SimpleId(name,loc),args,floc) ->
         let inst = "_inst"^(string_of_int state.fcall_index) in
         let new_state = {state with fcall_index = state.fcall_index+1} in
         new_state,PCall(NamedId(inst,name,loc,loc),args,floc)
      | _ -> state,exp

(* ======================= *)

(** Transforms all operators into function calls *)
let operatorsToFunctionCalls : ('data,parse_exp) transformation =
   fun state exp ->
      match exp with
      | PUnOp(op,e,loc) ->
         state,PCall(NamedId("_",op,loc,loc),[e],loc)
      | PBinOp(op,e1,e2,loc) ->
         state,PCall(NamedId("_",op,loc,loc),[e1;e2],loc)
      | _ -> state,exp

(* ======================= *)

(** Creates bindings for all function calls in an expression *)
let bindFunctionCallsInExp : (int * stmt list,parse_exp) transformation =
   fun data exp ->
      match exp with
      | PCall(name,args,loc) ->
         let count,stmts = data in
         let tmp_var = SimpleId("_tmp"^(string_of_int count),default_loc) in
         let decl = StmtVal([ValNoBind(tmp_var,None)]) in
         let bind_stmt = StmtBind(PId(tmp_var),exp) in 
         (count+1,[bind_stmt;decl]@stmts),PId(tmp_var)
      | _ -> data,exp

(** Binds all function calls to a variable. e.g. foo(bar(x)) -> tmp1 = bar(x); tmp2 = foo(tmp1); tmp2; *)

let bindFunctionCalls : ('data,stmt) expander  =
   fun state stmt ->
      match stmt with
      | StmtBind(lhs,PCall(name,args,loc)) ->
         let (count,stmts),new_args = TypesUtil.traverseBottomExpList bindFunctionCallsInExp (state.fcall_index,[]) args in
         {state with fcall_index = count},(List.rev (StmtBind(lhs,PCall(name,new_args,loc))::stmts))
      | StmtBind(lhs,rhs) ->
         let (count,stmts),new_rhs = TypesUtil.traverseBottomExp bindFunctionCallsInExp (state.fcall_index,[]) rhs in
         {state with fcall_index = count},(List.rev (StmtBind(lhs,new_rhs)::stmts))
      | StmtReturn(e) ->
         let (count,stmts),new_e = TypesUtil.traverseBottomExp bindFunctionCallsInExp (state.fcall_index,[]) e in
         {state with fcall_index = count},(List.rev (StmtReturn(new_e)::stmts))
      | StmtIf(cond,then_stmts,else_stmts) ->
         let (count,stmts),new_cond = TypesUtil.traverseBottomExp bindFunctionCallsInExp (state.fcall_index,[]) cond in
         {state with fcall_index = count},(List.rev (StmtIf(new_cond,then_stmts,else_stmts)::stmts))

      | _ -> state,[stmt]

(* ======================= *)      

let applyTransformations (results:parser_results) =
   let initial_state = { fcall_index = 0 ; dummy = 0 } in
   let transform_function stmts =
      (initial_state,stmts)
      |+> TypesUtil.traverseTopExpStmtList (nameFunctionCalls|->operatorsToFunctionCalls)
      |+> TypesUtil.expandStmtList separateBindAndDeclaration
      |+> TypesUtil.expandStmtList makeSingleDeclaration
      |+> TypesUtil.expandStmtList bindFunctionCalls
      |> snd
   in
   let new_stmts = Either.applyToRight transform_function results.presult in
   { results with presult = new_stmts }


