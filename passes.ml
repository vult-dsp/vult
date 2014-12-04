
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

(** Generic type of transformations *)
type ('a,'b) transformation = 'a -> 'b -> 'a * 'b

(** Generic type of expanders *)
type ('a,'b) expander       = 'a -> 'b -> 'a * 'b list

(** Makes a chain of transformations. E.g. foo |-> bar will apply first foo then bar. *)
let (|->) : ('a,'b) transformation -> ('a,'b) transformation -> ('a,'b) transformation =
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

let returnBindsAndDecl ((decls:val_bind list),(binds: stmt list)) (bind:val_bind) =
   match bind with
   | ValBind(name,init,exp) ->
      ValNoBind(name,init)::decls, StmtBind(PId(name),exp)::binds
   | ValNoBind(name,init) -> ValNoBind(name,init)::decls,binds

(** Transforms val x=0; -> val x; x=0; *)
let separateBindAndDeclaration : ('a,stmt) expander =
   fun state stmt ->
   match stmt with
   | StmtVal(vlist) ->
      let new_vlist,binds = List.fold_left returnBindsAndDecl ([],[]) vlist in
      let stmts = StmtVal(List.rev new_vlist)::binds in
      state,stmts
   | StmtMem(vlist) ->
      let new_vlist,binds = List.fold_left returnBindsAndDecl ([],[]) vlist in
      let stmts = StmtMem(List.rev new_vlist)::binds  in
      state,stmts
   | _ -> state,[stmt]

(** Adds a default name to all function calls. e.g. foo(x) ->  _inst_0:foo(x) *)
let nameFunctionCalls : ('a,parse_exp) transformation =
   fun state exp ->
   match exp with
   | PCall(SimpleId(name,loc),args,floc) ->
      let inst = "_inst"^(string_of_int state.fcall_index) in
      let new_state = {state with fcall_index = state.fcall_index+1} in
      new_state,PCall(NamedId(inst,name,loc,loc),args,floc)
   | _ -> state,exp

let applyTransformations (results:parser_results) =
   let initial_state = { fcall_index = 0 ; dummy = 0 } in
   let transform_function stmts =
      (initial_state,stmts)
      |+> (fun state stmts -> TypesUtil.expandStmtList separateBindAndDeclaration state stmts)
      |+> (fun state stmts -> TypesUtil.traverseTopExpStmtList nameFunctionCalls state stmts)
      |> snd
   in
   let new_stmts = Either.mapRight transform_function results.presult in
   { results with presult = new_stmts }


