
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

let returnBindsAndDecl ((decls:val_bind list),(binds: stmt list)) (bind:val_bind) =
   match bind with
   | ValBind(name,init,exp) ->
      ValNoBind(name,init)::decls, StmtBind(PId(name),exp)::binds
   | ValNoBind(name,init) -> ValNoBind(name,init)::decls,binds

(** Transforms val x=0; -> val x; x=0; *)
let separateBindAndDeclaration (state:'a) (stmt:stmt) : 'a * stmt list =
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


let transformations : 'a -> stmt -> 'a * stmt list =
   separateBindAndDeclaration

let applyTransformations (results:parser_results) =
   let transform_function stmts = TypesUtil.expandStmtList transformations () stmts |> snd in
   let new_stmts = Either.mapRight transform_function results.presult in
   {results with presult = new_stmts }


