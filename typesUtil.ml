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

(** Traverses expressions bottom-up *)
let rec traverseBottomExp (f: 'a -> parse_exp -> 'a * parse_exp) (state:'a) (exp:parse_exp) : 'a * parse_exp =
   match exp with
   | PEmpty
   | PUnit
   | PInt(_,_)
   | PReal(_,_)
   | PId(_,_)   -> f state exp
   | PUnOp(name,e) ->
      let state1,ne = traverseBottomExp f state e in
      f state1 (PUnOp(name,ne))
   | PBinOp(name,e1,e2) ->
      let state1,ne1 = traverseBottomExp f state e1 in
      let state2,ne2 = traverseBottomExp f state1 e2 in
      f state2 (PBinOp(name,ne1,ne2))
   | PGroup(e) ->
      let state1,ne = traverseBottomExp f state e in
      f state1 (PGroup(ne))
   | PTuple(expl) ->
      let state1,nexpl = traverseBottomExpList f state expl in
      f state1 (PTuple(nexpl))
   | PCall(name,expl,loc) ->
      let state1,nexpl = traverseBottomExpList f state expl in
      f state1 (PCall(name,nexpl,loc))
(** Traverses lists expressions bottom-up. The expressions are traversed right to left *)
and traverseBottomExpList (f: 'a -> parse_exp -> 'a * parse_exp) (state:'a) (expl:parse_exp list) : 'a * parse_exp list =
   let state2,acc =
      List.fold_left
         (fun (state,acc) exp ->
            let state1,ne = traverseBottomExp f state exp in
            (state1,ne::acc) )
      (state,[]) (List.rev expl) in
   state2,acc

(** Traverses statements bottom-up *)
let rec traverseBottomStmt (f: 'a -> stmt -> 'a * stmt) (state:'a) (stmt:stmt) : 'a * stmt =
   match stmt with
   | StmtVal(_)
   | StmtMem(_)
   | StmtReturn(_)
   | StmtBind(_)
   | StmtEmpty ->
      f state stmt
   | StmtFun(name,args,stmts) ->
      let state1,nstmts = traverseBottomStmtList f state stmts in
      f state1 (StmtFun(name,args,nstmts))
   | StmtIf(cond,then_stmts,None) ->
      let state1,nthen_stmts = traverseBottomStmtList f state then_stmts in
      f state1 (StmtIf(cond,nthen_stmts,None))
   | StmtIf(cond,then_stmts,Some(else_stmts)) ->
      let state1,nthen_stmts = traverseBottomStmtList f state then_stmts in
      let state2,nelse_stmts = traverseBottomStmtList f state1 else_stmts in
      f state2 (StmtIf(cond,nthen_stmts,Some(nelse_stmts)))

(** Traverses lists statements bottom-up. The statements are traversed right to left (last first) *)
and traverseBottomStmtList (f: 'a -> stmt -> 'a * stmt) (state:'a) (stmts:stmt list) : 'a * stmt list = 
   let state2,acc =
      List.fold_left
         (fun (state,acc) exp ->
            let state1,ne = traverseBottomStmt f state exp in
            (state1,ne::acc) )
      (state,[]) (List.rev stmts) in
   state2,acc