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

(** Traverses expressions top-down *)
let rec traverseTopExp (f: 'a -> parse_exp -> 'a * parse_exp) (state0:'a) (exp:parse_exp) : 'a * parse_exp =
   let state,nexp = f state0 exp in
   match nexp with
   | PEmpty
   | PUnit
   | PInt(_,_)
   | PReal(_,_)
   | PId(_,_)   -> state,nexp
   | PUnOp(name,e) ->
      let state1,ne = traverseTopExp f state e in
      state1,PUnOp(name,ne)
   | PBinOp(name,e1,e2) ->
      let state1,ne1 = traverseTopExp f state e1 in
      let state2,ne2 = traverseTopExp f state1 e2 in
      state2,PBinOp(name,ne1,ne2)
   | PGroup(e) ->
      let state1,ne = traverseTopExp f state e in
      state1,PGroup(ne)
   | PTuple(expl) ->
      let state1,nexpl = traverseTopExpList f state expl in
      state1,PTuple(nexpl)
   | PCall(name,expl,loc) ->
      let state1,nexpl = traverseTopExpList f state expl in
      state1,PCall(name,nexpl,loc)

(** Traverses lists expressions top-down. The expressions are traversed left to right *)
and traverseTopExpList (f: 'a -> parse_exp -> 'a * parse_exp) (state:'a) (expl:parse_exp list) : 'a * parse_exp list =
   let state2,acc =
      List.fold_left
         (fun (state,acc) exp ->
            let state1,ne = traverseTopExp f state exp in
            (state1,ne::acc) )
      (state,[]) expl in
   state2,List.rev acc

(** Traverses statements top-down *)
let rec traverseTopStmt (f: 'a -> stmt -> 'a * stmt) (state0:'a) (stmt:stmt) : 'a * stmt =
   let state,nstmt = f state0 stmt in
   match nstmt with
   | StmtVal(_)
   | StmtMem(_)
   | StmtReturn(_)
   | StmtBind(_)
   | StmtEmpty -> state,stmt
   | StmtFun(name,args,stmts) ->
      let state1,nstmts = traverseTopStmtList f state stmts in
      state1,StmtFun(name,args,nstmts)
   | StmtIf(cond,then_stmts,None) ->
      let state1,nthen_stmts = traverseTopStmtList f state then_stmts in
      state1,StmtIf(cond,nthen_stmts,None)
   | StmtIf(cond,then_stmts,Some(else_stmts)) ->
      let state1,nthen_stmts = traverseTopStmtList f state then_stmts in
      let state2,nelse_stmts = traverseTopStmtList f state1 else_stmts in
      state2,StmtIf(cond,nthen_stmts,Some(nelse_stmts))

(** Traverses lists statements top-down. The statements are traversed right to left (last first) *)
and traverseTopStmtList (f: 'a -> stmt -> 'a * stmt) (state:'a) (stmts:stmt list) : 'a * stmt list = 
   let state2,acc =
      List.fold_left
         (fun (state,acc) exp ->
            let state1,ne = traverseTopStmt f state exp in
            (state1,ne::acc) )
      (state,[]) stmts in
   state2,List.rev acc

let rec expandStmt (f: 'a -> stmt -> 'a * stmt list) (state0:'a) (stmt:stmt) : 'a * stmt list =
   let state,nstmt = f state0 stmt in
   let inner_fold (state,acc) stmt =
      match stmt with
      | StmtVal(_)
      | StmtMem(_)
      | StmtReturn(_)
      | StmtBind(_)
      | StmtEmpty -> state,stmt::acc
      | StmtFun(name,args,stmts) ->
         let state1,nstmts = expandStmtList f state stmts in
         state1,StmtFun(name,args,nstmts)::acc
      | StmtIf(cond,then_stmts,None) ->
         let state1,nthen_stmts = expandStmtList f state then_stmts in
         state1,StmtIf(cond,nthen_stmts,None)::acc
      | StmtIf(cond,then_stmts,Some(else_stmts)) ->
         let state1,nthen_stmts = expandStmtList f state then_stmts in
         let state2,nelse_stmts = expandStmtList f state1 else_stmts in
         state2,StmtIf(cond,nthen_stmts,Some(nelse_stmts))::acc
   in
   let state1,acc = List.fold_left inner_fold (state,[]) nstmt in
   state1,List.rev acc

and expandStmtList (f: 'a -> stmt -> 'a * stmt list) (state:'a) (stmts:stmt list) : 'a * stmt list =
   let state2,acc =
      List.fold_left
         (fun (state,acc) exp ->
            let state1,ne = expandStmt f state exp in
            (state1,(List.rev ne)::acc) )
      (state,[]) stmts in
   state2,List.rev (List.flatten acc)
