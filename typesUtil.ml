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

type ('a,'b) traverser = 'a -> 'b -> 'a * 'b

type ('a,'b) expander = 'a -> 'b -> 'a * 'b list

(** Folds the list (left-right) using the given traverser functions *)
let foldTraverser_left traverser_function (traverser:('a,'b) traverser) (state:'c) (elems:'d list) =
   let state2,acc =
      List.fold_left
         (fun (state,acc) elem ->
            let state1,ne = traverser_function traverser state elem in
            (state1,ne::acc) )
      (state,[]) elems in
   state2,List.rev acc

(** Folds the list (right-left) using the given traverser functions *)
let foldTraverser_right traverser_function (traverser:('a,'b) traverser) (state:'c) (elems:'d list) =
      let state2,acc =
      List.fold_left
         (fun (state,acc) elem ->
            let state1,ne = traverser_function traverser state elem in
            (state1,ne::acc) )
      (state,[]) (List.rev elems) in
   state2,acc

(** Traverses expressions bottom-up *)
let rec traverseBottomExp (f: ('a, parse_exp) traverser) (state:'a) (exp:parse_exp) : 'a * parse_exp =
   match exp with
   | PEmpty
   | PUnit
   | PInt(_,_)
   | PReal(_,_)
   | PId(_)   -> f state exp
   | PUnOp(name,e,loc) ->
      let state1,ne = traverseBottomExp f state e in
      f state1 (PUnOp(name,ne,loc))
   | PBinOp(name,e1,e2,loc) ->
      let state1,ne1 = traverseBottomExp f state e1 in
      let state2,ne2 = traverseBottomExp f state1 e2 in
      f state2 (PBinOp(name,ne1,ne2,loc))
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
and traverseBottomExpList (f: ('a, parse_exp) traverser) (state:'a) (expl:parse_exp list) : 'a * parse_exp list =
   foldTraverser_right traverseBottomExp f state expl

(** Traverses statements bottom-up *)
let rec traverseBottomStmt (f: ('a, stmt) traverser) (state:'a) (stmt:stmt) : 'a * stmt =
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
and traverseBottomStmtList (f: ('a, stmt) traverser) (state:'a) (stmts:stmt list) : 'a * stmt list = 
   foldTraverser_right traverseBottomStmt f state stmts

(** Traverses expressions top-down *)
let rec traverseTopExp (f: ('a, parse_exp) traverser) (state0:'a) (exp:parse_exp) : 'a * parse_exp =
   let state,nexp = f state0 exp in
   match nexp with
   | PEmpty
   | PUnit
   | PInt(_,_)
   | PReal(_,_)
   | PId(_)   -> state,nexp
   | PUnOp(name,e,loc) ->
      let state1,ne = traverseTopExp f state e in
      state1,PUnOp(name,ne,loc)
   | PBinOp(name,e1,e2,loc) ->
      let state1,ne1 = traverseTopExp f state e1 in
      let state2,ne2 = traverseTopExp f state1 e2 in
      state2,PBinOp(name,ne1,ne2,loc)
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
and traverseTopExpList (f: ('a, parse_exp) traverser) (state:'a) (expl:parse_exp list) : 'a * parse_exp list =
   foldTraverser_left traverseTopExp f state expl

let traverseTopOptExp (f: ('a, parse_exp) traverser) (state:'a) (exp_opt:parse_exp option) =
   match exp_opt with
   | Some(exp) ->
      let new_state,new_exp = f state exp in
      new_state,Some(new_exp)
   | None -> state,None

(** Traverses statements top-down *)
let rec traverseTopStmt (f: ('a, stmt) traverser) (state0:'a) (stmt:stmt) : 'a * stmt =
   let state,nstmt = f state0 stmt in
   match nstmt with
   | StmtVal(_)
   | StmtMem(_)
   | StmtReturn(_)
   | StmtBind(_)
   | StmtEmpty -> state,nstmt
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
and traverseTopStmtList (f: ('a, stmt) traverser) (state:'a) (stmts:stmt list) : 'a * stmt list = 
   foldTraverser_left traverseTopStmt f state stmts

let rec expandStmt (f: ('a, stmt) expander) (state0:'a) (stmt:stmt) : 'a * stmt list =
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

and expandStmtList (f: ('a, stmt) expander) (state:'a) (stmts:stmt list) : 'a * stmt list =
   let state2,acc =
      List.fold_left
         (fun (state,acc) exp ->
            let state1,ne = expandStmt f state exp in
            (state1,(List.rev ne)::acc) )
      (state,[]) stmts in
   state2,List.rev (List.flatten acc)

(** Applies a function to expressions in the bindings using a top-down traverser *)
let traverseTopValBindExp (f: ('a, parse_exp) traverser) (state:'a) (val_bind:val_bind) =
   match val_bind with
   | ValBind(name,init_opt,value) ->
      let state1,new_init_opt = traverseTopOptExp f state init_opt in
      let state2,new_value = traverseTopExp f state value in
      state2,ValBind(name,new_init_opt,new_value)
   | ValNoBind(name,init_opt) ->
      let state1,new_init_opt = traverseTopOptExp f state init_opt in
      state1,ValNoBind(name,new_init_opt)

let traverseTopValBindListExp (f: ('a, parse_exp) traverser) (state:'a) (val_binds:val_bind list) =
   foldTraverser_left traverseTopValBindExp f state val_binds

(** Applies a traverser function to all the expressions in the current level of a statememt *)
let traverseTopExpStmt_traverser (f: ('a, parse_exp) traverser) (state:'a) (stmt:stmt) =
   match stmt with
   | StmtVal(val_binds) ->
      let state1,new_val_binds = traverseTopValBindListExp f state val_binds in
      state1,StmtVal(new_val_binds)
   | StmtMem(val_binds) ->
      let state1,new_val_binds = traverseTopValBindListExp f state val_binds in
      state1,StmtMem(new_val_binds)
   | StmtReturn(exp) ->
      let state1,new_exp = traverseTopExp f state exp in
      state1,StmtReturn(new_exp)
   | StmtBind(e1,e2) ->
      let state1,new_e1 = traverseTopExp f state e1 in
      let state2,new_e2 = traverseTopExp f state1 e2 in
      state2,StmtBind(new_e1,new_e2)
   | StmtEmpty -> state,stmt
   | StmtFun(name,args,stmts) ->
      let state1,new_args = traverseTopValBindListExp f state args in
      state1,StmtFun(name,new_args,stmts)
   | StmtIf(cond,then_stmts,opt_else) ->
      let state1,new_cond = traverseTopExp f state cond in
      state1,StmtIf(new_cond,then_stmts,opt_else)

(** Applies a traverser to all the espressions in the statemet *)
let traverseTopExpStmt (f: ('a, parse_exp) traverser) (state:'a) (stmt:stmt) =
   traverseTopStmt (fun state exp -> traverseTopExpStmt_traverser f state exp) state stmt

(** Applies a traverser to all the espressions in the statemet list *)
let traverseTopExpStmtList (f: ('a, parse_exp) traverser) (state:'a) (stmts:stmt list) =
   foldTraverser_left traverseTopExpStmt f state stmts

(** Returns the minimal position of two given *)
let getMinPosition (pos1:Lexing.position) (pos2:Lexing.position) : Lexing.position =
   if pos1.Lexing.pos_lnum <> pos2.Lexing.pos_lnum then
      if pos1.Lexing.pos_lnum < pos2.Lexing.pos_lnum then pos1 else pos2
   else
      if pos1.Lexing.pos_cnum < pos2.Lexing.pos_cnum then pos1 else pos2

(** Returns the maximum position of two given *)
let getMaxPosition (pos1:Lexing.position) (pos2:Lexing.position) : Lexing.position =
   if pos1.Lexing.pos_lnum <> pos2.Lexing.pos_lnum then
      if pos1.Lexing.pos_lnum < pos2.Lexing.pos_lnum then pos2 else pos1
   else
      if pos1.Lexing.pos_cnum < pos2.Lexing.pos_cnum then pos2 else pos1

(** Retuns the minimum and maximum prositions from a given list *)
let getMinMaxPositions (pos_list:Lexing.position list) =
   match pos_list with
   | []  -> failwith "getMinMaxPositions: No positions passed"
   | [h] -> h,h
   | h::_   -> List.fold_left (fun (min,max) a -> getMinPosition a min, getMaxPosition a max) (h,h) pos_list

(** Returns a new location with the start and end positions updateds *)
let mergeLocations (loc1:location) (loc2:location) : location =
   let start_pos,end_pos = getMinMaxPositions [loc1.start_pos; loc2.start_pos; loc1.end_pos; loc2.end_pos] in
   { start_pos = start_pos; end_pos = end_pos }

let getNameFromNamedId (named_id:named_id) : string =
    match named_id with
   | SimpleId(name,_) -> name
   | NamedId (name,_,_,_) -> name

let getLocationFromNamedId (named_id:named_id) : location =
   match named_id with
   | SimpleId(_,loc) -> loc
   | NamedId (_,_,loc1,loc2) -> mergeLocations loc1 loc2

let getTypeFromNamedId (named_id:named_id) : string option =
   match named_id with
   | NamedId (_,nametype,_,_) -> Some nametype
   | _ -> None

let getFunctionTypeAndName (names_id:named_id) : string * string =
   match names_id with
   | NamedId(name,ftype,_,_) -> name,ftype
   | SimpleId(ftype,_) -> "_",ftype
