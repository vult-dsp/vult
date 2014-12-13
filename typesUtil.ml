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

(** Utility functions for the types *)

open Types
open Either

(* -- OVERVIEW PLEASE FILL IN
   Types:
      traverser: Visitor function for simultaneous mapping and folding.
      expander: Visitor function for simultaneous mapping and folding and insertion.
      expressionfolder: A type for doing some kind of transition between
         a deep and shallow embedding of Vult.

   Higher-order functions:


   Utility functions:
      getMinPosition: Get the min position from a list of positisons.
      getMaxPositions: Get the max positison from a list of positions.
      getMinMaxPositison: Get the max and min position from a list of positions.
      mergeLocations: Given two locations, construct the interval location with the
         given locations as endpoints.
      getNameFromNamedId: Get the actual name from a named_id.
      getLocationFromNamed: Get the source file location for a named_id, if any.
      getTypeFromNamedId: Don't really know what this does.
      getFunctionTypeName: Don't really know what this does.
*)

(** Type of all traversing functions *)
type ('data,'traversing_type) traverser = 'data -> 'traversing_type -> 'data * 'traversing_type

(** Type of all expanding functions *)
type ('data,'expanding_type) expander = 'data -> 'expanding_type -> 'data * 'expanding_type list

(** Type of folding functions *)
type ('data,'traversing_type) folder = 'data -> 'traversing_type -> 'data

type ('data, 'error, 'result) expfold =
   {
      vUnit  : 'data -> ('error, 'result) either;
      vInt   : 'data -> string -> location -> ('error, 'result) either;
      vReal  : 'data -> string -> location -> ('error, 'result) either;
      vId    : 'data -> named_id -> ('error, 'result) either;
      vUnOp  : 'data -> string -> 'result -> location -> ('error, 'result) either;
      vBinOp : 'data -> string -> 'result -> 'result -> location -> ('error, 'result) either;
      vCall  : 'data -> named_id -> 'result list -> location -> ('error, 'result) either;
      vIf    : 'data -> 'result -> 'result -> 'result -> ('error, 'result) either;
      vGroup : 'data -> 'result -> ('error, 'result) either;
      vTuple : 'data -> 'result list -> ('error, 'result) either;
      vEmpty : 'data -> ('error, 'result) either;
   }

(** Folds the list (left-right) using the given traverser functions *)
let foldTraverser_left traverser_function (traverser:('data,'traversing_type) traverser) (state:'data) (elems:'elem list) =
   let state2,acc =
      List.fold_left
         (fun (state,acc) elem ->
             let state1,ne = traverser_function traverser state elem in
             (state1,ne::acc) )
         (state,[]) elems in
   state2,List.rev acc

(** Folds the list (right-left) using the given traverser functions *)
let foldTraverser_right traverser_function (traverser:('data,'traversing_type) traverser) (state:'data) (elems:'elem list) =
   let state2,acc =
      List.fold_left
         (fun (state,acc) elem ->
             let state1,ne = traverser_function traverser state elem in
             (state1,ne::acc) )
         (state,[]) (List.rev elems) in
   state2,acc

(** Fold an expression with the 'expfold' type that contains the functions to apply  *)
let expressionFoldEither : ('data, 'error, 'result) expfold -> 'data -> parse_exp -> ('error, 'result) either =
   fun fold data exp ->
      let rec go e = match e with
         | PUnit -> fold.vUnit data
         | PInt (s,l) -> fold.vInt data s l
         | PReal (s,l) -> fold.vReal data s l
         | PId n -> fold.vId data n
         | PUnOp (s,e1,l) -> begin match go e1 with
               | Right r1 -> fold.vUnOp data s r1 l
               | Left _ as err -> err
            end
         | PBinOp (s,e1,e2,l) -> begin match (go e1, go e2) with
               | (Right r1, Right r2) -> fold.vBinOp data s r1 r2 l
               | (Left _ as err, _) -> err
               | (_, (Left _ as err)) -> err
            end
         | PCall (n,es,l) -> begin match eitherTryMap go es with
               | Right rs -> fold.vCall data n rs l
               | Left _ as err -> err
            end
         | PIf (e1,e2,e3) -> begin match (go e1, go e2, go e3) with
               | (Right r1, Right r2, Right r3) -> fold.vIf data r1 r2 r3
               | (Left _ as err,_,_) -> err
               | (_,(Left _ as err),_) -> err
               | (_,_,(Left _ as err)) -> err
            end
         | PGroup e1 -> begin match go e1 with
               | Right r1 -> fold.vGroup data r1
               | Left _ as err -> err
            end
         | PTuple es -> begin match eitherTryMap go es with
               | Right rs -> fold.vTuple data rs
               | Left _ as err -> err
            end
         | PEmpty -> fold.vEmpty data
      in
      go exp

(** Traverses expressions bottom-up *)
let rec traverseBottomExp (f: ('data, parse_exp) traverser) (state:'data) (exp:parse_exp) : 'data * parse_exp =
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
   | PIf(e1,e2,e3) ->
      let state1,ne1 = traverseBottomExp f state e1 in
      let state2,ne2 = traverseBottomExp f state1 e2 in
      let state3,ne3 = traverseBottomExp f state2 e3 in
      state3,PIf(ne1,ne2,ne3)
   | StmtVal(binds) ->
      let state1,nbinds = traverseBottomValBindListExp f state binds in
      state1,StmtVal(binds)
   | StmtMem(binds) ->
      let state1,nbinds = traverseBottomValBindListExp f state binds in
      state1,StmtVal(binds)
   | StmtReturn(e) ->
      let state1,ne = traverseBottomExp f state e in
      f state1 (StmtReturn(ne))
   | StmtBind(e1,e2) ->
      let state1,ne1 = traverseBottomExp f state e1 in
      let state2,ne2 = traverseBottomExp f state1 e2 in
      f state2 (StmtBind(ne1,ne2))
   | StmtEmpty ->
      f state exp
   | StmtFun(name,args,stmts) ->
      let state1,nstmts = traverseBottomExpList f state stmts in
      f state1 (StmtFun(name,args,nstmts))
   | StmtIf(cond,then_stmts,None) ->
      let state1,ncond = traverseBottomExp f state cond in
      let state2,nthen_stmts = traverseBottomExpList f state1 then_stmts in
      f state2 (StmtIf(ncond,nthen_stmts,None))
   | StmtIf(cond,then_stmts,Some(else_stmts)) ->
      let state1,ncond = traverseBottomExp f state cond in
      let state2,nthen_stmts = traverseBottomExpList f state1 then_stmts in
      let state3,nelse_stmts = traverseBottomExpList f state2 else_stmts in
      f state3 (StmtIf(ncond,nthen_stmts,Some(nelse_stmts)))

(** Traverses lists expressions bottom-up. The expressions are traversed right to left *)
and traverseBottomExpList (f: ('data, parse_exp) traverser) (state:'data) (expl:parse_exp list) : 'data * parse_exp list =
   foldTraverser_right traverseBottomExp f state expl

(** Applies a function to expressions in the bindings using a top-down traverser *)
and traverseBottomValBindExp (f: ('data, parse_exp) traverser) (state:'data) (val_bind:val_bind) =
   match val_bind with
   | ValBind(name,init_opt,value) ->
      let state1,new_init_opt = traverseBottomOptExp f state init_opt in
      let state2,new_value = traverseBottomExp f state value in
      state2,ValBind(name,new_init_opt,new_value)
   | ValNoBind(name,init_opt) ->
      let state1,new_init_opt = traverseBottomOptExp f state init_opt in
      state1,ValNoBind(name,new_init_opt)

and traverseBottomValBindListExp (f: ('data, parse_exp) traverser) (state:'data) (val_binds:val_bind list) =
   foldTraverser_right traverseBottomValBindExp f state val_binds

and traverseBottomOptExp (f: ('data, parse_exp) traverser) (state:'data) (exp_opt:parse_exp option) =
   match exp_opt with
   | Some(exp) ->
      let new_state,new_exp = f state exp in
      new_state,Some(new_exp)
   | None -> state,None

(** Traverses expressions top-down *)
let rec traverseTopExp (f: ('data, parse_exp) traverser) (state0:'data) (exp:parse_exp) : 'data * parse_exp =
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
   | PIf(e1,e2,e3) ->
      let state1,ne1 = traverseTopExp f state e1 in
      let state2,ne2 = traverseTopExp f state1 e2 in
      let state3,ne3 = traverseTopExp f state2 e3 in
      state3,PIf(ne1,ne2,ne3)
   | StmtVal(binds) ->
      let state1,nbinds = traverseTopValBindListExp f state binds in
      state1,StmtVal(nbinds)
   | StmtMem(binds) ->
      let state1,nbinds = traverseTopValBindListExp f state binds in
      state1,StmtMem(nbinds)
   | StmtReturn(e) ->
      let state1,ne = traverseTopExp f state e in
      state1,StmtReturn(ne)
   | StmtBind(e1,e2) ->
      let state1,ne1 = traverseTopExp f state e1 in
      let state2,ne2 = traverseTopExp f state1 e2 in
      state2,StmtBind(ne1,ne2)
   | StmtEmpty -> state,nexp
   | StmtFun(name,args,stmts) ->
      let state1,nstmts = traverseTopExpList f state stmts in
      state1,StmtFun(name,args,nstmts)
   | StmtIf(cond,then_stmts,None) ->
      let state1,ncond = traverseTopExp f state cond in
      let state2,nthen_stmts = traverseTopExpList f state1 then_stmts in
      state1,StmtIf(ncond,nthen_stmts,None)
   | StmtIf(cond,then_stmts,Some(else_stmts)) ->
      let state1,ncond = traverseTopExp f state cond in
      let state2,nthen_stmts = traverseTopExpList f state1 then_stmts in
      let state3,nelse_stmts = traverseTopExpList f state2 else_stmts in
      state3,StmtIf(ncond,nthen_stmts,Some(nelse_stmts))

(** Traverses lists expressions top-down. The expressions are traversed left to right *)
and traverseTopExpList (f: ('data, parse_exp) traverser) (state:'data) (expl:parse_exp list) : 'data * parse_exp list =
   foldTraverser_left traverseTopExp f state expl

(** Applies a function to expressions in the bindings using a top-down traverser *)
and traverseTopValBindExp (f: ('data, parse_exp) traverser) (state:'data) (val_bind:val_bind) =
   match val_bind with
   | ValBind(name,init_opt,value) ->
      let state1,new_init_opt = traverseTopOptExp f state init_opt in
      let state2,new_value = traverseTopExp f state value in
      state2,ValBind(name,new_init_opt,new_value)
   | ValNoBind(name,init_opt) ->
      let state1,new_init_opt = traverseTopOptExp f state init_opt in
      state1,ValNoBind(name,new_init_opt)

and traverseTopValBindListExp (f: ('data, parse_exp) traverser) (state:'data) (val_binds:val_bind list) =
   foldTraverser_left traverseTopValBindExp f state val_binds

and traverseTopOptExp (f: ('data, parse_exp) traverser) (state:'data) (exp_opt:parse_exp option) =
   match exp_opt with
   | Some(exp) ->
      let new_state,new_exp = f state exp in
      new_state,Some(new_exp)
   | None -> state,None

(** Folds expressions top-down *)
let rec foldTopExp (f: ('data, parse_exp) folder) (state0:'data) (exp:parse_exp) : 'data =
   let state = f state0 exp in
   match exp with
   | PEmpty
   | PUnit
   | PInt(_,_)
   | PReal(_,_)
   | PId(_)   -> state
   | PUnOp(name,e,loc) ->
      foldTopExp f state e
   | PBinOp(name,e1,e2,loc) ->
      let state1 = foldTopExp f state e1 in
      let state2 = foldTopExp f state1 e2 in
      state2
   | PGroup(e) ->
      let state1 = foldTopExp f state e in
      state1
   | PTuple(expl) ->
      let state1 = foldTopExpList f state expl in
      state1
   | PCall(name,expl,loc) ->
      let state1 = foldTopExpList f state expl in
      state1
   | PIf(e1,e2,e3) ->
      let state1 = foldTopExp f state e1 in
      let state2 = foldTopExp f state1 e2 in
      let state3 = foldTopExp f state2 e3 in
      state3
   | StmtVal(binds) ->
      let state1 = foldTopValBindExpList f state binds in
      state1
   | StmtMem(binds) ->
      let state1 = foldTopValBindExpList f state binds in
      state1
   | StmtReturn(e) ->
      let state1 = foldTopExp f state e in
      state1
   | StmtBind(e1,e2) ->
      let state1 = foldTopExp f state e1 in
      let state2 = foldTopExp f state1 e2 in
      state2
   | StmtEmpty -> state
   | StmtFun(name,args,stmts) ->
      let state1 = foldTopExpList f state stmts in
      state1
   | StmtIf(cond,then_stmts,None) ->
      let state1 = foldTopExp f state cond in
      let state2 = foldTopExpList f state1 then_stmts in
      state1
   | StmtIf(cond,then_stmts,Some(else_stmts)) ->
      let state1 = foldTopExp f state cond in
      let state2 = foldTopExpList f state1 then_stmts in
      let state3 = foldTopExpList f state2 else_stmts in
      state3

and foldTopExpList f state expl =
   List.fold_left
      (fun state elem ->
          let state1 = foldTopExp f state elem in
          state1)
      state expl

and foldTopValBindExp (f: ('data, parse_exp) folder) (state:'data) (val_bind:val_bind) =
   match val_bind with
   | ValBind(name,init_opt,value) ->
      let state1 = CCOpt.map (foldTopExp f state) init_opt |> CCOpt.get state in
      let state2 = foldTopExp f state1 value in
      state2
   | ValNoBind(name,init_opt) ->
      let state1 = CCOpt.map (foldTopExp f state) init_opt |> CCOpt.get state in
      state1

and foldTopValBindExpList f state expl =
   List.fold_left
      (fun state elem ->
          let state1 = foldTopValBindExp f state elem in
          state1)
      state expl

let rec expandStmt (f: ('data, parse_exp) expander) (state0:'data) (stmt:parse_exp) : 'data * parse_exp list =
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

and expandStmtList (f: ('data, parse_exp) expander) (state:'data) (stmts:parse_exp list) : 'data * parse_exp list =
   let state2,acc =
      List.fold_left
         (fun (state,acc) exp ->
             let state1,ne = expandStmt f state exp in
             (state1,(List.rev ne)::acc) )
         (state,[]) stmts in
   state2,List.rev (List.flatten acc)

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
   | NamedId (_,name,_,_) -> name

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

(** Used by getIdsInExp and getIdsInExpList to get the ids in expressions *)
let getId : ('data,parse_exp) folder =
   fun state exp ->
      match exp with
      | PId(name) -> name::state
      | _ -> state

(** Return the ids in an expression *)
let getIdsInExp (exp:parse_exp) : named_id list =
   foldTopExp getId [] exp

(** Return the ids in an expression list *)
let getIdsInExpList (expl:parse_exp list) : named_id list =
   foldTopExpList getId [] expl

(** Compares named_ids ignoring the locations *)
let compareName (a:named_id) (b:named_id) : bool =
   match a,b with
   | SimpleId(name_a,_),SimpleId(name_b,_)
   | NamedId(name_a,_,_,_),SimpleId(name_b,_)
   | SimpleId(name_a,_),NamedId(name_b,_,_,_)
   | NamedId(name_a,_,_,_),NamedId(name_b,_,_,_) when name_a = name_b -> true
   | _ -> false