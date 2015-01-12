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

(** Internal function used by 'joinSep' *)
let rec join_buff buff sep l =
  match l with
  | [] -> ()
  | [h] -> Buffer.add_string buff h
  | h::t ->
    Buffer.add_string buff h;
    Buffer.add_string buff sep;
    join_buff  buff sep t

(** Joins a list of strings using a separator 'sep' *)
let joinSep sep l =
  match l with
  | [] -> ""
  | _ ->
    let buff = Buffer.create 128 in
    join_buff buff sep l;
    Buffer.contents buff

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

(** Returns the location that follows the given location *)
let getFollowingLocation (loc:location) : location =
   let end_pos = { loc.end_pos with Lexing.pos_cnum = loc.end_pos.Lexing.pos_cnum } in
   { start_pos = end_pos; end_pos = end_pos }

(** Returns a new location with the start and end positions updated *)
let mergeLocations (loc1:location) (loc2:location) : location =
   if loc1 = default_loc then
      loc2
   else if loc2 = default_loc then
      loc1
   else
      let start_pos,end_pos = getMinMaxPositions [loc1.start_pos; loc2.start_pos; loc1.end_pos; loc2.end_pos] in
      { start_pos = start_pos; end_pos = end_pos }

(** Returns the location of a named_id *)
let getNamedIdLocation (id:named_id) : location =
   match id with
   | SimpleId(_,loc)        -> loc
   | NamedId(_,_,loc1,loc2) -> mergeLocations loc1 loc2

(** Returns the location of an expression *)
let getExpLocation (e:parse_exp)  : location =
   match e with
   | PUnit(loc)
   | PInt(_,loc)
   | PReal(_,loc) -> loc
   | PId(id) -> getNamedIdLocation id
   | PUnOp(_,_,loc)
   | PBinOp(_,_,_,loc)
   | PCall(_,_,loc,_)
   | PIf(_,_,_,loc)
   | PGroup(_,loc)
   | PTuple(_,loc) -> loc
   | PEmpty -> default_loc
   | PSeq(_,loc) -> loc

   | StmtVal(_,_,loc)
   | StmtMem(_,_,_,loc)
   | StmtReturn(_,loc)
   | StmtIf(_,_,_,loc)
   | StmtFun(_,_,_,loc)
   | StmtBind(_,_,loc) -> loc
   | StmtEmpty -> default_loc
   | StmtBlock(_,loc) -> loc

(** Folds the list (left-right) using the given traverser functions *)
let foldTraverser_left traverser_function pred (traverser:('data,'traversing_type) traverser) (state:'data) (elems:'elem list) =
   let state2,acc =
      List.fold_left
         (fun (state,acc) elem ->
             let state1,ne = traverser_function pred traverser state elem in
             (state1,ne::acc) )
         (state,[]) elems in
   state2,List.rev acc

(** Folds the list (right-left) using the given traverser functions *)
let foldTraverser_right traverser_function pred (traverser:('data,'traversing_type) traverser) (state:'data) (elems:'elem list) =
   let state2,acc =
      List.fold_left
         (fun (state,acc) elem ->
             let state1,ne = traverser_function pred traverser state elem in
             (state1,ne::acc) )
         (state,[]) (List.rev elems) in
   state2,acc

(** Fold an expression with the 'expfold' type that contains the functions to apply  *)
let expressionFoldEither : ('data, 'error, 'result) expfold -> 'data -> parse_exp -> ('error, 'result) either =
   fun fold data exp ->
      let rec go e = match e with
         | PUnit(_) -> fold.vUnit data
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
         | PCall (n,es,l,_) -> begin match eitherTryMap go es with
               | Right rs -> fold.vCall data n rs l
               | Left _ as err -> err
            end
         | PIf (e1,e2,e3,_) -> begin match (go e1, go e2, go e3) with
               | (Right r1, Right r2, Right r3) -> fold.vIf data r1 r2 r3
               | (Left _ as err,_,_) -> err
               | (_,(Left _ as err),_) -> err
               | (_,_,(Left _ as err)) -> err
            end
         | PGroup (e1,_) -> begin match go e1 with
               | Right r1 -> fold.vGroup data r1
               | Left _ as err -> err
            end
         | PTuple(es,_) -> begin match eitherTryMap go es with
               | Right rs -> fold.vTuple data rs
               | Left _ as err -> err
            end
         | PEmpty -> fold.vEmpty data
         | _ -> failwith "expressionFoldEither: does not support statements yet"
      in
      go exp

(** Used to traverse a named_id 'name' as 'PId(name)' *)
let traverseNamedIdAsExp (pred:(parse_exp -> bool) option) (f: ('data, parse_exp) traverser) (state:'data) (name:named_id) : 'data * named_id =
   let state1,new_name_exp = f state (PId(name)) in
   match new_name_exp with
   | PId(new_name) -> state1,new_name
   | _ -> state1,name

(** Traverses expressions bottom-up *)
let rec traverseBottomExp (pred:(parse_exp -> bool) option) (f: ('data, parse_exp) traverser) (state:'data) (exp:parse_exp) : 'data * parse_exp =
   match pred with
   | Some(pred_f) when not (pred_f exp)-> state,exp
   | _ ->
      match exp with
      | PEmpty
      | PUnit(_)
      | PInt(_,_)
      | PReal(_,_)
      | PId(_)   -> f state exp
      | PUnOp(name,e,loc) ->
         let state1,ne = traverseBottomExp pred f state e in
         f state1 (PUnOp(name,ne,loc))
      | PBinOp(name,e1,e2,loc) ->
         let state1,ne1 = traverseBottomExp pred f state e1 in
         let state2,ne2 = traverseBottomExp pred f state1 e2 in
         f state2 (PBinOp(name,ne1,ne2,loc))
      | PGroup(e,loc) ->
         let state1,ne = traverseBottomExp pred f state e in
         f state1 (PGroup(ne,loc))
      | PTuple(expl,loc) ->
         let state1,nexpl = traverseBottomExpList pred f state expl in
         f state1 (PTuple(nexpl,loc))
      | PCall(name,expl,loc,attr) ->
         let state1,nexpl = traverseBottomExpList pred f state expl in
         f state1 (PCall(name,nexpl,loc,attr))
      | PIf(e1,e2,e3,loc) ->
         let state1,ne1 = traverseBottomExp pred f state e1 in
         let state2,ne2 = traverseBottomExp pred f state1 e2 in
         let state3,ne3 = traverseBottomExp pred f state2 e3 in
         f state3 (PIf(ne1,ne2,ne3,loc))
      | PSeq(stmts,loc) ->
         let state1,nstmts = traverseBottomExpList pred f state stmts in
         f state1 (PSeq(nstmts,loc))
      | StmtVal(e1,e2,loc) ->
         let state1,ne1 = traverseBottomExp pred f state e1 in
         let state2,ne2 = traverseBottomOptExp pred f state1 e2 in
         f state2 (StmtVal(ne1,ne2,loc))
      | StmtMem(e1,e2,e3,loc) ->
         let state1,ne1 = traverseBottomExp pred f state e1 in
         let state2,ne2 = traverseBottomOptExp pred f state1 e2 in
         let state3,ne3 = traverseBottomOptExp pred f state2 e3 in
         f state3 (StmtMem(ne1,ne2,ne3,loc))
      | StmtReturn(e,loc) ->
         let state1,ne = traverseBottomExp pred f state e in
         f state1 (StmtReturn(ne,loc))
      | StmtBind(e1,e2,loc) ->
         let state1,ne1 = traverseBottomExp pred f state e1 in
         let state2,ne2 = traverseBottomExp pred f state1 e2 in
         f state2 (StmtBind(ne1,ne2,loc))
      | StmtEmpty ->
         f state exp
      | StmtFun(name,args,stmts,loc) ->
         let state1,nstmts = traverseBottomExp pred f state stmts in
         f state1 (StmtFun(name,args,nstmts,loc))
      | StmtIf(cond,then_stmts,None,loc) ->
         let state1,ncond = traverseBottomExp pred f state cond in
         let state2,nthen_stmts = traverseBottomExp pred f state1 then_stmts in
         f state2 (StmtIf(ncond,nthen_stmts,None,loc))
      | StmtIf(cond,then_stmts,Some(else_stmts),loc) ->
         let state1,ncond = traverseBottomExp pred f state cond in
         let state2,nthen_stmts = traverseBottomExp pred f state1 then_stmts in
         let state3,nelse_stmts = traverseBottomExp pred f state2 else_stmts in
         f state3 (StmtIf(ncond,nthen_stmts,Some(nelse_stmts),loc))
      | StmtBlock(stmts,loc) ->
         let state1,nstmts = traverseBottomExpList pred f state stmts in
         f state1 (StmtBlock(nstmts,loc))

(** Traverses lists expressions bottom-up. The expressions are traversed right to left *)
and traverseBottomExpList (pred:(parse_exp -> bool) option) (f: ('data, parse_exp) traverser) (state:'data) (expl:parse_exp list) : 'data * parse_exp list =
   foldTraverser_right traverseBottomExp pred f state expl

and traverseBottomOptExp (pred:(parse_exp -> bool) option) (f: ('data, parse_exp) traverser) (state:'data) (exp_opt:parse_exp option) =
   match exp_opt,pred with
   | Some(exp),Some(pred_f) when not (pred_f exp) -> state, exp_opt
   | Some(exp),_ ->
      let new_state,new_exp = traverseBottomExp pred f state exp in
      new_state,Some(new_exp)
   | None,_ -> state,None

(** Traverses expressions top-down *)
let rec traverseTopExp (pred:(parse_exp -> bool) option) (f: ('data, parse_exp) traverser) (state0:'data) (exp:parse_exp) : 'data * parse_exp =
   match pred with
   | Some(pred_f) when not (pred_f exp)-> state0,exp
   | _ ->
      let state,nexp = f state0 exp in
      match nexp with
      | PEmpty
      | PUnit(_)
      | PInt(_,_)
      | PReal(_,_)
      | PId(_)   -> state,nexp
      | PUnOp(name,e,loc) ->
         let state1,ne = traverseTopExp pred f state e in
         state1,PUnOp(name,ne,loc)
      | PBinOp(name,e1,e2,loc) ->
         let state1,ne1 = traverseTopExp pred f state e1 in
         let state2,ne2 = traverseTopExp pred f state1 e2 in
         state2,PBinOp(name,ne1,ne2,loc)
      | PGroup(e,loc) ->
         let state1,ne = traverseTopExp pred f state e in
         state1,PGroup(ne,loc)
      | PTuple(expl,loc) ->
         let state1,nexpl = traverseTopExpList pred f state expl in
         state1,PTuple(nexpl,loc)
      | PCall(name,expl,loc,attr) ->
         let state1,nexpl = traverseTopExpList pred f state expl in
         state1,PCall(name,nexpl,loc,attr)
      | PIf(e1,e2,e3,loc) ->
         let state1,ne1 = traverseTopExp pred f state e1 in
         let state2,ne2 = traverseTopExp pred f state1 e2 in
         let state3,ne3 = traverseTopExp pred f state2 e3 in
         state3,PIf(ne1,ne2,ne3,loc)
      | PSeq(stmts,loc) ->
         let state1,nstmts = traverseTopExpList pred f state stmts in
         state1,PSeq(nstmts,loc)
      | StmtVal(e1,e2,loc) ->
         let state1,ne1 = traverseTopExp pred f state e1 in
         let state2,ne2 = traverseTopOptExp pred f state1 e2 in
         state2,StmtVal(ne1,ne2,loc)
      | StmtMem(e1,e2,e3,loc) ->
         let state1,ne1 = traverseTopExp pred f state e1 in
         let state2,ne2 = traverseTopOptExp pred f state1 e2 in
         let state3,ne3 = traverseTopOptExp pred f state2 e3 in
         state3,StmtMem(ne1,ne2,ne3,loc)
      | StmtReturn(e,loc) ->
         let state1,ne = traverseTopExp pred f state e in
         state1,StmtReturn(ne,loc)
      | StmtBind(e1,e2,loc) ->
         let state1,ne1 = traverseTopExp pred f state e1 in
         let state2,ne2 = traverseTopExp pred f state1 e2 in
         state2,StmtBind(ne1,ne2,loc)
      | StmtEmpty -> state,nexp
      | StmtFun(name,args,stmts,loc) ->
         let state1,nstmts = traverseTopExp pred f state stmts in
         state1,StmtFun(name,args,nstmts,loc)
      | StmtIf(cond,then_stmts,None,loc) ->
         let state1,ncond = traverseTopExp pred f state cond in
         let state2,nthen_stmts = traverseTopExp pred f state1 then_stmts in
         state1,StmtIf(ncond,nthen_stmts,None,loc)
      | StmtIf(cond,then_stmts,Some(else_stmts),loc) ->
         let state1,ncond = traverseTopExp pred f state cond in
         let state2,nthen_stmts = traverseTopExp pred f state1 then_stmts in
         let state3,nelse_stmts = traverseTopExp pred f state2 else_stmts in
         state3,StmtIf(ncond,nthen_stmts,Some(nelse_stmts),loc)
      | StmtBlock(stmts,loc) ->
         let state1,nstmts = traverseTopExpList pred f state stmts in
         state1,StmtBlock(nstmts,loc)


(** Traverses lists expressions top-down. The expressions are traversed left to right *)
and traverseTopExpList (pred:(parse_exp -> bool) option) (f: ('data, parse_exp) traverser) (state:'data) (expl:parse_exp list) : 'data * parse_exp list =
   foldTraverser_left traverseTopExp pred f state expl

and traverseTopOptExp (pred:(parse_exp -> bool) option) (f: ('data, parse_exp) traverser) (state:'data) (exp_opt:parse_exp option) =
   match exp_opt,pred with
   | Some(exp),Some(pred_f) when not (pred_f exp) -> state,exp_opt
   | Some(exp),_ ->
      let new_state,new_exp = traverseTopExp pred f state exp in
      new_state,Some(new_exp)
   | None,_ -> state,None

(** Folds expressions top-down *)
let rec foldTopExp (pred:(parse_exp -> bool) option) (f: ('data, parse_exp) folder) (state0:'data) (exp:parse_exp) : 'data =
   match pred with
   | Some(pred_f) when not (pred_f exp)-> state0
   | _ ->
      let state = f state0 exp in
      match exp with
      | PEmpty
      | PUnit(_)
      | PInt(_,_)
      | PReal(_,_)
      | PId(_)   -> state
      | PUnOp(name,e,loc) ->
         foldTopExp pred f state e
      | PBinOp(name,e1,e2,loc) ->
         let state1 = foldTopExp pred f state e1 in
         let state2 = foldTopExp pred f state1 e2 in
         state2
      | PGroup(e,loc) ->
         let state1 = foldTopExp pred f state e in
         state1
      | PTuple(expl,_) ->
         let state1 = foldTopExpList pred f state expl in
         state1
      | PCall(name,expl,_,_) ->
         let state1 = foldTopExpList pred f state expl in
         state1
      | PIf(e1,e2,e3,loc) ->
         let state1 = foldTopExp pred f state e1 in
         let state2 = foldTopExp pred f state1 e2 in
         let state3 = foldTopExp pred f state2 e3 in
         state3
      | PSeq(stmts,_) ->
         let state1 = foldTopExpList pred f state stmts in
         state1
      | StmtVal(e1,e2,_) ->
         let state1 = foldTopExp pred f state e1 in
         let state2 = foldTopOptExp pred f state1 e2 in
         state2
      | StmtMem(e1,e2,e3,_) ->
         let state1 = foldTopExp pred f state e1 in
         let state2 = foldTopOptExp pred f state1 e2 in
         let state3 = foldTopOptExp pred f state2 e3 in
         state3
      | StmtReturn(e,_) ->
         let state1 = foldTopExp pred f state e in
         state1
      | StmtBind(e1,e2,_) ->
         let state1 = foldTopExp pred f state e1 in
         let state2 = foldTopExp pred f state1 e2 in
         state2
      | StmtEmpty -> state
      | StmtFun(name,args,stmts,_) ->
         let state1 = foldTopExp pred f state stmts in
         state1
      | StmtIf(cond,then_stmts,None,_) ->
         let state1 = foldTopExp pred f state cond in
         let state2 = foldTopExp pred f state1 then_stmts in
         state2
      | StmtIf(cond,then_stmts,Some(else_stmts),_) ->
         let state1 = foldTopExp pred f state cond in
         let state2 = foldTopExp pred f state1 then_stmts in
         let state3 = foldTopExp pred f state2 else_stmts in
         state3
      | StmtBlock(stmts,_) ->
         let state1 = foldTopExpList pred f state stmts in
         state1


and foldTopExpList (pred:(parse_exp -> bool) option) (f: ('data, parse_exp) folder) (state:'data) (expl:parse_exp list) : 'data =
   List.fold_left
      (fun state elem ->
          let state1 = foldTopExp pred f state elem in
          state1)
      state expl

and foldTopOptExp (pred:(parse_exp -> bool) option) (f: ('data, parse_exp) folder) (state:'data) (exp:parse_exp option) =
   match exp,pred with
   | Some(e),Some(pred_f) when not (pred_f e) -> state
   | Some(e),_ ->
      foldTopExp pred f state e
   | _ -> state

(** Folds expressions bottom-up *)
let rec foldDownExp (pred:(parse_exp -> bool) option) (f: ('data, parse_exp) folder) (state:'data) (exp:parse_exp) : 'data =
   match pred with
   | Some(pred_f) when not (pred_f exp)-> state
   | _ ->
      match exp with
      | PEmpty
      | PUnit(_)
      | PInt(_,_)
      | PReal(_,_)
      | PId(_)   -> f state exp
      | PUnOp(name,e,loc) ->
         let state1 = foldDownExp pred f state e in
         f state1 exp
      | PBinOp(name,e1,e2,loc) ->
         let state1 = foldDownExp pred f state e1 in
         let state2 = foldDownExp pred f state1 e2 in
         f state2 exp
      | PGroup(e,loc) ->
         let state1 = foldDownExp pred f state e in
         f state1 exp
      | PTuple(expl,_) ->
         let state1 = foldDownExpList pred f state expl in
         f state1 exp
      | PCall(name,expl,_,_) ->
         let state1 = foldDownExpList pred f state expl in
         f state1 exp
      | PIf(e1,e2,e3,loc) ->
         let state1 = foldDownExp pred f state e1 in
         let state2 = foldDownExp pred f state1 e2 in
         let state3 = foldDownExp pred f state2 e3 in
         f state3 exp
      | PSeq(stmts,_) ->
         let state1 = foldDownExpList pred f state stmts in
         f state1 exp
      | StmtVal(e1,e2,_) ->
         let state1 = foldDownExp pred f state e1 in
         let state2 = foldDownOptExp pred f state1 e2 in
         f state2 exp
      | StmtMem(e1,e2,e3,_) ->
         let state1 = foldDownExp pred f state e1 in
         let state2 = foldDownOptExp pred f state1 e2 in
         let state3 = foldDownOptExp pred f state2 e3 in
         f state3 exp
      | StmtReturn(e,_) ->
         let state1 = foldDownExp pred f state e in
         f state1 exp
      | StmtBind(e1,e2,_) ->
         let state1 = foldDownExp pred f state e1 in
         let state2 = foldDownExp pred f state1 e2 in
         f state2 exp
      | StmtEmpty -> state
      | StmtFun(name,args,stmts,_) ->
         let state1 = foldDownExp pred f state stmts in
         f state1 exp
      | StmtIf(cond,then_stmts,None,_) ->
         let state1 = foldDownExp pred f state cond in
         let state2 = foldDownExp pred f state1 then_stmts in
         f state2 exp
      | StmtIf(cond,then_stmts,Some(else_stmts),_) ->
         let state1 = foldDownExp pred f state cond in
         let state2 = foldDownExp pred f state1 then_stmts in
         let state3 = foldDownExp pred f state2 else_stmts in
         f state3 exp
      | StmtBlock(stmts,_) ->
         let state1 = foldDownExpList pred f state stmts in
         f state1 exp


and foldDownExpList (pred:(parse_exp -> bool) option) (f: ('data, parse_exp) folder) (state:'data) (expl:parse_exp list) : 'data =
   List.fold_left
      (fun state elem ->
          let state1 = foldDownExp pred f state elem in
          state1)
      state (List.rev expl)

and foldDownOptExp (pred:(parse_exp -> bool) option) (f: ('data, parse_exp) folder) (state:'data) (exp:parse_exp option) =
   match exp,pred with
   | Some(e),Some(pred_f) when not (pred_f e) -> state
   | Some(e),_ ->
      foldDownExp pred f state e
   | _ -> state

(** If the list of statements has more than one element return a PSeq instead *)
let makePSeq (loc:location) (l:parse_exp list) : parse_exp =
   match l with
   | [] -> PUnit(loc)
   | [h] -> h
   | _ -> PSeq(l,loc)

(** If the list of statements has more than one element return a StmtBlock instead *)
let makeStmtBlock (loc:location) (l:parse_exp list) : parse_exp =
   match l with
   | [] -> StmtBlock([],loc)
   | [h] -> h
   | _ -> StmtBlock(l,loc)

(** Appends the contents of a list of blocks *)
let appendBlocksList (l:parse_exp list) : parse_exp list * location =
   let rec loop acc loc e =
      match e with
      | [] -> List.flatten (List.rev acc),loc
      | StmtBlock(stmts,sloc)::t ->
         let new_loc = mergeLocations loc sloc in
         loop (stmts::acc) new_loc t
      | h::t ->
         let new_loc = mergeLocations loc (getExpLocation h) in
         loop ([h]::acc) new_loc t
   in
      loop [] default_loc l

(** Appends the contents of a list of blocks and returns a StmtBlock *)
let appendBlocks (l:parse_exp list) =
   let stmts,loc = appendBlocksList l in
   makeStmtBlock loc stmts

(** Appends the contents of a list of blocks and returns a PSeq *)
let appendPseq (l:parse_exp list) =
   let stmts,loc = appendBlocksList l in
   makePSeq loc stmts

let expandBlockOrSeq (stmt:parse_exp) : parse_exp list =
   match stmt with
   | StmtBlock(stmts,_) -> stmts
   | PSeq(stmts,_)      -> stmts
   | _ -> [stmt]

let rec expandStmt (pred:(parse_exp -> bool) option) (f: ('data, parse_exp) expander) (state:'data) (stmt:parse_exp) : 'data * parse_exp list =
   match pred with
   | Some(pred_f) when not (pred_f stmt)-> state,[stmt]
   | _ ->
      match stmt with
      | StmtEmpty -> f state stmt
      | StmtVal(e1,e2,loc) ->
         let state1,ne1 = expandStmt pred f state e1 in
         let state2,ne2 = expandOptStmt pred f state1 e2 in
         f state2 (StmtVal(appendPseq ne1,ne2,loc))
      | StmtMem(e1,e2,e3,loc) ->
         let state1,ne1 = expandStmt pred f state e1 in
         let state2,ne2 = expandOptStmt pred f state1 e2 in
         let state3,ne3 = expandOptStmt pred f state2 e3 in
         f state3 (StmtMem(appendPseq ne1,ne2,ne3,loc))
      | StmtBind(e1,e2,loc) ->
         let state1,ne1 = expandStmt pred f state e1 in
         let state2,ne2 = expandStmt pred f state1 e2 in
         f state2 (StmtBind(appendPseq ne1,appendPseq ne2,loc))
      | StmtReturn(e,loc) ->
         let state1,ne = expandStmt pred f state e in
         f state1 (StmtReturn(appendPseq ne,loc))
      | StmtFun(name,args,stmts,loc) ->
         let state1,nstmts = expandStmt pred f state stmts in
         f state1 (StmtFun(name,args,appendBlocks nstmts,loc))
      | StmtIf(cond,then_stmts,None,loc) ->
         let state1,nthen_stmts = expandStmt pred f state then_stmts in
         f state1 (StmtIf(cond,appendBlocks nthen_stmts,None,loc))
      | StmtIf(cond,then_stmts,Some(else_stmts),loc) ->
         let state1,nthen_stmts = expandStmt pred f state then_stmts in
         let state2,nelse_stmts = expandStmt pred f state1 else_stmts in
         f state2 (StmtIf(cond,appendBlocks nthen_stmts,Some(appendBlocks nelse_stmts),loc))
      | PSeq(el,loc) ->
         let state1,nel = expandStmtList pred f state el in
         f state1 (PSeq(nel,loc))

      | PUnit(_)
      | PInt(_,_)
      | PReal(_,_)
      | PEmpty
      | PId(_) -> f state stmt
      | PUnOp(op,e,loc) ->
         let state1,ne = expandStmt pred f state e in
         f state1 (PUnOp(op,appendPseq ne,loc))
      | PBinOp(op,e1,e2,loc) ->
         let state1,ne1 = expandStmt pred f state e1 in
         let state2,ne2 = expandStmt pred f state1 e2 in
         f state2 (PBinOp(op,appendPseq ne1,appendPseq ne2,loc))
      | PCall(name,args,loc,attr) ->
         let state1,nargs = expandStmtList pred f state args in
         f state1 (PCall(name,nargs,loc,attr))
      | PIf(cond,then_exp,else_exp,loc) ->
         let state1,ncond = expandStmt pred f state cond in
         let state2,nthen_exp = expandStmt pred f state1 then_exp in
         let state3,nelse_exp = expandStmt pred f state2 else_exp in
         f state3 (PIf(appendPseq ncond,appendPseq nthen_exp, appendPseq nelse_exp,loc))
      | PGroup(e,loc) ->
         let state1,ne = expandStmt pred f state e in
         f state1 (PGroup(appendPseq ne,loc))
      | PTuple(el,loc) ->
         let state1,nel = expandStmtList pred f state el in
         f state1 (PTuple(nel,loc))
      | StmtBlock(el,loc) ->
         let state1,nel = expandStmtList pred f state el in
         f state1 (StmtBlock(nel,loc))

and expandStmtList (pred:(parse_exp -> bool) option) (f: ('data, parse_exp) expander) (state:'data) (stmts:parse_exp list) : 'data * parse_exp list =
   let state2,acc =
      List.fold_left
         (fun (state,acc) exp ->
             let state1,ne = expandStmt pred f state exp in
             (state1,ne::acc) )
         (state,[]) (List.rev stmts) in
   state2,acc |> List.flatten |> appendBlocksList |> fst

and expandOptStmt (pred:(parse_exp -> bool) option) (f: ('data, parse_exp) expander) (state:'data) (stmt:parse_exp option) : 'data * parse_exp option =
   match stmt with
   | None -> state,None
   | Some(e) ->
      let state1,ne = expandStmt pred f state e in
      state1,Some(appendPseq ne)

let getNameFromNamedId (named_id:named_id) : identifier =
   match named_id with
   | SimpleId(name,_) -> name
   | NamedId (_,name,_,_) -> name

let getLocationFromNamedId (named_id:named_id) : location =
   match named_id with
   | SimpleId(_,loc) -> loc
   | NamedId (_,_,loc1,loc2) -> mergeLocations loc1 loc2

let getTypeFromNamedId (named_id:named_id) : identifier option =
   match named_id with
   | NamedId (_,nametype,_,_) -> Some nametype
   | _ -> None

let getFunctionTypeAndName (names_id:named_id) : identifier * identifier =
   match names_id with
   | NamedId(name,ftype,_,_) -> name,ftype
   | SimpleId(ftype,_) -> ["_"],ftype

(** Used by getIdsInExp and getIdsInExpList to get the ids in expressions *)
let getId : ('data,parse_exp) folder =
   fun state exp ->
      match exp with
      | PId(name) -> name::state
      | _ -> state

(** Return the ids in an expression *)
let getIdsInExp (exp:parse_exp) : named_id list =
   foldTopExp None getId [] exp

(** Return the ids in an expression list *)
let getIdsInExpList (expl:parse_exp list) : named_id list =
   foldTopExpList None getId [] expl

(** Compares named_ids ignoring the locations *)
let compareName (a:named_id) (b:named_id) : bool =
   match a,b with
   | SimpleId(name_a,_),SimpleId(name_b,_)
   | NamedId(name_a,_,_,_),SimpleId(name_b,_)
   | SimpleId(name_a,_),NamedId(name_b,_,_,_)
   | NamedId(name_a,_,_,_),NamedId(name_b,_,_,_) when name_a = name_b -> true
   | _ -> false

(** Compares named_ids ignoring the locations and returning an integer*)
let compareIntName (a:named_id) (b:named_id) : int =
   match a,b with
   | SimpleId(name_a,_),SimpleId(name_b,_)
   | NamedId(name_a,_,_,_),SimpleId(name_b,_)
   | SimpleId(name_a,_),NamedId(name_b,_,_,_)
   | NamedId(name_a,_,_,_),NamedId(name_b,_,_,_) -> compare name_a name_b

(** Compares a list of name_id ignoring the locations *)
let rec compareNameList (a: named_id list) (b:named_id list) : int =
   match a,b with
   | [],[] -> 0
   | h1::t1,h2::t2 ->
      let ret = compareIntName h1 h2 in
      if ret = 0 then compareNameList t1 t2
      else ret
   | _ -> compare a b

module NamedId =
struct
   type t = named_id
   let compare = compareIntName
end

module Identifier =
struct
   type t = identifier
   let compare = compare
end

module NamedIdMap = Map.Make(NamedId)
module IdentifierMap = CCMap.Make(Identifier)

(** Compares two expressions ignoring the locations *)
let rec compareExp (a:parse_exp) (b:parse_exp) : int =
   match a,b with
   | PUnit(_),PUnit(_) -> 0
   | PInt(v1,_),PInt(v2,_)   -> compare v1 v2
   | PReal(v1,_),PReal(v2,_) -> compare v1 v2
   | PId(v1),PId(v2)         -> compareIntName v1 v2
   | PUnOp(o1,e1,_),PUnOp(o2,v1,_) when o1=o2 ->
      compareExp e1 v1
   | PBinOp(o1,e1,e2,_),PBinOp(o2,v1,v2,_) when o1=o2 ->
      let ret = compareExp e1 v1 in
      if ret=0 then compareExp e2 v2 else ret
   | PCall(name1,args1,_,_),PCall(name2,args2,_,_) ->
      let ret = compareIntName name1 name2 in
      if ret = 0 then
         compareExpList args1 args2
      else ret
   | PIf(cond1,then1,else1,_),PIf(cond2,then2,else2,_) ->
      let ret1 = compareExp cond1 cond2 in
      if ret1 = 0 then
         let ret2 = compareExp then1 then2 in
         if ret2 = 0 then
            compareExp else1 else2
         else ret2
      else ret1
   | PGroup(e1,_),PGroup(e2,_) -> compareExp e1 e2
   | PTuple(e1,_),PTuple(e2,_) -> compareExpList e1 e2
   | PSeq(e1,_),PSeq(e2,_) -> compareExpList e1 e2
   | PEmpty,PEmpty -> 0
   | StmtVal(e1,b1,_),StmtVal(e2,b2,_) ->
      let ret = compareExp e1 e2 in
      if ret = 0 then
         compareOptExp b1 b2
      else ret
   | StmtMem(e1,b1,i1,_),StmtMem(e2,b2,i2,_) ->
      let ret1 = compareExp e1 e2 in
      if ret1 = 0 then
         let ret2 = compareOptExp b1 b2 in
         if ret2 = 0 then compareOptExp i1 i2
         else ret2
      else ret1
   | StmtReturn(e1,_),StmtReturn(e2,_) -> compareExp e1 e2
   | StmtIf(cond1,then1,else1,_),StmtIf(cond2,then2,else2,_) ->
      let ret1 = compareExp cond1 cond2 in
      if ret1 = 0 then
         let ret2 = compareExp then1 then2 in
         if ret2 = 0 then
            compareOptExp else1 else2
         else ret2
      else ret1
   | StmtFun(name1,args1,body1,_),StmtFun(name2,args2,body2,_) ->
      let ret1 = compareIntName name1 name2 in
      if ret1=0 then
         let ret2 = compareNameList args1 args2 in
         if ret2=0 then compareExp body1 body2
         else ret2
      else ret1
   | StmtBind(e1,v1,_),StmtBind(e2,v2,_) ->
      let ret = compareExp e1 e2 in
      if ret=0 then compareExp v1 v2
      else ret
   | StmtBlock(e1,_),StmtBlock(e2,_) -> compareExpList e1 e2
   | StmtEmpty,StmtEmpty -> 0
   | _ -> compare a b

(** Compares two expression lists ignoring the locations *)
and compareExpList (a:parse_exp list) (b:parse_exp list) : int =
   match a,b with
   | [],[] -> 0
   | h1::t1,h2::t2 ->
      let ret = compareExp h1 h2 in
      if ret = 0 then
         compareExpList t1 t2
      else ret
   | _ -> compare a b

(** Compares two option expressions ignoring the locations *)
and compareOptExp (a:parse_exp option) (b:parse_exp option) : int =
   match a,b with
   | None,None         -> 0
   | Some(e1),Some(e2) -> compareExp e1 e2
   | _ -> compare a b

(** Returns the full location of an expression *)
let getExpFullLocation (e:parse_exp) : location =
   let f state e =
      let current_loc = getExpLocation e in
      mergeLocations state current_loc
   in foldTopExp None f default_loc e

(**  Counts the number of function calls (operations) expression list has *)
let getExpWeight (e:parse_exp) : int =
   let count acc e =
      match e with
      | PCall(_) -> acc+1
      | _ -> acc
   in foldTopExp None count 0 e

(** Removes the type from a named_id *)
let removeNamedIdType (name:named_id) : named_id =
   match name with
   | NamedId(n,_,loc,_) -> SimpleId(n,loc)
   | _ -> name

(** Converts an indentifier in a string by separating the names with dot *)
let identifierStr (id:identifier) = (joinSep "." id)

(** Prefixes an identifier with a string *)
let prefixId (pre:string) (id:identifier) =
   match id with
   | []   -> []
   | h::t -> (pre^h)::t

(** Postfix the identifier with the given string *)
let rec postfixId (id:identifier) (pos:string) =
   match id with
   | []  -> []
   | [h] -> [pos^h]
   | h::t -> h::(postfixId t pos)

(** Converts a name_id to string. This function is used mainly for debugging since PrintTypes contains more powerful functions *)
let namedIdStr (name:named_id) : string =
   match name with
   | NamedId(n,t,_,_) -> (identifierStr n)^":"^(identifierStr t)
   | SimpleId(n,_) -> identifierStr n

(** Adds the give prefix to the named_id *)
let prefixNamedId (prefix:string) (name:named_id) : named_id =
   match name with
   | SimpleId(n,loc)         -> SimpleId((prefixId prefix n),loc)
   | NamedId(n,tp,loc1,loc2) -> NamedId((prefixId prefix n),tp,loc1,loc2)

(** Checks if the expression contains any node for which the function 'f' is true *)
let exists f exp =
   foldTopExp None (fun state a -> state || f a) false exp

