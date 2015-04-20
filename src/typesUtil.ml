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

open TypesVult
open Either
open Scope

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

type binding_info =
   | FunctionInfo
   | TypeInfo
   [@@deriving show,eq,ord]

(** Converts an indentifier in a string by separating the names with dot *)
let identifierStr (id:identifier) : string = joinSep "." id

(** Converts a list of identifiers into a coma separated string *)
let identifierStrList (ids:identifier list) : string =
   List.map identifierStr ids |> joinSep ", "

module BindingInfo =
struct
   type t = identifier
   type v = binding_info
   let compare  = compare_identifier
   let string_t = identifierStr
   let string_v _ = ""
end

module BindingsScope = Scope(BindingInfo)
(** Used to track the scope in all traversers *)
type 'a tstate =
   {
      scope : BindingsScope.t;
      data  : 'a
   }

(** Type of all traversing functions *)
type ('data,'traversing_type) traverser = 'data tstate -> 'traversing_type -> 'data tstate * 'traversing_type

(** Type of all expanding functions *)
type ('data,'expanding_type) expander = 'data tstate -> 'expanding_type -> 'data tstate * 'expanding_type list

(** Type of folding functions *)
type ('data,'traversing_type) folder = 'data tstate -> 'traversing_type -> 'data tstate

type ('data, 'error, 'result) expfold =
   {
      vUnit  : 'data -> ('error, 'result) either;
      vInt   : 'data -> string -> location -> ('error, 'result) either;
      vReal  : 'data -> string -> location -> ('error, 'result) either;
      vId    : 'data -> identifier -> location -> ('error, 'result) either;
      vUnOp  : 'data -> string -> 'result -> location -> ('error, 'result) either;
      vBinOp : 'data -> string -> 'result -> 'result -> location -> ('error, 'result) either;
      vCall  : 'data -> identifier -> 'result list -> location -> ('error, 'result) either;
      vIf    : 'data -> 'result -> 'result -> 'result -> ('error, 'result) either;
      vGroup : 'data -> 'result -> ('error, 'result) either;
      vTuple : 'data -> 'result list -> ('error, 'result) either;
      vEmpty : 'data -> ('error, 'result) either;
   }



(** Returns a traversing state *)
let createState (data:'a) : 'a tstate =
   { scope = BindingsScope.empty; data = data }

(** Sets the data for the traversing state *)
let setState (s:'a tstate) (data:'a) : 'a tstate =
   { s with data = data }

(** Gets the data for the traversing state *)
let getState (s:'a tstate) : 'a =
   s.data

(** Creates a new state keepin the internal data *)
let deriveState (s:'a tstate) (data:'b) : 'b tstate =
   { scope = s.scope; data = data}

(** Adds a name to the scope *)
let pushScope (s:'a tstate) (name:identifier) : 'a tstate =
   (*Printf.printf " - Entering to scope '%s'\n" (joinSep "." name);*)
   { s with scope = BindingsScope.enter s.scope name }

(** Removes the last name from the scope *)
let popScope (s:'a tstate) : 'a tstate =
   { s with scope = BindingsScope.exit s.scope }

(** Returns the current scope *)
let getScope (s:'a tstate) : identifier =
   BindingsScope.getCurrentPath s.scope |> List.flatten

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
   | SimpleId(_,loc)  -> loc
   | NamedId(_,_,loc) -> loc

(** Returns the location of an expression *)
let getExpLocation (e:exp)  : location =
   match e with
   | PUnit(loc)
   | PInt(_,loc)
   | PBool(_,loc)
   | PReal(_,loc) -> loc
   | PId(_,_,loc) -> loc
   | PUnOp(_,_,loc)
   | PBinOp(_,_,_,loc)
   | PCall(_,_,_,loc,_)
   | PIf(_,_,_,loc)
   | PGroup(_,loc)
   | PTuple(_,loc) -> loc
   | PEmpty -> default_loc
   | PSeq(_,_,loc) -> loc
   | PTyped(_,_,loc) -> loc

   | StmtVal(_,_,loc)
   | StmtMem(_,_,_,loc)
   | StmtReturn(_,loc)
   | StmtIf(_,_,_,loc)
   | StmtFun(_,_,_,_,_,loc)
   | StmtBind(_,_,loc) -> loc
   | StmtEmpty -> default_loc
   | StmtBlock(_,_,loc) -> loc
   | StmtWhile(_,_,loc) -> loc
   | StmtType(_,_,_,loc) -> loc
   | StmtAliasType(_,_,_,loc) -> loc

(** Folds the list (left-right) using the given traverser functions *)
let foldTraverser_left traverser_function pred (traverser:('data,'traversing_type) traverser) (state:'data tstate) (elems:'elem list) =
   let state2,acc =
      List.fold_left
         (fun (state,acc) elem ->
             let state1,ne = traverser_function pred traverser state elem in
             (state1,ne::acc) )
         (state,[]) elems in
   state2,List.rev acc

(** Folds the list (right-left) using the given traverser functions *)
let foldTraverser_right traverser_function pred (traverser:('data,'traversing_type) traverser) (state:'data tstate) (elems:'elem list) =
   let state2,acc =
      List.fold_left
         (fun (state,acc) elem ->
             let state1,ne = traverser_function pred traverser state elem in
             (state1,ne::acc) )
         (state,[]) (List.rev elems) in
   state2,acc

(** Fold an expression with the 'expfold' type that contains the functions to apply  *)
let expressionFoldEither : ('data, 'error, 'result) expfold -> 'data -> exp -> ('error, 'result) either =
   fun fold data exp ->
      let rec go e = match e with
         | PUnit(_) -> fold.vUnit data
         | PInt (s,l) -> fold.vInt data s l
         | PReal (s,l) -> fold.vReal data s l
         | PId(n,_,loc) -> fold.vId data n loc
         | PUnOp (s,e1,l) -> begin match go e1 with
               | Right r1 -> fold.vUnOp data s r1 l
               | Left _ as err -> err
            end
         | PBinOp (s,e1,e2,l) -> begin match (go e1, go e2) with
               | (Right r1, Right r2) -> fold.vBinOp data s r1 r2 l
               | (Left _ as err, _) -> err
               | (_, (Left _ as err)) -> err
            end
         | PCall (_,n,es,l,_) -> begin match eitherTryMap go es with
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

(** Used to traverse a identifier 'name' as 'PId(name)' *)
let traverseNamedIdAsExp (pred:(exp -> bool) option) (f: ('data, exp) traverser) (state:'data tstate) (name:identifier) : 'data tstate * identifier =
   let state1,new_name_exp = f state (PId(name,None,default_loc)) in
   match new_name_exp with
   | PId(new_name,_,_) -> state1,new_name
   | _ -> state1,name

(** Traverses expressions bottom-up *)
let rec traverseBottomExp (pred:(exp -> bool) option) (f: ('data, exp) traverser) (state:'data tstate) (exp:exp) : 'data tstate * exp =
   match pred with
   | Some(pred_f) when not (pred_f exp)-> state,exp
   | _ ->
      match exp with
      | PEmpty
      | PUnit(_)
      | PInt(_,_)
      | PReal(_,_)
      | PBool(_,_)
      | PId(_)   -> f state exp
      | PUnOp(name,e,loc) ->
         let state1,ne = traverseBottomExp pred f state e in
         f state1 (PUnOp(name,ne,loc))
      | PTyped(e,type_exp,loc) ->
         let state1,ne = traverseBottomExp pred f state e in
         f state1 (PTyped(ne,type_exp,loc))
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
      | PCall(name,fname,expl,loc,attr) ->
         let state1,nexpl = traverseBottomExpList pred f state expl in
         f state1 (PCall(name,fname,nexpl,loc,attr))
      | PIf(e1,e2,e3,loc) ->
         let state1,ne1 = traverseBottomExp pred f state e1 in
         let state2,ne2 = traverseBottomExp pred f state1 e2 in
         let state3,ne3 = traverseBottomExp pred f state2 e3 in
         f state3 (PIf(ne1,ne2,ne3,loc))
      | PSeq(name,stmts,loc) ->
         let state1,nstmts = traverseBottomExpList pred f state stmts in
         f state1 (PSeq(name,nstmts,loc))
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
      | StmtFun(name,args,stmts,type_exp,active,loc) ->
         let state0 = pushScope state name in
         let state1,nstmts = traverseBottomExp pred f state0 stmts in
         let state2,fexp = f state1 (StmtFun(name,args,nstmts,type_exp,active,loc)) in
         (popScope state2),fexp
      | StmtIf(cond,then_stmts,None,loc) ->
         let state1,ncond = traverseBottomExp pred f state cond in
         let state2,nthen_stmts = traverseBottomExp pred f state1 then_stmts in
         f state2 (StmtIf(ncond,nthen_stmts,None,loc))
      | StmtIf(cond,then_stmts,Some(else_stmts),loc) ->
         let state1,ncond = traverseBottomExp pred f state cond in
         let state2,nthen_stmts = traverseBottomExp pred f state1 then_stmts in
         let state3,nelse_stmts = traverseBottomExp pred f state2 else_stmts in
         f state3 (StmtIf(ncond,nthen_stmts,Some(nelse_stmts),loc))
      | StmtBlock(name,stmts,loc) ->
         let state1,nstmts = traverseBottomExpList pred f state stmts in
         f state1 (StmtBlock(name,nstmts,loc))
      | StmtWhile(e1,e2,loc) ->
         let state1,ne1 = traverseBottomExp pred f state e1 in
         let state2,ne2 = traverseBottomExp pred f state1 e2 in
         f state2 (StmtWhile(ne1,ne2,loc))
      | StmtType(_,_,_,_) -> (* Does not travers the internal expressions *)
         f state exp
      | StmtAliasType(_,_,_,_) -> (* Does not travers the internal expressions *)
         f state exp

(** Traverses lists expressions bottom-up. The expressions are traversed right to left *)
and traverseBottomExpList (pred:(exp -> bool) option) (f: ('data, exp) traverser) (state:'data tstate) (expl:exp list) : 'data tstate * exp list =
   foldTraverser_right traverseBottomExp pred f state expl

and traverseBottomOptExp (pred:(exp -> bool) option) (f: ('data, exp) traverser) (state:'data tstate) (exp_opt:exp option) =
   match exp_opt,pred with
   | Some(exp),Some(pred_f) when not (pred_f exp) -> state, exp_opt
   | Some(exp),_ ->
      let new_state,new_exp = traverseBottomExp pred f state exp in
      new_state,Some(new_exp)
   | None,_ -> state,None

(** Traverses expressions top-down *)
let rec traverseTopExp (pred:(exp -> bool) option) (f: ('data, exp) traverser) (state0:'data tstate) (exp:exp) : 'data tstate * exp =
   match pred with
   | Some(pred_f) when not (pred_f exp)-> state0,exp
   | _ ->
      let state,nexp =
         match exp with
         | StmtFun(name,_,_,_,_,_) ->
            (* If it's a function  enter to the scope before applying *)
            let state1 = pushScope state0 name in
            let state2,nexp = f state1 exp in
            popScope state2,nexp
         | _ -> f state0 exp
      in
      match nexp with
      | PEmpty
      | PUnit(_)
      | PInt(_,_)
      | PReal(_,_)
      | PBool(_,_)
      | PId(_)   -> state,nexp
      | PUnOp(name,e,loc) ->
         let state1,ne = traverseTopExp pred f state e in
         state1,PUnOp(name,ne,loc)
      | PTyped(e,type_exp,loc) ->
         let state1,ne = traverseTopExp pred f state e in
         state1,PTyped(ne,type_exp,loc)
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
      | PCall(name,fname,expl,loc,attr) ->
         let state1,nexpl = traverseTopExpList pred f state expl in
         state1,PCall(name,fname,nexpl,loc,attr)
      | PIf(e1,e2,e3,loc) ->
         let state1,ne1 = traverseTopExp pred f state e1 in
         let state2,ne2 = traverseTopExp pred f state1 e2 in
         let state3,ne3 = traverseTopExp pred f state2 e3 in
         state3,PIf(ne1,ne2,ne3,loc)
      | PSeq(name,stmts,loc) ->
         let state1,nstmts = traverseTopExpList pred f state stmts in
         state1,PSeq(name,nstmts,loc)
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
      | StmtFun(name,args,stmts,type_exp,active,loc) ->
         let state0 = pushScope state name in
         let state1,nstmts = traverseTopExp pred f state0 stmts in
         (popScope state1),StmtFun(name,args,nstmts,type_exp,active,loc)
      | StmtIf(cond,then_stmts,None,loc) ->
         let state1,ncond = traverseTopExp pred f state cond in
         let state2,nthen_stmts = traverseTopExp pred f state1 then_stmts in
         state1,StmtIf(ncond,nthen_stmts,None,loc)
      | StmtIf(cond,then_stmts,Some(else_stmts),loc) ->
         let state1,ncond = traverseTopExp pred f state cond in
         let state2,nthen_stmts = traverseTopExp pred f state1 then_stmts in
         let state3,nelse_stmts = traverseTopExp pred f state2 else_stmts in
         state3,StmtIf(ncond,nthen_stmts,Some(nelse_stmts),loc)
      | StmtBlock(name,stmts,loc) ->
         let state1,nstmts = traverseTopExpList pred f state stmts in
         state1,StmtBlock(name,nstmts,loc)
      | StmtWhile(e1,e2,loc) ->
         let state1,ne1 = traverseTopExp pred f state e1 in
         let state2,ne2 = traverseTopExp pred f state1 e2 in
         state2,StmtWhile(ne1,ne2,loc)
      | StmtType(_,_,_,_) ->
         state,nexp
      | StmtAliasType(_,_,_,_) ->
         state,nexp


(** Traverses lists expressions top-down. The expressions are traversed left to right *)
and traverseTopExpList (pred:(exp -> bool) option) (f: ('data, exp) traverser) (state:'data tstate) (expl:exp list) : 'data tstate * exp list =
   foldTraverser_left traverseTopExp pred f state expl

and traverseTopOptExp (pred:(exp -> bool) option) (f: ('data, exp) traverser) (state:'data tstate) (exp_opt:exp option) =
   match exp_opt,pred with
   | Some(exp),Some(pred_f) when not (pred_f exp) -> state,exp_opt
   | Some(exp),_ ->
      let new_state,new_exp = traverseTopExp pred f state exp in
      new_state,Some(new_exp)
   | None,_ -> state,None

(** Folds expressions top-down *)
let rec foldTopExp (pred:(exp -> bool) option) (f: ('data, exp) folder) (state0:'data tstate) (exp:exp) : 'data tstate =
   match pred with
   | Some(pred_f) when not (pred_f exp)-> state0
   | _ ->
      let state =
         match exp with
         | StmtFun(name,_,_,_,_,_) ->
            (* If it's a function  enter to the scope before applying *)
            let state1 = pushScope state0 name in
            let state2 = f state1 exp in
            popScope state2
         | _ -> f state0 exp
      in
      match exp with
      | PEmpty
      | PUnit(_)
      | PInt(_,_)
      | PReal(_,_)
      | PBool(_,_)
      | PId(_)   -> state
      | PUnOp(name,e,loc) ->
         foldTopExp pred f state e
      | PTyped(e,_,_) ->
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
      | PCall(name,fname,expl,_,_) ->
         let state1 = foldTopExpList pred f state expl in
         state1
      | PIf(e1,e2,e3,loc) ->
         let state1 = foldTopExp pred f state e1 in
         let state2 = foldTopExp pred f state1 e2 in
         let state3 = foldTopExp pred f state2 e3 in
         state3
      | PSeq(_,stmts,_) ->
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
      | StmtFun(name,args,stmts,type_exp,_,_) ->
         let state0 = pushScope state name in
         let state1 = foldTopExp pred f state0 stmts in
         popScope state1
      | StmtIf(cond,then_stmts,None,_) ->
         let state1 = foldTopExp pred f state cond in
         let state2 = foldTopExp pred f state1 then_stmts in
         state2
      | StmtIf(cond,then_stmts,Some(else_stmts),_) ->
         let state1 = foldTopExp pred f state cond in
         let state2 = foldTopExp pred f state1 then_stmts in
         let state3 = foldTopExp pred f state2 else_stmts in
         state3
      | StmtBlock(_,stmts,_) ->
         let state1 = foldTopExpList pred f state stmts in
         state1
      | StmtWhile(e1,e2,_) ->
         let state1 = foldTopExp pred f state e1 in
         let state2 = foldTopExp pred f state1 e2 in
         state2
      | StmtType(_,_,_,_) -> state
      | StmtAliasType(_,_,_,_) -> state


and foldTopExpList (pred:(exp -> bool) option) (f: ('data, exp) folder) (state:'data tstate) (expl:exp list) : 'data tstate =
   List.fold_left
      (fun state elem ->
          let state1 = foldTopExp pred f state elem in
          state1)
      state expl

and foldTopOptExp (pred:(exp -> bool) option) (f: ('data, exp) folder) (state:'data tstate) (exp:exp option) =
   match exp,pred with
   | Some(e),Some(pred_f) when not (pred_f e) -> state
   | Some(e),_ ->
      foldTopExp pred f state e
   | _ -> state

(** Folds expressions bottom-up *)
let rec foldDownExp (pred:(exp -> bool) option) (f: ('data, exp) folder) (state:'data tstate) (exp:exp) : 'data tstate =
   match pred with
   | Some(pred_f) when not (pred_f exp)-> state
   | _ ->
      match exp with
      | PEmpty
      | PUnit(_)
      | PInt(_,_)
      | PReal(_,_)
      | PBool(_,_)
      | PId(_)   -> f state exp
      | PUnOp(name,e,loc) ->
         let state1 = foldDownExp pred f state e in
         f state1 exp
      | PTyped(e,_,_) ->
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
      | PCall(name,_,expl,_,_) ->
         let state1 = foldDownExpList pred f state expl in
         f state1 exp
      | PIf(e1,e2,e3,loc) ->
         let state1 = foldDownExp pred f state e1 in
         let state2 = foldDownExp pred f state1 e2 in
         let state3 = foldDownExp pred f state2 e3 in
         f state3 exp
      | PSeq(_,stmts,_) ->
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
      | StmtFun(name,args,stmts,type_exp,_,_) ->
         let state0 = pushScope state name in
         let state1 = foldDownExp pred f state0 stmts in
         f state1 exp |> popScope
      | StmtIf(cond,then_stmts,None,_) ->
         let state1 = foldDownExp pred f state cond in
         let state2 = foldDownExp pred f state1 then_stmts in
         f state2 exp
      | StmtIf(cond,then_stmts,Some(else_stmts),_) ->
         let state1 = foldDownExp pred f state cond in
         let state2 = foldDownExp pred f state1 then_stmts in
         let state3 = foldDownExp pred f state2 else_stmts in
         f state3 exp
      | StmtBlock(_,stmts,_) ->
         let state1 = foldDownExpList pred f state stmts in
         f state1 exp
      | StmtWhile(e1,e2,_) ->
         let state1 = foldDownExp pred f state e1 in
         let state2 = foldDownExp pred f state1 e2 in
         f state2 exp
      | StmtType(_,_,_,_) ->
         f state exp
      | StmtAliasType(_,_,_,_) ->
         f state exp


and foldDownExpList (pred:(exp -> bool) option) (f: ('data, exp) folder) (state:'data tstate) (expl:exp list) : 'data tstate =
   List.fold_left
      (fun state elem ->
          let state1 = foldDownExp pred f state elem in
          state1)
      state (List.rev expl)

and foldDownOptExp (pred:(exp -> bool) option) (f: ('data, exp) folder) (state:'data tstate) (exp:exp option) =
   match exp,pred with
   | Some(e),Some(pred_f) when not (pred_f e) -> state
   | Some(e),_ ->
      foldDownExp pred f state e
   | _ -> state

(** If the list of statements has more than one element return a PSeq instead *)
let makePSeq (loc:location) (l:exp list) : exp =
   match l with
   | [] -> PUnit(loc)
   | [h] -> h
   | _ -> PSeq(None,l,loc)

(** If the list of statements has more than one element return a StmtBlock instead *)
let makeStmtBlock (loc:location) (l:exp list) : exp =
   match l with
   | [] -> StmtBlock(None,[],loc)
   | [h] -> h
   | _ -> StmtBlock(None,l,loc)

(** Appends the contents of a list of blocks *)
let appendBlocksList (l:exp list) : exp list * location =
   let rec loop acc loc e =
      match e with
      | [] -> List.flatten (List.rev acc),loc
      | StmtBlock(_,stmts,sloc)::t ->
         let new_loc = mergeLocations loc sloc in
         loop (stmts::acc) new_loc t
      | h::t ->
         let new_loc = mergeLocations loc (getExpLocation h) in
         loop ([h]::acc) new_loc t
   in
   loop [] default_loc l

(** Appends the contents of a list of blocks and returns a StmtBlock *)
let appendBlocks (l:exp list) =
   let stmts,loc = appendBlocksList l in
   makeStmtBlock loc stmts

(** Appends the contents of a list of blocks and returns a PSeq *)
let appendPseq (l:exp list) =
   let stmts,loc = appendBlocksList l in
   makePSeq loc stmts

let expandBlockOrSeq (stmt:exp) : exp list =
   match stmt with
   | StmtBlock(_,stmts,_) -> stmts
   | PSeq(_,stmts,_)      -> stmts
   | _ -> [stmt]

let rec expandStmt (pred:(exp -> bool) option) (f: ('data, exp) expander) (state:'data tstate) (stmt:exp) : 'data tstate * exp list =
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
      | StmtFun(name,args,stmts,type_exp,active,loc) ->
         let state0 = pushScope state name in
         let state1,nstmts = expandStmt pred f state0 stmts in
         let state2,exp = f state1 (StmtFun(name,args,appendBlocks nstmts,type_exp,active,loc)) in
         (popScope state2),exp
      | StmtIf(cond,then_stmts,None,loc) ->
         let state1,nthen_stmts = expandStmt pred f state then_stmts in
         f state1 (StmtIf(cond,appendBlocks nthen_stmts,None,loc))
      | StmtIf(cond,then_stmts,Some(else_stmts),loc) ->
         let state1,nthen_stmts = expandStmt pred f state then_stmts in
         let state2,nelse_stmts = expandStmt pred f state1 else_stmts in
         f state2 (StmtIf(cond,appendBlocks nthen_stmts,Some(appendBlocks nelse_stmts),loc))
      | PSeq(name,el,loc) ->
         let state1,nel = expandStmtList pred f state el in
         f state1 (PSeq(name,nel,loc))

      | PUnit(_)
      | PInt(_,_)
      | PReal(_,_)
      | PEmpty
      | PBool(_,_)
      | PId(_) -> f state stmt
      | PTyped(e,type_exp,loc) ->
         let state1,ne = expandStmt pred f state e in
         f state1 (PTyped(appendPseq ne,type_exp,loc))
      | PUnOp(op,e,loc) ->
         let state1,ne = expandStmt pred f state e in
         f state1 (PUnOp(op,appendPseq ne,loc))
      | PBinOp(op,e1,e2,loc) ->
         let state1,ne1 = expandStmt pred f state e1 in
         let state2,ne2 = expandStmt pred f state1 e2 in
         f state2 (PBinOp(op,appendPseq ne1,appendPseq ne2,loc))
      | PCall(name,fname,args,loc,attr) ->
         let state1,nargs = expandStmtList pred f state args in
         f state1 (PCall(name,fname,nargs,loc,attr))
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
      | StmtBlock(name,el,loc) ->
         let state1,nel = expandStmtList pred f state el in
         f state1 (StmtBlock(name,nel,loc))
      | StmtWhile(e1,e2,loc) ->
         let state1,ne1 = expandStmt pred f state e1 in
         let state2,ne2 = expandStmt pred f state1 e2 in
         f state2 (StmtWhile(appendPseq ne1,appendBlocks ne2,loc))
      | StmtType(_,_,_,_) -> f state stmt
      | StmtAliasType(_,_,_,_) -> f state stmt

and expandStmtList (pred:(exp -> bool) option) (f: ('data, exp) expander) (state:'data tstate) (stmts:exp list) : 'data tstate * exp list =
   let state2,acc =
      List.fold_left
         (fun (state,acc) exp ->
             let state1,ne = expandStmt pred f state exp in
             (state1,ne::acc) )
         (state,[]) (List.rev stmts) in
   state2,acc |> List.flatten |> appendBlocksList |> fst

and expandOptStmt (pred:(exp -> bool) option) (f: ('data, exp) expander) (state:'data tstate) (stmt:exp option) : 'data tstate * exp option =
   match stmt with
   | None -> state,None
   | Some(e) ->
      let state1,ne = expandStmt pred f state e in
      state1,Some(appendPseq ne)

(** Takes a fold function and wrap it as it was a transformation so it can be chained with |+> *)
let foldAsTransformation (pred:(exp -> bool) option) (f:('data,exp) folder) (state:'data tstate) (exp_list:exp list) : 'data tstate * exp list =
   let new_state = foldTopExpList pred f state exp_list in
   new_state,exp_list

let getNameFromNamedId (named_id:named_id) : identifier =
   match named_id with
   | SimpleId(name,_)   -> name
   | NamedId (name,_,_) -> name

let getLocationFromNamedId (named_id:named_id) : location =
   match named_id with
   | SimpleId(_,loc) -> loc
   | NamedId (_,_,loc) -> loc

let getTypeFromNamedId (named_id:named_id) : exp option =
   match named_id with
   | NamedId (_,nametype,_) -> Some nametype
   | _ -> None

let getFunctionTypeAndName (names_id:named_id) : identifier * exp =
   match names_id with
   | NamedId(name,ftype,_) -> name,ftype
   | SimpleId(ftype,loc) -> ["_"],PId(ftype,None,loc)

(** Used by getIdsInExp and getIdsInExpList to get the ids in expressions *)
let getId : ('data,exp) folder =
   fun state exp ->
      match exp with
      | PId(name,_,_) -> setState state (name::state.data)
      | _ -> state

(** Return the ids in an expression *)
let getIdsInExp (exp:exp) : identifier list =
   foldTopExp None getId (createState []) exp
   |> getState

(** Used by getIdsInExp and getIdsInExpList to get the ids in expressions *)
let getIdExp : ('data,exp) folder =
   fun state exp ->
      match exp with
      | PId(_,_,_) -> setState state (exp::state.data)
      | _ -> state

(** Return the id expressions *)
let getIdAsExp (exp:exp) : exp list =
   foldTopExp None getIdExp (createState []) exp
   |> getState

(** Return the ids in an expression list *)
let getIdsInExpList (expl:exp list) : identifier list =
   foldTopExpList None getId (createState []) expl
   |> getState

module Identifier =
struct
   type t = identifier
   let compare = compare
end

module IdentifierMap = CCMap.Make(Identifier)


let mapfindDefault key map default =
   if IdentifierMap.mem key map then
      IdentifierMap.find key map
   else default

let mapfindOption key map =
   if IdentifierMap.mem key map then
      Some(IdentifierMap.find key map)
   else None

(** Compares two expressions ignoring the locations *)
let compareExp (a:exp) (b:exp) : int = compare_exp a b

(** Compares two expression lists ignoring the locations *)
and compareExpList (a:exp list) (b:exp list) : int = compare_exp_list a b

(** Returns the full location of an expression *)
let getExpFullLocation (e:exp) : location =
   let f state e =
      let current_loc = getExpLocation e in
      let new_state = mergeLocations state.data current_loc in
      setState state new_state
   in
   foldTopExp None f (createState default_loc) e
   |> getState

(** Removes the type from a named_id *)
let removeNamedIdType (name:named_id) : named_id =
   match name with
   | NamedId(n,_,loc) -> SimpleId(n,loc)
   | _ -> name

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
   | NamedId(n,t,_) -> (identifierStr n)^":fix_this"(*^(identifierStr t)*)
   | SimpleId(n,_) -> identifierStr n

(** Adds the give prefix to the named_id *)
let prefixNamedId (prefix:string) (name:named_id) : named_id =
   match name with
   | SimpleId(n,loc)   -> SimpleId((prefixId prefix n),loc)
   | NamedId(n,tp,loc) -> NamedId((prefixId prefix n),tp,loc)

(** Checks if the expression contains any node for which the function 'f' is true *)
let exists (f:exp -> bool) (exp:exp) : bool =
   foldTopExp None (fun state a -> setState state (state.data || f a)) (createState false) exp
   |> getState

(** Returns true if the name is a builtin type *)
let isBuiltinType (name:identifier) : bool =
   match name with
   | ["real"] | ["int"] | ["bool"] -> true
   | _ -> false

