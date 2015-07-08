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
   type t    = identifier
   type v    = binding_info
   type kind = scope_kind
   let compare     = compare_identifier
   let string_t    = identifierStr
   let string_v _  = ""
   let lookup_cond = (* Only scapes local scopes *)
      function | Some(LocalScope) -> true | _ -> false
end

module BindingsScope = Scope(BindingInfo)
(** Used to track the scope in all traversers *)
type 'a tstate =
   {
      scope   : BindingsScope.t;
      data    : 'a;
      revisit : bool;
   }

(** Type of all traversing functions *)
type ('data,'traversing_type) traverser = 'data tstate -> 'traversing_type -> 'data tstate * 'traversing_type

(** Type of all expanding functions *)
type ('data,'expanding_type) expander = 'data tstate -> 'expanding_type -> 'data tstate * 'expanding_type list

(** Type of folding functions *)
type ('data,'traversing_type) folder = 'data tstate -> 'traversing_type -> 'data tstate

(** Returns a traversing state *)
let createState (data:'a) : 'a tstate =
   { scope = BindingsScope.empty; data = data; revisit = false }

(** Sets the data for the traversing state *)
let setState (s:'a tstate) (data:'a) : 'a tstate =
   { s with data = data }

(** Gets the data for the traversing state *)
let getState (s:'a tstate) : 'a =
   s.data

(** Creates a new state keepin the internal data *)
let deriveState (s:'a tstate) (data:'b) : 'b tstate =
   { scope = s.scope; data = data; revisit = false }

(** Adds a name to the scope *)
let pushScope (s:'a tstate) (name:identifier) (kind:scope_kind) : 'a tstate =
   (*Printf.printf " - Entering to scope '%s'\n" (joinSep "." name);*)
   { s with scope = BindingsScope.enter s.scope name (Some(kind)) }

(** Removes the last name from the scope *)
let popScope (s:'a tstate) : 'a tstate =
   { s with scope = BindingsScope.exit s.scope }

(** Returns the current scope *)
let getScope (s:'a tstate) : identifier =
   BindingsScope.getCurrentPath s.scope |> List.flatten

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

(** Traverses the statements in a top-down fashion following the execution order *)
let rec traverseStmt (pred:(stmt->bool) option) (f: 'a tstate -> stmt -> 'a tstate * stmt) (state:'a tstate) (stmt:stmt) : 'a tstate * stmt =
   match pred with
   | Some(pred_f) when not (pred_f stmt) ->
      state,stmt
   | _ ->
      let state1,nstmt = f state stmt in
      match nstmt with
      | StmtVal(_,_,_)
      | StmtMem(_,_,_,_)
      | StmtTable(_,_,_)
      | StmtReturn(_,_)
      | StmtBind(_,_,_)
      | StmtType(_,_,_,_)
      | StmtAliasType(_,_,_,_)
      | StmtEmpty
         -> revisitStmt pred f state nstmt
      | StmtWhile(cond,stmts,attr) ->
         let state1,nstmts = traverseStmt pred f state stmts in
         revisitStmt pred f  state1 (StmtWhile(cond,nstmts,attr))
      | StmtIf(cond,then_,Some(else_),attr) ->
         let state1,nthen_ = traverseStmt pred f state then_ in
         let state2,nelse_ = traverseStmt pred f state1 else_ in
         revisitStmt pred f state2 (StmtIf(cond,nthen_,Some(nelse_),attr))
      | StmtIf(cond,then_,None,attr) ->
         let state1,nthen_ = traverseStmt pred f state then_ in
         revisitStmt pred f  state1 (StmtIf(cond,nthen_,None,attr))
      | StmtFun(name,args,body,ret,attr) ->
         let state1,nbody = traverseStmt pred f state body in
         revisitStmt pred f state1 (StmtFun(name,args,nbody,ret,attr))
      | StmtBlock(name,stmts,attr) ->
         let state1,nstmts = traverseStmtList pred f state stmts in
         revisitStmt pred f state (StmtBlock(name,nstmts,attr))

and traverseStmtList (pred:(stmt->bool) option) (f: 'a tstate -> stmt -> 'a tstate * stmt) (state:'a tstate) (stmts:stmt list) : 'a tstate * stmt list =
   let ns,acc = List.fold_left (fun (s,acc) a -> let ns,na = traverseStmt pred f state a in ns,a::acc) (state,[]) stmts in
   ns,List.rev acc

and revisitStmt (pred:(stmt->bool) option) (f: 'a tstate -> stmt -> 'a tstate * stmt) (state:'a tstate) (stmt:stmt) : 'a tstate * stmt =
   if state.revisit then
      traverseStmt pred f ({ state with revisit = false }) stmt
   else
      state,stmt

(** Traverses the expression in a bottom-up fashion *)
let rec traverseExp (pred:(exp->bool) option) (f: 'a tstate -> exp -> 'a tstate * exp) (state:'a tstate) (exp:exp) : 'a tstate * exp =
   match pred with
   | Some(pred_f) when not (pred_f exp) ->
      state,exp
   | _ ->
      match exp with
      | PUnit(_)
      | PBool(_,_)
      | PInt(_,_)
      | PReal(_,_)
      | PId(_,_)
      | PSeq(_,_,_)
      | PEmpty -> f state exp
      | PUnOp(op,e,loc) ->
         let state1,ne = traverseExp pred f state e in
         revisitExp pred f (f state1 (PUnOp(op,ne,loc)))
      | PBinOp(op,e1,e2,loc) ->
         let state1,ne1 = traverseExp pred f state e1 in
         let state2,ne2 = traverseExp pred f state1 e2 in
         revisitExp pred f (f state2 (PBinOp(op,ne1,ne2,loc)))
      | PCall(inst,name,args,attr) ->
         let state1,nargs = traverseExpList pred f state args in
         revisitExp pred f (f state1 (PCall(inst,name,nargs,attr)))
      | PIf(cond,then_,else_,attr) ->
         let state1,ncond  = traverseExp pred f state cond in
         let state2,nthen_ = traverseExp pred f state1 then_ in
         let state3,nelse_ = traverseExp pred f state2 else_ in
         revisitExp pred f (f state3 (PIf(ncond,nthen_,nelse_,attr)))
      | PGroup(e,attr) ->
         let state1,ne = traverseExp pred f state e in
         revisitExp pred f (f state1 (PGroup(ne,attr)))
      | PTuple(el,attr) ->
         let state1,nel = traverseExpList pred f state el in
         revisitExp pred f (f state1 (PTuple(nel,attr)))

and traverseExpList (pred:(exp->bool) option) (f: 'a tstate -> exp -> 'a tstate * exp) (state:'a tstate) (exps:exp list) : 'a tstate * exp list =
   let ns,acc = List.fold_left (fun (s,acc) a -> let ns,na = traverseExp pred f state a in ns,a::acc) (state,[]) exps in
   ns,List.rev acc

and revisitExp (pred:(exp->bool) option) (f: 'a tstate -> exp -> 'a tstate * exp) ((state,exp):'a tstate * exp) : 'a tstate * exp =
   if state.revisit then
      traverseExp pred f ({ state with revisit = false }) exp
   else
      state,exp

(** Returns the full location (start and end) of an expression *)
let rec getFullExpLocation (e:exp) : Loc.t =
   match e with
   | PUnit(attr)
   | PInt(_,attr)
   | PBool(_,attr)
   | PReal(_,attr)
   | PId(_,attr) -> attr.loc
   | PUnOp(_,e1,attr) -> Loc.merge attr.loc (getFullExpLocation e1)
   | PBinOp(_,e1,e2,attr) ->
      let loc1 = getFullExpLocation e1 in
      let loc2 = getFullExpLocation e2 in
      Loc.merge attr.loc (Loc.merge loc1 loc2)
   | PCall(_,_,args,attr) ->
      getFullExpListLocation attr.loc args
   | PIf(e1,e2,e3,attr) ->
      let loc1 = getFullExpLocation e1 in
      let loc2 = getFullExpLocation e2 in
      let loc3 = getFullExpLocation e3 in
      Loc.merge attr.loc (Loc.merge loc1 (Loc.merge loc2 loc3))
   | PGroup(e1,attr) ->
      let loc1 = getFullExpLocation e1 in
      Loc.merge attr.loc loc1
   | PTuple(el,attr) ->
      getFullExpListLocation attr.loc el
   | PSeq(_,_,attr) -> attr.loc
   | PEmpty -> Loc.default

and getFullExpListLocation (loc:Loc.t) (el:exp list) : Loc.t =
   List.fold_left (fun s a -> Loc.merge s (getFullExpLocation a)) loc el

let rec getFullTypeLocation (exp:type_exp) : Loc.t =
   match exp with
   | TUnit(attr) -> attr.loc
   | TWild(attr) -> attr.loc
   | TId(_,attr) -> attr.loc
   | TTuple(el,attr) ->
      getFullTypeListLocation attr.loc el
   | TComposed(_,el,attr) ->
      getFullTypeListLocation attr.loc el
   | TSignature(el,attr) ->
      getFullTypeListLocation attr.loc el

and getFullTypeListLocation (loc:Loc.t) (el:type_exp list) : Loc.t =
   List.fold_left (fun s a -> Loc.merge s (getFullTypeLocation a)) loc el

