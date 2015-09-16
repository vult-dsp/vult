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

(** Converts an indentifier in a string by separating the names with dot *)
let identifierStr (id:identifier) : string = joinSep "." id

(** Converts a list of identifiers into a coma separated string *)
let identifierStrList (ids:identifier list) : string =
   List.map identifierStr ids |> joinSep ", "

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





