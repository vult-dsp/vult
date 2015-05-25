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
open ParserTypes
open Either
open Scope
open Location

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


