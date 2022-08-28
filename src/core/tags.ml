(*
   The MIT License (MIT)

   Copyright (c) 2014 Leonardo Laguna Ruiz

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

open Prog

type t =
   | Bool
   | Int
   | Real
   | Id
   | String

let rec has (tags : tag list) (id : Id.t) =
   match tags with
   | [] -> false
   | TId (name, _) :: _ when id = name -> true
   | TFun (name, _, _) :: _ when id = name -> true
   | _ :: t -> has t id
;;

let is_empty (tags : tag list) = tags = []

let getLocation (attr : tag) : Loc.t =
   match attr with
   | TBool (_, loc) -> loc
   | TInt (_, loc) -> loc
   | TId (_, loc) -> loc
   | TReal (_, loc) -> loc
   | TFun (_, _, loc) -> loc
   | TString (_, loc) -> loc
;;

let getType (attr : tag) : string =
   match attr with
   | TBool (_, _) -> "integer"
   | TInt (_, _) -> "integer"
   | TId (_, _) -> "identifier"
   | TReal (_, _) -> "real"
   | TFun (_, _, _) -> "tag"
   | TString (_, _) -> "string"
;;

let getTypeLiteral (t : t) : string =
   match t with
   | Bool -> "bool"
   | Int -> "integer"
   | Real -> "real"
   | Id -> "identifier"
   | String -> "string"
;;

let rec getParam (remaining : (Id.t * tag) list) (args : (Id.t * tag) list) (id : string) =
   match args with
   | [] -> remaining, None
   | (name, value) :: t when name = [ id ] -> remaining @ t, Some value
   | h :: t -> getParam (h :: remaining) t id
;;

let getTypedParam (args : (Id.t * tag) list) (id, typ) =
   let lattr loc = { emptyAttr with loc } in
   match getParam [] args id with
   | r, Some (TReal (value, loc)) when typ = Real -> r, Some (PReal (float_of_string value, Float, lattr loc))
   | r, Some (TInt (value, loc)) when typ = Int -> r, Some (PInt (int_of_string value, lattr loc))
   | r, Some (TBool (value, loc)) when typ = Bool ->
      let v = value = "true" in
      r, Some (PBool (v, lattr loc))
   | r, Some (TId (value, loc)) when typ = Id -> r, Some (PId (value, lattr loc))
   | r, Some (TString (value, loc)) when typ = String -> r, Some (PString (value, lattr loc))
   | _, Some value ->
      let loc = getLocation value in
      let msg =
         Printf.sprintf
            "The parameter '%s' was expected to be of type '%s' but it is '%s'"
            id
            (getTypeLiteral typ)
            (getType value)
      in
      Error.raiseError msg loc
   | r, None -> r, None
;;

let getParameterList (args : (Id.t * tag) list) (params : (string * t) list) : (Id.t * tag) list * exp option list =
   let rec loop remaning found params =
      match params with
      | [] -> remaning, List.rev found
      | h :: t ->
         let remaining, value = getTypedParam remaning h in
         loop remaining (value :: found) t
   in
   loop args [] params
;;

let getTableIndividualParams (loc : Loc.t) params args =
   let remaining, found = getParameterList args params in
   match remaining with
   | _ :: _ ->
      let params_s = List.map (fun (id, _) -> PrintProg.identifierStr id) remaining |> String.concat ", " in
      let msg = "The following arguments are unknown for the current tag: " ^ params_s in
      Error.raiseError msg loc
   | [] -> found
;;

let rec getTableParams (fname : string) (params : (string * t) list) (msg : string) (attr : tag list) =
   match attr with
   | [] -> None
   | TFun (name, args, loc) :: _ when name = [ fname ] -> Some (loc, getTableIndividualParams loc params args)
   | _ :: t -> getTableParams fname params msg t
;;

let rec removeAttrFunc (fname : string) (attr : tag list) : tag list =
   match attr with
   | [] -> []
   | TFun (name, _, _) :: t when name = [ fname ] -> removeAttrFunc fname t
   | h :: t -> h :: removeAttrFunc fname t
;;
