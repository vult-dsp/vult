(*
   The MIT License (MIT)

   Copyright (c) 2020 Leonardo Laguna Ruiz

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
open Util

type tag_type =
  | TypeInt
  | TypeBool
  | TypeReal
  | TypeString
  | TypeId

type value =
  | Int of int
  | Bool of bool
  | Real of float
  | String of string
  | Id of string

type tag_d =
  | TagId of string
  | TagInt of int
  | TagBool of bool
  | TagReal of float
  | TagString of string
  | TagCall of
      { name : string
      ; args : (string * tag * Loc.t) list
      }
[@@deriving show, eq, ord]

and tag =
  { g : tag_d
  ; loc : Loc.t
  }

let rec print_tag t : Pla.t =
  match t.g with
  | TagId id -> Pla.string id
  | TagInt i -> Pla.int i
  | TagBool b -> Pla.string (if b then "true" else "false")
  | TagReal f -> Pla.float f
  | TagString s -> Pla.string_quoted s
  | TagCall { name; args } ->
    let args =
      Pla.map_sep
        Pla.commaspace
        (fun (n, tag, _) ->
          let tag = print_tag tag in
          [%pla {|<#n#s> = <#tag#>|}])
        args
    in
    [%pla {|<#name#s>(<#args#>)|}]
;;

let print_tags tags =
  match tags with
  | [] -> Pla.unit
  | _ ->
    let tags = Pla.map_sep Pla.commaspace print_tag tags in
    [%pla {|@[<#tags#>]|}]
;;

let has (tags : tag list) n =
  List.exists
    (fun t ->
      match t.g with
      | TagCall { name; _ } -> name = n
      | TagId name -> name = n
      | _ -> false)
    tags
;;

let getType (tag : tag) : string =
  match tag.g with
  | TagBool _ -> "integer"
  | TagInt _ -> "integer"
  | TagId _ -> "identifier"
  | TagReal _ -> "real"
  | TagCall _ -> "tag"
  | TagString _ -> "string"
;;

let getTypeLiteral t : string =
  match t with
  | TypeBool -> "bool"
  | TypeInt -> "integer"
  | TypeReal -> "real"
  | TypeId -> "identifier"
  | TypeString -> "string"
;;

let rec getParam (remaining : (string * tag * Loc.t) list) (args : (string * tag * Loc.t) list) (id : string) =
  match args with
  | [] -> remaining, None
  | (name, value, _) :: t when name = id -> remaining @ t, Some value
  | h :: t -> getParam (h :: remaining) t id
;;

let getTypedParam (args : (string * tag * Loc.t) list) (id, typ) =
  match getParam [] args id with
  | r, Some { g = TagReal value; _ } when typ = TypeReal -> r, Some (Real value)
  | r, Some { g = TagInt value; _ } when typ = TypeInt -> r, Some (Int value)
  | r, Some { g = TagBool value; _ } when typ = TypeBool -> r, Some (Bool value)
  | r, Some { g = TagId value; _ } when typ = TypeId -> r, Some (Id value)
  | r, Some { g = TagString value; _ } when typ = TypeString -> r, Some (String value)
  | _, Some tag ->
    let msg =
      Printf.sprintf
        "The parameter '%s' was expected to be of type '%s' but it is '%s'"
        id
        (getTypeLiteral typ)
        (getType tag)
    in
    Error.raiseError msg tag.loc
  | r, None -> r, None
;;

let getArguments tags n =
  CCList.find_map
    (fun t ->
      match t.g with
      | TagCall { name; args } when name = n -> Some args
      | TagId name when name = n -> Some []
      | _ -> None)
    tags
;;

let getParameterList tags name (params : (string * tag_type) list) : value option list =
  let rec loop remaning found params =
    match params with
    | [] -> List.rev found
    | h :: t ->
      let remaining, value = getTypedParam remaning h in
      loop remaining (value :: found) t
  in
  match getArguments tags name with
  | Some args -> loop args [] params
  | None -> []
;;

let getStringValueOr ~default v =
  match v with
  | Some (String v) -> v
  | _ -> default
;;

let getBoolValueOr ~default v =
  match v with
  | Some (Bool v) -> v
  | _ -> default
;;
