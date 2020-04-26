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

type tag_d =
  | TagId     of string
  | TagInt    of int
  | TagBool   of bool
  | TagReal   of float
  | TagString of string
  | TagCall   of
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
            {pla|<#n#s> = <#tag#>|pla})
          args
      in
      {pla|<#name#s>(<#args#>)|pla}


let print_tags tags =
  match tags with
  | [] -> Pla.unit
  | _ ->
      let tags = Pla.map_sep Pla.comma print_tag tags in
      {pla|@[<#tags#>]|pla}


let has (tags : tag list) n =
  List.exists
    (fun t ->
      match t.g with
      | TagCall { name; _ } -> name = n
      | TagId name -> name = n
      | _ -> false)
    tags
