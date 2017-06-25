(*
The MIT License (MIT)

Copyright (c) 2017 Leonardo Laguna Ruiz

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

type t = string list
[@@deriving show,eq,ord]

(** This path is used to differentiate simple identifiers from full paths *)
type path =
   | Path of t

(** Adds the given string at the end of the identifier *)
let postfix (id:t) (post:string) : t =
   match id with
   | [name] -> [name ^ post]
   | [pkg; name] -> [pkg; name ^ post]
   | _ -> failwith "invalid id"

(** Takes two identifier and makes one (with a single name) seperated by the given character. *)
let joinSep (sep:string) (fname:t) (var:t) : t =
   [String.concat sep (fname @ var)]

let join (id1:t) (id2:t) : t =
   id1 @ id2

(** Tries to return the name without the module  *)
let getNameNoModule (id:t) : string option =
   match id with
   | _ :: name :: _ -> Some name
   | _ -> None


(* ===== PATHS ======= *)

(** Tries to return the module name of a path *)
let getPathModule (path:path) : string option =
   match path with
   | Path([]) -> None
   | Path(module_ :: _ ) -> Some module_

(** Returns the identifier of a path *)
let pathToId (path:path) : t =
   let Path(id) = path in
   id

(** Joins an identifier to a path *)
let pathJoin (path:path) (id:t) : path =
   let Path(p) = path in
   Path(p@id)
