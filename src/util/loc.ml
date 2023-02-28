(*
   The MIT License (MIT)

   Copyright (c) 2020 Leonardo Laguna Ruiz, Carl Jönsson

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

(** Type to hold the location *)
open Lexing

(** Source of the code *)
type source =
  | File of string
  | Text of string

(** Location information *)
type t =
  { start_pos : position
  ; end_pos : position
  ; source : source
  }

let pp fmt _ = Format.pp_print_string fmt "loc"
let equal _ _ = true
let compare _ _ = 0
let default = { start_pos = dummy_pos; end_pos = dummy_pos; source = Text "" }

(** [isSamePos p1 p2] returns true if the positions are equal *)
let isSamePos (p1 : position) (p2 : position) : bool =
  p1.pos_lnum = p2.pos_lnum && p1.pos_bol = p2.pos_bol && p1.pos_cnum = p2.pos_cnum
;;

(** [isSameLoc loc1 loc2] returns true if the locations are equal *)
let isSameLoc (loc1 : t) (loc2 : t) : bool =
  isSamePos loc1.start_pos loc2.start_pos && isSamePos loc1.end_pos loc2.end_pos
;;

(** Returns the current location (start and end) *)
let getLocation (source : source) lexbuf : t = { start_pos = lexbuf.lex_start_p; end_pos = lexbuf.lex_curr_p; source }

(** Returns the start column *)
let startColumn (location : t) : int = location.start_pos.pos_cnum - location.start_pos.pos_bol

(** Returns the end column *)
let endColumn (location : t) : int = location.end_pos.pos_cnum - location.start_pos.pos_bol

(** Returns the filename *)
let file (location : t) : string = location.start_pos.pos_fname

let line (location : t) : int = location.start_pos.pos_lnum

let compareLoc (loc1 : t) (loc2 : t) =
  let o = compare (line loc1) (line loc2) in
  if o = 0 then compare (startColumn loc1) (startColumn loc2) else o
;;

(** Returns a simple string representation of the location *)
let to_string (location : t) : string =
  Printf.sprintf
    "%s:%i:%i-%i"
    location.start_pos.pos_fname
    location.start_pos.pos_lnum
    (location.start_pos.pos_cnum - location.start_pos.pos_bol)
    (location.end_pos.pos_cnum - location.start_pos.pos_bol)
;;

(** Returns a readable string representation of the location *)
let to_string_readable (location : t) : string =
  Printf.sprintf
    "line %i, columns %i-%i"
    location.start_pos.pos_lnum
    (location.start_pos.pos_cnum - location.start_pos.pos_bol)
    (location.end_pos.pos_cnum - location.start_pos.pos_bol)
;;

(** Returns the location that follows the given location *)
let getNext (loc : t) : t =
  let end_pos = { loc.end_pos with Lexing.pos_cnum = loc.end_pos.Lexing.pos_cnum } in
  { start_pos = end_pos; end_pos; source = loc.source }
;;

(** Returns the minimal position of two given *)
let getMinPosition (pos1 : Lexing.position) (pos2 : Lexing.position) : Lexing.position =
  if pos1.Lexing.pos_lnum <> pos2.Lexing.pos_lnum
  then if pos1.Lexing.pos_lnum < pos2.Lexing.pos_lnum then pos1 else pos2
  else if pos1.Lexing.pos_cnum < pos2.Lexing.pos_cnum
  then pos1
  else pos2
;;

(** Returns the maximum position of two given *)
let getMaxPosition (pos1 : Lexing.position) (pos2 : Lexing.position) : Lexing.position =
  if pos1.Lexing.pos_lnum <> pos2.Lexing.pos_lnum
  then if pos1.Lexing.pos_lnum < pos2.Lexing.pos_lnum then pos2 else pos1
  else if pos1.Lexing.pos_cnum < pos2.Lexing.pos_cnum
  then pos2
  else pos1
;;

(** Retuns the minimum and maximum prositions from a given list *)
let getMinMaxPositions (pos_list : Lexing.position list) =
  match pos_list with
  | [] -> failwith "getMinMaxPositions: No positions passed"
  | [ h ] -> h, h
  | h :: _ -> List.fold_left (fun (min, max) a -> getMinPosition a min, getMaxPosition a max) (h, h) pos_list
;;

(** Returns a new location with the start and end positions updated *)
let merge (loc1 : t) (loc2 : t) : t =
  if loc1 = default
  then loc2
  else if loc2 = default
  then loc1
  else (
    let start_pos, end_pos = getMinMaxPositions [ loc1.start_pos; loc2.start_pos; loc1.end_pos; loc2.end_pos ] in
    { start_pos; end_pos; source = loc1.source })
;;

let merge3 (loc1 : t) (loc2 : t) (loc3 : t) : t = merge (merge loc1 loc2) loc3

let mergeList default loc =
  match loc with
  | [] -> default
  | h :: t -> List.fold_left merge h t
;;

let hashString (t : t) =
  Printf.sprintf
    "%.2x%.2x%.2x"
    (0xFF land Hashtbl.hash t.source)
    (0xFF land Hashtbl.hash t.start_pos)
    (0xFF land Hashtbl.hash t.end_pos)
;;
