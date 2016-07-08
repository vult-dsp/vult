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

(** Represents the different types of errors *)

type t =
   | PointedError of Loc.t * string
   | SimpleError  of string

exception Errors of t list

(** Takes a location and returns a string in the format "Error in file: file: line l col:c1-c2" *)
let errorLocationMessage (location:Loc.t) : string =
   let col_start = Loc.startColumn location in
   let col_end   = Loc.endColumn location in
   Printf.sprintf "Error in file: %s: line %i col:%i-%i\n"
      (Loc.file location)
      (Loc.line location)
      col_start
      col_end

(** Takes the current line and a location returns a string pointing to the
    location *)
let errorLocationIndicator (line:string) (location:Loc.t) : string =
   let col_start = max (Loc.startColumn location) 0 in
   let col_end   = max (Loc.endColumn location) 0 in
   let pointer   = if (col_end - col_start) <> 0 then (String.make (col_end - col_start) '^') else "^" in
   Printf.sprintf "%s\n%s%s\n"
      line
      (String.make col_start ' ')
      pointer

(** Reads all the lines of a file *)
let readFile (name:string) : string =
   let buffer = Buffer.create 128 in
   let i = open_in name in
   try
      while true do
         Buffer.add_char buffer (input_char i)
      done; ""
   with
   | End_of_file ->
      close_in i;
      Buffer.contents buffer

(** Returns the lines corresponding to the given location *)
let getErrorLines (location:Loc.t) =
   let lines =
      match location.Loc.source with
      | Loc.File(filename) -> CCString.lines (readFile filename)
      | Loc.Text(code) -> CCString.lines code
   in
   match Loc.line location with
   | 0 | 1 -> List.nth lines 0
   | n -> (List.nth lines (n-2))^"\n"^(List.nth lines (n-1))

(** Takes an error and the lines of the code and returns an error message *)
let reportErrorString (error:t) : string =
   match error with
   | SimpleError(msg) -> msg^"\n"
   | PointedError(location,msg) ->
      let loc = errorLocationMessage location in
      let line = getErrorLines location in
      let indicator = errorLocationIndicator line location in
      loc^msg^"\n"^indicator

let reportErrors (errors: t list) : string =
   List.map reportErrorString errors
   |> List.fold_left (fun s a -> s^"\n"^a) ""

(** Returns a tuple with the error an all its information *)
let reportErrorStringNoLoc (error:t) : string * string * int * int =
   match error with
   | PointedError(location,msg) ->
      let col_start = Loc.startColumn location in
      let line      = getErrorLines location in
      let indicator = errorLocationIndicator line location in
      let full_msg = msg^"\n"^indicator in
      full_msg, (Loc.file location), (Loc.line location), col_start
   | SimpleError(msg) ->
      msg, "-", 0, 0

(** Joins two errors *)
let joinErrors : t list -> t list -> t list = List.append

(** Joins two optional errors *)
let joinErrorOptions : t list option -> t list option -> t list option =
   fun maybeErr1 maybeErr2 ->
      match (maybeErr1,maybeErr2) with
      | (Some _ as ret, None) -> ret
      | (None, (Some _ as ret)) -> ret
      | (Some err1, Some err2) -> Some (joinErrors err1 err2)
      | (None, None) -> None

(** Joins a list of optional errors *)
let joinErrorOptionsList : t list option list -> t list option = List.fold_left joinErrorOptions None

let makeError (msg:string) (loc:Loc.t) =
   PointedError(loc,msg)

let raiseError (msg:string) (loc:Loc.t) =
   raise (Errors([makeError msg loc]))
