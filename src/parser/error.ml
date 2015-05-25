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

type t =
   | PointedError of Location.t * string
   | SimpleError  of string

(** Takes a location and returns a string in the format "Error in file: file: line l col:c1-c2" *)
let errorLocationMessage (location:Location.t) : string =
   let col_start = Location.startColumn location in
   let col_end   = Location.endColumn location in
   Printf.sprintf "Error in file: %s: line %i col:%i-%i\n"
      (Location.file location)
      (Location.line location)
      col_start
      col_end

(** Takes the current line and a location returns a string pointing to the
    location *)
let errorLocationIndicator (line:string) (location:Location.t) : string =
   let col_start = Location.startColumn location in
   let col_end   = Location.endColumn location in
   let pointer   = if (col_end - col_start) <> 0 then (String.make (col_end - col_start) '^') else "^" in
   Printf.sprintf "%s\n%s%s\n"
      line
      (String.make col_start ' ')
      pointer

(** Returns the lines corresponding to the given location *)
let getErrorLines (location:Location.t) (lines:string array) =
   match Location.line location with
   | 0 | 1 -> Array.get lines 0
   | n -> (Array.get lines (n-2))^"\n"^(Array.get lines (n-1))

(** Takes an error and the lines of the code and returns an error message *)
let reportErrorString (lines:string array) (error:t) :string =
   match error with
   | SimpleError(msg) -> msg^"\n"
   | PointedError(location,msg) ->
      let loc = errorLocationMessage location in
      let line = getErrorLines location lines in
      let indicator = errorLocationIndicator line location in
      loc^msg^"\n"^indicator

(** Takes an Either value and returns the errors *)
let reportErrors (results:('a,t list) CCError.t) (lines:string array) : string list =
   match results with
   | `Ok(_) -> []
   | `Error(errors) ->
      List.map (reportErrorString lines) errors

(** Takes an Either value containing a list of errors and prints the errors *)
let printErrors (results:('a,t list) CCError.t) (lines:string array) =
   let errors = reportErrors results lines in
   List.iter (fun (a:string) -> print_endline a) errors

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
