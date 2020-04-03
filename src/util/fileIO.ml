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

(** This file provides a hackish way of replacing the file IO functions for Node.js
    implementations that in order to run Vult with node *)

(** ref holding the current working directory.
    The default function is the native.
*)
let cwd_fun = ref (fun () -> Sys.getcwd ())

(** ref holding the read file function.
    The default function is the native.
*)
let read_fun =
  ref (fun path ->
      if Sys.file_exists path then (
        let file = open_in path in
        let buffer = Buffer.create 16 in
        try
          while true do
            let c = input_char file in
            Buffer.add_char buffer c
          done ;
          None
        with
        | _ ->
            close_in file ;
            Some (Buffer.contents buffer) )
      else
        None)


(** ref holding the read_bytes file function.
    The default function is the native.
*)
let read_bytes_fun =
  ref (fun path ->
      if Sys.file_exists path then (
        let file = open_in path in
        let buffer = Buffer.create 16 in
        try
          while true do
            let c = input_char file in
            Buffer.add_char buffer c
          done ;
          None
        with
        | End_of_file ->
            close_in file ;
            Some buffer )
      else
        None)


(** ref holding the read file function.
    The default function is the native.
*)
let write_fun =
  ref (fun path text ->
      try
        let file = open_out path in
        output_string file text ;
        close_out file ;
        true
      with
      | _ -> false)


(** ref holding the file exists.
    The default function is the native.
*)
let exists_fun = ref (fun path -> Sys.file_exists path)

(** This function is used to change the actual read_bytes function.
    When compiling for node.js the function is replaced at runtime by the code
*)
let setReadBytes f = read_bytes_fun := f

(** This function is used to change the actual read file function.
    When compiling for node.js the function is replaced at runtime by the code
*)
let setRead f = read_fun := f

(** This function is used to change the actual write to file function.
    When compiling for node.js the function is replaced at runtime by the code
*)
let setWrite (f : string -> string -> bool) = write_fun := f

(** This function is used to change the actual file-exists function.
    When compiling for node.js the function is replaced at runtime by the code
*)
let setExists (f : string -> bool) = exists_fun := f

(** This function is used to change the actual cwd function.
    When compiling for node.js the function is replaced at runtime by the code
*)
let setCwd (f : unit -> string) = cwd_fun := f

(** Main function to read file bytes given the path *)
let read_bytes (path : string) : Buffer.t option = !read_bytes_fun path

(** Main function to read files given the path *)
let read (path : string) : string option = !read_fun path

(** Main function to write files given the path and the text *)
let write (path : string) (text : string) : bool = !write_fun path text

(** Main function to check if a file exists *)
let exists (path : string) : bool = !exists_fun path

(* Gets the current directory *)
let cwd () : string = !cwd_fun ()

let findFile (includes : string list) (filename : string) : string option =
  if exists filename then
    Some filename
  else
    let rec loop inc =
      match inc with
      | [] -> None
      | h :: t ->
          let fullname = Filename.concat h filename in
          if exists fullname then
            Some fullname
          else
            loop t
    in
    loop includes


let writeIfDifferent (path : string) (text : string) : bool =
  if exists path then
    match read path with
    | Some current when compare current text = 0 -> true
    | _ -> write path text
  else
    write path text
