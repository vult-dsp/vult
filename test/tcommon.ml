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

(*open Util.Args*)

let initial_dir = Sys.getcwd ()

let tmp_dir = Filename.get_temp_dir_name ()

let call_uname () =
  let ic = Unix.open_process_in "uname" in
  let uname = input_line ic in
  let () = close_in ic in
  uname


let os : string =
  match Sys.os_type with
  | "Win32"
   |"Cygwin" ->
      "Windows"
  | "Unix" ->
      begin
        match call_uname () with
        | "Linux" -> "Linux"
        | "Darwin" -> "OSX"
        | _ -> failwith "cannot get os"
        | exception _ -> failwith "cannot get os"
      end
  | _ -> failwith "cannot get os"


let tryToRun cmd =
  Sys.chdir tmp_dir ;
  let result =
    match Sys.command cmd with
    | 0 -> true
    | _ -> false
    | exception _ -> false
  in
  Sys.chdir initial_dir ;
  result

(*let getFile (args : args) (ext : FileKind.t) : string =
  match ext with
  | FileKind.ExtOnly e -> args.output ^ "." ^ e
  | FileKind.FullName n -> Filename.concat (Filename.dirname args.output) n*)

(*let writeFiles args files =
  List.iter (fun (text, file) -> Util.FileIO.write (getFile args file) (Pla.print text) |> ignore) files
*)
