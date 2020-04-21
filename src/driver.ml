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

(** Contains top level functions to perform common tasks *)

open Args
open Parser

let stage2 (args : args) (parsed : parsed_file list) : output list =
  let _, typed = Inference.infer parsed in
  [ ParsedCode (Pla.print (Typed.PX.print_prog typed)) ]


(*
   let generateCode (args : args) (parsed : parsed_file list) : output list =
   match Generate.generateCode parsed args with
   | [] -> []
   | results -> [ GeneratedCode results ]
*)

(** Prints the parsed files if -dparse was passed as argument *)
let dumpParsedFiles (args : args) (parsed : parsed_file list) : output list =
  if args.dparse then
    parsed |> List.map (fun a -> Syntax.show_stmts a.stmts) |> String.concat "\n" |> fun a -> [ ParsedCode a ]
  else
    []


(*

(** Prints the parsed files if -dparse was passed as argument *)
let runFiles (args : args) (parsed : parsed list) : output list =
  let print_val e =
    match e with
    | PUnit _ -> []
    | _ -> [ PrintProg.expressionStr e ]
  in
  if args.eval then
    Passes.applyTransformations args ~options:PassCommon.interpreter_options parsed
    |> fst
    |> Interpreter.eval args
    |> List.map print_val
    |> List.flatten
    |> fun a -> [ Interpret (String.concat "\n" a) ]
  else
    []


(** Checks the code and returns a list with the errors *)
let checkCode (args : args) (parsed : parsed list) : output list =
  if args.check then
    try parsed |> Passes.applyTransformations args |> fun _ -> [ CheckOk ] with
    | Error.Errors errors -> [ Errors errors ]
  else
    []

*)

let version = String.sub Version.version 1 (String.length Version.version - 2)

let main (args : args) : output list =
  try
    if args.show_version then
      [ Version version ]
    else (* Parse the files *)
      match args.files with
      | [] -> [ Message ("vult " ^ version ^ " - https://github.com/modlfo/vult\nno input files") ]
      | _ ->
          let parsed = Loader.loadFiles args args.files in
          if args.deps then
            List.map (fun r -> r.file) parsed |> fun s -> [ Dependencies s ]
          else
            dumpParsedFiles args parsed @ stage2 args parsed
    (*      @ runFiles args parsed
            @ checkCode args parsed*)
  with
  | Error.Errors errors -> [ Errors errors ]
