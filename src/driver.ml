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

open TypesVult

let writeFile (code:string) (file:string) : unit =
   let oc = open_out file in
   Printf.fprintf oc "%s\n" code;
   close_out oc

let writeOutput (args:arguments) (files:(Pla.t * string) list) : string =
   let write_files = args.output<>"" in
   let txt = List.fold_left
         (fun s (code_t,ext) ->
             let code = Pla.print code_t in
             let () = if write_files then writeFile code (args.output^"."^ext) in
             s^"\n"^code)
         "" files
   in
   if not write_files then print_endline txt;
   txt

let generateCode (args:arguments) (parser_results:parser_results list) : string =
   try
      if args.ccode then
         let files = VultCh.generateChCode args parser_results in
         writeOutput args files
      else
      if args.jscode then
         let files = VultJs.generateJSCode args parser_results in
         writeOutput args files
      else ""
   with
   | Error.Errors(errors) ->
      let error_strings = Error.reportErrors errors in
      print_endline ("Errors in the program:\n"^error_strings);
      exit (-1)

(** Prints the parsed files if -dparse was passed as argument *)
let dumpParsedFiles (args:arguments) (parser_results:parser_results list) : unit =
   try
      if args.dparse then
         parser_results
         |> List.iter (fun a -> PrintTypes.stmtListStr a.presult |> print_string)
   with
   | Error.Errors(errors) ->
      let error_strings = Error.reportErrors errors in
      print_endline ("Errors in the program:\n"^error_strings)

(** Parses the code and and generates the target *)
let parseStringGenerateCode (args:arguments) (code:string) : string =
   try
      ParserVult.parseString code
      |> fun a -> generateCode args [a]
   with
   | Error.Errors(errors) ->
      let error_strings = Error.reportErrors errors in
      "Errors in the program:\n"^error_strings

(** Parses the code and returns either the transformed code or the error message *)
let parsePrintCode (code:string) : string =
   try
      ParserVult.parseString code
      |> Passes.applyTransformationsSingle default_arguments
      |> PrintTypes.stmtListStr
   with
   | Error.Errors(errors) ->
      let error_strings = Error.reportErrors errors in
      "Errors in the program:\n"^error_strings

(** Checks the code and returns a list with the errors *)
let checkCode (code:string) : (string * string * int * int) list =
   try
      ParserVult.parseString code
      |> Passes.applyTransformationsSingle default_arguments
      |> fun _ -> []
   with
   | Error.Errors(errors) ->
      List.map Error.reportErrorStringNoLoc errors

