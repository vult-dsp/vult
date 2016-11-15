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


let writeOutput (args:arguments) (files:(Pla.t * string) list) : string =
   let write_files = args.output<>"" in
   let txt = List.fold_left
         (fun s (code_t,ext) ->
             let code = Pla.print code_t in
             let () =
                if write_files then
                   let filename = args.output^"."^ext in
                   if not (FileIO.write filename code) then
                      failwith ("Failed to write file "^filename)
             in
             s^"\n"^code)
         "" files
   in
   if not write_files && txt <> "" then print_endline txt;
   txt

let generateCode (args:arguments) (parser_results:parser_results list) : string =
   try
      let files = Generate.generateCode parser_results args in
      writeOutput args files
   with
   | Error.Errors(errors) ->
      let error_strings = Error.reportErrors errors in
      print_endline error_strings;
      exit (-1)

(** Prints the parsed files if -dparse was passed as argument *)
let dumpParsedFiles (args:arguments) (parser_results:parser_results list) : unit =
   try
      if args.dparse then
         parser_results
         |> Passes.applyTransformations args
         |> List.iter (fun a -> PrintTypes.stmtListStr a.presult |> print_string)
   with
   | Error.Errors(errors) ->
      let error_strings = Error.reportErrors errors in
      print_endline error_strings

(** Prints the parsed files if -dparse was passed as argument *)
let runFiles (args:arguments) (parser_results:parser_results list) : unit =
   try
      let print_val e =
         match e with
         | PUnit _ -> ()
         | _ -> print_endline (PrintTypes.expressionStr e)
      in
      if args.eval then
         parser_results
         |> Interpreter.eval
         |> List.iter print_val
   with
   | Error.Errors(errors) ->
      let error_strings = Error.reportErrors errors in
      print_endline error_strings

(** Parses the code and and generates the target *)
let parseStringGenerateCode (args:arguments) (code:string) : string =
   try
      ParserVult.parseString None code
      |> fun a -> generateCode args [a]
   with
   | Error.Errors(errors) ->
      let error_strings = Error.reportErrors errors in
      "Errors in the program:\n"^error_strings

(** Parses the code and returns either the transformed code or the error message *)
let parsePrintCode (code:string) : string =
   try
      ParserVult.parseString None code
      |> Passes.applyTransformationsSingle default_arguments
      |> (fun a -> PrintTypes.stmtListStr a.presult)
   with
   | Error.Errors(errors) ->
      let error_strings = Error.reportErrors errors in
      "Errors in the program:\n"^error_strings

(** Checks the code and returns a list with the errors *)
let checkCode (code:string) : (string * string * int * int) list =
   try
      ParserVult.parseString None code
      |> Passes.applyTransformationsSingle default_arguments
      |> fun _ -> []
   with
   | Error.Errors(errors) ->
      List.map Error.reportErrorStringNoLoc errors

let generateLuaCode (files:string list) : string =
   let args = { default_arguments with files; luacode = true } in
   match Generate.generateCode (List.map ParserVult.parseFile files) args with
   | [code_tpl,_] ->
      let code = Pla.print code_tpl |> String.escaped in
      Pla.print [%pla{|return { error = {}, code = "<#code#s>" } |}]
   | _ ->
      Pla.print  [%pla{|return { error = {}, code = "" }|}]
   | exception Error.Errors(errors) ->
      let makeErrorObject error =
         let msg,file,line,col = Error.reportErrorStringNoLoc error in
         let msg = String.escaped msg in
         [%pla{|{ msg = "<#msg#s>", file = "<#file#s>", line = <#line#i>, col = <#col#i>}|}]
      in
      let error =
         Pla.map_sep Pla.comma makeErrorObject errors
         |> Pla.wrap (Pla.string "{") (Pla.string "}")
      in
      Pla.print  [%pla{|return { error = <#error#>, code = "" }|}]
;;

Callback.register "generateLuaCode" generateLuaCode ;;