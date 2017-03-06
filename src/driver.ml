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

let generateCode (args:arguments) (parser_results:parser_results list) : output list =
   match Generate.generateCode parser_results args with
   | [] -> []
   | results -> [GeneratedCode results]

(** Prints the parsed files if -dparse was passed as argument *)
let dumpParsedFiles (args:arguments) (parser_results:parser_results list) : output list  =
   if args.dparse then
      parser_results
      |> Passes.applyTransformations args
      |> List.map (fun a -> PrintTypes.stmtListStr a.presult)
      |> String.concat "\n"
      |> (fun a -> [ParsedCode a])
   else []

(** Prints the parsed files if -dparse was passed as argument *)
let runFiles (args:arguments) (parser_results:parser_results list) : output list =
   let print_val e =
      match e with
      | PUnit _ -> []
      | _ -> [PrintTypes.expressionStr e]
   in
   if args.eval then
      Passes.applyTransformations args ~options:PassCommon.interpreter_options parser_results
      |> Interpreter.eval
      |> List.map print_val
      |> List.flatten
      |> (fun a -> [Interpret (String.concat "\n" a)])
   else
      []

(** Checks the code and returns a list with the errors *)
let checkCode (code:string) : (string * string * int * int) list =
   try
      ParserVult.parseString None code
      |> Passes.applyTransformationsSingle default_arguments
      |> fun _ -> []
   with
   | Error.Errors(errors) ->
      List.map Error.reportErrorStringNoLoc errors

(*
let generateLuaCode (files:string list) : string =
   let args = { default_arguments with files; luacode = true } in
   let parsed = Loader.loadFiles args files in
   match Generate.generateCode parsed args with
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

*)

let version = String.sub Version.version 1 ((String.length Version.version) - 2)

let main (args:arguments)  : output list =
   try
      if args.show_version then
         [Version version]
      else
         (* Parse the files *)
         match args.files with
         | [] ->
            [Message ("vult " ^ version ^ " - https://github.com/modlfo/vult\nno input files")]
         | _ ->
            let parser_results = Loader.loadFiles args args.files in
            if args.deps then
               List.map (fun r -> r.file) parser_results
               |> (fun s -> [Dependencies s])
            else
               begin
                  dumpParsedFiles args parser_results
                  @  generateCode args parser_results
                  @ runFiles args parser_results
               end
   with
   | Error.Errors(errors) -> [Errors errors]

