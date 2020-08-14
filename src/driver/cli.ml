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
open Util
open Util.Args
open Parser
open Core
open Generators

let showResult (args : args) (output : output) =
  match output with
  | Version v -> print_endline v
  | Message v -> print_endline v
  | Dependencies deps -> String.concat " " deps |> print_endline
  | ParsedCode v -> print_endline v
  | GeneratedCode files when args.output <> None ->
      List.iter
        (fun (text, filename) ->
          let code = Pla.print text in
          if args.force_write then
            FileIO.write filename code |> ignore
          else
            FileIO.writeIfDifferent filename code |> ignore)
        files
  | GeneratedCode files -> List.iter (fun (text, _) -> print_endline (Pla.print text)) files
  | Interpret v -> print_endline v
  | CheckOk -> ()
  | Errors errors ->
      let error_strings = Error.reportErrors errors in
      prerr_endline error_strings


let generate args stmts =
  let vm = Vm.Interpreter.createVm stmts in
  let cstmts = Code.Convert.prog stmts in
  let cstmts = Tables.create vm cstmts in
  let code =
    match args.code with
    | NoCode -> []
    | CCode -> Cpp.generate args.output args.template cstmts
    | LuaCode -> Lua.generate args.output args.template stmts
    | JSCode -> failwith "Javascript generator not implemented yet"
    | JavaCode -> failwith "Javascript generator not implemented yet"
  in
  GeneratedCode code


let generateCode (args : args) (parsed : Parse.parsed_file list) : output list =
  let env, typed = Inference.infer parsed in
  let stmts = Prog.convert env typed in
  let stmts = Passes.run args stmts in
  let code = generate args stmts in
  let run =
    match args.eval with
    | Some e ->
        let s = Vm.Interpreter.run env stmts e in
        [ ParsedCode s ]
    | None -> []
  in
  code :: run


(** Prints the parsed files if -dparse was passed as argument *)
let dumpParsedFiles (args : args) (parsed : Parse.parsed_file list) : output list =
  if args.dparse then
    [ ParsedCode "not implemented" ]
  else
    []


let version = String.sub Version.version 1 (String.length Version.version - 2)

let driver (args : args) : output list =
  try
    if args.show_version then
      [ Version version ]
    else (* Parse the files *)
      match args.files with
      | [] -> [ Message ("vult " ^ version ^ " - https://github.com/modlfo/vult\nno input files") ]
      | _ ->
          let parsed = Loader.loadFiles args args.files in
          if args.deps then
            List.map (fun r -> r.Parse.file) parsed |> fun s -> [ Dependencies s ]
          else
            dumpParsedFiles args parsed @ generateCode args parsed
  with
  | Error.Errors errors -> [ Errors errors ]


let main () =
  let args = processArguments () in
  let results = driver args in
  List.iter (showResult args) results ;
  exit 0
