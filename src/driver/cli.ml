(*
   The MIT License (MIT)

   Copyright (c) 2021 Leonardo Laguna Ruiz, Carl Jönsson

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
open Pparser
open Core
open Generators

let showResult (args : args) (output : output) =
  match output with
  | Version v -> print_endline v
  | Message v -> print_endline v
  | Dependencies deps -> String.concat " " deps |> print_endline
  | EvalResult v -> print_endline v
  | ParsedCode v -> print_endline v
  | Typed v -> print_endline v
  | Prog v -> print_endline v
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
    prerr_endline error_strings;
    exit 1


let generateCode args file_deps (stmts, vm, acc) =
  let stmts = Util.Profile.time "Generate Tables" (fun () -> Tables.create args vm stmts) in
  if args.code <> NoCode || args.dcode then
    let stmts = Util.Profile.time "Convert" (fun () -> Tocode.prog args stmts) in
    let prog_out =
      if args.dcode then
        [ Prog (Pla.print (Prog.Print.print_prog stmts)) ]
      else
        []
    in
    let code =
      match args.code with
      | NoCode -> []
      | CppCode ->
        Util.Profile.time "Generate Code" (fun () -> Cpp.generate file_deps args.split args args.template stmts)
      | LuaCode -> Lua.generate args stmts
      | JSCode -> Js.generate args stmts
      | JavaCode -> failwith "Javascript generator not implemented yet"
    in
    (GeneratedCode code :: prog_out) @ acc
  else
    acc


let compileCode (args : args) env stmts : Prog.top_stmt list * Interpreter.iprog * output list =
  let env, stmts = Toprog.convert args env stmts in
  let prog = Util.Profile.time "Passes" (fun () -> Passes.run args stmts) in
  let iprog = Util.Profile.time "Compile" (fun () -> Interpreter.transformProgram false prog) in
  let prog_out =
    if args.dprog then
      [ Prog (Pla.print (Prog.Print.print_prog prog)) ]
    else
      []
  in
  let run =
    match args.eval with
    | Some fn ->
      let result = Util.Profile.time "Eval" (fun () -> Interpreter.evalProgram iprog fn []) in
      let str = Interpreter.printDvalue result in
      [ EvalResult str ]
    | None -> []
  in
  prog, iprog, run @ prog_out


let version = String.sub Version.version 1 (String.length Version.version - 2)

let driver (args : args) : output list =
  try
    if args.show_version then
      [ Version version ]
    else
      (* Parse the files *)
      match args.files with
      | [] -> [ Message ("vult " ^ version ^ " - https://github.com/vult-dsp/vult\nno input files") ]
      | _ ->
        let parsed, file_deps = Util.Profile.time "Load files" (fun () -> Loader.loadFiles args args.files) in
        if args.deps then
          List.map (fun r -> r.Parse.file) parsed |> fun s -> [ Dependencies s ]
        else if args.dparse then
          List.map (fun (r : Parse.parsed_file) -> ParsedCode (Syntax.Print.print r.stmts)) parsed
        else
          let env, stmts = Util.Profile.time "Inference" (fun () -> Inference.infer args parsed) in
          if args.dtyped then
            [ Typed (Pla.print (Typed.print_prog stmts)) ]
          else
            compileCode args env stmts |> generateCode args file_deps
  with
  | Error.Errors errors when args.debug = false -> [ Errors errors ]


let main () =
  let args = processArguments () in
  let results = driver args in
  List.iter (showResult args) results;
  if args.profile then
    Util.Profile.show ();
  exit 0
