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
  | ParsedCode v -> print_endline v
  | Byte v -> print_endline v
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
    prerr_endline error_strings


let generateCode args (stmts, vm, acc) =
  if args.code <> NoCode then (
    let cstmts = Util.Profile.time "Convert" (fun () -> Code.Convert.prog args stmts) in
    let cstmts = Util.Profile.time "Generate Tables" (fun () -> Tables.create args vm cstmts) in
    let code =
      match args.code with
      | NoCode -> []
      | CppCode -> Util.Profile.time "Generate Code" (fun () -> Cpp.generate args.output args.template cstmts)
      | LuaCode -> Lua.generate args.output args.template cstmts
      | JSCode -> failwith "Javascript generator not implemented yet"
      | JavaCode -> failwith "Javascript generator not implemented yet"
    in
    GeneratedCode code :: acc)
  else
    acc


let compileCode (args : args) (parsed, acc) =
  let env, stmts = Util.Profile.time "Inference" (fun () -> Inference.infer args parsed) in
  let typed_out = if args.dtyped then [ Prog (Pla.print (Prog.Print.print_prog stmts)) ] else [] in
  let stmts = Util.Profile.time "Passes" (fun () -> Passes.run args stmts) in
  let prog_out = if args.dprog then [ Prog (Pla.print (Prog.Print.print_prog stmts)) ] else [] in
  let vm, bytecode = Util.Profile.time "Create VM" (fun () -> Vm.Interpreter.createVm stmts) in
  let bc_out = if args.dbytecode then [ Byte (Pla.print (Vm.Compile.print_bytecode bytecode)) ] else [] in
  let run =
    match args.eval with
    | Some e ->
      let s = Util.Profile.time "Run code" (fun () -> Vm.Interpreter.run args env stmts e) in
      [ ParsedCode s ]
    | None -> []
  in
  stmts, vm, run @ bc_out @ prog_out @ typed_out @ acc


(** Prints the parsed files if -dparse was passed as argument *)
let dumpParsedFiles (args : args) (parsed : Parse.parsed_file list) =
  if args.dparse then
    parsed, List.map (fun (r : Parse.parsed_file) -> ParsedCode (Syntax.Print.print r.stmts)) parsed
  else
    parsed, []


let version = String.sub Version.version 1 (String.length Version.version - 2)

let driver (args : args) : output list =
  try
    if args.show_version then
      [ Version version ]
    else ((* Parse the files *)
      match args.files with
      | [] -> [ Message ("vult " ^ version ^ " - https://github.com/modlfo/vult\nno input files") ]
      | _ ->
        let parsed = Util.Profile.time "Load files" (fun () -> Loader.loadFiles args args.files) in
        if args.deps then
          List.map (fun r -> r.Parse.file) parsed |> fun s -> [ Dependencies s ]
        else
          parsed |> dumpParsedFiles args |> compileCode args |> generateCode args)
  with
  | Error.Errors errors when args.debug = false -> [ Errors errors ]


let main () =
  let args = processArguments () in
  let results = driver args in
  List.iter (showResult args) results;
  if args.profile then Util.Profile.show ();
  exit 0
