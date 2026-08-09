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
  | Version v ->
      print_endline v
  | Message v ->
      print_endline v
  | Dependencies deps ->
      String.concat " " deps |> print_endline
  | EvalResult v ->
      print_endline v
  | AudioRendered msg ->
      print_endline msg
  | ParsedCode v ->
      print_endline v
  | Typed v ->
      print_endline v
  | Prog v ->
      print_endline v
  | GeneratedCode files when args.output <> None ->
      CCList.iter
        (fun (text, filename) ->
          let code = Pla.print text in
          if Filename.check_suffix filename ".table" then FileIO.write_bytes filename code |> ignore
          else if args.force_write then FileIO.write filename code |> ignore
          else FileIO.writeIfDifferent filename code |> ignore )
        files
  | GeneratedCode files ->
      CCList.iter (fun (text, _) -> print_endline (Pla.print text)) files
  | Interpret v ->
      print_endline v
  | CheckOk ->
      ()
  | Errors errors ->
      let error_strings = Error.reportErrors errors in
      prerr_endline error_strings ; exit 1

let generateCode args file_deps (stmts, vm, acc) =
  let stmts = Util.Profile.time "Generate Tables" (fun () -> Tables.create args vm stmts) in
  if args.code <> NoCode || args.dcode then
    let stmts = Util.Profile.time "Convert" (fun () -> Tocode.prog args stmts) in
    let prog_out = if args.dcode then [Prog (Pla.print (Prog.Print.print_prog stmts))] else [] in
    let code =
      match args.code with
      | NoCode ->
          []
      | CppCode ->
          Util.Profile.time "Generate Code" (fun () -> Cpp.generate file_deps args.split args args.template stmts)
      | LuaCode ->
          Lua.generate args stmts
      | JSCode ->
          Js.generate args stmts
      | JavaCode ->
          Java.generate args stmts
      | JuliaCode ->
          Julia.generate args stmts
      | PythonCode ->
          Python.generate args stmts
      | ZigCode ->
          Zig.generate args stmts
    in
    (GeneratedCode code :: prog_out) @ acc
  else acc

let compileCode (args : args) env stmts : Prog.top_stmt list * Interpreter.iprog * output list =
  let env, stmts = Toprog.convert args env stmts in
  let prog = Util.Profile.time "Passes" (fun () -> Passes.run args stmts) in
  let iprog = Util.Profile.time "Compile" (fun () -> Interpreter.transformProgram prog) in
  let prog_out = if args.dprog then [Prog (Pla.print (Prog.Print.print_prog prog))] else [] in
  (* Resolve effective backend: fall back to Interpreter under js_of_ocaml *)
  let effective_backend : eval_backend =
    match args.eval_backend with
    | CVM | OcamlVM -> (
      match Sys.backend_type with Other s when String.equal s "js_of_ocaml" -> Interpreter | _ -> args.eval_backend )
    | b ->
        b
  in
  (* Lazily compile bytecode only when needed *)
  let bc_prog_lazy = lazy (Util.Profile.time "Bytecode Compile" (fun () -> Vm.Compiler.compile prog)) in
  let bytecode_dump =
    if args.dbytecode then
      let bc_prog = Lazy.force bc_prog_lazy in
      [Prog (Vm.Bytecode.dump bc_prog)]
    else []
  in
  let eval_out =
    match args.eval with
    | Some fn -> (
      match effective_backend with
      | Interpreter ->
          let result = Util.Profile.time "Eval" (fun () -> Interpreter.evaluateMainExpression args env iprog fn) in
          [EvalResult (Interpreter.printDvalue result)]
      | OcamlVM ->
          let bc_prog = Lazy.force bc_prog_lazy in
          let result =
            Util.Profile.time "Bytecode Eval" (fun () -> Vm.Exec.evaluateMainExpression args env bc_prog fn)
          in
          [EvalResult (Vm.Bytecode.printValue result)]
      | CVM ->
          let bc_prog = Lazy.force bc_prog_lazy in
          let result =
            Util.Profile.time "Bytecode Eval" (fun () -> Vm.Exec.evaluateMainExpressionC args env bc_prog fn)
          in
          [EvalResult (Vm.Bytecode.printValue result)] )
    | None ->
        []
  in
  let render_out =
    match args.render with
    | Some tag -> (
      match effective_backend with
      | Interpreter ->
          let filename, duration =
            Util.Profile.time "Render" (fun () -> Interpreter.renderAudioExpression args env iprog tag)
          in
          [AudioRendered (Printf.sprintf "Audio rendered to: %s (%.3fs)" filename duration)]
      | OcamlVM ->
          let bc_prog = Lazy.force bc_prog_lazy in
          let filename, duration =
            Util.Profile.time "Bytecode Render" (fun () -> Vm.Exec.renderAudioExpression args env bc_prog tag)
          in
          [AudioRendered (Printf.sprintf "Audio rendered to: %s (%.3fs)" filename duration)]
      | CVM ->
          let bc_prog = Lazy.force bc_prog_lazy in
          let filename, duration =
            Util.Profile.time "Bytecode Render" (fun () -> Vm.Exec.renderAudioExpressionC args env bc_prog tag)
          in
          [AudioRendered (Printf.sprintf "Audio rendered to: %s (%.3fs)" filename duration)] )
    | None ->
        []
  in
  (prog, iprog, eval_out @ render_out @ prog_out @ bytecode_dump)

let version = Version.version

let driver (args : args) : output list =
  try
    if args.show_version then [Version version]
    else
      (* Parse the files *)
      match args.files with
      | [] ->
          [Message ("vult " ^ version ^ " - https://github.com/vult-dsp/vult\nno input files")]
      | _ ->
          let parsed, file_deps = Util.Profile.time "Load files" (fun () -> Loader.loadFiles args args.files) in
          if args.deps then CCList.map (fun r -> r.Parse.file) parsed |> fun s -> [Dependencies s]
          else if args.dparse then
            CCList.map (fun (r : Parse.parsed_file) -> ParsedCode (Syntax.Print.print r.stmts)) parsed
          else if args.dump_sexpr then
            CCList.map
              (fun (r : Parse.parsed_file) ->
                ParsedCode (String.concat "\n" (CCList.map Syntax.SExpr.print_top_stmt r.stmts)) )
              parsed
          else
            let env, stmts =
              Util.Profile.time "Typechecking" (fun () -> Typechecking.typecheck_and_elaborate args parsed)
            in
            if args.dtyped then
              let () = Typed.print_exp_locs := args.dlocs in
              [Typed (Pla.print (Typed.print_prog stmts))]
            else compileCode args env stmts |> generateCode args file_deps
  with Error.Errors errors when args.debug = false -> [Errors errors]

let run (args : args) : unit =
  let results = driver args in
  CCList.iter (showResult args) results ;
  if args.profile then Util.Profile.show () ;
  exit 0

let main () =
  let args = processArguments () in
  run args
