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
  | Warnings warnings ->
      prerr_endline (Error.reportWarnings warnings)

let generateCode args (stmts, iprog, acc) =
  (* Before the tables are generated, so their lookups are built in the target's precision. *)
  let stmts = if args.code = JavaCode then Passes.fix16ToReal stmts else stmts in
  (* The tables were checked after type checking; -check skips their (slow) evaluation *)
  let stmts =
    if args.tables && not args.check then Util.Profile.time "Generate Tables" (fun () -> Tables.create args iprog stmts)
    else stmts
  in
  let acc = if args.dtables then acc @ [Prog (Pla.print (Prog.Print.print_prog stmts))] else acc in
  (* -check runs every stage that can report an error, but does not generate code *)
  if args.check then acc @ [CheckOk]
  else if args.code <> NoCode || args.dcode then
    let stmts = Util.Profile.time "Convert" (fun () -> Tocode.prog args stmts) in
    let stmts = if args.shorten then Shorten.run stmts else stmts in
    let prog_out = if args.dcode then [Prog (Pla.print (Prog.Print.print_prog stmts))] else [] in
    let code =
      match args.code with
      | NoCode ->
          []
      | CppCode ->
          Util.Profile.time "Generate Code" (fun () -> Cpp.generate args.split args args.template stmts)
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
    (* The generators return a Pla.t that only runs when it is printed, and printing happens after
       the error handler in [driver] is out of scope. Rendering the files here keeps the errors the
       templates raise (an out-of-range fixed-point literal, an unsupported cast) reportable. *)
    let code = CCList.map (fun (text, file) -> (Pla.string (Pla.print text), file)) code in
    (GeneratedCode code :: prog_out) @ acc
  else acc

(* Reports the runtime errors of an evaluation (constants, -eval, -render) as errors *)
let reportRuntimeErrors (f : unit -> 'a) : 'a =
  try f () with
  | Interpreter.Runtime_error msg ->
      Error.raiseErrorMsg ("Runtime error: " ^ msg)
  | Division_by_zero ->
      Error.raiseErrorMsg "Runtime error: Integer division by zero"

let compileCode (args : args) env stmts : Prog.top_stmt list * Interpreter.iprog * output list =
  let env, stmts = Toprog.convert args env stmts in
  let prog = Util.Profile.time "Passes" (fun () -> Passes.run args stmts) in
  let iprog =
    Util.Profile.time "Compile" (fun () -> reportRuntimeErrors (fun () -> Interpreter.transformProgram prog))
  in
  let prog_out = if args.dprog then [Prog (Pla.print (Prog.Print.print_prog prog))] else [] in
  let eval_out =
    match args.eval with
    | Some fn ->
        let result =
          Util.Profile.time "Eval" (fun () ->
              reportRuntimeErrors (fun () -> Interpreter.evaluateMainExpression args env iprog fn) )
        in
        [EvalResult (Interpreter.printDvalue result)]
    | None ->
        []
  in
  let render_out =
    match args.render with
    | Some tag ->
        let filename, duration =
          Util.Profile.time "Render" (fun () ->
              reportRuntimeErrors (fun () -> Interpreter.renderAudioExpression args env iprog tag) )
        in
        [AudioRendered (Printf.sprintf "Audio rendered to: %s (%.3fs)" filename duration)]
    | None ->
        []
  in
  (prog, iprog, eval_out @ render_out @ prog_out)

let version = Version.version

let warningsOutput (env : Env.env) : output list =
  match Env.getWarnings env with [] -> [] | warnings -> [Warnings warnings]

let driver (args : args) : output list =
  try
    if args.show_version then [Version version]
    else
      (* Parse the files *)
      match args.files with
      | [] ->
          [Message ("vult " ^ version ^ " - https://github.com/vult-dsp/vult\nno input files")]
      | _ ->
          let parsed, _file_deps = Util.Profile.time "Load files" (fun () -> Loader.loadFiles args args.files) in
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
            let warnings = warningsOutput env in
            if args.dtyped then warnings @ [Typed (Pla.print (Typed.print_prog ~locs:args.dlocs stmts))]
            else if args.dtoprog then
              let _, prog = Toprog.convert args env stmts in
              warnings @ [Prog (Pla.print (Prog.Print.print_prog prog))]
            else warnings @ (compileCode args env stmts |> generateCode args)
  with Error.Errors errors when args.debug = false -> [Errors errors]

let run (args : args) : unit =
  let results = driver args in
  CCList.iter (showResult args) results ;
  if args.profile then Util.Profile.show () ;
  exit 0

let main () =
  let args = processArguments () in
  run args
