(*
   The MIT License (MIT)

   Copyright (c) 2014-2024 Leonardo Laguna Ruiz

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

open Util.Args

let tmp_dir = Filename.get_temp_dir_name ()

let init_dir = Sys.getcwd ()

let in_proj_dir file = Filename.concat init_dir file

let call_uname () =
  let ic = Unix.open_process_in "uname" in
  let uname = input_line ic in
  let () = close_in ic in
  uname


let os : string =
  match Sys.os_type with
  | "Win32" | "Cygwin" -> "Windows"
  | "Unix" -> (
    match call_uname () with
    | "Linux" -> "Linux"
    | "Darwin" -> "OSX"
    | _ -> failwith "cannot get os"
    | exception _ -> failwith "cannot get os")
  | _ -> failwith "cannot get os"


let files =
  [ "test/perf/saw_eptr_perf.vult"
  ; "test/perf/saw_ptr1_perf.vult"
  ; "test/perf/saw_ptr2_perf.vult"
  ; "test/perf/saw_r_perf.vult"
  ; "test/perf/sawcore_perf.vult"
  ; "test/perf/saw_blit_perf.vult"
  ; "test/perf/blit_perf.vult" (*; "test/perf/minblep_perf.vult"*)
  ; "test/perf/noise_perf.vult"
  ; "test/perf/phd_perf.vult"
  ; "test/perf/sine_perf.vult"
  ; "test/perf/tricore_perf.vult"
  ; "test/perf/svf_perf.vult"
  ; "test/perf/ladder_euler_perf.vult"
  ; "test/perf/ladder_heun_perf.vult"
  ; "test/perf/rescomb_perf.vult"
  ; "test/perf/bitcrush_perf.vult"
  ; "test/perf/saturate_soft_perf.vult"
  ; "test/perf/saturate_perf.vult"
  ; "test/perf/clipper_perf.vult"
  ; "test/perf/short_delay_perf.vult"
  ]
  |> CCList.map in_proj_dir


let includes = [ "examples/util"; "examples/osc"; "examples/filters"; "examples/effects" ] |> CCList.map in_proj_dir

let showError e =
  match e with
  | Util.Error.Errors errors ->
    let error_strings = Util.Error.reportErrors errors in
    prerr_endline error_strings
  | _ -> raise e


let compileFile (file : string) =
  let basename = Filename.chop_extension (Filename.basename file) in
  let cmd =
    Printf.sprintf "g++ -O3 -std=c++11 -ffast-math -Werror -I. -I%s -c %s -o %s.o" (in_proj_dir "runtime") file basename
  in
  if Sys.command cmd <> 0 then
    failwith ("Failed to compile " ^ file)


let linkFiles (output : string) (files : string list) =
  let lflags =
    if os = "Linux" then
      "-lm"
    else
      ""
  in
  let cmd = Printf.sprintf "g++ -o %s %s %s" output (String.concat " " files) lflags in
  if Sys.command cmd <> 0 then
    failwith "Failed to link "


let generateC (filename : string) (output : string) real : unit =
  let args =
    { default_arguments with
      files = [ File filename ]
    ; code = CppCode
    ; output = Some output
    ; real
    ; template = Some "performance"
    ; includes
    }
  in
  let output = Driver.Cli.driver args in
  CCList.iter (Driver.Cli.showResult args) output


let generateJs (filename : string) (output : string) : unit =
  let args =
    { default_arguments with
      files = [ File filename ]
    ; code = JSCode
    ; output = Some output
    ; real = Float
    ; template = Some "performance"
    ; includes
    }
  in
  let output = Driver.Cli.driver args in
  CCList.iter (Driver.Cli.showResult args) output


let generateLua (filename : string) (output : string) : unit =
  let args =
    { default_arguments with
      files = [ File filename ]
    ; code = LuaCode
    ; output = Some output
    ; real = Float
    ; template = Some "performance"
    ; includes
    }
  in
  let output = Driver.Cli.driver args in
  CCList.iter (Driver.Cli.showResult args) output


let generateJulia (filename : string) (output : string) : unit =
  let args =
    { default_arguments with
      files = [ File filename ]
    ; code = JuliaCode
    ; output = Some output
    ; real = Float
    ; template = Some "performance"
    ; includes
    }
  in
  let output = Driver.Cli.driver args in
  CCList.iter (Driver.Cli.showResult args) output


let generateJava (filename : string) (output : string) : unit =
  let args =
    { default_arguments with
      files = [ File filename ]
    ; code = JavaCode
    ; output = Some output
    ; real = Float
    ; template = Some "performance"
    ; includes
    }
  in
  let output = Driver.Cli.driver args in
  CCList.iter (Driver.Cli.showResult args) output


let realString f =
  match f with
  | Fixed -> "fixed"
  | Float -> "float"


let runC real_type vultfile =
  try
    let output = Filename.chop_extension (Filename.basename vultfile) in
    Sys.chdir tmp_dir;
    generateC vultfile output real_type;
    compileFile (output ^ ".cpp");
    compileFile (in_proj_dir "runtime/vultin.cpp");
    linkFiles ("perf_" ^ realString real_type) [ "vultin.o"; output ^ ".o" ];
    ignore (Sys.command ("./perf_" ^ realString real_type));
    Sys.remove (output ^ ".cpp");
    Sys.remove (output ^ ".h");
    Sys.chdir init_dir
  with
  | e -> showError e


let runJs vultfile =
  try
    let output = Filename.chop_extension (Filename.basename vultfile) in
    Sys.chdir tmp_dir;
    generateJs vultfile output;
    ignore (Sys.command ("node " ^ output ^ ".js"));
    Sys.chdir init_dir
  with
  | e -> showError e


let generateJsBun (filename : string) (output : string) : unit =
  let args =
    { default_arguments with
      files = [ File filename ]
    ; code = JSCode
    ; output = Some output
    ; real = Float
    ; template = Some "performance-bun"
    ; includes
    }
  in
  let output = Driver.Cli.driver args in
  CCList.iter (Driver.Cli.showResult args) output


let runBun vultfile =
  try
    let output = Filename.chop_extension (Filename.basename vultfile) in
    Sys.chdir tmp_dir;
    generateJsBun vultfile output;
    ignore (Sys.command ("bun " ^ output ^ ".js"));
    Sys.chdir init_dir
  with
  | e -> showError e


let runLua vultfile =
  try
    let output = Filename.chop_extension (Filename.basename vultfile) in
    Sys.chdir tmp_dir;
    generateLua vultfile output;
    ignore (Sys.command ("luajit -O3 " ^ output ^ ".lua"));
    Sys.chdir init_dir
  with
  | e -> showError e


let runStandardLua vultfile =
  try
    let output = Filename.chop_extension (Filename.basename vultfile) in
    Sys.chdir tmp_dir;
    generateLua vultfile output;
    ignore (Sys.command ("lua " ^ output ^ ".lua"));
    Sys.chdir init_dir
  with
  | e -> showError e


let runJulia vultfile =
  try
    let output = Filename.chop_extension (Filename.basename vultfile) in
    Sys.chdir tmp_dir;
    generateJulia vultfile output;
    ignore (Sys.command ("julia -O3 " ^ output ^ ".jl"));
    Sys.chdir init_dir
  with
  | e -> showError e


[@@@warning "-32"]

let runJava vultfile =
  try
    let output = Filename.chop_extension (Filename.basename vultfile) in
    let module_name = Pparser.Parse.moduleName vultfile in
    let class_name = String.capitalize_ascii output in
    Sys.chdir tmp_dir;
    generateJava vultfile output;
    (* Now we have two files: output.java and outputPerf.java *)
    let main_java_file = output ^ ".java" in
    let perf_java_file = output ^ "Perf.java" in
    let proper_main_file = class_name ^ ".java" in
    let proper_perf_file = module_name ^ "Perf.java" in
    (* Rename files to match class names *)
    Sys.rename main_java_file proper_main_file;
    Sys.rename perf_java_file proper_perf_file;
    (* Create package directory *)
    ignore (Sys.command "mkdir -p vult");
    (* Compile both Java files *)
    let compile_cmd = "javac " ^ proper_main_file ^ " " ^ proper_perf_file in
    if Sys.command compile_cmd <> 0 then (
      (* If compilation fails, print a warning but don't fail the whole test *)
      Printf.eprintf "Warning: Java compilation failed for %s\n%!" module_name;
      Printf.printf "%s\tJava\tCompilation Failed\n%!" module_name)
    else (
      (* Move class files to package directory *)
      ignore (Sys.command "mv *.class vult/");
      (* Run the performance test *)
      let run_cmd = "java -cp . vult." ^ module_name ^ "Perf" in
      ignore (Sys.command run_cmd));
    (* Clean up *)
    (try Sys.remove proper_main_file with
    | _ -> ());
    (try Sys.remove proper_perf_file with
    | _ -> ());
    ignore (Sys.command "rm -rf vult/");
    Sys.chdir init_dir
  with
  | e -> showError e


let runInterpreter vultfile =
  try
    let module_name = Pparser.Parse.moduleName vultfile in
    let args =
      { default_arguments with
        files =
          [ Code ("Perf.vult", "fun main() { iter(i, 5 * 44100) { " ^ module_name ^ ".process(0.0); } }")
          ; File vultfile
          ]
      ; eval = Some "Perf.main"
      ; includes
      }
    in
    let parsed, _ = Driver.Loader.loadFiles args args.files in
    let env, stmts = Core.Inference.infer args parsed in
    let _env, stmts = Core.Toprog.convert args env stmts in
    let stmts = Core.Passes.run args stmts in
    let iprog = Core.Interpreter.transformProgram false stmts in
    let t1 = Sys.time () in
    let _result = Core.Interpreter.evalProgram iprog "Perf.main" [] in
    let t2 = Sys.time () in
    print_endline (Printf.sprintf "%s\tEval\t%f ms/s" module_name ((t2 -. t1) /. 5.0 *. 1000.0))
  with
  | Util.Error.Errors errors ->
    let error_strings = Util.Error.reportErrors errors in
    prerr_endline error_strings;
    exit 1


let main () =
  CCList.iter
    (fun f ->
      runC Float f;
      runC Fixed f;
      runStandardLua f;
      runLua f;
      runJs f;
      runBun f;
      runJulia f;
      runJava f;
      runInterpreter f)
    files

;;

main ()
