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
  ; "test/perf/blit_perf.vult"
  ; "test/perf/minblep_perf.vult"
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
  |> List.map in_proj_dir


let includes = [ "examples/util"; "examples/osc"; "examples/filters"; "examples/effects" ] |> List.map in_proj_dir

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
  if Sys.command cmd <> 0 then failwith ("Failed to compile " ^ file)


let linkFiles (output : string) (files : string list) =
  let lflags = if os = "Linux" then "-lm" else "" in
  let cmd = Printf.sprintf "g++ -o %s %s %s" output (String.concat " " files) lflags in
  if Sys.command cmd <> 0 then failwith "Failed to link "


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
  List.iter (Driver.Cli.showResult args) output


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
  List.iter (Driver.Cli.showResult args) output


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
  List.iter (Driver.Cli.showResult args) output


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


let _runJs vultfile =
  try
    let output = Filename.chop_extension (Filename.basename vultfile) in
    Sys.chdir tmp_dir;
    generateJs vultfile output;
    ignore (Sys.command "node main.js");
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


let main () =
  List.iter
    (fun f ->
      runC Float f;
      runC Fixed f;
      runLua f (*  runJs f*))
    files

;;

main ()
