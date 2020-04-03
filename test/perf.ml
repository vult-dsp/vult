(*
   The MIT License (MIT)

   Copyright (c) 2017 Leonardo Laguna Ruiz

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

open Args
open Tcommon

let init_dir = Sys.getcwd ()

let in_proj_dir file = Filename.concat init_dir file

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
  | Error.Errors errors ->
      let error_strings = Error.reportErrors errors in
      prerr_endline error_strings
  | _ -> raise e


let compileFile (file : string) =
  let basename = Filename.chop_extension (Filename.basename file) in
  let cmd = Printf.sprintf "gcc -ffast-math -Werror -I. -I%s -O3 -c %s -o %s.o" (in_proj_dir "runtime") file basename in
  if Sys.command cmd <> 0 then
    failwith ("Failed to compile " ^ file)


let linkFiles (output : string) (files : string list) =
  let lflags = if os = "Linux" then "-lm" else "" in
  let cmd = Printf.sprintf "gcc -o %s %s %s" output (String.concat " " files) lflags in
  if Sys.command cmd <> 0 then
    failwith "Failed to link "


let generateC (filename : string) (output : string) (real : string) : unit =
  let args =
    { default_arguments with files = [ File filename ]; code = CCode; output; real; template = "performance"; includes }
  in
  let parser_results = Loader.loadFiles args [ File filename ] in
  let gen = Generate.generateCode parser_results args in
  writeFiles args gen


let generateJs (filename : string) (output : string) : unit =
  let args =
    { default_arguments with files = [ File filename ]; template = "performance"; includes; code = JSCode; output }
  in
  let parser_results = Loader.loadFiles args [ File filename ] in
  let gen = Generate.generateCode parser_results args in
  writeFiles args gen


let generateLua (filename : string) (output : string) : unit =
  let args =
    { default_arguments with files = [ File filename ]; template = "performance"; includes; code = LuaCode; output }
  in
  let parser_results = Loader.loadFiles args [ File filename ] in
  let gen = Generate.generateCode parser_results args in
  writeFiles args gen


let runC real_type vultfile =
  try
    let output = Filename.chop_extension (Filename.basename vultfile) in
    Sys.chdir tmp_dir ;
    generateC vultfile output real_type ;
    compileFile (output ^ ".cpp") ;
    compileFile (in_proj_dir "runtime/vultin.cpp") ;
    compileFile "main.cpp" ;
    linkFiles ("perf_" ^ real_type) [ "vultin.o"; output ^ ".o"; "main.o" ] ;
    ignore (Sys.command ("./perf_" ^ real_type)) ;
    Sys.remove (output ^ ".cpp") ;
    Sys.remove (output ^ ".h") ;
    Sys.chdir initial_dir
  with
  | e -> showError e


let runJs vultfile =
  try
    let output = Filename.chop_extension (Filename.basename vultfile) in
    Sys.chdir tmp_dir ;
    generateJs vultfile output ;
    ignore (Sys.command "node main.js") ;
    Sys.chdir initial_dir
  with
  | e -> showError e


let runLua vultfile =
  try
    let output = Filename.chop_extension (Filename.basename vultfile) in
    Sys.chdir tmp_dir ;
    generateLua vultfile output ;
    ignore (Sys.command "luajit -O3 main.lua") ;
    Sys.chdir initial_dir
  with
  | e -> showError e


let main () =
  List.iter
    (fun f ->
      runC "float" f ;
      runC "fixed" f ;
      runLua f ;
      runJs f)
    files


;;
main ()
