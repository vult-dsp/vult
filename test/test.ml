(*
   The MIT License (MIT)

   Copyright (c) 2021 Leonardo Laguna Ruiz

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
open OUnit2

(*open Core.Prog*)

open Util
open Tcommon;;

Util.Vfloat.reduce_precision := true

let test_directory = Filename.concat initial_dir "test"
let in_test_directory path = Filename.concat test_directory path
let in_tmp_dir path = Filename.concat tmp_dir path

(** checks if node can be called with flag -c *)
let has_node =
  if tryToRun ("node -c " ^ in_test_directory "other/test.js") then (
    let () = print_endline "Js syntax will be checked" in
    true)
  else (
    let () = print_endline "Js syntax will not be checked" in
    false)


(** checks if luajit can be called *)
let has_lua =
  if tryToRun ("luajit -bl " ^ in_test_directory "other/test.lua" ^ " > out") then (
    let () = print_endline "Lua syntax will be checked" in
    true)
  else (
    let () = print_endline "Lua syntax will not be checked" in
    false)


let parser_files =
  [ "stmt_val_mem.vult"
  ; "stmt_functions.vult"
  ; "stmt_types.vult"
  ; "stmt_external.vult"
  ; "stmt_misc.vult"
  ; "exp_basic.vult"
  ; "exp_misc.vult"
  ; "exp_precedence.vult"
  ; "exp_array.vult"
  ; "exp_tuples.vult"
  ; "array_syntax.vult"
  ; "mem_tag.vult"
  ; "member_access.vult"
  ; "stmt_call.vult"
  ; "new_type_spec.vult"
  ]


let errors_files =
  [ "error1.vult"
  ; "error2.vult"
  ; "error3.vult"
  ; "error4.vult"
  ; "error5.vult"
  ; "error6.vult"
  ; "error7.vult"
  ; "error9.vult"
  ; "error10.vult"
  ; "error13.vult"
  ; "error14.vult"
  ; "error15.vult"
  ; "error16.vult"
  ; "error17.vult"
  ; "error18.vult"
  ; "error19.vult"
  ; "error20.vult"
  ; "error21.vult"
  ; "error22.vult"
  ; "error23.vult"
  ; "error24.vult"
  ; "error25.vult"
  ; "error26.vult"
  ; "error27.vult"
  ; "error28.vult"
  ; "error29.vult"
  ; "error30.vult"
  ; "error31.vult"
  ; "error32.vult"
  ; "error33.vult"
  ; "error34.vult"
  ; "error36.vult"
  ; "error37.vult"
  ; "error38.vult"
  ; "error39.vult"
  ; "error40.vult"
  ; "error41.vult"
  ; "error42.vult"
  ]


let template_files =
  [ "sf_f.vult"; "sff_f.vult"; "sff_ff.vult"; "sfi_fi.vult"; "af_f.vult"; "aff_f.vult"; "aff_ff.vult"; "afi_fi.vult" ]


let perf_files =
  [ "saw_eptr_perf.vult"
  ; "saw_ptr1_perf.vult"
  ; "saw_ptr2_perf.vult"
  ; "saw_r_perf.vult"
  ; "sawcore_perf.vult"
  ; "saw_blit_perf.vult"
  ; "blit_perf.vult"
  ; "noise_perf.vult"
  ; "phd_perf.vult"
  ; "sine_perf.vult"
  ; "tricore_perf.vult"
  ; "svf_perf.vult"
  ; "ladder_euler_perf.vult"
  ; "rescomb_perf.vult"
  ; "bitcrush_perf.vult"
  ; "saturate_soft_perf.vult"
  ; "saturate_perf.vult"
  ; "clipper_perf.vult"
  ; "short_delay_perf.vult"
  ]


let passes_files =
  [ "split_mem.vult"
  ; "tuple_assign.vult"
  ; "if_to_stmt.vult"
  ; "context_simple.vult"
  ; "context_nested.vult"
  ; "tuple_to_types.vult"
  ; "simplify.vult"
  ; "external_calls.vult"
  ; "output_references.vult"
  ; "nested_if.vult"
  ]


let all_files =
  [ "web/phasedist.vult"
  ; "web/synth1.vult"
  ; "web/synth2.vult"
  ; "web/template.vult"
  ; "web/volume.vult"
  ; "web/delay.vult"
  ; "../test/passes/wav_file.vult"
  ; "effects/bitcrush.vult"
  ; "effects/short_delay.vult"
  ; "effects/saturate_soft.vult"
  ; "effects/saturate.vult"
  ; "effects/rescomb.vult"
  ; "env/ad.vult"
  ; "env/adsr.vult"
  ; "env/ahr.vult"
  ; "env/lfo.vult"
  ; "env/swept.vult"
  ; "filters/ladder.vult"
  ; "filters/svf.vult"
  ; "midi/gates.vult"
  ; "midi/monocv.vult"
  ; "midi/polycv.vult"
  ; "osc/blit.vult"
  ; "osc/noise.vult"
  ; "osc/phd.vult"
  ; "osc/saw_eptr.vult"
  ; "osc/saw_ptr1.vult"
  ; "osc/saw_ptr2.vult"
  ; "osc/saw_r.vult"
  ; "osc/phase.vult"
  ; "osc/sawcore.vult"
  ; "osc/sine.vult"
  ; "osc/tricore.vult"
  ; "units/kick.vult"
  ; "units/voice_4.vult"
  ; "newfeatures/datatypes.vult"
  ; "newfeatures/genericarrays.vult"
  ; "newfeatures/instancearray.vult"
  ; "newfeatures/iterloop.vult"
  ; "newfeatures/stringtype.vult"
  ; "newfeatures/fixedandfloat.vult"
  ; "../test/compile/defined_types.vult"
  ; "../test/compile/array_defined_type.vult"
  ; "../test/compile/array_return.vult"
  ; "../test/compile/array_instance.vult"
  ; "../test/compile/array_dimension.vult"
  ; "../test/compile/multi_return.vult"
  ]


let includes =
  [ "effects"; "env"; "filters"; "midi"; "osc"; "unit"; "util" ]
  |> List.map (fun dir -> in_test_directory ("../examples/" ^ dir))


let test_random_code =
  let rec loop n = if n > 0 then Printf.sprintf "test%i.vult" n :: loop (n - 1) else [] in
  loop 1


(** Flags that defines if a baseline should be created for tests *)
let update_test = Conf.make_bool "update" false "Creates a file with the current results"
(** Flags that defines if we should use the command line version of the compiler *)

(** Flags that defines if we should use the command line version of the compiler *)
let internal_test = Conf.make_bool "internal" false "Uses static linked compiler instead of the command line"

let write file contents =
  let oc = open_out file in
  Printf.fprintf oc "%s" contents;
  close_out oc


let read file =
  let ic = open_in file in
  let n = in_channel_length ic in
  let s = Bytes.create n in
  really_input ic s 0 n;
  close_in ic;
  Bytes.to_string s


let readOutputAndReference (create : bool) (outdir : string) (output, reference) =
  let output_txt =
    if Sys.file_exists output then (
      let contents = read output in
      let () = Sys.remove output in
      contents)
    else
      assert_failure (Printf.sprintf "The file '%s' was not generated" output)
  in
  let reference = Filename.concat outdir (Filename.basename reference) in
  let reference_txt =
    if create then (
      let () = write reference output_txt in
      output_txt)
    else if Sys.file_exists reference then
      read reference
    else
      assert_failure (Printf.sprintf "The file '%s' has no reference data" reference)
  in
  output_txt, reference_txt


(** Returns the contents of the reference file for the given vult file *)
let readReference (update : bool) (ext : string) (contents : string) (file : string) (outdir : string) : string =
  let basefile = Filename.chop_extension (Filename.basename file) in
  let ref_file = Filename.concat outdir (basefile ^ "." ^ ext) in
  if update then (
    let () = write ref_file contents in
    contents)
  else if Sys.file_exists ref_file then
    read ref_file
  else
    assert_failure (Printf.sprintf "The file '%s' has no reference data" file)


(** Asserts if the file does not exists *)
let checkFile (filename : string) : string =
  if Sys.file_exists filename then filename else assert_failure (Printf.sprintf "The file '%s' does not exits" filename)


let showResults (result : Pparser.Parse.parsed_file) : string = Pparser.Syntax.Print.print result.stmts

(** Module to perform parsing tests *)
module ParserTest = struct
  let process (fullfile : string) : string = Pparser.Parse.parseFile fullfile |> fun a -> showResults a

  let run (file : string) context =
    let folder = "parser" in
    let fullfile = checkFile (in_test_directory (folder ^ "/" ^ file)) in
    let current = process fullfile in
    let reference = readReference (update_test context) "base" current fullfile (in_test_directory folder) in
    assert_equal
      ~cmp:Diff.compare
      ~msg:("Parsing file " ^ fullfile)
      ~pp_diff:(fun ff (a, b) -> Format.fprintf ff "\n%s" (Diff.lineDiff a b))
      reference
      current


  let get files = "parser" >::: List.map (fun file -> Filename.basename file >:: run file) files
end

module ErrorTest = struct
  let process (fullfile : string) =
    let basefile = in_tmp_dir @@ Filename.chop_extension (Filename.basename fullfile) in
    let args = Args.{ default_arguments with includes; check = true } in
    let args = { args with output = Some basefile; files = [ File fullfile ] } in
    let results = Driver.Cli.driver args in
    List.fold_left
      (fun s a ->
        match a with
        | Args.Errors e ->
          List.map
            (fun a ->
              let msg, _, _, _ = Error.reportErrorStringNoLoc a in
              msg)
            e
          @ s
        | _ -> s)
      []
      results
    |> String.concat "\n"


  let run (file : string) context =
    let folder = "errors" in
    let fullfile = checkFile (in_test_directory (folder ^ "/" ^ file)) in
    let current = process fullfile in
    let reference = readReference (update_test context) "base" current fullfile (in_test_directory folder) in
    assert_equal
      ~cmp:Diff.compare
      ~msg:("Error mismatch in file " ^ fullfile)
      ~pp_diff:(fun ff (a, b) -> Format.fprintf ff "\n%s" (Diff.lineDiff a b))
      reference
      current


  let get files = "errors" >::: List.map (fun file -> Filename.basename file >:: run file) files
end

(** Module to perform transformation tests *)
module PassesTest = struct
  let show o =
    match o with
    | Args.Prog s -> s
    | _ -> ""


  let process (fullfile : string) : string =
    let args = Args.{ default_arguments with dprog = true; files = [ File fullfile ] } in
    Driver.Cli.driver args |> List.map show |> String.concat "\n"


  let run (file : string) context =
    let folder = "passes" in
    let fullfile = checkFile (in_test_directory (folder ^ "/" ^ file)) in
    let current = process fullfile in
    let reference = readReference (update_test context) "base" current fullfile (in_test_directory folder) in
    assert_equal
      ~cmp:Diff.compare
      ~msg:("Transforming file " ^ fullfile)
      ~pp_diff:(fun fmt (a, b) -> Format.fprintf fmt "\n%s" (Diff.lineDiff a b))
      reference
      current


  let get files = "passes" >::: List.map (fun file -> Filename.basename file >:: run file) files
end

(** Tries to compile all the examples *)
module RandomCompileTest = struct
  let compileFile (file : string) =
    let basename = Filename.chop_extension (Filename.basename file) in
    let cmd =
      Printf.sprintf
        "gcc -Wno-return-type -Wno-div-by-zero -Wno-narrowing -Wno-constant-logical-operand -Wno-division-by-zero \
         -Wno-unused-value -Wno-tautological-compare -Wconversion -I%s -c %s -o %s"
        (in_test_directory "../runtime")
        file
        basename
    in
    if Sys.command cmd <> 0 then assert_failure ("Failed to compile " ^ file)


  let generateCPP (filename : string) (output : string) : unit =
    let args = Args.{ default_arguments with files = [ File filename ]; code = CppCode; output = Some output } in
    let seed = Hashtbl.hash filename in
    let code = RandProg.run seed in
    write filename code;
    let parser_results = Pparser.Parse.parseString (Some filename) code in
    let stmts, vm, _ = Driver.Cli.compileCode args ([ parser_results ], []) in
    let gen = Driver.Cli.generateCode args (stmts, vm, []) in
    writeFiles args gen


  let run (file : string) _ =
    let output = Filename.chop_extension (Filename.basename file) in
    Sys.chdir tmp_dir;
    generateCPP file output;
    assert_bool "No code generated" (Sys.file_exists (output ^ ".cpp"));
    compileFile (output ^ ".cpp");
    Sys.remove (output ^ ".cpp");
    Sys.remove (output ^ ".h");
    Sys.remove (output ^ ".vult");
    Sys.remove output;
    Sys.chdir initial_dir


  let get files = "compile" >::: List.map (fun file -> Filename.basename file ^ ".float" >:: run file) files
end

type compiler =
  | Node
  | Native

let callCompiler (file : string) : unit =
  let basename = Filename.chop_extension (Filename.basename file) in
  let cmd =
    Printf.sprintf
      "gcc -std=c++11 -Werror -Wno-write-strings -Wconversion -I%s -I%s -c %s -o %s"
      (in_test_directory "../runtime")
      (in_test_directory "../examples/cmake/pd-deps")
      file
      basename
  in
  if Sys.command cmd <> 0 then assert_failure ("Failed to compile " ^ file)


let compileCppFile ext (file : string) : unit =
  let output = Filename.chop_extension (Filename.basename file) in
  Sys.chdir tmp_dir;
  assert_bool "No code generated" (Sys.file_exists (output ^ ext));
  callCompiler (output ^ ext);
  callCompiler (in_test_directory "../runtime/vultin.cpp");
  Sys.chdir initial_dir


module CliTest = struct
  let checkJsFile (file : string) : unit =
    let output = Filename.chop_extension (Filename.basename file) in
    Sys.chdir tmp_dir;
    assert_bool "No code generated" (Sys.file_exists (output ^ ".js"));
    let cmd = "node -c " ^ output ^ ".js" in
    if Sys.command cmd <> 0 then assert_failure ("Failed to check " ^ file);
    Sys.chdir initial_dir


  let checkLuaFile (file : string) : unit =
    let output = Filename.chop_extension (Filename.basename file) in
    Sys.chdir tmp_dir;
    assert_bool "No code generated" (Sys.file_exists (output ^ ".lua"));
    let cmd = "luajit -bl " ^ output ^ ".lua > " ^ output ^ ".b" in
    if Sys.command cmd <> 0 then assert_failure ("Failed to check " ^ file);
    Sys.remove (output ^ ".b");
    Sys.chdir initial_dir


  let getFlags code_type =
    match code_type with
    | "fixed" -> "-code cpp -real fixed", [ ".cpp", ".cpp.fixed.base"; ".h", ".h.fixed.base" ]
    | "float" -> "-code cpp", [ ".cpp", ".cpp.float.base"; ".h", ".h.float.base" ]
    | "c" -> "-code c", [ ".c", ".c.float.base"; ".h", ".h.float.base" ]
    | "js" -> "-code js", [ ".js", ".js.base" ]
    | "lua" -> "-code lua", [ ".lua", ".lua.base" ]
    | "java" -> "-code java -prefix vult.com", [ ".java", ".java.base" ]
    | _ -> failwith "Unknown target to run test"


  let callVultCli (compiler : compiler) (fullfile : string) code_type =
    let basefile = in_tmp_dir @@ Filename.chop_extension (Filename.basename fullfile) in
    let flags, ext = getFlags code_type in
    let includes_flags = List.map (fun a -> "-i " ^ a) includes |> String.concat " " in
    let flags = flags ^ " " ^ includes_flags in
    let vultc = if compiler = Node then "node ./vult.js" else "./_build/default/src/vult.exe" in
    let cmd = vultc ^ " -test " ^ flags ^ " -o " ^ basefile ^ " " ^ fullfile in
    let generated_files =
      match Sys.command cmd with
      | 0 -> List.map (fun e -> basefile ^ fst e, basefile ^ snd e) ext
      | _ -> assert_failure "failed to call the compiler"
      | exception _ -> assert_failure "failed to call the compiler in a bad way"
    in
    let () =
      match code_type with
      | "fixed" | "float" -> compileCppFile ".cpp" fullfile
      | "c" -> compileCppFile ".c" fullfile
      | "js" -> if has_node then checkJsFile fullfile
      | "lua" -> if has_lua then checkLuaFile fullfile
      | _ -> ()
    in
    generated_files


  let callVultInternal (_ : compiler) (fullfile : string) code_type =
    let basefile = in_tmp_dir @@ Filename.chop_extension (Filename.basename fullfile) in
    let args = Args.{ default_arguments with includes } in
    let args, ext =
      match code_type with
      | "fixed" -> { args with code = CppCode; real = Fixed }, [ ".cpp", ".cpp.fixed.base"; ".h", ".h.fixed.base" ]
      | "float" -> { args with code = CppCode }, [ ".cpp", ".cpp.float.base"; ".h", ".h.float.base" ]
      | "js" -> { args with code = JSCode }, [ ".js", ".js.base" ]
      | "lua" -> { args with code = LuaCode }, [ ".lua", ".lua.base" ]
      | "java" -> { args with code = JavaCode; prefix = Some "vult.com" }, [ ".java", ".java.base" ]
      | _ -> failwith "Unknown target to run test"
    in
    let args = { args with output = Some basefile; files = [ File fullfile ] } in
    let results = Driver.Cli.driver args in
    let () = List.iter (Driver.Cli.showResult args) results in
    let generated_files = List.map (fun e -> basefile ^ fst e, basefile ^ snd e) ext in
    generated_files


  let callVult context (compiler : compiler) (fullfile : string) code_type =
    if internal_test context then
      callVultInternal compiler fullfile code_type
    else
      callVultCli compiler fullfile code_type


  let process context (compiler : compiler) (fullfile : string) code_type =
    try callVult context compiler fullfile code_type with
    | Error.Errors errors ->
      let msg = Error.reportErrors errors in
      assert_failure msg


  let run (file : string) use_node real_type context : unit =
    Sys.chdir initial_dir;
    let fullfile = checkFile (in_test_directory ("../examples/" ^ file)) in
    let generated_files = process context use_node fullfile real_type in
    let files_content =
      List.map (readOutputAndReference (update_test context) (in_test_directory "code")) generated_files
    in
    assert_bool "No code generated" (files_content <> []);
    List.iter
      (fun (current, reference) ->
        assert_equal
          ~cmp:Diff.compare
          ~msg:("Generating file " ^ fullfile)
          ~pp_diff:(fun ff (a, b) -> Format.fprintf ff "\n%s" (Diff.lineDiff a b))
          reference
          current)
      files_content


  let get files use_node real_type =
    "cli" >::: List.map (fun file -> Filename.basename file ^ "." ^ real_type >:: run file use_node real_type) files
end

module Templates = struct
  let tryCompile (args : Args.args) generated_files =
    match args.template, args.code, generated_files with
    | Some "pd", CppCode, (filename, _) :: _ -> compileCppFile ".cpp" filename
    | _ -> ()


  let callVult template (fullfile : string) code_type =
    let basefile = Filename.chop_extension (Filename.basename fullfile) in
    let moduleName = String.capitalize_ascii basefile in
    let output = in_tmp_dir basefile in
    let args = Args.{ default_arguments with includes; roots = [ moduleName ^ ".process" ] } in
    let args, ext =
      match code_type with
      | "fixed" ->
        ( { args with template = Some template; code = CppCode; real = Fixed }
        , [ ".cpp", ".cpp.fixed.base." ^ template; ".h", ".h.fixed.base." ^ template ] )
      | "float" ->
        ( { args with template = Some template; code = CppCode }
        , [ ".cpp", ".cpp.float.base." ^ template; ".h", ".h.float.base." ^ template ] )
      | "java" ->
        ( { args with template = Some template; code = JavaCode; prefix = Some "vult.com" }
        , [ ".java", ".java.base." ^ template ] )
      | "js" -> { args with template = Some template; code = JSCode }, [ ".js", ".js.base." ^ template ]
      | "lua" -> { args with template = Some template; code = LuaCode }, [ ".lua", ".lua.base." ^ template ]
      | _ -> failwith "Unknown target to run test"
    in
    let args = { args with output = Some output; files = [ File fullfile ] } in
    let results = Driver.Cli.driver args in
    let () = List.iter (Driver.Cli.showResult args) results in
    let generated_files = List.map (fun e -> output ^ fst e, output ^ snd e) ext in
    let () = tryCompile args generated_files in
    generated_files


  let process _context template (fullfile : string) code_type =
    try callVult template fullfile code_type with
    | Error.Errors errors ->
      let msg = Error.reportErrors errors in
      assert_failure msg


  let run (file : string) template real_type context : unit =
    Sys.chdir initial_dir;
    let fullfile = checkFile (in_test_directory ("templates/" ^ file)) in
    let generated_files = process context template fullfile real_type in
    let files_content =
      List.map (readOutputAndReference (update_test context) (in_test_directory "code")) generated_files
    in
    assert_bool "No code generated" (files_content <> []);
    List.iter
      (fun (current, reference) ->
        assert_equal
          ~cmp:Diff.compare
          ~msg:("Generating file " ^ fullfile)
          ~pp_diff:(fun ff (a, b) -> Format.fprintf ff "\n%s" (Diff.lineDiff a b))
          reference
          current)
      files_content


  let get files template real_type =
    "template"
    >::: List.map
           (fun file -> Filename.basename file ^ "." ^ real_type ^ "_" ^ template >:: run file template real_type)
           files
end

module Interpret = struct
  let run file _context =
    let fullfile = checkFile (in_test_directory ("perf/" ^ file)) in
    let moduleName file = Filename.basename file |> Filename.chop_extension |> String.capitalize_ascii in
    let module_name = moduleName fullfile in
    let code =
      [%pla
        {|fun run() {
   val sample = 0;
   val out = 0.0;
   while (sample < 100) {
      out = <#module_name#s>.process(0.0);
      sample = sample + 1;
   }
   return out;
}|}]
      |> Pla.print
    in
    let args =
      Args.
        { default_arguments with
          files = [ Code ("intepret.vult", code) ]
        ; eval = Some "Intepret.run()"
        ; includes = in_test_directory "perf" :: includes
        }
    in
    let results = Driver.Cli.driver args in
    List.iter
      (fun result ->
        match result with
        | Args.Errors errors -> assert_failure (Error.reportErrors errors)
        | _ -> ())
      results


  let get files = "run" >::: List.map (fun file -> Filename.basename file >:: run file) files
end

let suite =
  "vult"
  >::: [ ErrorTest.get errors_files
       ; ParserTest.get parser_files
       ; PassesTest.get passes_files
       ; Templates.get template_files "pd" "float"
         (* ; Templates.get template_files "pd" "fixed"
            ; Templates.get template_files "max" "float"
            ; Templates.get template_files "max" "fixed"
            ; Templates.get template_files "modelica" "float"
            ; Templates.get template_files "modelica" "fixed"
            ; Templates.get template_files "teensy" "fixed"
            ; Templates.get template_files "webaudio" "js"
            ; Templates.get template_files "browser" "js"*)
       ; CliTest.get all_files Native "float" (*; CliTest.get all_files Native "c"*)
       ; CliTest.get all_files Native "fixed" (* ; CliTest.get all_files Native "js"*)
       ; CliTest.get all_files Native "lua"
       ; CliTest.get all_files Node "float"
         (* ; CliTest.get all_files Native "java"
            ; CliTest.get all_files Node "fixed"
            ; CliTest.get all_files Node "js" *)
       ; CliTest.get all_files Node "lua"
       ; RandomCompileTest.get test_random_code
       ; Interpret.get perf_files
       ]


let () =
  let _ = run_test_tt_main suite in
  ()
