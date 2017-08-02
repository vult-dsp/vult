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
open OUnit2
open Prog
open Args
open Tcommon
;;

Float.reduce_precision := true;;


let test_directory = Filename.concat initial_dir "test"

let in_test_directory path = Filename.concat test_directory path

let in_tmp_dir path = Filename.concat tmp_dir path

(** checks if node can be called with flag -c *)
let has_node =
   if tryToRun ("node -c " ^ (in_test_directory "other/test.js")) then
      let () = print_endline "Js syntax will be checked" in
      true
   else
      let () = print_endline "Js syntax will not be checked" in
      false

(** checks if luajit can be called *)
let has_lua =
   if tryToRun ("luajit -bl " ^ (in_test_directory "other/test.lua") ^ " > out") then
      let () = print_endline "Lua syntax will be checked" in
      true
   else
      let () = print_endline "Lua syntax will not be checked" in
      false


let parser_files =
   [
      "stmt_val_mem.vult";
      "stmt_functions.vult";
      "stmt_types.vult";
      "stmt_external.vult";
      "stmt_misc.vult";
      "exp_basic.vult";
      "exp_misc.vult";
      "exp_precedence.vult";
      "exp_array.vult";
      "exp_tuples.vult";
      "types_basic.vult";
   ]

let errors_files =
   [
      "error1.vult";
      "error2.vult";
      "error3.vult";
      "error4.vult";
      "error5.vult";
      "error6.vult";
      "error7.vult";
      "error8.vult";
      "error9.vult";
      "error10.vult";
   ]


let no_context = PassCommon.{ default_options with pass4 = false; pass3 = false; pass2 = false; }
let no_eval = PassCommon.{ default_options with pass2 = false; }

let passes_files =
   [
      "split_mem.vult", no_context;
      "tuple_assign.vult", no_context;
      "if_to_stmt.vult", no_context;
      "context_simple.vult", no_eval;
      "context_nested.vult", no_eval;
      "tuple_to_types.vult", no_context;
      "simplify.vult", no_context;
      "external_calls.vult", no_eval;
      "output_references.vult", no_eval;
   ]

let all_files =
   [
      "web/phasedist.vult";
      "web/synth1.vult";
      "web/synth2.vult";
      "web/template.vult";
      "web/volume.vult";
      "web/delay.vult";
      "../test/bench/bench.vult";
      "../test/passes/wav_file.vult";
      "../test/other/log.vult";

      "effects/bitcrush.vult";
      "effects/short_delay.vult";
      "effects/saturate_soft.vult";
      "effects/saturate.vult";
      "effects/rescomb.vult";

      "env/ad.vult";
      "env/adsr.vult";
      "env/ahr.vult";
      "env/lfo.vult";
      "env/swept.vult";

      "filters/ladder.vult";
      "filters/svf.vult";

      "midi/gates.vult";
      "midi/monocv.vult";
      "midi/polycv.vult";

      "osc/blit.vult";
      "osc/noise.vult";
      "osc/phd.vult";
      "osc/saw_eptr.vult";
      "osc/saw_ptr1.vult";
      "osc/saw_ptr2.vult";
      "osc/saw_r.vult";
      "osc/phase.vult";
      "osc/sawcore.vult";
      "osc/sine.vult";
      "osc/tricore.vult";

      "units/kick.vult";
      "units/voice_4.vult";

   ]

let includes =
   [
      "effects";
      "env";
      "filters";
      "midi";
      "osc";
      "unit";
      "util";
   ]
   |> List.map (fun dir -> in_test_directory ("../examples/"^dir))

let test_random_code =
   let rec loop n =
      if n > 0 then
         (Printf.sprintf "test%i.vult" n) :: loop (n-1)
      else []
   in loop 10

(** Flags that defines if a baseline should be created for tests *)
let update_test = Conf.make_bool "update" false "Creates a file with the current results"
(** Flags that defines if we should use the command line version of the compiler *)
let coverage_test = Conf.make_bool "coverage" false "Uses static linked compiler instead of the command line"

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
   s

let readOutputAndReference (create:bool) (outdir:string) (output, reference) =
   let output_txt =
      if Sys.file_exists output then
         let contents = read output in
         let () = Sys.remove output in
         contents
      else
         assert_failure (Printf.sprintf "The file '%s' was not generated" output)
   in
   let reference = Filename.concat outdir (Filename.basename reference) in
   let reference_txt =
      if Sys.file_exists reference then
         read reference
      else
      if create then
         let () = write reference output_txt in
         output_txt
      else
         assert_failure (Printf.sprintf "The file '%s' has no reference data" reference)
   in
   output_txt, reference_txt

(** Returns the contents of the reference file for the given vult file *)
let readReference (create:bool) (ext:string) (contents:string) (file:string) (outdir:string) : string =
   let basefile = Filename.chop_extension (Filename.basename file) in
   let ref_file = Filename.concat outdir (basefile^"."^ext) in
   if Sys.file_exists ref_file then
      read ref_file
   else
   if create then
      let () = write ref_file contents in
      contents
   else
      assert_failure (Printf.sprintf "The file '%s' has no reference data" file)

(** Asserts if the file does not exists *)
let checkFile (filename:string) : string =
   if Sys.file_exists filename then filename
   else
      assert_failure (Printf.sprintf "The file '%s' does not exits" filename)

let showResults result : string =
   PrintProg.stmtListStr result.presult

(** Module to perform parsing tests *)
module ParserTest = struct

   let process (fullfile:string) : string =
      Parser.parseFile fullfile
      |> fun a -> showResults a

   let run (file:string) context =
      let folder = "parser" in
      let fullfile  = checkFile (in_test_directory (folder^"/"^file)) in
      let current   = process fullfile in
      let reference = readReference (update_test context) "base" current fullfile (in_test_directory folder) in
      assert_equal
         ~msg:("Parsing file "^fullfile)
         ~pp_diff:(fun ff (a,b) -> Format.fprintf ff "\n%s" (Diff.lineDiff a b) )
         reference current

   let get files = "parser">::: (List.map (fun file -> (Filename.basename file) >:: run file) files)

end

module ErrorTest = struct

   let process (fullfile:string) =
      let basefile = in_tmp_dir @@ Filename.chop_extension (Filename.basename fullfile) in
      let args = { default_arguments with includes = includes } in
      let args = { args with output = basefile; files = [ File fullfile ] } in
      let results = Driver.main args in
      List.fold_left
         (fun s a ->
             match a with
             | Errors e ->
                (List.map Error.reportErrorString e) @ s
             | _ -> s)
         []
         results
      |> String.concat "\n"

   let run (file:string) context =
      let folder = "errors" in
      let fullfile  = checkFile (in_test_directory (folder^"/"^file)) in
      let current   = process fullfile in
      let reference = readReference (update_test context) "base" current fullfile (in_test_directory folder) in
      assert_equal
         ~msg:("Error mismatch in file "^fullfile)
         ~pp_diff:(fun ff (a,b) -> Format.fprintf ff "\n%s" (Diff.lineDiff a b) )
         reference current

   let get files = "errors">::: (List.map (fun file -> (Filename.basename file) >:: run file) files)

end

(** Module to perform transformation tests *)
module PassesTest = struct

   let process options (fullfile:string) : string =
      let args = { default_arguments with ccode = true } in
      Parser.parseFile fullfile
      |> Passes.applyTransformationsSingle args ~options:options
      |> showResults

   let run options (file:string) context =
      let folder = "passes" in
      let fullfile  = checkFile (in_test_directory (folder^"/"^file)) in
      let current   = process options fullfile in
      let reference = readReference (update_test context) "base" current fullfile (in_test_directory folder) in
      assert_equal
         ~msg:("Transforming file "^fullfile)
         ~pp_diff:(fun fmt (a,b) -> Format.fprintf fmt "\n%s" (Diff.lineDiff a b) )
         reference current

   let get files = "passes">::: (List.map (fun (file,options) -> (Filename.basename file) >:: run options file) files)

end

(** Tries to compile all the examples *)
module RandomCompileTest = struct

   let compileFile (file:string) =
      let basename = Filename.chop_extension (Filename.basename file) in
      let cmd = Printf.sprintf "gcc -Wno-return-type -Wno-div-by-zero -Wno-narrowing -Wno-constant-logical-operand -Wno-division-by-zero -Wno-unused-value -Wno-tautological-compare -I%s -c %s -o %s" (in_test_directory "../runtime") file basename in
      if Sys.command cmd <> 0 then
         assert_failure ("Failed to compile "^file)

   let generateCPP (filename:string) (output:string) (real_type:string) : unit =
      let args = { default_arguments with  files = [File filename]; ccode = true; output = output; real = real_type } in
      let seed = Hashtbl.hash filename in
      let code = RandProg.run seed in
      write filename code;
      let parser_results = Parser.parseString (Some(filename)) code in
      let gen = Generate.generateCode [parser_results] args in
      writeFiles args gen

   let run (real_type:string) (file:string) _ =
      let output   = Filename.chop_extension (Filename.basename file) in
      Sys.chdir tmp_dir;
      generateCPP file output real_type;
      assert_bool "No code generated" (Sys.file_exists (output^".cpp"));
      compileFile (output^".cpp");
      Sys.remove (output^".cpp");
      Sys.remove (output^".h");
      Sys.remove (output^".vult");
      Sys.remove (output);
      Sys.chdir initial_dir

   let get files real_type = "compile">::: (List.map (fun file -> (Filename.basename file) ^"."^ real_type >:: run real_type file) files)

end

(** Compiles the benchmark *)
module BenchTest = struct

   let compileFile (file:string) =
      let basename = Filename.chop_extension (Filename.basename file) in
      let cmd = Printf.sprintf "gcc -Werror -I. -I%s -O3 -c %s -o %s.o" (in_test_directory "../runtime") file basename in
      if Sys.command cmd <> 0 then
         assert_failure ("Failed to compile "^file)

   let linkFiles (output:string) (files:string list) =
      let lflags = if os = "Linux" then "-lm" else "" in
      let cmd = Printf.sprintf "gcc -o %s %s %s" output (String.concat " " files) lflags in
      if Sys.command cmd <> 0 then
         assert_failure ("Failed to link ")

   let generateC (filename:string) (output:string) (real_type:string) : unit =
      let args = { default_arguments with files = [File filename]; ccode = true; output = output; real = real_type } in
      let parser_results = Parser.parseFile filename  in
      let gen = Generate.generateCode [parser_results] args in
      writeFiles args gen

   let generateJs (filename:string) (output:string) : unit =
      let args = { default_arguments with files = [File filename]; template = "node"; jscode = true; output = output } in
      let parser_results = Parser.parseFile filename  in
      let gen = Generate.generateCode [parser_results] args in
      writeFiles args gen

   let generateLua (filename:string) (output:string) : unit =
      let args = { default_arguments with files = [File filename]; luacode = true; output = output } in
      let parser_results = Parser.parseFile filename  in
      let gen = Generate.generateCode [parser_results] args in
      writeFiles args gen

   let run real_type _ =
      let vultfile = checkFile (in_test_directory ("../test/bench/bench.vult")) in
      let output   = Filename.chop_extension (Filename.basename vultfile) in
      Sys.chdir tmp_dir;
      generateC vultfile output real_type;
      assert_bool "No code generated" (Sys.file_exists (output^".cpp"));
      compileFile (output^".cpp");
      compileFile (in_test_directory "../runtime/vultin.c");
      compileFile (in_test_directory "../test/bench/main.cpp");
      linkFiles ("bench_"^real_type) ["vultin.o";"bench.o";"main.o"];
      print_endline ("### Real numbers: "^real_type ^ "");
      ignore (Sys.command ("./bench_"^real_type));
      Sys.remove (output^".cpp");
      Sys.remove (output^".h");
      Sys.chdir initial_dir

   let runJsLua _ =
      let vultfile = checkFile (in_test_directory ("../test/bench/bench.vult")) in
      let output   = Filename.chop_extension (Filename.basename vultfile) in

      Sys.chdir (in_test_directory "bench");
      generateJs vultfile output;
      generateLua vultfile output;
      if has_node then ignore (Sys.command "node main.js");
      if has_lua then ignore (Sys.command "luajit main.lua");
      Sys.chdir initial_dir


   let get = "bench">::: (["bench_float" >:: run "float"; "bench_fixed" >:: run "fixed"; "js_lua" >:: runJsLua])

end

type compiler =
   | Node
   | Native

module CliTest = struct

   let callCompiler (file:string) : unit =
      let basename = Filename.chop_extension (Filename.basename file) in
      let cmd = Printf.sprintf "gcc -Werror -Wno-write-strings -I%s -c %s -o %s" (in_test_directory "../runtime") file basename in
      if Sys.command cmd <> 0 then
         assert_failure ("Failed to compile "^file)

   let compileCppFile (file:string) : unit =
      let output = Filename.chop_extension (Filename.basename file) in
      Sys.chdir tmp_dir;
      assert_bool "No code generated" (Sys.file_exists (output^".cpp"));
      callCompiler (output^".cpp");
      callCompiler (in_test_directory "../runtime/vultin.c");
      Sys.chdir initial_dir

   let checkJsFile (file:string) : unit =
      let output = Filename.chop_extension (Filename.basename file) in
      Sys.chdir tmp_dir;
      assert_bool "No code generated" (Sys.file_exists (output^".js"));
      let cmd = "node -c " ^ output ^ ".js" in
      if Sys.command cmd <> 0 then
         assert_failure ("Failed to check "^file);
      Sys.chdir initial_dir

   let checkLuaFile (file:string) : unit =
      let output = Filename.chop_extension (Filename.basename file) in
      Sys.chdir tmp_dir;
      assert_bool "No code generated" (Sys.file_exists (output^".lua"));
      let cmd = "luajit -bl " ^ output ^ ".lua > " ^ output ^ ".b" in
      if Sys.command cmd <> 0 then
         assert_failure ("Failed to check "^file);
      Sys.remove (output ^ ".b");
      Sys.chdir initial_dir

   let callVultCli (compiler:compiler) (fullfile:string) code_type =
      let basefile = in_tmp_dir @@ Filename.chop_extension (Filename.basename fullfile) in
      let flags, ext =
         match code_type with
         | "fixed" -> "-ccode -real fixed", [".cpp",".cpp.fixed.base"; ".h", ".h.fixed.base"]
         | "float" -> "-ccode", [".cpp",".cpp.float.base"; ".h", ".h.float.base"]
         | "js" -> "-jscode", [".js", ".js.base"]
         | "lua" -> "-luacode", [".lua", ".lua.base"]
         | _ -> failwith "Unknown target to run test"
      in
      let includes_flags = List.map (fun a -> "-i "^a) includes |> String.concat " " in
      let flags = flags ^ " " ^ includes_flags in
      let vultc = if compiler = Node then "node ./vultc.js" else "./vultc.native" in
      let cmd = vultc ^ " -test " ^ flags ^ " -o " ^ basefile ^ " " ^ fullfile in
      let generated_files =
         match Sys.command cmd with
         | 0 -> List.map (fun e -> basefile ^ (fst e), basefile ^ (snd e)) ext
         | _ -> assert_failure "failed to call the compiler"
         | exception _ -> assert_failure "failed to call the compiler in a bad way"
      in
      let () =
         match code_type with
         | "fixed" | "float" -> compileCppFile fullfile
         | "js" -> if has_node then checkJsFile fullfile
         | "lua" -> if has_lua then checkLuaFile fullfile
         | _ -> ()
      in
      generated_files

   let callVultInternal (_:compiler) (fullfile:string) code_type =
      let basefile = in_tmp_dir @@ Filename.chop_extension (Filename.basename fullfile) in
      let args = { default_arguments with includes = includes } in
      let args, ext =
         match code_type with
         | "fixed" -> { args with ccode = true; real = "fixed" }, [".cpp",".cpp.fixed.base"; ".h", ".h.fixed.base"]
         | "float" -> { args with ccode = true }, [".cpp",".cpp.float.base"; ".h", ".h.float.base"]
         | "js" -> { args with jscode = true }, [".js", ".js.base"]
         | "lua" -> { args with luacode = true }, [".lua", ".lua.base"]
         | _ -> failwith "Unknown target to run test"
      in
      let args = { args with output = basefile; files = [ File fullfile ] } in
      let results = Driver.main args in
      let () = List.iter (Cli.showResult args) results in
      let generated_files =  List.map (fun e -> basefile ^ (fst e), basefile ^ (snd e)) ext in
      generated_files

   let callVult context (compiler:compiler) (fullfile:string) code_type =
      if coverage_test context then
         callVultInternal compiler fullfile code_type
      else
         callVultCli compiler fullfile code_type

   let process context (compiler:compiler) (fullfile:string) code_type =
      try
         callVult context compiler fullfile code_type
      with
      | Error.Errors errors ->
         let msg = Error.reportErrors errors in
         assert_failure msg


   let run (file:string) use_node real_type context : unit =
      Sys.chdir initial_dir;
      let fullfile = checkFile (in_test_directory ("../examples/"^file)) in
      let generated_files = process context use_node fullfile real_type in
      let files_content =
         List.map (readOutputAndReference (update_test context) (in_test_directory "code")) generated_files
      in
      assert_bool "No code generated" (files_content <> []);
      List.iter
         (fun (current, reference) ->
             assert_equal
                ~msg:("Generating file "^fullfile)
                ~pp_diff:(fun ff (a,b) -> Format.fprintf ff "\n%s" (Diff.lineDiff a b) )
                reference current
         )
         files_content

   let get files use_node real_type =
      "code">::: (List.map (fun file -> (Filename.basename file) ^ "."^ real_type >:: run file use_node real_type) files)

end

let suite =
   "vult">:::
   [
      ErrorTest.get errors_files;
      ParserTest.get  parser_files;
      PassesTest.get  passes_files;
      CliTest.get all_files Native "float";
      CliTest.get all_files Native "fixed";
      CliTest.get all_files Native "js";
      CliTest.get all_files Native "lua";
      CliTest.get all_files Node "float";
      CliTest.get all_files Node "fixed";
      CliTest.get all_files Node "js";
      CliTest.get all_files Node "lua";
      RandomCompileTest.get test_random_code "float";
      BenchTest.get;
   ]


let () =
   let _ = run_test_tt_main suite in
   ()
;;
