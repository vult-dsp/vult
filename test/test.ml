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
open TypesVult

let initial_dir = Sys.getcwd ()

let test_directory = Filename.concat initial_dir "test"

let in_test_directory path = Filename.concat test_directory path

let tmp_dir = Filename.get_temp_dir_name ()

let in_tmp_dir path = Filename.concat tmp_dir path

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

let stand_alone_files =
   [
      "blit.vult";
      "voice.vult";
      "sin.vult";
      "web/phasedist.vult";
      "web/synth1.vult";
      "web/synth2.vult";
      "web/template.vult";
      "web/volume.vult";
      "web/delay.vult";
   ]

let partial_files =
   [
      "adsr.vult";
      "filters.vult";
      "monoin.vult";
      "moog_filter.vult";
      "state_variable.vult";
      "lib/math.vult";
      "lib/util.vult";
   ]

let all_files = stand_alone_files @ partial_files

let test_random_code =
   let rec loop n =
      if n > 0 then
         (Printf.sprintf "test%i.vult" n) :: loop (n-1)
      else []
   in loop 50

(** Flags that defines if a baseline should be created for tests *)
let writeOutput = Conf.make_bool "writeout" false "Creates a file with the current results"

let write file contents =
   let oc = open_out file in
   Printf.fprintf oc "%s" contents;
   close_out oc

(** Returns the contents of the reference file for the given vult file *)
let readReference (create:bool) (ext:string) (contents:string) (file:string) (outdir:string) : string =
   let basefile = Filename.chop_extension (Filename.basename file) in
   let ref_file = Filename.concat outdir (basefile^"."^ext) in
   if Sys.file_exists ref_file then
      let ic = open_in ref_file in
      let n = in_channel_length ic in
      let s = Bytes.create n in
      really_input ic s 0 n;
      close_in ic;
      s
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
   PrintTypes.stmtListStr result.presult

(** Module to perform parsing tests *)
module ParserTest = struct

   let process (fullfile:string) : string =
      ParserVult.parseFile fullfile
      |> fun a -> showResults a

   let run (file:string) context =
      let folder = "parser" in
      let fullfile  = checkFile (in_test_directory (folder^"/"^file)) in
      let current   = process fullfile in
      let reference = readReference (writeOutput context) "base" current fullfile (in_test_directory folder) in
      assert_equal
         ~msg:("Parsing file "^fullfile)
         ~pp_diff:(fun ff (a,b) -> Format.fprintf ff "\n%s" (Diff.lineDiff a b) )
         reference current

   let get files = "parser">::: (List.map (fun file -> (Filename.basename file) >:: run file) files)

end

(** Module to perform code generation tests *)
module CodeGenerationTest = struct

   let process (fullfile:string) (real_type:string) : (string * string) list =
      let basefile = Filename.chop_extension (Filename.basename fullfile) in
      let ccode = real_type = "fixed" || real_type = "float" in
      let jscode = real_type = "js" in
      let args  = { default_arguments with output = basefile; real = real_type; ccode; jscode } in
      let stmts = ParserVult.parseFile fullfile in
      let ()    = showResults stmts |> ignore in
      let files = Generate.generateCode [stmts] args in
      files |> List.map (fun (code,ext) -> Pla.print code, ext)

   let run (file:string) real_type context : unit =
      let fullfile = checkFile (in_test_directory ("../examples/"^file)) in
      let currents = process fullfile real_type in
      let folder = "code" in
      let base_ext ext =
         if real_type = "js" then
            ext^".base"
         else
            ext^"."^real_type^".base"
      in
      let references =
         List.map
            (fun (code,ext) ->
                readReference (writeOutput context) (base_ext ext) code fullfile (in_test_directory folder))
            currents
      in
      assert_bool "No code generated" (currents <> []);
      List.iter2
         (fun (current,ext) reference ->
             assert_equal
                ~msg:("Generating "^ext^" for file "^fullfile)
                ~pp_diff:(fun ff (a,b) -> Format.fprintf ff "\n%s" (Diff.lineDiff a b) )
                reference current
         )
         currents references

   let get files real_type =
      "code">::: (List.map (fun file -> (Filename.basename file) ^ "."^ real_type >:: run file real_type) files)

end

(** Module to perform transformation tests *)
module PassesTest = struct

   let process options (fullfile:string) : string =
      let args = { default_arguments with ccode = true } in
      ParserVult.parseFile fullfile
      |> Passes.applyTransformationsSingle args ~options:options
      |> showResults

   let run options (file:string) context =
      let folder = "passes" in
      let fullfile  = checkFile (in_test_directory (folder^"/"^file)) in
      let current   = process options fullfile in
      let reference = readReference (writeOutput context) "base" current fullfile (in_test_directory folder) in
      assert_equal
         ~msg:("Transforming file "^fullfile)
         ~pp_diff:(fun fmt (a,b) -> Format.fprintf fmt "\n%s" (Diff.lineDiff a b) )
         reference current

   let get files = "passes">::: (List.map (fun (file,options) -> (Filename.basename file) >:: run options file) files)

end

(** Tries to compile all the examples *)
module CompileTest = struct

   let compileFile (file:string) =
      let basename = Filename.chop_extension (Filename.basename file) in
      let cmd = Printf.sprintf "gcc -Werror -I%s -c %s -o %s" (in_test_directory "../runtime") file basename in
      if Sys.command cmd <> 0 then
         assert_failure ("Failed to compile "^file)

   let generateCPP (filename:string) (output:string) (real_type:string) : unit =
      let args = { default_arguments with  files = [filename]; ccode = true; output = output; real = real_type } in
      let parser_results = ParserVult.parseFile filename  in
      Driver.generateCode args [parser_results] |> ignore

   let run (real_type:string) (file:string) _ =
      let fullfile = checkFile (in_test_directory ("../examples/"^file)) in
      let output   = Filename.chop_extension (Filename.basename fullfile) in
      Sys.chdir tmp_dir;
      generateCPP fullfile output real_type;
      assert_bool "No code generated" (Sys.file_exists (output^".cpp"));
      compileFile (output^".cpp");
      compileFile (in_test_directory "../runtime/vultin.c");
      Sys.remove (output^".cpp");
      Sys.remove (output^".h");
      Sys.chdir initial_dir

   let get files real_type = "compile">::: (List.map (fun file -> (Filename.basename file) ^"."^ real_type >:: run real_type file) files)

end

(** Tries to compile all the examples *)
module RandomCompileTest = struct

   let compileFile (file:string) =
      let basename = Filename.chop_extension (Filename.basename file) in
      let cmd = Printf.sprintf "gcc -Wno-return-type -Wno-div-by-zero -Wno-narrowing -Wno-constant-logical-operand -Wno-division-by-zero -Wno-unused-value -Wno-tautological-compare -I%s -c %s -o %s" (in_test_directory "../runtime") file basename in
      if Sys.command cmd <> 0 then
         assert_failure ("Failed to compile "^file)

   let generateCPP (filename:string) (output:string) (real_type:string) : unit =
      let args = { default_arguments with  files = [filename]; ccode = true; output = output; real = real_type } in
      let seed = Hashtbl.hash filename in
      let code = RandProg.run seed in
      write filename code;
      let parser_results = ParserVult.parseString (Some(filename)) code in
      Driver.generateCode args [parser_results] |> ignore

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

let suite =
   "vult">:::
   [
      ParserTest.get  parser_files;
      PassesTest.get  passes_files;
      CodeGenerationTest.get all_files "float";
      CodeGenerationTest.get all_files "fixed";
      CodeGenerationTest.get stand_alone_files "js";
      CompileTest.get all_files "float";
      CompileTest.get all_files "fixed";
      RandomCompileTest.get test_random_code "float";
   ]


let () =
   let _ = run_test_tt_main suite in
   ()
;;
