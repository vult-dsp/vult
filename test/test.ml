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
      "exp_tuples.vult";
      "types_basic.vult";
   ]

let pass1_options = { Passes.default_options with Passes.pass2 = false }

let passes_files =
   [
      "split_mem.vult", pass1_options;
      "tuple_assign.vult", pass1_options;
      "context_simple.vult", Passes.default_options;
      "context_nested.vult", Passes.default_options;
      "tuple_to_types.vult", Passes.default_options;
   ]


let code_files =
   [
      "adsr.vult";
      "blit.vult";
      "filters.vult";
      "monoin.vult";
      "moog_filter.vult";
      "state_variable.vult";
      "voice.vult";
      "sin.vult";
      "web/phasedist.vult";
      "web/synth1.vult";
      "web/template.vult";
      "web/volume.vult";
   ]

(** Flags that defines if a baseline should be created for tests *)
let writeOutput = Conf.make_bool "writeout" false "Creates a file with the current results"

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
         let oc = open_out ref_file in
         Printf.fprintf oc "%s" contents;
         close_out oc;
         contents
      else
         assert_failure (Printf.sprintf "The file '%s' has no reference data" file)

(** Asserts if the file does not exists *)
let checkFile (filename:string) : string =
   if Sys.file_exists filename then filename
   else
      assert_failure (Printf.sprintf "The file '%s' does not exits" filename)

let showResults result : string =
   match result.presult with
   | `Ok(b) -> PrintTypes.stmtListStr b
   | `Error(_) ->
      let error_strings = Error.reportErrors result.presult result.lines in
      let result = List.fold_left (fun s a -> s^"\n"^a) "" error_strings in
      assert_failure ("Errors in the program:\n"^result)

(** Module to perform parsing tests *)
module ParserTest = struct

   let process (fullfile:string) : string =
      ParserVult.parseFile fullfile
      |> showResults

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
      let stmts =
         ParserVult.parseFile fullfile
         |> Passes.applyTransformations
      in
      let () = showResults stmts |> ignore in
      if real_type = "js" then
         VultJs.generateJSCode  { default_arguments with output = basefile } [stmts]
      else
         VultCh.generateChCode { default_arguments with output = basefile; real = real_type } [stmts]


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
      ParserVult.parseFile fullfile
      |> Passes.applyTransformations ~options:options
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
      let cmd = Printf.sprintf "gcc -I%s -c %s -o %s" (in_test_directory "../runtime") file basename in
      if Sys.command cmd <> 0 then
         assert_failure ("Failed to compile "^file)

   let generateCPP (filename:string) (output:string) (real_type:string) : unit =
      let parser_results =
         ParserVult.parseFile filename
         |> Passes.applyTransformations in
      let args = { default_arguments with  files = [filename]; ccode = true; output = output; real = real_type } in
      Driver.generateCode args [parser_results] |> ignore

   let run (real_type:string) (file:string) _ =
      let fullfile = checkFile (in_test_directory ("../examples/"^file)) in
      let output   = Filename.chop_extension (Filename.basename fullfile) in
      Sys.chdir tmp_dir;
      generateCPP fullfile output real_type;
      compileFile (output^".cpp");
      compileFile (in_test_directory "../runtime/vultin.c");
      Sys.chdir initial_dir

   let get files real_type = "compile">::: (List.map (fun file -> (Filename.basename file) ^"."^ real_type >:: run real_type file) files)

end

let suite =
   "vult">:::
   [
      ParserTest.get  parser_files;
      PassesTest.get  passes_files;
      CodeGenerationTest.get code_files "float";
      CodeGenerationTest.get code_files "fixed";
      CodeGenerationTest.get code_files "js";
      CompileTest.get code_files "float";
      CompileTest.get code_files "fixed";
   ]


let () =
   let _ = run_test_tt_main suite in
   ()
;;