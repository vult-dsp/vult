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

let pass1_files =
   [
      "split_mem.vult", pass1_options;
      "tuple_assign.vult", pass1_options;
   ]


(** Flags that defines if a baseline should be created for tests *)
let writeOutput = Conf.make_bool "writeout" false "Creates a file with the current results"

(** Returns the contents of the reference file for the given vult file *)
let readReference (create:bool) (contents:string) (file:string) : string =
   let ref_file = (Filename.chop_extension file)^".base" in
   if Sys.file_exists ref_file then
      let ic = open_in ref_file in
      let n = in_channel_length ic in
      let s = Bytes.create n in
      really_input ic s 0 n;
      close_in ic;
      s
   else
      if create then
         let out_file = (Filename.chop_extension file)^".base" in
         let oc = open_out out_file in
         Printf.fprintf oc "%s" contents;
         close_out oc;
         contents
      else
         assert_failure (Printf.sprintf "The file '%s' has no reference data" file)

(** Writes the contents of the baseline to the given file *)
let writeBase (file:string) (contents:string) : unit =
   let out_file = (Filename.chop_extension file)^".base" in
   let oc = open_out out_file in
   Printf.fprintf oc "%s" contents;
   close_out oc

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
      let fullfile  = checkFile (in_testdata_dir context ["parser";file]) in
      let current   = process fullfile in
      let reference = readReference (writeOutput context) current fullfile in
      assert_equal
         ~msg:("Parsing file "^fullfile)
         ~pp_diff:(fun ff (a,b) -> Format.fprintf ff "\n%s" (Diff.lineDiff a b) )
         reference current

   let get files = "parser">::: (List.map (fun file -> (Filename.basename file) >:: run file) files)

end


(** Module to perform transformation tests *)
module Pass1Test = struct

   let process options (fullfile:string) : string =
      ParserVult.parseFile fullfile
      |> Passes.applyTransformations ~options:options
      |> showResults

   let run options (file:string) context =
      let fullfile  = checkFile (in_testdata_dir context ["passes";file]) in
      let current   = process options fullfile in
      let reference = readReference (writeOutput context) current fullfile in
      assert_equal
         ~msg:("Transforming file "^fullfile)
         ~pp_diff:(fun fmt (a,b) -> Format.fprintf fmt "\n%s" (Diff.lineDiff a b) )
         reference current

   let get files = "passes">::: (List.map (fun (file,options) -> (Filename.basename file) >:: run options file) files)

end

let suite =
   "vult">:::
   [
      ParserTest.get parser_files;
      Pass1Test.get  pass1_files;
   ]


let () =
   let _ = run_test_tt_main suite in
   ()
;;