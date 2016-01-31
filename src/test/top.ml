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

let writeBase (file:string) (contents:string) : unit =
   let out_file = (Filename.chop_extension file)^".base" in
   let oc = open_out out_file in
   Printf.fprintf oc "%s" contents;
   close_out oc

let parseFile (file:string) : TypesVult.stmt list =
   let result = ParserVult.parseFile file in
   match result.presult with
   | `Ok(b) -> b
   | `Error(_) ->
      let error_strings:string list = Error.reportErrors result.presult result.lines in
      let result =List.fold_left (fun s a -> s^"\n"^a) "" error_strings in
      assert_failure ("Errors in the program:\n"^result)

module ParserTest = struct

   let process (fullfile:string) : string =
      parseFile fullfile
      |> PrintTypes.stmtListStr

   let run (file:string) context =
      let fullfile  = in_testdata_dir context ["parser";file] in
      let current   = process fullfile in
      let reference = readReference (writeOutput context) current fullfile in
      assert_equal
         ~msg:("Parsing file "^fullfile)
         ~printer:(fun a->a) reference current

   let get =
      let files =
         [
            "val_mem.vult"
         ]
      in
      "parser">::: (List.map (fun file -> (Filename.basename file) >:: run file) files)

end


let suite =
   "vult">:::
   [
      ParserTest.get;
   ]


let () =
   let _ = run_test_tt_main suite in
   ()
;;