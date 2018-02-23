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
open Args

(** Returns a 'arguments' type containing the options passed in the command line *)
let processArguments () : args =
   let result = { default_arguments with files = [] }  in
   let opts = [
      "-dparse",    (Arg.Unit   (fun () -> result.dparse   <-true)),      " Dumps the parse tree (default: off)";
      "-deps",      (Arg.Unit   (fun () -> result.deps     <-true)),      " Prints all file dependencies";
      "-ccode",     (Arg.Unit   (fun () -> result.code     <-CCode)),     " Converts the code to c (default: off)";
      "-check",     (Arg.Unit   (fun () -> result.check    <-true)),      " Checks the code without generating any code (default: off)";
      "-jscode",    (Arg.Unit   (fun () -> result.code     <-JSCode)),    " Converts the code to javascript (default: off)";
      "-luacode",   (Arg.Unit   (fun () -> result.code     <-LuaCode)),   " Converts the code to lua (default: off)";
      "-llvmcode",  (Arg.Unit   (fun () -> result.code     <-LLVMCode)),  " Converts the code to LLVM IR (default: off)";
      "-o",         (Arg.String (fun output -> result.output <- output)), "output Defines the prefix of the output files";
      "-real",      (Arg.String (fun real -> result.real   <- real)),     " Defines the numeric type for the generated code: double, fixed";
      "-samplerate",(Arg.Float  (fun fs -> result.fs <- Some fs)),         "number When set, the function samplerate() is evaluated";
      "-template",  (Arg.String (fun temp -> result.template <- temp)),   "name Defines the template used to generate code (ccode only): pd, teensy";
      "-eval",      (Arg.Unit   (fun () -> result.eval       <- true)),   " Runs the code (default: off)";
      "-i",         (Arg.String (fun path -> result.includes <- path :: result.includes)), "path Adds the given path to the list of places to look for modules";
      "-version",   (Arg.Unit   (fun () -> result.show_version <- true)), " Show the version of vult";
      "-test",      (Arg.Unit   (fun () -> Float.reduce_precision := true)), " Enters a special mode useful only for testing (default: off)";
   ]
      |> Arg.align
   in
   let _ = Arg.parse opts (fun a -> result.files <- File(a)::result.files) "Usage: vultc file.vult [options]\noptions:" in
   let _ = result.files <- List.rev result.files in (* Put the files in the correct order  *)
   result

let getFile (args:args) (ext:FileKind.t) : string =
   match ext with
   | FileKind.ExtOnly(e) -> args.output ^ "." ^ e
   | FileKind.FullName(n) -> Filename.concat (Filename.dirname args.output) n


let showResult (args:args) (output:output) =
   match output with
   | Version v -> print_endline v
   | Message v -> print_endline v
   | Dependencies deps -> String.concat " " deps |> print_endline
   | ParsedCode v -> print_endline v
   | GeneratedCode files when args.output <> "" ->
      List.iter (fun (text, file) -> FileIO.write (getFile args file) (Pla.print text) |> ignore) files
   | GeneratedCode files ->
      List.iter (fun (text, _) -> print_endline (Pla.print text)) files
   | Interpret v -> print_endline v
   | CheckOk -> ()
   | Errors errors ->
      let error_strings = Error.reportErrors errors in
      prerr_endline (error_strings)


let main () =
   let args = processArguments () in
   let results = Driver.main args in
   List.iter (showResult args) results;
   exit 0
