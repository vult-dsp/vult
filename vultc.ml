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
open ParserVult
open TypesVult
open Passes

(** Returns a 'arguments' type containing the options passed in the command line *)
let processArguments () : arguments =
   let result =
      {
         files     = [];
         dparse    = false;
         run_check = false;
         rundyn    = false;
         debug     = false;
         ccode     = false;
         jscode    = false;
         output    = "";
         real      = "float"
      } in
   let opts = [
      "-dparse", (Arg.Unit   (fun () -> result.dparse   <-true)), "Dumps the parse tree (default: off)";
      "-check",  (Arg.Unit   (fun () -> result.run_check<-true)), "Runs checker on program (default: off)";
      "-rundyn", (Arg.Unit   (fun () -> result.rundyn   <-true)), "Runs the dynamic interpreter (default: off)";
      "-debug",  (Arg.Unit   (fun () -> result.debug    <-true)), "Runs the debugger (default: off)";
      "-ccode",  (Arg.Unit   (fun () -> result.ccode    <-true)), "Converts the code to c (default: off)";
      "-jscode", (Arg.Unit   (fun () -> result.jscode   <-true)), "Converts the code to javascript (default: off)";
      "-o",      (Arg.String (fun output -> result.output<-output)), "Defines the prefix of the output files";
      "-real",   (Arg.String (fun real -> result.real<-real)), "Defines the numeric type for the generated code (float,double,fixed)";
   ]
   in
   let _ = Arg.parse opts (fun a -> result.files <- a::result.files) "Usage: vultc file.vult\n" in
   let _ = result.files <- List.rev result.files in (* Put the files in the correct order  *)
   result

(** Prints the parsed files if -dparse was passed as argument *)
let dumpParsedFiles (parser_results:parser_results list) =
   parser_results
   |> List.iter (
      fun a -> match a.presult with
         | `Ok(b) ->
            let _ = print_endline "\n= Transformed Code =" in
            let _ = PrintTypes.stmtListStr b |> print_string in
            let _ = print_endline "\n= JS Code =" in
            let _ = VultJs.generateJsCode b |> print_string in
            ()
            (*List.iter (fun stmt -> print_endline (show_stmt stmt)) b;*)
         | _ -> () )

let main () =
   let args = processArguments () in
   (* Parse the files *)
   let parser_results =
      List.map parseFile args.files
   in
   (* Reports error of parsing *)
   let _ = List.iter (fun a -> Error.printErrors a.presult a.lines ) parser_results in
   (* Prints the parsed files if -dparse was passed as argument *)
   let _ = if args.dparse then
      dumpParsedFiles (List.map applyTransformations parser_results) in
   ()
;;
main ();;
