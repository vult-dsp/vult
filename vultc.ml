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
open LexerVult
open ParserVult
open TypesVult
open TypesUtil
open CheckerVult
open Passes
open DynInterpreter
open Debugger
open ErrorsVult

(** Stores the options passed to the command line *)
type arguments =
   {
      mutable files  : string list;
      mutable dparse : bool;
      mutable rundyn : bool;
      mutable debug  : bool;
      mutable run_check  : bool;
   }

(** Returns a 'arguments' type containing the options passed in the command line *)
let processArguments () : arguments =
   let result = { files = [] ; dparse = false; run_check = false; rundyn = false; debug = false } in
   let opts = [
      "-dparse", (Arg.Unit  (fun () -> result.dparse   <-true)), "Dumps the parse tree (default: off)";
      "-check",  (Arg.Unit  (fun () -> result.run_check<-true)), "Runs checker on program (default: off)";
      "-rundyn", (Arg.Unit  (fun () -> result.rundyn   <-true)), "Runs the dynamic interpreter (default: off)";
      "-debug",  (Arg.Unit  (fun () -> result.debug   <-true)), "Runs the debugger (default: off)";
   ]
   in
   let _ = Arg.parse opts (fun a -> result.files <- a::result.files) "Usage: vultc file.vlt\n" in
   let _ = result.files <- List.rev result.files in (* Put the files in the correct order  *)
   result

let main () =
   let args = processArguments () in
   (* Parse the files *)
   let parser_results =
      List.map parseFile args.files
   in
   (* Reports error of parsing *)
   let _ = List.iter (fun a -> ErrorsVult.printErrors a.presult a.lines ) parser_results in
   (* Prints the parsed files if -dparse was passed as argument *)
   let _ = if args.dparse then
         parser_results
         |> List.map (applyTransformations { opt_no_transform with inline_weight = 100000})
         |> List.iter (fun a -> match a.presult with `Ok(b) -> Format.printf "tree: %a@." pp_parse_exp_list b; PrintTypes.stmtListStr b |> print_string | _ -> () )
   in
   (* Runs the dynamic interpreter if -rundyn was passed as argument *)
   let _ = if args.rundyn then
         parser_results
         |> List.map (applyTransformations opt_full_transform)
         |> List.map interpret
         |> ignore
   in
   (* Runs the checker if -check was passed as argument *)
   let _ =
      if args.run_check then
         let errors = List.map programState parser_results in
         List.iter (fun a -> ErrorsVult.printErrors a.iresult a.lines ) errors
   in
   let _ =
      if args.debug then
         parser_results
         (*|> List.map (applyTransformations opt_simple_transform)
         |> List.map Debugger.debug*)
         |> ignore
   in
   ()
;;
main ();;
