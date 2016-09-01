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

(** Returns a 'arguments' type containing the options passed in the command line *)
let processArguments () : arguments =
   let result = { default_arguments with files = [] }  in
   let opts = [
      "-dparse",   (Arg.Unit   (fun () -> result.dparse   <-true)), "Dumps the parse tree (default: off)";
      "-ccode",    (Arg.Unit   (fun () -> result.ccode    <-true)), "Converts the code to c (default: off)";
      "-jscode",   (Arg.Unit   (fun () -> result.jscode   <-true)), "Converts the code to javascript (default: off)";
      "-luacode",  (Arg.Unit   (fun () -> result.luacode  <-true)), "Converts the code to lua (default: off)";
      "-o",        (Arg.String (fun output -> result.output<-output)), "Defines the prefix of the output files";
      "-real",     (Arg.String (fun real -> result.real<-real)),       "Defines the numeric type for the generated code (float,double,fixed)";
      "-template", (Arg.String (fun temp -> result.template<-temp)),   "Defines the template used to generate code";
   ]
   in
   let _ = Arg.parse opts (fun a -> result.files <- a::result.files) "Usage: vultc file.vult\n" in
   let _ = result.files <- List.rev result.files in (* Put the files in the correct order  *)
   result

let parseFiles files =
   try
      List.map parseFile files
   with
   | Error.Errors(errors) ->
      let error_strings = Error.reportErrors errors in
      print_endline ("Errors in the program:\n"^error_strings);
      exit (-1)

let main () =
   let args = processArguments () in
   (* Parse the files *)
   let parser_results = parseFiles args.files in
   (* Prints the parsed files if -dparse was passed as argument *)
   Driver.dumpParsedFiles args parser_results;
   Driver.generateCode args parser_results |> ignore

;;
main ();;
