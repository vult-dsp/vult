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

(** Contains top level functions to perform common tasks *)

open TypesVult
open PassesUtil


(** Parses a string and runs it with the interpreter *)
let parseStringRun s =
   ParserVult.parseString s
   |> Passes.applyTransformations PassesUtil.opt_full_transform
   |> DynInterpreter.interpret

(** Parses a string and runs it with the interpreter *)
let parseStringRunWithOptions options s =
   ParserVult.parseString s
   |> Passes.applyTransformations options
   |> DynInterpreter.interpret

(** Generates the .c and .h file contents for the given parsed files *)
let generateCode (args:arguments) (parser_results:parser_results list) =
   let file = if args.output<>"" then args.output else "code" in
   let file_up = String.uppercase file in
   let stmts =
      parser_results
      |> List.map (Passes.applyTransformations { opt_full_transform with inline = true; codegen = true })
      |> List.map (
         fun a -> match a.presult with
         | `Ok(b) -> b
         | _ -> [] )
      |> List.flatten
   in

   let c_text,h_text = ProtoGenC.generateHeaderAndImpl args stmts in
   (Printf.sprintf "#include \"%s.h\"\n\n%s\n" file c_text),
   (Printf.sprintf "#ifndef _%s_\n#define _%s_\n\n#include \"math.h\"\n\n%s\n#endif\n" file_up file_up h_text)
