(*
The MIT License (MIT)

Copyright (c) 2014 Leonardo Laguna Ruiz

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

open TypesVult
open GenerateParams

let checkRealType (real:string) : unit =
   match real with
   | "fixed" -> ()
   | "float" -> ()
   | "js" -> ()
   | _ ->
      let msg = ("Unknown type '"^real^"'\nThe only valid values for -real are: fixed or float") in
      Error.raiseErrorMsg msg

(** Gets the name of the main module, which is the last parsed file *)
let rec getMainModule (parser_results:parser_results list) : string =
   match parser_results with
   | []   -> failwith "No files given"
   | [h] when h.file = "" -> "Vult"
   | [h] -> moduleName h.file
   | _::t -> getMainModule t

let createParameters (parser_results:parser_results list) (args:arguments) : params =
   let ()     = DefaultReplacements.initialize () in
   let ()     = checkRealType args.real in
   let output = if args.output = "" then "Vult" else Filename.basename args.output in
   let repl   = Replacements.getReplacements args.real in
   let module_name = getMainModule parser_results in
   { real = args.real; template = args.template; is_header = false; output; repl; module_name }

let generateCode (parser_results:parser_results list) (args:arguments) : (Pla.t * string) list =
   let stmts =
      parser_results
      |> Passes.applyTransformations args
      |> List.flatten
   in
   let params = createParameters parser_results args in
   let ccode =
      if args.ccode then
         let cparams = VultToCLike.{repl = params.repl; return_by_ref = true } in
         let clike_stmts = VultToCLike.convertStmtList cparams stmts in
         VultCh.print params clike_stmts
      else []
   in
   let jscode =
      if args.jscode then
         let cparams = VultToCLike.{repl = params.repl; return_by_ref = false } in
         let clike_stmts = VultToCLike.convertStmtList cparams stmts in
         VultJs.print params clike_stmts
      else []
   in
   jscode @ ccode


