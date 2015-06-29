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
(*open PassesUtil*)

(*
(** Generates the .c and .h file contents for the given parsed files *)
let generateCCode (args:arguments) (parser_results:parser_results list) : string =
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
   let c_final = Printf.sprintf "#include \"%s.h\"\n\n%s\n" file c_text in
   let h_final = Printf.sprintf
"#ifndef _%s_
#define _%s_

#include <math.h>
#include <stdint.h>
#include \"vultin.h\"

#ifdef __cplusplus
extern \"C\"
{
#endif

%s

#ifdef __cplusplus
}
#endif
#endif" file_up file_up h_text
   in
   let _ =
   if args.output<>"" then
      begin
         let oc = open_out (args.output^".c") in
         Printf.fprintf oc "%s\n" c_final;
         close_out oc;
         let oh = open_out (args.output^".h") in
         Printf.fprintf oh "%s\n" h_final;
         close_out oh
      end
   else
      begin
         print_endline h_final;
         print_endline c_final;
      end
   in h_final^c_final

(** Generates the .c and .h file contents for the given parsed files *)
let generateJSCode (args:arguments) (parser_results:parser_results list) : string =
   let stmts =
      parser_results
      |> List.map (Passes.applyTransformations { opt_full_transform with inline = false; codegen = true })
      |> List.map (
         fun a -> match a.presult with
            | `Ok(b) -> b
            | _ -> [] )
      |> List.flatten
   in

   let js_text = ProtoGenJS.generateModule args stmts in
   let js_final =
      if List.length stmts = 0 then "null"
      else
         Printf.sprintf
"function Process(){
 this.clip = function(x,low,high) { return x<low?low:(x>high?high:x); };
 this.not  = function(x)          { x==0?1:0; };
 %s
 this.context = {};
 if(this._live_struct_process_init) this._live_struct_process_init(this.context);
 if(this.live__default)             this.live__default(this.context);
 if(this.live__process)             this.live__process(this.context,i);
 this.noteOn        = function(note,velocity) { if(this.live__noteOn)        this.live__noteOn(this.context,note,velocity); };
 this.noteOff       = function(note,velocity) { if(this.live__noteOff)       this.live__noteOff(this.context,note,velocity); };
 this.controlChange = function(note,velocity) { if(this.live__controlChange) this.live__controlChange(this.context,note,velocity); };
 this.process       = function(input)         { if(this.live__process)       return this.live__process(this.context,input); else return 0; };
 this.default       = function()              { if(this.live__default)       return this.live__default(this.context); };
}
new Process()"
      js_text
      in
      if args.output<>"" then
         begin
            let oc = open_out (args.output^".js") in
            Printf.fprintf oc "%s\n" js_final;
            close_out oc;
            js_final
         end
      else
         begin
            print_endline js_final;
            js_final
         end

let generateCode (args:arguments) (parser_results:parser_results list) : string =
   if args.ccode then
      generateCCode args parser_results
   else
   if args.jscode then
      generateJSCode args parser_results
   else ""


let parseStringGenerateCode s =
   ParserVult.parseString s
   |> fun a -> generateCode default_arguments [a]
*)

let parsePrintCode s =
   let result = ParserVult.parseString s in
   match result.presult with
   | `Ok(b) ->
      PrintTypes.stmtListStr b
   | `Error(msg) ->
      let error_strings:string list = Error.reportErrors result.presult result.lines in
      let result =List.fold_left (fun s a -> s^"\n"^a) "" error_strings in
      "Errors in the program:\n"^result
