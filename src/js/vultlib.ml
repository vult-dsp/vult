(*
The MIT License (MIT)

Copyright (c) 2017 Leonardo Laguna Ruiz

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

(**
   Provides (when compiled with js_of_ocaml) a node.js module with all the
   Vult compile functions.
*)

open Args


(** Js object to represent the errors *)
class type error = object
   method msg  : Js.js_string Js.t Js.readonly_prop
   method file : Js.js_string Js.t Js.readonly_prop
   method line : int Js.readonly_prop
   method col  : int Js.readonly_prop
end


(** Js object to represent the files (input and output) :  { file: "file", code : "code" } *)
class type js_file_code = object
   method code : Js.js_string Js.t Js.optdef_prop
   method file : Js.js_string Js.t Js.optdef_prop
end


(** Js object to represent the arguments *)
class type js_args = object
   method version  : bool Js.optdef_prop
   method dparse   : bool Js.optdef_prop
   method deps     : bool Js.optdef_prop
   method check    : bool Js.optdef_prop
   method eval     : bool Js.optdef_prop
   method code     : Js.js_string Js.t Js.optdef_prop
   method output   : Js.js_string Js.t Js.optdef_prop
   method real     : Js.js_string Js.t Js.optdef_prop
   method template : Js.js_string Js.t Js.optdef_prop
   method includes : ((Js.js_string Js.t) Js.js_array) Js.t Js.optdef_prop
   method files    : ((js_file_code Js.t) Js.js_array Js.t) Js.optdef_prop
end


(** Js object to represent the options (reduced set of arguments) used in code generation *)
class type options = object
   method output   : Js.js_string Js.t Js.optdef_prop
   method real     : Js.js_string Js.t Js.optdef_prop
   method template : Js.js_string Js.t Js.optdef_prop
   method includes : ((Js.js_string Js.t) Js.js_array) Js.t Js.optdef_prop
end

(** returns a new unsafe js object *)
let new_object () =
   Js.Unsafe.coerce
      (object%js
      end)

(** Converts the Vult errors to a Js form *)
let convertOutputErrors errors =
   let makeErrorObject error : error Js.t =
      let msg, file, line, col = Error.reportErrorStringNoLoc error in
      object%js
         val msg = Js.string msg
         val file = Js.string file
         val line = line
         val col =  col
      end
   in
   List.map makeErrorObject errors |> Array.of_list |> Js.array


(** Converts a Js [js_file_code] to an [input] file *)
let convertInputFile (i:js_file_code Js.t) : input list =
   match Js.Optdef.to_option i##.file, Js.Optdef.to_option i##.code with
   | Some(file), Some(code) ->
      [Code(Js.to_string file, (Js.to_string code))]
   | Some(file), None ->
      [File((Js.to_string file))]
   | None, Some(code) ->
      [Code("live.vult", (Js.to_string code))]
   | None, None -> []


(** Converts an array of [js_file_code] to an [input] files *)
let convertInputFiles (files:js_file_code Js.t Js.js_array Js.t) : input list =
   Js.to_array files
   |> Array.map convertInputFile
   |> Array.to_list
   |> List.flatten


let getFile (args:args) (ext:FileKind.t) : string =
   match ext with
   | FileKind.ExtOnly(e) -> args.output ^ "." ^ e
   | FileKind.FullName(n) -> Filename.concat (Filename.dirname args.output) n


(** Returns a [js_file_code] given the file name and its code (used as return value)*)
let convertOutputFile (file:string) (code:string) : js_file_code Js.t =
   let obj = new_object () in
   obj##.file := Js.string file;
   obj##.code := Js.string code;
   obj

(** Converts a list of output files *)
let convertOutputFiles args files =
   List.map (fun (text, file) -> convertOutputFile (getFile args file) (Pla.print text)) files
   |> Array.of_list |> Js.array


(** Used to set the value of an optional js value to the [arguments] *)
let set value fset =
   if Js.Optdef.test value then
      Js.Optdef.iter value (fun a -> fset a)

let convertCodeName code =
   match code with
   | "c" -> CCode
   | "js" -> JSCode
   | "lua" -> LuaCode
   | _ -> failwith (Printf.sprintf "unknow code generator '%s'" code)

(** Returns the Vult [arguments] based on the arguments object passed from js *)
let getArguments (obj:js_args Js.t) =
   let args = { default_arguments with files = [] } in
   set (obj##.dparse)   (fun v -> args.dparse <- v);
   set (obj##.deps)     (fun v -> args.deps <- v);
   set (obj##.check)    (fun v -> args.check <- v);
   set (obj##.eval)     (fun v -> args.eval <- v);
   set (obj##.version)  (fun v -> args.show_version <- v);
   set (obj##.code)     (fun v  -> args.code <- convertCodeName(Js.to_string v));
   set (obj##.output)   (fun v -> args.output <- Js.to_string v);
   set (obj##.real)     (fun v -> args.real <- Js.to_string v);
   set (obj##.template) (fun v -> args.template <- Js.to_string v);
   set (obj##.includes) (fun v -> args.includes <- Js.to_array v |> Array.map Js.to_string |> Array.to_list );
   set (obj##.files)    (fun v -> args.files <- convertInputFiles v);
   args


(** Applies the reduced set of options to the vult [arguments] *)
let applyOptions (options:options Js.t) (args:args) =
   set (options##.output)   (fun v -> args.output <- Js.to_string v);
   set (options##.real)     (fun v -> args.real <- Js.to_string v);
   set (options##.template) (fun v -> args.template <- Js.to_string v);
   set (options##.includes) (fun v -> args.includes <- Js.to_array v |> Array.map Js.to_string |> Array.to_list )


(** Generic function to convert the vult [output] to js *)
let showResult (args:args) (output:output) : 'a Js.t =
   match output with
   | Version v ->
      let obj = new_object () in
      obj##.version := Js.string v;
      obj
   | Message v ->
      let obj = new_object () in
      obj##.message := Js.string v;
      obj
   | Dependencies deps ->
      let obj = new_object () in
      obj##.dependencies := Js.array (List.map Js.string deps |> Array.of_list);
      obj
   | ParsedCode v ->
      let obj = new_object () in
      obj##.parsedCode := Js.string v;
      obj
   | GeneratedCode files ->
      let obj = new_object () in
      obj##.generatedCode := convertOutputFiles args files;
      obj
   | Interpret v ->
      let obj = new_object () in
      obj##.interpret := Js.string v;
      obj
   | CheckOk ->
      let obj = new_object () in
      obj##.check := Js.bool true;
      obj
   | Errors errors ->
      let obj = new_object () in
      obj##.errors := convertOutputErrors errors;
      obj

(** Uses [showResults] to convert the vult [output] to a js array *)
let showResults (args:args) (results:output list) =
   results
   |> List.map (showResult args)
   |> Array.of_list
   |> Js.array


(** main function to run all Vult compiler functions based on the input arguments *)
let main (input:js_args Js.t) =
   let args = getArguments input in
   Driver.main args
   |> showResults args


(** Returns the version of the compiler *)
let version =
   let args = { default_arguments with show_version = true } in
   match Driver.main args with
   | [Version v ] -> Js.string v
   | _ -> failwith "unknown error"

(** This function is used by all the code generation functions to process the results *)
let codeGeneration args results =
   match results with
   | GeneratedCode files :: _ -> convertOutputFiles args files |> Js.Unsafe.coerce
   | Errors errors :: _ -> convertOutputErrors errors |> Js.Unsafe.coerce
   | _ -> failwith "unknown error"

let generateJs (files:js_file_code Js.t Js.js_array Js.t) (options: 'a Js.t) =
   let args    = { default_arguments with code = JSCode; files = convertInputFiles files } in
   let ()      = applyOptions options args in
   let results = Driver.main args in
   codeGeneration args results


let generateC (files:js_file_code Js.t Js.js_array Js.t) (options: 'a Js.t) =
   let args    = { default_arguments with code = CCode; files = convertInputFiles files } in
   let ()      = applyOptions options args in
   let results = Driver.main args in
   codeGeneration args results

let generateLua (files:js_file_code Js.t Js.js_array Js.t) (options: 'a Js.t) =
   let args = { default_arguments with code = LuaCode; files = convertInputFiles files } in
   let ()      = applyOptions options args in
   let results = Driver.main args in
   codeGeneration args results


let _ =
   Js.export_all
      (object%js
         method main        = main
         method version     = version
         method generateJs  = generateJs
         method generateC   = generateC
         method generateLua = generateLua
      end)

