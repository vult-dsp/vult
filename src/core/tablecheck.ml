(*
   The MIT License (MIT)

   Copyright (c) 2014-2024 Leonardo Laguna Ruiz

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

(* Checks the functions tagged @[table] and the externals tagged @[wave] or @[wavetable] right
   after type checking: the tag parameters, the number and types of the arguments, and the wav
   files. The tables are generated much later (Generators.Tables, after the optimizations and the
   interpreter), so checking them here reports the errors sooner, also in the editor, and lets
   -check skip the generation. Only the errors that need the functions evaluated are left to the
   generation.

   The messages and locations are the ones of Generators.Tables, which keeps its checks for
   programs that do not come from the type checker. *)

open Typed
module Tags = Pparser.Ptags

(* The name of a builtin type, or None for the others *)
let simpleType (subst : Subst.t) (t : type_) : string option =
  match (Subst.zonk subst t).tx with
  | TEId {id= ("int" | "int16" | "real" | "fix16" | "bool" | "string") as id; n= None; _} ->
      Some id
  | _ ->
      None

let isFractional subst t = match simpleType subst t with Some ("real" | "fix16") -> true | _ -> false

let isInteger subst t = match simpleType subst t with Some ("int" | "int16") -> true | _ -> false

let typeName subst t =
  match simpleType subst t with Some id -> id | None -> Pla.print (print_type_ (Subst.zonk subst t))

let displayName (def : function_def) = Pla.print (Pparser.Syntax.print_path def.name)

let reportPrecision subst loc attribute name requirement kind t =
  let msg =
    Printf.sprintf "The %s function '%s' has %s of type '%s'. %s" attribute name kind (typeName subst t) requirement
  in
  Util.Error.raiseError msg loc

let checkTablePrecision subst loc attribute name requirement in_t out_t =
  if not (isFractional subst in_t) then reportPrecision subst loc attribute name requirement "an input" in_t ;
  if not (isFractional subst out_t) then reportPrecision subst loc attribute name requirement "a result" out_t

let checkInputParam loc (args : arg list) : arg =
  match args with
  | [p] ->
      p
  | _ ->
      Util.Error.raiseError
        "Table generation attribute requires a function with exactly one parameter (e.g., 'fun foo(x:type) : type')" loc

let checkBoundCheck loc (v : Tags.value option) =
  match v with
  | Some (Tags.Bool _) | None ->
      ()
  | Some _ ->
      Util.Error.raiseError "The value of 'bound_check' must be 'true' or 'false'." loc

let checkOrder loc (v : Tags.value option) =
  match v with
  | None | Some (Tags.Int (1 | 2)) ->
      ()
  | Some _ ->
      Util.Error.raiseError "The value of 'order' must be 1 or 2." loc

(* The wav file must exist, be readable and have the given number of channels *)
let checkWaveFile (includes : string list) loc (file : string) (channels : int) : unit =
  match Util.FileIO.findFile includes file with
  | None ->
      Util.Error.raiseError ("The file '" ^ file ^ "' was not found in any of the include locations") loc
  | Some filename -> (
    match Util.WaveFile.read filename with
    | Error read_msg ->
        Util.Error.raiseError ("Failed to read the wav file '" ^ file ^ "': " ^ read_msg) loc
    | Ok wave ->
        if wave.channels <> channels then
          Util.Error.raiseError
            ( "The given number of channels (" ^ string_of_int channels
            ^ ") does not match the actual number of the channels in the file (" ^ string_of_int wave.channels ^ ")" )
            loc )

let checkRealTable subst (def : function_def) =
  let loc = def.loc in
  let params =
    Tags.[("size", TypeInt); ("min", TypeReal); ("max", TypeReal); ("order", TypeInt); ("bound_check", TypeBool)]
  in
  match Tags.getParameterList def.tags "table" params with
  | Tags.[Some (Int size); Some (Real _); Some (Real _); order; bound_check] ->
      checkBoundCheck loc bound_check ;
      checkOrder loc order ;
      if size < 0 then Util.Error.raiseError "The size of the table cannot be negative" loc ;
      let param = checkInputParam loc def.args in
      let requirement = "Tables declared with real 'min' and 'max' require 'real' or 'fix16' on both sides." in
      checkTablePrecision subst loc "table" (displayName def) requirement param.t (snd def.t)
  | _ ->
      Util.Error.raiseError
        "The attribute 'table' requires specific parameters. e.g. 'table(size = 128, min = 0.0, max = 1.0, [order = \
         2], [bound_check = true])'"
        loc

let checkIntTable subst (def : function_def) =
  let loc = def.loc in
  let wrong () =
    Util.Error.raiseError
      "The attribute 'table' on integer tables requires specific parameters. e.g. 'table(min = 0, max = 16)'" loc
  in
  match Tags.getParameterList def.tags "table" Tags.[("min", TypeInt); ("max", TypeInt)] with
  | Tags.[Some (Int min); Some (Int max)] ->
      if max < min then Util.Error.raiseError "The range of the table is empty: 'max' is smaller than 'min'" loc ;
      let out_t = snd def.t in
      if isFractional subst out_t then ignore (checkInputParam loc def.args)
      else if isInteger subst out_t then (
        let arg_t = match def.args with [a] -> a.t | _ -> out_t in
        if simpleType subst arg_t <> simpleType subst out_t then
          Util.Error.raiseError "To generate the table, the function requires the same types as input and output" loc ;
        ignore (checkInputParam loc def.args) )
      else wrong ()
  | _ ->
      wrong ()

let checkWave subst includes (def : function_def) =
  let loc = def.loc in
  match Tags.getParameterList def.tags "wave" Tags.[("channels", TypeInt); ("file", TypeString)] with
  | [Some (Int channels); Some (String file)] ->
      let precision = snd def.t in
      if not (isFractional subst precision) then
        reportPrecision subst loc "wave" (displayName def) "Waves require a 'real' or 'fix16' result." "a result"
          precision ;
      checkWaveFile includes loc file channels ;
      if CCList.length def.args <> 2 then
        Util.Error.raiseError
          "This attribute requires the function to have the following arguments:\n\
           \"external wave(channel:int, index:int) : real\""
          loc
  | _ ->
      Util.Error.raiseError
        "The attribute 'wave' requires specific parameters. e.g. 'wave(channels=1, file=\"file.wav\")'" loc

let checkWavetable subst includes (def : function_def) =
  let loc = def.loc in
  match Tags.getParameterList def.tags "wavetable" Tags.[("file", TypeString); ("bound_check", TypeBool)] with
  | [Some (String file); bound_check] ->
      checkBoundCheck loc bound_check ;
      let param = checkInputParam loc def.args in
      let requirement = "Wavetables require 'real' or 'fix16' on both sides." in
      checkTablePrecision subst loc "wavetable" (displayName def) requirement param.t (snd def.t) ;
      checkWaveFile includes loc file 1
  | _ ->
      Util.Error.raiseError "This attribute can only be applied to functions returning 'real'" loc

let checkTopStmt subst includes (s : top_stmt) =
  match s.top with
  | TopFunction (def, _) when Tags.has def.tags "table" -> (
    match def.args with [a] when isInteger subst a.t -> checkIntTable subst def | _ -> checkRealTable subst def )
  | TopExternal (def, _) when Tags.has def.tags "wave" ->
      checkWave subst includes def
  | TopExternal (def, _) when Tags.has def.tags "wavetable" ->
      checkWavetable subst includes def
  | _ ->
      ()

(* Checks the tables of the modules, raising the first error *)
let check (subst : Subst.t) (includes : string list) (modules : (string * top_stmt list) list) : unit =
  CCList.iter (fun (_, stmts) -> CCList.iter (checkTopStmt subst includes) stmts) modules
