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

open Prog
open Config
open Env
open Args

(** Reports an error if the 'real' argument is invalid *)
let checkRealType (real:string) : unit =
   match real with
   | "fixed" -> ()
   | "float" -> ()
   | "js" -> ()
   | _ ->
      let msg = ("Unknown type '"^real ^ "'\nThe only valid values for -real are: fixed or float") in
      Error.raiseErrorMsg msg

(** Determines the number of inputs and outputs of the key function to generate templates *)
module Configuration = struct

   (** If the first argument is data, returns true and remove it *)
   let rec passData (inputs:'a list) =
      match inputs with
      | `Context ctx :: t ->
         let inputs, outputs = passData t in
         ctx :: inputs, outputs
      | `Input typ :: t ->
         let inputs, outputs = passData t in
         typ :: inputs, outputs
      | `Output elems :: t ->
         let inputs, _ = passData t in
         inputs, elems
      | [] ->
         [], []

   (** Checks that the type is a numeric type *)
   let checkNumeric repl (name:string) (typ:Typ.t) : Config.input option =
      match !typ with
      | Typ.TId(["real"], _) -> Some (Config.IReal (Replacements.getKeyword repl name))
      | Typ.TId(["int"], _)  -> Some (Config.IInt (Replacements.getKeyword repl name))
      | Typ.TId(["bool"], _)  -> Some (Config.IBool (Replacements.getKeyword repl name))
      | _ -> None

   (** Checks that the output is a numeric or a tuple of numbers *)
   let rec getOutputs (loc:Loc.t) (typ:Typ.t) : Config.output list =
      match !typ with
      | Typ.TId(["real"], _) -> [Config.OReal]
      | Typ.TId(["int"], _) -> [Config.OInt]
      | Typ.TComposed(["tuple"], elems, _) ->
         List.map (getOutputs loc) elems
         |> List.flatten
      | _ ->
         let msg = "The return type of the function process should be a numeric value or a tuple with numeric elements" in
         Error.raiseError msg loc

   let getOutputsOrDefault outputs (loc:Loc.t) (typ:Typ.t) =
      match !typ, outputs with
      | Typ.TId(["unit"], _), _ -> outputs
      | _, [] -> getOutputs loc typ
      | _ ->
         failwith "Generate.getOutputsOrDefault: strage error"

   (** Returns the type of the argument as a string, if it's the context then the type is data *)
   let getType repl (arg:typed_id) =
      match arg with
      | TypedId(_, _, ContextArg, _) -> `Input Config.IContext
      | TypedId([name], [typ], InputArg, attr) ->
         begin
            match checkNumeric repl name typ with
            | Some(typ_name) -> `Input typ_name
            | None ->
               let msg = "The type of this argument must be numeric" in
               Error.raiseError msg attr.loc
         end
      | TypedId([_], [typ], OutputArg, attr) ->
         `Output (getOutputs attr.loc typ)
      | _ -> failwith "Configuration.getType: Undefined type"

   let rec checkNoteOn loc (inputs:Config.input list) =
      match inputs with
      | IContext :: t -> checkNoteOn loc t
      | [_; _; _] -> ()
      | _ ->
         let msg = "The function 'noteOn' must have three arguments (note, velocity, channel)" in
         Error.raiseError msg loc

   let rec checkNoteOff loc (inputs:Config.input list) =
      match inputs with
      | IContext :: t -> checkNoteOff loc t
      | [_; _] -> ()
      | _ ->
         let msg = "The function 'noteOff' must have two arguments (note, channel)" in
         Error.raiseError msg loc

   let rec checkControlChange loc (inputs:Config.input list) =
      match inputs with
      | IContext :: t -> checkControlChange loc t
      | [_; _; _] -> ()
      | _ ->
         let msg = "The function 'checkControlChange' must have three arguments (control, value, channel)" in
         Error.raiseError msg loc

   let rec checkDefault loc (inputs:Config.input list) =
      match inputs with
      | IContext :: t -> checkDefault loc t
      | [] -> ()
      | _ ->
         let msg = "The function 'default' must have no arguments" in
         Error.raiseError msg loc

   (** This traverser checks the function declarations of the key functions to generate templates *)
   let stmt : ('a Env.t, stmt) Mapper.mapper_func =
      Mapper.make "Configuration.stmt" @@ fun state stmt ->
      let (conf : Config.config), repl = Env.get state in
      match stmt with
      | StmtFun([cname; "process"], args, _, Some(rettype), attr) when conf.module_name = cname ->
         let process_inputs, process_outputs = List.map (getType repl) args |> passData in
         let process_outputs = getOutputsOrDefault process_outputs attr.loc rettype in
         let state' = Env.set state ({ conf with process_inputs; process_outputs }, repl) in
         state', stmt
      | StmtFun([cname; "noteOn"], args, _, _, attr) when conf.module_name = cname ->
         let noteon_inputs, _ = List.map (getType repl) args |> passData in
         let () = checkNoteOn attr.loc noteon_inputs in
         let state' = Env.set state ({ conf with noteon_inputs }, repl) in
         state', stmt
      | StmtFun([cname; "noteOff"], args, _, _, attr) when conf.module_name = cname ->
         let noteoff_inputs, _ = List.map (getType repl) args |> passData in
         let () = checkNoteOff attr.loc noteoff_inputs in
         let state' = Env.set state ({ conf with noteoff_inputs }, repl) in
         state', stmt
      | StmtFun([cname; "controlChange"], args, _, _, attr) when conf.module_name = cname ->
         let controlchange_inputs, _ = List.map (getType repl) args |> passData in
         let () = checkControlChange attr.loc controlchange_inputs in
         let state' = Env.set state ({ conf with controlchange_inputs }, repl) in
         state', stmt
      | StmtFun([cname; "default"], args, _, _, attr) when conf.module_name = cname ->
         let default_inputs, _ = List.map (getType repl) args |> passData in
         let () = checkDefault attr.loc default_inputs in
         let state' = Env.set state ({ conf with default_inputs }, repl) in
         state', stmt
      | _ -> state, stmt

   let mapper =
      { Mapper.default_mapper with Mapper.stmt = stmt }

   (** Get the configuration from the statements *)
   let get (repl:Replacements.t) (module_name:string) (stmts:Prog.stmt list) : config =
      let env = Env.empty (empty_conf module_name, repl) in
      let env', _ = Mapper.map_stmt_list mapper env stmts in
      fst (Env.get env')
end

(** Gets the name of the main module, which is the last parsed file *)
let rec getMainModule (parser_results:parser_results list) : string =
   match parser_results with
   | []   -> Error.raiseErrorMsg "No files given"
   | [h] when h.file = "" -> "Vult"
   | [h] -> moduleName h.file
   | _ :: t -> getMainModule t

(* Generates the C/C++ code if the flag was passed *)
let generateC (args:args) (params:params) (stmts:Prog.stmt list) : (Pla.t * FileKind.t) list=
   let cparams     = ProgToCode.{ repl = params.repl; code = args.code; cleanup = args.roots <> [] } in
   (* Converts the statements to Code form *)
   let clike_stmts = ProgToCode.convert cparams stmts in
   CodeC.print params clike_stmts

(* Generates the C/C++ code if the flag was passed *)
let generateJava (args:args) (params:params) (stmts:Prog.stmt list) : (Pla.t * FileKind.t) list=
   let cparams     = ProgToCode.{ repl = params.repl; code = args.code; cleanup = args.roots <> [] } in
   (* Converts the statements to Code form *)
   let clike_stmts = ProgToCode.convert cparams stmts in
   CodeJava.print params clike_stmts

let generateLLVM (args:args) (params:params) (stmts:Prog.stmt list) : (Pla.t * FileKind.t) list=
   let cparams     = ProgToCode.{ repl = params.repl; code = args.code; cleanup = args.roots <> [] } in
   (* Converts the statements to Code form *)
   let clike_stmts = ProgToCode.convert cparams stmts in
   CodeLLVM.print params clike_stmts

(* Generates the JS code if the flag was passed *)
let generateJS (args:args) (params:params) (stmts:Prog.stmt list) : (Pla.t * FileKind.t) list=
   let cparams     = ProgToCode.{ repl = params.repl; code = args.code; cleanup = args.roots <> [] } in
   (* Converts the statements to Code form *)
   let clike_stmts = ProgToCode.convert cparams stmts in
   CodeJs.print params clike_stmts

(* Generates the JS code if the flag was passed *)
let generateLua (args:args) (params:params) (stmts:Prog.stmt list) : (Pla.t * FileKind.t) list=
   let cparams     = ProgToCode.{ repl = params.repl; code = args.code; cleanup = args.roots <> [] } in
   (* Converts the statements to Code form *)
   let clike_stmts = ProgToCode.convert cparams stmts in
   CodeLua.print params clike_stmts

let checkConfig (config:config) (args:args) =
   if args.code = CCode && args.template <> "default" || args.code = LuaCode || args.code = JSCode then
      if config.process_outputs = []
      || config.noteon_inputs = []
      || config.noteoff_inputs = []
      || config.controlchange_inputs = [] then
         let msg =
            Pla.print
               {pla|
Required functions are not defined or have incorrect inputs or outputs. Here's a template you can use:

fun process(input:real){ return input; }
and noteOn(note:int, velocity:int, channel:int){ }
and noteOff(note:int, channel:int){ }
and controlChange(control:int, value:int, channel:int){ }
and default(){ }|pla}
         in
         Error.raiseErrorMsg msg

let replacements args =
   match args.code with
   | JavaCode -> "java"
   | JSCode -> "js"
   | LuaCode -> "lua"
   | CCode -> args.real
   | _ -> "default"

(** Returns the code generation parameters based on the vult code *)
let createParameters (results:parser_results list) (args:args) : params =
   (* Gets the name of the main module (the last passes file name) *)
   let module_name = getMainModule results in
   let stmts       = List.map (fun a -> a.presult) results in
   (* Looks for the replacements based on the 'real' argument *)
   let repl        = Replacements.getReplacements (replacements args) in
   (** Takes the statememts of the last file to search the configuration *)
   let last_stmts  = CCList.last 1 stmts |> List.flatten in
   let config      = Configuration.get repl module_name last_stmts in
   let ()          = checkConfig config args in
   (* Defines the name of the output module *)
   let output      = if args.output = "" then "Vult" else Filename.basename args.output in
   { real = args.real; template = args.template; is_header = false; output; repl; module_name; config }

let generateCode (parser_results:parser_results list) (args:args) : (Pla.t * FileKind.t) list =
   if args.code <> NoCode && parser_results <> [] then
      (* Initialize the replacements *)
      let ()          = DefaultReplacements.initialize () in
      (* Checks the 'real' argument is valid *)
      let ()          = checkRealType args.real in
      (* Applies all passes to the statements *)
      let stmts       = Passes.applyTransformations args parser_results in
      let params_c    = createParameters stmts args in
      let params_js   = createParameters stmts { args with real = "js" } in
      let params_lua  = createParameters stmts { args with real = "lua" } in
      (* Calls the code generation  *)
      let all_stmts   = List.flatten (List.map (fun a -> a.presult) stmts) in
      match args.code with
      | NoCode -> []
      | CCode -> generateC args params_c all_stmts
      | JavaCode -> generateJava args params_c all_stmts
      | JSCode -> generateJS args params_js all_stmts
      | LuaCode -> generateLua args params_lua all_stmts
      | LLVMCode -> generateLLVM args params_c all_stmts
   else []


