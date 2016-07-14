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
open VEnv

let checkRealType (real:string) : unit =
   match real with
   | "fixed" -> ()
   | "float" -> ()
   | "js" -> ()
   | _ ->
      let msg = ("Unknown type '"^real^"'\nThe only valid values for -real are: fixed or float") in
      Error.raiseErrorMsg msg


type configuration =
   {
      module_name     : string;
      process_inputs  : string list;
      process_outputs : string list;
      noteon_inputs   : string list;
      noteoff_inputs  : string list;
      controlchange_inputs : string list;
   }

let empty_conf module_name =
   {
      module_name;
      process_inputs  = [];
      process_outputs = [];
      noteon_inputs   = [];
      noteoff_inputs  = [];
      controlchange_inputs = [];
   }

(** Determines the number of inputs and outputs of the key function to generate templates *)
module Configuration = struct

   (** Checks that the type is a numeric type *)
   let checkNumeric (typ:VType.t) : string option =
      match !typ with
      | VType.TId(["real"],_) -> Some("real")
      | VType.TId(["int"],_)  -> Some("int")
      | _ -> None

   (** Checks that the output is a numeric or a tuple of numbers *)   
   let rec getOutputs (loc:Loc.t) (typ:VType.t) : string list =
      match !typ with
      | VType.TId(["real"],_) -> ["real"]
      | VType.TId(["int"],_) -> ["int"]
      | VType.TComposed(["tuple"],elems,_) ->
         List.map (getOutputs loc) elems
         |> List.flatten
      | _ ->
         let msg = "The return type of the function process should be a numeric value or a tuple with numeric elements" in
         Error.raiseError msg loc

   (** Returns the type of the argument as a string, if it's the context then the type is data *)      
   let getType (arg:typed_id) : string =
      match arg with
      | TypedId(["_ctx"],_,_) -> "data"
      | TypedId(_,typ,attr) ->
         begin match checkNumeric typ with
         | Some(typ_name) -> typ_name
         | None ->
            let msg = "The type of this argument must be numeric" in
            Error.raiseError msg attr.loc
         end
      | _ -> failwith "Configuration.getType: Undefined type"

   (** Returns the number of inputs excluding 'data' *)
   let countInputs (inputs:string list) : int =
      List.filter (fun a -> a <> "data") inputs
      |> List.length

   (** This traverser checks the function declarations of the key functions to generate templates *)
   let stmt : (configuration Env.t,stmt) Mapper.mapper_func =
      Mapper.make @@ fun state stmt ->
      let conf = Env.get state in
      let mname = conf.module_name in
      match stmt with
      | StmtFun([cname;"process"],args,_,Some(rettype),attr) when mname = cname ->
         let process_inputs = List.map getType args in
         let process_outputs = getOutputs attr.loc rettype in
         let state' = Env.set state { conf with process_inputs; process_outputs } in
         state', stmt
      | StmtFun([cname;"noteOn"],args,_,_,attr) when mname = cname ->
         let noteon_inputs = List.map getType args in
         let ninputs = countInputs noteon_inputs in
         let () = (* Report error if the number of inputs do not match *)
            if ninputs <> 2 && ninputs <> 3 then
            let msg = "The noteOn function should have 2 or 3 arguments (note,velocity) or (note,velocity,channel) " in
            Error.raiseError msg attr.loc
         in
         let state' = Env.set state { conf with noteon_inputs } in
         state', stmt
      | StmtFun([cname;"noteOff"],args,_,_,attr) when mname = cname ->
         let noteoff_inputs = List.map getType args in
         let ninputs = countInputs noteoff_inputs in
         let () = (* Report error if the number of inputs do not match *)
            if ninputs <> 1 && ninputs <> 2 then
            let msg = "The noteOff function should have 1 or 2 arguments (note) or (note,channel) " in
            Error.raiseError msg attr.loc
         in
         let state' = Env.set state { conf with noteoff_inputs } in
          state', stmt
      | StmtFun([cname;"controlChange"],args,_,_,attr) when mname = cname ->
         let controlchange_inputs = List.map getType args in
         let ninputs = countInputs controlchange_inputs in
         let () = (* Report error if the number of inputs do not match *)
            if ninputs <> 2 && ninputs <> 3 then
            let msg = "The controlChange function should have 2 or 3 arguments (control,value) or (control,value,channel) " in
            Error.raiseError msg attr.loc
         in
         let state' = Env.set state { conf with controlchange_inputs; } in
         state', stmt
      | StmtFun([cname;"default"],args,_,_,attr) when mname = cname ->
         let default_inputs = List.map getType args in
         let ninputs = countInputs default_inputs in
         let () = (* Report error if the number of inputs do not match *)
            if ninputs <> 0 then
            let msg = "The 'default' function should not have arguments" in
            Error.raiseError msg attr.loc
         in
         state, stmt
      | _ -> state, stmt

   let mapper =
      { Mapper.default_mapper with Mapper.stmt = stmt }

   (** Get the configuration from the statements *)
   let get (module_name:string) (stmts:TypesVult.stmt list) : configuration =
      let env = Env.empty (empty_conf module_name) in
      let env',_ = Mapper.map_stmt_list mapper env stmts in
      Env.get env'
end

(** Gets the name of the main module, which is the last parsed file *)
let rec getMainModule (parser_results:parser_results list) : string =
   match parser_results with
   | []   -> failwith "No files given"
   | [h] when h.file = "" -> "Vult"
   | [h] -> moduleName h.file
   | _::t -> getMainModule t

let createParameters (module_name:string) (args:arguments) : params =
   let ()     = DefaultReplacements.initialize () in
   let ()     = checkRealType args.real in
   let output = if args.output = "" then "Vult" else Filename.basename args.output in
   let repl   = Replacements.getReplacements args.real in
   { real = args.real; template = args.template; is_header = false; output; repl; module_name }


let generateC (args:arguments) (params:params) (stmts:TypesVult.stmt list) : (Pla.t * string) list=
   if args.ccode then
      let cparams = VultToCLike.{repl = params.repl; return_by_ref = true } in
      let clike_stmts = VultToCLike.convertStmtList cparams stmts in
      VultCh.print params clike_stmts
   else []


let generateJS (args:arguments) (params:params) (stmts:TypesVult.stmt list) : (Pla.t * string) list=
   if args.jscode then
      let cparams = VultToCLike.{repl = params.repl; return_by_ref = false } in
      let clike_stmts = VultToCLike.convertStmtList cparams stmts in
      VultJs.print params clike_stmts
   else []


let generateCode (parser_results:parser_results list) (args:arguments) : (Pla.t * string) list =
   let stmts =
      parser_results
      |> Passes.applyTransformations args
   in
   let module_name = getMainModule parser_results in
   let last_stmts  = CCList.last 1 stmts |> List.flatten in
   let conf        = Configuration.get module_name last_stmts in   
   let all_stmts   = List.flatten stmts in
   let params      = createParameters module_name args in
   let ccode       = generateC args params all_stmts in
   let jscode      = generateJS args params all_stmts in
   jscode @ ccode


