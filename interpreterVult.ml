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

(** Vult abstract syntax interpreter **)
open Types
open Lexing
open List
open CCMap
open Either
open Errors
open TypesUtil

type literal =
   | LInt of int
   | LReal of float
   | LBool of bool
   | LString of string
   | LUnbound

type vultFunction =
   {
      functionname : string;
      returntype   : string option;
      inputs       : Types.val_bind list;
      body         : Types.stmt list;
   }

module StringMap = CCMap.Make(String)
type bindings = literal StringMap.t
type functionBindings = vultFunction StringMap.t
let noBindings = StringMap.empty
let noFunctions = StringMap.empty

type environment =
   {
      memory : bindings;
      temp   : bindings;
      functions : functionBindings;
   }
let emptyEnv = { memory = noBindings; temp = noBindings; functions = noFunctions; }

(* Processing of functions. *)
let isFunctionStmt : Types.stmt -> bool =
   fun stmt ->
      match stmt with
      | StmtFun _ -> true
      | _ -> false

let bindFunction : functionBindings -> vultFunction -> (errors,functionBindings) either =
   fun binds ({functionname = funcname;} as f) ->
      match StringMap.get funcname binds with
      | None -> Right (StringMap.add funcname f binds)
      | Some _ -> Left ([SimpleError("Redeclaration of function " ^ funcname ^ ".")])

let getFunction : environment -> string -> (errors,vultFunction) either =
   fun {functions = binds;} fname ->
      match StringMap.get fname binds with
      | None ->  Left ([SimpleError("No function named " ^ fname ^ " exists.")])
      | Some f -> Right f

(* Processing of variables. *)
(* For easy construction of updater function in bindVariable. *)
let varBinder : literal -> literal option -> literal option =
   fun newval oldval ->
      match oldval with
      | Some _ -> Some newval
      | None -> None (* If the variable is not present, don't create it. *)

(* For easy creation of creation function in createVariable. *)
let varCreator : literal option -> literal option -> literal option =
   fun newval oldval ->
      match oldval,newval with
      | (Some x,_) -> Some x (* If the variable already exists, don't rebind it. *)
      | (None,Some _) -> newval
      | (None,None) -> Some LUnbound

(* bindVariable expects there to be a binding already present. *)
let bindVariable : environment -> string -> literal  -> environment =
   fun ({ memory = mem; temp = tmp; } as env) name value ->
      let updater = varBinder value in
      {
         env with
         memory = StringMap.update name updater mem;
         temp = StringMap.update name updater tmp;
      }

(* variableExists only checks if there is a variable with the given name. *)
let variableExists : environment -> string -> bool =
   fun { memory = mem; temp = tmp; } name ->
      match StringMap.get name tmp with
      | Some _ -> true
      | None -> match StringMap.get name mem with
         | Some _ -> true
         | None -> false

let createVariable : environment -> string -> literal option -> (errors,environment) either =
   fun ({temp = tmp; } as env) name possibleValue ->
      match variableExists env name with
      | true -> Left([SimpleError("Redeclaration of variable " ^ name ^ ".")])
      | false -> let updater = varCreator possibleValue in
         Right { env with temp = (StringMap.update name updater tmp); }

let createMemory : environment -> string -> literal option -> (errors,environment) either  =
   fun ({memory = mem; } as env) name possibleValue ->
      match variableExists env name with
      | true -> Left([SimpleError("Redeclaration of variable " ^ name ^ ".")])
      | false -> let updater = varCreator possibleValue in
         Right { env with memory = (StringMap.update name updater mem); }

(* Check family of functions. Checks that things are valid in the given environment. *)
let checkNamedId : environment -> named_id -> Types.errors option =
   fun env namedId ->
      let name = getNameFromNamedId namedId in
      match variableExists env name with
      | true -> None
      | false ->
         let loc = getLocationFromNamedId namedId in
         Some ([PointedError(loc,"No declaration of variable " ^ name ^ ".")])

let rec flattenExp : Types.parse_exp -> Types.parse_exp list =
   fun groupExp ->
      match groupExp with
      | PGroup exp -> flattenExp exp
      | PTuple es -> es
      | e -> [e]

let rec checkExp : environment -> Types.parse_exp -> Types.errors option =
   fun env exp ->
      let rec internalChecker : Types.parse_exp -> Types.errors option =
         fun exp -> match exp with
            | PId (name) -> checkNamedId env name
            | PBinOp (_,e1,e2,_) -> joinErrorOptions (internalChecker e1) (internalChecker e2)
            | PUnOp (_,e1,_) -> internalChecker e1
            | PCall (fname,values,loc) -> checkFunctionCall env fname values loc
            | PGroup e1 -> internalChecker e1
            | PTuple es -> joinErrorOptionsList (List.map internalChecker es)
            | _ -> None (* All others: Nothing to check. *)
      in
      internalChecker exp

(* Checking of function calls: Here we just check that the call is correct,
    that the function executes is checked somewhere else.
*)
and checkFunctionCall : environment -> Types.named_id -> Types.parse_exp list -> location -> errors option =
   fun env fId paramsUnflattened loc ->
      let _,fname = getFunctionTypeAndName fId in
      let params = List.flatten (List.map flattenExp paramsUnflattened) in
      match getFunction env fname with
      | Left errs -> Some (PointedError(loc,"Could not evaluate function call to " ^ fname ^ ".\n")::errs)
      | Right { inputs = inputs; } ->
         begin match joinErrorOptionsList (List.map (checkExp env) params) with
            | Some errs -> Some (PointedError(loc,"Could not evaluate function call parameter in function call to " ^ fname ^ ".\n")::errs)
            | None ->
               begin
                  let paramlength = List.length params in
                  let expectedlength = List.length inputs in
                  if paramlength == expectedlength
                  then None
                  else Some [SimpleError("Wrong number of arguments in call to function " ^ fname
                             ^ "; expected " ^ (string_of_int expectedlength) ^ " but got "
                             ^ (string_of_int paramlength) ^ ".")]
               end
         end

and checkRegularValBind : environment -> Types.val_bind -> (errors,environment) either  =
   fun env valbind ->
      match valbind with
      | ValNoBind (name,_) -> createVariable env (getNameFromNamedId name) None
      | ValBind (nameId,_,valBind) ->
         let name = getNameFromNamedId nameId in
         match checkExp env valBind with
         | Some errs -> Left (SimpleError("In binding of variable " ^ name ^ ".")::errs)
         | None -> createVariable env name None

and checkMemValBind : environment -> Types.val_bind -> (errors,environment) either  =
   fun env valbind ->
      match valbind with
      | ValNoBind (name,_) -> createMemory env (getNameFromNamedId name) None
      | ValBind (nameId,_,valBind) ->
         let name = getNameFromNamedId nameId in
         match checkExp env valBind with
         | Some errs -> Left (SimpleError("In binding of variable " ^ name ^ ".")::errs)
         | None -> createMemory env name None

and checkStmt : environment -> Types.stmt -> (errors,environment) either =
   fun env stmt ->
      match stmt with
      | StmtVal valbinds -> eitherFold_left checkRegularValBind env valbinds
      | StmtMem valbinds -> eitherFold_left checkMemValBind env valbinds
      | StmtReturn exp ->
         begin match checkExp env exp with
            | None -> Right env
            | Some errs -> Left (SimpleError("Could not evaluate expression in return statement.")::errs)
         end
      | StmtIf (cond,trueStmts,None) ->
         begin match checkExp env cond with
            | Some errs -> Left (SimpleError("Could not evaluate condition expression in if statement.")::errs)
            | None -> begin match eitherFold_left checkStmt env trueStmts with
                  | Right _ as success -> success
                  | Left errs -> Left (SimpleError("In if body true-branch statements.")::errs)
               end
         end
      | StmtIf (cond,trueStmts,Some falseStmts) ->
         begin match checkExp env cond with
            | Some errs -> Left (SimpleError("Could not evaluate condition expression in if statement.")::errs)
            | None ->
               begin match eitherFold_left checkStmt env trueStmts with
                  | Left errs -> Left (SimpleError("In if body true-branch statements.")::errs)
                  | Right env2 ->
                     begin match eitherFold_left checkStmt env2 falseStmts with
                        | Left errs -> Left (SimpleError("In if body false-branch statements")::errs)
                        | Right _ as success -> success
                     end
               end
         end
      | StmtFun _ -> Right env (* Ignore function declarations, these should be stored in env. *)
      | StmtBind (PId (name),rhs) ->
         begin match checkNamedId env name with
            | Some errs -> Left errs
            | None -> begin match checkExp env rhs with
                  | Some errs -> Left (SimpleError("Could not evaluate right hand side of bind.")::errs)
                  | None -> Right env
               end
         end
      | StmtBind _ -> Left [SimpleError("Left hand side of bind is not a variable.")]
      | StmtEmpty -> Right env

let checkStmts : environment -> Types.stmt list -> errors option =
   fun env stmts ->
      match eitherFold_left checkStmt env stmts with
      | Right _ -> None
      | Left errs -> Some errs

let checkFunction : environment -> vultFunction -> errors option =
   fun env func ->
      match eitherFold_left checkRegularValBind env func.inputs with
      | Left errs -> Some (SimpleError("In function input declarations of function " ^ func.functionname ^ ".")::errs)
      | Right env ->
         begin match checkStmts env func.body with
            | Some errs -> Some (SimpleError("In function " ^ func.functionname ^ ".")::errs)
            | None -> None
         end

let insertIfFunction : functionBindings -> Types.stmt  -> (errors,functionBindings) either =
   fun funcs stmt ->
      match stmt with
      | StmtFun (namedid,inputs,body) ->
         let funcname = getNameFromNamedId namedid in
         let functype = getTypeFromNamedId namedid in
         let vultfunc = { functionname = funcname; returntype = functype; inputs = inputs; body = body; } in
         bindFunction funcs vultfunc
      | _ -> Right funcs

let processFunctions : Types.stmt list -> (errors,functionBindings) either =
   fun stmts ->
      (* First check in the function declarations. *)
      match eitherFold_left insertIfFunction noFunctions stmts with
      | Left errs -> Left (SimpleError("When processing function declarations.")::errs)
      | Right fbinds -> (* Then check the function bodies. *)
         begin
            let funclist = map snd (StringMap.to_list fbinds) in
            let env = { emptyEnv with functions = fbinds } in
            match joinErrorOptionsList (List.map (checkFunction env) funclist) with
            | Some errs -> Left errs
            | None -> Right fbinds
         end

let checkStmtsMain : Types.stmt list -> errors option =
   fun stmts ->
      match processFunctions stmts with
      | Left errs -> Some errs
      | Right funcs ->  checkStmts {emptyEnv with functions = funcs} stmts

let programState : Types.parser_results -> interpreter_results =
   fun results ->
      match results.presult with
      | Left(_) ->
         { iresult = Left([SimpleError("Parse unsuccessful; no checking possible.")]); lines=results.lines }
      | Right stmts -> begin match checkStmtsMain stmts with
            | None -> { iresult = Right(); lines = results.lines }
            | Some errsRev ->
               let
                  errs = List.rev errsRev
               in
               { iresult = Left(errs); lines = results.lines }
         end











