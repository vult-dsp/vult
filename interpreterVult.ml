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

type error = string list

let joinErrors : error -> error -> error = List.append

let joinErrorOptions : error option -> error option -> error option =
    fun maybeErr1 maybeErr2 ->
        match (maybeErr2,maybeErr2) with
            | (None,None) -> None
            | (Some _ as ret,None) -> ret
            | (None,(Some _ as ret)) -> ret
            | (Some err1,Some err2) -> Some (joinErrors err1 err2)

type ('a,'b) either =
    | Left of 'a
    | Right of 'b

(* eitherFold_left folds left over a list, and stops if it encounters Left. *)
let eitherFold_left : ('a -> 'b -> ('c,'a) either) -> 'a -> 'b list -> ('c,'a) either =
    fun f val1 yt1 ->
    let rec go : 'a -> 'b list -> ('c,'a) either =
        fun v yt -> match yt with
            | (y::ys) ->
                begin match f v y with
                    | Left _ as failure -> failure
                    | Right success -> go success ys
                end
            | [] -> Right v
    in
        go val1 yt1

type literal =
    | LInt of int
    | LReal of float
    | LBool of bool
    | LString of string
    | LUnbound

module StringMap = CCMap.Make(String)
type simpleEnv = literal StringMap.t
let emptySimpleEnv = StringMap.empty

type environment = 
    {
        memory : simpleEnv;
        temp   : simpleEnv;
    }
let emptyEnv = { memory = emptySimpleEnv; temp = emptySimpleEnv; }

(* For easy construction of updater function in bindVariable. *)
let var_binder : literal -> literal option -> literal option =
    fun newval oldval ->
        match oldval with
         | Some _ -> Some newval
         | None -> None (* If the variable is not present, don't create it. *)

(* For easy creation of creation function in createVariable. *)
let var_creator : literal option -> literal option -> literal option =
    fun newval oldval ->
        match oldval,newval with
            | (Some x,_) -> Some x (* If the variable already exists, don't rebind it. *)
            | (None,Some _) -> newval
            | (None,None) -> Some LUnbound

(* bindVariable expects there to be a binding already present. *)
let bindVariable : environment -> string -> literal  -> environment =
    fun { memory = mem; temp = tmp; } name value ->
        let updater = var_binder value in
        { 
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

let createVariable : environment -> string -> literal option -> (error,environment) either =
    fun ({temp = tmp; } as env) name possibleValue ->
        match variableExists env name with
            | true -> Left ["Redeclaration of variable " ^ name ^ "."]
            | false -> let updater = var_creator possibleValue in
                Right { env with temp = (StringMap.update name updater tmp); }

let createMemory : environment -> string -> literal option -> (error,environment) either  =
    fun ({memory = mem; } as env) name possibleValue ->
        match variableExists env name with
            | true -> Left ["Redeclaration of variable " ^ name ^ "."]
            | false -> let updater = var_creator possibleValue in
                Right { env with memory = (StringMap.update name updater mem); }

(* Check family of functions. Checks that things are valid in the given environment. *)
let getNameFromNamedId : Types.named_id -> string =
    fun namedId ->
        match namedId with
            | SimpleId name -> name
            | NamedId (name,_) -> name          

let checkNamedId : environment -> Types.named_id -> error option =
    fun env namedId ->
        let name = getNameFromNamedId namedId in
        match variableExists env name with
            | true -> None 
            | false -> Some ["No declaration of variable " ^ name ^ "."]

let checkExp : environment -> Types.parse_exp -> error option =
    fun env exp ->
        let rec internalChecker : Types.parse_exp -> error option =
            fun exp -> match exp with
                | PId (name,_) -> checkNamedId env name
                | PBinOp (_,e1,e2) -> joinErrorOptions (internalChecker e1) (internalChecker e2)
                | PUnOp (_,e1) -> internalChecker e1
                | PCall _ -> None (* TODO: Function checks. *)
                | PGroup e1 -> internalChecker e1
                | PTuple es -> List.fold_left joinErrorOptions None (List.map internalChecker es)
                | _ -> None (* All others: Nothing to check. *)
        in
            internalChecker exp

let checkRegularValBind : environment -> Types.val_bind -> (error,environment) either  =
    fun env valbind ->
        match valbind with
            | ValBind (name,_,_) -> createVariable env (getNameFromNamedId name) None
            | ValNoBind (name,_) -> createVariable env (getNameFromNamedId name) None

let checkMemValBind : environment -> Types.val_bind -> (error,environment) either  =
    fun env valbind ->
        match valbind with
            | ValBind (name,_,_) -> createMemory env (getNameFromNamedId name) None
            | ValNoBind (name,_) -> createMemory env (getNameFromNamedId name) None

let rec checkStmt : environment -> Types.stmt -> (error,environment) either =
    fun env stmt ->
        match stmt with
            | StmtVal valbinds -> eitherFold_left checkRegularValBind env valbinds
            | StmtMem valbinds -> eitherFold_left checkMemValBind env valbinds
            | StmtReturn exp ->
                begin match checkExp env exp with
                    | None -> Right env
                    | Some errs -> Left ("Could not evaluate expression in return statement."::errs)
                end
            | StmtIf (cond,trueStmts,None) ->
                begin match checkExp env cond with
                    | Some errs -> Left ("Could not evaluate condition expression in if statement."::errs)
                    | None -> begin match eitherFold_left checkStmt env trueStmts with
                            | Right _ as success -> success
                            | Left errs -> Left ("In if body true-branch statements."::errs)
                        end
                end
            | StmtIf (cond,trueStmts,Some falseStmts) ->
                begin match checkExp env cond with
                    | Some errs -> Left ("Could not evaluate condition expression in if statement."::errs)
                    | None -> 
                        begin match eitherFold_left checkStmt env trueStmts with
                            | Left errs -> Left ("In if body true-branch statements."::errs)
                            | Right env2 ->
                                begin match eitherFold_left checkStmt env2 falseStmts with
                                    | Left errs -> Left ("In if body false-branch statements"::errs)
                                    | Right _ as success -> success
                                end
                        end
                end
            | StmtFun _ -> Right env (* TODO: Function evaluation. *)
            | StmtBind (PId (name,_),rhs) ->
                begin match checkNamedId env name with
                    | Some errs -> Left errs
                    | None -> begin match checkExp env rhs with
                        | Some errs -> Left ("Could not evaluate right hand side of bind."::errs)
                        | None -> Right env
                    end
                end
            | StmtBind _ -> Left ["Left hand side of bind is not a variable."]
            | StmtEmpty -> Right env

let checkStmts : Types.stmt list -> error option =
    fun stmts -> match eitherFold_left checkStmt emptyEnv stmts with
        | Right _ -> None
        | Left errs -> Some errs

let programState : Types.stmt list option -> unit =
    fun maybeStmts ->
        match maybeStmts with
            | None -> print_string "Parse unsuccessful; no checking possible.\n"
            | Some stmts -> begin match checkStmts stmts with
                | None -> print_string "Program checked succesfully.\n"
                | Some errsRev ->
                    let
                        errs = List.rev errsRev
                    in
                        print_string "Program checked unsuccessfully, errors:\n" ;
                        List.iter (print_endline) errs;
                        ()
                end











