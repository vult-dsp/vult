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

(** Vult abstract syntax interpreter *)
open Types
open TypesUtil
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
   | LTuple of literal list
   | LUnit
   | LUnbound

type vultFunction =
   {
      functionname : string;
      returntype   : string option;
      inputs       : named_id list;
      body         : parse_exp list;
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

let rec literalTypeString : literal -> string =
   fun lit -> match lit with
      | LInt _ -> "integer"
      | LReal _ -> "real"
      | LBool _ -> "bool"
      | LString _ -> "string"
      | LTuple xs -> "(" ^ (String.concat ", " (List.map literalTypeString xs)) ^ ")"
      | LUnit -> "unit"
      | LUnbound -> "undecided"

let rec literalTypeEqual : literal -> literal -> bool =
   fun lit1 lit2 -> match (lit1,lit2) with
      | (LInt _), (LInt _) -> true
      | (LInt _), (LReal _) -> true (* Assume ints can be coerced to reals *)
      | (LReal _), (LInt _) -> true
      | (LReal _), (LReal _) -> true
      | (LBool _), (LBool _) -> true
      | (LString _), (LString _) -> true
      | (LTuple xs), (LTuple ys) -> literalTypeEqualList xs ys
      | LUnit,LUnit -> true
      | LUnbound,_ -> true
      | _,LUnbound -> true
      | _ -> false

and literalTypeEqualList : literal list -> literal list -> bool =
   fun lits1 lits2 -> match (lits1,lits2) with
      | (l1::r1), (l2::r2) -> if literalTypeEqual l1 l2 then literalTypeEqualList r1 r2 else false
      | [], [] -> true
      | _ -> false


(* Processing of functions. *)
let isFunctionStmt : parse_exp -> bool =
   fun stmt ->
      match stmt with
      | StmtFun _ -> true
      | _ -> false

let bindFunction : functionBindings -> vultFunction -> (errors,functionBindings) either =
   fun binds ({functionname = funcname;} as f) ->
      match StringMap.get funcname binds with
      | None -> Right (StringMap.add funcname f binds)
      | Some _ -> Left ([SimpleError("Redeclaration of function " ^ funcname ^ ".")])

let getFunction : environment -> string -> vultFunction option =
   fun {functions = binds;} fname -> StringMap.get fname binds

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

let variableBinding : environment -> string -> literal option =
   fun { memory = mem; temp = tmp; } name ->
      match StringMap.get name tmp with
      | Some _ as ret -> ret
      | None -> match StringMap.get name mem with
         | Some _ as ret -> ret
         | None as ret -> ret

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
      | PGroup (exp,loc) -> flattenExp exp
      | PTuple(es,_) -> es
      | e -> [e]


let checkUnit : environment -> (Types.error, literal) either =
   fun _ -> Right LUnit

let checkInt : 'a -> string -> location -> (Types.error, literal) either =
   fun _ intstring loc ->
      try
         Right (LInt (int_of_string intstring))
      with Failure _ -> Left (PointedError(loc,"The literal " ^ intstring ^ " can not be interpreted as an integer."))

let checkReal : 'a -> string -> location -> (Types.error, literal) either =
   fun _ realstring loc ->
      try
         Right (LReal (float_of_string realstring))
      with Failure _ -> Left (PointedError(loc,"The literal " ^ realstring ^ " can not be interpreted as a real."))

let checkId : environment -> named_id -> (Types.error, literal) either =
   fun env namedid ->
      let vname = getNameFromNamedId namedid in
      match variableBinding env vname with
      | Some lit -> Right lit
      | None -> Left (SimpleError ("The variable " ^ vname ^ " is used in an expression but has no value binding at this point."))

let checkUnOp : 'a -> string -> literal -> location -> (Types.error, literal) either =
   fun _ op lit loc ->
      let
         minusCheck lit = match lit with
         | LInt x -> Right (LInt (- x))
         | LReal x -> Right (LReal (-. x))
         | LUnbound -> Right (LUnbound)
         | l -> Left (PointedError(loc,"Operation unary minus is not compatible with type " ^ (literalTypeString lit) ^ "."))
      in let   notCheck lit = match lit with
         | LBool x -> Right (LBool (not x))
         | l -> Left (PointedError(loc,"Operation boolean negation is not compation with type " ^ (literalTypeString lit) ^ "."))
      in match op with
      | "-" -> minusCheck lit
      | "!" -> notCheck lit
      | _ -> Left (PointedError(loc,"Unary operator " ^ op ^ " not recognized."))

let checkBinOp : 'a -> string -> literal -> literal -> location -> (Types.error, literal) either =
   fun _ op lit1 lit2 loc ->
      if not (literalTypeEqual lit1 lit2)
      then Left (PointedError(loc,"Type missmatch for operator " ^ op ^ ", 
                  operand 1 is of type " ^ (literalTypeString lit1) ^ " and 
                  operand 2 is of type " ^ (literalTypeString lit2) ^ "."
                             ))
      else match op with (* Just some filler values for now *)
         | "+" -> Right lit1
         | "-" -> Right lit1
         | "*" -> Right lit1
         | "/" -> Right lit1
         | "<" -> Right (LBool true)
         | ">" -> Right (LBool true)
         | "<=" -> Right (LBool true)
         | ">=" -> Right (LBool true)
         | "==" -> Right (LBool true)
         | "!=" -> Right (LBool true)
         | _ -> Left (PointedError(loc,"Unrecognized binary operator " ^ op ^ ".")) 

let checkCall : environment -> named_id -> literal list -> location -> (Types.error, literal) either =
   fun env namedid args loc ->
      let fname = getNameFromNamedId namedid in
      match getFunction env fname with
      | None -> Left (PointedError(loc,"Could not locate function " ^ fname ^ "."))
      | Some f ->
         let expectedlength = List.length f.inputs
         in let paramlength = List.length args
         in if expectedlength == paramlength
         then Right LUnbound
         else Left (PointedError(loc,"Wrong number of arguments in call to function " ^ fname
                                     ^ "; expected " ^ (string_of_int expectedlength) ^ " but got "
                                     ^ (string_of_int paramlength) ^ "."))

let checkIf : 'a -> literal -> literal -> literal -> (Types.error, literal) either =
   fun _ cond l1 l2 ->
      if not (literalTypeEqual l1 l2)
      then Left (SimpleError("Type missmatch in if-expression, 
            the received types are " ^ (literalTypeString l1) ^ " and " ^ (literalTypeString l2) ^ "."))
      else match cond with
         | LBool true -> Right l1
         | LBool false -> Right l2
         | LUnbound -> Right l1
         | _ -> Left (SimpleError("Condition in if-expression is not of type bool, but of type " ^ literalTypeString cond ^ "."))

let checkGroup : 'a -> literal -> (Types.error, literal) either =
   fun _ lit -> Right lit

let checkTuple : 'a -> literal list -> (Types.error, literal) either =
   fun _ lits -> match lits with
      | [] -> Right LUnit
      | xs -> Right (LTuple xs)

let checkEmpty : 'a -> (Types.error, literal) either =
   fun _ -> Right LUnit

let checkFunctions : (environment, error, literal) TypesUtil.expfold =
   {
      vUnit  = checkUnit;
      vInt   = checkInt;
      vReal  = checkReal;
      vId    = checkId;
      vUnOp  = checkUnOp;
      vBinOp = checkBinOp;
      vCall  = checkCall;
      vIf    = checkIf;
      vGroup = checkGroup;
      vTuple = checkTuple;
      vEmpty = checkEmpty;
   }

let rec checkExp : environment -> Types.parse_exp -> Types.errors option =
   fun env exp -> match TypesUtil.expressionFoldEither checkFunctions env exp with
      | Left err -> Some [err]
      | Right _ -> None

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

and checkStmt : environment -> parse_exp -> (errors,environment) either =
   fun env stmt ->
      match stmt with
      | StmtVal (valbinds,loc) -> eitherFold_left checkRegularValBind env valbinds
      | StmtMem (valbinds,loc) -> eitherFold_left checkMemValBind env valbinds
      | StmtReturn (exp,loc) ->
         begin match checkExp env exp with
            | None -> Right env
            | Some errs -> Left (SimpleError("Could not evaluate expression in return statement.")::errs)
         end
      | StmtIf (cond,trueStmts,None,loc) ->
         begin match checkExp env cond with
            | Some errs -> Left (SimpleError("Could not evaluate condition expression in if statement.")::errs)
            | None -> begin match eitherFold_left checkStmt env trueStmts with
                  | Right _ as success -> success
                  | Left errs -> Left (SimpleError("In if body true-branch statements.")::errs)
               end
         end
      | StmtIf (cond,trueStmts,Some falseStmts,loc) ->
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
      | StmtBind (PId (name),rhs,loc) ->
         begin match checkNamedId env name with
            | Some errs -> Left errs
            | None -> begin match checkExp env rhs with
                  | Some errs -> Left (SimpleError("Could not evaluate right hand side of bind.")::errs)
                  | None -> Right env
               end
         end
      | StmtBind _ -> Left [SimpleError("Left hand side of bind is not a variable.")]
      | StmtEmpty -> Right env
      | _ -> failwith "There should not be expressions here"

let checkStmts : environment -> parse_exp list -> errors option =
   fun env stmts ->
      match eitherFold_left checkStmt env stmts with
      | Right _ -> None
      | Left errs -> Some errs

let checkFunction : environment -> vultFunction -> errors option =
   fun env func ->
      match checkStmts env func.body with
      | Some errs -> Some (SimpleError("In function " ^ func.functionname ^ ".")::errs)
      | None -> None


let insertIfFunction : functionBindings -> parse_exp  -> (errors,functionBindings) either =
   fun funcs stmt ->
      match stmt with
      | StmtFun (namedid,inputs,body,loc) ->
         let funcname = getNameFromNamedId namedid in
         let functype = getTypeFromNamedId namedid in
         let vultfunc = { functionname = funcname; returntype = functype; inputs = inputs; body = body; } in
         bindFunction funcs vultfunc
      | _ -> Right funcs

let processFunctions : parse_exp list -> (errors,functionBindings) either =
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

let checkStmtsMain : parse_exp list -> errors option =
   fun stmts ->
      match processFunctions stmts with
      | Left errs -> Some errs
      | Right funcs ->  checkStmts {emptyEnv with functions = funcs} stmts

let programState : Types.parser_results -> interpreter_results =
   fun results ->
      match results.presult with
      | `Error(_) ->
         { iresult = `Error([SimpleError("Parse unsuccessful; no checking possible.")]); lines=results.lines }
      | `Ok stmts -> begin match checkStmtsMain stmts with
            | None -> { iresult = `Ok("()"); lines = results.lines }
            | Some errsRev ->
               let
                  errs = List.rev errsRev
               in
               { iresult = `Error(errs); lines = results.lines }
         end











