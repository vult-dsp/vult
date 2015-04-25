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

(** Dynamic-typing interpreter used as reference *)

open TypesVult
open TypesUtil
open Scope

let apply_default (f:'a -> 'b) (v:'a option) (def:'b) =
   match v with
   | Some(x) -> f x
   | _ -> def

(** Joins a list of strings *)
let rec joinStrings sep elems =
   match elems with
   | [] -> ""
   | [h] -> h
   | h::t -> h^sep^(joinStrings sep t)

(** Return values of the interpreter *)
type value =
   | VUnit
   | VString  of string
   | VNum     of float
   | VBool    of bool
   | VTuple   of value list

(** Used to define the kind of a function: builtin and declared by the user.
    The body of the function is stored in order to evaluate it. *)
type function_body =
   | BuiltinF  of (value list -> value)
   | DeclaredF of exp

(** Used to store the type definitions *)
type type_body =
   | BuiltinT  of value
   | DeclaredT of exp

type obj =
   | Value    of value
   | Function of function_body
   | Type     of type_body


(** Converts a value to string *)
let rec valueStr (value:value) : string =
   match value with
   | VUnit         -> "()"
   | VNum(v)       -> string_of_float v
   | VString(s)    -> "\""^s^"\""
   | VBool(true)   -> "true"
   | VBool(false)  -> "false"
   | VTuple(elems) ->
      let elems_s = elems
                    |> List.map valueStr
                    |> joinStrings ","
      in "("^elems_s^")"

module IdObjSig =
struct
   type t = identifier
   type v = obj
   let compare  = compare_identifier
   let string_t = identifierStr
   let string_v o =
      match o with
      | Value(v)    -> valueStr v
      | Function(_) -> "function"
      | Type(_)     -> "type"
end

module IdScope = Scope(IdObjSig)
(** Environment of the interpreter used to store all bindings and declarations *)
type local_env = IdScope.t

(** Returns the name in a named_id *)
let getVarName (named_id:named_id) : identifier =
   match named_id with
   | SimpleId(name,_) -> name
   | NamedId (name,_,_) -> name

(** Returns the name in an id expression *)
let getExpName (exp:exp) : identifier =
   match exp with
   | PId(name,_,_) ->  name
   | _ -> failwith "This expression should be an id"

(** Returns the statements of a declared function *)
let getFunctionBody (loc:local_env) (name:identifier) : function_body =
   (*let _ = Printf.printf "Searching function body '%s'\n" (identifierStr name) in*)
   match IdScope.lookup loc name with
   | Some(Function(body)) ->
      (*let _ = print_endline "Found" in*)
      body
   | _ ->
      (*let _ = print_endline "Not found" in*)
      failwith ("Unknown function "^(identifierStr name))

(** Adds a declared function to the environment *)
let declFunction (loc:local_env) (name:identifier) (body:function_body) : local_env =
   (*let _ = Printf.printf "Declaring function body '%s'\n" (identifierStr name) in*)
   IdScope.bind loc name (Function(body))

(** Adds a declared function to the environment *)
let declType (loc:local_env) (name:identifier) (body:type_body) : local_env =
   (*let _ = Printf.printf "Declaring type body '%s'\n" (identifierStr name) in*)
   IdScope.bind loc name (Type(body))

(** Clears a local environment *)
let clearLocal (loc:local_env) : local_env =
   loc

(** Pushes a new local variable environment *)
let pushLocal (loc:local_env) (optname:identifier option) : local_env =
   match optname with
   | Some(name) ->
      (*let _ = Printf.printf "Opening scope %s\n" (identifierStr name) in*)
      let loc = IdScope.enter loc name in
      (*let _ = IdScope.printScope loc in*)
      loc
   | _ -> loc

(** Pops the local variable environment *)
let popLocal (loc:local_env) (optname:identifier option) : local_env =
   match optname with
   | Some(name) ->
      (*let _ = Printf.printf "Closing scope %s\n" (identifierStr name) in*)
      let loc = IdScope.exit loc in
      (*let _ = IdScope.printScope loc in*)
      loc
   | _ -> loc


(** Returns the value for the given variable *)
let getExpValueFromEnv (loc:local_env) (name:identifier) : value =
   (*let _ = Printf.printf "Looking up for value '%s'\n" (identifierStr name) in*)
   match IdScope.lookup loc name with
   | Some(Value(v)) ->
      (*let _ = Printf.printf "Got %s\n" (valueStr v) in*)
      v
   | Some(_)        -> failwith "getExpValueFromEnv: not a value"
   | None           -> failwith ("getExpValueFromEnv: not found"^(identifierStr name))

(** Sets the value of a given variable *)
let setValMem (loc:local_env) (name:identifier) (value:value) : local_env =
   (*let _ = Printf.printf "Updating value for '%s' to %s\n" (identifierStr name) (valueStr value) in*)
   IdScope.rebind loc name (Value(value))

(** Declares a variable name *)
let declVal (loc:local_env) (name:identifier) (value:value) : local_env =
   (*let _ = Printf.printf "Setting val value for '%s' to %s\n" (identifierStr name) (valueStr value) in*)
   IdScope.bind loc name (Value(value))

(** Declares a memory name *)
let declMem (loc:local_env) (name:identifier) (init:value) : local_env =
   match IdScope.lookup loc name with
   | None ->
      (*let _ = Printf.printf "Setting mem value for '%s' to %s\n" (identifierStr name) (valueStr init) in*)
      let loc = IdScope.bind loc name (Value(init)) in
      loc
   | Some(_) -> loc

(** Returns true if the value is zero *)
let isTrue (value:value) : bool =
   match value with
   | VNum(0.0)   -> false
   | VString("") -> false
   | VBool(v)    -> v
   | _           -> true

(** Evaluates a function call *)
let rec evalFun (loc:local_env) (scope_name:identifier option) (body:function_body) (args:value list) : value * local_env * bool =
   match body with
   | DeclaredF(StmtFun(_,arg_names,stmts,type_exp,active,_)) ->
      let inputs = List.map getVarName arg_names in
      let loc = List.fold_left2 (fun s n v -> declVal s n v) loc inputs args in
      runStmtList loc scope_name [stmts]
   | BuiltinF(f) ->
      f args,loc,false
   | _ -> failwith "Invalid function body"

(** Evaluates an expression or statement *)
and runExp (loc:local_env) (exp:exp) : value * local_env * bool =
   match exp with
   | PUnit(_)   -> VUnit,loc,false
   | PBool(v,_) -> VBool(v),loc,false
   | PInt(v,_)  -> VNum(float_of_string v),loc,false
   | PReal(v,_) -> VNum(float_of_string v),loc,false
   | PId(name,_,_)  ->
      getExpValueFromEnv loc name,loc,false
   | PTyped(e,_,_) -> runExp loc e
   | PGroup(e,_)  -> runExp loc e
   | PTuple(elems,_) ->
      let elems_val,loc = runExpList loc elems in
      VTuple(elems_val),loc,false
   | PIf(cond,then_exp,else_exp,_) ->
      let cond_val,loc,_ = runExp loc cond in
      if isTrue cond_val then
         runExp loc then_exp
      else
         runExp loc else_exp
   | PCall(name,ftype,args,_,_) ->
      let loc          = pushLocal loc name in
      let body         = getFunctionBody loc ftype in
      let args_val,loc = runExpList loc args in
      let result,loc,_ = evalFun loc name body args_val in
      let loc          = popLocal loc name in
      result,loc,false
   | PEmpty -> failwith "There should not be Empty expressions when calling the intepreter"
   | PBinOp(_,_,_,_)
   | PUnOp(_,_,_) -> failwith "There should not be operators when calling the intepreter"
   | StmtVal(PId(name,_,_),opt_init,_) ->
      let init,loc,_ = apply_default (runExp loc) opt_init (VNum(0.0),loc,false) in
      VUnit,declVal loc name init,false
   | StmtMem(PId(name,_,_),opt_init,None,_) ->
      let init,loc,_ = apply_default (runExp loc) opt_init (VNum(0.0),loc,false) in
      VUnit,declMem loc name init,false
   | StmtVal(_)
   | StmtMem(_) -> failwith "Declarations with more that one element should have been removed by the transformations"
   | StmtBind(PId(name,_,_),rhs,_) ->
      let rhs_val,loc,_ = runExp loc rhs in
      VUnit,setValMem loc name rhs_val,false
   | StmtBind(PTuple(elems,_),rhs,_) ->
      let vnames = List.map getExpName elems in
      let rhs_val,loc,_ = runExp loc rhs in
      begin
         match rhs_val with
         | VTuple(elems_val) ->
            VUnit,List.fold_left2 (fun s n v -> setValMem s n v) loc vnames elems_val,false
         | _ -> failwith "Not returning a tuple"
      end
   | StmtBind(PUnit(_),rhs,_) ->
      let _,loc,_ = runExp loc rhs in
      VUnit,loc,false
   | StmtBind(_,rhs,_) -> failwith "Invalid binding"
   | StmtReturn(e,_) ->
      let e_val,loc,_ = runExp loc e in
      e_val,loc,true
   | StmtType(name,_,_,_) ->
      let loc = declType loc name (DeclaredT(exp)) in
      VUnit,loc,false
   | StmtAliasType(name,_,_,_) ->
      let loc = declType loc name (DeclaredT(exp)) in
      VUnit,loc,false
   | StmtFun(name,_,_,_,_,_) ->
      let loc = declFunction loc name (DeclaredF(exp)) in
      VUnit,loc,false
   | StmtIf(cond,then_stmts,None,_) ->
      let cond_val,loc,_ = runExp loc cond in
      if isTrue cond_val then
         runExp loc then_stmts
      else VUnit,loc,false
   | StmtIf(cond,then_stmts,Some(else_stmts),_) ->
      let cond_val,loc,_ = runExp loc cond in
      if isTrue cond_val then
         runExp loc then_stmts
      else
         runExp loc else_stmts
   | StmtEmpty -> VUnit,loc,false
   | PSeq(name,stmts,_) ->
      let loc          = pushLocal loc name in
      let v,loc,is_ret = runStmtList loc name stmts in
      let loc          = popLocal loc name in
      v,loc,is_ret
   | StmtBlock(name,stmts,_) ->
      let loc          = pushLocal loc name in
      let v,loc,is_ret = runStmtList loc name stmts in
      let loc          = popLocal loc name in
      v,loc,is_ret
   | StmtWhile(cond,stmts,_) ->
      let cond_val,loc,_ = runExp loc cond in
      let rec loop cond_val loc n =
         if isTrue cond_val then
            let ret,loc,is_ret = runExp loc stmts in
            if is_ret then
               ret,loc,is_ret
            else
               let new_cond_val,loc,_ = runExp loc cond in
               (* Hard coded maximum number of iterations *)
               if n<1000000 then
                  loop new_cond_val loc (n+1)
               else
                  VUnit,loc,true
         else
            VUnit,loc,false
      in loop cond_val loc 0

(** Evaluates a list of expressions *)
and runExpList (loc:local_env) (expl:exp list) : value list * local_env =
   let loc,acc = List.fold_left (fun (s,acc) a -> let v,ns,_ = runExp s a in ns,v::acc) (loc,[]) expl in
   List.rev acc,loc

(** Evaluates a list of statements *)
and runStmtList (loc:local_env) (name:identifier option) (expl:exp list) : value * local_env * bool =
   let rec loop loc stmts =
      match stmts with
      | [] -> VUnit,loc,false
      | h::t ->
         let value,loc,is_ret = runExp loc h in
         if is_ret then
            value,loc,true
         else loop loc t
   in
   let ret,loc,is_ret = loop loc expl in
   ret,loc,is_ret

(** Used to create functions that take one number and return one number *)
let opNumNum (op:float->float) (args:value list) =
   match args with
   | [VNum(v1)] -> VNum(op v1)
   | _ -> failwith "opNumNum: Invalid arguments"

(** Used to create functions that take two numbers and return one number *)
let opNumNumNum (op:float->float->float) (args:value list) =
   match args with
   | [VNum(v1); VNum(v2)] -> VNum(op v1 v2)
   | _ -> failwith "opNumNumNum: Invalid arguments"

(** Used to create functions that take two numbers and return one boolean *)
let opNumNumBool (op:float->float->bool) (args:value list) =
   match args with
   | [VNum(v1); VNum(v2)] -> VBool(op v1 v2)
   | _ -> failwith "opNumNumBool: Invalid arguments"

(** Used to create functions that take two booleans and return one boolean *)
let opBoolBoolBool (op:bool->bool->bool) (args:value list) =
   match args with
   | [VBool(v1); VBool(v2)] -> VBool(op v1 v2)
   | _ -> failwith "opBoolBoolBool: Invalid arguments"

(** Used to create functions that take one booleans and return one boolean *)
let opBoolBool (op:bool->bool) (args:value list) =
   match args with
   | [VBool(v1)] -> VBool(op v1)
   | [v] -> VBool(op (isTrue v))
   | _ -> failwith "opBoolBool: Invalid arguments"

(** Adds all the builtin functions to the environment *)
let addBuiltinFunctions (loc:local_env) : local_env =
   let plus = opNumNumNum (+.) in
   let mult = opNumNumNum ( *. ) in
   let minus = fun args ->
      match args with
      | [_] -> opNumNumNum (-.) (VNum(0.0)::args)
      | _ -> opNumNumNum (-.) args
   in
   let div           = opNumNumNum (/.) in
   let equal         = opNumNumBool (=) in
   let unequal       = opNumNumBool (<>) in
   let smaller       = opNumNumBool (<) in
   let larger        = opNumNumBool (>) in
   let smaller_equal = opNumNumBool (<=) in
   let larger_equal  = opNumNumBool (>=) in
   let or_op         = opBoolBoolBool (||) in
   let and_op        = opBoolBoolBool (&&) in
   let not_op        = opBoolBool (not) in
   let tanh_fun      = opNumNum tanh in
   let abs_fun       = opNumNum abs_float in
   let floor_fun     = opNumNum floor in
   let sin_fun       = opNumNum sin in
   let fixdenorm     = opNumNum (fun a -> if (abs_float a)<1e-12 then 0.0 else a) in
   let fexp          = opNumNum (fun a -> exp a) in
   let print_fun args   = List.map valueStr args |> joinStrings "," |> (fun a -> print_string a;VUnit) in
   let println_fun args = List.map valueStr args |> joinStrings "," |> (fun a -> print_endline a;VUnit) in
   [
      ["'+'"],BuiltinF(plus);
      ["'-'"],BuiltinF(minus);
      ["'*'"],BuiltinF(mult);
      ["'/'"],BuiltinF(div);
      ["'=='"],BuiltinF(equal);
      ["'!='"],BuiltinF(unequal);
      ["'<'"],BuiltinF(smaller);
      ["'>'"],BuiltinF(larger);
      ["'<='"],BuiltinF(smaller_equal);
      ["'>='"],BuiltinF(larger_equal);
      ["'||'"],BuiltinF(or_op);
      ["'&&'"],BuiltinF(and_op);
      ["'!'"],BuiltinF(not_op);
      ["print"],BuiltinF(print_fun);
      ["println"],BuiltinF(println_fun);
      ["tanh"],BuiltinF(tanh_fun);
      ["abs"],BuiltinF(abs_fun);
      ["floor"],BuiltinF(floor_fun);
      ["sin"],BuiltinF(sin_fun);
      ["fixdenorm"],BuiltinF(fixdenorm);
      ["exp"],BuiltinF(fexp);
   ]
   |> List.fold_left (fun env (a,b) -> declFunction env a b) loc

(** Main function that takes a parse program and runs it*)
let interpret (results:parser_results) : interpreter_results =
   let loc = IdScope.empty |> addBuiltinFunctions in
   try

      let result = CCError.map (fun stmts -> let ret,loc,_ = runStmtList loc None stmts in ret,loc) results.presult in
      match result with
      | `Ok(ret,loc) ->
         { iresult = `Ok(valueStr ret); lines = results.lines }
      | `Error(error) ->
         { iresult = `Error(error); lines = results.lines }
   with
   | Failure(msg) ->
      let _ = print_endline msg in
      { iresult = `Error([SimpleError(msg)]); lines = results.lines }

