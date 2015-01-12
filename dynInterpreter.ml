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

open Types
open TypesUtil

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

(** Used to define types of functions: builtin and declared by the user *)
type function_body =
   | Builtin  of (value list -> value)
   | Declared of parse_exp

(** Environment of the interpreter used to store all bindings and declarations *)
type local_env =
   {
      val_binds : (value IdentifierMap.t) list;
      mem_binds : value IdentifierMap.t;
      fun_bind  : local_env IdentifierMap.t;
      fun_decl  : function_body IdentifierMap.t;
   }

(** Creates an environment. It optionally receives the function declarations.  *)
let newLocalEnv (fun_decl:(function_body IdentifierMap.t) option) =
   let functions =
      match fun_decl with
      | Some(a) -> a
      | _ -> IdentifierMap.empty
   in
   {
      val_binds = [];
      mem_binds = IdentifierMap.empty;
      fun_bind  = IdentifierMap.empty;
      fun_decl  = functions;
   }

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

(** Prints the current state of the environment *)
let localEnvStr (loc:local_env) : string =
   let dumpIdentifierMap env =
      IdentifierMap.fold (fun name value state -> state^(identifierStr name)^" = "^(valueStr value)^"\n" ) env ""
   in
   let val_s = List.map dumpIdentifierMap loc.val_binds |> joinStrings "\n" in
   let mem_s = dumpIdentifierMap loc.mem_binds in
   let fun_s = IdentifierMap.fold (fun name value state -> (identifierStr name)::state ) loc.fun_bind [] |> joinStrings "," in
   Printf.sprintf "= val =\n%s= mem =\n%s= fun =\n%s\n" val_s mem_s fun_s

(** Returns the name in a named_id *)
let getVarName (named_id:named_id) : identifier =
   match named_id with
   | SimpleId(name,_) -> name
   | NamedId (name,_,_,_) -> name

(** Returns the name in an id expression *)
let getExpName (exp:parse_exp) : identifier =
   match exp with
   | PId(name) -> getVarName name
   | _ -> failwith "This expression should be an id"

(** Returns the statements of a declared function *)
let getFunctionBody (loc:local_env) (name:identifier) : function_body =
   if IdentifierMap.mem name loc.fun_decl then
      IdentifierMap.find name loc.fun_decl
   else
      failwith ("Unknown function "^(identifierStr name))

(** Adds a declared function to the environment *)
let declFunction (loc:local_env) (name:identifier) (body:function_body) : local_env =
   { loc with fun_decl = IdentifierMap.add name body loc.fun_decl }

(** Gets the local environment for a function call *)
let getFunctionEnv (loc:local_env) (name:identifier) : local_env =
   if name = ["_"] then newLocalEnv (Some(loc.fun_decl)) else
   if IdentifierMap.mem name loc.fun_bind then
      IdentifierMap.find name loc.fun_bind
   else
      let env = newLocalEnv (Some(loc.fun_decl)) in
      let _ = IdentifierMap.add name env loc.fun_bind in
      env

(** Adds a local environment for a function call *)
let setFunctionEnv (loc:local_env) (name:identifier) (floc:local_env) : local_env =
   if name = ["_"] then loc else
      { loc with fun_bind = IdentifierMap.add name floc loc.fun_bind }

(** Clears a local environment *)
let clearLocal (loc:local_env) : local_env =
   { loc with val_binds = [] }

(** Pushes a new local variable environment *)
let pushLocal (loc:local_env) : local_env =
   { loc with val_binds = (IdentifierMap.empty)::loc.val_binds }

(** Pops the local variable environment *)
let popLocal (loc:local_env) : local_env =
   match loc.val_binds with
   | [] -> loc
   | _::t -> { loc with val_binds = t }

(** Returns the value for the given variable *)
let getExpValueFromEnv (loc:local_env) (name:identifier) : value =
   let rec loop locals =
      match locals with
      | [] ->
         if IdentifierMap.mem name loc.mem_binds then
            IdentifierMap.find name loc.mem_binds
         else
            let _ = print_string (localEnvStr loc) in
            failwith ("Undeclared variable "^(identifierStr name))
      | h::t ->
         if IdentifierMap.mem name h then
            IdentifierMap.find name h
         else loop t
   in loop loc.val_binds

(** Sets the value of a given variable *)
let setValMem (loc:local_env) (name:identifier) (value:value) : local_env =
   let rec loop locals acc =
      match locals with
      | [] ->
         if IdentifierMap.mem name loc.mem_binds then
            { loc with mem_binds = IdentifierMap.add name value loc.mem_binds }
         else
            let _ = print_string (localEnvStr loc) in
            failwith ("Undeclared variable "^(identifierStr name))
      | h::t ->
         if IdentifierMap.mem name h then
            { loc with val_binds = (List.rev acc)@[IdentifierMap.add name value h]@t}
         else loop t (h::acc)
   in loop loc.val_binds []

(** Declares a variable name *)
let declVal (loc:local_env) (name:identifier) (value:value) : local_env =
   match loc.val_binds with
   | [] ->
      let new_env = IdentifierMap.add name value (IdentifierMap.empty) in
      { loc with val_binds = [new_env] }
   | h::t ->
      let new_env = IdentifierMap.add name value h in
      { loc with val_binds = new_env::t }

(** Declares a memory name *)
let declMem (loc:local_env) (name:identifier) (init:value) : local_env =
   if not (IdentifierMap.mem name loc.mem_binds) then
      { loc with mem_binds = IdentifierMap.add name init loc.mem_binds }
   else loc

(** Returns true if the value is zero *)
let isTrue (value:value) : bool =
   match value with
   | VNum(0.0)   -> false
   | VString("") -> false
   | VBool(v)    -> v
   | _           -> true

(** Evaluates a function call *)
let rec evalFun (loc:local_env) (body:function_body) (args:value list) : value * local_env * bool =
   match body with
   | Declared(StmtFun(_,arg_names,stmts,_)) ->
      let inputs = List.map getVarName arg_names in
      let loc = List.fold_left2 (fun s n v -> declVal s n v) loc inputs args in
      runStmtList loc [stmts]
   | Builtin(f) ->
      f args,loc,false
   | _ -> failwith "Invalid function body"

(** Evaluates an expression or statement *)
and runExp (loc:local_env) (exp:parse_exp) : value * local_env * bool =
   match exp with
   | PUnit(_)   -> VUnit,loc,false
   | PBool(v,_) -> VBool(v),loc,false
   | PInt(v,_)  -> VNum(float_of_string v),loc,false
   | PReal(v,_) -> VNum(float_of_string v),loc,false
   | PId(name)  ->
      let vname  = getVarName name in
      getExpValueFromEnv loc vname,loc,false
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
   | PCall(name_id,args,_,_) ->
      let name,ftype = TypesUtil.getFunctionTypeAndName name_id in
      let body = getFunctionBody loc ftype in
      let args_val,loc = runExpList loc args in
      let env = getFunctionEnv loc name in
      let env = clearLocal env in
      let result,env,_ = evalFun env body args_val in
      let loc = setFunctionEnv loc name env in
      result,loc,false
   | PEmpty -> failwith "There should not be Empty expressions when calling the intepreter"
   | PBinOp(_,_,_,_)
   | PUnOp(_,_,_) -> failwith "There should not be operators when calling the intepreter"
   | StmtVal(PId(name),opt_init,_) ->
      let vname = getVarName name in
      let init,loc,_ = apply_default (runExp loc) opt_init (VNum(0.0),loc,false) in
      VUnit,declVal loc vname init,false
   | StmtMem(PId(name),opt_init,None,_) ->
      let vname = getVarName name in
      let init,loc,_ = apply_default (runExp loc) opt_init (VNum(0.0),loc,false) in
      VUnit,declMem loc vname init,false
   | StmtVal(_)
   | StmtMem(_) -> failwith "Declarations with more that one element should have been removed by the transformations"
   | StmtBind(PId(name),rhs,_) ->
      let rhs_val,loc,_ = runExp loc rhs in
      let vname = getVarName name in
      VUnit,setValMem loc vname rhs_val,false
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
   | StmtFun(name_id,_,_,_) ->
      let _,ftype = TypesUtil.getFunctionTypeAndName name_id in
      let loc = declFunction loc ftype (Declared(exp)) in
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
   | PSeq(stmts,_) ->
      runStmtList loc stmts
   | StmtBlock(stmts,_) ->
      runStmtList loc stmts

(** Evaluates a list of expressions *)
and runExpList (loc:local_env) (expl:parse_exp list) : value list * local_env =
   let loc,acc = List.fold_left (fun (s,acc) a -> let v,ns,_ = runExp s a in ns,v::acc) (loc,[]) expl in
   List.rev acc,loc

(** Evaluates a list of statements *)
and runStmtList (loc:local_env) (expl:parse_exp list) : value * local_env * bool =
   let loc = pushLocal loc in
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
   let loc = popLocal loc in
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
   let print_fun args   = List.map valueStr args |> joinStrings "," |> (fun a -> print_string a;VUnit) in
   let println_fun args = List.map valueStr args |> joinStrings "," |> (fun a -> print_endline a;VUnit) in
   [
      ["'+'"],Builtin(plus);
      ["'-'"],Builtin(minus);
      ["'*'"],Builtin(mult);
      ["'/'"],Builtin(div);
      ["'=='"],Builtin(equal);
      ["'!='"],Builtin(unequal);
      ["'<'"],Builtin(smaller);
      ["'>'"],Builtin(larger);
      ["'<='"],Builtin(smaller_equal);
      ["'>='"],Builtin(larger_equal);
      ["'||'"],Builtin(or_op);
      ["'&&'"],Builtin(and_op);
      ["'!'"],Builtin(not_op);
      ["print"],Builtin(print_fun);
      ["println"],Builtin(println_fun);
      ["tanh"],Builtin(tanh_fun);
      ["abs"],Builtin(abs_fun);
      ["floor"],Builtin(floor_fun);
      ["sin"],Builtin(sin_fun);
      ["fixdenorm"],Builtin(fixdenorm);
   ]
   |> List.fold_left (fun env (a,b) -> declFunction env a b) loc

(** Main function that takes a parse program and runs it*)
let interpret (results:parser_results) : interpreter_results =
   let loc = newLocalEnv None |> addBuiltinFunctions in
   try

      let result = CCError.map (fun stmts -> let ret,loc,_ = runStmtList loc stmts in ret,loc) results.presult in
      match result with
      | `Ok(ret,loc) ->
         (*let _ = print_string (localEnvStr loc) in*)
         { iresult = `Ok(valueStr ret); lines = results.lines }
      | `Error(error) ->
         { iresult = `Error(error); lines = results.lines }
   with
   | Failure(msg) -> { iresult = `Error([SimpleError(msg)]); lines = results.lines }
