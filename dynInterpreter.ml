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

let apply_default (f:'a -> 'b) (v:'a option) (def:'b) =
   match v with
   | Some(x) -> f x
   | _ -> def

type value =
   | VUnit
   | VString  of string
   | VNum     of float
   | VBool    of bool
   | VTuple   of value list

type local_env =
   {
      val_binds : (string,value) Hashtbl.t;
      mem_binds : (string,value) Hashtbl.t;
      fun_bind  : (string,local_env) Hashtbl.t;
      mutable ret_val   : value option;
   }

type function_body =
   | Builtin  of (value list -> value)
   | Declared of stmt
   
type global_env =
   {
      fun_decl : (string,function_body) Hashtbl.t;
   }
   
let newLocalEnv () =
   {
      val_binds = Hashtbl.create 10;
      mem_binds = Hashtbl.create 10;
      fun_bind  = Hashtbl.create 10;
      ret_val   = None;
   }
   
let newGlobalEnv () =
   {
      fun_decl = Hashtbl.create 10;
   }

let rec joinStrings sep elems =
   match elems with
   | [] -> ""
   | [h] -> h
   | h::t -> h^sep^(joinStrings sep t)

let rec valueStr (value:value) : string =
   match value with
   | VUnit      -> "()"
   | VNum(v)    -> string_of_float v
   | VString(s) -> "\""^s^"\""
   | VBool(true) -> "true"
   | VBool(false) -> "false"
   | VTuple(elems) ->
      let elems_s = elems
         |> List.map valueStr
         |> joinStrings ","
      in "("^elems_s^")"

let localEnvStr (loc:local_env) : string =
   let val_s = Hashtbl.fold (fun name value state -> state^name^" = "^(valueStr value)^"\n" ) loc.val_binds "" in
   let mem_s = Hashtbl.fold (fun name value state -> state^name^" = "^(valueStr value)^"\n" ) loc.mem_binds "" in
   let fun_s = Hashtbl.fold (fun name value state -> name::state ) loc.fun_bind [] |> joinStrings "," in
   let ret_s = apply_default valueStr loc.ret_val "-" in
   Printf.sprintf "= val =\n%s= mem =\n%s= fun =\n%s\n= ret =\n%s\n" val_s mem_s fun_s ret_s
   
let getVarName (named_id:named_id) : string =
   match named_id with
   | SimpleId(name,_) -> name
   | NamedId (name,_,_,_) -> name

let getExpName (exp:parse_exp) : string =
   match exp with
   | PId(name) -> getVarName name
   | _ -> failwith "This expression should be an id"
   

let getFunctionBody (glob:global_env) (name:string) : function_body =
   if Hashtbl.mem glob.fun_decl name then
      Hashtbl.find glob.fun_decl name
   else
      failwith ("Unknown function "^name)

let default_env = newLocalEnv ()

let declFunction (glob:global_env) (name:string) (body:stmt) : unit =
   Hashtbl.replace glob.fun_decl name (Declared(body))

let getFunctionEnv (loc:local_env) (name:string) : local_env =
   if Hashtbl.mem loc.fun_bind name then
      Hashtbl.find loc.fun_bind name
   else
      let env = newLocalEnv () in
      let _ = Hashtbl.add loc.fun_bind name env in
      env

let clearLocal (loc:local_env) =
   let _ = Hashtbl.clear loc.val_binds in
   loc.ret_val <- None

let getExpValueFromEnv (loc:local_env) (name:string) : value = 
   if Hashtbl.mem loc.val_binds name then
     Hashtbl.find loc.val_binds name
   else
      if Hashtbl.mem loc.mem_binds name then
         Hashtbl.find loc.mem_binds name
      else
         failwith ("Undeclared variable "^name)

let setValMem (loc:local_env) (name:string)  (value:value) : unit = 
   if Hashtbl.mem loc.val_binds name then
     Hashtbl.replace loc.val_binds name value
   else
      if Hashtbl.mem loc.mem_binds name then
         Hashtbl.replace loc.mem_binds name value
      else
         failwith ("Undeclared variable "^name)
         
let setReturn (loc:local_env) (value:value) : unit =
   loc.ret_val <- Some(value)

let declVal (loc:local_env) (name:string) (value:value) : unit =
   Hashtbl.replace loc.val_binds name value
   
let declMem (loc:local_env) (name:string) (init:value) : unit =
   if not (Hashtbl.mem loc.mem_binds name) then
      Hashtbl.add loc.mem_binds name init

let isTrue (value:value) : bool =
   match value with
   | VNum(0.0) -> false
   | VString("") -> false
   | VBool(v) -> v
   | _ -> true

let rec getInputsNames (inputs:val_bind list) : string list =
   match inputs with
   | [] -> []
   | ValNoBind(name,_)::t ->
      (getVarName name)::(getInputsNames t)
   | _ -> failwith "Invalid function declaration"

let rec evalFun (glob:global_env) (loc:local_env) (body:function_body) (args:value list) : value =
   match body with
   | Declared(StmtFun(_,arg_names,stmts)) ->
      let inputs = getInputsNames arg_names in
      let _ = List.map2 (fun n v -> declVal loc n v) inputs args in
      let _ = runStmtList glob loc stmts in
      apply_default (fun a -> a) loc.ret_val  VUnit
   | Builtin(f) ->
      f args
   | _ -> failwith "Invalid function body"

and runExp (glob:global_env) (loc:local_env) (exp:parse_exp) : value =
   match exp with
   | PUnit      -> VUnit
   | PInt(v,_)  -> VNum(float_of_string v)
   | PReal(v,_) -> VNum(float_of_string v)
   | PId(name)  ->
      let vname  = getVarName name in
      getExpValueFromEnv loc vname
   | PGroup(e)  -> runExp glob loc e
   | PTuple(elems) ->
      let elems_val = List.map (runExp glob loc) elems in
      VTuple(elems_val)
   | PIf(cond,then_exp,else_exp) ->
      let cond_val = runExp glob loc cond in
      if isTrue cond_val then
         runExp glob loc then_exp
      else
         runExp glob loc else_exp
   | PCall(name_id,args,_) ->
      let name,ftype = TypesUtil.getFunctionTypeAndName name_id in
      let body = getFunctionBody glob ftype in
      let args_val = List.map (runExp glob loc) args in
      let env =
         if name="_" then
            default_env
         else
            getFunctionEnv loc name
      in
      let _ = clearLocal env in
      evalFun glob env body args_val
   | PEmpty -> failwith "There should not be Empty expressions when calling the intepreter"
   | PBinOp(_,_,_,_)
   | PUnOp(_,_,_) -> failwith "There should not be operators when calling the intepreter"

and runStmt (glob:global_env) (loc:local_env) (stmt:stmt) : unit =
   match stmt with
   | StmtVal([ValNoBind(name,opt_init)]) ->
      let vname = getVarName name in
      let init = apply_default (runExp glob loc) opt_init (VNum(0.0)) in
      declVal loc vname init
   | StmtMem([ValNoBind(name,opt_init)]) ->
      let vname = getVarName name in
      let init = apply_default (runExp glob loc) opt_init (VNum(0.0)) in
      declMem loc vname init
   | StmtVal(_)
   | StmtMem(_) -> failwith "Declarations with more that one element should have been removed by the transformations"
   | StmtBind(PId(name),rhs) ->
      let rhs_val = runExp glob loc rhs in
      let vname = getVarName name in
      setValMem loc vname rhs_val
   | StmtBind(PTuple(elems),rhs) ->
      let vnames = List.map getExpName elems in
      let rhs_val = runExp glob loc rhs in
      begin
         match rhs_val with
         | VTuple(elems_val) ->
            List.map2 (fun n v -> setValMem loc n v) vnames elems_val |> ignore
         | _ -> failwith "Not returning a tuple"
      end
   | StmtBind(_,rhs) -> failwith "Invalid binding"
   | StmtReturn(e) ->
      let e_val = runExp glob loc e in
      setReturn loc e_val
   | StmtFun(name_id,_,_) ->
      let _,ftype = TypesUtil.getFunctionTypeAndName name_id in
      declFunction glob ftype stmt
   | StmtIf(cond,then_stmts,None) ->
      let cond_val = runExp glob loc cond in
      if isTrue cond_val then
         (* This should create a sub-environment *)
         runStmtList glob loc then_stmts
   | StmtIf(cond,then_stmts,Some(else_stmts)) ->
      let cond_val = runExp glob loc cond in
      (* This should create a sub-environment *)
      if isTrue cond_val then
         runStmtList glob loc then_stmts
      else
         runStmtList glob loc else_stmts
   | StmtEmpty -> ()
      
      
and runStmtList (glob:global_env) (loc:local_env) (stmts:stmt list) : unit =
   List.iter (runStmt glob loc) stmts
      
let opNumNumNum (op:float->float->float) (args:value list) =
   match args with
   | [VNum(v1); VNum(v2)] -> VNum(op v1 v2)
   | _ -> failwith "Invalid arguments"

let opNumNumBool (op:float->float->bool) (args:value list) =
   match args with
   | [VNum(v1); VNum(v2)] -> VBool(op v1 v2)
   | _ -> failwith "Invalid arguments"

let opBoolBoolBool (op:bool->bool->bool) (args:value list) =
   match args with
   | [VBool(v1); VBool(v2)] -> VBool(op v1 v2)
   | _ -> failwith "Invalid arguments"

let addBuiltinFunctions (glob:global_env) : unit =
   let plus = opNumNumNum (+.) in
   let mult = opNumNumNum ( *. ) in
   let minus = fun args ->
      match args with
      | [_] -> opNumNumNum (-.) (VNum(0.0)::args)
      | _ -> opNumNumNum (-.) args
   in
   let div = opNumNumNum (/.) in
   let equal = opNumNumBool (=) in
   let unequal = opNumNumBool (<>) in
   let smaller = opNumNumBool (<) in
   let larger = opNumNumBool (>) in
   let smaller_equal = opNumNumBool (<=) in
   let larger_equal = opNumNumBool (>=) in
   let or_op = opBoolBoolBool (||) in
   let and_op = opBoolBoolBool (&&) in
   [
      "+",Builtin(plus);
      "-",Builtin(minus);
      "*",Builtin(mult);
      "/",Builtin(div);
      "==",Builtin(equal);
      "<>",Builtin(unequal);
      "<",Builtin(smaller);
      ">",Builtin(larger);
      "<=",Builtin(smaller_equal);
      ">=",Builtin(larger_equal);
      "||",Builtin(or_op);
      "&&",Builtin(and_op);
   ]
   |> List.iter (fun (a,b) -> Hashtbl.add glob.fun_decl a b)

let interpret (results:parser_results) =
   let glob = newGlobalEnv () in
   let _ = addBuiltinFunctions glob in
   let loc = newLocalEnv () in
   let _ = Either.applyToRight (fun stmts -> runStmtList glob loc stmts;stmts) results.presult in
   print_string (localEnvStr loc)
   