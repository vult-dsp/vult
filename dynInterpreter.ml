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

module StringMap = Map.Make(String)

type local_env =
   {
      val_binds : (value StringMap.t) list;
      mem_binds : value StringMap.t;
      fun_bind  : local_env StringMap.t;
   }

type function_body =
   | Builtin  of (value list -> value)
   | Declared of parse_exp

type global_env =
   {
      fun_decl : (string,function_body) Hashtbl.t;
   }

let newLocalEnv () =
   {
      val_binds = [];
      mem_binds = StringMap.empty;
      fun_bind  = StringMap.empty;
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
   let dumpStringMap env =
      StringMap.fold (fun name value state -> state^name^" = "^(valueStr value)^"\n" ) env ""
   in
   let val_s = List.map dumpStringMap loc.val_binds |> joinStrings "\n" in
   let mem_s = dumpStringMap loc.mem_binds in
   let fun_s = StringMap.fold (fun name value state -> name::state ) loc.fun_bind [] |> joinStrings "," in
   Printf.sprintf "= val =\n%s= mem =\n%s= fun =\n%s\n" val_s mem_s fun_s

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

let declFunction (glob:global_env) (name:string) (body:parse_exp) : unit =
   Hashtbl.replace glob.fun_decl name (Declared(body))

let getFunctionEnv (loc:local_env) (name:string) : local_env =
   if name = "_" then default_env else
   if StringMap.mem name loc.fun_bind then
      StringMap.find name loc.fun_bind
   else
      let env = newLocalEnv () in
      let _ = StringMap.add name env loc.fun_bind in
      env

let setFunctionEnv (loc:local_env) (name:string) (floc:local_env) : local_env =
   if name = "_" then loc else
   { loc with fun_bind = StringMap.add name floc loc.fun_bind }

let clearLocal (loc:local_env) : local_env =
   { loc with val_binds = [] }


let pushLocal (loc:local_env) : local_env =
   { loc with val_binds = (StringMap.empty)::loc.val_binds }

let popLocal (loc:local_env) : local_env =
   match loc.val_binds with
   | [] -> loc
   | _::t -> { loc with val_binds = t }

let findValMemTable (loc:local_env) (name:string) =
   let rec loop locals =
      match locals with
      | [] ->
         if StringMap.mem name loc.mem_binds then
            loc.mem_binds,`MemTable
         else
            let _ = print_string (localEnvStr loc) in
            failwith ("Undeclared variable "^name)
      | h::t ->
         if StringMap.mem name h then
            h,`ValTable
         else loop t
   in loop loc.val_binds

let getExpValueFromEnv (loc:local_env) (name:string) : value =
   let table,_ = findValMemTable loc name in
   StringMap.find name table

let setValMem (loc:local_env) (name:string)  (value:value) : local_env =
   match findValMemTable loc name with
   | table,`ValTable -> { loc with val_binds = (StringMap.add name value table)::(List.tl loc.val_binds) }
   | table,`MemTable -> { loc with mem_binds = StringMap.add name value table }

let declVal (loc:local_env) (name:string) (value:value) : local_env =
   match loc.val_binds with
   | [] ->
      let new_env = StringMap.add name value (StringMap.empty) in
      { loc with val_binds = [new_env] }
   | h::t ->
      let new_env = StringMap.add name value h in
      { loc with val_binds = new_env::t }

let declMem (loc:local_env) (name:string) (init:value) : local_env =
   if not (StringMap.mem name loc.mem_binds) then
      { loc with mem_binds = StringMap.add name init loc.mem_binds }
   else loc

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

let rec evalFun (glob:global_env) (loc:local_env) (body:function_body) (args:value list) : value * local_env =
   match body with
   | Declared(StmtFun(_,arg_names,stmts)) ->
      let inputs = getInputsNames arg_names in
      let loc = List.fold_left2 (fun s n v -> declVal s n v) loc inputs args in
      runStmtList glob loc stmts
   | Builtin(f) ->
      f args,loc
   | _ -> failwith "Invalid function body"

and runExp (glob:global_env) (loc:local_env) (exp:parse_exp) : value * local_env =
   match exp with
   | PUnit      -> VUnit,loc
   | PInt(v,_)  -> VNum(float_of_string v),loc
   | PReal(v,_) -> VNum(float_of_string v),loc
   | PId(name)  ->
      let vname  = getVarName name in
      getExpValueFromEnv loc vname,loc
   | PGroup(e)  -> runExp glob loc e
   | PTuple(elems) ->
      let elems_val,loc = runExpList glob loc elems in
      VTuple(elems_val),loc
   | PIf(cond,then_exp,else_exp) ->
      let cond_val,loc = runExp glob loc cond in
      if isTrue cond_val then
         runExp glob loc then_exp
      else
         runExp glob loc else_exp
   | PCall(name_id,args,_,_) ->
      let name,ftype = TypesUtil.getFunctionTypeAndName name_id in
      let body = getFunctionBody glob ftype in
      let args_val,loc = runExpList glob loc args in
      let env = getFunctionEnv loc name in
      let env = clearLocal env in
      let result,env = evalFun glob env body args_val in
      let loc = setFunctionEnv loc name env in
      result,loc
   | PEmpty -> failwith "There should not be Empty expressions when calling the intepreter"
   | PBinOp(_,_,_,_)
   | PUnOp(_,_,_) -> failwith "There should not be operators when calling the intepreter"
      | StmtVal([ValNoBind(name,opt_init)]) ->
      let vname = getVarName name in
      let init,loc = apply_default (runExp glob loc) opt_init (VNum(0.0),loc) in
      VUnit,declVal loc vname init
   | StmtMem([ValNoBind(name,opt_init)]) ->
      let vname = getVarName name in
      let init,loc = apply_default (runExp glob loc) opt_init (VNum(0.0),loc) in
      VUnit,declMem loc vname init
   | StmtVal(_)
   | StmtMem(_) -> failwith "Declarations with more that one element should have been removed by the transformations"
   | StmtBind(PId(name),rhs) ->
      let rhs_val,loc = runExp glob loc rhs in
      let vname = getVarName name in
      VUnit,setValMem loc vname rhs_val
   | StmtBind(PTuple(elems),rhs) ->
      let vnames = List.map getExpName elems in
      let rhs_val,loc = runExp glob loc rhs in
      begin
         match rhs_val with
         | VTuple(elems_val) ->
            VUnit,List.fold_left2 (fun s n v -> setValMem s n v) loc vnames elems_val
         | _ -> failwith "Not returning a tuple"
      end
   | StmtBind(PEmpty,rhs) ->
      let _,loc = runExp glob loc rhs in
      VUnit,loc
   | StmtBind(_,rhs) -> failwith "Invalid binding"
   | StmtReturn(e) ->
      let e_val,loc = runExp glob loc e in
      e_val,loc
   | StmtFun(name_id,_,_) ->
      let _,ftype = TypesUtil.getFunctionTypeAndName name_id in
      let _ = declFunction glob ftype exp in
      VUnit,loc
   | StmtIf(cond,then_stmts,None) ->
      let cond_val,loc = runExp glob loc cond in
      if isTrue cond_val then
         (* This should create a sub-environment *)
         runStmtList glob loc then_stmts
      else VUnit,loc
   | StmtIf(cond,then_stmts,Some(else_stmts)) ->
      let cond_val,loc = runExp glob loc cond in
      if isTrue cond_val then
         runStmtList glob loc then_stmts
      else
         runStmtList glob loc else_stmts
   | StmtEmpty -> VUnit,loc
   | StmtSequence(stmts) ->
      runStmtList glob loc stmts

and runExpList (glob:global_env) (loc:local_env) (expl:parse_exp list) : value list * local_env =
   let loc,acc = List.fold_left (fun (s,acc) a -> let v,ns = runExp glob s a in ns,v::acc) (loc,[]) expl in
   List.rev acc,loc
and runStmtList (glob:global_env) (loc:local_env) (expl:parse_exp list) : value * local_env =
   let loc = pushLocal loc in
   let rec loop loc stmts =
      match stmts with
      | [] -> VUnit,loc
      | h::t ->
         let value,loc = runExp glob loc h in
         begin
            match value with
            | VUnit -> loop loc t
            | _ -> value,loc
         end
   in
   let ret,loc = loop loc expl in
   let loc = popLocal loc in
   ret,loc

let opNumNum (op:float->float) (args:value list) =
   match args with
   | [VNum(v1)] -> VNum(op v1)
   | _ -> failwith "Invalid arguments"

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
   let print_fun args = List.map valueStr args |> joinStrings "," |> (fun a -> print_string a;VUnit) in
   let println_fun args = List.map valueStr args |> joinStrings "," |> (fun a -> print_endline a;VUnit) in
   let tanh_fun = opNumNum tanh in
   let abs_fun = opNumNum abs_float in
   let floor_fun = opNumNum floor in
   let sin_fun = opNumNum sin in
   let fixdenorm = opNumNum (fun a -> if (abs_float a)<1e-12 then 0.0 else a) in
   [
      "+",Builtin(plus);
      "-",Builtin(minus);
      "*",Builtin(mult);
      "/",Builtin(div);
      "==",Builtin(equal);
      "!=",Builtin(unequal);
      "<",Builtin(smaller);
      ">",Builtin(larger);
      "<=",Builtin(smaller_equal);
      ">=",Builtin(larger_equal);
      "||",Builtin(or_op);
      "&&",Builtin(and_op);
      "print",Builtin(print_fun);
      "println",Builtin(println_fun);
      "tanh",Builtin(tanh_fun);
      "abs",Builtin(abs_fun);
      "floor",Builtin(floor_fun);
      "sin",Builtin(sin_fun);
      "fixdenorm",Builtin(fixdenorm);
   ]
   |> List.iter (fun (a,b) -> Hashtbl.add glob.fun_decl a b)

let interpret (results:parser_results) : interpreter_results =
   let glob = newGlobalEnv () in
   let _ = addBuiltinFunctions glob in
   let loc = newLocalEnv () in
   try

      let result = CCError.map (fun stmts -> let ret,loc = runStmtList glob loc stmts in ret,loc) results.presult in
      match result with
      | `Ok(ret,loc) ->
         (*let _ = print_string (localEnvStr loc) in*)
         { iresult = `Ok(valueStr ret); lines = results.lines }
      | `Error(error) ->
         { iresult = `Error(error); lines = results.lines }
   with
   | Failure(msg) -> { iresult = `Error([SimpleError(msg)]); lines = results.lines }
