(*
The MIT License (MIT)

Copyright (c) 2016 Leonardo Laguna Ruiz, Carl Jönsson

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
open Common

module Env = struct

   type t =
      {
         locals    : (id, exp) Hashtbl.t list;
         context   : (id, exp) Hashtbl.t;
         instances : (id, t) Hashtbl.t;
         functions : (id, stmt) Hashtbl.t;
         modules   : (id, t) Hashtbl.t;
      }

   (** Non empty list of 't' *)
   type env = t list

   let new_t () =
      {
         locals    = [Hashtbl.create 0];
         context   = Hashtbl.create 0;
         instances = Hashtbl.create 0;
         functions = Hashtbl.create 0;
         modules   = Hashtbl.create 0;
      }

   let top () : env = [new_t ()]

   let first (env:env) : t =
      match env with
      | t ::_ -> t
      | []   -> failwith "invalid env"

   let rec lookupFunction_loop (t:t) (env:env) (id:id) : stmt =
      match id with
      | [] -> failwith "unknown function"
      | name1::name2 ->
         if Hashtbl.mem t.functions [name1] && name2 = [] then
            (* exists in the functions table and it's a single name *)
            Hashtbl.find t.functions [name1]
         else if Hashtbl.mem t.modules [name1] && name2 <> [] then
            (* exists in the modules table and it is not a single name *)
            let t' = Hashtbl.find t.modules [name1] in
            lookupFunction_loop t' env name2
         else
            (* not found, go one level up *)
            match env with
            | t' :: env' -> lookupFunction_loop t' env' id
            | [] -> failwith "unknown function"

   let lookupFunction (env:env) (id:id) : stmt =
      match env with
      | h::t -> lookupFunction_loop h t id
      | []   -> failwith "invalid env"

   let rec lookupVar_loop locals id : exp =
      match locals with
      | [] -> raise Not_found
      | h::t ->
         match Hashtbl.find h id with
         | value -> value
         | exception Not_found ->
            lookupVar_loop t id

   let lookupVar (env:env) (id:id) : exp =
      let t = first env in
      match Hashtbl.find t.context id with
      | value -> value
      | exception Not_found ->
         lookupVar_loop t.locals id

   let declareMem (env:env) (id:id) (value:exp) : unit =
      match lookupVar env id with
      | _ -> ()
      | exception Not_found ->
         let t = first env in
         Hashtbl.add t.context id value

   let declareVal (env:env) (id:id) (value:exp) : unit =
      match lookupVar env id with
      | _ -> ()
      | exception Not_found ->
         let t = first env in
         match t.locals with
         | h::_ -> Hashtbl.replace h id value
         | [] -> failwith "invalid env"

   let rec updateVar_loop (locals:'a list) (id:id) (value:exp) : unit =
      match locals with
      | [] -> failwith "unknow variable"
      | h::t ->
         match Hashtbl.mem h id with
         | _ -> Hashtbl.replace h id value
         | exception Not_found ->
            updateVar_loop t id value

   let updateVar (env:env) (id:id) (value:exp) : unit =
      let t = first env in
      match Hashtbl.mem t.context id with
      | _ -> Hashtbl.replace t.context id value
      | exception Not_found ->
         updateVar_loop t.locals id value

   let addFunction env id stmt =
      let t = first env in
      Hashtbl.add t.functions id stmt

   let addModule (env:env) (id:id) : unit =
      let t = first env in
      Hashtbl.add t.modules id (new_t ())

   let enterModule (env:env) (id:id) : env =
      let t = first env in
      match Hashtbl.find t.modules id with
      | module_t -> module_t :: env
      | exception Not_found -> failwith "unknown module"

   (** Inserts a table to store local variables *)
   let enterLocal (env:env) : env =
      match env with
      | [t] ->
         let locals = (Hashtbl.create 0) :: t.locals in
         let t' = { t with locals } in
         [t']
      | t::l ->
         let locals = (Hashtbl.create 0) :: t.locals in
         let t' = { t with locals } in
         t'::l
      | [] -> failwith "invalid env"

   let enterInstance (env:env) (id:id) : env =
      let t = first env in
      match Hashtbl.find t.instances id with
      | inst -> inst :: env
      | exception Not_found ->
         let inst = new_t () in
         Hashtbl.add t.instances id inst;
         inst :: env


end

let makeInstName (fn:id) (attr:attr) : id =
   let line = Loc.line attr.loc |> string_of_int in
   let col  = Loc.startColumn attr.loc  |> string_of_int in
   match fn with
   | [id] -> [id^"_"^line^"_"^col]
   | _ -> failwith "invalid function name"

(** Returns the initial value given an expression *)
let rec getInitValue (tp:VType.t) : exp =
   match !tp with
   | VType.TId(["unit"],_) -> PUnit(emptyAttr)
   | VType.TId(["real"],_) -> PReal(0.0,emptyAttr)
   | VType.TId(["int"],_)  -> PInt(0,emptyAttr)
   | VType.TId(["bool"],_) -> PBool(false,emptyAttr)
   | VType.TComposed(["array"],[sub;{ contents = VType.TInt(size,_) }],_) ->
      let sub_init = getInitValue sub in
      let elems = CCList.init size (fun _ -> sub_init) in
      PArray(elems,emptyAttr)
   | VType.TComposed(["tuple"],types,_) ->
      let elems = List.map getInitValue types in
      PTuple(elems,emptyAttr)
   | VType.TLink(tp) -> getInitValue tp
   | _ -> failwith "Interpreter.getInitValue"

let getInitExp (lhs:lhs_exp) : exp =
   match (GetAttr.fromLhsExp lhs).typ with
   | Some(typ) -> getInitValue typ
   | None -> failwith "Interpreter.getInitExp: cannot get the initial expression"

let ret_unit = PUnit(emptyAttr)

let evalUop (op:string) (exp:exp) : exp =
   match op, exp with
   | "-", PInt(v, attr) -> PInt(-v, attr)
   | "-", PReal(v, attr) -> PReal(-.v, attr)
   | _ -> PUnOp(op,exp, emptyAttr)

let evalOp (op:string) (e1:exp) (e2:exp) : exp =
   match op, e1, e2 with
   | "+", PReal(v1, attr), PReal(v2, _)  -> PReal(v1 +. v2, attr)
   | "+", PInt(v1, attr),  PInt(v2, _)   -> PInt(v1 + v2, attr)
   | "-", PReal(v1, attr), PReal(v2, _)  -> PReal(v1 -. v2, attr)
   | "-", PInt(v1, attr),  PInt(v2 ,_)   -> PInt(v1 - v2, attr)
   | "*", PReal(v1, attr), PReal(v2, _)  -> PReal(v1 *. v2, attr)
   | "*", PInt(v1, attr),  PInt(v2, _)   -> PInt(v1 * v2, attr)
   | "/", PReal(v1, attr), PReal(v2, _)  -> PReal(v1  /. v2, attr)
   | "/", PInt(v1, attr),  PInt(v2,_)    -> PInt(v1 / v2, attr)
   | "%", PReal(v1, attr), PReal(v2, _)  -> PReal(mod_float  v1  v2, attr)
   | "%", PInt(v1, attr),  PInt(v2, _)   -> PInt(v1 mod v2, attr)
   | "==", PReal(v1, attr), PReal(v2, _) -> PBool(v1 = v2, attr)
   | "==", PInt(v1, attr),  PInt(v2, _)  -> PBool(v1 = v2, attr)
   | "<>", PReal(v1, attr), PReal(v2, _) -> PBool(v1 <> v2, attr)
   | "<>", PInt(v1, attr),  PInt(v2, _)  -> PBool(v1 <> v2, attr)
   | ">", PReal(v1, attr), PReal(v2, _)  -> PBool(v1 > v2, attr)
   | ">", PInt(v1, attr),  PInt(v2, _)   -> PBool(v1 > v2, attr)
   | "<", PReal(v1, attr), PReal(v2, _)  -> PBool(v1 < v2, attr)
   | "<", PInt(v1, attr),  PInt(v2, _)   -> PBool(v1 < v2, attr)
   | ">=", PReal(v1, attr), PReal(v2, _) -> PBool(v1 >= v2, attr)
   | ">=", PInt(v1, attr),  PInt(v2, _)  -> PBool(v1 >= v2, attr)
   | "<=", PReal(v1, attr), PReal(v2, _) -> PBool(v1 <= v2, attr)
   | "<=", PInt(v1, attr),  PInt(v2, _)  -> PBool(v1 <= v2, attr)
   | _ -> POp(op, [e1; e2], emptyAttr)

let foldOp (op:string) (args:exp list) : exp =
   match args with
   | [] -> failwith ""
   | h::t ->
      List.fold_left (evalOp op) h t

type bind_kind =
   | Update
   | DeclareVal
   | DeclareMem

let rec bind (kind:bind_kind) (env:Env.env) (lhs:lhs_exp) (rhs:exp) =
   match lhs, rhs with
   | LWild _, _ -> ()
   | LTyped(lhs,_,_),_
   | LGroup(lhs,_),_ ->
      bind kind env lhs rhs
   | LId(id, _, _), rhs ->
      begin
         match kind with
         | Update     -> Env.updateVar env id rhs
         | DeclareVal -> Env.declareVal env id rhs
         | DeclareMem -> Env.declareMem env id rhs
      end
   | LTuple(lhs_elems,_), PTuple(rhs_elems,_) ->
      List.iter2 (bind kind env) lhs_elems rhs_elems
   | _ -> failwith "Interpreter.bind: invalid input"

let rec bind_arg (env:Env.env) (lhs:typed_id) (rhs:exp) =
   match lhs, rhs with
   | (TypedId(id,_,_,_) | SimpleId(id, _, _)), rhs ->
      Env.declareVal env id rhs

let rec evalExp (env:Env.env) (exp:exp) : exp =
   match exp with
   | PEmpty  -> PEmpty
   | PBool _ -> exp
   | PUnit _ -> exp
   | PInt _  -> exp
   | PReal _ -> exp
   | PGroup(e, _) -> evalExp env e
   | PId(id,_) ->  Env.lookupVar env id
   | PUnOp(op, exp, _) ->
      let exp' = evalExp env exp in
      evalUop op exp'

   | POp(op, elems, _) ->
      let elems' = List.map (evalExp env) elems in
      foldOp op elems'

   | PArray(elems,attr) ->
      let elems' = List.map (evalExp env) elems in
      PArray(elems',attr)

   | PTuple(elems,attr) ->
      let elems' = List.map (evalExp env) elems in
      PTuple(elems',attr)

   | PIf(cond, then_, else_, attr) ->
      let cond' = evalExp env cond in
      begin
         match cond' with
         | PBool(true, _) ->
            evalExp env then_
         | PBool(false, _) ->
            evalExp env else_
         | _ -> PIf(cond', then_, else_, attr)
      end

   | PSeq(_, stmt, _) -> evalStmt env stmt

   | PCall(Some(inst), name, args, _) ->
      let args' = List.map (evalExp env) args in
      let fn    = Env.lookupFunction env name in
      let env'  = Env.enterInstance env inst in
      evalFunction env' fn args'

   | PCall(None, name, args, attr) ->
      let args' = List.map (evalExp env) args in
      let fn    = Env.lookupFunction env name in
      let inst  = makeInstName name attr in
      let env'  = Env.enterInstance env inst in
      evalFunction env' fn args'

and evalFunction (env:Env.env) (fn:stmt) (args:exp list) : exp =
   match fn with
   | StmtFun(_, inputs, stmt, _, _) ->
      List.iter2 (bind_arg env) inputs args ;
      evalStmt env stmt
   | _ -> failwith "cannot evaluate function"

and evalStmt (env:Env.env) (stmt:stmt) =
   match stmt with
   | StmtVal(lhs, Some(rhs), _) ->
      let rhs' = evalExp env rhs in
      bind Update env lhs rhs';
      ret_unit

   | StmtVal(lhs, None, _) ->
      let rhs = getInitExp lhs in
      bind DeclareVal env lhs rhs;
      ret_unit

   | StmtMem(lhs, Some(rhs), _) ->
      let rhs' = evalExp env rhs in
      bind Update env lhs rhs';
      ret_unit

   | StmtMem(lhs, None, _) ->
      let rhs = getInitExp lhs in
      bind DeclareMem env lhs rhs;
      ret_unit

   | StmtBind(lhs, rhs, _) ->
      let rhs' = evalExp env rhs in
      bind Update env lhs rhs';
      ret_unit

   | StmtReturn(e, _) -> evalExp env e

   | StmtBlock(_, stmts, _) ->
      let env' = Env.enterLocal env in
      evalStmts env' stmts

   | StmtFun(name,_,_,_,_) ->
      Env.addFunction env name stmt;
      ret_unit

   | h -> failwith ("Not implemented "^(show_stmt h))

and evalStmts (env:Env.env) (stmts:stmt list) : exp =
   match stmts with
   | [] -> ret_unit
   | h::t ->
      match evalStmt env h with
      | PUnit _ -> evalStmts env t
      | ret -> ret

let evalModule (env:Env.env) results =
   let module_name =
      results.file
      |> moduleName
   in
   Env.addModule env [module_name];
   let env' = Env.enterModule env [module_name] in
   evalStmts env' results.presult

let eval (results:parser_results list) =
   let env = Env.top () in
   List.map  (evalModule env) results
