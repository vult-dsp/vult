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

This code is based on the type inference examples provided in:
   http://okmij.org/ftp/ML/generalization.html

*)

open Prog
open Env
open Common

type return_type =
   | GivenType of Typ.t
   | ReturnType of Typ.t
   | NoType

let expLoc e  = lazy (GetLocation.fromExp e)
let lhsLoc e  = lazy (GetLocation.fromLhsExp e)
let stmtLoc e = lazy (GetLocation.fromStmt e)
let typLoc t  = lazy (GetLocation.fromType t)
let typListLoc t  = lazy (GetLocation.fromType (Typ.first t))
let attrLoc attr = lazy (attr.loc)
let expOptLoc e =
   lazy (
      match e with
      | None -> Loc.default
      | Some(e) -> GetLocation.fromExp e)

let unifyRaise (loc:Loc.t Lazy.t) (t1:Typ.t) (t2:Typ.t) : unit =
   let raise = true in
   if not (Typ.unify t1 t2) then
      let msg =
         Printf.sprintf "This expression has type '%s' but '%s' was expected"
            (PrintProg.typeStr t2) (PrintProg.typeStr t1)
      in
      if raise then
         Error.raiseError msg (Lazy.force loc)
      else
         begin
            print_endline (Loc.to_string (Lazy.force loc));
            print_endline msg
         end

let rec checkType (loc:Loc.t Lazy.t) (env:'a Env.t) (typ:Typ.t) : unit =
   match !typ with
   | Typ.TId(name, _) ->
      begin
         try Env.lookup Scope.Type env name |> ignore with
         | _ ->
            let msg = Printf.sprintf "The type '%s' of this variable is unknown" (Id.show name) in
            Error.raiseError msg (Lazy.force loc)
      end
   | Typ.TComposed(["array"], [kind; {contents = Typ.TInt(_, _)}], _) ->
      checkType loc env kind

   | Typ.TComposed(["array"], _, _)  ->
      let msg =
         Printf.sprintf
            "This array type is invalid '%s'. Arrays are described as array(kind, size)."
            (PrintProg.typeStr typ)
      in
      Error.raiseError msg (Lazy.force loc)

   | Typ.TComposed(["tuple"], elems, _)  ->
      List.iter (checkType loc env) elems

   | Typ.TComposed(name, _, _)  ->
      let msg = Printf.sprintf "Unknow composed type '%s'." (Id.show name) in
      Error.raiseError msg (Lazy.force loc)

   | Typ.TArrow(t1, t2, _) ->
      checkType loc env t1;
      checkType loc env t2;

   | Typ.TLink(t) -> checkType loc env t

   | Typ.TExpAlt(elems) ->
      List.iter (checkType loc env) elems

   | Typ.TInt(_, _) -> ()
   | Typ.TUnbound(_, _, _) -> ()

let rec unifyListSameType (args:exp list) (types:Typ.t list) (common_type:Typ.t) =
   match args, types with
   | [], [] -> common_type
   | arg :: args, typ :: types ->
      unifyRaise (expLoc arg) typ common_type;
      unifyListSameType args types common_type
   | _ -> raise (Invalid_argument "unifyListSameType")


let rec addLhsToEnv mem_var (env:'a Env.t) (lhs:lhs_exp) : 'a Env.t =
   match lhs with
   | LWild _ -> env
   | LId(id, Some(typ), attr) ->
      let ftyp = Typ.first typ in
      if mem_var = `Mem then Env.addMem env id ftyp attr else Env.addVar env id ftyp attr
   | LId(_, None, _) -> env
   | LTuple(elems, _) ->
      List.fold_left (fun e a -> addLhsToEnv mem_var e a) env elems
   | LTyped(e, _, _) ->
      addLhsToEnv mem_var env e
   | LGroup(e, _) ->
      addLhsToEnv mem_var env e
   | LIndex(_, None, _, _) -> env
   | LIndex(id, Some(typ), _, attr) ->
      let ftyp = Typ.first typ in
      if mem_var = `Mem then Env.addMem env id ftyp attr else Env.addVar env id ftyp attr

let rec addArgsToEnv (env:'a Env.t) (args:typed_id list) : typed_id list * Typ.t list * 'a Env.t =
   match args with
   | [] -> [], [], env
   | SimpleId(id, kind, attr) :: t ->
      let typ  = Typ.newvar () in
      let env' = Env.addVar env id typ attr in
      let inner_args, inner_typ, env' = addArgsToEnv env' t in
      TypedId(id, [typ], kind, attr) :: inner_args, typ :: inner_typ, env'
   | TypedId(id, typ, kind, attr) :: t ->
      let first_typ = Typ.first typ in
      checkType (typLoc first_typ) env first_typ;
      let env' = Env.addVar env id first_typ attr in
      let inner_args, inner_typ, env' = addArgsToEnv env' t in
      TypedId(id, typ, kind, attr) :: inner_args, first_typ :: inner_typ, env'


let registerSpecialFunction (env:'a Env.t) (name:Id.t) (args:typed_id list) (ret:Typ.t) (attr:attr) =
   let env' = Env.addFunction env name attr in
   let env' = Env.enter Scope.Function env' name attr in
   let _, types, env' = addArgsToEnv env' args in
   let typ  = Typ.makeArrowType ret types in
   let env' = Env.setCurrentType env' typ true in
   Env.exit env'

let unifyOpt (loc:Loc.t Lazy.t) (typ1:Typ.t option) (typ2:Typ.t option) : Typ.t option =
   match typ1, typ2 with
   | None, None    -> None
   | Some(_), None -> typ1
   | None, Some(_) -> typ2
   | Some(t1), Some(t2) ->
      unifyRaise loc t1 t2;
      typ1

let unifyReturn (loc:Loc.t Lazy.t) (typ1:return_type) (typ2:return_type) : return_type =
   match typ1, typ2 with
   | NoType, NoType -> NoType
   | _, NoType        -> typ1
   | NoType, _        -> typ2
   | GivenType(gt), ReturnType(rt)
   | ReturnType(rt), GivenType(gt) ->
      unifyRaise loc gt rt;
      ReturnType(rt)
   | ReturnType(rt), ReturnType(gt) ->
      unifyRaise loc gt rt;
      ReturnType(rt)
   | GivenType(rt), GivenType(gt) ->
      unifyRaise loc gt rt;
      GivenType(rt)

let unifyArrayElem (loc:Loc.t Lazy.t) (t1:Typ.t) (t2:Typ.t) =
   if not (Typ.unify t1 t2) then
      let msg =
         Printf.sprintf
            "This expression has type '%s' but the previous members of the array have type '%s'"
            (PrintProg.typeStr t2) (PrintProg.typeStr t1)
      in
      Error.raiseError msg (Lazy.force loc)

let getReturnType (typ:return_type) : Typ.t =
   match typ with
   | NoType        -> Typ.Const.unit_type
   | GivenType(t)  -> t
   | ReturnType(t) -> t

let makeReturnType (v:Typ.t option) : return_type =
   match v with
   | None    -> NoType
   | Some(s) -> GivenType(s)

let raiseReturnError (loc:Loc.t Lazy.t) (given:Typ.t option) (typ:return_type) =
   match given, typ with
   | None, _ -> ()
   | Some(gt), GivenType(rt) when Typ.compare gt rt = 0 && Typ.compare gt Typ.Const.unit_type = 0 -> ()
   | Some(gt), GivenType(rt) when Typ.compare gt rt = 0 ->
      let msg =
         Printf.sprintf
            "This function is expected to have type '%s' but nothing was returned"
            (PrintProg.typeStr gt)
      in
      Error.raiseError msg (Lazy.force loc)
   | _ -> ()

let inferApplyArgCount (loc:Loc.t Lazy.t) (args_typ:Typ.t list) (fn_args:Typ.t list) : unit =
   let n_args    = List.length args_typ in
   let n_fn_args = List.length fn_args in
   if  n_args <> n_fn_args then
      let msg = Printf.sprintf "This function takes %i arguments but %i are passed" n_fn_args n_args in
      Error.raiseError msg (Lazy.force loc)

let rec inferApplyArg (args:exp list) (args_typ:Typ.t list) (fn_args:Typ.t list) : unit =
   match args, args_typ, fn_args with
   | [], [], [] -> ()
   | arg :: args, arg_typ :: args_typ, fn_arg :: fn_args ->
      unifyRaise (expLoc arg) fn_arg arg_typ;
      inferApplyArg args args_typ fn_args
   | _ -> failwith "Inference.inferApplyArg: invalid input"

let inferApply (loc:Loc.t Lazy.t) (args:exp list) (args_typ:Typ.t list) (ret_type:Typ.t)
      (fn_args:Typ.t list) (fn_ret_type:Typ.t) =
   inferApplyArgCount loc args_typ fn_args;
   inferApplyArg args args_typ fn_args;
   unifyRaise loc fn_ret_type ret_type

let addInstance (env:'a Env.t) (isactive:bool) (name:Id.t option) (typ:Typ.t) (attr:attr) : 'a Env.t * Id.t option =
   if isactive then
      match name with
      | Some(n) ->
         let env' = Env.addInstance env n typ attr in
         env', name
      | None ->
         let n, env' = Env.tick env in
         let inst_name = [("_inst" ^ (string_of_int n))] in
         let env' = Env.addInstance env' inst_name typ attr in
         env', Some(inst_name)
   else
      env, None

let rec inferLhsExp mem_var (env:'a Env.t) (e:lhs_exp) : lhs_exp * Typ.t =
   match e with
   | LWild(attr) ->
      let typ = Typ.newvar () in
      LWild( { attr with typ = Some(typ) }), typ
   | LId(id, None, attr) ->
      let typ =
         match Env.lookupVariable env id with
         | Some(var) -> var.Scope.typ
         | _ ->
            if mem_var = `Mem || mem_var = `Val then
               Typ.newvar ()
            else
               let msg = Printf.sprintf "The symbol '%s' is not defined" (Id.show id) in
               Error.raiseError msg attr.loc
      in
      LId(id, Some [typ], { attr with typ = Some typ }), typ
   | LId(id, Some typ, attr) ->
      let ftyp = Typ.first typ in
      LId(id, Some typ, { attr with typ = Some ftyp }), ftyp
   | LTuple(elems, attr) ->
      let elems', tpl =
         List.fold_left (fun (elems, tpl) a ->
               let a', typ = inferLhsExp mem_var env a in
               a' :: elems, typ :: tpl )
            ([], [])
            elems
      in
      let typ = ref (Typ.TComposed(["tuple"], List.rev tpl, None)) in
      LTuple(List.rev elems', { attr with typ = Some(typ) }), typ
   | LTyped(e, typ, _) ->
      checkType (lhsLoc e) env typ;
      let e', tpi = inferLhsExp mem_var env e in
      if not (Typ.unify typ tpi) then
         let msg =
            Printf.sprintf
               "This declaration has type '%s' but it has been defined before as '%s'"
               (PrintProg.typeStr typ)
               (PrintProg.typeStr tpi)
         in
         Error.raiseError msg (GetLocation.fromLhsExp e)
      else
         e', tpi
   | LGroup(eg, _) ->
      inferLhsExp mem_var env eg

   | LIndex(id, otyp, PInt(size, sattr), attr) when mem_var = `Val || mem_var = `Mem ->
      let typ =
         match Env.lookupVariable env id, otyp with
         | Some(var), _ -> var.Scope.typ
         | _, Some typ -> Typ.first typ
         | _ -> Typ.newvar ()
      in
      let a = ref (Typ.TUnbound("'a", None, None)) in
      let size_t = ref (Typ.TInt(size, None)) in
      let arr_type = ref (Typ.TComposed(["array"], [a;size_t], None)) in
      unifyRaise (attrLoc attr) typ arr_type;
      LIndex(id, Some [typ], PInt(size, sattr), { attr with typ = Some arr_type}), arr_type

   | LIndex(_, _, index, _) when mem_var = `Val || mem_var = `Mem ->
      let msg =
         Printf.sprintf
            "This expression '%s' defines the size of the array and must be an integer"
            (PrintProg.expressionStr index)
      in
      Error.raiseError msg (GetLocation.fromExp index)

   | LIndex(id, otyp, index, attr) ->
      let typ =
         match Env.lookupVariable env id, otyp with
         | Some(var), _ -> var.Scope.typ
         | _, Some typ -> Typ.first typ
         | _ -> Typ.newvar ()
      in
      let index', _, index_typ = inferExp env index in
      let a = ref (Typ.TUnbound("'a", None, None)) in
      let size = ref (Typ.TUnbound("'size", None, None)) in
      let arr_type = ref (Typ.TComposed(["array"], [a;size], None)) in
      unifyRaise (attrLoc attr) typ arr_type;
      unifyRaise (expLoc index) index_typ Typ.Const.int_type;
      LIndex(id, Some [typ], index', { attr with typ = Some a }), a

and inferExp (env:'a Env.t) (e:exp) : exp * ('a Env.t) * Typ.t =
   match e with
   | PUnit(attr) ->
      let typ = Typ.Const.unit_type in
      PUnit({ attr with typ = Some(typ) }), env, typ
   | PBool(v, attr) ->
      let typ = Typ.Const.bool_type in
      PBool(v, { attr with typ = Some(typ) }), env, typ
   | PInt(v, attr) ->
      let typ = Typ.Const.int_type in
      PInt(v, { attr with typ = Some(typ) }), env, typ
   | PReal(v, attr) ->
      let typ = Typ.Const.real_type in
      PReal(v, { attr with typ = Some(typ) }), env, typ
   | PString(s, attr) ->
      let typ = Typ.Const.string_type in
      PString(s, { attr with typ = Some(typ) }), env, typ
   | PId(id, attr) ->
      let var = Env.lookupVariableRaise env id attr.loc in
      PId(id, { attr with typ = Some(var.Scope.typ) }), env, var.Scope.typ
   | PIndex(e, index, attr) ->
      let index', env', index_type = inferExp env index in
      unifyRaise (expLoc index') (Typ.Const.int_type) index_type;
      let e', env', e_type = inferExp env' e in
      let a = ref (Typ.TUnbound("'a", None, None)) in
      let size = ref (Typ.TUnbound("'size", None, None)) in
      let array_type = ref (Typ.TComposed(["array"], [a;size], None)) in
      unifyRaise (expLoc index') (Typ.Const.int_type) index_type;
      unifyRaise (expLoc e) array_type e_type;
      PIndex(e', index', { attr with typ = Some(a) }), env', a

   | PArray(elems, attr) ->
      let elems', env', atype, n = inferArrayElems env elems in
      let typ = ref (Typ.TComposed(["array"], [atype;ref (Typ.TInt(n, None))], None)) in
      PArray(elems', { attr with typ = Some(typ) }), env', typ
   | PGroup(e, _) ->
      inferExp env e
   | PTuple(elems, attr) ->
      let elems', env', types = inferExpList env elems in
      let typ = ref (Typ.TComposed(["tuple"], types, None)) in
      PTuple(elems', { attr with typ = Some(typ) }), env', typ
   | PIf(cond, then_, else_, attr) ->
      let cond', env', cond_type  = inferExp env cond in
      let then_', env', then_type = inferExp env' then_ in
      let else_', env', else_type = inferExp env' else_ in
      unifyRaise (expLoc cond') (Typ.Const.bool_type) cond_type;
      unifyRaise (expLoc else_') then_type else_type;
      PIf(cond', then_', else_', { attr with typ = Some(then_type) }), env', then_type
   | PCall(name, fname, args, attr) ->
      let args', env', types = inferExpList env args in
      let ret_type        = Typ.newvar () in
      let _, fn_type, ft  = Env.lookupRaise Scope.Function env' fname attr.loc in
      let fn_args, fn_ret = Typ.stripArrow fn_type in
      let active          = Scope.isActive ft in
      let fname_typ       = ref (Typ.TId(fname, None)) in
      let env', name'     = addInstance env' active name fname_typ attr in
      inferApply (expLoc e) args' types ret_type fn_args fn_ret;
      PCall(name', fname, args', { attr with typ = Some(ret_type) }), env', ret_type
   | POp(op, [e1;e2], attr) ->
      let e1', env', e1_typ  = inferExp env e1 in
      let e2', env', e2_typ  = inferExp env' e2 in
      let ret_type           = Typ.newvar () in
      let _, fn_type, _      = Env.lookupRaise Scope.Operator env [op] attr.loc in
      let fn_args, fn_ret    = Typ.stripArrow fn_type in
      inferApply (expLoc e) [e1'; e2'] [e1_typ; e2_typ] ret_type fn_args fn_ret;
      POp(op, [e1';e2'], { attr with typ = Some(ret_type) }), env', ret_type
   | POp(op, args, attr) ->
      let args', env', types = inferExpList env args in
      let _, fn_type, _ = Env.lookupRaise Scope.Operator env [op] attr.loc in
      let common_type =
         match Typ.stripArrow fn_type with
         | [arg1;arg2], ret ->
            if Typ.unify arg1 arg2 && Typ.unify arg2 ret then
               ret
            else
               failwith "The operator cannot be used with multiple arguments"
         | _ -> failwith "The operator cannot be used with multiple arguments"
      in
      let ret_type = unifyListSameType args' types common_type in
      POp(op, args', { attr with typ = Some(ret_type) }), env', ret_type
   | PUnOp(op, arg, attr) ->
      let arg', env', arg_type = inferExp env arg in
      let ret_type        = Typ.newvar () in
      let _, fn_type, _   = Env.lookupRaise Scope.Operator env ["|"^op ^ "|"] attr.loc in
      let fn_args, fn_ret = Typ.stripArrow fn_type in
      inferApply (expLoc e) [arg'] [arg_type] ret_type fn_args fn_ret;
      PUnOp(op, arg', { attr with typ = Some(ret_type) }), env', ret_type
   | PSeq(name, stmt, attr) ->
      let stmt', _, ret_type = inferStmt env NoType stmt in
      let typ = getReturnType ret_type in
      PSeq(name, stmt', { attr with typ = Some(typ)}), env, typ
   | PEmpty -> PEmpty, env, Typ.Const.unit_type

and inferExpList (env:'a Env.t) (elems:exp list) : exp list * 'a Env.t * Typ.t list =
   let elems', env', types =
      List.fold_left (fun (elems, env, types) a ->
            let a', env', typ = inferExp env a in
            a' :: elems, env', typ :: types )
         ([], env, [])
         elems
   in List.rev elems', env', List.rev types

and inferArrayElems (env:'a Env.t) (elems:exp array) : exp array * 'a Env.t * Typ.t * int =
   let atype = Typ.newvar() in
   let ret = Array.copy elems in
   let env', count =
      Array.fold_left (fun (env, i) a ->
            let a', env', typ = inferExp env a in
            unifyArrayElem (expLoc a') atype typ;
            Array.set ret i a';
            env', i+1)
         (env, 0)
         elems
   in
   ret, env', atype, count

and inferOptExp (env:'a Env.t) (e:exp option) : exp option * 'a Env.t * Typ.t =
   match e with
   | None    -> None, env, Typ.newvar ()
   | Some(e) ->
      let e', env', typ = inferExp env e in
      Some(e'), env', typ

and inferStmt (env:'a Env.t) (ret_type:return_type) (stmt:stmt) : stmt * 'a Env.t * return_type =
   match stmt with
   | StmtVal(lhs, rhs, attr) ->
      let lhs', lhs_typ = inferLhsExp `Val env lhs in
      let rhs', env', rhs_typ = inferOptExp env rhs in
      unifyRaise (expOptLoc rhs) lhs_typ rhs_typ;
      let env' = addLhsToEnv `Val env' lhs' in
      StmtVal(lhs', rhs', attr), env', ret_type
   | StmtMem(lhs, rhs, attr) ->
      let lhs', lhs_typ = inferLhsExp `Mem env lhs in
      let env'          = addLhsToEnv `Mem env lhs' in
      let rhs', env', rhs_typ = inferOptExp env' rhs in
      unifyRaise (expOptLoc rhs') lhs_typ rhs_typ;
      StmtMem(lhs', rhs', attr), env', ret_type
   | StmtReturn(e, attr) ->
      let e', env', typ = inferExp env e in
      let ret_type'     = unifyReturn (expLoc e) ret_type (ReturnType(typ)) in
      StmtReturn(e', attr), env', ret_type'
   | StmtBind(lhs, rhs, attr) ->
      let lhs', lhs_typ       = inferLhsExp `None env lhs in
      let rhs', env', rhs_typ = inferExp env rhs in
      unifyRaise (expLoc rhs') lhs_typ rhs_typ;
      StmtBind(lhs', rhs', attr), env', ret_type
   | StmtBlock(name, stmts, attr) ->
      let env' = Env.enterBlock env in
      let env', name' =
         if CCOpt.is_none name then
            let n, env' = Env.tick env' in
            env', Some(["scope_" ^ (string_of_int n)])
         else
            env', name
      in
      let stmts', env', stmt_ret_type = inferStmtList env' ret_type stmts in
      let env' = Env.exitBlock env' in
      StmtBlock(name', stmts', attr), env', stmt_ret_type
   | StmtFun(name, args, body, ret_type, attr) ->
      let env'                = Env.addFunction env name attr in
      let env'                = Env.enter Scope.Function env' name attr in
      let args', types, env'  = addArgsToEnv env' args in
      let types', table       = Typ.fixTypeList [] types in
      let ret_type', _        = Typ.fixOptType table ret_type in
      let possible_ret_type   = Typ.newvar () in
      let typ                 = Typ.makeArrowType possible_ret_type types' in
      let env'                = Env.setCurrentType env' typ true in
      let body', env', body_ret = inferStmt env' (makeReturnType ret_type') body in
      let last_type           = getReturnType body_ret in
      let env'                = Env.exit env' in
      let  _                  = raiseReturnError (attrLoc attr) ret_type' body_ret in
      unifyRaise (stmtLoc stmt) possible_ret_type last_type;
      StmtFun(name, args', body', Some(last_type), attr), env', NoType
   | StmtIf(cond, then_, else_, attr) ->
      let cond', env', cond_type  = inferExp env cond in
      unifyRaise (expLoc cond') (Typ.Const.bool_type) cond_type;
      let then_', env', ret_type' = inferStmt env' ret_type then_ in
      let else_', env', ret_type' = inferOptStmt env' ret_type' else_ in
      StmtIf(cond', then_', else_', attr), env', ret_type'
   | StmtWhile(cond, body, attr) ->
      let cond', env', cond_type  = inferExp env cond in
      unifyRaise (expLoc cond') (Typ.Const.bool_type) cond_type;
      let body', env', ret_type' = inferStmt env' ret_type body in
      StmtWhile(cond', body', attr), env', ret_type'
   | StmtType(name, members, attr) ->
      StmtType(name, members, attr), env, ret_type
   | StmtAliasType (name, alias, attr) ->
      StmtAliasType (name, alias, attr), env, ret_type
   | StmtExternal(name, args, fun_ret_type, linkname, attr) ->
      let env' = Env.addFunction env name attr in
      let env' = Env.enter Scope.Function env' name attr in
      let args', types, env' = addArgsToEnv env' args in
      let typ  = Typ.makeArrowType fun_ret_type types in
      let env' = Env.setCurrentType env' typ true in
      let env' = Env.exit env' in
      let env', attr =
         (* her we register the functions that are generated by tags *)
         if Tags.has attr.tags ["wave"] then
            let size_name = Id.postfix name "_samples" in
            let attr = { attr with ext_fn = None } in
            registerSpecialFunction env' size_name [] Typ.Const.int_type attr, attr
         else
            env', attr
      in
      StmtExternal(name, args', fun_ret_type, linkname, attr), env', ret_type
   | StmtEmpty -> StmtEmpty, env, ret_type


and inferStmtList (env:'a Env.t) (ret_type_in:return_type) (stmts:stmt list) : stmt list * 'a Env.t * return_type =
   let stmts', env', ret_type' =
      List.fold_left
         (fun (stmts, env, ret_type) stmt ->
             let stmt', env', ret_type' = inferStmt env ret_type stmt in
             stmt' :: stmts, env', ret_type')
         ([], env, ret_type_in)
         stmts
   in (List.rev stmts'), env', ret_type'

and inferOptStmt (env:'a Env.t) (ret_type:return_type) (stmt:stmt option) : stmt option * 'a Env.t * return_type =
   match stmt with
   | None -> None, env, ret_type
   | Some(s) ->
      let s', s_type, s_ret_type = inferStmt env ret_type s in
      Some(s'), s_type, s_ret_type

let inferFile state (results:parser_results) =
   let module_name = [moduleName results.file] in
   let state' = Env.enter Scope.Module state module_name emptyAttr in
   let stmts, state', _ = inferStmtList state' NoType results.presult in
   let state' = Env.exit state' in
   state', { results with presult = stmts }

let infer (results:parser_results list) : parser_results list =
   let state = Env.empty () in
   let _, results' =
      List.fold_left
         (fun (state, acc) result ->
             let state', result' = inferFile state result in
             state', result' :: acc)
         (state, [])
         results
   in
   List.rev results'
