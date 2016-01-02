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

open TypesVult
open VEnv
open Common

let gensym_counter = ref 0
let reset_gensym : unit -> unit =
   fun () -> gensym_counter := 0

let gensym : unit -> string = fun () ->
   let n = !gensym_counter in
   let () = incr gensym_counter in
   if n < 26 then String.make 1 (Char.chr (Char.code 'a' + n))
   else "t" ^ string_of_int n


(* Determining the |let|-nesting level during the type-checking,
   or just the _level_.
   Each top-level expression to type-check is implicitly wrapped into a let.
   So, the numbering starts with 1.
*)
let current_level = ref 1
let reset_level () = current_level := 1

let reset_type_variables () =       (* name from OCaml's typing/typetext.ml *)
   reset_gensym ();
   reset_level ()

(* Increase level *)
let enter_level () =
   incr current_level
(* Restore level *)
let leave_level () =
   decr current_level


(* Make a fresh type variable *)
let newvar : unit -> vtype =
   fun () -> (ref (TUnbound (gensym (),!current_level,None)))

let pickLoc (loc1:Loc.t option) (loc2:Loc.t option) : Loc.t option =
   match loc1, loc2 with
   | None, _ -> loc2
   | _, None -> loc1
   | Some(l1),_ when l1 = Loc.default -> loc2
   | _,Some(l2) when l2 = Loc.default -> loc1
   | _ -> loc1

let expLoc e = lazy (GetLocation.fromExp e)
let lhsLoc e = lazy (GetLocation.fromLhsExp e)
let expOptLoc e =
   lazy (
   match e with
   | None -> Loc.default
   | Some(e) -> GetLocation.fromExp e)

let rec makeArrowType (last:vtype) (types:vtype list) : vtype =
   match types with
   | [] -> last
   | (h:vtype)::t -> ref (TArrow(h,makeArrowType last t,None))

let rec stripArrow (typ:vtype) : vtype list * vtype =
   match typ with
   | { contents = TArrow(t1,t2,_) } ->
      let args,last = stripArrow t2 in
      t1 :: args, last
   | _ -> [], typ

let rec unify (t1:vtype) (t2:vtype) : bool =
   if t1 == t2 then true else
   match t1,t2 with
   | { contents = TUnbound(n1,l1,loc1)}, { contents = TUnbound(_,l2,loc2) } ->
      let loc = pickLoc loc1 loc2 in
      let level = min l1 l2 in
      let n = if n1 = "" then gensym () else n1 in
      let t = TUnbound(n,level,loc) in
      t1 := t;
      t2 := TLink(t1);
      true
   | { contents = TLink(tlink) },t | t, { contents = TLink(tlink) } ->
      unify t tlink
   | ({ contents = TUnbound _ } as tu), t | t, ({ contents = TUnbound _ } as tu) ->
      tu := TLink(t);
      true
   | { contents = TComposed(n1,elems1,_) }, { contents = TComposed(n2,elems2,_) } when n1 = n2 ->
      List.for_all2 unify elems1 elems2
   | { contents = TArrow(a1,a2,_) }, { contents = TArrow(b1,b2,_) } ->
      unify a1 b1 && unify a2 b2
   | { contents = TId(id1,_) }, { contents = TId(id2,_) } when id1 = id2 -> true

   | { contents = TExpAlt(first_alt :: alt_rest) }, t
   | t, { contents = TExpAlt(first_alt :: alt_rest) } ->
      if unify first_alt t then
         begin
            t1 := !first_alt;
            true
         end
      else
         unify (ref(TExpAlt(alt_rest))) t
   | { contents = tp1 }, { contents = tp2 } when equal_vtype_c tp1 tp2 -> true

   | _ -> false


let unifyRaise (loc:Loc.t Lazy.t) (t1:vtype) (t2:vtype) : unit =
   if not (unify t1 t2) then
      let msg = Printf.sprintf "This expression has type '%s' but '%s' was expected" (PrintTypes.typeStr t2) (PrintTypes.typeStr t1) in
      Error.raiseError msg (Lazy.force loc)


let rec unifyOperator (fn_type:vtype) (types:vtype list) : vtype =
   match types with
   | [] -> failwith "unifyOperator: the number of operands is no more than two"
   | [e1; e2] ->
      let ret_type = newvar () in
      let op_type  = makeArrowType ret_type [e1; e2] in
      unify fn_type op_type;
      ret_type
   | h::t ->
      let inner_typ = unifyOperator fn_type t in
      let ret_type  = newvar () in
      let op_type   = makeArrowType ret_type [h; inner_typ] in
      unify fn_type op_type;
      ret_type


let rec inferLhsExp (env:'a Env.t) (e:lhs_exp) : lhs_exp * vtype =
   match e with
   | LWild(attr) ->
      let typ = newvar () in
      LWild( { attr with typ = Some(typ) }), typ
   | LId(id,None,attr) ->
      let typ = try Env.lookup env id |> snd with | _ -> newvar () in
      LId(id,Some(typ),{ attr with typ = Some(typ) }), typ
   | LId(id,Some(typ),attr) ->
      LId(id,Some(typ),{ attr with typ = Some(typ) }), typ
   | LTuple(elems,attr) ->
      let elems',tpl =
         List.fold_left (fun (elems,tpl) a ->
            let a',typ = inferLhsExp env a in
            a' :: elems, typ :: tpl )
         ([],[])
         elems
      in
      let typ = ref (TComposed(["tuple"],List.rev tpl,None)) in
      LTuple(List.rev elems',attr),typ
   | LTyped(e,typ,_) ->
      let e',tpi = inferLhsExp env e in
      unifyRaise (lhsLoc e) typ tpi;
      e',tpi

let rec addLhsToEnv mem_var (env:'a Env.t) (lhs:lhs_exp) : 'a Env.t =
   match lhs with
   | LWild _ -> env
   | LId(id,Some(typ),_) ->
      if mem_var = `Mem then Env.addMem env id typ else Env.addVar env id typ
   | LId(_,None,_) -> env
   | LTuple(elems,_) ->
      List.fold_left (fun e a -> addLhsToEnv mem_var e a) env elems
   | LTyped(e,_,_) ->
      addLhsToEnv mem_var env e

let rec addArgsToEnv (env:'a Env.t) (args:typed_id list) : typed_id list * vtype list * 'a Env.t =
   match args with
   | [] -> [], [], env
   | SimpleId(id,attr)::t ->
      let typ  = newvar () in
      let env' = Env.addVar env id typ in
      let inner_args, inner_typ, env' = addArgsToEnv env' t in
      TypedId(id,typ,attr) :: inner_args, typ :: inner_typ, env'
   | TypedId(id,typ,attr)::t ->
      let env' = Env.addVar env id typ in
      let inner_args, inner_typ, env' = addArgsToEnv env' t in
      TypedId(id,typ,attr) :: inner_args, typ :: inner_typ, env'


let unifyOpt (loc:Loc.t Lazy.t) (typ1:vtype option) (typ2:vtype option) : vtype option =
   match typ1,typ2 with
   | None, None    -> None
   | Some(_), None -> typ1
   | None, Some(_) -> typ2
   | Some(t1), Some(t2) ->
      unifyRaise loc t1 t2;
      typ1

let getOptType (typ:vtype option) : vtype =
   match typ with
   | None    -> newvar ()
   | Some(t) -> t


let inferApplyArgCount (loc:Loc.t Lazy.t) (args_typ:vtype list) (fn_args:vtype list) : unit =
   let n_args    = List.length args_typ in
   let n_fn_args = List.length fn_args in
   if  n_args <> n_fn_args then
      Error.raiseError (Printf.sprintf "This function takes %i arguments but %i are passed" n_fn_args n_args) (Lazy.force loc)

let rec inferApplyArg (args:exp list) (args_typ:vtype list) (fn_args:vtype list) : unit =
   match args, args_typ, fn_args with
   | [], [], [] -> ()
   | arg::args, arg_typ::args_typ, fn_arg::fn_args ->
      unifyRaise (expLoc arg) fn_arg arg_typ;
      inferApplyArg args args_typ fn_args
   | _ -> failwith "Inference.inferApplyArg: invalid input"

let inferApply (loc:Loc.t Lazy.t) (args:exp list) (args_typ:vtype list) (ret_type:vtype) (fn_args:vtype list) (fn_ret_type:vtype) =
   inferApplyArgCount loc args_typ fn_args;
   inferApplyArg args args_typ fn_args;
   unifyRaise loc fn_ret_type ret_type


let rec inferExp (env:'a Env.t) (e:exp) : exp * vtype =
   match e with
   | PUnit(attr) ->
      PUnit({ attr with typ = Some(Constants.unit_type()) }), Constants.unit_type ()
   | PBool(v,attr) ->
      PBool(v,{ attr with typ = Some(Constants.bool_type()) }), Constants.bool_type()
   | PInt(v,attr) ->
      let typ = ref (TExpAlt([Constants.int_type(); Constants.real_type()])) in
      PInt(v,{ attr with typ = Some(typ) }), typ
   | PReal(v,attr) ->
      PReal(v,{ attr with typ = Some(Constants.real_type()) }), Constants.real_type()
   | PId(id,attr) ->
      let typ = try Env.lookup env id |> snd with | _ -> failwith ("Undefined symbol '" ^ (PrintTypes.identifierStr id)^"'") in
      PId(id, { attr with typ = Some(typ) }), typ
   | PGroup(e,_) ->
      inferExp env e
   | PTuple(elems,attr) ->
      let elems', types = inferExpList env elems in
      let typ           = ref (TComposed(["tuple"],types,None)) in
      PTuple(elems',{ attr with typ = Some(typ) }), typ
   | PIf(cond,then_,else_,attr) ->
      let cond',cond_type  = inferExp env cond in
      let then_',then_type = inferExp env then_ in
      let else_',else_type = inferExp env else_ in
      unifyRaise (expLoc cond') (Constants.bool_type()) cond_type;
      unifyRaise (expLoc else_') then_type else_type;
      PIf(cond',then_',else_',{ attr with typ = Some(then_type) }), then_type
   | PCall(name,fname,args,attr) ->
      let args',types = inferExpList env args in
      let ret_type    = newvar () in
      let _, fn_type  = Env.lookup env fname in
      let fn_args,fn_ret = stripArrow fn_type in
      inferApply (expLoc e) args' types ret_type fn_args fn_ret;
      PCall(name,fname,args',{ attr with typ = Some(ret_type) }), ret_type
   | POp(op,[e1;e2],attr) ->
      let e1',e1_typ  = inferExp env e1 in
      let e2',e2_typ  = inferExp env e2 in
      let ret_type    = newvar () in
      let _, fn_type  = Env.lookup env ["|"^op^"|"] in
      let op_type     = makeArrowType ret_type [e1_typ; e2_typ] in
      unify fn_type op_type;
      POp(op,[e1';e2'],{ attr with typ = Some(ret_type) }), ret_type
   | POp(op,args,attr) ->
      let args',types = inferExpList env args in
      let _, fn_type  = Env.lookup env ["|"^op^"|"] in
      let ret_type    = unifyOperator fn_type types in
      POp(op,args',{ attr with typ = Some(ret_type) }), ret_type
   | PUnOp(op,arg,attr) ->
      let arg',arg_type = inferExp env arg in
      let ret_type      = newvar () in
      let op_type       = ref (TArrow(arg_type,ret_type,None)) in
      let _, fn_type    = Env.lookup env ["|"^op^"|"] in
      unify fn_type op_type;
      PUnOp(op,arg',{ attr with typ = Some(ret_type) }), ret_type
   | PSeq(name,stmt,attr) ->
      let stmt', _, ret_type = inferStmt env None stmt in
      let typ = getOptType ret_type in
      PSeq(name,stmt',{ attr with typ = Some(typ)}), typ
   | PEmpty -> PEmpty, Constants.unit_type()

and inferExpList (env:'a Env.t) (elems:exp list) : exp list * vtype list =
   let elems',types =
      List.fold_left (fun (elems,types) a ->
         let a',typ = inferExp env a in
         a' :: elems, typ :: types )
      ([],[])
      elems
   in List.rev elems', List.rev types

and inferOptExp (env:'a Env.t) (e:exp option) : exp option * vtype =
   match e with
   | None    -> None, newvar ()
   | Some(e) ->
      let e',typ = inferExp env e in
      Some(e'), typ

and inferStmt (env:'a Env.t) (ret_type:vtype option) (stmt:stmt) : stmt * 'a Env.t * vtype option =
   match stmt with
   | StmtVal(lhs,rhs,attr) ->
      let lhs', lhs_typ = inferLhsExp env lhs in
      let rhs', rhs_typ = inferOptExp env rhs in
      unifyRaise (expOptLoc rhs) lhs_typ rhs_typ;
      let env' = addLhsToEnv `Var env lhs' in
      StmtVal(lhs', rhs', attr), env', None
   | StmtMem(lhs,init,rhs,attr) ->
      let lhs', lhs_typ   = inferLhsExp env lhs in
      let env'            = addLhsToEnv `Mem env lhs' in
      let init', init_typ = inferOptExp env' init in
      let rhs', rhs_typ   = inferOptExp env' rhs in
      unifyRaise (expOptLoc init') lhs_typ init_typ;
      unifyRaise (expOptLoc rhs') lhs_typ rhs_typ;
      StmtMem(lhs', init', rhs', attr), env', None
   | StmtTable(id,elems,attr) ->
      let elems',types = inferExpList env elems in
      let typ  = List.fold_left (fun typ a -> unify typ a; typ) (newvar ()) types in
      let env' = Env.addVar env id typ in
      StmtTable(id,elems',attr), env', None
   | StmtReturn(e,attr) ->
      let e', typ   = inferExp env e in
      let ret_type' = unifyOpt (expLoc e) ret_type (Some(typ)) in
      StmtReturn(e',attr), env, ret_type'
   | StmtBind(lhs,rhs,attr) ->
      let lhs', lhs_typ = inferLhsExp env lhs in
      let rhs', rhs_typ = inferExp env rhs in
      unifyRaise (expLoc rhs') lhs_typ rhs_typ;
      StmtBind(lhs',rhs',attr), env, ret_type
   | StmtBlock(name,stmts,attr) ->
      let env' = Env.enterBlock env in
      let stmts', env', stmt_ret_type = inferStmtList env' ret_type stmts in
      let env' = Env.exitBlock env' in
      StmtBlock(name,stmts',attr), env', stmt_ret_type
   | StmtFun(name,args,body,ret_type,attr) ->
      let env' = Env.enterFunction env name |> Env.prepareContext attr.fun_and in
      let args', types, env'  = addArgsToEnv env' args in
      let body',env',body_ret = inferStmt env' ret_type body in
      let last_type = getOptType body_ret in
      let typ  = makeArrowType last_type types in
      let env' = Env.setCurrentType env' typ in
      let env' = Env.exit env' in
      StmtFun(name,args',body',body_ret,attr), env', None
   | StmtIf(cond,then_,else_,attr) ->
      let cond', cond_type  = inferExp env cond in
      unifyRaise (expLoc cond') (Constants.bool_type()) cond_type;
      let then_', env', ret_type' = inferStmt env ret_type then_ in
      let else_', env', ret_type' = inferOptStmt env' ret_type' else_ in
      StmtIf(cond',then_',else_',attr), env', ret_type'
   | StmtWhile(cond,body,attr) ->
      let cond', cond_type  = inferExp env cond in
      unifyRaise (expLoc cond') (Constants.bool_type()) cond_type;
      let body', env', ret_type' = inferStmt env ret_type body in
      StmtWhile(cond',body',attr), env', ret_type'
   | StmtType(name,args,members,attr) ->
      StmtType(name,args,members,attr), env, ret_type
   | StmtAliasType (name, args, alias, attr) ->
      StmtAliasType (name, args, alias, attr), env, ret_type
   | StmtExternal(name,args,fun_ret_type,attr) ->
      let env' = Env.enterFunction env name in
      let args',types, env' = addArgsToEnv env' args in
      let typ  = makeArrowType fun_ret_type types in
      let env' = Env.setCurrentType env' typ in
      let env' = Env.exit env' in
      StmtExternal(name,args',fun_ret_type,attr), env', ret_type
   | StmtEmpty -> StmtEmpty, env, ret_type


and inferStmtList (env:'a Env.t) (ret_type:vtype option) (stmts:stmt list) : stmt list * 'a Env.t * vtype option =
   let stmts', env', ret_type' =
      List.fold_left
         (fun (stmts,env,ret_type) stmt ->
            let stmt', env', ret_type' = inferStmt env ret_type stmt in
            stmt' :: stmts, env', ret_type')
         ([], env, ret_type)
         stmts
   in (List.rev stmts'), env', ret_type'

and inferOptStmt (env:'a Env.t) (ret_type:vtype option) (stmt:stmt option) : stmt option * 'a Env.t * vtype option =
   match stmt with
   | None -> None, env, ret_type
   | Some(s) ->
      let s', s_type,s_ret_type = inferStmt env ret_type s in
      Some(s'), s_type, s_ret_type
