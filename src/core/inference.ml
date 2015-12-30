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

let gensym_counter = ref 0
let reset_gensym : unit -> unit =
   fun () -> gensym_counter := 0


let unit_type   = ref (TId(["unit"],None))
let bool_type   = ref (TId(["bool"],None))
let int_type    = ref (TId(["int"],None))
let real_type   = ref (TId(["real"],None))
let string_type = ref (TId(["string"],None))

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
let newvar : unit -> type_ref =
   fun () -> (ref (TUnbound (gensym (),!current_level,None)))


let pickLoc (loc1:Loc.t option) (loc2:Loc.t option) : Loc.t option =
   match loc1, loc2 with
   | None, _ -> loc2
   | _, None -> loc1
   | Some(l1),_ when l1 = Loc.default -> loc2
   | _,Some(l2) when l2 = Loc.default -> loc1
   | _ -> loc1


let rec makeArrowType (last:type_ref) (types:type_ref list) : type_ref =
   match types with
   | [] -> last
   | (h:type_ref)::t -> ref (TArrow(h,makeArrowType last t,None))


let rec unify (t1:type_ref) (t2:type_ref) : unit =
   if t1 == t2 then () else
   match t1,t2 with
   | { contents = TUnbound(n1,l1,loc1)}, { contents = TUnbound(_,l2,loc2) } ->
      let loc = pickLoc loc1 loc2 in
      let level = min l1 l2 in
      let n = if n1 = "" then gensym () else n1 in
      let t = TUnbound(n,level,loc) in
      t1 := t;
      t2 := TLink(t1)
   | { contents = TLink(tlink) },t | t, { contents = TLink(tlink) } ->
      unify t tlink
   | ({ contents = TUnbound _ } as tu), t | t, ({ contents = TUnbound _ } as tu) ->
      tu := TLink(t)
   | { contents = TComposed(n1,elems1,_) }, { contents = TComposed(n2,elems2,_) } when n1 = n2 ->
      List.iter2 unify elems1 elems2
   | { contents = tp1 }, { contents = tp2 } when equal_type_exp tp1 tp2 -> ()

   | _ -> failwith "type error"


let rec inferLhsExp (e:lhs_exp) : lhs_exp * type_ref =
   match e with
   | LWild(attr) ->
      let typ = newvar () in
      LWild( { attr with typ = Some(typ) }), typ
   | LId(id,None,attr) ->
      let typ = newvar () in
      LId(id,Some(typ),{ attr with typ = Some(typ) }), typ
   | LId(id,Some(typ),attr) ->
      LId(id,Some(typ),{ attr with typ = Some(typ) }), typ
   | LTuple(elems,attr) ->
      let elems',tpl =
         List.fold_left (fun (elems,tpl) a ->
            let a',typ = inferLhsExp a in
            a' :: elems, typ :: tpl )
         ([],[])
         elems
      in
      let typ = ref (TComposed(["tuple"],List.rev tpl,None)) in
      LTuple(List.rev elems',attr),typ
   | LTyped(e,typ,_) ->
      let e',tpi = inferLhsExp e in
      unify typ tpi;
      e',tpi

let rec inferExp (env:'a Env.t) (e:exp) : exp * type_ref =
   match e with
   | PUnit(attr) ->
      PUnit({ attr with typ = Some(unit_type) }), unit_type
   | PBool(v,attr) ->
      PBool(v,{ attr with typ = Some(bool_type) }), bool_type
   | PInt(v,attr) ->
      let typ = ref (TId(["int"],None)) in
      PInt(v,{ attr with typ = Some(typ) }), typ
   | PReal(v,attr) ->
      let typ = ref (TId(["real"],None)) in
      PReal(v,{ attr with typ = Some(typ) }), typ
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
      unify bool_type cond_type;
      unify then_type else_type;
      PIf(cond',then_',else_',{ attr with typ = Some(then_type) }), then_type
   | PCall(name,fname,args,attr) ->
      let args',types = inferExpList env args in
      let ret_type    = newvar () in
      let call_type   = makeArrowType ret_type types in
      let _, fn_type  = Env.lookup env fname in
      unify fn_type call_type;
      PCall(name,fname,args',{ attr with typ = Some(ret_type) }), ret_type
   | POp(op,args,attr) ->
      let args',types = inferExpList env args in
      let ret_type    = newvar () in
      let fn_type     = makeArrowType ret_type types in
      (* unify with operator type *)
      POp(op,args',{ attr with typ = Some(ret_type) }), ret_type
   | PUnOp(op,arg,attr) ->
      let arg',arg_type = inferExp env arg in
      let ret_type      = newvar () in
      let fn_type       = ref (TArrow(arg_type,ret_type,None)) in
      (* unify with operator type *)
      PUnOp(op,arg',{ attr with typ = Some(ret_type) }), ret_type
   | PEmpty -> PEmpty, unit_type

   | PSeq(_,_,_) -> failwith "PSeq: todo"

and inferExpList (env:'a Env.t) (elems:exp list) : exp list * type_ref list =
   let elems',types =
      List.fold_left (fun (elems,types) a ->
         let a',typ = inferExp env a in
         a' :: elems, typ :: types )
      ([],[])
      elems
   in List.rev elems', List.rev types

let inferOptExp (env:'a Env.t) (e:exp option) : exp option * type_ref =
   match e with
   | None    -> None, newvar ()
   | Some(e) ->
      let e',typ = inferExp env e in
      Some(e'), typ

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

let rec addArgsToEnv (env:'a Env.t) (args:typed_id list) : type_ref list * 'a Env.t =
   match args with
   | [] -> [], env
   | SimpleId(id,_)::t ->
      let typ = newvar () in
      let inner_typ, env' = addArgsToEnv env t in
      typ :: inner_typ, Env.addVar env' id typ
   | TypedId(id,typ,_)::t ->
      let inner_typ, env' = addArgsToEnv env t in
      typ :: inner_typ, Env.addVar env' id typ


let unifyOpt (typ1:type_ref option) (typ2:type_ref option) : type_ref option =
   match typ1,typ2 with
   | None, None    -> None
   | Some(_), None -> typ1
   | None, Some(_) -> typ2
   | Some(t1), Some(t2) ->
      unify t1 t2;
      typ1

let getOptType (typ:type_ref option) : type_ref =
   match typ with
   | None    -> newvar ()
   | Some(t) -> t

let rec inferStmt (env:'a Env.t) (ret_type:type_ref option) (stmt:stmt) : stmt * 'a Env.t * type_ref option =
   match stmt with
   | StmtVal(lhs,rhs,attr) ->
      let lhs', lhs_typ = inferLhsExp lhs in
      let rhs', rhs_typ = inferOptExp env rhs in
      unify lhs_typ rhs_typ;
      let env' = addLhsToEnv `Var env lhs' in
      StmtVal(lhs', rhs', attr), env', None
   | StmtMem(lhs,init,rhs,attr) ->
      let lhs', lhs_typ   = inferLhsExp lhs in
      let init', init_typ = inferOptExp env init in
      let rhs', rhs_typ   = inferOptExp env rhs in
      unify lhs_typ init_typ;
      unify init_typ rhs_typ;
      let env' = addLhsToEnv `Mem env lhs' in
      StmtMem(lhs', init', rhs', attr), env', None
   | StmtTable(id,elems,attr) ->
      let elems',types = inferExpList env elems in
      (* TODO: unify all types *)
      let typ = newvar () in
      let env' = Env.addVar env id typ in
      StmtTable(id,elems',attr), env', None
   | StmtReturn(e,attr) ->
      let e', typ   = inferExp env e in
      let ret_type' = unifyOpt (Some(typ)) ret_type in
      StmtReturn(e',attr), env, ret_type'
   | StmtBind(lhs,rhs,attr) ->
      let lhs', lhs_typ = inferLhsExp lhs in
      let rhs', rhs_typ = inferExp env rhs in
      unify lhs_typ rhs_typ;
      StmtBind(lhs',rhs',attr), env, ret_type
   | StmtBlock(name,stmts,attr) ->
      let env' = Env.enterBlock env in
      let stmts', env', stmt_ret_type = inferStmtList env' None stmts in
      let env' = Env.exitBlock env' in
      StmtBlock(name,stmts',attr), env', stmt_ret_type
   | StmtFun(name,args,body,ret_type,attr) ->
      let env' = Env.enterFunction env name in
      let types, env' = addArgsToEnv env' args in
      let body',env',body_ret = inferStmt env' None body in
      let last_type = unifyOpt ret_type body_ret |> getOptType in
      let typ  = makeArrowType last_type types in
      let env' = Env.setCurrentType env' typ in
      let env' = Env.exit env' in
      StmtFun(name,args,body',body_ret,attr), env', None
   | StmtIf(cond,then_,else_,attr) ->
      let cond', cond_type  = inferExp env cond in
      unify bool_type cond_type;
      let then_', env', ret_type' = inferStmt env ret_type then_ in
      let else_', env', ret_type' = inferOptStmt env' ret_type' else_ in
      StmtIf(cond',then_',else_',attr), env', ret_type'
   | StmtWhile(cond,body,attr) ->
      let cond', cond_type  = inferExp env cond in
      unify bool_type cond_type;
      let body', env', ret_type' = inferStmt env ret_type body in
      StmtWhile(cond',body',attr), env', ret_type'
   | StmtType(name,args,members,attr) ->
      StmtType(name,args,members,attr), env, ret_type
   | StmtAliasType (name, args, alias, attr) ->
      StmtAliasType (name, args, alias, attr), env, ret_type
   | StmtEmpty -> StmtEmpty, env, ret_type


and inferStmtList (env:'a Env.t) (ret_type:type_ref option) (stmts:stmt list) : stmt list * 'a Env.t * type_ref option =
   let stmts', env', ret_type' =
      List.fold_left
         (fun (stmts,env,ret_type) stmt ->
            let stmt', env', ret_type' = inferStmt env ret_type stmt in
            stmt' :: stmts, env', ret_type')
         ([], env, ret_type)
         stmts
   in (List.rev stmts'), env', ret_type'

and inferOptStmt (env:'a Env.t) (ret_type:type_ref option) (stmt:stmt option) : stmt option * 'a Env.t * type_ref option =
   match stmt with
   | None -> None, env, ret_type
   | Some(s) ->
      let s', s_type,s_ret_type = inferStmt env ret_type s in
      Some(s'), s_type, s_ret_type
