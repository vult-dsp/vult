(*
   The MIT License (MIT)

   Copyright (c) 2014-2024 Leonardo Laguna Ruiz

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
open Util
open Pparser
open Env
open Typed

let context_name = "_ctx"

let pickLoc (t1 : type_) (t2 : type_) : unit =
  if t1.loc == Loc.default then t1.loc <- t2.loc else if t2.loc == Loc.default then t2.loc <- t1.loc


let linkType ~from ~into =
  into.tx <- TELink from;
  pickLoc from into;
  true


let rec unlink (t : type_) =
  match t.tx with
  | TELink t -> unlink t
  | _ -> t


let path_string (p : Syntax.path) : string =
  match p with
  | { id; n = None; _ } -> id
  | { id; n = Some n; _ } -> n ^ "_" ^ id


(* Tries to unity the given type with all the elements of the set, if they can be unified we increase the counter *)
let rec pushTypeToSet (set : (type_ * int) list) (elem : type_) =
  match set, elem with
  | [], _ -> [ elem, 1 ]
  | (({ tx = TEComposed (n1, e1); _ } as h), count) :: t, { tx = TEComposed (n2, e2); _ }
    when n2 = n1 && List.length e1 = List.length e2 ->
    if unify h elem then
      (h, count + 1) :: t
    else
      (h, count) :: pushTypeToSet t elem
  (* Don't try to unify composed types with different arguments *)
  | (({ tx = TEComposed (n1, _); _ } as h), count) :: t, { tx = TEComposed (n2, _); _ } when n2 = n1 ->
    (h, count) :: pushTypeToSet t elem
  | (h, count) :: t, _ ->
    if unify h elem then
      (h, count + 1) :: t
    else
      (h, count) :: pushTypeToSet t elem


and constrainOption loc l1 l2 =
  let set = List.fold_left pushTypeToSet (List.map (fun e -> e, 1) l1) l2 in
  let final_set = List.filter_map (fun (e, n) -> if n > 1 then Some e else None) set in
  match final_set with
  | [] ->
    let t1 = Pla.map_sep Pla.commaspace (Typed.print_type_ ~show_unbound:false) l1 in
    let t2 = Pla.map_sep Pla.commaspace (Typed.print_type_ ~show_unbound:false) l2 in
    let msg = Pla.print {%pla|None of the following types: <#t1#>, matches with any of the following types <#t2#>. |} in
    Error.raiseError msg loc
  | [ t ] -> t
  | l -> { tx = TEOption l; loc = Loc.default }


and pickOption original l tt =
  let rec loop l =
    match l with
    | [] -> false
    | h :: t -> if unify h tt then linkType ~from:tt ~into:original else loop t
  in
  loop l


and unify (t1 : type_) (t2 : type_) =
  if t1 == t2 then
    true
  else (
    match t1.tx, t2.tx with
    | TEId t1, TEId t2 -> Pparser.Syntax.compare_path t1 t2 = 0
    | TESize t1, TESize t2 -> t1 = t2
    (* special case for arrays without dimensions *)
    | TEComposed ("array", [ e1; _ ]), TEComposed ("array", [ e2 ])
    | TEComposed ("array", [ e1 ]), TEComposed ("array", [ e2; _ ]) -> unify e1 e2
    | TEComposed (n1, e1), TEComposed (n2, e2) when n1 = n2 && List.length e1 = List.length e2 ->
      List.for_all2 unify e1 e2
    (* follow the links *)
    | TELink tlink, _ -> unify tlink t2
    | _, TELink tlink -> unify t1 tlink
    | TENoReturn, _ -> linkType ~from:t2 ~into:t1
    | _, TENoReturn -> linkType ~from:t1 ~into:t2
    (* replace any unbound *)
    | TEUnbound None, TEUnbound _ -> linkType ~from:t1 ~into:t2
    | TEUnbound _, TEUnbound None -> linkType ~from:t2 ~into:t1
    | TEUnbound _, _ -> linkType ~from:t2 ~into:t1
    | _, TEUnbound _ -> linkType ~from:t1 ~into:t2
    (* types with alternatives *)
    | TEOption l1, TEOption l2 ->
      let t3 = constrainOption t2.loc l1 l2 in
      let _ = linkType ~from:t3 ~into:t2 in
      linkType ~from:t3 ~into:t1
    | TEOption l, _ -> pickOption t1 l t2
    | _, TEOption l -> pickOption t2 l t1
    | TEId _, _ -> false
    | TESize _, _ -> false
    | TEComposed _, _ -> false)


let unifyRaise (loc : Loc.t) (t1 : type_) (t2 : type_) : unit =
  (* TODO: improve unify error reporting for tuples *)
  let raise = true in
  if not (unify t1 t2) then (
    let msg =
      let t1 = print_type_ t1 in
      let t2 = print_type_ t2 in
      Pla.print {%pla|This expression has type '<#t2#>' but '<#t1#>' was expected|}
    in
    if raise then
      Error.raiseError msg loc
    else (
      print_endline (Loc.to_string loc);
      print_endline msg))


let rec type_in_m (env : in_module) (t : Syntax.type_) =
  match t with
  | { t = STUnbound; loc } -> { tx = TEUnbound None; loc }
  | { t = STId path; loc } ->
    let found = Env.lookTypeInModule env path loc in
    { tx = TEId found.path; loc }
  | { t = STSize n; loc } ->
    let () =
      if n = 0 then (
        let msg = "Empty arrays are not supported" in
        Error.raiseError msg loc)
    in
    { tx = TESize n; loc }
  | { t = STComposed (name, l); loc } ->
    let l = List.map (type_in_m env) l in
    { tx = TEComposed (name, l); loc }


let rec checkArrayDimensions (t : type_) =
  match t.tx with
  | TEComposed ("array", [ _ ]) ->
    Error.raiseError "Array declarations should provide dimensions in the form array(type, size)" t.loc
  | TEComposed ("array", [ _; _ ]) -> ()
  | TELink t -> checkArrayDimensions t
  | _ -> ()


let type_in_c (env : Env.in_context) (t : Syntax.type_) = type_in_m (Env.exitContext env) t
let type_in_f (env : Env.in_func) (t : Syntax.type_) = type_in_c (Env.exitFunction env) t

let applyFunction loc (args_t_in : type_ list) (ret : type_) (args_in : exp list) =
  let rec loop (args_t : type_ list) args =
    match args_t, args with
    | [], _ :: _ ->
      let required_n = List.length args_t_in in
      let got_n = List.length args_in in
      let loc = Loc.mergeList loc (List.map (fun (e : exp) -> e.loc) args_in) in
      let msg = Pla.print {%pla|Extra arguments in function call. Expecting <#required_n#i> but got <#got_n#i>.|} in
      Error.raiseError msg loc
    | _ :: _, [] ->
      let required_n = List.length args_t_in in
      let got_n = List.length args_in in
      let loc = Loc.mergeList loc (List.map (fun (e : exp) -> e.loc) args_in) in
      let msg = Pla.print {%pla|Missing arguments in function call. Expecting <#required_n#i> but got <#got_n#i>.|} in
      Error.raiseError msg loc
    | [], [] -> ret
    | h :: args_t, (ht : exp) :: args ->
      unifyRaise ht.loc h ht.t;
      loop args_t args
  in
  loop args_t_in args_in


let rec addContextArg (env : Env.in_func) instance (f : Env.f) args loc =
  if Env.isFunctionActive f then (
    let cpath = Env.getContext env in
    let fpath = Env.getFunctionContext f in
    let ctx_t = C.path_t loc cpath in
    match Syntax.compare_path cpath fpath, instance with
    | 0, None ->
      let t = C.path_t loc fpath in
      let e = { e = EId context_name; t; loc } in
      env, e :: args
    | 0, Some _ ->
      let msg =
        Pla.print {%pla|This function belongs to the same instance and it must not be called on a different instance.|}
      in
      Error.raiseError msg loc
    (* no instance name provided *)
    | _, None ->
      let number =
        Printf.sprintf
          "%.2x%.2x"
          (0xFF land Hashtbl.hash (path_string fpath))
          (0xFF land Hashtbl.hash (path_string cpath))
      in
      let n = Env.getFunctionTick env in
      let name = "inst_" ^ string_of_int n ^ number in
      let t = C.path_t loc fpath in
      let env = Env.addVar env unify name t Inst loc in
      let e = { e = EMember ({ e = EId context_name; t = ctx_t; loc }, name); loc; t } in
      env, e :: args
    (* intance without subscripts *)
    | _, Some (name, None) ->
      let t = C.path_t loc fpath in
      let env = Env.addVar env unify name t Inst loc in
      let e = { e = EMember ({ e = EId context_name; t = ctx_t; loc }, name); loc; t } in
      env, e :: args
    (* array of instances *)
    | _, Some (name, Some index) ->
      let env, index = exp env index in
      unifyRaise index.loc (C.int ~loc:Loc.default) index.t;
      let et = C.path_t loc fpath in
      let t = C.array ~loc et in
      let env = Env.addVar env unify name t Inst loc in
      let e = { e = EMember ({ e = EId context_name; t = ctx_t; loc }, name); loc; t = et } in
      let e = { e = EIndex { e; index }; loc; t } in
      env, e :: args)
  else
    env, args


and exp (env : Env.in_func) (e : Syntax.exp) : Env.in_func * exp =
  match e with
  | { e = SEBool value; loc } ->
    let t = C.bool ~loc in
    env, { e = EBool value; t; loc }
  | { e = SEInt value; loc } ->
    let t = C.int ~loc in
    env, { e = EInt (int_of_string value); t; loc }
  | { e = SEReal value; loc } ->
    let t = C.real ~loc in
    env, { e = EReal (float_of_string value); t; loc }
  | { e = SEFixed value; loc } ->
    let t = C.fix16 ~loc in
    let value = String.sub value 0 (String.length value - 1) in
    env, { e = EFixed (float_of_string value); t; loc }
  | { e = SEString value; loc } ->
    let t = C.string ~loc in
    env, { e = EString value; t; loc }
  | { e = SEGroup e; _ } -> exp env e
  | { e = SEId name; loc } ->
    let var = Env.lookVar env name loc in
    let t = var.t in
    let e =
      match var.kind with
      | Val -> { e = EId name; t; loc }
      | Const -> { e = EConst { id = name; n = Some env.m.name; loc }; t; loc }
      | Mem _ | Inst ->
        let ctx = Env.getContext env in
        let ctx_t = C.path_t loc ctx in
        { e = EMember ({ e = EId context_name; t = ctx_t; loc }, name); t; loc }
    in
    env, e
  | { e = SEIndex { e; index }; loc } ->
    let env, e = exp env e in
    let env, index = exp env index in
    let t = C.unbound Loc.default in
    unifyRaise e.loc (C.array ~fixed:false t) e.t;
    unifyRaise index.loc (C.int ~loc:Loc.default) index.t;
    env, { e = EIndex { e; index }; t; loc }
  | { e = SEArray []; loc } -> Error.raiseError "Empty arrays are not supported." loc
  | { e = SEArray (h :: t); loc } ->
    let env, h = exp env h in
    let env, t_rev, size =
      List.fold_left
        (fun (env, acc, size) e ->
          let env, e = exp env e in
          unifyRaise e.loc h.t e.t;
          env, e :: acc, size + 1)
        (env, [], 1)
        t
    in
    let t = C.array ~size:(C.size ~loc size) h.t in
    env, { e = EArray (h :: List.rev t_rev); t; loc }
  | { e = SETuple l; loc } ->
    let env, l = exp_list env l in
    let t = C.tuple ~loc (List.map (fun (e : exp) -> e.t) l) in
    env, { e = ETuple l; t; loc }
  | { e = SEIf { cond; then_; else_ }; loc } ->
    let env, cond = exp env cond in
    let env, then_ = exp env then_ in
    let env, else_ = exp env else_ in
    let t = then_.t in
    unifyRaise cond.loc (C.bool ~loc) cond.t;
    unifyRaise else_.loc then_.t else_.t;
    env, { e = EIf { cond; then_; else_ }; t; loc }
  | { e = SECall { instance; path; args }; loc } ->
    let env, args = exp_list env args in
    let f = Env.lookFunctionCall env path loc in
    let args_t, ret = f.t in
    let t = applyFunction e.loc args_t ret args in
    let env, args = addContextArg env instance f args loc in
    env, { e = ECall { instance = None; path = f.path; args }; t; loc }
  | { e = SEOp (op, e1, e2); loc } ->
    let env, e1 = exp env e1 in
    let env, e2 = exp env e2 in
    let f = Env.lookOperator env op in
    let args_t, ret = f.t in
    let t = applyFunction e.loc args_t ret [ e1; e2 ] in
    env, { e = EOp (op, e1, e2); t; loc }
  | { e = SEUnOp (op, e); loc } ->
    let env, e = exp env e in
    let f = Env.lookOperator env ("u" ^ op) in
    let args_t, ret = f.t in
    let t = applyFunction e.loc args_t ret [ e ] in
    env, { e = EUnOp (op, e); t; loc }
  | { e = SEMember (e, m); loc } -> (
    let env, e = exp env e in
    match (unlink e.t).tx with
    | TEId path -> (
      match Env.lookType env path loc with
      | { path; descr = Record members; _ } -> (
        match Map.find m members with
        | None -> Error.raiseError ("The field '" ^ m ^ "' is not part of the type '" ^ pathString path ^ "'") loc
        | Some { t; _ } -> env, { e = EMember (e, m); t; loc })
      | _ ->
        let t = Pla.print (Typed.print_type_ e.t) in
        let e = Pla.print (Typed.print_exp e) in
        Error.raiseError ("The expression '" ^ e ^ "' of type '" ^ t ^ "' does not have a member '" ^ m ^ "'.") loc)
    | _ ->
      let t = Pla.print (Typed.print_type_ e.t) in
      let e = Pla.print (Typed.print_exp e) in
      Error.raiseError ("The expression '" ^ e ^ "' of type '" ^ t ^ "' does not have a member '" ^ m ^ "'.") loc)
  | { e = SEEnum path; loc } ->
    let type_path, tloc, index = Env.lookEnum env path loc in
    let t = C.path_t tloc type_path in
    env, { e = EInt index; t; loc }


and exp_list (env : Env.in_func) (l : Syntax.exp list) : Env.in_func * exp list =
  let env, rev_l =
    List.fold_left
      (fun (env, acc) e ->
        let env, e = exp env e in
        env, e :: acc)
      (env, [])
      l
  in
  env, List.rev rev_l


and lexp (env : Env.in_func) (e : Syntax.lexp) : Env.in_func * lexp =
  match e with
  | { l = SLWild; loc } ->
    let t = C.noreturn loc in
    env, { l = LWild; t; loc }
  | { l = SLId name; loc } ->
    let var = Env.lookVar env name loc in
    var.const <- false;
    let t = var.t in
    let e =
      match var.kind with
      | Val -> { l = LId name; t; loc }
      | Mem _ | Inst ->
        let ctx = Env.getContext env in
        let ctx_t = C.path_t loc ctx in
        { l = LMember ({ l = LId context_name; t = ctx_t; loc }, name); t; loc }
      | Const -> Error.raiseError ("The constant '" ^ name ^ "' cannot be assigned") loc
    in
    env, e
  | { l = SLGroup e; _ } -> lexp env e
  | { l = SLTuple elems; loc } ->
    let env, elems =
      List.fold_left
        (fun (env, acc) e ->
          let env, e = lexp env e in
          env, e :: acc)
        (env, [])
        (List.rev elems)
    in
    let t_elems = List.map (fun (e : lexp) -> e.t) elems in
    let t = C.tuple ~loc t_elems in
    env, { l = LTuple elems; t; loc }
  | { l = SLIndex { e; index }; loc } ->
    let env, e = lexp env e in
    let env, index = exp env index in
    let t = C.unbound loc in
    unifyRaise index.loc (C.int ~loc) index.t;
    unifyRaise e.loc (C.array ~fixed:false ~loc t) e.t;
    env, { l = LIndex { e; index }; t; loc }
  | { l = SLMember (e, m); loc } -> (
    let env, e = lexp env e in
    match (unlink e.t).tx with
    | TEId path -> (
      match Env.lookType env path loc with
      | { path; descr = Record members; _ } -> (
        match Map.find m members with
        | None -> Error.raiseError ("The field '" ^ m ^ "' is not part of the type '" ^ pathString path ^ "'") loc
        | Some { t; _ } -> env, { l = LMember (e, m); t; loc })
      | _ ->
        let t = Pla.print (Typed.print_type_ e.t) in
        let e = Pla.print (Typed.print_lexp e) in
        Error.raiseError ("The expression' " ^ e ^ "' of type '" ^ t ^ "' does not have a member '" ^ m ^ "'.") loc)
    | _ ->
      let t = Pla.print (Typed.print_type_ e.t) in
      let e = Pla.print (Typed.print_lexp e) in
      Error.raiseError ("The expression' " ^ e ^ "' of type '" ^ t ^ "' does not have a member '" ^ m ^ "'.") loc)


and dexp (env : Env.in_func) (e : Syntax.dexp) (kind : var_kind) : Env.in_func * dexp =
  match e with
  | { d = SDWild; loc } ->
    let t = C.noreturn loc in
    env, { d = DWild; t; loc }
  | { d = SDTuple l; loc } ->
    let env, l =
      List.fold_left
        (fun (env, acc) e ->
          let env, e = dexp env e kind in
          env, e :: acc)
        (env, [])
        (List.rev l)
    in
    let t = C.tuple ~loc (List.map (fun (e : dexp) -> e.t) l) in
    env, { d = DTuple l; t; loc }
  | { d = SDGroup e; _ } -> dexp env e kind
  | { d = SDTyped (e, t); _ } ->
    let env, e = dexp env e kind in
    let t = type_in_f env t in
    checkArrayDimensions t;
    unifyRaise e.loc t e.t;
    env, e
  | { d = SDId (name, dims); loc } ->
    let t =
      match dims with
      | Some size -> C.array ~loc ~size:(C.size ~loc size) (C.unbound loc)
      | None -> C.unbound loc
    in
    let env = Env.addVar env unify name t kind loc in
    env, { d = DId (name, dims); t; loc }


let rec dexp_to_lexp (d : Syntax.dexp) : Syntax.lexp =
  let loc = d.loc in
  match d.d with
  | SDTuple l ->
    let l = List.map dexp_to_lexp l in
    { l = SLTuple l; loc }
  | SDWild -> { l = SLWild; loc }
  | SDId (name, _) -> { l = SLId name; loc }
  | SDGroup e -> dexp_to_lexp e
  | SDTyped (e, _) -> dexp_to_lexp e


let stmt_block (stmts : stmt list) =
  match stmts with
  | [ s ] -> s
  | _ -> { s = StmtBlock stmts; loc = Loc.default }


let makeIterWhile (env : Env.in_func) name id_loc value body loc =
  let tick = Env.getFunctionTick env in
  let itname = name ^ "__" ^ string_of_int tick in
  let open Syntax in
  let int_type = { t = STId { id = "int"; n = None; loc = id_loc }; loc } in
  let dlhs = { d = SDTyped ({ d = SDId (itname, None); loc = id_loc }, int_type); loc = id_loc } in
  let lhs = { l = SLId itname; loc = id_loc } in
  let rhs = { e = SEId itname; loc = id_loc } in
  let decl = { s = SStmtVal (dlhs, Some { e = SEInt "0"; loc }); loc } in
  let incr = { s = SStmtBind (lhs, { e = SEOp ("+", rhs, { e = SEInt "1"; loc }); loc }); loc } in
  let new_body = Syntax.ReaplaceId.inStmt [ name, itname ] { s = SStmtBlock [ body; incr ]; loc } in
  let cond = { e = SEOp ("<", rhs, value); loc } in
  let while_s = { s = SStmtWhile (cond, new_body); loc } in
  { s = SStmtBlock [ decl; while_s ]; loc }


let makeIfOfMatch e cases =
  let rec makeComparison (e : Syntax.exp) (p : Syntax.pattern) =
    let makeEq e1 e2 = Syntax.{ e = SEOp ("==", e1, e2); loc = e1.loc } in
    let makeAnd e1 e2 = Syntax.{ e = SEOp ("&&", e1, e2); loc = e1.loc } in
    match e, p with
    | _, { p = SPWild; loc } -> Syntax.{ e = SEBool true; loc }
    | { e = SEGroup e; _ }, _ -> makeComparison e p
    | e, { p = SPGroup p; _ } -> makeComparison e p
    | { e = SETuple elems; _ }, { p = SPTuple patterns; loc } ->
      if List.length elems = List.length patterns then (
        let conds = List.map2 (fun e p -> makeComparison e p) elems patterns in
        List.fold_right makeAnd conds Syntax.{ e = SEBool true; loc })
      else (
        let msg =
          "The pattern cannot be matched with the input expression because it has different number of elements."
        in
        let loc = Loc.mergeList Loc.default @@ List.map (fun (p : Syntax.pattern) -> p.loc) patterns in
        Error.raiseError msg loc)
    | { e = SETuple _; _ }, { loc; _ } ->
      let msg =
        "The pattern cannot be matched with the input expression because it has different number of elements."
      in
      Error.raiseError msg loc
    | _, { p = SPTuple patterns; _ } ->
      let loc = Loc.mergeList Loc.default @@ List.map (fun (p : Syntax.pattern) -> p.loc) patterns in
      let msg =
        "The pattern cannot be matched with the input expression because it has different number of elements."
      in
      Error.raiseError msg loc
    | _, { p = SPBool b; loc } -> makeEq e Syntax.{ e = SEBool b; loc }
    | _, { p = SPInt i; loc } -> makeEq e Syntax.{ e = SEInt i; loc }
    | _, { p = SPReal f; loc } -> makeEq e Syntax.{ e = SEReal f; loc }
    | _, { p = SPFixed f; loc } -> makeEq e Syntax.{ e = SEFixed f; loc }
    | _, { p = SPString s; loc } -> makeEq e Syntax.{ e = SEString s; loc }
    | _, { p = SPEnum p; loc } -> makeEq e Syntax.{ e = SEEnum p; loc }
  in
  let if_stmt =
    List.fold_right
      (fun (p, case) else_ ->
        let cond = makeComparison e p in
        Some Syntax.{ s = SStmtIf (cond, case, else_); loc = cond.loc })
      cases
      None
  in
  match if_stmt with
  | None -> failwith ""
  | Some stmt -> stmt


let rec stmt (env : Env.in_func) (return : type_) (s : Syntax.stmt) : Env.in_func * stmt list =
  match s with
  | { s = SStmtError; _ } -> env, []
  | { s = SStmtBlock stmts; loc } ->
    let env = Env.pushScope env in
    let env, stmts = stmt_list env return stmts in
    let env = Env.popScope env in
    env, [ { s = StmtBlock stmts; loc } ]
  | { s = SStmtVal (lhs, None); loc } ->
    let env, lhs = dexp env lhs Val in
    env, [ { s = StmtVal lhs; loc } ]
  | { s = SStmtVal (lhs, Some rhs); loc } ->
    let env, dlhs = dexp env lhs Val in
    let env, lhs = lexp env (dexp_to_lexp lhs) in
    let env, rhs = exp env rhs in
    unifyRaise rhs.loc dlhs.t rhs.t;
    env, [ { s = StmtVal dlhs; loc }; { s = StmtBind (lhs, rhs); loc } ]
  | { s = SStmtMem (lhs, None, tags); loc } ->
    let env, lhs = dexp env lhs (Mem tags) in
    env, [ { s = StmtMem (lhs, tags); loc } ]
  | { s = SStmtMem (lhs, Some rhs, tags); loc } ->
    let env, dlhs = dexp env lhs (Mem tags) in
    let env, lhs = lexp env (dexp_to_lexp lhs) in
    let env, rhs = exp env rhs in
    unifyRaise rhs.loc lhs.t rhs.t;
    env, [ { s = StmtMem (dlhs, tags); loc }; { s = StmtBind (lhs, rhs); loc } ]
  | { s = SStmtBind (lhs, rhs); loc } ->
    let env, lhs = lexp env lhs in
    let env, rhs = exp env rhs in
    unifyRaise rhs.loc lhs.t rhs.t;
    env, [ { s = StmtBind (lhs, rhs); loc } ]
  | { s = SStmtReturn e; loc } ->
    let env, e = exp env e in
    unifyRaise e.loc return e.t;
    env, [ { s = StmtReturn e; loc } ]
  | { s = SStmtIf (cond, then_, else_); loc } ->
    let env, cond = exp env cond in
    unifyRaise cond.loc (C.bool ~loc) cond.t;
    let env, then_ = stmt env return then_ in
    let env, else_ = stmt_opt env return else_ in
    env, [ { s = StmtIf (cond, stmt_block then_, else_); loc } ]
  | { s = SStmtWhile (cond, s); loc } ->
    let env, cond = exp env cond in
    unifyRaise cond.loc (C.bool ~loc) cond.t;
    let env, s = stmt env return s in
    env, [ { s = StmtWhile (cond, stmt_block s); loc } ]
  | { s = SStmtIter { id = name, id_loc; value; body }; loc } ->
    let while_s = makeIterWhile env name id_loc value body loc in
    stmt env return while_s
  | { s = SStmtMatch { e; cases }; _ } ->
    let if_stmt = makeIfOfMatch e cases in
    stmt env return if_stmt


and stmt_opt env return s =
  match s with
  | None -> env, None
  | Some s ->
    let env, s = stmt env return s in
    env, Some (stmt_block s)


and stmt_list env return l =
  let env, l_rev =
    List.fold_left
      (fun (env, acc) s ->
        let env, s = stmt env return s in
        env, s :: acc)
      (env, [])
      l
  in
  env, List.flatten (List.rev l_rev)


let addGeneratedFunctions tags name next =
  if Ptags.has tags "wave" then (
    let code = Pla.print {%pla|fun <#name#s>_samples() : int @[placeholder]|} in
    let def = Parse.parseFunctionSpec code in
    Some ({ def with next }, Syntax.{ s = SStmtBlock []; loc = Loc.default }))
  else if Ptags.has tags "wavetable" then (
    let empty = Syntax.{ s = SStmtBlock []; loc = Loc.default } in
    let samples = Pla.print {%pla|fun <#name#s>_samples() : int @[placeholder]|} in
    let code1 = Pla.print {%pla|fun <#name#s>_raw_c0(i:int) : real @[placeholder]|} in
    let code2 = Pla.print {%pla|fun <#name#s>_raw_c1(i:int) : real @[placeholder]|} in
    let samples = Parse.parseFunctionSpec samples in
    let def1 = Parse.parseFunctionSpec code1 in
    let def2 = Parse.parseFunctionSpec code2 in
    Some ({ def1 with next = Some ({ def2 with next = Some ({ samples with next }, empty) }, empty) }, empty))
  else
    next


let getOptType env loc (t : Syntax.type_ option) =
  match t with
  | None -> C.unbound loc
  | Some t -> type_in_c env t


let getReturnType env (t : Syntax.type_ option) =
  match t with
  | None -> None
  | Some t -> Some (type_in_c env t)


let convertArguments env (args : Syntax.arg list) : arg list =
  List.map (fun (name, t, loc) -> name, getOptType env loc t, loc) args


let registerMultiReturnMem (env : Env.in_context) name t loc =
  let _, ret = t in
  match unlink ret with
  | { tx = TEComposed ("tuple", elems); _ } ->
    let names = List.mapi (fun i t -> path_string name ^ "_ret_" ^ string_of_int i, t) elems in
    List.fold_left (fun env (name, t) -> Env.addReturnVar env name t loc) env names
  | _ -> env


let isRoot (args : Args.args) path =
  let s_path = Pla.print (Syntax.print_path path) in
  List.mem s_path args.roots


let customInitializer (env : Env.in_context) tags name =
  if Ptags.has tags "init" then Env.addCustomInitFunction env name else env


let reportReturnTypeMismatch is_placeholder loc (specified_ret : type_ option) (inferred_ret : type_) =
  match specified_ret, inferred_ret with
  | None, { tx = Typed.TENoReturn; _ } -> unifyRaise loc (C.noreturn loc) inferred_ret
  | None, _ -> ()
  | Some t, { tx = Typed.TENoReturn; _ } ->
    (* If the function is a placeholder it will not have body, then the inferred type will be unbound.
       In this case we need to unify the specified and the inferred. *)
    if is_placeholder then
      unifyRaise loc t inferred_ret
    else (
      let t = Pla.print (print_type_ t) in
      Error.raiseError ("This function is expected to have type '" ^ t ^ "' but nothing was returned.") loc)
  | Some t1, t2 -> unifyRaise loc t1 t2


let rec function_def (iargs : Args.args) (env : Env.in_context) ((def : Syntax.function_def), (body : Syntax.stmt))
  : Env.in_context * (function_def * stmt)
  =
  let specified_ret = getReturnType env def.t in
  let inferred_ret = C.noreturn def.loc in
  let args = convertArguments env def.args in
  let env, path, t = Env.enterFunction env def.name args inferred_ret def.loc in
  let env, body = stmt env inferred_ret body in
  let env = Env.exitFunction env in
  let next = addGeneratedFunctions def.tags def.name def.next in
  let env, next = function_def_opt iargs env next in
  let env = registerMultiReturnMem env path t def.loc in
  let env = customInitializer env def.tags path in
  let is_root = isRoot iargs path in
  let is_placeholder = Ptags.has def.tags "placeholder" in
  let () = reportReturnTypeMismatch is_placeholder def.loc specified_ret inferred_ret in
  env, ({ name = path; args; t; loc = def.loc; tags = def.tags; next; is_root }, stmt_block body)


and function_def_opt (iargs : Args.args) (env : Env.in_context) def_opt =
  match def_opt with
  | None -> env, None
  | Some (def, body) ->
    let env = Env.addAliasToContext env def.name def.loc in
    let env, def_body = function_def iargs env (def, body) in
    env, Some def_body


let ext_function (iargs : Args.args) (env : Env.in_context) (def : Syntax.ext_def) : Env.in_context * function_def =
  let ret = getOptType env def.loc def.t in
  let args = convertArguments env def.args in
  let env, path, t = Env.enterFunction env def.name args ret def.loc in
  let env = Env.exitFunction env in
  let next = addGeneratedFunctions def.tags def.name None in
  let env, next = function_def_opt iargs env next in
  env, { name = path; args; t; loc = def.loc; tags = def.tags; next; is_root = false }


let getContextArgument (context : Env.context) loc : arg option =
  match context with
  | Some (p, { descr = Record members; _ }) ->
    if Map.is_empty members then
      None
    else (
      let ctx_t = C.path_t loc p in
      Some (context_name, ctx_t, loc))
  | _ -> None


let insertContextArgument (env : Env.in_context) (def : function_def) : function_def =
  match getContextArgument env.context def.loc with
  | None -> def
  | Some arg ->
    let rec loop next =
      match next with
      | Some (def, body) ->
        let next = loop def.next in
        Some ({ def with args = arg :: def.args; next }, body)
      | None -> None
    in
    let next = loop def.next in
    { def with args = arg :: def.args; next }


let top_dexp (env : Env.in_module) (d : Syntax.dexp) =
  match d with
  | { d = SDId (name, dims); loc } ->
    let t =
      match dims with
      | Some size -> C.array ~loc ~size:(C.size ~loc size) (C.unbound loc)
      | None -> C.unbound loc
    in
    (*let env = Env.addVar env unify name t kind loc in*)
    env, { d = DId (name, dims); t; loc }
  | _ -> failwith "invalid constant"


let rec top_exp (env : Env.in_module) (e : Syntax.exp) : Env.in_module * exp =
  match e with
  | { e = SEBool value; loc } ->
    let t = C.bool ~loc in
    env, { e = EBool value; t; loc }
  | { e = SEInt value; loc } ->
    let t = C.int ~loc in
    env, { e = EInt (int_of_string value); t; loc }
  | { e = SEReal value; loc } ->
    let t = C.real ~loc in
    env, { e = EReal (float_of_string value); t; loc }
  | { e = SEFixed value; loc } ->
    let t = C.fix16 ~loc in
    let value = String.sub value 0 (String.length value - 1) in
    env, { e = EFixed (float_of_string value); t; loc }
  | { e = SEString value; loc } ->
    let t = C.string ~loc in
    env, { e = EString value; t; loc }
  | { e = SEGroup e; _ } -> top_exp env e
  | { e = SEId name; loc } ->
    let var = Env.lookConstant env name loc in
    let t = var.t in
    env, { e = EConst { id = name; n = Some env.m.name; loc }; t; loc }
  | { e = SEIndex { e; index }; loc } ->
    let env, e = top_exp env e in
    let env, index = top_exp env index in
    let t = C.unbound Loc.default in
    unifyRaise e.loc (C.array ~fixed:false t) e.t;
    unifyRaise index.loc (C.int ~loc:Loc.default) index.t;
    env, { e = EIndex { e; index }; t; loc }
  | { e = SEArray []; loc } -> Error.raiseError "Empty arrays are not supported." loc
  | { e = SEArray (h :: t); loc } ->
    let env, h = top_exp env h in
    let env, t_rev, size =
      List.fold_left
        (fun (env, acc, size) e ->
          let env, e = top_exp env e in
          unifyRaise e.loc h.t e.t;
          env, e :: acc, size + 1)
        (env, [], 1)
        t
    in
    let t = C.array ~size:(C.size ~loc size) h.t in
    env, { e = EArray (h :: List.rev t_rev); t; loc }
  | { e = SETuple l; loc } ->
    let env, l = top_exp_list env l in
    let t = C.tuple ~loc (List.map (fun (e : exp) -> e.t) l) in
    env, { e = ETuple l; t; loc }
  | { e = SEIf { cond; then_; else_ }; loc } ->
    let env, cond = top_exp env cond in
    let env, then_ = top_exp env then_ in
    let env, else_ = top_exp env else_ in
    let t = then_.t in
    unifyRaise cond.loc (C.bool ~loc) cond.t;
    unifyRaise else_.loc then_.t else_.t;
    env, { e = EIf { cond; then_; else_ }; t; loc }
  | { e = SECall _; loc } -> Error.raiseError "Function calls are currently not supported in constants." loc
  | { e = SEOp (op, e1, e2); loc } ->
    let env, e1 = top_exp env e1 in
    let env, e2 = top_exp env e2 in
    let f = Env.lookOperatorInModule env op in
    let args_t, ret = f.t in
    let t = applyFunction e.loc args_t ret [ e1; e2 ] in
    env, { e = EOp (op, e1, e2); t; loc }
  | { e = SEUnOp (op, e); loc } ->
    let env, e = top_exp env e in
    let f = Env.lookOperatorInModule env ("u" ^ op) in
    let args_t, ret = f.t in
    let t = applyFunction e.loc args_t ret [ e ] in
    env, { e = EUnOp (op, e); t; loc }
  | { e = SEMember (e, m); loc } -> (
    let env, e = top_exp env e in
    match (unlink e.t).tx with
    | TEId path -> (
      match Env.lookTypeInModule env path loc with
      | { path; descr = Record members; _ } -> (
        match Map.find m members with
        | None -> Error.raiseError ("The field '" ^ m ^ "' is not part of the type '" ^ pathString path ^ "'") loc
        | Some { t; _ } -> env, { e = EMember (e, m); t; loc })
      | _ ->
        let t = Pla.print (Typed.print_type_ e.t) in
        let e = Pla.print (Typed.print_exp e) in
        Error.raiseError ("The expression '" ^ e ^ "' of type '" ^ t ^ "' does not have a member '" ^ m ^ "'.") loc)
    | _ ->
      let t = Pla.print (Typed.print_type_ e.t) in
      let e = Pla.print (Typed.print_exp e) in
      Error.raiseError ("The expression '" ^ e ^ "' of type '" ^ t ^ "' does not have a member '" ^ m ^ "'.") loc)
  | { e = SEEnum path; loc } ->
    let type_path, tloc, index = Env.lookEnumInModule env path loc in
    let t = C.path_t tloc type_path in
    env, { e = EInt index; t; loc }


and top_exp_list (env : Env.in_module) (l : Syntax.exp list) : Env.in_module * exp list =
  let env, rev_l =
    List.fold_left
      (fun (env, acc) e ->
        let env, e = top_exp env e in
        env, e :: acc)
      (env, [])
      l
  in
  env, List.rev rev_l


let rec top_stmt (iargs : Args.args) (env : Env.in_module) (s : Syntax.top_stmt) : Env.in_module * top_stmt =
  match s with
  | { top = STopError; _ } -> failwith "Parser error"
  | { top = STopFunction (def, body); _ } ->
    let env = Env.createContextForFunction env def.name def.loc in
    let env, (def, body) = function_def iargs env (def, body) in
    let def = insertContextArgument env def in
    let env = Env.exitContext env in
    env, { top = TopFunction (def, body); loc = def.loc }
  | { top = STopExternal (def, link_name); _ } ->
    let env = Env.createContextForExternal env in
    let env, def = ext_function iargs env def in
    let env = Env.exitContext env in
    env, { top = TopExternal (def, link_name); loc = def.loc }
  | { top = STopType { name; members }; loc } ->
    let members = List.map (fun (name, t, tags, loc) -> name, type_in_m env t, tags, loc) members in
    let members = List.sort (fun (n1, _, _, _) (n2, _, _, _) -> compare n1 n2) members in
    let env = Env.addType env name members loc in
    let path = Env.getPath env.m name loc in
    env, { top = TopType { path; members }; loc }
  | { top = STopEnum { name; members }; loc } ->
    let env = Env.addEnum env name members loc in
    let path = Env.getPath env.m name loc in
    env, { top = TopEnum { path; members }; loc }
  | { top = STopConstant (({ d = SDId (name, dim); _ } as d), e); loc } ->
    let env, d = top_dexp env d in
    let env, e = top_exp env e in
    unifyRaise e.loc d.t e.t;
    let path = Env.getPath env.m name loc in
    let env = Env.addConstant env unify name d.t loc in
    env, { top = TopConstant (path, dim, d.t, e); loc }
  | { top = STopConstant _; _ } -> failwith ""


and top_stmt_list (iargs : Args.args) (env : Env.in_module) (s : Syntax.top_stmt list) : Env.in_module * top_stmt list =
  let env, rev_s =
    List.fold_left
      (fun (env, acc) s ->
        let env, s = top_stmt iargs env s in
        env, s :: acc)
      (env, [])
      s
  in
  env, rev_s


let getTypesFromModule m =
  Map.fold
    (fun _ t s ->
      match t.descr with
      | Record members when Map.is_empty members -> s
      | Record _ -> t :: s
      | Alias _ -> t :: s
      | Simple | Enum _ -> s)
    []
    m.Env.types


let createTypes (env : Env.in_top) =
  let types =
    Map.fold
      (fun _ m s ->
        let types = getTypesFromModule m in
        types @ s)
      []
      env.modules
  in
  (* sort the types *)
  let types =
    types |> List.filter (fun (t : Env.t) -> t.generated) |> List.sort (fun (a : Env.t) b -> compare a.index b.index)
  in
  List.map
    (fun (t : Env.t) ->
      match t.descr with
      | Record members ->
        let members = Map.fold (fun _ (var : Env.var) s -> (var.name, var.t, var.tags, var.loc) :: s) [] members in
        let members = List.sort (fun (n1, _, _, _) (n2, _, _, _) -> compare n1 n2) members in
        { top = TopType { path = t.path; members }; loc = t.loc }
      | Alias (path, alias_of) -> { top = TopAlias { path; alias_of }; loc = t.loc }
      | Enum _ | Simple -> failwith "There should not be other than records here")
    types


module Set = Set.Make (struct
    type t = path

    let compare = Syntax.compare_path
  end)

let rec createExistingTypeSet stmts : Set.t =
  match stmts with
  | [] -> Set.empty
  | { top = TopType { path; _ }; _ } :: t -> Set.add path (createExistingTypeSet t)
  | _ :: t -> createExistingTypeSet t


let removeExistingTypes set types =
  let f s =
    match s with
    | { top = TopType { path; _ }; _ } when Set.mem path set -> false
    | _ -> true
  in
  List.filter f types


let infer_single (iargs : Args.args) (env : Env.in_top) (h : Parse.parsed_file) : Env.in_top * top_stmt list =
  let set = createExistingTypeSet (createTypes env) in
  let env = Env.enterModule env h.name in
  let env, stmt = top_stmt_list iargs env h.stmts in
  let env = Env.exitModule env in
  let types = removeExistingTypes set (createTypes env) in
  env, stmt @ types


let infer (iargs : Args.args) (parsed : Parse.parsed_file list) : Env.in_top * top_stmt list =
  let env, stmts =
    List.fold_left
      (fun (env, acc) (h : Parse.parsed_file) ->
        let env = Env.enterModule env h.name in
        let env, stmt = top_stmt_list iargs env h.stmts in
        let env = Env.exitModule env in
        env, stmt @ acc)
      (Env.empty (), [])
      parsed
  in
  let types = createTypes env in
  env, types @ List.rev stmts
