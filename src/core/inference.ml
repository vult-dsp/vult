(*
   The MIT License (MIT)

   Copyright (c) 2020 Leonardo Laguna Ruiz

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

open Env
open Typed
open ExtendedType

let context_name = "_ctx"

let pickLoc (t1 : type_) (t2 : type_) : unit =
  if t1.loc == Loc.default then
    t1.loc <- t2.loc
  else if t2.loc == Loc.default then
    t2.loc <- t1.loc


let linkType ~from ~into =
  into.tx <- TELink from ;
  pickLoc from into ;
  true


let constrainOption l1 l2 =
  let l2_ = List.filter (fun e2 -> List.exists (fun e1 -> compare_type_ e1 e2 = 0) l1) l2 in
  let l1_ = List.filter (fun e1 -> List.exists (fun e2 -> compare_type_ e2 e1 = 0) l2_) l1 in
  match l1_ with
  | [] -> failwith "cannot unify the two options"
  | [ t ] -> t
  | l -> { tx = TEOption l; loc = Loc.default }


let rec pickOption original l tt =
  let rec loop l =
    match l with
    | [] -> failwith "does not match a ny type"
    | h :: t ->
        if unify h tt then
          linkType tt original
        else
          loop t
  in
  loop l


and unify (t1 : type_) (t2 : type_) =
  if t1 == t2 then
    true
  else
    match t1.tx, t2.tx with
    | TEId t1, TEId t2 -> Syntax.compare_path t1 t2 = 0
    | TESize t1, TESize t2 -> t1 = t2
    | TEComposed (n1, e1), TEComposed (n2, e2) when n1 = n2 && List.length e1 = List.length e2 ->
        List.for_all2 unify e1 e2
    (* follow the links *)
    | TELink tlink, _ -> unify tlink t2
    | _, TELink tlink -> unify t1 tlink
    | TENoReturn, _ -> linkType t2 t1
    | _, TENoReturn -> linkType t1 t2
    (* replace any unbound *)
    | TEUnbound _, _ -> linkType t2 t1
    | _, TEUnbound _ -> linkType t1 t2
    (* types with alternatives *)
    | TEOption l1, TEOption l2 ->
        let t3 = constrainOption l1 l2 in
        let _ = linkType t3 t2 in
        linkType t3 t1
    | TEOption l, _ -> pickOption t1 l t2
    | _, TEOption l -> pickOption t2 l t1
    | TEId _, _ -> false
    | TESize _, _ -> false
    | TEComposed _, _ -> false


let unifyRaise (loc : Loc.t) (t1 : type_) (t2 : type_) : unit =
  let raise = true in
  if not (unify t1 t2) then
    let msg =
      let t1 = print_type_ t1 in
      let t2 = print_type_ t2 in
      Pla.print {pla|"This expression has type '<#t2#>' but '<#t1#>' was expected"|pla}
    in
    if raise then
      Error.raiseError msg loc
    else (
      print_endline (Loc.to_string loc) ;
      print_endline msg )


let rec type_ (t : Syntax.type_) =
  match t with
  | { t = STId path; loc } -> { tx = TEId path; loc }
  | { t = STSize n; loc } -> { tx = TESize n; loc }
  | { t = STComposed (name, l); loc } ->
      let l = List.map type_ l in
      { tx = TEComposed (name, l); loc }


let applyFunction (args_t : type_ list) (ret : type_) (args : PX.exp list) =
  let rec loop args_t args =
    match args_t, args with
    | [], _ :: _ -> failwith "missing arguments"
    | _ :: _, [] -> failwith "excess of arguments "
    | [], [] -> ret
    | h :: args_t, (ht : PX.exp) :: args ->
        unifyRaise ht.loc h ht.t ;
        loop args_t args
  in
  loop args_t args


let getInstanceName instance =
  match instance with
  | Some i -> "$" ^ i
  | None -> "$"


let addContextArg (env : EnvX.in_func) instance (f : EnvX.f) args loc =
  if EnvX.isFunctionActive f then
    let cpath = EnvX.getContext env in
    let fpath = EnvX.getFunctionContext f in
    let t = TX.path loc fpath in
    let instance = if Syntax.compare_path cpath fpath = 0 then context_name else instance in
    let e = PX.{ e = EId instance; loc; t } in
    e :: args
  else
    args


let rec exp (env : EnvX.in_func) (e : Syntax.exp) : EnvX.in_func * PX.exp =
  match e with
  | { e = SEUnit; loc } ->
      let t = TX.unit ~loc in
      env, { e = EUnit; t; loc }
  | { e = SEBool value; loc } ->
      let t = TX.bool ~loc in
      env, { e = EBool value; t; loc }
  | { e = SEInt value; loc } ->
      let t = TX.int ~loc in
      env, { e = EInt value; t; loc }
  | { e = SEReal value; loc } ->
      let t = TX.real ~loc in
      env, { e = EReal value; t; loc }
  | { e = SEString value; loc } ->
      let t = TX.string ~loc in
      env, { e = EString value; t; loc }
  | { e = SEGroup e } -> exp env e
  | { e = SEId name; loc } ->
      let var = EnvX.lookVar env name in
      let t = var.t in
      let e =
        match var.kind with
        | Val -> PX.{ e = EId name; t; loc }
        | Mem ->
            let ctx = EnvX.getContext env in
            let ctx_t = TX.path loc ctx in
            { e = EMember ({ e = EId context_name; t = ctx_t; loc }, name); t; loc }
      in
      env, e
  | { e = SEIndex { e; index }; loc } ->
      let env, e = exp env e in
      let env, index = exp env index in
      let t = TX.unbound Loc.default in
      unifyRaise e.loc (TX.array t) e.t ;
      unifyRaise index.loc (TX.int ~loc:Loc.default) index.t ;
      env, { e = EIndex { e; index }; t; loc }
  | { e = SEArray [] } -> failwith "empty array"
  | { e = SEArray (h :: t); loc } ->
      let env, h = exp env h in
      let env, t_rev, size =
        List.fold_left
          (fun (env, acc, size) e ->
            let env, e = exp env e in
            unifyRaise e.loc h.t e.t ;
            env, e :: acc, size + 1)
          (env, [], 1)
          t
      in
      let t = TX.array ~size:(TX.size ~loc size) h.t in
      env, { e = EArray (h :: List.rev t_rev); t; loc }
  | { e = SETuple l; loc } ->
      let env, l = exp_list env l in
      let t = TX.tuple ~loc (List.map (fun (e : PX.exp) -> e.t) l) in
      env, { e = ETuple l; t; loc }
  | { e = SEIf { cond; then_; else_ }; loc } ->
      let env, cond = exp env cond in
      let env, then_ = exp env then_ in
      let env, else_ = exp env else_ in
      let t = then_.t in
      unifyRaise cond.loc (TX.bool ~loc) cond.t ;
      unifyRaise else_.loc then_.t else_.t ;
      env, { e = EIf { cond; then_; else_ }; t; loc }
  | { e = SECall { instance; path; args }; loc } ->
      let env, args = exp_list env args in
      let f = EnvX.lookFunctionCall env path in
      let args_t, ret = f.t in
      let t = applyFunction args_t ret args in
      let instance = getInstanceName instance in
      let args = addContextArg env instance f args loc in
      env, { e = ECall { instance = None; path = f.path; args }; t; loc }
  | { e = SEOp (op, e1, e2); loc } ->
      let env, e1 = exp env e1 in
      let env, e2 = exp env e2 in
      let f = EnvX.lookOperator env op in
      let args_t, ret = f.t in
      let t = applyFunction args_t ret [ e1; e2 ] in
      env, { e = EOp (op, e1, e2); t; loc }
  | { e = SEUnOp (op, e); loc } ->
      let env, e = exp env e in
      let f = EnvX.lookOperator env ("u" ^ op) in
      let args_t, ret = f.t in
      let t = applyFunction args_t ret [ e ] in
      env, { e = EUnOp (op, e); t; loc }
  | { e = SEMember (e, m); loc } ->
      let env, e = exp env e in
      ( match e.t.tx with
      | TEId path ->
          let def = EnvX.lookType env path in
          begin
            match Map.find m def.members with
            | None -> failwith "member not found"
            | Some { t } -> env, { e = EMember (e, m); t; loc }
          end
      | _ -> failwith "invalid access to type" )


and exp_list (env : EnvX.in_func) (l : Syntax.exp list) : EnvX.in_func * PX.exp list =
  let env, rev_l =
    List.fold_left
      (fun (env, acc) e ->
        let env, e = exp env e in
        env, e :: acc)
      (env, [])
      l
  in
  env, List.rev rev_l


and lexp (env : EnvX.in_func) (e : Syntax.lexp) : EnvX.in_func * PX.lexp =
  match e with
  | { l = SLWild; loc } ->
      let t = TX.noreturn loc in
      env, { l = LWild; t; loc }
  | { l = SLId name; loc } ->
      let var = EnvX.lookVar env name in
      let t = var.t in
      let e =
        match var.kind with
        | Val -> PX.{ l = LId name; t; loc }
        | Mem ->
            let ctx = EnvX.getContext env in
            let ctx_t = TX.path loc ctx in
            { l = LMember ({ l = LId context_name; t = ctx_t; loc }, name); t; loc }
      in
      env, e
  | { l = SLGroup e } -> lexp env e
  | { l = SLTuple elems; loc } ->
      let env, elems =
        List.fold_left
          (fun (env, acc) e ->
            let env, e = lexp env e in
            env, e :: acc)
          (env, [])
          (List.rev elems)
      in
      let t_elems = List.map (fun (e : PX.lexp) -> e.t) elems in
      let t = TX.tuple ~loc t_elems in
      env, { l = LTuple elems; t; loc }
  | { l = SLIndex { e; index }; loc } ->
      let env, e = lexp env e in
      let env, index = exp env index in
      let t = TX.unbound loc in
      unifyRaise index.loc (TX.int ~loc) index.t ;
      unifyRaise e.loc (TX.array ~loc t) e.t ;
      env, { l = LIndex { e; index }; t; loc }
  | { l = SLMember (e, m); loc } ->
      let env, e = lexp env e in
      ( match e.t.tx with
      | TEId path ->
          let def = EnvX.lookType env path in
          begin
            match Map.find m def.members with
            | None -> failwith "member not found"
            | Some { t } -> env, { l = LMember (e, m); t; loc }
          end
      | _ -> failwith "invalid access to type" )


and dexp (env : EnvX.in_func) (e : Syntax.dexp) (kind : var_kind) : EnvX.in_func * PX.dexp =
  match e with
  | { d = SDWild; loc } ->
      let t = TX.noreturn loc in
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
      let t = TX.tuple ~loc (List.map (fun (e : PX.dexp) -> e.t) l) in
      env, { d = DTuple l; t; loc }
  | { d = SDGroup e } -> dexp env e kind
  | { d = SDTyped (e, t); loc } ->
      let env, e = dexp env e kind in
      let t = type_ t in
      unifyRaise e.loc t e.t ;
      env, e
  | { d = SDId (name, dims); loc } ->
      let t =
        match dims with
        | Some size -> TX.array ~loc ~size:(TX.size ~loc size) (TX.unbound loc)
        | None -> TX.unbound loc
      in
      let env = EnvX.addVar env name t kind loc in
      env, { d = DId (name, dims); t; loc }


let rec stmt (env : EnvX.in_func) (return : type_) (s : Syntax.stmt) : EnvX.in_func * PX.stmt =
  match s with
  | { s = SStmtError } -> failwith "There was an error parsing "
  | { s = SStmtBlock stmts; loc } ->
      let env = EnvX.pushScope env in
      let env, stmts = stmt_list env return stmts in
      let env = EnvX.popScope env in
      env, { s = StmtBlock stmts; loc }
  | { s = SStmtVal (lhs, None); loc } ->
      let env, lhs = dexp env lhs Val in
      env, { s = StmtVal (lhs, None); loc }
  | { s = SStmtVal (lhs, Some rhs); loc } ->
      let env, lhs = dexp env lhs Val in
      let env, rhs = exp env rhs in
      unifyRaise rhs.loc lhs.t rhs.t ;
      env, { s = StmtVal (lhs, Some rhs); loc }
  | { s = SStmtMem (lhs, None, tag); loc } ->
      let env, lhs = dexp env lhs Mem in
      env, { s = StmtMem (lhs, None, tag); loc }
  | { s = SStmtMem (lhs, Some rhs, tag); loc } ->
      let env, lhs = dexp env lhs Mem in
      let env, rhs = exp env rhs in
      unifyRaise rhs.loc lhs.t rhs.t ;
      env, { s = StmtMem (lhs, Some rhs, tag); loc }
  | { s = SStmtBind (lhs, rhs); loc } ->
      let env, lhs = lexp env lhs in
      let env, rhs = exp env rhs in
      unifyRaise rhs.loc lhs.t rhs.t ;
      env, { s = StmtBind (lhs, rhs); loc }
  | { s = SStmtReturn e; loc } ->
      let env, e = exp env e in
      unifyRaise e.loc return e.t ;
      env, { s = StmtReturn e; loc }
  | { s = SStmtIf (cond, then_, else_); loc } ->
      let env, cond = exp env cond in
      unifyRaise cond.loc (TX.bool ~loc) cond.t ;
      let env, then_ = stmt env return then_ in
      let env, else_ = stmt_opt env return else_ in
      env, { s = StmtIf (cond, then_, else_); loc }
  | { s = SStmtWhile (cond, s); loc } ->
      let env, cond = exp env cond in
      unifyRaise cond.loc (TX.bool ~loc) cond.t ;
      let env, s = stmt env return s in
      env, { s = StmtWhile (cond, s); loc }


and stmt_opt env return s =
  match s with
  | None -> env, None
  | Some s ->
      let env, s = stmt env return s in
      env, Some s


and stmt_list env return l =
  let env, l_rev =
    List.fold_left
      (fun (env, acc) s ->
        let env, s = stmt env return s in
        env, s :: acc)
      (env, [])
      l
  in
  env, List.rev l_rev


let addGeneratedFunctions tags name next =
  if Tags.has tags "wave" || Tags.has tags "wavetable" then
    let code = Pla.print {pla|fun <#name#s>_samples() : int|pla} in
    let def = Parser.parseFunctionSpec code in
    Some ({ def with next }, Syntax.{ s = SStmtBlock []; loc = Loc.default })
  else
    next


let getOptType loc (t : Syntax.type_ option) =
  match t with
  | None -> TX.unbound loc
  | Some t -> type_ t


let getReturnType loc (t : Syntax.type_ option) =
  match t with
  | None -> TX.noreturn loc
  | Some t -> type_ t


let convertArguments (args : Syntax.arg list) : PX.arg list =
  List.map (fun (name, t, loc) -> name, getOptType loc t, loc) args


let rec function_def (env : EnvX.in_context) ((def : Syntax.function_def), (body : Syntax.stmt)) :
    EnvX.in_context * (PX.function_def * PX.stmt) =
  let ret = getReturnType def.loc def.t in
  let args = convertArguments def.args in
  let env, path, t = EnvX.enterFunction env def.name args ret def.loc in
  let env, body = stmt env ret body in
  let env = EnvX.exitFunction env in
  let next = addGeneratedFunctions def.tags def.name def.next in
  let env, next = function_def_opt env next in
  env, ({ name = path; args; t; loc = def.loc; tags = def.tags; next }, body)


and function_def_opt (env : EnvX.in_context) def_opt =
  match def_opt with
  | None -> env, None
  | Some def_body ->
      let env, def_body = function_def env def_body in
      env, Some def_body


let rec ext_function (env : EnvX.in_context) ((def : Syntax.function_def), (link_name : string option)) :
    EnvX.in_context * (PX.function_def * string) =
  let ret = getOptType def.loc def.t in
  let args = convertArguments def.args in
  let env, path, t = EnvX.enterFunction env def.name args ret def.loc in
  let env = EnvX.exitFunction env in
  let link_name = CCOpt.get_or ~default:def.name link_name in
  let next = addGeneratedFunctions def.tags def.name def.next in
  let env, next = function_def_opt env next in
  env, ({ name = path; args; t; loc = def.loc; tags = def.tags; next = None }, link_name)


let rec top_stmt (env : EnvX.in_module) (s : Syntax.top_stmt) : EnvX.in_module * PX.top_stmt =
  match s with
  | { top = STopError } -> failwith "Parser error"
  | { top = STopFunction (def, body) } ->
      let env = EnvX.createContextForFunction env def.name def.loc in
      let env, (def, body) = function_def env (def, body) in
      let env = EnvX.exitContext env in
      env, { top = TopFunction (def, body); loc = def.loc }
  | { top = STopExternal (def, link_name) } ->
      let env = EnvX.createContextForExternal env def.name def.loc in
      let env, (def, link_name) = ext_function env (def, link_name) in
      let env = EnvX.exitContext env in
      env, { top = TopExternal (def, link_name); loc = def.loc }
  | { top = STopType { name; members }; loc } ->
      let members = List.map (fun (name, t, loc) -> name, type_ t, loc) members in
      env, { top = TopType { name; members }; loc }


and top_stmt_list (env : EnvX.in_module) (s : Syntax.top_stmt list) : EnvX.in_module * PX.top_stmt list =
  let env, rev_s =
    List.fold_left
      (fun (env, acc) s ->
        let env, s = top_stmt env s in
        env, s :: acc)
      (env, [])
      s
  in
  env, rev_s


let rec infer (parsed : Parser.parsed_file list) : 'a * Typed.PX.program =
  let env, stmts =
    List.fold_left
      (fun (env, acc) (h : Parser.parsed_file) ->
        let env = EnvX.enterModule env h.name in
        let env, stmt = top_stmt_list env h.stmts in
        let env = EnvX.exitModule env in
        env, acc @ stmt)
      (EnvX.empty (), [])
      parsed
  in
  env, List.rev stmts
