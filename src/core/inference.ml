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
open Util
open Parser
open Env
open Typed

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
          linkType ~from:tt ~into:original
        else
          loop t
  in
  loop l


and unify (t1 : type_) (t2 : type_) =
  if t1 == t2 then
    true
  else
    match t1.tx, t2.tx with
    | TEId t1, TEId t2 -> Parser.Syntax.compare_path t1 t2 = 0
    | TESize t1, TESize t2 -> t1 = t2
    | TEComposed (n1, e1), TEComposed (n2, e2) when n1 = n2 && List.length e1 = List.length e2 ->
        List.for_all2 unify e1 e2
    (* follow the links *)
    | TELink tlink, _ -> unify tlink t2
    | _, TELink tlink -> unify t1 tlink
    | TENoReturn, _ -> linkType ~from:t2 ~into:t1
    | _, TENoReturn -> linkType ~from:t1 ~into:t2
    (* replace any unbound *)
    | TEUnbound _, _ -> linkType ~from:t2 ~into:t1
    | _, TEUnbound _ -> linkType ~from:t1 ~into:t2
    (* types with alternatives *)
    | TEOption l1, TEOption l2 ->
        let t3 = constrainOption l1 l2 in
        let _ = linkType ~from:t3 ~into:t2 in
        linkType ~from:t3 ~into:t1
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


let applyFunction (args_t : type_ list) (ret : type_) (args : exp list) =
  let rec loop args_t args =
    match args_t, args with
    | [], _ :: _ -> failwith "missing arguments"
    | _ :: _, [] -> failwith "excess of arguments "
    | [], [] -> ret
    | h :: args_t, (ht : exp) :: args ->
        unifyRaise ht.loc h ht.t ;
        loop args_t args
  in
  loop args_t args


let addContextArg (env : Env.in_func) instance (f : Env.f) args loc =
  if Env.isFunctionActive f then
    let cpath = Env.getContext env in
    let fpath = Env.getFunctionContext f in
    if Syntax.compare_path cpath fpath = 0 then
      let t = C.path loc fpath in
      let e = { e = EId context_name; t; loc } in
      env, e :: args
    else
      let instance =
        let number = Printf.sprintf "%.2x%.2x" (0xFF land Hashtbl.hash fpath) (0xFF land Hashtbl.hash cpath) in
        match instance with
        | Some i -> i ^ "_" ^ number
        | None ->
            let n = Env.getFunctionTick env in
            "inst_" ^ string_of_int n ^ number
      in
      let t = C.path loc fpath in
      let ctx_t = C.path loc cpath in
      let env = Env.addVar env unify instance t Inst loc in
      let e = { e = EMember ({ e = EId context_name; t = ctx_t; loc }, instance); loc; t } in
      env, e :: args
  else
    env, args


let rec exp (env : Env.in_func) (e : Syntax.exp) : Env.in_func * exp =
  match e with
  | { e = SEUnit; loc } ->
      let t = C.unit ~loc in
      env, { e = EUnit; t; loc }
  | { e = SEBool value; loc } ->
      let t = C.bool ~loc in
      env, { e = EBool value; t; loc }
  | { e = SEInt value; loc } ->
      let t = C.int ~loc in
      env, { e = EInt value; t; loc }
  | { e = SEReal value; loc } ->
      let t = C.real ~loc in
      env, { e = EReal value; t; loc }
  | { e = SEString value; loc } ->
      let t = C.string ~loc in
      env, { e = EString value; t; loc }
  | { e = SEGroup e; _ } -> exp env e
  | { e = SEId name; loc } ->
      let var = Env.lookVar env name in
      let t = var.t in
      let e =
        match var.kind with
        | Val -> { e = EId name; t; loc }
        | Mem
         |Inst ->
            let ctx = Env.getContext env in
            let ctx_t = C.path loc ctx in
            { e = EMember ({ e = EId context_name; t = ctx_t; loc }, name); t; loc }
      in
      env, e
  | { e = SEIndex { e; index }; loc } ->
      let env, e = exp env e in
      let env, index = exp env index in
      let t = C.unbound Loc.default in
      unifyRaise e.loc (C.array t) e.t ;
      unifyRaise index.loc (C.int ~loc:Loc.default) index.t ;
      env, { e = EIndex { e; index }; t; loc }
  | { e = SEArray []; _ } -> failwith "empty array"
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
      unifyRaise cond.loc (C.bool ~loc) cond.t ;
      unifyRaise else_.loc then_.t else_.t ;
      env, { e = EIf { cond; then_; else_ }; t; loc }
  | { e = SECall { instance; path; args }; loc } ->
      let env, args = exp_list env args in
      let f = Env.lookFunctionCall env path in
      let args_t, ret = f.t in
      let t = applyFunction args_t ret args in
      let env, args = addContextArg env instance f args loc in
      env, { e = ECall { instance = None; path = f.path; args }; t; loc }
  | { e = SEOp (op, e1, e2); loc } ->
      let env, e1 = exp env e1 in
      let env, e2 = exp env e2 in
      let f = Env.lookOperator env op in
      let args_t, ret = f.t in
      let t = applyFunction args_t ret [ e1; e2 ] in
      env, { e = EOp (op, e1, e2); t; loc }
  | { e = SEUnOp (op, e); loc } ->
      let env, e = exp env e in
      let f = Env.lookOperator env ("u" ^ op) in
      let args_t, ret = f.t in
      let t = applyFunction args_t ret [ e ] in
      env, { e = EUnOp (op, e); t; loc }
  | { e = SEMember (e, m); loc } ->
      let env, e = exp env e in
      ( match e.t.tx with
      | TEId path ->
          let def = Env.lookType env path in
          begin
            match Map.find m def.members with
            | None -> failwith "member not found"
            | Some { t; _ } -> env, { e = EMember (e, m); t; loc }
          end
      | _ -> failwith "invalid access to type" )


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
      let var = Env.lookVar env name in
      let t = var.t in
      let e =
        match var.kind with
        | Val -> { l = LId name; t; loc }
        | Mem
         |Inst ->
            let ctx = Env.getContext env in
            let ctx_t = C.path loc ctx in
            { l = LMember ({ l = LId context_name; t = ctx_t; loc }, name); t; loc }
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
      unifyRaise index.loc (C.int ~loc) index.t ;
      unifyRaise e.loc (C.array ~loc t) e.t ;
      env, { l = LIndex { e; index }; t; loc }
  | { l = SLMember (e, m); loc } ->
      let env, e = lexp env e in
      ( match e.t.tx with
      | TEId path ->
          let def = Env.lookType env path in
          begin
            match Map.find m def.members with
            | None -> failwith "member not found"
            | Some { t; _ } -> env, { l = LMember (e, m); t; loc }
          end
      | _ -> failwith "invalid access to type" )


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
      let t = type_ t in
      unifyRaise e.loc t e.t ;
      env, e
  | { d = SDId (name, dims); loc } ->
      let t =
        match dims with
        | Some size -> C.array ~loc ~size:(C.size ~loc size) (C.unbound loc)
        | None -> C.unbound loc
      in
      let env = Env.addVar env unify name t kind loc in
      env, { d = DId (name, dims); t; loc }


let rec stmt (env : Env.in_func) (return : type_) (s : Syntax.stmt) : Env.in_func * stmt =
  match s with
  | { s = SStmtError; _ } -> failwith "There was an error parsing "
  | { s = SStmtBlock stmts; loc } ->
      let env = Env.pushScope env in
      let env, stmts = stmt_list env return stmts in
      let env = Env.popScope env in
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
      unifyRaise cond.loc (C.bool ~loc) cond.t ;
      let env, then_ = stmt env return then_ in
      let env, else_ = stmt_opt env return else_ in
      env, { s = StmtIf (cond, then_, else_); loc }
  | { s = SStmtWhile (cond, s); loc } ->
      let env, cond = exp env cond in
      unifyRaise cond.loc (C.bool ~loc) cond.t ;
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
    let def = Parse.parseFunctionSpec code in
    Some ({ def with next }, Syntax.{ s = SStmtBlock []; loc = Loc.default })
  else
    next


let getOptType loc (t : Syntax.type_ option) =
  match t with
  | None -> C.unbound loc
  | Some t -> type_ t


let getReturnType loc (t : Syntax.type_ option) =
  match t with
  | None -> C.noreturn loc
  | Some t -> type_ t


let convertArguments (args : Syntax.arg list) : arg list =
  List.map (fun (name, t, loc) -> name, getOptType loc t, loc) args


let rec function_def (env : Env.in_context) ((def : Syntax.function_def), (body : Syntax.stmt)) :
    Env.in_context * (function_def * stmt) =
  let ret = getReturnType def.loc def.t in
  let args = convertArguments def.args in
  let env, path, t = Env.enterFunction env def.name args ret def.loc in
  let env, body = stmt env ret body in
  let env = Env.exitFunction env in
  let next = addGeneratedFunctions def.tags def.name def.next in
  let env, next = function_def_opt env next in
  env, ({ name = path; args; t; loc = def.loc; tags = def.tags; next }, body)


and function_def_opt (env : Env.in_context) def_opt =
  match def_opt with
  | None -> env, None
  | Some def_body ->
      let env, def_body = function_def env def_body in
      env, Some def_body


let ext_function (env : Env.in_context) ((def : Syntax.function_def), (link_name : string option)) :
    Env.in_context * (function_def * string) =
  let ret = getOptType def.loc def.t in
  let args = convertArguments def.args in
  let env, path, t = Env.enterFunction env def.name args ret def.loc in
  let env = Env.exitFunction env in
  let link_name = CCOpt.get_or ~default:def.name link_name in
  let next = addGeneratedFunctions def.tags def.name def.next in
  let env, next = function_def_opt env next in
  env, ({ name = path; args; t; loc = def.loc; tags = def.tags; next }, link_name)


let getContextArgument (context : Env.context) loc : arg option =
  match context with
  | Some (p, c) ->
      if Map.is_empty c.members then
        None
      else
        let ctx_t = C.path loc p in
        Some (context_name, ctx_t, loc)
  | None -> None


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


let rec top_stmt (env : Env.in_module) (s : Syntax.top_stmt) : Env.in_module * top_stmt =
  match s with
  | { top = STopError; _ } -> failwith "Parser error"
  | { top = STopFunction (def, body); _ } ->
      let env = Env.createContextForFunction env def.name def.loc in
      let env, (def, body) = function_def env (def, body) in
      let def = insertContextArgument env def in
      let env = Env.exitContext env in
      env, { top = TopFunction (def, body); loc = def.loc }
  | { top = STopExternal (def, link_name); _ } ->
      let env = Env.createContextForExternal env in
      let env, (def, link_name) = ext_function env (def, link_name) in
      let env = Env.exitContext env in
      env, { top = TopExternal (def, link_name); loc = def.loc }
  | { top = STopType { name; members }; loc } ->
      let members = List.map (fun (name, t, loc) -> name, type_ t, loc) members in
      let path = Env.getPath env.m name loc in
      env, { top = TopType { path; members }; loc }


and top_stmt_list (env : Env.in_module) (s : Syntax.top_stmt list) : Env.in_module * top_stmt list =
  let env, rev_s =
    List.fold_left
      (fun (env, acc) s ->
        let env, s = top_stmt env s in
        env, s :: acc)
      (env, [])
      s
  in
  env, rev_s


let getTypesFromModule m =
  Map.fold
    (fun _ t s ->
      if Map.is_empty t.Env.members then
        s
      else
        t :: s)
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
  let types = List.sort (fun (a : Env.t) b -> compare a.index b.index) types in
  List.map
    (fun (t : Env.t) ->
      let members = Map.fold (fun _ (var : Env.var) s -> (var.name, var.t, var.loc) :: s) [] t.members in
      { top = TopType { path = t.path; members }; loc = t.loc })
    types


let infer_single (env : Env.in_top) (h : Parse.parsed_file) : Env.in_top * Typed.program =
  let env = Env.enterModule env h.name in
  let env, stmt = top_stmt_list env h.stmts in
  let env = Env.exitModule env in
  env, stmt


let infer (parsed : Parse.parsed_file list) : Env.in_top * Typed.program =
  let env, stmts =
    List.fold_left
      (fun (env, acc) (h : Parse.parsed_file) ->
        let env = Env.enterModule env h.name in
        let env, stmt = top_stmt_list env h.stmts in
        let env = Env.exitModule env in
        env, stmt @ acc)
      (Env.empty (), [])
      parsed
  in
  let types = createTypes env in
  env, types @ List.rev stmts
