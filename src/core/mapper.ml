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

open Prog

type 'data state =
  { data : 'data
  ; pre_stmts : stmt list
  }

type ('env, 'data, 'kind) mapper_func = ('env -> 'data state -> 'kind -> 'data state * 'kind) option
type ('env, 'data, 'kind) expand_func = ('env -> 'data state -> 'kind -> 'data state * 'kind list) option
type ('env, 'kind) env_func = ('env -> 'kind -> 'env) option

let defaultState data = { data; pre_stmts = [] }
let getData (state : 'data state) = state.data
let setData (state : 'data state) data = { state with data }
let pushStmts (state : 'data state) stmts = { state with pre_stmts = state.pre_stmts @ stmts }
let getStmts (state : 'data state) = { state with pre_stmts = [] }, state.pre_stmts

let apply (mapper : ('env, 'data, 'kind) mapper_func) (env : 'env) (data : 'data state) (kind : 'kind)
  : 'data state * 'kind
  =
  match mapper with
  | Some f -> f env data kind
  | None -> data, kind


let applyExpander (mapper : ('env, 'data, 'kind) expand_func) (env : 'env) (data : 'data state) (kind : 'kind)
  : 'data state * 'kind list
  =
  match mapper with
  | Some f -> f env data kind
  | None -> data, [ kind ]


let applyExpanderList (mapper : ('env, 'datas, 'kind) expand_func) (env : 'env) (data : 'data) (kind_list : 'kind list)
  : 'data * 'kind list
  =
  let state', rev_exp_list =
    List.fold_left
      (fun (s, acc) k ->
        let s', kl = applyExpander mapper env s k in
        s', kl :: acc)
      (data, [])
      kind_list
  in
  state', rev_exp_list |> List.rev |> List.flatten


let enter (env_func : ('env, 'kind) env_func) (env : 'env) (kind : 'kind) : 'env =
  match env_func with
  | None -> env
  | Some f -> f env kind


let make (mapper : 'env -> 'data state -> 'kind -> 'data state * 'kind) : ('env, 'data, 'kind) mapper_func = Some mapper

let makeExpander (mapper : 'env -> 'data state -> 'kind -> 'data state * 'kind list) : ('env, 'data, 'kind) expand_func =
  Some mapper


let makeEnv (f : 'env -> 'kind -> 'env) : ('env, 'kind) env_func = Some f

let list mapper_app mapper env state el =
  let state', rev_el =
    List.fold_left
      (fun (s, acc) e ->
        let s', e' = mapper_app mapper env s e in
        s', e' :: acc)
      (state, [])
      el
  in
  state', List.rev rev_el


type ('env, 'data) mapper =
  { type_ : ('env, 'data, type_) mapper_func
  ; exp : ('env, 'data, exp) mapper_func
  ; lexp : ('env, 'data, lexp) mapper_func
  ; dexp : ('env, 'data, dexp) mapper_func
  ; param : ('env, 'data, param) mapper_func
  ; member : ('env, 'data, member) mapper_func
  ; struct_descr : ('env, 'data, struct_descr) mapper_func
  ; stmt : ('env, 'data, stmt) expand_func
  ; function_def : ('env, 'data, function_def) mapper_func
  ; top_stmt : ('env, 'data, top_stmt) expand_func
  ; type__env : ('env, type_) env_func
  ; exp_env : ('env, exp) env_func
  ; lexp_env : ('env, lexp) env_func
  ; dexp_env : ('env, dexp) env_func
  ; param_env : ('env, param) env_func
  ; member_env : ('env, member) env_func
  ; struct_descr_env : ('env, struct_descr) env_func
  ; stmt_env : ('env, stmt) env_func
  ; function_def_env : ('env, function_def) env_func
  ; top_stmt_env : ('env, top_stmt) env_func
  }

let identity =
  { type_ = None
  ; exp = None
  ; lexp = None
  ; dexp = None
  ; param = None
  ; member = None
  ; struct_descr = None
  ; stmt = None
  ; function_def = None
  ; top_stmt = None
  ; type__env = None
  ; exp_env = None
  ; lexp_env = None
  ; dexp_env = None
  ; param_env = None
  ; member_env = None
  ; struct_descr_env = None
  ; stmt_env = None
  ; function_def_env = None
  ; top_stmt_env = None
  }


let seqMapperFunc a b =
  if a = None then
    b
  else if b = None then
    a
  else (
    let c env state exp =
      let state, exp = apply a env state exp in
      apply b env state exp
    in
    Some c)


let seqEnvFunc (a : ('env, 'kind) env_func) (b : ('env, 'kind) env_func) : ('env, 'kind) env_func =
  if a = None then
    b
  else if b = None then
    a
  else (
    let mapper3 env exp =
      let env = enter a env exp in
      enter b env exp
    in
    Some mapper3)


let seqExpandFunc a b =
  if a = None then
    b
  else if b = None then
    a
  else (
    let c env state exp =
      let state, exp_list = applyExpander a env state exp in
      let state, exp_list = applyExpanderList b env state exp_list in
      state, exp_list
    in
    Some c)


(** Merges two mappers *)
let seq b a =
  { type_ = seqMapperFunc a.type_ b.type_
  ; exp = seqMapperFunc a.exp b.exp
  ; lexp = seqMapperFunc a.lexp b.lexp
  ; dexp = seqMapperFunc a.dexp b.dexp
  ; param = seqMapperFunc a.param b.param
  ; member = seqMapperFunc a.member b.member
  ; struct_descr = seqMapperFunc a.struct_descr b.struct_descr
  ; stmt = seqExpandFunc a.stmt b.stmt
  ; function_def = seqMapperFunc a.function_def b.function_def
  ; top_stmt = seqExpandFunc a.top_stmt b.top_stmt
  ; type__env = seqEnvFunc a.type__env b.type__env
  ; exp_env = seqEnvFunc a.exp_env b.exp_env
  ; lexp_env = seqEnvFunc a.lexp_env b.lexp_env
  ; dexp_env = seqEnvFunc a.dexp_env b.dexp_env
  ; param_env = seqEnvFunc a.param_env b.param_env
  ; member_env = seqEnvFunc a.member_env b.member_env
  ; struct_descr_env = seqEnvFunc a.struct_descr_env b.struct_descr_env
  ; stmt_env = seqEnvFunc a.stmt_env b.stmt_env
  ; function_def_env = seqEnvFunc a.function_def_env b.function_def_env
  ; top_stmt_env = seqEnvFunc a.top_stmt_env b.top_stmt_env
  }


let rec type_ (mapper : ('env, 'data) mapper) (env : 'env) (state : 'data state) (t : type_) : 'data state * type_ =
  let sub_env = enter mapper.type__env env t in
  let loc = t.loc in
  match t with
  | { t = TVoid None; _ } -> apply mapper.type_ env state t
  | { t = TVoid (Some elems); _ } ->
    let state, elems = (list type_) mapper sub_env state elems in
    apply mapper.type_ env state { t = TVoid (Some elems); loc }
  | { t = TInt; _ } -> apply mapper.type_ env state t
  | { t = TReal; _ } -> apply mapper.type_ env state t
  | { t = TString; _ } -> apply mapper.type_ env state t
  | { t = TBool; _ } -> apply mapper.type_ env state t
  | { t = TFix16; _ } -> apply mapper.type_ env state t
  | { t = TArray (dim, t1); _ } ->
    let state, t1 = type_ mapper sub_env state t1 in
    apply mapper.type_ env state { t = TArray (dim, t1); loc }
  | { t = TTuple elems; _ } ->
    let state, elems = (list type_) mapper sub_env state elems in
    apply mapper.type_ env state { t = TTuple elems; loc }
  | { t = TStruct s; _ } ->
    let state, s = struct_descr mapper sub_env state s in
    apply mapper.type_ env state { t = TStruct s; loc }


and struct_descr (mapper : ('env, 'data) mapper) (env : 'env) (state : 'data state) (s : struct_descr)
  : 'data state * struct_descr
  =
  let sub_env = enter mapper.struct_descr_env env s in
  match s with
  | { path; members } ->
    let state, members = (list member) mapper sub_env state members in
    apply mapper.struct_descr env state { path; members }


and param (mapper : ('env, 'data) mapper) (env : 'env) (state : 'data state) (p : param) : 'data state * param =
  let name, t, loc = p in
  let sub_env = enter mapper.param_env env p in
  let state, t = type_ mapper sub_env state t in
  apply mapper.param env state (name, t, loc)


and member (mapper : ('env, 'data) mapper) (env : 'env) (state : 'data state) (p : member) : 'data state * member =
  let name, t, tags, loc = p in
  let sub_env = enter mapper.member_env env p in
  let state, t = type_ mapper sub_env state t in
  apply mapper.member env state (name, t, tags, loc)


let rec exp (mapper : ('env, 'data) mapper) (env : 'env) (state : 'data state) (e : exp) : 'data state * exp =
  let loc = e.loc in
  let sub_env = enter mapper.exp_env env e in
  let state, t = type_ mapper sub_env state e.t in
  match e with
  | { e = EUnit; _ } -> apply mapper.exp env state { e = EUnit; t; loc }
  | { e = EBool b; _ } -> apply mapper.exp env state { e = EBool b; t; loc }
  | { e = EInt n; _ } -> apply mapper.exp env state { e = EInt n; t; loc }
  | { e = EReal n; _ } -> apply mapper.exp env state { e = EReal n; t; loc }
  | { e = EFixed n; _ } -> apply mapper.exp env state { e = EFixed n; t; loc }
  | { e = EString n; _ } -> apply mapper.exp env state { e = EString n; t; loc }
  | { e = EId n; _ } -> apply mapper.exp env state { e = EId n; t; loc }
  | { e = EIndex { e; index }; _ } ->
    let state, e = exp mapper sub_env state e in
    let state, index = exp mapper sub_env state index in
    apply mapper.exp env state { e = EIndex { e; index }; t; loc }
  | { e = EArray elems; _ } ->
    let state, elems = list exp mapper sub_env state elems in
    apply mapper.exp env state { e = EArray elems; t; loc }
  | { e = ECall { path; args }; _ } ->
    let state, args = list exp mapper sub_env state args in
    apply mapper.exp env state { e = ECall { path; args }; t; loc }
  | { e = EUnOp (op, e1); _ } ->
    let state, e1 = exp mapper sub_env state e1 in
    apply mapper.exp env state { e = EUnOp (op, e1); t; loc }
  | { e = EOp (op, e1, e2); _ } ->
    let state, e1 = exp mapper sub_env state e1 in
    let state, e2 = exp mapper sub_env state e2 in
    apply mapper.exp env state { e = EOp (op, e1, e2); t; loc }
  | { e = EIf { cond; then_; else_ }; _ } ->
    let state, cond = exp mapper sub_env state cond in
    let state, then_ = exp mapper sub_env state then_ in
    let state, else_ = exp mapper sub_env state else_ in
    apply mapper.exp env state { e = EIf { cond; then_; else_ }; t; loc }
  | { e = ETuple elems; _ } ->
    let state, elems = list exp mapper sub_env state elems in
    apply mapper.exp env state { e = ETuple elems; t; loc }
  | { e = EMember (e1, n); _ } ->
    let state, e1 = exp mapper sub_env state e1 in
    apply mapper.exp env state { e = EMember (e1, n); t; loc }
  | { e = ETMember (e1, n); _ } ->
    let state, e1 = exp mapper sub_env state e1 in
    apply mapper.exp env state { e = ETMember (e1, n); t; loc }


let rec lexp (mapper : ('env, 'data) mapper) (env : 'env) (state : 'data state) (e : lexp) : 'data state * lexp =
  let loc = e.loc in
  let sub_env = enter mapper.lexp_env env e in
  let state, t = type_ mapper env state e.t in
  match e with
  | { l = LWild; _ } -> apply mapper.lexp env state { l = LWild; t; loc }
  | { l = LId n; _ } -> apply mapper.lexp env state { l = LId n; t; loc }
  | { l = LMember (e1, n); _ } ->
    let state, e1 = lexp mapper sub_env state e1 in
    apply mapper.lexp env state { l = LMember (e1, n); t; loc }
  | { l = LIndex { e; index }; _ } ->
    let state, e = lexp mapper sub_env state e in
    let state, index = exp mapper sub_env state index in
    apply mapper.lexp env state { l = LIndex { e; index }; t; loc }
  | { l = LTuple elems; _ } ->
    let state, elems = list lexp mapper sub_env state elems in
    apply mapper.lexp env state { l = LTuple elems; t; loc }


let rec dexp (mapper : ('env, 'data) mapper) (env : 'env) (state : 'data state) (e : dexp) : 'data state * dexp =
  let loc = e.loc in
  let sub_env = enter mapper.dexp_env env e in
  let state, t = type_ mapper env state e.t in
  match e with
  | { d = DWild; _ } -> apply mapper.dexp env state { d = DWild; t; loc }
  | { d = DId (n, dim); _ } -> apply mapper.dexp env state { d = DId (n, dim); t; loc }
  | { d = DTuple elems; _ } ->
    let state, elems = list dexp mapper sub_env state elems in
    apply mapper.dexp env state { d = DTuple elems; t; loc }


let block stmts loc =
  match stmts with
  | [] -> { s = StmtBlock []; loc }
  | [ stmt ] -> stmt
  | _ -> { s = StmtBlock stmts; loc }


let applyStmtExpander mapper env state pre s =
  let s = block (pre @ [ s ]) s.loc in
  applyExpander mapper env state s


let rec stmt (mapper : ('env, 'data) mapper) (env : 'env) (state : 'data state) (s : stmt) : 'data state * stmt list =
  let loc = s.loc in
  let sub_env = enter mapper.stmt_env env s in
  match s with
  | { s = StmtDecl d; _ } ->
    let state, d = dexp mapper sub_env state d in
    let state, pre = getStmts state in
    applyStmtExpander mapper.stmt env state pre { s = StmtDecl d; loc }
  | { s = StmtBind (lhs, rhs); _ } ->
    let state, lhs = lexp mapper sub_env state lhs in
    let state, rhs = exp mapper sub_env state rhs in
    let state, pre = getStmts state in
    applyStmtExpander mapper.stmt env state pre { s = StmtBind (lhs, rhs); loc }
  | { s = StmtReturn e; _ } ->
    let state, e = exp mapper sub_env state e in
    let state, pre = getStmts state in
    applyStmtExpander mapper.stmt env state pre { s = StmtReturn e; loc }
  | { s = StmtWhile (cond, body); _ } ->
    let state, cond = exp mapper sub_env state cond in
    let state, pre = getStmts state in
    let state, body_l = stmt mapper sub_env state body in
    applyStmtExpander mapper.stmt env state pre { s = StmtWhile (cond, block body_l body.loc); loc }
  | { s = StmtIf (cond, then_, None); _ } ->
    let state, cond = exp mapper sub_env state cond in
    let state, pre = getStmts state in
    let state, then_l = stmt mapper sub_env state then_ in
    let then_ = block then_l then_.loc in
    applyStmtExpander mapper.stmt env state pre { s = StmtIf (cond, then_, None); loc }
  | { s = StmtIf (cond, then_, Some else_); _ } ->
    let state, cond = exp mapper sub_env state cond in
    let state, pre = getStmts state in
    let state, then_l = stmt mapper sub_env state then_ in
    let state, else_l = stmt mapper sub_env state else_ in
    let then_ = block then_l then_.loc in
    let else_ = block else_l else_.loc in
    applyStmtExpander mapper.stmt env state pre { s = StmtIf (cond, then_, Some else_); loc }
  | { s = StmtBlock stmts; _ } ->
    let state, stmts = list stmt mapper sub_env state stmts in
    applyExpander mapper.stmt env state { s = StmtBlock (List.flatten stmts); loc }


let function_def (mapper : ('env, 'data) mapper) (env : 'env) (state : 'data state) (f : function_def)
  : 'data state * function_def
  =
  let sub_env = enter mapper.function_def_env env f in
  match f with
  | { name; args; t = t_args, ret; loc; tags; info } ->
    let state, args = list param mapper sub_env state args in
    let state, t_args = list type_ mapper sub_env state t_args in
    let state, ret = type_ mapper sub_env state ret in
    apply mapper.function_def env state { name; args; t = t_args, ret; loc; tags; info }


let top_stmt (mapper : ('env, 'data) mapper) (env : 'env) (state : 'data state) (s : top_stmt)
  : 'data state * top_stmt list
  =
  let loc = s.loc in
  let sub_env = enter mapper.top_stmt_env env s in
  match s with
  | { top = TopExternal (f, link_name); _ } ->
    let state, f = function_def mapper sub_env state f in
    applyExpander mapper.top_stmt env state { top = TopExternal (f, link_name); loc }
  | { top = TopFunction (f, body); _ } ->
    let state, f = function_def mapper sub_env state f in
    let state, body = stmt mapper sub_env state body in
    applyExpander mapper.top_stmt env state { top = TopFunction (f, block body loc); loc }
  | { top = TopType descr; _ } ->
    let state, descr = struct_descr mapper sub_env state descr in
    applyExpander mapper.top_stmt env state { top = TopType descr; loc }
  | { top = TopAlias { path; alias_of }; _ } ->
    applyExpander mapper.top_stmt env state { top = TopAlias { path; alias_of }; loc }


let prog (mapper : ('env, 'data) mapper) (env : 'env) (state : 'data state) (p : prog) : 'data state * prog =
  let state, p = list top_stmt mapper env state p in
  state, List.flatten p
