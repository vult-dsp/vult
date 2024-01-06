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
open Core
open Util.Maps
open Util

type tag = Prog.tag

type type_ =
  | Void of type_ list option
  | Int
  | Real
  | String
  | Bool
  | Fixed
  | Array of int option * type_
  | Struct of struct_descr
  | Tuple of type_ list

and struct_descr =
  { path : string
  ; members : param list
  }

and param = string * type_

type operator =
  | Add
  | Sub
  | Mul
  | Div
  | Mod
  | Land
  | Lor
  | Bor
  | Band
  | Bxor
  | Lsh
  | Rsh
  | Eq
  | Ne
  | Lt
  | Le
  | Gt
  | Ge

type uoperator =
  | Neg
  | Not

type exp_d =
  | Unit
  | Bool of bool
  | Int of int
  | Real of float
  | Fixed of float
  | String of string
  | Id of string
  | Index of
      { e : exp
      ; index : exp
      }
  | Array of exp list
  | Call of
      { path : string
      ; args : exp list
      }
  | UnOp of uoperator * exp
  | Op of operator * exp * exp
  | If of
      { cond : exp
      ; then_ : exp
      ; else_ : exp
      }
  | Tuple of exp list
  | Member of exp * string
  | TMember of exp * int

and exp =
  { e : exp_d
  ; t : type_
  }

and lexp_d =
  | LWild
  | LId of string
  | LMember of lexp * string
  | LIndex of lexp * exp

and lexp =
  { l : lexp_d
  ; t : type_
  }

type dexp_d = DId of string * int option

and dexp =
  { d : dexp_d
  ; t : type_
  }

type function_info =
  { original_name : string option
  ; is_root : bool
  }

type stmt =
  | StmtDecl of dexp * exp option
  | StmtBind of lexp * exp
  | StmtReturn of exp
  | StmtBlock of stmt list
  | StmtIf of exp * stmt * stmt option
  | StmtWhile of exp * stmt
  | StmtSwitch of exp * (exp * stmt) list * stmt option

and function_def =
  { name : string
  ; args : param list
  ; t : type_ list * type_
  ; tags : tag list
  ; loc : Util.Loc.t
  ; info : function_info
  }

type top_stmt_d =
  | TopExternal of function_def * string option
  | TopFunction of function_def * stmt
  | TopType of struct_descr
  | TopAlias of string * string
  | TopDecl of dexp * exp

type top_stmt =
  { top : top_stmt_d
  ; loc : Loc.t
  }

type code = top_stmt list

type env =
  { decl : (int option * type_) Map.t
  ; dummy : int
  }

let default_env = { decl = Map.empty; dummy = 0 }

module CodeMapper = struct
  type 'a context =
    { context : 'a
    ; recurse : bool
    }

  type 'data state =
    { data : 'data
    ; repeat : bool
    }

  let makeContext context = { context; recurse = true }
  let getContext context = context.context
  let setContext context data = { context with context = data }
  let makeState data = { data; repeat = false }
  let get (state : 'a state) : 'a = state.data
  let set (state : 'a state) (data : 'a) : 'a state = { state with data }
  let make f = Some f
  let reapply (state : 'a state) : 'a state = if state.repeat then state else { state with repeat = true }
  let clearReapply (state : 'a state) : 'a state = { state with repeat = false }
  let defaultContext data : 'a context = data

  type ('kind, 'data, 'ctx) mapper_func = ('ctx context -> 'data state -> 'kind -> 'data state * 'kind) option

  type ('kind, 'data, 'ctx) pre_mapper_func =
    ('ctx context -> 'data state -> 'kind -> 'ctx context * 'data state * 'kind) option

  let apply (mapper : ('kind, 'data, 'ctx) mapper_func) (context : 'ctx context) (state : 'data state) (kind : 'kind)
    : 'data state * 'kind
    =
    match mapper with
    | Some f -> f context state kind
    | None -> state, kind


  let apply_pre
    (mapper : ('kind, 'data, 'ctx) pre_mapper_func)
    (context : 'ctx context)
    (state : 'data state)
    (kind : 'kind)
    : 'ctx context * 'data state * 'kind
    =
    match mapper with
    | Some f -> f context state kind
    | None -> context, state, kind


  let seqPreMapperFunc a b =
    match a, b with
    | None, _ -> b
    | _, None -> a
    | _ ->
      let new_mapper context state kind =
        let context', state', kind' = apply_pre a context state kind in
        apply_pre b context' state' kind'
      in
      Some new_mapper


  let seqMapperFunc a b =
    match a, b with
    | None, _ -> b
    | _, None -> a
    | _ ->
      let new_mapper context state kind =
        let state', kind' = apply a context state kind in
        apply b context state' kind'
      in
      Some new_mapper


  let ( |+> ) a b = seqMapperFunc a b

  let mapper_list mapper_app mapper (context : 'ctx context) (state : 'data state) (el : 'kind list) =
    let state', rev_el =
      List.fold_left
        (fun (state, acc) e ->
          let state', e' = mapper_app mapper context state e in
          state', e' :: acc)
        (state, [])
        el
    in
    state', List.rev rev_el


  let bypass _ (_ : 'ctx context) (state : 'data state) a = state, a

  let mapper_tuple2 mapper_app1 mapper_app2 mapper (context : 'ctx context) (state : 'data state) (t1, t2) =
    let state, t1 = mapper_app1 mapper context state t1 in
    let state, t2 = mapper_app2 mapper context state t2 in
    state, (t1, t2)


  let mapper_tuple3
    mapper_app1
    mapper_app2
    mapper_app3
    mapper
    (context : 'ctx context)
    (state : 'data state)
    (t1, t2, t3)
    =
    let state, t1 = mapper_app1 mapper context state t1 in
    let state, t2 = mapper_app2 mapper context state t2 in
    let state, t3 = mapper_app3 mapper context state t3 in
    state, (t1, t2, t3)


  let mapper_tuple4
    mapper_app1
    mapper_app2
    mapper_app3
    mapper_app4
    mapper
    (context : 'ctx context)
    (state : 'data state)
    (t1, t2, t3, t4)
    =
    let state, t1 = mapper_app1 mapper context state t1 in
    let state, t2 = mapper_app2 mapper context state t2 in
    let state, t3 = mapper_app3 mapper context state t3 in
    let state, t4 = mapper_app4 mapper context state t4 in
    state, (t1, t2, t3, t4)


  let mapper_opt mapper_app mapper (context : 'ctx context) (state : 'data state) (e_opt : 'kind option) =
    match e_opt with
    | None -> state, None
    | Some e ->
      let state', e' = mapper_app mapper context state e in
      state', Some e'


  type ('data, 'ctx) mapper =
    { code : (code, 'data, 'ctx) mapper_func
    ; dexp : (dexp, 'data, 'ctx) mapper_func
    ; dexp_d : (dexp_d, 'data, 'ctx) mapper_func
    ; exp : (exp, 'data, 'ctx) mapper_func
    ; exp_d : (exp_d, 'data, 'ctx) mapper_func
    ; function_def : (function_def, 'data, 'ctx) mapper_func
    ; function_info : (function_info, 'data, 'ctx) mapper_func
    ; lexp : (lexp, 'data, 'ctx) mapper_func
    ; lexp_d : (lexp_d, 'data, 'ctx) mapper_func
    ; operator : (operator, 'data, 'ctx) mapper_func
    ; param : (param, 'data, 'ctx) mapper_func
    ; stmt : (stmt, 'data, 'ctx) mapper_func
    ; struct_descr : (struct_descr, 'data, 'ctx) mapper_func
    ; tag : (tag, 'data, 'ctx) mapper_func
    ; top_stmt : (top_stmt, 'data, 'ctx) mapper_func
    ; top_stmt_d : (top_stmt_d, 'data, 'ctx) mapper_func
    ; type_ : (type_, 'data, 'ctx) mapper_func
    ; uoperator : (uoperator, 'data, 'ctx) mapper_func
    ; code_pre : (code, 'data, 'ctx) pre_mapper_func
    ; dexp_d_pre : (dexp_d, 'data, 'ctx) pre_mapper_func
    ; dexp_pre : (dexp, 'data, 'ctx) pre_mapper_func
    ; exp_d_pre : (exp_d, 'data, 'ctx) pre_mapper_func
    ; exp_pre : (exp, 'data, 'ctx) pre_mapper_func
    ; function_def_pre : (function_def, 'data, 'ctx) pre_mapper_func
    ; function_info_pre : (function_info, 'data, 'ctx) pre_mapper_func
    ; lexp_d_pre : (lexp_d, 'data, 'ctx) pre_mapper_func
    ; lexp_pre : (lexp, 'data, 'ctx) pre_mapper_func
    ; operator_pre : (operator, 'data, 'ctx) pre_mapper_func
    ; param_pre : (param, 'data, 'ctx) pre_mapper_func
    ; stmt_pre : (stmt, 'data, 'ctx) pre_mapper_func
    ; struct_descr_pre : (struct_descr, 'data, 'ctx) pre_mapper_func
    ; tag_pre : (tag, 'data, 'ctx) pre_mapper_func
    ; top_stmt_d_pre : (top_stmt_d, 'data, 'ctx) pre_mapper_func
    ; top_stmt_pre : (top_stmt, 'data, 'ctx) pre_mapper_func
    ; type__pre : (type_, 'data, 'ctx) pre_mapper_func
    ; uoperator_pre : (uoperator, 'data, 'ctx) pre_mapper_func
    }

  let default =
    { code = None
    ; dexp = None
    ; dexp_d = None
    ; exp = None
    ; exp_d = None
    ; function_def = None
    ; function_info = None
    ; lexp = None
    ; lexp_d = None
    ; operator = None
    ; param = None
    ; stmt = None
    ; struct_descr = None
    ; tag = None
    ; top_stmt = None
    ; top_stmt_d = None
    ; type_ = None
    ; uoperator = None
    ; code_pre = None
    ; dexp_d_pre = None
    ; dexp_pre = None
    ; exp_d_pre = None
    ; exp_pre = None
    ; function_def_pre = None
    ; function_info_pre = None
    ; lexp_d_pre = None
    ; lexp_pre = None
    ; operator_pre = None
    ; param_pre = None
    ; stmt_pre = None
    ; struct_descr_pre = None
    ; tag_pre = None
    ; top_stmt_d_pre = None
    ; top_stmt_pre = None
    ; type__pre = None
    ; uoperator_pre = None
    }


  let seq a b =
    { code = seqMapperFunc a.code b.code
    ; dexp = seqMapperFunc a.dexp b.dexp
    ; dexp_d = seqMapperFunc a.dexp_d b.dexp_d
    ; exp = seqMapperFunc a.exp b.exp
    ; exp_d = seqMapperFunc a.exp_d b.exp_d
    ; function_def = seqMapperFunc a.function_def b.function_def
    ; function_info = seqMapperFunc a.function_info b.function_info
    ; lexp = seqMapperFunc a.lexp b.lexp
    ; lexp_d = seqMapperFunc a.lexp_d b.lexp_d
    ; operator = seqMapperFunc a.operator b.operator
    ; param = seqMapperFunc a.param b.param
    ; stmt = seqMapperFunc a.stmt b.stmt
    ; struct_descr = seqMapperFunc a.struct_descr b.struct_descr
    ; tag = seqMapperFunc a.tag b.tag
    ; top_stmt = seqMapperFunc a.top_stmt b.top_stmt
    ; top_stmt_d = seqMapperFunc a.top_stmt_d b.top_stmt_d
    ; type_ = seqMapperFunc a.type_ b.type_
    ; uoperator = seqMapperFunc a.uoperator b.uoperator
    ; code_pre = seqPreMapperFunc a.code_pre b.code_pre
    ; dexp_d_pre = seqPreMapperFunc a.dexp_d_pre b.dexp_d_pre
    ; dexp_pre = seqPreMapperFunc a.dexp_pre b.dexp_pre
    ; exp_d_pre = seqPreMapperFunc a.exp_d_pre b.exp_d_pre
    ; exp_pre = seqPreMapperFunc a.exp_pre b.exp_pre
    ; function_def_pre = seqPreMapperFunc a.function_def_pre b.function_def_pre
    ; function_info_pre = seqPreMapperFunc a.function_info_pre b.function_info_pre
    ; lexp_d_pre = seqPreMapperFunc a.lexp_d_pre b.lexp_d_pre
    ; lexp_pre = seqPreMapperFunc a.lexp_pre b.lexp_pre
    ; operator_pre = seqPreMapperFunc a.operator_pre b.operator_pre
    ; param_pre = seqPreMapperFunc a.param_pre b.param_pre
    ; stmt_pre = seqPreMapperFunc a.stmt_pre b.stmt_pre
    ; struct_descr_pre = seqPreMapperFunc a.struct_descr_pre b.struct_descr_pre
    ; tag_pre = seqPreMapperFunc a.tag_pre b.tag_pre
    ; top_stmt_d_pre = seqPreMapperFunc a.top_stmt_d_pre b.top_stmt_d_pre
    ; top_stmt_pre = seqPreMapperFunc a.top_stmt_pre b.top_stmt_pre
    ; type__pre = seqPreMapperFunc a.type__pre b.type__pre
    ; uoperator_pre = seqPreMapperFunc a.uoperator_pre b.uoperator_pre
    }


  let rec map_tag mapper ocontext state idata =
    let context, state, idata = apply_pre mapper.tag_pre ocontext state idata in
    let state, odata =
      if context.recurse then (
        match idata with
        | _ -> state, idata)
      else
        state, idata
    in
    apply mapper.tag ocontext state odata


  and map_type_ mapper ocontext state idata =
    let context, state, idata = apply_pre mapper.type__pre ocontext state idata in
    let state, odata =
      if context.recurse then (
        match idata with
        | Void field_0 ->
          let state, field_0' = (mapper_opt (mapper_list map_type_)) mapper context state field_0 in
          let odata = if field_0 == field_0' then idata else Void field_0' in
          state, odata
        | Int -> state, Int
        | Real -> state, Real
        | String -> state, String
        | Bool -> state, Bool
        | Fixed -> state, Fixed
        | Array (field_0, field_1) ->
          let state, field_1' = map_type_ mapper context state field_1 in
          let field_0' = field_0 in
          let odata = if field_0 == field_0' && field_1 == field_1' then idata else Array (field_0', field_1') in
          state, odata
        | Struct field_0 ->
          let state, field_0' = map_struct_descr mapper context state field_0 in
          let odata = if field_0 == field_0' then idata else Struct field_0' in
          state, odata
        | Tuple field_0 ->
          let state, field_0' = (mapper_list map_type_) mapper context state field_0 in
          let odata = if field_0 == field_0' then idata else Tuple field_0' in
          state, odata)
      else
        state, idata
    in
    apply mapper.type_ ocontext state odata


  and map_struct_descr mapper ocontext state idata =
    let context, state, idata = apply_pre mapper.struct_descr_pre ocontext state idata in
    let state, odata =
      if context.recurse then (
        match idata with
        | { path; members } ->
          let state, members' = (mapper_list map_param) mapper context state members in
          let path' = path in
          let odata =
            if path == path' && members == members' then
              idata
            else
              { path = path'; members = members' }
          in
          state, odata)
      else
        state, idata
    in
    apply mapper.struct_descr ocontext state odata


  and map_param mapper ocontext state idata =
    let context, state, idata = apply_pre mapper.param_pre ocontext state idata in
    let state, odata =
      if context.recurse then (
        match idata with
        | _ -> (mapper_tuple2 bypass map_type_) mapper context state idata)
      else
        state, idata
    in
    apply mapper.param ocontext state odata


  and map_operator mapper ocontext state idata =
    let context, state, idata = apply_pre mapper.operator_pre ocontext state idata in
    let state, odata =
      if context.recurse then (
        match idata with
        | Add -> state, Add
        | Sub -> state, Sub
        | Mul -> state, Mul
        | Div -> state, Div
        | Mod -> state, Mod
        | Land -> state, Land
        | Lor -> state, Lor
        | Bor -> state, Bor
        | Band -> state, Band
        | Bxor -> state, Bxor
        | Lsh -> state, Lsh
        | Rsh -> state, Rsh
        | Eq -> state, Eq
        | Ne -> state, Ne
        | Lt -> state, Lt
        | Le -> state, Le
        | Gt -> state, Gt
        | Ge -> state, Ge)
      else
        state, idata
    in
    apply mapper.operator ocontext state odata


  and map_uoperator mapper ocontext state idata =
    let context, state, idata = apply_pre mapper.uoperator_pre ocontext state idata in
    let state, odata =
      if context.recurse then (
        match idata with
        | Neg -> state, Neg
        | Not -> state, Not)
      else
        state, idata
    in
    apply mapper.uoperator ocontext state odata


  and map_exp_d mapper ocontext state idata =
    let context, state, idata = apply_pre mapper.exp_d_pre ocontext state idata in
    let state, odata =
      if context.recurse then (
        match idata with
        | Unit -> state, Unit
        | Bool _ -> state, idata
        | Int _ -> state, idata
        | Real _ -> state, idata
        | Fixed _ -> state, idata
        | String _ -> state, idata
        | Id _ -> state, idata
        | Index { e; index } ->
          let state, index' = map_exp mapper context state index in
          let state, e' = map_exp mapper context state e in
          let odata =
            if e == e' && index == index' then
              idata
            else
              Index { e = e'; index = index' }
          in
          state, odata
        | Array field_0 ->
          let state, field_0' = (mapper_list map_exp) mapper context state field_0 in
          let odata = if field_0 == field_0' then idata else Array field_0' in
          state, odata
        | Call { path; args } ->
          let state, args' = (mapper_list map_exp) mapper context state args in
          let path' = path in
          let odata =
            if path == path' && args == args' then
              idata
            else
              Call { path = path'; args = args' }
          in
          state, odata
        | UnOp (field_0, field_1) ->
          let state, field_1' = map_exp mapper context state field_1 in
          let state, field_0' = map_uoperator mapper context state field_0 in
          let odata = if field_0 == field_0' && field_1 == field_1' then idata else UnOp (field_0', field_1') in
          state, odata
        | Op (field_0, field_1, field_2) ->
          let state, field_2' = map_exp mapper context state field_2 in
          let state, field_1' = map_exp mapper context state field_1 in
          let state, field_0' = map_operator mapper context state field_0 in
          let odata =
            if field_0 == field_0' && field_1 == field_1' && field_2 == field_2' then
              idata
            else
              Op (field_0', field_1', field_2')
          in
          state, odata
        | If { cond; then_; else_ } ->
          let state, else_' = map_exp mapper context state else_ in
          let state, then_' = map_exp mapper context state then_ in
          let state, cond' = map_exp mapper context state cond in
          let odata =
            if cond == cond' && then_ == then_' && else_ == else_' then
              idata
            else
              If { cond = cond'; then_ = then_'; else_ = else_' }
          in
          state, odata
        | Tuple field_0 ->
          let state, field_0' = (mapper_list map_exp) mapper context state field_0 in
          let odata = if field_0 == field_0' then idata else Tuple field_0' in
          state, odata
        | Member (field_0, field_1) ->
          let field_1' = field_1 in
          let state, field_0' = map_exp mapper context state field_0 in
          let odata = if field_0 == field_0' && field_1 == field_1' then idata else Member (field_0', field_1') in
          state, odata
        | TMember (field_0, field_1) ->
          let field_1' = field_1 in
          let state, field_0' = map_exp mapper context state field_0 in
          let odata = if field_0 == field_0' && field_1 == field_1' then idata else TMember (field_0', field_1') in
          state, odata)
      else
        state, idata
    in
    apply mapper.exp_d ocontext state odata


  and map_exp mapper ocontext state idata =
    let context, state, idata = apply_pre mapper.exp_pre ocontext state idata in
    let state, odata =
      if context.recurse then (
        match idata with
        | { e; t } ->
          let state, t' = map_type_ mapper context state t in
          let state, e' = map_exp_d mapper context state e in
          let odata =
            if e == e' && t == t' then
              idata
            else
              { e = e'; t = t' }
          in
          state, odata)
      else
        state, idata
    in
    apply mapper.exp ocontext state odata


  and map_lexp_d mapper ocontext state idata =
    let context, state, idata = apply_pre mapper.lexp_d_pre ocontext state idata in
    let state, odata =
      if context.recurse then (
        match idata with
        | LWild -> state, LWild
        | LId _ -> state, idata
        | LMember (field_0, field_1) ->
          let field_1' = field_1 in
          let state, field_0' = map_lexp mapper context state field_0 in
          let odata = if field_0 == field_0' && field_1 == field_1' then idata else LMember (field_0', field_1') in
          state, odata
        | LIndex (field_0, field_1) ->
          let state, field_1' = map_exp mapper context state field_1 in
          let state, field_0' = map_lexp mapper context state field_0 in
          let odata = if field_0 == field_0' && field_1 == field_1' then idata else LIndex (field_0', field_1') in
          state, odata)
      else
        state, idata
    in
    apply mapper.lexp_d ocontext state odata


  and map_lexp mapper ocontext state idata =
    let context, state, idata = apply_pre mapper.lexp_pre ocontext state idata in
    let state, odata =
      if context.recurse then (
        match idata with
        | { l; t } ->
          let state, t' = map_type_ mapper context state t in
          let state, l' = map_lexp_d mapper context state l in
          let odata =
            if l == l' && t == t' then
              idata
            else
              { l = l'; t = t' }
          in
          state, odata)
      else
        state, idata
    in
    apply mapper.lexp ocontext state odata


  and map_dexp_d mapper ocontext state idata =
    let context, state, idata = apply_pre mapper.dexp_d_pre ocontext state idata in
    let state, odata =
      if context.recurse then (
        match idata with
        | DId _ -> state, idata)
      else
        state, idata
    in
    apply mapper.dexp_d ocontext state odata


  and map_dexp mapper ocontext state idata =
    let context, state, idata = apply_pre mapper.dexp_pre ocontext state idata in
    let state, odata =
      if context.recurse then (
        match idata with
        | { d; t } ->
          let state, t' = map_type_ mapper context state t in
          let state, d' = map_dexp_d mapper context state d in
          let odata =
            if d == d' && t == t' then
              idata
            else
              { d = d'; t = t' }
          in
          state, odata)
      else
        state, idata
    in
    apply mapper.dexp ocontext state odata


  and map_function_info mapper ocontext state idata =
    let context, state, idata = apply_pre mapper.function_info_pre ocontext state idata in
    let state, odata =
      if context.recurse then (
        match idata with
        | { original_name; is_root } ->
          let is_root' = is_root in
          let original_name' = original_name in
          let odata =
            if original_name == original_name' && is_root == is_root' then
              idata
            else
              { original_name = original_name'; is_root = is_root' }
          in
          state, odata)
      else
        state, idata
    in
    apply mapper.function_info ocontext state odata


  and map_stmt mapper ocontext state idata =
    let context, state, idata = apply_pre mapper.stmt_pre ocontext state idata in
    let state, odata =
      if context.recurse then (
        match idata with
        | StmtDecl (field_0, field_1) ->
          let state, field_1' = (mapper_opt map_exp) mapper context state field_1 in
          let state, field_0' = map_dexp mapper context state field_0 in
          let odata = if field_0 == field_0' && field_1 == field_1' then idata else StmtDecl (field_0', field_1') in
          state, odata
        | StmtBind (field_0, field_1) ->
          let state, field_1' = map_exp mapper context state field_1 in
          let state, field_0' = map_lexp mapper context state field_0 in
          let odata = if field_0 == field_0' && field_1 == field_1' then idata else StmtBind (field_0', field_1') in
          state, odata
        | StmtReturn field_0 ->
          let state, field_0' = map_exp mapper context state field_0 in
          let odata = if field_0 == field_0' then idata else StmtReturn field_0' in
          state, odata
        | StmtBlock field_0 ->
          let state, field_0' = (mapper_list map_stmt) mapper context state field_0 in
          let odata = if field_0 == field_0' then idata else StmtBlock field_0' in
          state, odata
        | StmtIf (field_0, field_1, field_2) ->
          let state, field_2' = (mapper_opt map_stmt) mapper context state field_2 in
          let state, field_1' = map_stmt mapper context state field_1 in
          let state, field_0' = map_exp mapper context state field_0 in
          let odata =
            if field_0 == field_0' && field_1 == field_1' && field_2 == field_2' then
              idata
            else
              StmtIf (field_0', field_1', field_2')
          in
          state, odata
        | StmtWhile (field_0, field_1) ->
          let state, field_1' = map_stmt mapper context state field_1 in
          let state, field_0' = map_exp mapper context state field_0 in
          let odata = if field_0 == field_0' && field_1 == field_1' then idata else StmtWhile (field_0', field_1') in
          state, odata
        | StmtSwitch (field_0, field_1, field_2) ->
          let state, field_2' = (mapper_opt map_stmt) mapper context state field_2 in
          let state, field_1' = (mapper_list (mapper_tuple2 map_exp map_stmt)) mapper context state field_1 in
          let state, field_0' = map_exp mapper context state field_0 in
          let odata =
            if field_0 == field_0' && field_1 == field_1' && field_2 == field_2' then
              idata
            else
              StmtSwitch (field_0', field_1', field_2')
          in
          state, odata)
      else
        state, idata
    in
    apply mapper.stmt ocontext state odata


  and map_function_def mapper ocontext state idata =
    let context, state, idata = apply_pre mapper.function_def_pre ocontext state idata in
    let state, odata =
      if context.recurse then (
        match idata with
        | { name; args; t; tags; loc; info } ->
          let state, info' = map_function_info mapper context state info in
          let state, tags' = (mapper_list map_tag) mapper context state tags in
          let state, t' = (mapper_tuple2 (mapper_list map_type_) map_type_) mapper context state t in
          let state, args' = (mapper_list map_param) mapper context state args in
          let name' = name in
          let odata =
            if name == name' && args == args' && t == t' && tags == tags' && info == info' then
              idata
            else
              { name = name'; args = args'; t = t'; tags = tags'; loc; info = info' }
          in
          state, odata)
      else
        state, idata
    in
    apply mapper.function_def ocontext state odata


  and map_top_stmt_d mapper ocontext state idata =
    let context, state, idata = apply_pre mapper.top_stmt_d_pre ocontext state idata in
    let state, odata =
      if context.recurse then (
        match idata with
        | TopExternal (field_0, field_1) ->
          let field_1' = field_1 in
          let state, field_0' = map_function_def mapper context state field_0 in
          let odata = if field_0 == field_0' && field_1 == field_1' then idata else TopExternal (field_0', field_1') in
          state, odata
        | TopFunction (field_0, field_1) ->
          let state, field_1' = map_stmt mapper context state field_1 in
          let state, field_0' = map_function_def mapper context state field_0 in
          let odata = if field_0 == field_0' && field_1 == field_1' then idata else TopFunction (field_0', field_1') in
          state, odata
        | TopType field_0 ->
          let state, field_0' = map_struct_descr mapper context state field_0 in
          let odata = if field_0 == field_0' then idata else TopType field_0' in
          state, odata
        | TopAlias _ -> state, idata
        | TopDecl (field_0, field_1) ->
          let state, field_1' = map_exp mapper context state field_1 in
          let state, field_0' = map_dexp mapper context state field_0 in
          let odata = if field_0 == field_0' && field_1 == field_1' then idata else TopDecl (field_0', field_1') in
          state, odata)
      else
        state, idata
    in
    apply mapper.top_stmt_d ocontext state odata


  and map_top_stmt mapper ocontext state idata =
    let context, state, idata = apply_pre mapper.top_stmt_pre ocontext state idata in
    let state, odata =
      if context.recurse then (
        match idata with
        | { top; loc } ->
          let state, top' = map_top_stmt_d mapper context state top in
          let odata =
            if top == top' then
              idata
            else
              { top = top'; loc }
          in
          state, odata)
      else
        state, idata
    in
    apply mapper.top_stmt ocontext state odata


  and map_code mapper ocontext state idata =
    let context, state, idata = apply_pre mapper.code_pre ocontext state idata in
    let state, odata =
      if context.recurse then (
        match idata with
        | _ -> (mapper_list map_top_stmt) mapper context state idata)
      else
        state, idata
    in
    apply mapper.code ocontext state odata
end

module GetVariables = struct
  let exp_d =
    CodeMapper.make
    @@ fun _context state (e : exp_d) ->
    match e with
    | Id var ->
      let data = CodeMapper.get state in
      let state = CodeMapper.set state (Set.add var data) in
      state, e
    | _ -> state, e


  let lexp_d =
    CodeMapper.make
    @@ fun _context state (e : lexp_d) ->
    match e with
    | LId var ->
      let data = CodeMapper.get state in
      let state = CodeMapper.set state (Set.add var data) in
      state, e
    | _ -> state, e


  let dexp_d =
    CodeMapper.make
    @@ fun _context state (e : dexp_d) ->
    match e with
    | DId (var, _) ->
      let data = CodeMapper.get state in
      let state = CodeMapper.set state (Set.add var data) in
      state, e


  let mapper = { CodeMapper.default with exp_d; lexp_d; dexp_d }

  let from_exp (e : exp) =
    let state = CodeMapper.makeState Set.empty in
    let context = CodeMapper.makeContext () in
    let state, _ = CodeMapper.map_exp mapper context state e in
    Set.to_list (CodeMapper.get state)


  let from_stmts (s : stmt list) =
    let state = CodeMapper.makeState Set.empty in
    let context = CodeMapper.makeContext () in
    let state, _ = CodeMapper.mapper_list CodeMapper.map_stmt mapper context state s in
    Set.to_list (CodeMapper.get state)
end
