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

type path =
  { id : string
  ; n : string option
  ; loc : Loc.t
  }

type type_d =
  | STId of path
  | STSize of int
  | STComposed of string * type_ list

and type_ =
  { t : type_d
  ; loc : Loc.t
  }

type exp_d =
  | SEBool of bool
  | SEInt of string
  | SEReal of string
  | SEFixed of string
  | SEString of string
  | SEId of string
  | SEEnum of path
  | SEIndex of
      { e : exp
      ; index : exp
      }
  | SEArray of exp list
  | SECall of
      { instance : (string * exp option) option
      ; path : path
      ; args : exp list
      }
  | SEUnOp of string * exp
  | SEOp of string * exp * exp
  | SEIf of
      { cond : exp
      ; then_ : exp
      ; else_ : exp
      }
  | SETuple of exp list
  | SEMember of exp * string
  | SEGroup of exp

and exp =
  { e : exp_d
  ; loc : Loc.t
  }

and lexp_d =
  | SLWild
  | SLId of string
  | SLMember of lexp * string
  | SLIndex of
      { e : lexp
      ; index : exp
      }
  | SLGroup of lexp
  | SLTuple of lexp list

and lexp =
  { l : lexp_d
  ; loc : Loc.t
  }

type dexp_d =
  | SDWild
  | SDId of string * int option
  | SDTuple of dexp list
  | SDGroup of dexp
  | SDTyped of dexp * type_

and dexp =
  { d : dexp_d
  ; loc : Loc.t
  }

type arg = string * type_ option * Loc.t

and stmt_d =
  | SStmtError
  | SStmtVal of dexp * exp option
  | SStmtMem of dexp * exp option * Ptags.tag list
  | SStmtBind of lexp * exp
  | SStmtReturn of exp
  | SStmtBlock of stmt list
  | SStmtIf of exp * stmt * stmt option
  | SStmtWhile of exp * stmt
  | SStmtIter of
      { id : string * Loc.t
      ; value : exp
      ; body : stmt
      }

and stmt =
  { s : stmt_d
  ; loc : Loc.t
  }

and function_def =
  { name : string
  ; args : arg list
  ; t : type_ option
  ; next : (function_def * stmt) option
  ; loc : Loc.t
  ; tags : Ptags.tag list
  }

and ext_def =
  { name : string
  ; args : arg list
  ; t : type_ option
  ; loc : Loc.t
  ; tags : Ptags.tag list
  }

type top_stmt_d =
  | STopError
  | STopExternal of ext_def * string option
  | STopFunction of function_def * stmt
  | STopType of
      { name : string
      ; members : (string * type_ * Loc.t) list
      }
  | STopEnum of
      { name : string
      ; members : (string * Loc.t) list
      }

and top_stmt =
  { top : top_stmt_d
  ; loc : Loc.t
  }

type stmts = top_stmt list

let compare_path (p1 : path) (p2 : path) =
  match p1, p2 with
  | { id = id1; n = Some n1; _ }, { id = id2; n = Some n2; _ } ->
    let ret = String.compare id1 id2 in
    if ret = 0 then String.compare n1 n2 else ret
  | { id = id1; n = None; _ }, { id = id2; n = None; _ } -> String.compare id1 id2
  | _ -> compare p1 p2


let print_path (p : path) =
  match p with
  | { id; n = None; _ } -> Pla.string id
  | { id; n = Some m; _ } -> [%pla {|<#m#s>.<#id#s>|}]


module Print = struct
  let stags tags =
    match tags with
    | [] -> Pla.unit
    | _ -> Pla.append Pla.space (Ptags.print_tags tags)


  let path (p : path) =
    match p with
    | { id; n = None; _ } -> Pla.string id
    | { id; n = Some m; _ } -> [%pla {|<#id#s>.<#m#s>|}]


  let rec type_ (t : type_) = type_d t.t

  and type_d t =
    match t with
    | STId p -> path p
    | STSize i -> Pla.int i
    | STComposed (name, subs) ->
      let subs = Pla.map_sep Pla.commaspace type_ subs in
      [%pla {|<#name#s><<#subs#>>|}]


  let rec exp (e : exp) = exp_d e.e

  and exp_d (e : exp_d) =
    match e with
    | SEBool b -> Pla.string (if b then "true" else "false")
    | SEInt i -> Pla.string i
    | SEReal f -> Pla.string f
    | SEFixed f -> Pla.string f
    | SEString s -> Pla.string_quoted s
    | SEId s -> Pla.string s
    | SEEnum p -> path p
    | SEIndex { e; index } ->
      let e = exp e in
      let index = exp index in
      [%pla {|<#e#>[<#index#>]|}]
    | SEArray elems ->
      let elems = Pla.map_sep Pla.commaspace exp elems in
      [%pla {|[ <#elems#> ]|}]
    | SECall { instance = None; path = name; args } ->
      let name = path name in
      let args = Pla.map_sep Pla.commaspace exp args in
      [%pla {|<#name#>(<#args#>)|}]
    | SECall { instance = Some (inst, sub); path = name; args } ->
      let name = path name in
      let args = Pla.map_sep Pla.commaspace exp args in
      let sub =
        Option.value
          (Option.map
             (fun sub ->
               let s = exp sub in
               [%pla {|[<#s#>]|}])
             sub)
          ~default:Pla.unit
      in
      [%pla {|<#inst#s><#sub#>:<#name#>(<#args#>)|}]
    | SEUnOp (op, e) ->
      let e = exp e in
      [%pla {|(<#op#s><#e#>)|}]
    | SEOp (op, e1, e2) ->
      let e1 = exp e1 in
      let e2 = exp e2 in
      [%pla {|(<#e1#> <#op#s> <#e2#>)|}]
    | SEIf { cond; then_; else_ } ->
      let cond = exp cond in
      let then_ = exp then_ in
      let else_ = exp else_ in
      [%pla {|if <#cond#> then <#then_#> else <#else_#>|}]
    | SETuple elems ->
      let elems = Pla.map_sep Pla.commaspace exp elems in
      [%pla {|<#elems#>|}]
    | SEMember (e, m) ->
      let e = exp e in
      [%pla {|<#e#>.<#m#s>|}]
    | SEGroup e ->
      let e = exp e in
      [%pla {|(<#e#>)|}]


  let rec lexp (l : lexp) = lexp_d l.l

  and lexp_d (l : lexp_d) =
    match l with
    | SLWild -> Pla.string "_"
    | SLId id -> Pla.string id
    | SLMember (e, m) ->
      let e = lexp e in
      [%pla {|<#e#>.<#m#s>|}]
    | SLIndex { e; index } ->
      let e = lexp e in
      let index = exp index in
      [%pla {|<#e#>[<#index#>]|}]
    | SLGroup e ->
      let e = lexp e in
      [%pla {|(<#e#>)|}]
    | SLTuple elems ->
      let elems = Pla.map_sep Pla.commaspace lexp elems in
      [%pla {|<#elems#>|}]


  let rec dexp (d : dexp) = dexp_d d.d

  and dexp_d (d : dexp_d) =
    match d with
    | SDWild -> Pla.string "_"
    | SDId (id, None) -> Pla.string id
    | SDId (id, Some n) -> [%pla {|<#id#s>[<#n#i>]|}]
    | SDTuple elems ->
      let elems = Pla.map_sep Pla.commaspace dexp elems in
      [%pla {|<#elems#>|}]
    | SDGroup e ->
      let e = dexp e in
      [%pla {|(<#e#>)|}]
    | SDTyped (e, t) ->
      let e = dexp e in
      let t = type_ t in
      [%pla {|<#e#> : <#t#>|}]


  let arg (n, t, _) =
    match t with
    | None -> Pla.string n
    | Some t ->
      let t = type_ t in
      [%pla {|<#n#s> : <#t#>|}]


  let rec stmt (s : stmt) = stmt_d s.s

  and stmt_d (s : stmt_d) =
    match s with
    | SStmtError -> Pla.string "<error>"
    | SStmtVal (d, None) ->
      let d = dexp d in
      [%pla {|val <#d#>;|}]
    | SStmtVal (d, Some e) ->
      let d = dexp d in
      let e = exp e in
      [%pla {|val <#d#> = <#e#>;|}]
    | SStmtMem (d, None, tags) ->
      let d = dexp d in
      let tags = stags tags in
      [%pla {|mem <#d#><#tags#>;|}]
    | SStmtMem (d, Some e, tags) ->
      let d = dexp d in
      let e = exp e in
      let tags = stags tags in
      [%pla {|mem <#d#><#tags#> = <#e#>;|}]
    | SStmtBind (l, e) ->
      let l = lexp l in
      let e = exp e in
      [%pla {|<#l#> = <#e#>;|}]
    | SStmtReturn e ->
      let e = exp e in
      [%pla {|return <#e#>;|}]
    | SStmtBlock stmts ->
      let stmts = Pla.map_sep_all Pla.newline stmt stmts in
      [%pla {| {<#stmts#+>}|}]
    | SStmtIf (cond, then_, None) ->
      let cond = exp cond in
      let then_ = stmt then_ in
      [%pla {|if (<#cond#>)<#then_#>|}]
    | SStmtIf (cond, then_, Some else_) ->
      let cond = exp cond in
      let then_ = stmt then_ in
      let else_ = stmt else_ in
      [%pla {|if (<#cond#>)<#then_#><#>else<#else_#>|}]
    | SStmtWhile (cond, body) ->
      let cond = exp cond in
      let body = stmt body in
      [%pla {|while (<#cond#>)<#body#>|}]
    | SStmtIter { id = id, _; value; body } ->
      let value = exp value in
      let body = stmt body in
      [%pla {|iter (<#id#s>, <#value#>)<#body#>|}]


  let genera_def name args t tags =
    let retType t =
      match t with
      | None -> Pla.unit
      | Some t ->
        let t = type_ t in
        [%pla {| : <#t#>|}]
    in
    let args = Pla.map_sep Pla.commaspace arg args in
    let t = retType t in
    let tags = stags tags in
    [%pla {|<#name#s>(<#args#>)<#t#><#tags#>|}]


  let rec function_def (def : function_def) body =
    let next f =
      match f with
      | None -> Pla.unit
      | Some (f, body) ->
        let d = function_def f body in
        [%pla {|<#>and <#d#>|}]
    in
    let decl = genera_def def.name def.args def.t def.tags in
    let body = stmt body in
    let n = next def.next in
    [%pla {|<#decl#><#body#><#n#>|}]


  let ext_def (def : ext_def) = genera_def def.name def.args def.t def.tags

  let member (n, t, _) =
    let t = type_ t in
    [%pla {|val <#n#s> : <#t#>|}]


  let enum_member (n, _) = Pla.string n

  let rec top_stmt (t : top_stmt) = top_stmt_d t.top

  and top_stmt_d (t : top_stmt_d) =
    match t with
    | STopError -> Pla.string "<error>"
    | STopExternal (def, None) ->
      let d = ext_def def in
      [%pla {|external <#d#>;|}]
    | STopExternal (def, Some name) ->
      let d = ext_def def in
      [%pla {|external <#d#> "<#name#s>";|}]
    | STopFunction (def, body) ->
      let d = function_def def body in
      [%pla {|fun <#d#>|}]
    | STopType { name; members } ->
      let members = Pla.map_sep_all [%pla {|;<#>|}] member members in
      [%pla {|type <#name#s> {<#members#+>}|}]
    | STopEnum { name; members } ->
      let members = Pla.map_sep [%pla {|;<#>|}] enum_member members in
      [%pla {|enum <#name#s> {<#members#+>}|}]


  let stmts (s : top_stmt list) = Pla.map_sep_all Pla.newline top_stmt s
  let print s = Pla.print @@ stmts s
end

module Mapper = struct
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
    { arg : (arg, 'data, 'ctx) mapper_func
    ; dexp : (dexp, 'data, 'ctx) mapper_func
    ; dexp_d : (dexp_d, 'data, 'ctx) mapper_func
    ; exp : (exp, 'data, 'ctx) mapper_func
    ; exp_d : (exp_d, 'data, 'ctx) mapper_func
    ; ext_def : (ext_def, 'data, 'ctx) mapper_func
    ; function_def : (function_def, 'data, 'ctx) mapper_func
    ; lexp : (lexp, 'data, 'ctx) mapper_func
    ; lexp_d : (lexp_d, 'data, 'ctx) mapper_func
    ; path : (path, 'data, 'ctx) mapper_func
    ; stmt : (stmt, 'data, 'ctx) mapper_func
    ; stmt_d : (stmt_d, 'data, 'ctx) mapper_func
    ; stmts : (stmts, 'data, 'ctx) mapper_func
    ; top_stmt : (top_stmt, 'data, 'ctx) mapper_func
    ; top_stmt_d : (top_stmt_d, 'data, 'ctx) mapper_func
    ; type_ : (type_, 'data, 'ctx) mapper_func
    ; type_d : (type_d, 'data, 'ctx) mapper_func
    ; arg_pre : (arg, 'data, 'ctx) pre_mapper_func
    ; dexp_d_pre : (dexp_d, 'data, 'ctx) pre_mapper_func
    ; dexp_pre : (dexp, 'data, 'ctx) pre_mapper_func
    ; exp_d_pre : (exp_d, 'data, 'ctx) pre_mapper_func
    ; exp_pre : (exp, 'data, 'ctx) pre_mapper_func
    ; ext_def_pre : (ext_def, 'data, 'ctx) pre_mapper_func
    ; function_def_pre : (function_def, 'data, 'ctx) pre_mapper_func
    ; lexp_d_pre : (lexp_d, 'data, 'ctx) pre_mapper_func
    ; lexp_pre : (lexp, 'data, 'ctx) pre_mapper_func
    ; path_pre : (path, 'data, 'ctx) pre_mapper_func
    ; stmt_d_pre : (stmt_d, 'data, 'ctx) pre_mapper_func
    ; stmt_pre : (stmt, 'data, 'ctx) pre_mapper_func
    ; stmts_pre : (stmts, 'data, 'ctx) pre_mapper_func
    ; top_stmt_d_pre : (top_stmt_d, 'data, 'ctx) pre_mapper_func
    ; top_stmt_pre : (top_stmt, 'data, 'ctx) pre_mapper_func
    ; type__pre : (type_, 'data, 'ctx) pre_mapper_func
    ; type_d_pre : (type_d, 'data, 'ctx) pre_mapper_func
    }

  let default =
    { arg = None
    ; dexp = None
    ; dexp_d = None
    ; exp = None
    ; exp_d = None
    ; ext_def = None
    ; function_def = None
    ; lexp = None
    ; lexp_d = None
    ; path = None
    ; stmt = None
    ; stmt_d = None
    ; stmts = None
    ; top_stmt = None
    ; top_stmt_d = None
    ; type_ = None
    ; type_d = None
    ; arg_pre = None
    ; dexp_d_pre = None
    ; dexp_pre = None
    ; exp_d_pre = None
    ; exp_pre = None
    ; ext_def_pre = None
    ; function_def_pre = None
    ; lexp_d_pre = None
    ; lexp_pre = None
    ; path_pre = None
    ; stmt_d_pre = None
    ; stmt_pre = None
    ; stmts_pre = None
    ; top_stmt_d_pre = None
    ; top_stmt_pre = None
    ; type__pre = None
    ; type_d_pre = None
    }


  let seq a b =
    { arg = seqMapperFunc a.arg b.arg
    ; dexp = seqMapperFunc a.dexp b.dexp
    ; dexp_d = seqMapperFunc a.dexp_d b.dexp_d
    ; exp = seqMapperFunc a.exp b.exp
    ; exp_d = seqMapperFunc a.exp_d b.exp_d
    ; ext_def = seqMapperFunc a.ext_def b.ext_def
    ; function_def = seqMapperFunc a.function_def b.function_def
    ; lexp = seqMapperFunc a.lexp b.lexp
    ; lexp_d = seqMapperFunc a.lexp_d b.lexp_d
    ; path = seqMapperFunc a.path b.path
    ; stmt = seqMapperFunc a.stmt b.stmt
    ; stmt_d = seqMapperFunc a.stmt_d b.stmt_d
    ; stmts = seqMapperFunc a.stmts b.stmts
    ; top_stmt = seqMapperFunc a.top_stmt b.top_stmt
    ; top_stmt_d = seqMapperFunc a.top_stmt_d b.top_stmt_d
    ; type_ = seqMapperFunc a.type_ b.type_
    ; type_d = seqMapperFunc a.type_d b.type_d
    ; arg_pre = seqPreMapperFunc a.arg_pre b.arg_pre
    ; dexp_d_pre = seqPreMapperFunc a.dexp_d_pre b.dexp_d_pre
    ; dexp_pre = seqPreMapperFunc a.dexp_pre b.dexp_pre
    ; exp_d_pre = seqPreMapperFunc a.exp_d_pre b.exp_d_pre
    ; exp_pre = seqPreMapperFunc a.exp_pre b.exp_pre
    ; ext_def_pre = seqPreMapperFunc a.ext_def_pre b.ext_def_pre
    ; function_def_pre = seqPreMapperFunc a.function_def_pre b.function_def_pre
    ; lexp_d_pre = seqPreMapperFunc a.lexp_d_pre b.lexp_d_pre
    ; lexp_pre = seqPreMapperFunc a.lexp_pre b.lexp_pre
    ; path_pre = seqPreMapperFunc a.path_pre b.path_pre
    ; stmt_d_pre = seqPreMapperFunc a.stmt_d_pre b.stmt_d_pre
    ; stmt_pre = seqPreMapperFunc a.stmt_pre b.stmt_pre
    ; stmts_pre = seqPreMapperFunc a.stmts_pre b.stmts_pre
    ; top_stmt_d_pre = seqPreMapperFunc a.top_stmt_d_pre b.top_stmt_d_pre
    ; top_stmt_pre = seqPreMapperFunc a.top_stmt_pre b.top_stmt_pre
    ; type__pre = seqPreMapperFunc a.type__pre b.type__pre
    ; type_d_pre = seqPreMapperFunc a.type_d_pre b.type_d_pre
    }


  let rec map_path mapper ocontext state idata =
    let context, state, idata = apply_pre mapper.path_pre ocontext state idata in
    let state, odata =
      if context.recurse then (
        match idata with
        | { id; n; loc } ->
          let n' = n in
          let id' = id in
          let odata =
            if id == id' && n == n' then
              idata
            else
              { id = id'; n = n'; loc }
          in
          state, odata)
      else
        state, idata
    in
    apply mapper.path ocontext state odata


  and map_type_d mapper ocontext state idata =
    let context, state, idata = apply_pre mapper.type_d_pre ocontext state idata in
    let state, odata =
      if context.recurse then (
        match idata with
        | STId field_0 ->
          let state, field_0' = map_path mapper context state field_0 in
          let odata = if field_0 == field_0' then idata else STId field_0' in
          state, odata
        | STSize _ -> state, idata
        | STComposed (field_0, field_1) ->
          let state, field_1' = (mapper_list map_type_) mapper context state field_1 in
          let field_0' = field_0 in
          let odata = if field_0 == field_0' && field_1 == field_1' then idata else STComposed (field_0', field_1') in
          state, odata)
      else
        state, idata
    in
    apply mapper.type_d ocontext state odata


  and map_type_ mapper ocontext state idata =
    let context, state, idata = apply_pre mapper.type__pre ocontext state idata in
    let state, odata =
      if context.recurse then (
        match idata with
        | { t; loc } ->
          let state, t' = map_type_d mapper context state t in
          let odata =
            if t == t' then
              idata
            else
              { t = t'; loc }
          in
          state, odata)
      else
        state, idata
    in
    apply mapper.type_ ocontext state odata


  and map_exp_d mapper ocontext state idata =
    let context, state, idata = apply_pre mapper.exp_d_pre ocontext state idata in
    let state, odata =
      if context.recurse then (
        match idata with
        | SEBool _ -> state, idata
        | SEInt _ -> state, idata
        | SEReal _ -> state, idata
        | SEFixed _ -> state, idata
        | SEString _ -> state, idata
        | SEId _ -> state, idata
        | SEEnum field_0 ->
          let state, field_0' = map_path mapper context state field_0 in
          let odata = if field_0 == field_0' then idata else SEEnum field_0' in
          state, odata
        | SEIndex { e; index } ->
          let state, index' = map_exp mapper context state index in
          let state, e' = map_exp mapper context state e in
          let odata =
            if e == e' && index == index' then
              idata
            else
              SEIndex { e = e'; index = index' }
          in
          state, odata
        | SEArray field_0 ->
          let state, field_0' = (mapper_list map_exp) mapper context state field_0 in
          let odata = if field_0 == field_0' then idata else SEArray field_0' in
          state, odata
        | SECall { instance; path; args } ->
          let state, args' = (mapper_list map_exp) mapper context state args in
          let state, path' = map_path mapper context state path in
          let state, instance' =
            (mapper_opt (mapper_tuple2 bypass (mapper_opt map_exp))) mapper context state instance
          in
          let odata =
            if instance == instance' && path == path' && args == args' then
              idata
            else
              SECall { instance = instance'; path = path'; args = args' }
          in
          state, odata
        | SEUnOp (field_0, field_1) ->
          let state, field_1' = map_exp mapper context state field_1 in
          let field_0' = field_0 in
          let odata = if field_0 == field_0' && field_1 == field_1' then idata else SEUnOp (field_0', field_1') in
          state, odata
        | SEOp (field_0, field_1, field_2) ->
          let state, field_2' = map_exp mapper context state field_2 in
          let state, field_1' = map_exp mapper context state field_1 in
          let field_0' = field_0 in
          let odata =
            if field_0 == field_0' && field_1 == field_1' && field_2 == field_2' then
              idata
            else
              SEOp (field_0', field_1', field_2')
          in
          state, odata
        | SEIf { cond; then_; else_ } ->
          let state, else_' = map_exp mapper context state else_ in
          let state, then_' = map_exp mapper context state then_ in
          let state, cond' = map_exp mapper context state cond in
          let odata =
            if cond == cond' && then_ == then_' && else_ == else_' then
              idata
            else
              SEIf { cond = cond'; then_ = then_'; else_ = else_' }
          in
          state, odata
        | SETuple field_0 ->
          let state, field_0' = (mapper_list map_exp) mapper context state field_0 in
          let odata = if field_0 == field_0' then idata else SETuple field_0' in
          state, odata
        | SEMember (field_0, field_1) ->
          let field_1' = field_1 in
          let state, field_0' = map_exp mapper context state field_0 in
          let odata = if field_0 == field_0' && field_1 == field_1' then idata else SEMember (field_0', field_1') in
          state, odata
        | SEGroup field_0 ->
          let state, field_0' = map_exp mapper context state field_0 in
          let odata = if field_0 == field_0' then idata else SEGroup field_0' in
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
        | { e; loc } ->
          let state, e' = map_exp_d mapper context state e in
          let odata =
            if e == e' then
              idata
            else
              { e = e'; loc }
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
        | SLWild -> state, SLWild
        | SLId _ -> state, idata
        | SLMember (field_0, field_1) ->
          let field_1' = field_1 in
          let state, field_0' = map_lexp mapper context state field_0 in
          let odata = if field_0 == field_0' && field_1 == field_1' then idata else SLMember (field_0', field_1') in
          state, odata
        | SLIndex { e; index } ->
          let state, index' = map_exp mapper context state index in
          let state, e' = map_lexp mapper context state e in
          let odata =
            if e == e' && index == index' then
              idata
            else
              SLIndex { e = e'; index = index' }
          in
          state, odata
        | SLGroup field_0 ->
          let state, field_0' = map_lexp mapper context state field_0 in
          let odata = if field_0 == field_0' then idata else SLGroup field_0' in
          state, odata
        | SLTuple field_0 ->
          let state, field_0' = (mapper_list map_lexp) mapper context state field_0 in
          let odata = if field_0 == field_0' then idata else SLTuple field_0' in
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
        | { l; loc } ->
          let state, l' = map_lexp_d mapper context state l in
          let odata =
            if l == l' then
              idata
            else
              { l = l'; loc }
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
        | SDWild -> state, SDWild
        | SDId _ -> state, idata
        | SDTuple field_0 ->
          let state, field_0' = (mapper_list map_dexp) mapper context state field_0 in
          let odata = if field_0 == field_0' then idata else SDTuple field_0' in
          state, odata
        | SDGroup field_0 ->
          let state, field_0' = map_dexp mapper context state field_0 in
          let odata = if field_0 == field_0' then idata else SDGroup field_0' in
          state, odata
        | SDTyped (field_0, field_1) ->
          let state, field_1' = map_type_ mapper context state field_1 in
          let state, field_0' = map_dexp mapper context state field_0 in
          let odata = if field_0 == field_0' && field_1 == field_1' then idata else SDTyped (field_0', field_1') in
          state, odata)
      else
        state, idata
    in
    apply mapper.dexp_d ocontext state odata


  and map_dexp mapper ocontext state idata =
    let context, state, idata = apply_pre mapper.dexp_pre ocontext state idata in
    let state, odata =
      if context.recurse then (
        match idata with
        | { d; loc } ->
          let state, d' = map_dexp_d mapper context state d in
          let odata =
            if d == d' then
              idata
            else
              { d = d'; loc }
          in
          state, odata)
      else
        state, idata
    in
    apply mapper.dexp ocontext state odata


  and map_arg mapper ocontext state idata =
    let context, state, idata = apply_pre mapper.arg_pre ocontext state idata in
    let state, odata =
      if context.recurse then (
        match idata with
        | _ -> (mapper_tuple3 bypass (mapper_opt map_type_) bypass) mapper context state idata)
      else
        state, idata
    in
    apply mapper.arg ocontext state odata


  and map_stmt_d mapper ocontext state idata =
    let context, state, idata = apply_pre mapper.stmt_d_pre ocontext state idata in
    let state, odata =
      if context.recurse then (
        match idata with
        | SStmtError -> state, SStmtError
        | SStmtVal (field_0, field_1) ->
          let state, field_1' = (mapper_opt map_exp) mapper context state field_1 in
          let state, field_0' = map_dexp mapper context state field_0 in
          let odata = if field_0 == field_0' && field_1 == field_1' then idata else SStmtVal (field_0', field_1') in
          state, odata
        | SStmtMem (field_0, field_1, field_2) ->
          let state, field_1' = (mapper_opt map_exp) mapper context state field_1 in
          let state, field_0' = map_dexp mapper context state field_0 in
          let odata =
            if field_0 == field_0' && field_1 == field_1' then
              idata
            else
              SStmtMem (field_0', field_1', field_2)
          in
          state, odata
        | SStmtBind (field_0, field_1) ->
          let state, field_1' = map_exp mapper context state field_1 in
          let state, field_0' = map_lexp mapper context state field_0 in
          let odata = if field_0 == field_0' && field_1 == field_1' then idata else SStmtBind (field_0', field_1') in
          state, odata
        | SStmtReturn field_0 ->
          let state, field_0' = map_exp mapper context state field_0 in
          let odata = if field_0 == field_0' then idata else SStmtReturn field_0' in
          state, odata
        | SStmtBlock field_0 ->
          let state, field_0' = (mapper_list map_stmt) mapper context state field_0 in
          let odata = if field_0 == field_0' then idata else SStmtBlock field_0' in
          state, odata
        | SStmtIf (field_0, field_1, field_2) ->
          let state, field_2' = (mapper_opt map_stmt) mapper context state field_2 in
          let state, field_1' = map_stmt mapper context state field_1 in
          let state, field_0' = map_exp mapper context state field_0 in
          let odata =
            if field_0 == field_0' && field_1 == field_1' && field_2 == field_2' then
              idata
            else
              SStmtIf (field_0', field_1', field_2')
          in
          state, odata
        | SStmtWhile (field_0, field_1) ->
          let state, field_1' = map_stmt mapper context state field_1 in
          let state, field_0' = map_exp mapper context state field_0 in
          let odata = if field_0 == field_0' && field_1 == field_1' then idata else SStmtWhile (field_0', field_1') in
          state, odata
        | SStmtIter { id; value; body } ->
          let state, body' = map_stmt mapper context state body in
          let state, value' = map_exp mapper context state value in
          let state, id' = (mapper_tuple2 bypass bypass) mapper context state id in
          let odata =
            if id == id' && value == value' && body == body' then
              idata
            else
              SStmtIter { id = id'; value = value'; body = body' }
          in
          state, odata)
      else
        state, idata
    in
    apply mapper.stmt_d ocontext state odata


  and map_stmt mapper ocontext state idata =
    let context, state, idata = apply_pre mapper.stmt_pre ocontext state idata in
    let state, odata =
      if context.recurse then (
        match idata with
        | { s; loc } ->
          let state, s' = map_stmt_d mapper context state s in
          let odata =
            if s == s' then
              idata
            else
              { s = s'; loc }
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
        | { name; args; t; next; loc; tags } ->
          let state, next' = (mapper_opt (mapper_tuple2 map_function_def map_stmt)) mapper context state next in
          let state, t' = (mapper_opt map_type_) mapper context state t in
          let state, args' = (mapper_list map_arg) mapper context state args in
          let name' = name in
          let odata =
            if name == name' && args == args' && t == t' && next == next' then
              idata
            else
              { name = name'; args = args'; t = t'; next = next'; loc; tags }
          in
          state, odata)
      else
        state, idata
    in
    apply mapper.function_def ocontext state odata


  and map_ext_def mapper ocontext state idata =
    let context, state, idata = apply_pre mapper.ext_def_pre ocontext state idata in
    let state, odata =
      if context.recurse then (
        match idata with
        | { name; args; t; loc; tags } ->
          let state, t' = (mapper_opt map_type_) mapper context state t in
          let state, args' = (mapper_list map_arg) mapper context state args in
          let name' = name in
          let odata =
            if name == name' && args == args' && t == t' then
              idata
            else
              { name = name'; args = args'; t = t'; loc; tags }
          in
          state, odata)
      else
        state, idata
    in
    apply mapper.ext_def ocontext state odata


  and map_top_stmt_d mapper ocontext state idata =
    let context, state, idata = apply_pre mapper.top_stmt_d_pre ocontext state idata in
    let state, odata =
      if context.recurse then (
        match idata with
        | STopError -> state, STopError
        | STopExternal (field_0, field_1) ->
          let field_1' = field_1 in
          let state, field_0' = map_ext_def mapper context state field_0 in
          let odata = if field_0 == field_0' && field_1 == field_1' then idata else STopExternal (field_0', field_1') in
          state, odata
        | STopFunction (field_0, field_1) ->
          let state, field_1' = map_stmt mapper context state field_1 in
          let state, field_0' = map_function_def mapper context state field_0 in
          let odata = if field_0 == field_0' && field_1 == field_1' then idata else STopFunction (field_0', field_1') in
          state, odata
        | STopType { name; members } ->
          let state, members' = (mapper_list (mapper_tuple3 bypass map_type_ bypass)) mapper context state members in
          let name' = name in
          let odata =
            if name == name' && members == members' then
              idata
            else
              STopType { name = name'; members = members' }
          in
          state, odata
        | STopEnum { name; members } ->
          let state, members' = (mapper_list (mapper_tuple2 bypass bypass)) mapper context state members in
          let name' = name in
          let odata =
            if name == name' && members == members' then
              idata
            else
              STopEnum { name = name'; members = members' }
          in
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


  and map_stmts mapper ocontext state idata =
    let context, state, idata = apply_pre mapper.stmts_pre ocontext state idata in
    let state, odata =
      if context.recurse then (
        match idata with
        | _ -> (mapper_list map_top_stmt) mapper context state idata)
      else
        state, idata
    in
    apply mapper.stmts ocontext state odata
end
