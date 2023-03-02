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
  | SEUnit
  | SEBool of bool
  | SEInt of int
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
      { instance : string option
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
      [%pla {|<#name#s>(<#subs#>)|}]


  let rec exp (e : exp) = exp_d e.e

  and exp_d (e : exp_d) =
    match e with
    | SEUnit -> Pla.string "()"
    | SEBool b -> Pla.string (if b then "true" else "false")
    | SEInt i -> Pla.int i
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
    | SECall { instance = Some inst; path = name; args } ->
      let name = path name in
      let args = Pla.map_sep Pla.commaspace exp args in
      [%pla {|<#inst#s>:<#name#>(<#args#>)|}]
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
