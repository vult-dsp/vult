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
open Pparser

type path = Syntax.path

let print_path (p : path) = Syntax.print_path p

type type_d_ =
  | TENoReturn
  | TEUnbound  of int
  | TEId       of path
  | TESize     of int
  | TELink     of type_
  | TEOption   of type_ list
  | TEComposed of string * type_ list

and type_ =
  { mutable tx : type_d_
  ; mutable loc : Loc.t
  }

type fun_type = type_ list * type_

let rec compare_type_ (a : type_) (b : type_) =
  if a == b then
    0
  else
    match a.tx, b.tx with
    | TELink a, _ -> compare_type_ a b
    | _, TELink b -> compare_type_ a b
    | TEId p1, TEId p2 -> Syntax.compare_path p1 p2
    | TESize p1, TESize p2 -> compare p1 p2
    | TEComposed (n1, e1), TEComposed (n2, e2) -> CCOrd.(string n1 n2 <?> (compare_type_list_, e1, e2))
    | TEOption e1, TEOption e2 -> compare_type_list_ e1 e2
    | TEUnbound n1, TEUnbound n2 -> compare n1 n2
    | _ -> compare a.tx b.tx


and compare_type_list_ a b = CCOrd.list compare_type_ a b

type tag = Ptags.tag

type exp_d =
  | EUnit
  | EBool   of bool
  | EInt    of int
  | EReal   of float
  | EFixed  of float
  | EString of string
  | EId     of string
  | EIndex  of
      { e : exp
      ; index : exp
      }
  | EArray  of exp list
  | ECall   of
      { instance : string option
      ; path : path
      ; args : exp list
      }
  | EUnOp   of string * exp
  | EOp     of string * exp * exp
  | EIf     of
      { cond : exp
      ; then_ : exp
      ; else_ : exp
      }
  | ETuple  of exp list
  | EMember of exp * string

and exp =
  { e : exp_d
  ; loc : Loc.t
  ; t : type_
  }

and lexp_d =
  | LWild
  | LId     of string
  | LMember of lexp * string
  | LIndex  of
      { e : lexp
      ; index : exp
      }
  | LTuple  of lexp list

and lexp =
  { l : lexp_d
  ; loc : Loc.t
  ; t : type_
  }

type dexp_d =
  | DWild
  | DId    of string * int option
  | DTuple of dexp list

and dexp =
  { d : dexp_d
  ; loc : Loc.t
  ; t : type_
  }

and stmt_d =
  | StmtVal    of dexp
  | StmtMem    of dexp * tag list
  | StmtBind   of lexp * exp
  | StmtReturn of exp
  | StmtBlock  of stmt list
  | StmtIf     of exp * stmt * stmt option
  | StmtWhile  of exp * stmt

and stmt =
  { s : stmt_d
  ; loc : Loc.t
  }

type arg = string * type_ * Loc.t

and function_def =
  { name : path
  ; args : arg list
  ; t : type_ list * type_
  ; next : (function_def * stmt) option
  ; loc : Loc.t
  ; tags : tag list
  ; is_root : bool
  }

type top_stmt_d =
  | TopExternal of function_def * string option
  | TopFunction of function_def * stmt
  | TopType     of
      { path : path
      ; members : (string * type_ * Loc.t) list
      }
  | TopEnum     of
      { path : path
      ; members : (string * Loc.t) list
      }

and top_stmt =
  { top : top_stmt_d
  ; loc : Loc.t
  }

type program = top_stmt list

let rec print_type_ (t : type_) : Pla.t =
  match t.tx with
  | TENoReturn -> Pla.string "noreturn"
  | TELink t -> print_type_ t
  | TEUnbound i -> [%pla {|'unbound(<#i#i>)|}]
  | TEId p -> print_path p
  | TESize n -> Pla.int n
  | TEOption alt -> Pla.parenthesize @@ Pla.map_sep (Pla.string "|") print_type_ alt
  | TEComposed (name, elems) ->
      let elems = Pla.map_sep Pla.commaspace print_type_ elems in
      [%pla {|<#name#s>(<#elems#>)|}]


let rec print_exp e =
  match e.e with
  | EUnit -> Pla.string "()"
  | EBool v -> Pla.string (if v then "true" else "false")
  | EInt n -> Pla.int n
  | EReal n -> Pla.float n
  | EFixed n -> [%pla {|<#n#f>x]|}]
  | EString s -> Pla.string_quoted s
  | EId id -> Pla.string id
  | EIndex { e; index } ->
      let e = print_exp e in
      let index = print_exp index in
      [%pla {|<#e#>[<#index#>]|}]
  | EArray l -> Pla.wrap (Pla.string "{") (Pla.string "}") (Pla.map_sep Pla.commaspace print_exp l)
  | ECall { instance; path; args } ->
      let instance = CCOpt.map_or ~default:Pla.unit (fun s -> [%pla {|<#s#s>:|}]) instance in
      let path = print_path path in
      let args = Pla.map_sep Pla.commaspace print_exp args in
      [%pla {|<#instance#><#path#>(<#args#>)|}]
  | EUnOp (op, e) ->
      let e = print_exp e in
      [%pla {|(<#op#s><#e#>)|}]
  | EOp (op, e1, e2) ->
      let e1 = print_exp e1 in
      let e2 = print_exp e2 in
      [%pla {|(<#e1#> <#op#s> <#e2#>)|}]
  | EIf { cond; then_; else_ } ->
      let cond = print_exp cond in
      let then_ = print_exp then_ in
      let else_ = print_exp else_ in
      [%pla {|(if <#cond#> then <#then_#> else <#else_#>)|}]
  | ETuple l ->
      let l = Pla.map_sep Pla.commaspace print_exp l in
      [%pla {|(<#l#>)|}]
  | EMember (e, m) ->
      let e = print_exp e in
      [%pla {|<#e#>.<#m#s>|}]


let rec print_lexp e =
  match e.l with
  | LWild -> Pla.string "_"
  | LId s -> Pla.string s
  | LMember (e, m) ->
      let e = print_lexp e in
      [%pla {|<#e#>.<#m#s>|}]
  | LIndex { e; index } ->
      let e = print_lexp e in
      let index = print_exp index in
      [%pla {|<#e#>[<#index#>]|}]
  | LTuple l ->
      let l = Pla.map_sep Pla.commaspace print_lexp l in
      [%pla {|(<#l#>)|}]


let rec print_dexp (e : dexp) =
  let t = print_type_ e.t in
  match e.d with
  | DWild -> [%pla {|_ : <#t#>|}]
  | DId (id, None) -> [%pla {|<#id#s> : <#t#>|}]
  | DId (id, Some dim) -> [%pla {|<#id#s>[<#dim#i>] : <#t#>|}]
  | DTuple l ->
      let l = Pla.map_sep Pla.commaspace print_dexp l in
      [%pla {|(<#l#>) : <#t#>|}]


let rec print_stmt s =
  match s.s with
  | StmtVal lhs ->
      let lhs = print_dexp lhs in
      [%pla {|val <#lhs#>;|}]
  | StmtMem (lhs, _) ->
      let lhs = print_dexp lhs in
      [%pla {|mem <#lhs#>;|}]
  | StmtBind (lhs, rhs) ->
      let lhs = print_lexp lhs in
      let rhs = print_exp rhs in
      [%pla {|<#lhs#> = <#rhs#>;|}]
  | StmtReturn e ->
      let e = print_exp e in
      [%pla {|return <#e#>;|}]
  | StmtIf (cond, then_, None) ->
      let e = print_exp cond in
      let then_ = print_stmt then_ in
      [%pla {|if (<#e#>) <#then_#>|}]
  | StmtIf (cond, then_, Some else_) ->
      let cond = print_exp cond in
      let then_ = print_stmt then_ in
      let else_ = print_stmt else_ in
      [%pla {|if (<#cond#>) <#then_#><#>else <#else_#>|}]
  | StmtWhile (cond, stmt) ->
      let cond = print_exp cond in
      let stmt = print_stmt stmt in
      [%pla {|while (<#cond#>)<#stmt#+>|}]
  | StmtBlock stmts ->
      let stmt = Pla.map_sep_all Pla.newline print_stmt stmts in
      [%pla {|{<#stmt#+>}|}]


let print_arg (n, t, _) =
  let t = print_type_ t in
  [%pla {|<#n#s> : <#t#>|}]


let next_kind kind =
  match kind with
  | "fun" -> "and"
  | "and" -> "and"
  | "external" -> "external"
  | _ -> failwith "invalid kind"


let print_body_linkname body_linkname =
  match body_linkname with
  | `Body stmt -> print_stmt stmt
  | `LinkName name -> [%pla {| "<#name#s>"|}]
  | `NoLinkName -> Pla.unit


let rec print_function_def kind (def : function_def) body_linkname =
  let name = print_path def.name in
  let args = Pla.map_sep Pla.commaspace print_arg def.args in
  let tags = Ptags.print_tags def.tags in
  let t = print_type_ (snd def.t) in
  let body = print_body_linkname body_linkname in
  let next = print_next_function_def kind def.next in
  [%pla {|<#kind#s> <#name#>(<#args#>) : <#t#> <#tags#><#body#><#><#next#>|}]


and print_next_function_def kind next =
  match next with
  | None -> Pla.unit
  | Some (def, body) -> print_function_def (next_kind kind) def (`Body body)


let print_record_member (name, t, _) =
  let t = print_type_ t in
  [%pla {|<#name#s> : <#t#>;|}]


let print_enum_member (name, _) = [%pla {|<#name#s>|}]

let print_top_stmt t =
  match t.top with
  | TopFunction (def, body) -> print_function_def "fun" def (`Body body)
  | TopExternal (def, Some linkname) -> print_function_def "external" def (`LinkName linkname)
  | TopExternal (def, None) -> print_function_def "external" def `NoLinkName
  | TopType { path = p; members } ->
      let p = print_path p in
      let members = Pla.map_sep_all Pla.newline print_record_member members in
      [%pla {|type <#p#> {<#members#+>}<#>|}]
  | TopEnum { path = p; members } ->
      let p = print_path p in
      let members = Pla.map_sep Pla.commaspace print_enum_member members in
      [%pla {|enum <#p#> {<#members#+>}<#>|}]


let print_prog prog = Pla.map_sep_all Pla.newline print_top_stmt prog

module C = struct
  let tick = ref 0

  let makeId loc id = { tx = TEId { id; n = None; loc }; loc }

  let path loc path = { tx = TEId path; loc }

  let unbound loc =
    incr tick ;
    { tx = TEUnbound !tick; loc }


  let noreturn loc = { tx = TENoReturn; loc }

  let unit ~loc = makeId loc "unit"

  let int ~loc = makeId loc "int"

  let bool ~loc = makeId loc "bool"

  let string ~loc = makeId loc "string"

  let real ~loc = makeId loc "real"

  let fix16 ~loc = makeId loc "fix16"

  let num loc = { tx = TEOption [ real ~loc; int ~loc; fix16 ~loc ]; loc }

  let num_bool loc = { tx = TEOption [ real ~loc; int ~loc; fix16 ~loc; bool ~loc ]; loc }

  let size ?(loc = Loc.default) n = { tx = TESize n; loc }

  let array ?(loc = Loc.default) ?(size = unbound loc) t = { tx = TEComposed ("array", [ t; size ]); loc }

  let tuple ?(loc = Loc.default) l = { tx = TEComposed ("tuple", l); loc }

  let freal_type () =
    let loc = Loc.default in
    { tx = TEOption [ real ~loc; fix16 ~loc ]; loc }


  let array_set () : fun_type =
    let loc = Loc.default in
    let a = unbound loc in
    let a_array = array a in
    [ a_array; int ~loc; a ], unit ~loc


  let array_get () : fun_type =
    let loc = Loc.default in
    let a = unbound loc in
    let a_array = array a in
    [ a_array; int ~loc ], a


  let array_size () : fun_type =
    let loc = Loc.default in
    let a = unbound loc in
    let a_array = array a in
    [ a_array ], int ~loc


  let array_make () : fun_type =
    let loc = Loc.default in
    let a = unbound loc in
    let a_array = array a in
    [ int ~loc; a ], a_array


  let wrap_array () : fun_type =
    let loc = Loc.default in
    let a = unbound loc in
    let array_type = array a in
    [ array_type ], array_type


  let freal_freal () : fun_type =
    let t = freal_type () in
    [ t ], t


  let real_real_real () : fun_type =
    let loc = Loc.default in
    let t = real ~loc in
    [ t; t ], t


  let clip () : fun_type =
    let loc = Loc.default in
    let t = unbound loc in
    [ t; t; t ], t


  let num_int () : fun_type =
    let loc = Loc.default in
    [ num loc ], int ~loc


  let num_real () : fun_type =
    let loc = Loc.default in
    [ num loc ], real ~loc


  let num_bool_real () : fun_type =
    let loc = Loc.default in
    [ num_bool loc ], real ~loc


  let num_fix16 () : fun_type =
    let loc = Loc.default in
    [ num loc ], fix16 ~loc


  let num_num () : fun_type =
    let loc = Loc.default in
    let t = num loc in
    [ t ], t


  let num_num_num () : fun_type =
    let loc = Loc.default in
    let t = num loc in
    [ t; t ], t


  let int_int_int () : fun_type =
    let loc = Loc.default in
    let t = int ~loc in
    [ t; t ], t


  let num_num_bool () : fun_type =
    let loc = Loc.default in
    let t = num loc in
    [ t; t ], bool ~loc


  let a_a_bool () : fun_type =
    let loc = Loc.default in
    let t = unbound loc in
    [ t; t ], bool ~loc


  let bool_bool () : fun_type =
    let loc = Loc.default in
    let t = bool ~loc in
    [ t ], t


  let bool_bool_bool () : fun_type =
    let loc = Loc.default in
    let t = bool ~loc in
    [ t; t ], t


  let unit_int () : fun_type =
    let loc = Loc.default in
    [], int ~loc


  let unit_real () : fun_type =
    let loc = Loc.default in
    [], real ~loc
end
