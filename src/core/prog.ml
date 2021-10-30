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
open Pparser
open Util

type tag = Ptags.tag

type type_d_ =
  | TVoid   of type_ list option (* used to keep the original return type *)
  | TInt
  | TReal
  | TString
  | TBool
  | TFixed
  | TArray  of int * type_
  | TStruct of struct_descr
  | TTuple  of type_ list

and struct_descr =
  { path : string
  ; members : param list
  }

and param = string * type_ * Loc.t

and type_ =
  { t : type_d_
  ; loc : Loc.t
  }

type operator =
  | OpAdd
  | OpSub
  | OpMul
  | OpDiv
  | OpMod
  | OpLand
  | OpLor
  | OpBor
  | OpBand
  | OpBxor
  | OpLsh
  | OpRsh
  | OpEq
  | OpNe
  | OpLt
  | OpLe
  | OpGt
  | OpGe

type uoperator =
  | UOpNeg
  | UOpNot

type exp_d =
  | EUnit
  | EBool   of bool
  | EInt    of int
  | EReal   of float
  | EString of string
  | EId     of string
  | EIndex  of
      { e : exp
      ; index : exp
      }
  | EArray  of exp list
  | ECall   of
      { path : string
      ; args : exp list
      }
  | EUnOp   of uoperator * exp
  | EOp     of operator * exp * exp
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

type function_info =
  { original_name : string option
  ; is_root : bool
  }

type stmt_d =
  | StmtDecl   of dexp
  | StmtBind   of lexp * exp
  | StmtReturn of exp
  | StmtBlock  of stmt list
  | StmtIf     of exp * stmt * stmt option
  | StmtWhile  of exp * stmt

and stmt =
  { s : stmt_d
  ; loc : Loc.t
  }

and function_def =
  { name : string
  ; args : param list
  ; t : type_ list * type_
  ; loc : Loc.t
  ; tags : tag list
  ; info : function_info
  }

type top_stmt_d =
  | TopExternal of function_def * string option
  | TopFunction of function_def * stmt
  | TopType     of struct_descr

and top_stmt =
  { top : top_stmt_d
  ; loc : Loc.t
  }

type prog = top_stmt list

let default_info = { original_name = None; is_root = false }

module Print = struct
  let rec print_type_ (t : type_) : Pla.t =
    match t.t with
    | TVoid _ -> Pla.string "void"
    | TInt -> Pla.string "int"
    | TReal -> Pla.string "real"
    | TString -> Pla.string "string"
    | TBool -> Pla.string "bool"
    | TFixed -> Pla.string "fixed"
    | TArray (dim, t) ->
        let t = print_type_ t in
        [%pla {|<#t#>[<#dim#i>]|}]
    | TStruct { path; _ } -> [%pla {|struct <#path#s>|}]
    | TTuple elems ->
        let elems = Pla.map_sep Pla.commaspace print_type_ elems in
        [%pla {|(<#elems#>)|}]


  let print_operator op =
    match op with
    | OpAdd -> Pla.string "+"
    | OpSub -> Pla.string "-"
    | OpMul -> Pla.string "*"
    | OpDiv -> Pla.string "/"
    | OpMod -> Pla.string "%"
    | OpLand -> Pla.string "&&"
    | OpLor -> Pla.string "||"
    | OpBor -> Pla.string "|"
    | OpBand -> Pla.string "&"
    | OpBxor -> Pla.string "^"
    | OpLsh -> Pla.string "<<"
    | OpRsh -> Pla.string ">>"
    | OpEq -> Pla.string "=="
    | OpNe -> Pla.string "<>"
    | OpLt -> Pla.string "<"
    | OpLe -> Pla.string "<="
    | OpGt -> Pla.string ">"
    | OpGe -> Pla.string ">="


  let print_uoperator op =
    match op with
    | UOpNeg -> Pla.string "-"
    | UOpNot -> Pla.string "not"


  let rec print_exp (e : exp) =
    match e.e with
    | EUnit -> Pla.string "()"
    | EBool v -> Pla.string (if v then "true" else "false")
    | EInt n -> Pla.int n
    | EReal n -> Pla.float n
    | EString s -> Pla.string_quoted s
    | EId id -> Pla.string id
    | EIndex { e; index } ->
        let e = print_exp e in
        let index = print_exp index in
        [%pla {|<#e#>[<#index#>]|}]
    | EArray l -> Pla.wrap (Pla.string "{") (Pla.string "}") (Pla.map_sep Pla.commaspace print_exp l)
    | ECall { path; args } ->
        let args = Pla.map_sep Pla.commaspace print_exp args in
        [%pla {|<#path#s>(<#args#>)|}]
    | EUnOp (op, e) ->
        let e = print_exp e in
        let op = print_uoperator op in
        [%pla {|(<#op#><#e#>)|}]
    | EOp (op, e1, e2) ->
        let e1 = print_exp e1 in
        let e2 = print_exp e2 in
        let op = print_operator op in
        [%pla {|(<#e1#> <#op#> <#e2#>)|}]
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
    | StmtDecl lhs ->
        let lhs = print_dexp lhs in
        [%pla {|val <#lhs#>;|}]
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


  let print_function_def kind (def : function_def) =
    let name = def.name in
    let args = Pla.map_sep Pla.commaspace print_arg def.args in
    let tags = Pparser.Ptags.print_tags def.tags in
    let t = print_type_ (snd def.t) in
    [%pla {|<#kind#s> <#name#s>(<#args#>) : <#t#> <#tags#>|}]


  let print_member (name, t, _) =
    let t = print_type_ t in
    [%pla {|<#name#s> : <#t#>;|}]


  let print_body body =
    match body.s with
    | StmtBlock _ -> print_stmt body
    | _ ->
        let stmt = print_stmt body in
        [%pla {|{<#stmt#+><#>}|}]


  let print_top_stmt t =
    match t.top with
    | TopFunction (def, body) ->
        let def = print_function_def "fun" def in
        let body = print_body body in
        [%pla {|<#def#> <#body#><#>|}]
    | TopExternal (def, Some link) ->
        let def = print_function_def "external" def in
        [%pla {|<#def#> "<#link#s>"<#>|}]
    | TopExternal (def, None) ->
        let def = print_function_def "external" def in
        [%pla {|<#def#><#>|}]
    | TopType { path = p; members } ->
        let members = Pla.map_sep_all Pla.newline print_member members in
        [%pla {|struct <#p#s> {<#members#+>}<#>|}]


  let print_prog t = Pla.map_sep_all Pla.newline print_top_stmt t
end
