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
open Pparser
open Util

type tags = Ptags.tags

type type_d_ =
  | TVoid of type_ list option (* used to keep the original return type *)
  | TInt
  | TReal
  | TString
  | TBool
  | TFix16
  | TArray of int option * type_
  | TStruct of struct_descr
  | TTuple of type_ list

and struct_descr =
  { path : string
  ; members : member list
  }

and member = string * type_ * tags * Loc.t
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
  | EBool of bool
  | EInt of int
  | EReal of float
  | EFixed of float
  | EString of string
  | EId of string
  | EUnOp of uoperator * exp
  | EOp of operator * exp * exp
  | EIndex of
      { e : exp
      ; index : exp
      }
  | EArray of exp list
  | ECall of
      { path : string
      ; args : exp list
      }
  | EIf of
      { cond : exp
      ; then_ : exp
      ; else_ : exp
      }
  | ETuple of exp list
  | EMember of exp * string
  | ETMember of exp * int

and exp =
  { e : exp_d
  ; loc : Loc.t
  ; t : type_
  }

and lexp_d =
  | LWild
  | LId of string
  | LMember of lexp * string
  | LIndex of
      { e : lexp
      ; index : exp
      }
  | LTuple of lexp list

and lexp =
  { l : lexp_d
  ; loc : Loc.t
  ; t : type_
  }

type dexp_d = DId of string * int option

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
  | StmtDecl of dexp * exp option
  | StmtBind of lexp * exp
  | StmtReturn of exp
  | StmtBlock of stmt list
  | StmtIf of exp * stmt * stmt option
  | StmtWhile of exp * stmt
  | StmtSwitch of exp * (exp * stmt) list * stmt option

and stmt =
  { s : stmt_d
  ; loc : Loc.t
  }

and function_def =
  { name : string
  ; args : param list
  ; t : type_ list * type_
  ; loc : Loc.t
  ; tags : tags
  ; info : function_info
  }

type top_stmt_d =
  | TopExternal of function_def * string option
  | TopFunction of function_def * stmt
  | TopType of struct_descr
  | TopAlias of
      { path : string
      ; alias_of : string
      }
  | TopConstant of string * int option * type_ * exp

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
    | TFix16 -> Pla.string "fixed"
    | TArray (Some dim, t) ->
      let t = print_type_ t in
      {%pla|<#t#>[<#dim#i>]|}
    | TArray (None, t) ->
      let t = print_type_ t in
      {%pla|<#t#>[:]|}
    | TStruct { path; _ } -> {%pla|struct <#path#s>|}
    | TTuple elems ->
      let elems = Pla.map_sep Pla.commaspace print_type_ elems in
      {%pla|(<#elems#>)|}


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


  let rec print_exp ?(no_types = false) (e : exp) =
    let t = print_type_ e.t in
    match e.e with
    | EUnit -> Pla.string "()"
    | EBool v -> Pla.string (if v then "true" else "false")
    | EInt n -> Pla.int n
    | EReal n -> Pla.float n
    | EFixed n -> {%pla|<#n#f>x|}
    | EString s -> Pla.string_quoted s
    | EId id ->
      if no_types then
        {%pla|<#id#s>|}
      else
        {%pla|(<#id#s> : <#t#>)|}
    | EIndex { e; index } ->
      let e = (print_exp ~no_types) e in
      let index = (print_exp ~no_types) index in
      {%pla|<#e#>[<#index#>]|}
    | EArray l -> Pla.wrap (Pla.string "{") (Pla.string "}") (Pla.map_sep Pla.commaspace (print_exp ~no_types) l)
    | ECall { path; args } ->
      let args = Pla.map_sep Pla.commaspace (print_exp ~no_types) args in
      if no_types then
        {%pla|<#path#s>(<#args#>)|}
      else
        {%pla|(<#path#s>(<#args#>) : <#t#>)|}
    | EUnOp (op, e) ->
      let e = (print_exp ~no_types) e in
      let op = print_uoperator op in
      {%pla|(<#op#><#e#>)|}
    | EOp (op, e1, e2) ->
      let e1 = (print_exp ~no_types) e1 in
      let e2 = (print_exp ~no_types) e2 in
      let op = print_operator op in
      {%pla|(<#e1#> <#op#> <#e2#>)|}
    | EIf { cond; then_; else_ } ->
      let cond = (print_exp ~no_types) cond in
      let then_ = (print_exp ~no_types) then_ in
      let else_ = (print_exp ~no_types) else_ in
      {%pla|(if <#cond#> then <#then_#> else <#else_#>)|}
    | ETuple l ->
      let l = Pla.map_sep Pla.commaspace (print_exp ~no_types) l in
      {%pla|(<#l#>)|}
    | EMember (e, m) ->
      let e = (print_exp ~no_types) e in
      {%pla|<#e#>.<#m#s>|}
    | ETMember (e, i) ->
      let e = (print_exp ~no_types) e in
      {%pla|<#e#>.<#i#i>|}


  let rec print_lexp (e : lexp) =
    let t = print_type_ e.t in
    match e.l with
    | LWild -> Pla.string "_"
    | LId s -> {%pla|(<#s#s> : <#t#>)|}
    | LMember (e, m) ->
      let e = print_lexp e in
      {%pla|<#e#>.<#m#s>|}
    | LIndex { e; index } ->
      let e = print_lexp e in
      let index = print_exp index in
      {%pla|<#e#>[<#index#>]|}
    | LTuple l ->
      let l = Pla.map_sep Pla.commaspace print_lexp l in
      {%pla|(<#l#>)|}


  let print_dexp (e : dexp) =
    let t = print_type_ e.t in
    match e.d with
    | DId (id, None) -> {%pla|<#id#s> : <#t#>|}
    | DId (id, Some dim) -> {%pla|<#id#s>[<#dim#i>] : <#t#>|}


  let rec print_stmt s =
    match s.s with
    | StmtDecl (lhs, None) ->
      let lhs = print_dexp lhs in
      {%pla|val <#lhs#>;|}
    | StmtDecl (lhs, Some rhs) ->
      let lhs = print_dexp lhs in
      let rhs = print_exp rhs in
      {%pla|val <#lhs#> = <#rhs#>;|}
    | StmtBind (lhs, rhs) ->
      let lhs = print_lexp lhs in
      let rhs = print_exp rhs in
      {%pla|<#lhs#> = <#rhs#>;|}
    | StmtReturn e ->
      let e = print_exp e in
      {%pla|return <#e#>;|}
    | StmtIf (cond, then_, None) ->
      let e = print_exp cond in
      let then_ = print_stmt then_ in
      {%pla|if (<#e#>) <#then_#>|}
    | StmtIf (cond, then_, Some else_) ->
      let cond = print_exp cond in
      let then_ = print_stmt then_ in
      let else_ = print_stmt else_ in
      {%pla|if (<#cond#>) <#then_#><#>else <#else_#>|}
    | StmtWhile (cond, stmt) ->
      let cond = print_exp cond in
      let stmt = print_stmt stmt in
      {%pla|while (<#cond#>)<#stmt#+>|}
    | StmtBlock stmts ->
      let stmt = Pla.map_sep_all Pla.newline print_stmt stmts in
      {%pla|{<#stmt#+>}|}
    | StmtSwitch (cond, cases, default) ->
      let cond = print_exp cond in
      let stmt =
        Pla.map_sep_all
          Pla.newline
          (fun (case, then_) -> Pla.join [ print_exp case; Pla.string " -> "; print_stmt then_ ])
          cases
      in
      let stmt =
        match default with
        | None -> stmt
        | Some default -> Pla.join [ stmt; Pla.newline; Pla.string "_ -> "; print_stmt default ]
      in
      {%pla|match (<#cond#>) {<#stmt#+>}|}


  let print_arg (n, t, _) =
    let t = print_type_ t in
    {%pla|<#n#s> : <#t#>|}


  let print_function_def kind (def : function_def) =
    let name = def.name in
    let args = Pla.map_sep Pla.commaspace print_arg def.args in
    let tags = Pparser.Ptags.print_tags def.tags in
    let t = print_type_ (snd def.t) in
    {%pla|<#kind#s> <#name#s>(<#args#>) : <#t#><#tags#>|}


  let print_member (name, t, tags, _) =
    let tags = Ptags.print_tags tags in
    let t = print_type_ t in
    {%pla|<#name#s> : <#t#><#tags#>;|}


  let print_body body =
    match body.s with
    | StmtBlock _ -> print_stmt body
    | _ ->
      let stmt = print_stmt body in
      {%pla|{<#stmt#+><#>}|}


  let print_top_stmt t =
    match t.top with
    | TopFunction (def, body) ->
      let def = print_function_def "fun" def in
      let body = print_body body in
      {%pla|<#def#> <#body#><#>|}
    | TopExternal (def, Some link) ->
      let def = print_function_def "external" def in
      {%pla|<#def#> "<#link#s>"<#>|}
    | TopExternal (def, None) ->
      let def = print_function_def "external" def in
      {%pla|<#def#><#>|}
    | TopType { path = p; members } ->
      let members = Pla.map_sep_all Pla.newline print_member members in
      {%pla|struct <#p#s> {<#members#+>}<#>|}
    | TopAlias { path = p; alias_of } -> {%pla|type <#p#s> = <#alias_of#s><#>|}
    | TopConstant (path, dim, _, e) ->
      let e = print_exp e in
      let dim =
        match dim with
        | None -> Pla.unit
        | Some dim -> {%pla|[<#dim#i>]|}
      in
      {%pla|constant <#path#s><#dim#> = <#e#><#>|}


  let print_prog t = Pla.map_sep_all Pla.newline print_top_stmt t
end

module C = struct
  let void_t = { t = TVoid None; loc = Loc.default }
  let int_t = { t = TInt; loc = Loc.default }
  let string_t = { t = TString; loc = Loc.default }
  let bool_t = { t = TBool; loc = Loc.default }
  let real_t = { t = TReal; loc = Loc.default }
  let fix16_t = { t = TFix16; loc = Loc.default }
  let array_t ?dim t = { t = TArray (dim, t); loc = Loc.default }
  let ereal ?(loc = Loc.default) i = { e = EReal i; t = real_t; loc }
  let efix16 ?(loc = Loc.default) i = { e = EFixed i; t = fix16_t; loc }
  let estring ?(loc = Loc.default) i = { e = EString i; t = string_t; loc }
  let eint ?(loc = Loc.default) i = { e = EInt i; t = int_t; loc }
  let eunit = { e = EUnit; t = void_t; loc = Loc.default }
  let ebool ?(loc = Loc.default) i = { e = EBool i; t = int_t; loc }
  let eid ?(loc = Loc.default) id t = { e = EId id; t; loc }
  let did ?(loc = Loc.default) ?(size = None) id t = { d = DId (id, size); t; loc }
  let eadd ?(loc = Loc.default) e1 e2 = { e = EOp (OpAdd, e1, e2); t = e1.t; loc }
  let esub ?(loc = Loc.default) e1 e2 = { e = EOp (OpSub, e1, e2); t = e1.t; loc }
  let emul ?(loc = Loc.default) e1 e2 = { e = EOp (OpMul, e1, e2); t = e1.t; loc }
  let emod ?(loc = Loc.default) e1 e2 = { e = EOp (OpMod, e1, e2); t = e1.t; loc }
  let eeq ?(loc = Loc.default) e1 e2 = { e = EOp (OpEq, e1, e2); t = bool_t; loc }
  let elt ?(loc = Loc.default) e1 e2 = { e = EOp (OpLt, e1, e2); t = bool_t; loc }
  let ecall ?(loc = Loc.default) path args t = { e = ECall { path; args }; t; loc }
  let eindex ?(loc = Loc.default) e index t = { e = EIndex { e; index }; t; loc }
  let emember ?(loc = Loc.default) e name t = { e = EMember (e, name); t; loc }
  let earray ?(loc = Loc.default) elems t = { e = EArray elems; t; loc }
  let lid ?(loc = Loc.default) id t = { l = LId id; t; loc }
  let lindex ?(loc = Loc.default) e index t = { l = LIndex { e; index }; t; loc }
  let sbind_wild ?(loc = Loc.default) (e : exp) = { s = StmtBind ({ l = LWild; t = e.t; loc }, e); loc }
  let sbind ?(loc = Loc.default) (l : lexp) (e : exp) = { s = StmtBind (l, e); loc }
  let sdecl ?(loc = Loc.default) ?(size = None) ?init id t = { s = StmtDecl (did ~loc ~size id t, init); loc }
  let sdecl_init ?(loc = Loc.default) ?(size = None) id init t = { s = StmtDecl (did ~loc ~size id t, init); loc }

  let sdecl_bind ?(loc = Loc.default) ?(size = None) id e t =
    [ { s = StmtDecl (did ~loc ~size id t, None); loc }; { s = StmtBind (lid id t, e); loc } ]


  let sblock ?(loc = Loc.default) elems = { s = StmtBlock elems; loc }
  let sif ?(loc = Loc.default) cond then_ else_ = { s = StmtIf (cond, then_, else_); loc }
  let swhile ?(loc = Loc.default) cond body = { s = StmtWhile (cond, body); loc }
  let sreturn ?(loc = Loc.default) e = { s = StmtReturn e; loc }
  let sswitch ?(loc = Loc.default) e cases default = { s = StmtSwitch (e, cases, default); loc }
end

module Compare = struct
  let rec exp (e1 : exp) (e2 : exp) =
    match e1, e2 with
    | { e = EUnit; _ }, { e = EUnit; _ } -> 0
    | { e = EUnit; _ }, _ -> compare e1 e2
    | { e = EBool b1; _ }, { e = EBool b2; _ } -> compare b1 b2
    | { e = EBool _; _ }, _ -> compare e1 e2
    | { e = EInt b1; _ }, { e = EInt b2; _ } -> compare b1 b2
    | { e = EInt _; _ }, _ -> compare e1 e2
    | { e = EReal b1; _ }, { e = EReal b2; _ } -> compare b1 b2
    | { e = EReal _; _ }, _ -> compare e1 e2
    | { e = EFixed b1; _ }, { e = EFixed b2; _ } -> compare b1 b2
    | { e = EFixed _; _ }, _ -> compare e1 e2
    | { e = EString b1; _ }, { e = EString b2; _ } -> String.compare b1 b2
    | { e = EString _; _ }, _ -> compare e1 e2
    | { e = EId n1; _ }, { e = EId n2; _ } -> String.compare n1 n2
    | { e = EId _; _ }, _ -> compare e1 e2
    | { e = EOp (op1, a1, b1); _ }, { e = EOp (op2, a2, b2); _ } when op1 = op2 -> (
      match exp a1 a2 with
      | 0 -> exp b1 b2
      | n -> n)
    | { e = EOp _; _ }, _ -> compare e1 e2
    | { e = EUnOp (op1, a1); _ }, { e = EUnOp (op2, a2); _ } when op1 = op2 -> exp a1 a2
    | { e = EUnOp _; _ }, _ -> compare e1 e2
    | { e = EIndex { e = e1; index = index1 }; _ }, { e = EIndex { e = e2; index = index2 }; _ } -> (
      match exp e1 e2 with
      | 0 -> exp index1 index2
      | n -> n)
    | { e = EIndex _; _ }, _ -> compare e1 e2
    | { e = EArray elems1; _ }, { e = EArray elems2; _ } -> List.compare exp elems1 elems2
    | { e = EArray _; _ }, _ -> compare e1 e2
    | { e = ECall { path = path1; args = args1 }; _ }, { e = ECall { path = path2; args = args2 }; _ }
      when path1 = path2 -> List.compare exp args1 args2
    | { e = ECall _; _ }, _ -> compare e1 e2
    | { e = ETuple elems1; _ }, { e = ETuple elems2; _ } -> List.compare exp elems1 elems2
    | { e = ETuple _; _ }, _ -> compare e1 e2
    | { e = EMember (e1, member1); _ }, { e = EMember (e2, member2); _ } -> (
      match exp e1 e2 with
      | 0 -> String.compare member1 member2
      | n -> n)
    | { e = EMember _; _ }, _ -> compare e1 e2
    | { e = ETMember (e1, member1); _ }, { e = ETMember (e2, member2); _ } -> (
      match exp e1 e2 with
      | 0 -> compare member1 member2
      | n -> n)
    | { e = ETMember _; _ }, _ -> compare e1 e2
    | ( { e = EIf { cond = cond1; then_ = then1; else_ = else1 }; _ }
      , { e = EIf { cond = cond2; then_ = then2; else_ = else2 }; _ } ) -> (
      match exp cond1 cond2 with
      | 0 -> (
        match exp then1 then2 with
        | 0 -> exp else1 else2
        | n -> n)
      | n -> n)
    | { e = EIf _; _ }, _ -> compare e1 e2
end
