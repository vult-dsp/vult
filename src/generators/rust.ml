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

open Core.Prog

let runtime = {pla||pla}

let isValue (e : exp) =
  match e.e with
  | EUnit
   |EBool _
   |EInt _
   |EReal _
   |EString _
   |EId _
   |EMember _ ->
      true
  | _ -> false


let function_name name = Pla.string (String.uncapitalize_ascii name)

let rec print_type_ (t : type_) =
  match t.t with
  | TVoid -> Pla.string "()"
  | TInt -> Pla.string "i32"
  | TReal -> Pla.string "f32"
  | TBool -> Pla.string "bool"
  | TString -> Pla.string "str"
  | TFixed -> Pla.string "i32"
  | TTuple l ->
      let l = Pla.map_sep Pla.commaspace print_type_ l in
      {pla|(<#l#>)|pla}
  | TArray (dim, t) ->
      let t = print_type_ t in
      {pla|[<#t#>; <#dim#i>]|pla}
  | TStruct { path; _ } -> Pla.string path


let isReference (t : type_) =
  match t.t with
  | TStruct _
   |TArray _ ->
      true
  | _ -> false


let print_type_arg (t : type_) =
  if isReference t then
    let t = print_type_ t in
    {pla|&mut <#t#>|pla}
  else
    print_type_ t


let operator op =
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


let uoperator op =
  match op with
  | UOpNeg -> Pla.string "-"
  | UOpNot -> Pla.string "not"


let rec call_arg (e : exp) =
  let prefix =
    match e.t.t with
    | TTuple _
     |TArray _
     |TStruct _ ->
        Pla.string "&mut "
    | _ -> Pla.unit
  in
  Pla.append prefix (print_exp e)


and print_exp e =
  match e.e with
  | EUnit -> Pla.string ""
  | EBool v -> Pla.string (if v then "true" else "false")
  | EInt n -> {pla|<#n#i>i32|pla}
  | EReal n -> Pla.float n
  | EString s -> Pla.string_quoted s
  | EId id -> Pla.string id
  | EIndex { e; index } ->
      let e = print_exp e in
      let index = print_exp index in
      {pla|<#e#>[(<#index#>) as usize]|pla}
  | EArray l -> Pla.wrap (Pla.string "{") (Pla.string "}") (Pla.map_sep Pla.commaspace print_exp l)
  | ECall { path = "not"; args = [ e1 ] } ->
      let e1 = print_exp e1 in
      {pla|!<#e1#>|pla}
  | ECall { path; args } ->
      let args = Pla.map_sep Pla.commaspace call_arg args in
      let path = function_name path in
      {pla|<#path#>(<#args#>)|pla}
  | EUnOp (op, e) ->
      let e = print_exp e in
      let op = uoperator op in
      {pla|(<#op#><#e#>)|pla}
  | EOp (op, e1, e2) ->
      let e1 = print_exp e1 in
      let op = operator op in
      let e2 = print_exp e2 in
      {pla|(<#e1#> <#op#> <#e2#>)|pla}
  | EIf { cond; then_; else_ } ->
      let cond = print_exp cond in
      let then_ = print_exp then_ in
      let else_ = print_exp else_ in
      {pla|if <#cond#> { <#then_#> } else { <#else_#> }|pla}
  | ETuple l ->
      let l = Pla.map_sep Pla.commaspace print_exp l in
      {pla|(<#l#>)|pla}
  | EMember (e, m) ->
      let e = print_exp e in
      {pla|<#e#>.<#m#s>|pla}


let rec print_lexp e =
  match e.l with
  | LWild -> Pla.string "_"
  | LId s -> Pla.string s
  | LMember (e, m) ->
      let e = print_lexp e in
      {pla|<#e#>.<#m#s>|pla}
  | LIndex { e; index } ->
      let e = print_lexp e in
      let index = print_exp index in
      {pla|<#e#>[(<#index#>) as usize]|pla}
  | LTuple l ->
      let l = Pla.map_sep Pla.commaspace print_lexp l in
      {pla|(<#l#>)|pla}


let rec print_dexp (e : dexp) =
  match e.d with
  | DWild -> {pla|_|pla}
  | DId (id, None) -> {pla|mut <#id#s>|pla}
  | DId (id, Some dim) -> {pla|<#id#s>[<#dim#i>]|pla}
  | DTuple l ->
      let l = Pla.map_sep Pla.commaspace print_dexp l in
      {pla|(<#l#>)|pla}


and prefixWithType (t : type_) n : Pla.t =
  if isReference t then
    {pla|&mut <#n#>|pla}
  else
    n


let rec print_stmt s =
  match s.s with
  | StmtDecl (lhs, None) ->
      let t = print_type_ lhs.t in
      let lhs = print_dexp lhs in
      {pla|let <#lhs#> : <#t#>;|pla}
  | StmtDecl (lhs, Some rhs) ->
      let lhs = print_dexp lhs in
      let rhs = prefixWithType rhs.t (print_exp rhs) in
      {pla|let <#lhs#> = <#rhs#>;|pla}
  | StmtBind ({ l = LWild; _ }, rhs) ->
      let rhs = prefixWithType rhs.t (print_exp rhs) in
      {pla|let _ = <#rhs#>;|pla}
  | StmtBind (lhs, rhs) ->
      let lhs = print_lexp lhs in
      let rhs = prefixWithType rhs.t (print_exp rhs) in
      {pla|<#lhs#> = <#rhs#>;|pla}
  | StmtReturn e ->
      let e = prefixWithType e.t (print_exp e) in
      {pla|return <#e#>;|pla}
  | StmtIf (cond, then_, None) ->
      let e = print_exp cond in
      let then_ = print_block then_ in
      {pla|if <#e#> <#then_#><#>|pla}
  | StmtIf (cond, then_, Some else_) ->
      let cond = print_exp cond in
      let then_ = print_block then_ in
      let else_ = print_block else_ in
      {pla|if <#cond#> <#then_#><#>else <#else_#><#>|pla}
  | StmtWhile (cond, stmt) ->
      let cond = print_exp cond in
      let stmt = print_block stmt in
      {pla|while <#cond#> <#stmt#><#>|pla}
  | StmtBlock stmts ->
      let stmt = Pla.map_sep_all Pla.newline print_stmt stmts in
      {pla|{<#stmt#+>}|pla}


and print_block body =
  match body.s with
  | StmtBlock stmts ->
      let stmts = Pla.map_sep_all Pla.newline print_stmt stmts in
      {pla|{<#stmts#+>}|pla}
  | _ ->
      let stmt = print_stmt body in
      {pla|{<#stmt#+><#>}|pla}


let print_member (n, t, _) =
  let t = print_type_ t in
  {pla|<#n#s> : <#t#>|pla}


let addLifetime (args, t) = isReference t || List.exists isReference args

let print_type_life b t =
  if b && isReference t then
    let t = print_type_ t in
    {pla|&'a mut <#t#>|pla}
  else if isReference t then
    let t = print_type_ t in
    {pla|&mut <#t#>|pla}
  else
    print_type_ t


let print_fn_type_arg life (t : type_) =
  if isReference t then
    print_type_life life t
  else
    print_type_ t


let print_arg life (n, t, _) =
  let t = print_fn_type_arg life t in
  {pla|<#n#s> : <#t#>|pla}


let print_function_def (def : function_def) =
  let name = function_name def.name in
  let life = addLifetime def.t in
  let lifetime = if life then {pla|<'a>|pla} else Pla.unit in
  let args = Pla.map_sep Pla.commaspace (print_arg life) def.args in
  let ret = (print_type_life life) (snd def.t) in
  {pla|fn <#name#><#lifetime#>(<#args#>) -> <#ret#>|pla}


let print_top_stmt t =
  match t.top with
  | TopFunction (def, body) ->
      let def = print_function_def def in
      let body = print_block body in
      {pla|<#def#> <#body#><#><#>|pla}
  | TopExternal _ -> Pla.unit
  | TopType { path; members } ->
      let members = Pla.map_sep {pla|,<#>|pla} print_member members in
      {pla|struct <#path#s> {<#members#+><#>}<#><#>|pla}


let print_prog t = Pla.map_join print_top_stmt t

let generate (stmts : top_stmt list) : string =
  let code = print_prog stmts in
  Pla.print {pla|<#runtime#><#code#>|pla}
