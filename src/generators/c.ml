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

open Code

type target =
  | Header
  | Tables
  | Implementation

let threshold = 4

let isSmall stmts =
  let rec loop size stmts =
    if size > threshold then
      size
    else
      match stmts with
      | [] -> size
      | StmtDecl _ :: t -> loop size t
      | StmtReturn _ :: t -> loop size t
      | StmtBlock inner :: t ->
          let size = loop size inner in
          loop size t
      | StmtIf (_, then_, None) :: t ->
          let size = loop (size + 1) [ then_ ] in
          loop size t
      | StmtIf (_, then_, Some else_) :: t ->
          let size = loop (size + 1) [ then_ ] in
          let size = loop size [ else_ ] in
          loop size t
      | StmtWhile (_, body) :: t ->
          let size = loop (size + 1) [ body ] in
          loop size t
      | StmtBind _ :: t -> loop (size + 1) t
  in
  let size = loop 0 stmts in
  size <= threshold


let rec print_type_ (t : type_) =
  match t with
  | Void -> Pla.string "void"
  | Int -> Pla.string "int32_t"
  | Real -> Pla.string "float"
  | Bool -> Pla.string "uint8_t"
  | String -> Pla.string "char*"
  | Fixed -> Pla.string "int32_t"
  | Tuple l ->
      let l = Pla.map_sep Pla.commaspace print_type_ l in
      {pla|(<#l#>)|pla}
  | Array (_, t) ->
      let t = print_type_ t in
      {pla|<#t#>*|pla}
  | Struct { path; _ } -> {pla|<#path#s>*|pla}


let operator op =
  match op with
  | Add -> Pla.string "+"
  | Sub -> Pla.string "-"
  | Mul -> Pla.string "*"
  | Div -> Pla.string "/"
  | Mod -> Pla.string "%"
  | Land -> Pla.string "&&"
  | Lor -> Pla.string "||"
  | Bor -> Pla.string "|"
  | Band -> Pla.string "&"
  | Bxor -> Pla.string "^"
  | Lsh -> Pla.string "<<"
  | Rsh -> Pla.string ">>"
  | Eq -> Pla.string "=="
  | Ne -> Pla.string "!="
  | Lt -> Pla.string "<"
  | Le -> Pla.string "<="
  | Gt -> Pla.string ">"
  | Ge -> Pla.string ">="


let uoperator op =
  match op with
  | Neg -> Pla.string "-"
  | Not -> Pla.string "not"


let rec print_exp e =
  match e.e with
  | Unit -> Pla.string ""
  | Bool v -> Pla.string (if v then "1" else "0")
  | Int n -> {pla|<#n#i>|pla}
  | Real n -> {pla|<#n#f>f|pla}
  | String s -> Pla.string_quoted s
  | Id id -> Pla.string id
  | Index { e; index } ->
      let e = print_exp e in
      let index = print_exp index in
      {pla|<#e#>[<#index#>]|pla}
  | Array l -> Pla.wrap (Pla.string "{") (Pla.string "}") (Pla.map_sep Pla.commaspace print_exp l)
  | Call { path = "not"; args = [ e1 ] } ->
      let e1 = print_exp e1 in
      {pla|!<#e1#>|pla}
  | Call { path; args } ->
      let args = Pla.map_sep Pla.commaspace print_exp args in
      {pla|<#path#s>(<#args#>)|pla}
  | UnOp (op, e) ->
      let e = print_exp e in
      let op = uoperator op in
      {pla|(<#op#><#e#>)|pla}
  | Op (op, e1, e2) ->
      let e1 = print_exp e1 in
      let op = operator op in
      let e2 = print_exp e2 in
      {pla|(<#e1#> <#op#> <#e2#>)|pla}
  | If { cond; then_; else_ } ->
      let cond = print_exp cond in
      let then_ = print_exp then_ in
      let else_ = print_exp else_ in
      {pla|(<#cond#> ? <#then_#> : <#else_#>)|pla}
  | Tuple l ->
      let l = Pla.map_sep Pla.commaspace print_exp l in
      {pla|(<#l#>)|pla}
  | Member (e, m) ->
      let e = print_exp e in
      {pla|<#e#>-><#m#s>|pla}


let rec print_lexp e =
  match e.l with
  | LWild -> Pla.string "_"
  | LId s -> Pla.string s
  | LMember (e, m) ->
      let e = print_lexp e in
      {pla|<#e#>-><#m#s>|pla}
  | LIndex (e, index) ->
      let e = print_lexp e in
      let index = print_exp index in
      {pla|<#e#>[<#index#>]|pla}


let print_dexp (e : dexp) =
  match e.d with
  | DId (id, None) -> {pla|<#id#s>|pla}
  | DId (id, Some dim) -> {pla|<#id#s>[<#dim#i>]|pla}


let rec print_stmt s =
  match s with
  | StmtDecl (lhs, None) ->
      let t = print_type_ lhs.t in
      let lhs = print_dexp lhs in
      {pla|<#t#> <#lhs#>;|pla}
  | StmtDecl (lhs, Some rhs) ->
      let t = print_type_ lhs.t in
      let lhs = print_dexp lhs in
      let rhs = print_exp rhs in
      {pla|<#t#> <#lhs#> = <#rhs#>;|pla}
  | StmtBind ({ l = LWild; _ }, rhs) ->
      let rhs = print_exp rhs in
      {pla|<#rhs#>;|pla}
  | StmtBind (lhs, rhs) ->
      let lhs = print_lexp lhs in
      let rhs = print_exp rhs in
      {pla|<#lhs#> = <#rhs#>;|pla}
  | StmtReturn e ->
      let e = print_exp e in
      {pla|return <#e#>;|pla}
  | StmtIf (cond, then_, None) ->
      let e = print_exp cond in
      let then_ = print_block then_ in
      {pla|if (<#e#>) <#then_#>|pla}
  | StmtIf (cond, then_, Some else_) ->
      let cond = print_exp cond in
      let then_ = print_block then_ in
      let else_ = print_block else_ in
      {pla|if (<#cond#>) <#then_#><#>else <#else_#>|pla}
  | StmtWhile (cond, stmt) ->
      let cond = print_exp cond in
      let stmt = print_block stmt in
      {pla|while (<#cond#>) <#stmt#>|pla}
  | StmtBlock stmts ->
      let stmt = Pla.map_sep_all Pla.newline print_stmt stmts in
      {pla|{<#stmt#+>}|pla}


and print_block body =
  match body with
  | StmtBlock stmts ->
      let stmts = Pla.map_sep_all Pla.newline print_stmt stmts in
      {pla|{<#stmts#+>}|pla}
  | _ ->
      let stmt = print_stmt body in
      {pla|{<#stmt#+><#>}|pla}


let print_member (n, (t : type_)) =
  match t with
  | Array (dim, sub) ->
      let sub = print_type_ sub in
      {pla|<#sub#> <#n#s>[<#dim#i>];|pla}
  | _ ->
      let t = print_type_ t in
      {pla|<#t#> <#n#s>;|pla}


let print_arg (n, (t : type_)) =
  match t with
  | Array (_, sub) ->
      let sub = print_type_ sub in
      {pla|<#sub#> *<#n#s>|pla}
  | _ ->
      let t = print_type_ t in
      {pla|<#t#> <#n#s>|pla}


let print_function_def (def : function_def) =
  let name = Pla.string def.name in
  let args = Pla.map_sep Pla.commaspace print_arg def.args in
  let ret = print_type_ (snd def.t) in
  {pla|<#ret#> <#name#>(<#args#>)|pla}


let print_top_stmt (target : target) t =
  match t, target with
  | TopFunction (def, body), Header ->
      let inline = isSmall [ body ] in
      let def = print_function_def def in
      if inline then
        let body = print_block body in
        {pla|static_inline <#def#> <#body#><#><#>|pla}
      else
        {pla|<#def#>;<#><#>|pla}
  | TopFunction (def, body), Implementation ->
      let inline = isSmall [ body ] in
      if inline then
        Pla.unit
      else
        let def = print_function_def def in
        let body = print_block body in
        {pla|<#def#> <#body#><#><#>|pla}
  | TopFunction _, _ -> Pla.unit
  | TopExternal _, _ -> Pla.unit
  | TopType { path; members }, Header ->
      let members = Pla.map_sep {pla|<#>|pla} print_member members in
      {pla|typedef struct <#path#s> {<#members#+><#>} <#path#s>;<#><#>|pla}
  | TopType _, _ -> Pla.unit
  | TopDecl (lhs, rhs), Tables ->
      let t = print_type_ lhs.t in
      let lhs = print_dexp lhs in
      let rhs = print_exp rhs in
      {pla|static const <#t#> <#lhs#> = <#rhs#>;<#>|pla}
  | TopDecl _, _ -> Pla.unit


let print_prog target t = Pla.map_join (print_top_stmt target) t

let legend = Common.legend

let makeIfdef file =
  let def = CCString.replace ~sub:"." ~by:"_" (String.uppercase_ascii (Filename.basename file)) in
  ( {pla|
#ifndef <#def#s>
#define <#def#s>
#ifdef __cplusplus
extern "C" {
#endif
|pla}
  , {pla|
#ifdef __cplusplus
}
#endif
#endif // <#def#s>
|pla} )


let generate output _template (stmts : Code.top_stmt list) =
  let header = print_prog Header stmts in
  let implementation = print_prog Implementation stmts in
  let tables = print_prog Tables stmts in

  let header_file = Common.setExt ".h" output in
  let tables_file = Common.setExt ".tables.h" output in
  let impl_file = Common.setExt ".c" output in

  let header_file_base = Filename.basename header_file in
  let tables_file_base = Filename.basename tables_file in

  let header =
    let ifdef, endif = makeIfdef header_file in
    {pla|<#legend#><#ifdef#><#>#include "vultin.h"<#><#><#header#><#endif#>|pla}
  in
  let implementation =
    {pla|<#legend#><#><#>#include "<#header_file_base#s>"<#>#include "<#tables_file_base#s>"<#><#><#implementation#>|pla}
  in
  let tables =
    let ifdef, endif = makeIfdef tables_file in
    {pla|<#legend#><#ifdef#><#tables#><#endif#>|pla}
  in

  [ header, header_file; implementation, impl_file; tables, tables_file ]
