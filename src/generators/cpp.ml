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
    if size > threshold
    then size
    else (
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
      | StmtBind _ :: t -> loop (size + 1) t)
  in
  let size = loop 0 stmts in
  size <= threshold
;;

let rec print_type_ (t : type_) =
  match t with
  | Void _ -> Pla.string "void"
  | Int -> Pla.string "int32_t"
  | Real -> Pla.string "float"
  | Bool -> Pla.string "bool"
  | String -> Pla.string "char*"
  | Fixed -> Pla.string "fix16_t"
  | Tuple l ->
    let l = Pla.map_sep Pla.commaspace print_type_ l in
    [%pla {|(<#l#>)|}]
  | Array (dim, t) ->
    let t = print_type_ t in
    [%pla {|std::array<<#t#>, <#dim#i>>|}]
  | Struct { path; _ } -> [%pla {|<#path#s>|}]
;;

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
;;

let uoperator op =
  match op with
  | Neg -> Pla.string "-"
  | Not -> Pla.string "not"
;;

let getReplacement name (args : exp list) ret =
  let args_t = List.map (fun (e : exp) -> e.t) args in
  match Replacements.C.fun_to_fun name args_t ret with
  | Some path -> path
  | None -> name
;;

let rec print_exp (e : exp) =
  match e.e with
  | Unit -> Pla.string ""
  | Bool v -> Pla.string (if v then "true" else "false")
  | Int n -> [%pla {|<#n#i>|}]
  | Real n -> [%pla {|<#n#f>f|}]
  | Fixed n ->
    let n = Common.toFixed n in
    [%pla {|<#n#s>|}]
  | String s -> Pla.string_quoted s
  | Id id -> Pla.string id
  | Index { e; index } ->
    let e = print_exp e in
    let index = print_exp index in
    [%pla {|<#e#>[static_cast<uint32_t>(<#index#>)]|}]
  | Array l ->
    let rows = Common.splitArray 100 l in
    let l = Pla.map_sep [%pla {|,<#>|}] (Pla.map_sep Pla.commaspace print_exp) rows in
    [%pla {|{ <#l#> }|}]
  | Call { path = "not"; args = [ e1 ] } ->
    let e1 = print_exp e1 in
    [%pla {|!<#e1#>|}]
  | Call { path; args } ->
    let path = getReplacement path args e.t in
    let args = Pla.map_sep Pla.commaspace print_exp args in
    [%pla {|<#path#s>(<#args#>)|}]
  | UnOp (op, e) ->
    let e = print_exp e in
    let op = uoperator op in
    [%pla {|(<#op#> <#e#>)|}]
  | Op (op, e1, e2) ->
    let se1 = print_exp e1 in
    let se2 = print_exp e2 in
    (match Replacements.C.op_to_fun op e1.t e2.t e.t with
    | Some path -> [%pla {|<#path#s>(<#se1#>, <#se2#>)|}]
    | None ->
      let op = operator op in
      [%pla {|(<#se1#> <#op#> <#se2#>)|}])
  | If { cond; then_; else_ } ->
    let cond = print_exp cond in
    let then_ = print_exp then_ in
    let else_ = print_exp else_ in
    [%pla {|(<#cond#> ? <#then_#> : <#else_#>)|}]
  | Tuple l ->
    let l = Pla.map_sep Pla.commaspace print_exp l in
    [%pla {|(<#l#>)|}]
  | Member (e, m) ->
    let e = print_exp e in
    [%pla {|<#e#>.<#m#s>|}]
;;

let rec print_lexp e =
  match e.l with
  | LWild -> Pla.string "_"
  | LId s -> Pla.string s
  | LMember (e, m) ->
    let e = print_lexp e in
    [%pla {|<#e#>.<#m#s>|}]
  | LIndex (e, index) ->
    let e = print_lexp e in
    let index = print_exp index in
    [%pla {|<#e#>[static_cast<uint32_t>(<#index#>)]|}]
;;

let print_dexp (e : dexp) =
  match e.d with
  | DId (id, _) -> [%pla {|<#id#s>|}]
;;

(*| DId (id, Some dim) -> [%pla {|<#id#s>[<#dim#i>]|}]*)

let print_member (n, (t : type_)) =
  let t = print_type_ t in
  [%pla {|<#t#> <#n#s>;|}]
;;

let print_arg (n, (t : type_)) =
  match t with
  | Array (_, Array _) -> failwith "array of arrays are not implemented"
  | Array (dim, (Struct _ as sub)) ->
    let sub = print_type_ sub in
    [%pla {|<#sub#> (&<#n#s>)[<#dim#i>]|}]
  | Array (dim, sub) ->
    let sub = print_type_ sub in
    [%pla {|std::array<<#sub#>, <#dim#i>>& <#n#s>|}]
  | Struct { path; _ } -> [%pla {|<#path#s>& <#n#s>|}]
  | _ ->
    let t = print_type_ t in
    [%pla {|<#t#> <#n#s>|}]
;;

let print_decl (n, (t : type_)) =
  match t with
  | Array (_, Array _) -> failwith "array of arrays are not implemented"
  | Array (dim, (Struct _ as sub)) ->
    let sub = print_type_ sub in
    [%pla {|<#sub#> (&<#n#s>)[<#dim#i>]|}]
  | Array (dim, sub) ->
    let sub = print_type_ sub in
    [%pla {|std::array<<#sub#>, <#dim#i>> <#n#s>|}]
  | Struct { path; _ } -> [%pla {|<#path#s>& <#n#s>|}]
  | _ ->
    let t = print_type_ t in
    [%pla {|<#t#> <#n#s>|}]
;;

let arrayCopyFunction (t : type_) =
  match t with
  | Real -> "float_copy_array"
  | Int -> "int_copy_array"
  | Fixed -> "fix_copy_array"
  | Bool -> "bool_copy_array"
  | _ -> failwith "not a valid array copy"
;;

let rec parenthesize e =
  match e with
  | { e = Op (_, _, _); _ } -> print_exp e
  | _ -> Pla.parenthesize (print_exp e)

and print_stmt s =
  match s with
  (* declares and initializes a structure *)
  | StmtDecl (({ t = Struct { path; _ }; _ } as lhs), None) ->
    let t = print_type_ lhs.t in
    let lhs = print_dexp lhs in
    [%pla {|<#t#> <#lhs#>;<#><#path#s>_init(<#lhs#>);|}]
  | StmtDecl ({ d = DId (n, _); t; _ }, None) ->
    let t = print_decl (n, t) in
    [%pla {|<#t#>;|}]
  | StmtDecl ({ d = DId (n, _); t; _ }, Some rhs) ->
    let t = print_decl (n, t) in
    let rhs = print_exp rhs in
    [%pla {|<#t#> = <#rhs#>;|}]
  | StmtBind ({ l = LWild; _ }, rhs) ->
    let rhs = print_exp rhs in
    [%pla {|<#rhs#>;|}]
  (*| StmtBind (({ t = Array (n, t); _ } as lhs), ({ t = Array (_, _); _ } as rhs)) ->
    let lhs = print_lexp lhs in
    let rhs = print_exp rhs in
    let f = arrayCopyFunction t in
    [%pla {|<#f#s>(<#n#i>, <#lhs#>, <#rhs#>);|}]*)
  | StmtBind (lhs, rhs) ->
    let lhs = print_lexp lhs in
    let rhs = print_exp rhs in
    [%pla {|<#lhs#> = <#rhs#>;|}]
  | StmtReturn e ->
    let e = print_exp e in
    [%pla {|return <#e#>;|}]
  | StmtIf (cond, then_, None) ->
    let cond = parenthesize cond in
    let then_ = print_block then_ in
    [%pla {|if <#cond#> <#then_#>|}]
  | StmtIf (cond, then_, Some else_) ->
    let cond = parenthesize cond in
    let then_ = print_block then_ in
    let else_ = print_block else_ in
    [%pla {|if <#cond#> <#then_#><#>else <#else_#>|}]
  | StmtWhile (cond, stmt) ->
    let cond = parenthesize cond in
    let stmt = print_block stmt in
    [%pla {|while <#cond#> <#stmt#>|}]
  | StmtBlock stmts ->
    let stmt = Pla.map_sep_all Pla.newline print_stmt stmts in
    [%pla {|{<#stmt#+>}|}]

and print_block body =
  match body with
  | StmtBlock stmts ->
    let stmts = Pla.map_sep_all Pla.newline print_stmt stmts in
    [%pla {|{<#stmts#+>}|}]
  | _ ->
    let stmt = print_stmt body in
    [%pla {|{<#stmt#+><#>}|}]
;;

let print_function_def (def : function_def) =
  let name = Pla.string def.name in
  let args = Pla.map_sep Pla.commaspace print_arg def.args in
  let ret = print_type_ (snd def.t) in
  [%pla {|<#ret#> <#name#>(<#args#>)|}]
;;

let print_top_stmt (target : target) t =
  match t, target with
  | TopFunction (def, body), Header ->
    let inline = isSmall [ body ] in
    let def = print_function_def def in
    if inline
    then (
      let body = print_block body in
      [%pla {|static_inline <#def#> <#body#><#><#>|}])
    else [%pla {|<#def#>;<#><#>|}]
  | TopFunction (def, body), Implementation ->
    let inline = isSmall [ body ] in
    if inline
    then Pla.unit
    else (
      let def = print_function_def def in
      let body = print_block body in
      [%pla {|<#def#> <#body#><#><#>|}])
  | TopFunction _, _ -> Pla.unit
  | TopExternal (def, None), Header ->
    let def = print_function_def def in
    [%pla {|extern <#def#>;<#>|}]
  | TopExternal (def, Some link_name), Header ->
    let args = Pla.map_sep Pla.commaspace print_arg def.args in
    let ret = print_type_ (snd def.t) in
    [%pla {|extern <#ret#> <#link_name#s>(<#args#>);<#>|}]
  | TopExternal _, _ -> Pla.unit
  | TopType { path; members }, Header ->
    let members = Pla.map_sep [%pla {|<#>|}] print_member members in
    [%pla {|typedef struct <#path#s> {<#members#+><#>} <#path#s>;<#><#>|}]
  | TopType _, _ -> Pla.unit
  | TopDecl (lhs, rhs), Tables ->
    let t = print_type_ lhs.t in
    let lhs = print_dexp lhs in
    let rhs = print_exp rhs in
    [%pla {|static const <#t#> <#lhs#> = <#rhs#>;<#>|}]
  | TopDecl _, _ -> Pla.unit
;;

let print_prog target t = Pla.map_join (print_top_stmt target) t
let legend = Common.legend

let makeIfdef file =
  let def = CCString.replace ~sub:"." ~by:"_" (String.uppercase_ascii (Filename.basename file)) in
  [%pla {|
#ifndef <#def#s>
#define <#def#s>
|}], [%pla {|
#endif // <#def#s>
|}]
;;

let getTemplateCode (name : string option) (prefix : string) impl header (stmts : top_stmt list) =
  match name with
  | Some "pd" -> T_pd.generate prefix impl header stmts
  | None -> impl, header
  | Some name -> Util.Error.raiseErrorMsg ("Unknown template '" ^ name ^ "'")
;;

let getLibName output =
  match output with
  | None -> "MyLib"
  | Some name -> Filename.basename name
;;

let generate output template (stmts : top_stmt list) =
  let header = print_prog Header stmts in
  let impl = print_prog Implementation stmts in
  let tables = print_prog Tables stmts in
  let impl, header = getTemplateCode template (getLibName output) impl header stmts in
  let header_file = Common.setExt ".h" output in
  let tables_file = Common.setExt ".tables.h" output in
  let impl_file = Common.setExt ".cpp" output in
  let header_file_base = Filename.basename header_file in
  let tables_file_base = Filename.basename tables_file in
  let header =
    let ifdef, endif = makeIfdef header_file in
    [%pla {|<#legend#><#ifdef#><#>#include "vultin.h"<#>#include "<#tables_file_base#s>"<#><#><#header#><#endif#>|}]
  in
  let impl = [%pla {|<#legend#><#><#>#include "<#header_file_base#s>"<#><#><#impl#>|}] in
  let tables =
    let ifdef, endif = makeIfdef tables_file in
    [%pla {|<#legend#><#ifdef#><#tables#><#endif#>|}]
  in
  [ header, header_file; impl, impl_file; tables, tables_file ]
;;
