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
      | StmtBind _ :: t -> loop (size + 1) t
      | StmtSwitch (_, cases, _) :: t ->
        let n = List.fold_left (fun n (_, body) -> n + loop 0 [ body ]) 0 cases in
        loop (size + 1 + n) t)
  in
  let size = loop 0 stmts in
  size <= threshold


let rec print_type_ (t : type_) =
  match t with
  | Void _ -> Pla.string "void"
  | Int -> Pla.string "int32_t"
  | Real -> Pla.string "float"
  | Bool -> Pla.string "bool"
  | String -> Pla.string "std::string"
  | Fixed -> Pla.string "fix16_t"
  | Tuple l ->
    let l = Pla.map_sep Pla.commaspace print_type_ l in
    [%pla {|std::tuple<<#l#>>|}]
  | Array (Some dim, t) ->
    let t = print_type_ t in
    [%pla {|std::array<<#t#>, <#dim#i>>|}]
  | Array (None, t) ->
    let t = print_type_ t in
    [%pla {|std::array<<#t#>>|}]
  | Struct { path; _ } -> [%pla {|<#path#s>|}]


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


let level op =
  match op with
  | Mul -> 80
  | Div -> 85
  | Mod -> 80
  | Add -> 75
  | Sub -> 70
  | Lsh -> 60
  | Rsh -> 60
  | Lt -> 50
  | Le -> 50
  | Gt -> 50
  | Ge -> 50
  | Eq -> -1
  | Ne -> -1
  | Band -> -1
  | Bxor -> -1
  | Bor -> -1
  | Land -> -1
  | Lor -> -1


let uoperator op =
  match op with
  | Neg -> Pla.string "-"
  | Not -> Pla.string "not"


let getReplacement name (args : exp list) ret =
  let args_t = List.map (fun (e : exp) -> e.t) args in
  match Replacements.C.fun_to_fun name args_t ret with
  | Some path -> path
  | None -> name


let rec print_exp prec (e : exp) =
  match e.e with
  | Unit -> Pla.string ""
  | Bool v -> Pla.string (if v then "true" else "false")
  | Int n -> [%pla {|<#n#i>|}]
  | Real n ->
    let n = Util.Vfloat.adapt n in
    [%pla {|<#n#f>f|}]
  | Fixed n ->
    let n = Common.toFixed n in
    [%pla {|<#n#s>|}]
  | String s -> Pla.string_quoted s
  | Id id -> Pla.string id
  | Index { e; index } ->
    let e = print_exp prec e in
    let index = print_exp prec index in
    [%pla {|<#e#>[static_cast<uint32_t>(<#index#>)]|}]
  | Array l ->
    let rows = Common.splitArray 100 l in
    let l = Pla.map_sep [%pla {|,<#>|}] (Pla.map_sep Pla.commaspace (print_exp prec)) rows in
    [%pla {|{ <#l#> }|}]
  | Call { path = "size"; args = [ e1 ] } ->
    let e1 = print_exp prec e1 in
    [%pla {|<#e1#>.size()|}]
  | Call { path = "length"; args = [ e1 ] } ->
    let e1 = print_exp prec e1 in
    [%pla {|static_cast<int32_t>(<#e1#>.size())|}]
  | Call { path = "not"; args = [ e1 ] } ->
    let e1 = print_exp prec e1 in
    [%pla {|!(<#e1#>)|}]
  | Call { path; args } ->
    let path = getReplacement path args e.t in
    let args = Pla.map_sep Pla.commaspace (print_exp prec) args in
    [%pla {|<#path#s>(<#args#>)|}]
  | UnOp (op, e) ->
    let e = print_exp 0 e in
    let op = uoperator op in
    [%pla {|(<#op#> <#e#>)|}]
  | Op (op, e1, e2) -> (
    let current = level op in
    let se1 = print_exp current e1 in
    let se2 = print_exp current e2 in
    match Replacements.C.op_to_fun op e1.t e2.t e.t with
    | Some path -> [%pla {|<#path#s>(<#se1#>, <#se2#>)|}]
    | None ->
      let op = operator op in
      if (current >= prec && current <> -1) || prec = 0 then
        [%pla {|<#se1#> <#op#> <#se2#>|}]
      else
        [%pla {|(<#se1#> <#op#> <#se2#>)|}])
  | If { cond; then_; else_ } ->
    let cond = print_exp prec cond in
    let then_ = print_exp prec then_ in
    let else_ = print_exp prec else_ in
    [%pla {|(<#cond#> ? <#then_#> : <#else_#>)|}]
  | Tuple l ->
    let l = Pla.map_sep Pla.commaspace (print_exp prec) l in
    [%pla {|std::make_tuple(<#l#>)|}]
  | Member (e, m) ->
    let e = print_exp prec e in
    [%pla {|<#e#>.<#m#s>|}]
  | TMember (e, m) ->
    let e = print_exp prec e in
    [%pla {|std::get<<#m#i>>(<#e#>)|}]


let rec print_lexp e =
  match e.l with
  | LWild -> Pla.string "_"
  | LId s -> Pla.string s
  | LMember (e, m) ->
    let e = print_lexp e in
    [%pla {|<#e#>.<#m#s>|}]
  | LIndex (e, index) ->
    let e = print_lexp e in
    let index = print_exp 0 index in
    [%pla {|<#e#>[static_cast<uint32_t>(<#index#>)]|}]


let print_dexp (e : dexp) =
  match e.d with
  | DId (id, _) -> [%pla {|<#id#s>|}]


let print_member (n, (t : type_)) =
  let t = print_type_ t in
  [%pla {|<#t#> <#n#s>;|}]


let print_arg i (n, (t : type_)) =
  match t with
  | Array (_, Array _) -> failwith "array of arrays are not implemented"
  | Array (Some dim, (Struct _ as sub)) ->
    let sub = print_type_ sub in
    [%pla {|std::array<<#sub#>, <#dim#i>>& <#n#s>|}]
  | Array (Some dim, sub) ->
    let sub = print_type_ sub in
    [%pla {|std::array<<#sub#>, <#dim#i>>& <#n#s>|}]
  | Array (None, sub) ->
    let sub = print_type_ sub in
    [%pla {|std::array<<#sub#>, SIZE_<#i#i>>& <#n#s>|}]
  | Struct { path; _ } -> [%pla {|<#path#s>& <#n#s>|}]
  | _ ->
    let t = print_type_ t in
    [%pla {|<#t#> <#n#s>|}]


let print_decl (n, (t : type_)) =
  match t with
  | Array (_, Array _) -> failwith "array of arrays are not implemented"
  | Array (Some dim, (Struct _ as sub)) ->
    let sub = print_type_ sub in
    [%pla {|std::array<<#sub#>, <#dim#i>> &<#n#s>|}]
  | Array (Some dim, sub) ->
    let sub = print_type_ sub in
    [%pla {|std::array<<#sub#>, <#dim#i>> &<#n#s>|}]
  | Array (None, _) -> failwith "Cpp.print_decl: array without dimensions"
  | Struct { path; _ } -> [%pla {|<#path#s> &<#n#s>|}]
  | _ ->
    let t = print_type_ t in
    [%pla {|<#t#> <#n#s>|}]


let print_decl_alloc (n, (t : type_)) =
  match t with
  | Array (_, Array _) -> failwith "array of arrays are not implemented"
  | Array (Some dim, (Struct _ as sub)) ->
    let sub = print_type_ sub in
    [%pla {|std::array<<#sub#>, <#dim#i>> <#n#s>|}]
  | Array (Some dim, sub) ->
    let sub = print_type_ sub in
    [%pla {|std::array<<#sub#>, <#dim#i>> <#n#s>|}]
  | Array (None, _) -> failwith "Cpp.print_decl_alloc: array without dimensions"
  | Struct { path; _ } -> [%pla {|<#path#s> <#n#s>|}]
  | _ ->
    let t = print_type_ t in
    [%pla {|<#t#> <#n#s>|}]


let arrayCopyFunction (t : type_) =
  match t with
  | Real -> "float_copy_array"
  | Int -> "int_copy_array"
  | Fixed -> "fix_copy_array"
  | Bool -> "bool_copy_array"
  | _ -> failwith "not a valid array copy"


let rec print_stmt s =
  match s with
  (* declares and initializes a structure *)
  | StmtDecl (({ t = Struct { path; _ }; _ } as lhs), None) ->
    let t = print_type_ lhs.t in
    let lhs = print_dexp lhs in
    [%pla {|<#t#> <#lhs#>;<#><#path#s>_init(<#lhs#>);|}]
  | StmtDecl ({ d = DId (n, _); t; _ }, None) ->
    let t = print_decl_alloc (n, t) in
    [%pla {|<#t#>;|}]
  (* Special case when initializing an array. Here we declare the variable and not a reference *)
  | StmtDecl ({ d = DId (n, _); t; _ }, Some ({ e = Array _; _ } as rhs)) ->
    let t = print_decl_alloc (n, t) in
    let rhs = print_exp 0 rhs in
    [%pla {|<#t#> = <#rhs#>;|}]
  (* Special case for structures. When the structure is the result of a function call we need to declare it *)
  | StmtDecl ({ d = DId (n, _); t = Struct _ as t; _ }, Some ({ e = Call _; _ } as rhs)) ->
    let t = print_decl_alloc (n, t) in
    let rhs = print_exp 0 rhs in
    [%pla {|<#t#> = <#rhs#>;|}]
  (* For other case we use refences in the case of structures and arrays *)
  | StmtDecl ({ d = DId (n, _); t; _ }, Some rhs) ->
    let t = print_decl (n, t) in
    let rhs = print_exp 0 rhs in
    [%pla {|<#t#> = <#rhs#>;|}]
  | StmtBind ({ l = LWild; _ }, rhs) ->
    let rhs = print_exp 0 rhs in
    [%pla {|<#rhs#>;|}]
  | StmtBind (lhs, rhs) ->
    let lhs = print_lexp lhs in
    let rhs = print_exp 0 rhs in
    [%pla {|<#lhs#> = <#rhs#>;|}]
  | StmtReturn e ->
    let e = print_exp 0 e in
    [%pla {|return <#e#>;|}]
  | StmtIf (cond, then_, None) ->
    let cond = print_exp 0 cond in
    let then_ = print_block then_ in
    [%pla {|if (<#cond#>) <#then_#>|}]
  | StmtIf (cond, then_, Some else_) ->
    let cond = print_exp 0 cond in
    let then_ = print_block then_ in
    let else_ = print_block else_ in
    [%pla {|if (<#cond#>) <#then_#><#>else <#else_#>|}]
  | StmtWhile (cond, stmt) ->
    let cond = print_exp 0 cond in
    let stmt = print_block stmt in
    [%pla {|while (<#cond#>) <#stmt#>|}]
  | StmtBlock stmts ->
    let stmt = Pla.map_sep_all Pla.newline print_stmt stmts in
    [%pla {|{<#stmt#+>}|}]
  | StmtSwitch (e, cases, default_case) ->
    let e = print_exp 0 e in
    let break = Pla.string "break;" in
    let cases =
      Pla.map_sep_all
        Pla.newline
        (fun (e1, body) ->
          let e1 = print_exp 0 e1 in
          let body = print_stmt body in
          [%pla {|case <#e1#>:<#body#+><#break#+>|}])
        cases
    in
    let default_case =
      Option.map
        (fun body ->
          let body = print_stmt body in
          [%pla {|default:<#body#+><#break#+>|}])
        default_case
    in
    let default_case = Option.value default_case ~default:Pla.unit in
    [%pla {| switch (<#e#>) {<#cases#+><#default_case#><#>}|}]


and print_block body =
  match body with
  | StmtBlock stmts ->
    let stmts = Pla.map_sep_all Pla.newline print_stmt stmts in
    [%pla {|{<#stmts#+>}|}]
  | _ ->
    let stmt = print_stmt body in
    [%pla {|{<#stmt#+><#>}|}]


let print_function_def (def : function_def) =
  let name = Pla.string def.name in
  let args = List.mapi print_arg def.args |> Pla.join_sep Pla.commaspace in
  let template_args =
    def.args
    |> List.mapi (fun i (arg : param) ->
      match arg with
      | _, Array (None, _) -> Some ("std::size_t SIZE_" ^ string_of_int i)
      | _ -> None)
    |> List.filter_map (fun v -> v)
  in
  let template_decl =
    match template_args with
    | [] -> Pla.unit
    | _ ->
      let args = Pla.map_sep Pla.commaspace Pla.string template_args in
      {%pla|template<<#args#>><#>|}
  in
  let ret = print_type_ (snd def.t) in
  template_decl, [%pla {|<#ret#> <#name#>(<#args#>)|}]


let print_top_stmt (target : target) t =
  match t.top, target with
  | TopFunction (def, body), Header ->
    let inline = isSmall [ body ] in
    let template, def = print_function_def def in
    if inline then (
      let body = print_block body in
      [%pla {|<#template#>static_inline <#def#> <#body#><#><#>|}])
    else
      [%pla {|<#def#>;<#><#>|}]
  | TopFunction (def, body), Implementation ->
    let inline = isSmall [ body ] in
    if inline then
      Pla.unit
    else (
      let template, def = print_function_def def in
      let body = print_block body in
      [%pla {|<#template#><#def#> <#body#><#><#>|}])
  | TopFunction _, _ -> Pla.unit
  | TopExternal (def, None), Header ->
    let template, def = print_function_def def in
    [%pla {|<#template#>extern <#def#>;<#>|}]
  | TopExternal (def, Some link_name), Header ->
    let args = List.mapi print_arg def.args |> Pla.join_sep Pla.commaspace in
    let ret = print_type_ (snd def.t) in
    [%pla {|extern <#ret#> <#link_name#s>(<#args#>);<#>|}]
  | TopExternal _, _ -> Pla.unit
  | TopType { path; members }, Header ->
    let members = Pla.map_sep [%pla {|<#>|}] print_member members in
    [%pla {|typedef struct <#path#s> {<#members#+><#>} <#path#s>;<#><#>|}]
  | TopAlias (path, alias_of), Header -> [%pla {|typedef struct <#alias_of#s> <#path#s>;<#><#>|}]
  | TopType _, _ -> Pla.unit
  | TopAlias _, _ -> Pla.unit
  | TopDecl (lhs, rhs), Tables ->
    let t = print_type_ lhs.t in
    let lhs = print_dexp lhs in
    let rhs = print_exp 0 rhs in
    [%pla {|static const <#t#> <#lhs#> = <#rhs#+>;<#>|}]
  | TopDecl _, _ -> Pla.unit


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


let getTemplateCode (name : string option) (prefix : string) impl header (stmts : top_stmt list) =
  match name with
  | Some "pd" -> T_pd.generate prefix impl header stmts
  | None -> impl, header
  | Some name -> Util.Error.raiseErrorMsg ("Unknown template '" ^ name ^ "'")


let getLibName output =
  match output with
  | None -> "MyLib"
  | Some name -> Filename.basename name


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
