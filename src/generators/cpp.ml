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

open Core.Prog

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
      | { s = StmtDecl (_, None); _ } :: t -> loop size t
      | { s = StmtDecl (_, Some _); _ } :: t -> loop (size + 1) t
      | { s = StmtReturn _; _ } :: t -> loop size t
      | { s = StmtBlock inner; _ } :: t ->
        let size = loop size inner in
        loop size t
      | { s = StmtIf (_, then_, None); _ } :: t ->
        let size = loop (size + 1) [ then_ ] in
        loop size t
      | { s = StmtIf (_, then_, Some else_); _ } :: t ->
        let size = loop (size + 1) [ then_ ] in
        let size = loop size [ else_ ] in
        loop size t
      | { s = StmtWhile (_, body); _ } :: t ->
        let size = loop (size + 1) [ body ] in
        loop size t
      | { s = StmtBind _; _ } :: t -> loop (size + 1) t
      | { s = StmtSwitch (_, cases, _); _ } :: t ->
        let n = List.fold_left (fun n (_, body) -> n + loop 0 [ body ]) 0 cases in
        loop (size + 1 + n) t)
  in
  let size = loop 0 stmts in
  size <= threshold


let rec print_type_ (t : type_) =
  match t.t with
  | TVoid _ -> Pla.string "void"
  | TInt -> Pla.string "int32_t"
  | TReal -> Pla.string "float"
  | TBool -> Pla.string "bool"
  | TString -> Pla.string "std::string"
  | TFix16 -> Pla.string "fix16_t"
  | TTuple l ->
    let l = Pla.map_sep Pla.commaspace print_type_ l in
    [%pla {|std::tuple<<#l#>>|}]
  | TArray (Some dim, t) ->
    let t = print_type_ t in
    [%pla {|std::array<<#t#>, <#dim#i>>|}]
  | TArray (None, t) ->
    let t = print_type_ t in
    [%pla {|std::array<<#t#>>|}]
  | TStruct { path; _ } -> [%pla {|<#path#s>|}]


let operator (op : Core.Prog.operator) =
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
  | OpNe -> Pla.string "!="
  | OpLt -> Pla.string "<"
  | OpLe -> Pla.string "<="
  | OpGt -> Pla.string ">"
  | OpGe -> Pla.string ">="


let level (op : Core.Prog.operator) =
  match op with
  | OpMul -> 80
  | OpDiv -> 85
  | OpMod -> 80
  | OpAdd -> 75
  | OpSub -> 70
  | OpLsh -> 60
  | OpRsh -> 60
  | OpLt -> 50
  | OpLe -> 50
  | OpGt -> 50
  | OpGe -> 50
  | OpEq -> -1
  | OpNe -> -1
  | OpBand -> -1
  | OpBxor -> -1
  | OpBor -> -1
  | OpLand -> -1
  | OpLor -> -1


let uoperator (op : Core.Prog.uoperator) =
  match op with
  | UOpNeg -> Pla.string "-"
  | UOpNot -> Pla.string "not"


let rec print_exp prec (e : exp) =
  match e.e with
  | EUnit -> Pla.string ""
  | EBool v -> Pla.string (if v then "true" else "false")
  | EInt n -> [%pla {|<#n#i>|}]
  | EReal n ->
    let n = Util.Vfloat.adapt n in
    [%pla {|<#n#f>f|}]
  | EFixed n ->
    let n = Common.toFixed n in
    [%pla {|<#n#s>|}]
  | EString s -> Pla.string_quoted s
  | EId id -> Pla.string id
  | EIndex { e; index } ->
    let e = print_exp prec e in
    let index = print_exp prec index in
    [%pla {|<#e#>[static_cast<uint32_t>(<#index#>)]|}]
  | EArray l ->
    let rows = Common.splitArray 100 l in
    let l = Pla.map_sep [%pla {|,<#>|}] (Pla.map_sep Pla.commaspace (print_exp prec)) rows in
    [%pla {|{ <#l#> }|}]
  | ECall { path = "size"; args = [ e1 ] } ->
    let e1 = print_exp prec e1 in
    [%pla {|<#e1#>.size()|}]
  | ECall { path = "length"; args = [ e1 ] } ->
    let e1 = print_exp prec e1 in
    [%pla {|static_cast<int32_t>(<#e1#>.size())|}]
  | ECall { path = "not"; args = [ e1 ] } ->
    let e1 = print_exp prec e1 in
    [%pla {|!(<#e1#>)|}]
  | ECall { path; args } ->
    let args = Pla.map_sep Pla.commaspace (print_exp prec) args in
    [%pla {|<#path#s>(<#args#>)|}]
  | EUnOp (op, e) ->
    let e = print_exp 0 e in
    let op = uoperator op in
    [%pla {|(<#op#> <#e#>)|}]
  | EOp (op, e1, e2) ->
    let current = level op in
    let se1 = print_exp current e1 in
    let se2 = print_exp current e2 in
    let op = operator op in
    if (current >= prec && current <> -1) || prec = 0 then
      [%pla {|<#se1#> <#op#> <#se2#>|}]
    else
      [%pla {|(<#se1#> <#op#> <#se2#>)|}]
  | EIf { cond; then_; else_ } ->
    let cond = print_exp prec cond in
    let then_ = print_exp prec then_ in
    let else_ = print_exp prec else_ in
    [%pla {|(<#cond#> ? <#then_#> : <#else_#>)|}]
  | ETuple l ->
    let l = Pla.map_sep Pla.commaspace (print_exp prec) l in
    [%pla {|std::make_tuple(<#l#>)|}]
  | EMember (e, m) ->
    let e = print_exp prec e in
    [%pla {|<#e#>.<#m#s>|}]
  | ETMember (e, m) ->
    let e = print_exp prec e in
    [%pla {|std::get<<#m#i>>(<#e#>)|}]


let rec print_lexp e =
  match e.l with
  | LWild -> Pla.string "_"
  | LId s -> Pla.string s
  | LMember (e, m) ->
    let e = print_lexp e in
    [%pla {|<#e#>.<#m#s>|}]
  | LIndex { e; index } ->
    let e = print_lexp e in
    let index = print_exp 0 index in
    [%pla {|<#e#>[static_cast<uint32_t>(<#index#>)]|}]
  | _ -> failwith "print_lexp: LTuple not implemented"


let print_dexp (e : dexp) =
  match e.d with
  | DId (id, _) -> [%pla {|<#id#s>|}]


let print_member (n, (t : type_), _, _) =
  let t = print_type_ t in
  [%pla {|<#t#> <#n#s>;|}]


let print_arg i (n, (t : type_), _) =
  match t.t with
  | TArray (_, { t = TArray _; _ }) -> failwith "array of arrays are not implemented"
  | TArray (Some dim, ({ t = TStruct _; _ } as sub)) ->
    let sub = print_type_ sub in
    [%pla {|std::array<<#sub#>, <#dim#i>>& <#n#s>|}]
  | TArray (Some dim, sub) ->
    let sub = print_type_ sub in
    [%pla {|std::array<<#sub#>, <#dim#i>>& <#n#s>|}]
  | TArray (None, sub) ->
    let sub = print_type_ sub in
    [%pla {|std::array<<#sub#>, SIZE_<#i#i>>& <#n#s>|}]
  | TStruct { path; _ } -> [%pla {|<#path#s>& <#n#s>|}]
  | _ ->
    let t = print_type_ t in
    [%pla {|<#t#> <#n#s>|}]


let print_decl (n, (t : type_)) =
  match t.t with
  | TArray (_, { t = TArray _; _ }) -> failwith "array of arrays are not implemented"
  | TArray (Some dim, ({ t = TStruct _; _ } as sub)) ->
    let sub = print_type_ sub in
    [%pla {|std::array<<#sub#>, <#dim#i>> &<#n#s>|}]
  | TArray (Some dim, sub) ->
    let sub = print_type_ sub in
    [%pla {|std::array<<#sub#>, <#dim#i>> &<#n#s>|}]
  | TArray (None, _) -> failwith "Cpp.print_decl: array without dimensions"
  | TStruct { path; _ } -> [%pla {|<#path#s> &<#n#s>|}]
  | _ ->
    let t = print_type_ t in
    [%pla {|<#t#> <#n#s>|}]


let print_decl_alloc (n, (t : type_)) =
  match t.t with
  | TArray (_, { t = TArray _; _ }) -> failwith "array of arrays are not implemented"
  | TArray (Some dim, ({ t = TStruct _; _ } as sub)) ->
    let sub = print_type_ sub in
    [%pla {|std::array<<#sub#>, <#dim#i>> <#n#s>|}]
  | TArray (Some dim, sub) ->
    let sub = print_type_ sub in
    [%pla {|std::array<<#sub#>, <#dim#i>> <#n#s>|}]
  | TArray (None, _) -> failwith "Cpp.print_decl_alloc: array without dimensions"
  | TStruct { path; _ } -> [%pla {|<#path#s> <#n#s>|}]
  | _ ->
    let t = print_type_ t in
    [%pla {|<#t#> <#n#s>|}]


let arrayCopyFunction (t : type_) =
  match t.t with
  | TReal -> "float_copy_array"
  | TInt -> "int_copy_array"
  | TFix16 -> "fix_copy_array"
  | TBool -> "bool_copy_array"
  | _ -> failwith "not a valid array copy"


let rec print_stmt s =
  match s.s with
  (* declares and initializes a structure *)
  | StmtDecl (({ t = { t = TStruct { path; _ }; _ }; _ } as lhs), None) ->
    let t = print_type_ lhs.t in
    let lhs = print_dexp lhs in
    [%pla {|<#t#> <#lhs#>;<#><#path#s>_init(<#lhs#>);|}]
  | StmtDecl ({ d = DId (n, _); t; _ }, None) ->
    let t = print_decl_alloc (n, t) in
    [%pla {|<#t#>;|}]
  (* Special case when initializing an array. Here we declare the variable and not a reference *)
  | StmtDecl ({ d = DId (n, _); t; _ }, Some ({ e = EArray _; _ } as rhs)) ->
    let t = print_decl_alloc (n, t) in
    let rhs = print_exp 0 rhs in
    [%pla {|<#t#> = <#rhs#>;|}]
  (* Special case for structures. When the structure is the result of a function call we need to declare it *)
  | StmtDecl ({ d = DId (n, _); t = { t = TStruct _; _ } as t; _ }, Some ({ e = ECall _; _ } as rhs)) ->
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
  | { s = StmtBlock stmts; _ } ->
    let stmts = Pla.map_sep_all Pla.newline print_stmt stmts in
    [%pla {|{<#stmts#+>}|}]
  | _ ->
    let stmt = print_stmt body in
    [%pla {|{<#stmt#+><#>}|}]


let isTemplate (args : param list) =
  List.exists
    (fun (a : param) ->
      match a with
      | _, { t = TArray (None, _); _ }, _ -> true
      | _ -> false)
    args


let print_function_def (def : function_def) =
  let name = Pla.string def.name in
  let args = List.mapi print_arg def.args |> Pla.join_sep Pla.commaspace in
  let template_args =
    def.args
    |> List.mapi (fun i (arg : param) ->
      match arg with
      | _, { t = TArray (None, _); _ }, _ -> Some ("std::size_t SIZE_" ^ string_of_int i)
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


let print_top_stmt ~allow_inline (target : target) t =
  match t.top, target with
  | TopFunction (def, body), Header ->
    let inline = (allow_inline && isSmall [ body ]) || isTemplate def.args in
    let template, def = print_function_def def in
    if inline then (
      let body = print_block body in
      [%pla {|<#template#>static_inline <#def#> <#body#><#><#>|}])
    else
      [%pla {|<#def#>;<#><#>|}]
  | TopFunction (def, body), Implementation ->
    let inline = (allow_inline && isSmall [ body ]) || isTemplate def.args in
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
  | TopAlias { path; alias_of }, Header -> [%pla {|typedef struct <#alias_of#s> <#path#s>;<#><#>|}]
  | TopType _, _ -> Pla.unit
  | TopAlias _, _ -> Pla.unit
  | TopDecl (lhs, rhs), Tables ->
    let t = print_type_ lhs.t in
    let lhs = print_dexp lhs in
    let rhs = print_exp 0 rhs in
    [%pla {|static const <#t#> <#lhs#> = <#rhs#+>;<#>|}]
  | TopDecl _, _ -> Pla.unit


let print_prog ~allow_inline target t = Pla.map_join (print_top_stmt ~allow_inline target) t
let legend = Common.legend

let makeIfdef file =
  let def = CCString.replace ~sub:"." ~by:"_" (String.uppercase_ascii (Filename.basename file)) in
  [%pla {|
#ifndef <#def#s>
#define <#def#s>
|}], [%pla {|
#endif // <#def#s>
|}]


let getTemplateCode (name : string option) (prefix : string) (stmts : top_stmt list) =
  match name with
  | Some "pd" -> T_pd.generate prefix stmts
  | None -> (Pla.unit, Pla.unit), (Pla.unit, Pla.unit)
  | Some name -> Util.Error.raiseErrorMsg ("Unknown template '" ^ name ^ "'")


let getLibName output =
  match output with
  | None -> "MyLib"
  | Some name -> Filename.basename name


let generateIncludeList stmts = Pla.map_sep_all Pla.newline (fun (file, _) -> {%pla|#include "<#file#s>.h"|}) stmts

let generateSplit file_deps output template (stmts : top_stmt list) =
  let dir = CCOption.map_or ~default:"" (fun file -> Filename.dirname file) output in
  let main_header_file = Common.setExt ".h" output in
  let impl_file = Common.setExt ".cpp" output in
  let generateClassic (mname, stmts) =
    let output = Some (Filename.concat dir mname) in
    let allow_inline = false in
    let header = print_prog ~allow_inline Header stmts in
    let impl = print_prog ~allow_inline Implementation stmts in
    let tables = print_prog ~allow_inline Tables stmts in
    let header_file = Common.setExt ".h" output in
    let header_file_base = Filename.basename header_file in
    let impl_file = Common.setExt ".cpp" output in
    let dependencies =
      Hashtbl.find file_deps mname |> Pla.map_sep_all Pla.newline (fun inc -> {%pla|#include "<#inc#s>.h"|})
    in
    let header =
      let ifdef, endif = makeIfdef header_file in
      [%pla {|<#legend#><#ifdef#><#>#include "vultin.h"<#><#dependencies#><#><#><#header#><#endif#>|}]
    in
    let impl = [%pla {|<#legend#><#><#>#include "<#header_file_base#s>"<#><#><#tables#><#><#><#impl#>|}] in
    [ header, header_file; impl, impl_file ]
  in
  let (timpl_start, timpl_end), (theader_start, theader_end) = getTemplateCode template (getLibName output) stmts in
  let stmts = Common.splitByFile stmts in
  let files = CCList.flat_map generateClassic stmts in
  let include_list = generateIncludeList stmts in
  (Pla.join [ timpl_start; timpl_end ], impl_file)
  :: (Pla.join [ theader_start; include_list; theader_end ], main_header_file)
  :: files


let generateSingle output template (stmts : top_stmt list) =
  let allow_inline = true in
  let header = print_prog ~allow_inline Header stmts in
  let impl = print_prog ~allow_inline Implementation stmts in
  let tables = print_prog ~allow_inline Tables stmts in
  let (timpl_start, timpl_end), (theader_start, theader_end) = getTemplateCode template (getLibName output) stmts in
  let impl = Pla.join [ timpl_start; impl; timpl_end ] in
  let header = Pla.join [ theader_start; header; theader_end ] in
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


let generate file_deps split output template (stmts : top_stmt list) =
  if split then
    generateSplit file_deps output template stmts
  else
    generateSingle output template stmts
