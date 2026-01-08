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

let runtime = {%pla|
# High-performance Vult Runtime for Julia with typed arrays

|}

let rec isValueOrIf (e : exp) =
  match e.e with
  | EUnit | EBool _ | EInt _ | EReal _ | EString _ | EId _ | EMember _ -> true
  | EUnOp (_, e) -> isValueOrIf e
  | EIf { then_; else_; _ } -> isValueOrIf then_ && isValueOrIf else_
  | _ -> false


let operator (op : operator) =
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
  | OpBxor -> Pla.string "⊻" (* Julia XOR operator *)
  | OpLsh -> Pla.string "<<"
  | OpRsh -> Pla.string ">>"
  | OpEq -> Pla.string "=="
  | OpNe -> Pla.string "!="
  | OpLt -> Pla.string "<"
  | OpLe -> Pla.string "<="
  | OpGt -> Pla.string ">"
  | OpGe -> Pla.string ">="


let uoperator (op : uoperator) =
  match op with
  | UOpNeg -> Pla.string "-"
  | UOpNot -> Pla.string "!"


let rec print_exp e =
  match e.e with
  | EEmptyValue -> Pla.string "nothing"
  | EUnit -> Pla.string ""
  | EBool v ->
    Pla.string
      (if v then
         "true"
       else
         "false")
  | EInt n ->
    let n_str = string_of_int n in
    {%pla|Int32(<#n_str#s>)|}
  | EReal n ->
    let n_str = Util.Vfloat.to_string n in
    {%pla|Float32(<#n_str#s>)|}
  | EFixed n ->
    let n_str = Util.Vfloat.to_string n in
    {%pla|Float32(<#n_str#s>)|}
  | EString s -> Pla.string_quoted s
  | EId id -> Pla.string id
  | EIndex { e; index = { e = EInt i; _ } } ->
    let e = print_exp e in
    let index = i + 1 in
    {%pla|@inbounds <#e#>[<#index#i>]|}
  | EIndex { e; index } ->
    let e = print_exp e in
    let index = print_exp index in
    {%pla|@inbounds <#e#>[<#index#> + 1]|}
  | EArray l ->
    let l = Pla.map_sep Pla.commaspace print_exp l in
    {%pla|[<#l#>]|}
  | ECall { path; args } -> (
    (* Special cases for functions that need argument transformation *)
    match path, args with
    | "eps", [] -> Pla.string "eps(Float32)"
    | "irandom", [] -> Pla.string "rand(Int32)"
    | "int", [ arg ] ->
      let arg = print_exp arg in
      {%pla|trunc(Int32, <#arg#>)|}
    | "int16", [ arg ] ->
      let arg = print_exp arg in
      {%pla|trunc(Int16, <#arg#>)|}
    | "real", [ arg ] ->
      let arg = print_exp arg in
      {%pla|Float32(<#arg#>)|}
    | "bool", [ arg ] ->
      let arg = print_exp arg in
      {%pla|Bool(<#arg#>)|}
    | "float_to_int", [ arg ] ->
      let arg = print_exp arg in
      {%pla|trunc(Int32, <#arg#>)|}
    | "int16_to_int", [ arg ] ->
      let arg = print_exp arg in
      {%pla|Int32(<#arg#>)|}
    | "int_to_int16", [ arg ] ->
      let arg = print_exp arg in
      {%pla|trunc(Int16, <#arg#>)|}
    | "float_to_int16", [ arg ] ->
      let arg = print_exp arg in
      {%pla|trunc(Int16, <#arg#>)|}
    | "int16_to_float", [ arg ] ->
      let arg = print_exp arg in
      {%pla|Float32(<#arg#>)|}
    | "initializeArray", [ value; size ] ->
      let value = print_exp value in
      let size = print_exp size in
      {%pla|fill(<#value#>, <#size#>)|}
    (* Bit shift operations - only for integers *)
    | "lshift", [ a; b ] ->
      let a = print_exp a in
      let b = print_exp b in
      {%pla|<#a#> << <#b#>|}
    | "rshift", [ a; b ] ->
      let a = print_exp a in
      let b = print_exp b in
      {%pla|<#a#> >> <#b#>|}
    (* Integer division for Vult semantics *)
    | "intDiv", [ a; b ] ->
      let a = print_exp a in
      let b = print_exp b in
      {%pla|div(trunc(Int32, <#a#>), trunc(Int32, <#b#>))|}
    (* List operations *)
    | "list_size", [ e1 ] ->
      let e1 = print_exp e1 in
      {%pla|Int32(length(<#e1#>))|}
    | "list_capacity", [ _ ] -> {%pla|typemax(Int32)|}
    | "list_append", [ l; v ] ->
      let l = print_exp l in
      let v = print_exp v in
      {%pla|push!(<#l#>, <#v#>)|}
    | "list_insert", [ l; i; v ] ->
      let l = print_exp l in
      let i = print_exp i in
      let v = print_exp v in
      {%pla|insert!(<#l#>, <#i#> + 1, <#v#>)|}
    | "list_remove", [ l; i ] ->
      let l = print_exp l in
      let i = print_exp i in
      {%pla|deleteat!(<#l#>, <#i#> + 1)|}
    | "list_clear", [ e1 ] ->
      let e1 = print_exp e1 in
      {%pla|empty!(<#e1#>)|}
    | "list_reserve", [ l; n ] ->
      let l = print_exp l in
      let n = print_exp n in
      {%pla|sizehint!(<#l#>, <#n#>)|}
    | "list_get", [ l; i ] ->
      let l = print_exp l in
      let i = print_exp i in
      (* Julia is 1-based *)
      {%pla|<#l#>[<#i#> + 1]|}
    | "list_set", [ l; i; v ] ->
      let l = print_exp l in
      let i = print_exp i in
      let v = print_exp v in
      (* Julia is 1-based *)
      {%pla|(<#l#>[<#i#> + 1] = <#v#>)|}
    | _ ->
      (* Default case - use replacements system or original name *)
      let args = Pla.map_sep Pla.commaspace print_exp args in
      {%pla|<#path#s>(<#args#>)|})
  | EUnOp (op, e) ->
    let e = print_exp e in
    let op = uoperator op in
    {%pla|(<#op#><#e#>)|}
  | EOp (op, e1, e2) -> (
    let se1 = print_exp e1 in
    let se2 = print_exp e2 in
    match op with
    (* Handle bit shifts for integers *)
    | OpLsh -> (
      match e2.e with
      | EInt n when n >= 0 && n <= 63 ->
        (* Small shifts: convert to multiplication for clarity *)
        let multiplier = 1 lsl n in
        {%pla|(<#se1#> * <#multiplier#i>)|}
      | _ ->
        (* Variable shifts *)
        {%pla|(<#se1#> << <#se2#>)|})
    | OpRsh -> (
      match e2.e with
      | EInt n when n >= 0 && n <= 63 ->
        (* Small shifts: use div for integer division *)
        let divisor = 1 lsl n in
        {%pla|div(<#se1#>, <#divisor#i>)|}
      | _ ->
        (* Variable shifts *)
        {%pla|(<#se1#> >> <#se2#>)|})
    | _ ->
      let op = operator op in
      {%pla|(<#se1#> <#op#> <#se2#>)|})
  | EIf { cond; then_; else_ } when isValueOrIf then_ && isValueOrIf else_ ->
    let cond = print_exp cond in
    let then_ = print_exp then_ in
    let else_ = print_exp else_ in
    {%pla|(<#cond#> ? <#then_#> : <#else_#>)|}
  | EIf { cond; then_; else_ } ->
    let cond = print_exp cond in
    let then_ = print_exp then_ in
    let else_ = print_exp else_ in
    {%pla|(<#cond#> ? <#then_#> : <#else_#>)|}
  | ETuple l ->
    let l = Pla.map_sep Pla.commaspace print_exp l in
    {%pla|(<#l#>,)|}
  | EMember (e, m) ->
    let e = print_exp e in
    {%pla|<#e#>.<#m#s>|}
  | ETMember (e, i) ->
    let e = print_exp e in
    let m = i + 1 in
    {%pla|<#e#>[<#m#i>]|}
  | ERecord { elems; _ } ->
    let printElem (n, v) =
      let v = print_exp v in
      {%pla|<#n#s> = <#v#>|}
    in
    let elems = Pla.map_sep Pla.commaspace printElem elems in
    {%pla|(; <#elems#> )|}


let rec print_lexp e =
  match e.l with
  | LWild -> Pla.string "_"
  | LId s -> Pla.string s
  | LMember (e, m) ->
    let e = print_lexp e in
    {%pla|<#e#>.<#m#s>|}
  | LIndex { e; index = { e = EInt i; _ } } ->
    let e = print_lexp e in
    let index = i + 1 in
    {%pla|@inbounds <#e#>[<#index#i>]|}
  | LIndex { e; index } ->
    let e = print_lexp e in
    let index = print_exp index in
    {%pla|@inbounds <#e#>[<#index#> + 1]|}
  | _ -> failwith "Julia:print_lexp LTuple"


let print_dexp (e : dexp) =
  match e.d with
  | DId (id, None) -> {%pla|<#id#s>|}
  | DId (id, Some dim) -> {%pla|<#id#s>[<#dim#i>]|}


let rec print_stmt (s : stmt) =
  match s.s with
  (* Needs allocation for structs *)
  (* Use constructor directly for _ctx *)
  | StmtDecl (({ d = DId ("_ctx", _); t = { t = TStruct { path; _ }; _ }; _ } as lhs), None) ->
    let lhs = print_dexp lhs in
    {%pla|<#lhs#> = <#path#s>()|}
  | StmtDecl (({ t = { t = TStruct { path; _ }; _ }; _ } as lhs), None) ->
    let lhs = print_dexp lhs in
    {%pla|<#lhs#> = <#path#s>_alloc()|}
  | StmtDecl (lhs, None) ->
    let lhs = print_dexp lhs in
    {%pla|<#lhs#> = nothing|}
  | StmtDecl (lhs, Some rhs) ->
    let lhs = print_dexp lhs in
    let rhs = print_exp rhs in
    {%pla|<#lhs#> = <#rhs#>|}
  | StmtBind ({ l = LWild; _ }, rhs) ->
    let rhs = print_exp rhs in
    {%pla|<#rhs#>|}
  | StmtBind (lhs, rhs) ->
    let lhs = print_lexp lhs in
    let rhs = print_exp rhs in
    {%pla|<#lhs#> = <#rhs#>|}
  | StmtReturn e ->
    let e = print_exp e in
    {%pla|return <#e#>|}
  | StmtIf (cond, then_, None) ->
    let e = print_exp cond in
    let then_ = print_stmt then_ in
    {%pla|if <#e#><#then_#+><#>end|}
  | StmtIf (cond, then_, Some else_) ->
    let cond = print_exp cond in
    let then_ = print_stmt then_ in
    let else_ = print_stmt else_ in
    {%pla|if <#cond#><#then_#+><#>else<#else_#+><#>end|}
  | StmtWhile (cond, stmt) ->
    let cond = print_exp cond in
    let stmt = print_stmt stmt in
    {%pla|while <#cond#><#stmt#+><#>end|}
  | StmtBlock stmts ->
    let stmt = Pla.map_sep_all Pla.newline print_stmt stmts in
    {%pla|begin<#stmt#+>end|}
  | StmtSwitch (e1, cases, default) -> (
    let if_ =
      CCList.fold_right
        (fun (e2, body) else_ ->
          let cond = C.eeq e1 e2 in
          Some (C.sif cond body else_))
        cases
        default
    in
    match if_ with
    | None -> Pla.unit
    | Some if_ -> print_stmt if_)


let print_arg ({ name; _ } : param) = {%pla|<#name#s>|}

let print_function_def (def : function_def) =
  let name = def.name in
  let args = Pla.map_sep Pla.commaspace print_arg def.args in
  {%pla|function <#name#s>(<#args#>)|}


let print_body body =
  match body.s with
  | StmtBlock stmts ->
    let stmts = Pla.map_sep_all Pla.newline print_stmt stmts in
    {%pla|<#stmts#+>end|}
  | _ ->
    let stmt = print_stmt body in
    {%pla|<#stmt#+><#>end|}


let print_top_stmt (args : Util.Args.args) t =
  match t.top with
  | TopFunction (def, body) ->
    let def = print_function_def def in
    let body = print_body body in
    {%pla|<#def#><#body#><#><#>|}
  | TopExternal _ -> Pla.unit
  | TopType { path; members } ->
    let printMember (n, (t : type_), _, _) =
      let jtype =
        match t.t with
        | TInt -> "Int32"
        | TInt16 -> "Int16"
        | TReal -> "Float32"
        | TBool -> "Bool"
        | TString -> "String"
        | TStruct { path; _ } -> path
        | TArray (_, elem_type) -> (
          match elem_type.t with
          | TInt -> "Vector{Int32}"
          | TInt16 -> "Vector{Int16}"
          | TReal -> "Vector{Float32}"
          | TBool -> "Vector{Bool}"
          | _ -> "Vector{Any}")
        | _ -> "Any"
      in
      {%pla|    <#n#s>::<#jtype#s>|}
    in
    let members_typed = Pla.map_sep Pla.newline printMember members in
    let getDefaultValue (_n, (t : type_), _, _) =
      match t.t with
      | TInt -> "Int32(0)"
      | TInt16 -> "Int16(0)"
      | TReal -> "Float32(0.0)"
      | TBool -> "false"
      | TString -> "\"\""
      | TStruct { path; _ } -> path ^ "()"
      | TArray (size_opt, elem_type) -> (
        match size_opt with
        | Some size ->
          let elem_default =
            match elem_type.t with
            | TInt -> "Int32(0)"
            | TInt16 -> "Int16(0)"
            | TReal -> "Float32(0.0)"
            | TBool -> "false"
            | _ -> "nothing"
          in
          "fill(" ^ elem_default ^ ", " ^ string_of_int size ^ ")"
        | None -> "[]")
      | _ -> "nothing"
    in
    let default_values = CCList.map getDefaultValue members |> String.concat ", " in
    {%pla|mutable struct <#path#s><#members_typed#+><#>end<#><#># Default constructor for <#path#s><#>function <#path#s>()<#>    return <#path#s>(<#default_values#s>)<#>end<#><#>|}
  | TopAlias _ -> Pla.unit
  | TopConstant (name, _, _, _, _) when args.test_mode -> {%pla|<#name#s> = nothing<#>|}
  | TopConstant (name, _, _, rhs, _) ->
    let rhs = print_exp rhs in
    {%pla|const <#name#s> = <#rhs#><#>|}


let print_prog args t = Pla.map_join (print_top_stmt args) t

let getTemplateCode (args : Util.Args.args) =
  match args.template with
  | None -> Pla.unit, Pla.unit
  | Some "performance" -> T_performance.generateJulia args
  | Some name -> Util.Error.raiseErrorMsg ("Unknown template '" ^ name ^ "'")


let generate (args : Util.Args.args) (stmts : top_stmt list) =
  let file = Common.setExt ".jl" args.output in
  let code = print_prog args stmts in
  let pre, post = getTemplateCode args in
  [ {%pla|<#runtime#><#pre#><#code#><#post#>|}, file ]
