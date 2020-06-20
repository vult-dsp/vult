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

let runtime =
  {pla|
function ifExpressionValue(cond,then_,else_) if cond then return then_ else return else_ end end
function ifExpression(cond,then_,else_) if cond then return then_() else return else_() end end
function eps()              return 1e-18 end
function pi()               return 3.1415926535897932384 end
function random()           return math.random() end
function irandom()          return math.floor(math.random() * 4294967296) end
function clip(x,low,high)   if x > high then return high else if x < low then return low else return x end end end
function real(x)            return x end
function int(x)             local int_part,_ = math.modf(x) return int_part end
function sin(x)             return math.sin(x) end
function cos(x)             return math.cos(x) end
function abs(x)             return math.abs(x) end
function exp(x)             return math.exp(x) end
function floor(x)           return math.floor(x) end
function tan(x)             return math.tan(x) end
function tanh(x)            return math.tanh(x) end
function sqrt(x)            return x end
function set(a, i, v)       a[i+1]=v end
function get(a, i)          return a[i+1] end
function makeArray(size, v) local a = {} for i=1,size do a[i]=v end return a end

|pla}


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


let rec print_exp e =
  match e.e with
  | EUnit -> Pla.string ""
  | EBool v -> Pla.string (if v then "true" else "false")
  | EInt n -> Pla.int n
  | EReal n -> Pla.float n
  | EString s -> Pla.string_quoted s
  | EId id -> Pla.string id
  | EIndex { e; index } ->
      let e = print_exp e in
      let index = print_exp index in
      {pla|<#e#>[<#index#>]|pla}
  | EArray l -> Pla.wrap (Pla.string "{") (Pla.string "}") (Pla.map_sep Pla.commaspace print_exp l)
  | ECall { path; args } ->
      let args = Pla.map_sep Pla.commaspace print_exp args in
      {pla|<#path#s>(<#args#>)|pla}
  | EUnOp (op, e) ->
      let e = print_exp e in
      let op = uoperator op in
      {pla|(<#op#><#e#>)|pla}
  | EOp (op, e1, e2) ->
      let e1 = print_exp e1 in
      let op = operator op in
      let e2 = print_exp e2 in
      {pla|(<#e1#> <#op#> <#e2#>)|pla}
  | EIf { cond; then_; else_ } when isValue then_ && isValue else_ ->
      let cond = print_exp cond in
      let then_ = print_exp then_ in
      let else_ = print_exp else_ in
      {pla|ifExpressionValue(<#cond#>, <#then_#>, <#else_#>)|pla}
  | EIf { cond; then_; else_ } ->
      let cond = print_exp cond in
      let then_ = print_exp then_ in
      let else_ = print_exp else_ in
      {pla|ifExpression(<#cond#>, (function () return <#then_#> end), (function () return <#else_#> end))|pla}
  | ETuple l ->
      let l = Pla.map_sep Pla.commaspace print_exp l in
      {pla|(<#l#>)|pla}
  | EMember (e, m) ->
      let e = print_exp e in
      {pla|<#e#>.<#m#s>|pla}


let rec print_lexp e =
  match e.l with
  | LWild -> Pla.string "_wild"
  | LId s -> Pla.string s
  | LMember (e, m) ->
      let e = print_lexp e in
      {pla|<#e#>.<#m#s>|pla}
  | LIndex { e; index } ->
      let e = print_lexp e in
      let index = print_exp index in
      {pla|<#e#>[<#index#>]|pla}
  | LTuple l ->
      let l = Pla.map_sep Pla.commaspace print_lexp l in
      {pla|(<#l#>)|pla}


let rec print_dexp (e : dexp) =
  match e.d with
  | DWild -> {pla|_wild|pla}
  | DId (id, None) -> {pla|<#id#s>|pla}
  | DId (id, Some dim) -> {pla|<#id#s>[<#dim#i>]|pla}
  | DTuple l ->
      let l = Pla.map_sep Pla.commaspace print_dexp l in
      {pla|(<#l#>)|pla}


let rec print_stmt s =
  match s.s with
  | StmtDecl lhs ->
      let lhs = print_dexp lhs in
      {pla|local <#lhs#> = {};|pla}
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
      let then_ = print_stmt then_ in
      {pla|if <#e#> then<#then_#+><#>end|pla}
  | StmtIf (cond, then_, Some else_) ->
      let cond = print_exp cond in
      let then_ = print_stmt then_ in
      let else_ = print_stmt else_ in
      {pla|if <#cond#> then<#then_#+><#>else<#else_#+><#>end|pla}
  | StmtWhile (cond, stmt) ->
      let cond = print_exp cond in
      let stmt = print_stmt stmt in
      {pla|while <#cond#> do<#stmt#+><#>end|pla}
  | StmtBlock stmts ->
      let stmt = Pla.map_sep_all Pla.newline print_stmt stmts in
      {pla|do<#stmt#+>end|pla}


let print_arg (n, _, _) = {pla|<#n#s>|pla}

let print_function_def (def : function_def) =
  let name = def.name in
  let args = Pla.map_sep Pla.commaspace print_arg def.args in
  {pla|function <#name#s>(<#args#>)|pla}


let print_body body =
  match body.s with
  | StmtBlock stmts ->
      let stmts = Pla.map_sep_all Pla.newline print_stmt stmts in
      {pla|<#stmts#+>end|pla}
  | _ ->
      let stmt = print_stmt body in
      {pla|<#stmt#+><#>end|pla}


let print_top_stmt t =
  match t.top with
  | TopFunction (def, body) ->
      let def = print_function_def def in
      let body = print_body body in
      {pla|<#def#><#body#><#><#>|pla}
  | TopExternal _ -> Pla.unit
  | TopType _ -> Pla.unit


let print_prog t = Pla.map_join print_top_stmt t

let generate (stmts : top_stmt list) : string =
  let code = print_prog stmts in
  Pla.print {pla|<#runtime#><#code#>|pla}
