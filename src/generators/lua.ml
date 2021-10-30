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

let runtime =
  [%pla
    {|
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

|}]


let rec isValueOrIf (e : exp) =
  match e.e with
  | Unit
   |Bool _
   |Int _
   |Real _
   |String _
   |Id _
   |Member _ ->
      true
  | UnOp (_, e) -> isValueOrIf e
  | If { then_; else_; _ } -> isValueOrIf then_ && isValueOrIf else_
  | _ -> false


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
  | Ne -> Pla.string "<>"
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
  | Bool v -> Pla.string (if v then "true" else "false")
  | Int n -> Pla.int n
  | Real n -> Pla.float n
  | String s -> Pla.string_quoted s
  | Id id -> Pla.string id
  | Index { e; index } ->
      let e = print_exp e in
      let index = print_exp index in
      [%pla {|<#e#>[<#index#>]|}]
  | Array l -> Pla.wrap (Pla.string "{") (Pla.string "}") (Pla.map_sep Pla.commaspace print_exp l)
  | Call { path; args } ->
      let args = Pla.map_sep Pla.commaspace print_exp args in
      [%pla {|<#path#s>(<#args#>)|}]
  | UnOp (op, e) ->
      let e = print_exp e in
      let op = uoperator op in
      [%pla {|(<#op#><#e#>)|}]
  | Op (op, e1, e2) ->
      let e1 = print_exp e1 in
      let op = operator op in
      let e2 = print_exp e2 in
      [%pla {|(<#e1#> <#op#> <#e2#>)|}]
  | If { cond; then_; else_ } when isValueOrIf then_ && isValueOrIf else_ ->
      let cond = print_exp cond in
      let then_ = print_exp then_ in
      let else_ = print_exp else_ in
      [%pla {|ifExpressionValue(<#cond#>, <#then_#>, <#else_#>)|}]
  | If { cond; then_; else_ } ->
      let cond = print_exp cond in
      let then_ = print_exp then_ in
      let else_ = print_exp else_ in
      [%pla {|ifExpression(<#cond#>, (function () return <#then_#> end), (function () return <#else_#> end))|}]
  | Tuple l ->
      let l = Pla.map_sep Pla.commaspace print_exp l in
      [%pla {|(<#l#>)|}]
  | Member (e, m) ->
      let e = print_exp e in
      [%pla {|<#e#>.<#m#s>|}]


let rec print_lexp e =
  match e.l with
  | LWild -> Pla.string "_wild"
  | LId s -> Pla.string s
  | LMember (e, m) ->
      let e = print_lexp e in
      [%pla {|<#e#>.<#m#s>|}]
  | LIndex (e, index) ->
      let e = print_lexp e in
      let index = print_exp index in
      [%pla {|<#e#>[<#index#>]|}]


let print_dexp (e : dexp) =
  match e.d with
  | DId (id, None) -> [%pla {|<#id#s>|}]
  | DId (id, Some dim) -> [%pla {|<#id#s>[<#dim#i>]|}]


let rec print_stmt s =
  match s with
  | StmtDecl (lhs, None) ->
      let lhs = print_dexp lhs in
      [%pla {|local <#lhs#>;|}]
  | StmtDecl (lhs, Some rhs) ->
      let lhs = print_dexp lhs in
      let rhs = print_exp rhs in
      [%pla {|local <#lhs#> = <#rhs#>;|}]
  | StmtBind ({ l = LWild; _ }, rhs) ->
      let rhs = print_exp rhs in
      [%pla {|<#rhs#>;|}]
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
      [%pla {|if <#e#> then<#then_#+><#>end|}]
  | StmtIf (cond, then_, Some else_) ->
      let cond = print_exp cond in
      let then_ = print_stmt then_ in
      let else_ = print_stmt else_ in
      [%pla {|if <#cond#> then<#then_#+><#>else<#else_#+><#>end|}]
  | StmtWhile (cond, stmt) ->
      let cond = print_exp cond in
      let stmt = print_stmt stmt in
      [%pla {|while <#cond#> do<#stmt#+><#>end|}]
  | StmtBlock stmts ->
      let stmt = Pla.map_sep_all Pla.newline print_stmt stmts in
      [%pla {|do<#stmt#+>end|}]


let print_arg (n, _) = [%pla {|<#n#s>|}]

let print_function_def (def : function_def) =
  let name = def.name in
  let args = Pla.map_sep Pla.commaspace print_arg def.args in
  [%pla {|function <#name#s>(<#args#>)|}]


let print_body body =
  match body with
  | StmtBlock stmts ->
      let stmts = Pla.map_sep_all Pla.newline print_stmt stmts in
      [%pla {|<#stmts#+>end|}]
  | _ ->
      let stmt = print_stmt body in
      [%pla {|<#stmt#+><#>end|}]


let print_top_stmt t =
  match t with
  | TopFunction (def, body) ->
      let def = print_function_def def in
      let body = print_body body in
      [%pla {|<#def#><#body#><#><#>|}]
  | TopExternal _ -> Pla.unit
  | TopType _ -> Pla.unit
  | TopDecl (lhs, rhs) ->
      let lhs = print_dexp lhs in
      let rhs = print_exp rhs in
      [%pla {|local <#lhs#> = <#rhs#>;|}]


let print_prog t = Pla.map_join print_top_stmt t

let generate output _template (stmts : top_stmt list) =
  let file = Common.setExt ".lua" output in
  let code = print_prog stmts in
  [ [%pla {|<#runtime#><#code#>|}], file ]
