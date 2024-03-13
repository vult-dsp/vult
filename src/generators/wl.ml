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

(* TODO:
   - Runtime cleanup: generate only the required functions
   - String support: conversions and concatenation
*)

let runtime =
  {%pla|
IntegerDivision[a_, b_] := Floor[a / b];
newEmptyObject[] := CreateDataStructure["HashTable"];
setValue[var_, key_, value_] := var["Insert", key -> value];
getValue[var_, key_] := var["Lookup", key];
newObject[elems_] := Module[{t = newEmptyObject[]}, Map[t["Insert", #] &, elems]; t];

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
  | OpBxor -> Pla.string "^"
  | OpLsh -> Pla.string "<<"
  | OpRsh -> Pla.string ">>"
  | OpEq -> Pla.string "=="
  | OpNe -> Pla.string "~="
  | OpLt -> Pla.string "<"
  | OpLe -> Pla.string "<="
  | OpGt -> Pla.string ">"
  | OpGe -> Pla.string ">="


let uoperator (op : uoperator) =
  match op with
  | UOpNeg -> Pla.string "-"
  | UOpNot -> Pla.string "not"


let e_regex = Str.regexp_string "e"
let fixFloat s = Str.global_replace e_regex " 10^" s

let rec print_exp e =
  match e.e with
  | EUnit -> Pla.string ""
  | EBool v -> Pla.string (if v then "True" else "False")
  | EInt n -> Pla.int n
  | EReal n -> Pla.string (fixFloat (Util.Vfloat.to_string n))
  | EFixed n -> Pla.string (fixFloat (Util.Vfloat.to_string n))
  | EString s -> Pla.string_quoted s
  | EId id -> Pla.string id
  | EIndex { e; index } ->
    let e = print_exp e in
    let index = print_exp index in
    {%pla|<#e#>[[<#index#> + 1]]|}
  | EArray l -> Pla.wrap (Pla.string "{") (Pla.string "}") (Pla.map_sep Pla.commaspace print_exp l)
  | ECall { path = "clip"; args = [ v; min; max ] } ->
    let v = print_exp v in
    let min = print_exp min in
    let max = print_exp max in
    {%pla|Clip[<#v#>, {<#min#>, <#max#>}]|}
  | ECall { path; args } ->
    let args = Pla.map_sep Pla.commaspace print_exp args in
    {%pla|<#path#s>[<#args#>]|}
  | EUnOp (op, e) ->
    let e = print_exp e in
    let op = uoperator op in
    {%pla|(<#op#><#e#>)|}
  | EOp (op, e1, e2) ->
    let se1 = print_exp e1 in
    let se2 = print_exp e2 in
    let op = operator op in
    {%pla|(<#se1#> <#op#> <#se2#>)|}
  | EIf { cond; then_; else_ } ->
    let cond = print_exp cond in
    let then_ = print_exp then_ in
    let else_ = print_exp else_ in
    {%pla|If[<#cond#>, <#then_#>, <#else_#>]|}
  | ETuple l ->
    let l = Pla.map_sep Pla.commaspace print_exp l in
    {%pla|{ <#l#> }|}
  | EMember (e, m) ->
    let e = print_exp e in
    {%pla|getValue[<#e#>, "<#m#s>"]|}
  | ETMember (e, i) ->
    let e = print_exp e in
    let m = i + 1 in
    {%pla|<#e#>[[<#m#i>]]|}
  | ERecord { elems; _ } ->
    let printElem (n, v) =
      let v = print_exp v in
      {%pla|"<#n#s>" -> <#v#>|}
    in
    let elems = Pla.map_sep Pla.commaspace printElem elems in
    {%pla|newObject[{ <#elems#> }]|}


let rec print_lexp e =
  match e.l with
  | LWild -> Pla.string "`wild"
  | LId s -> Pla.string s
  | LMember (e, m) ->
    let e = print_lexp e in
    {%pla|getValue[<#e#>, "<#m#s>"]|}
  | LIndex { e; index } ->
    let e = print_lexp e in
    let index = print_exp index in
    {%pla|<#e#>[[<#index#> + 1]]|}
  | _ -> failwith "Wl:print_lexp LTuple"


let print_dexp (e : dexp) =
  match e.d with
  | DId (id, None) -> {%pla|<#id#s>|}
  | DId (id, Some dim) -> {%pla|<#id#s>[<#dim#i>]|}


let collectDeclaredNames (stmts : stmt list) =
  List.fold_left
    (fun acc s ->
      match s.s with
      | StmtDecl ({ d = DId (name, _); _ }, _) -> name :: acc
      | _ -> acc)
    []
    stmts


let rec print_stmt (s : stmt) =
  match s.s with
  (* if the name is _ctx, do not call the allocator*)
  | StmtDecl (({ d = DId ("`ctx", _); t = { t = TStruct _; _ }; _ } as lhs), None) ->
    let lhs = print_dexp lhs in
    {%pla|<#lhs#> = newEmptyObject[];|}
  (* needs allocation *)
  | StmtDecl (({ t = { t = TStruct { path; _ }; _ }; _ } as lhs), None) ->
    let lhs = print_dexp lhs in
    let path = Replacements.keyword Util.Args.WLCode path in
    {%pla|<#lhs#> = <#path#s>`alloc[];|}
  | StmtDecl ({ d = DId (name, _); t = { t = TArray (Some n, _); _ }; _ }, None) ->
    {%pla|<#name#s> = Table[0, { i, 1, <#n#i>}];|}
  | StmtDecl ({ d = DId (name, _); _ }, None) -> {%pla|<#name#s> = 0|}
  | StmtDecl (lhs, Some rhs) ->
    let lhs = print_dexp lhs in
    let rhs = print_exp rhs in
    {%pla|<#lhs#> = <#rhs#>;|}
  | StmtBind ({ l = LWild; _ }, rhs) ->
    let rhs = print_exp rhs in
    {%pla|<#rhs#>;|}
  | StmtBind ({ l = LMember (le, m); _ }, rhs) ->
    let rhs = print_exp rhs in
    let le = print_lexp le in
    {%pla|setValue[<#le#>,"<#m#s>",<#rhs#>];|}
  | StmtBind (lhs, rhs) ->
    let lhs = print_lexp lhs in
    let rhs = print_exp rhs in
    {%pla|<#lhs#> = <#rhs#>;|}
  | StmtReturn e ->
    let e = print_exp e in
    {%pla|Return[<#e#>];|}
  | StmtIf (cond, then_, None) ->
    let e = print_exp cond in
    let then_ = print_stmt then_ in
    {%pla|If[<#e#>, <#then_#+>];|}
  | StmtIf (cond, then_, Some else_) ->
    let cond = print_exp cond in
    let then_ = print_stmt then_ in
    let else_ = print_stmt else_ in
    {%pla|If[<#cond#>, <#then_#+>, <#else_#+>];|}
  | StmtWhile (cond, stmt) ->
    let cond = print_exp cond in
    let stmt = print_stmt stmt in
    {%pla|While[<#cond#>, <#stmt#+>];|}
  | StmtBlock stmts ->
    let locals = Pla.map_sep Pla.commaspace Pla.string (collectDeclaredNames stmts) in
    let stmt = Pla.map_sep_all Pla.newline print_stmt stmts in
    {%pla|Module[{ <#locals#> }, <#stmt#+>];|}
  | StmtSwitch (e1, cases, default) ->
    let cases = List.flatten (List.map (fun (e, v) -> [ print_exp e; print_stmt v ]) cases) in
    let default =
      match default with
      | None -> []
      | Some v -> [ Pla.string "_"; print_stmt v ]
    in
    let all_cases = Pla.join_sep Pla.commaspace (cases @ default) in
    let e1 = print_exp e1 in
    {%pla|Switch[<#e1#>, <#all_cases#>];|}


let print_arg ({ name; _ } : param) = {%pla|<#name#s>_|}

let print_function_def (def : function_def) =
  let name = def.name in
  let args = Pla.map_sep Pla.commaspace print_arg def.args in
  {%pla|<#name#s>[<#args#>]|}


let print_body body =
  match body.s with
  | StmtBlock stmts ->
    let locals = Pla.map_sep Pla.commaspace Pla.string (collectDeclaredNames stmts) in
    let stmts = Pla.map_sep_all Pla.newline print_stmt stmts in
    {%pla| := Module[{ <#locals#> }, <#stmts#+>]|}
  | _ ->
    let stmt = print_stmt body in
    {%pla| := <#stmt#+><#>|}


let print_top_stmt t =
  match t.top with
  | TopFunction (def, body) ->
    let def = print_function_def def in
    let body = print_body body in
    {%pla|<#def#><#body#><#><#>|}
  | TopExternal _ -> Pla.unit
  | TopType _ -> Pla.unit
  | TopAlias _ -> Pla.unit
  | TopConstant (name, _, _, rhs) ->
    let rhs = print_exp rhs in
    {%pla|<#name#s> = <#rhs#>;<#>|}


let print_prog t = Pla.map_join print_top_stmt t

let getTemplateCode (args : Util.Args.args) =
  match args.template with
  | None -> Pla.unit, Pla.unit
  | Some name -> Util.Error.raiseErrorMsg ("Unknown template '" ^ name ^ "'")


let generate (args : Util.Args.args) (stmts : top_stmt list) =
  let file = Common.setExt ".lua" args.output in
  let code = print_prog stmts in
  let pre, post = getTemplateCode args in
  [ {%pla|<#runtime#><#pre#><#code#><#post#>|}, file ]
