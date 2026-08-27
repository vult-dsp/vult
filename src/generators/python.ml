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

let runtime =
  {%pla|import math
import random as random_module

# Runtime functions (simple builtins like eps, pi, clip, sin, cos, etc. are inlined)

def random():
    return random_module.random()

def irandom():
    return int(random_module.random() * 4294967296)

def int_to_float(i):
    return float(i)

def float_to_int(i):
    return int(math.floor(i))

def initializeArray(v, size):
    return [v for _ in range(size)]

|}

let operator (op : operator) =
  match op with
  | OpAdd ->
      Pla.string "+"
  | OpSub ->
      Pla.string "-"
  | OpMul ->
      Pla.string "*"
  | OpDiv ->
      Pla.string "/"
  | OpMod ->
      Pla.string "%"
  | OpLand ->
      Pla.string "and"
  | OpLor ->
      Pla.string "or"
  | OpBor ->
      Pla.string "|"
  | OpBand ->
      Pla.string "&"
  | OpBxor ->
      Pla.string "^"
  | OpLsh ->
      Pla.string "<<"
  | OpRsh ->
      Pla.string ">>"
  | OpEq ->
      Pla.string "=="
  | OpNe ->
      Pla.string "!="
  | OpLt ->
      Pla.string "<"
  | OpLe ->
      Pla.string "<="
  | OpGt ->
      Pla.string ">"
  | OpGe ->
      Pla.string ">="

let uoperator (op : uoperator) = match op with UOpNeg -> Pla.string "-" | UOpNot -> Pla.string "not "

let rec print_exp (e : exp) =
  match e.e with
  | EEmptyValue -> (
    (* Create class instance based on type *)
    match e.t.t with
    | TStruct {path; _} ->
        {%pla|<#path#s>()|}
    | _ ->
        Pla.string "None" )
  | EUnit ->
      Pla.string "None"
  | EBool v ->
      Pla.string (if v then "True" else "False")
  | EInt n ->
      {%pla|<#n#i>|}
  | EReal n ->
      Pla.string (Util.Vfloat.to_string n)
  | EFixed n ->
      Pla.string (Util.Vfloat.to_string n)
  | EString s ->
      Pla.string_quoted s
  | EId id ->
      Pla.string id
  | EIndex {e; index= {e= EInt i; _}} ->
      let e = print_exp e in
      let index = i in
      {%pla|<#e#>[<#index#i>]|}
  | EIndex {e; index} ->
      let e = print_exp e in
      let index = print_exp index in
      {%pla|<#e#>[<#index#>]|}
  | EArray l ->
      Pla.wrap (Pla.string "[") (Pla.string "]") (Pla.map_sep Pla.commaspace print_exp l)
  (* List operations *)
  | ECall {path= "list_size"; args= [e1]} ->
      let e1 = print_exp e1 in
      {%pla|len(<#e1#>)|}
  | ECall {path= "list_capacity"; args= [_]} ->
      {%pla|2147483647|}
  | ECall {path= "list_append"; args= [l; v]} ->
      let l = print_exp l in
      let v = print_exp v in
      {%pla|<#l#>.append(<#v#>)|}
  | ECall {path= "list_insert"; args= [l; i; v]} ->
      let l = print_exp l in
      let i = print_exp i in
      let v = print_exp v in
      {%pla|<#l#>.insert(<#i#>, <#v#>)|}
  | ECall {path= "list_remove"; args= [l; i]} ->
      let l = print_exp l in
      let i = print_exp i in
      {%pla|<#l#>.pop(<#i#>)|}
  | ECall {path= "list_clear"; args= [e1]} ->
      let e1 = print_exp e1 in
      {%pla|<#e1#>.clear()|}
  | ECall {path= "list_reserve"; args= [_; _]} ->
      (* No-op for Python *)
      {%pla|None|}
  | ECall {path= "list_get"; args= [l; i]} ->
      let l = print_exp l in
      let i = print_exp i in
      {%pla|<#l#>[<#i#>]|}
  | ECall {path= "list_set"; args= [l; i; v]} ->
      let l = print_exp l in
      let i = print_exp i in
      let v = print_exp v in
      {%pla|<#l#>.__setitem__(<#i#>, <#v#>)|}
  (* Inline simple builtins to avoid function call overhead *)
  | ECall {path= "eps"; args= []} ->
      {%pla|1e-18|}
  | ECall {path= "pi"; args= []} ->
      {%pla|3.1415926535897932384|}
  | ECall {path= "real"; args= [x]} ->
      let x = print_exp x in
      {%pla|float(<#x#>)|}
  | ECall {path= "int_"; args= [x]} ->
      let x = print_exp x in
      {%pla|int(<#x#>)|}
  | ECall {path= "not_"; args= [x]} ->
      let x = print_exp x in
      {%pla|(0 if (<#x#>) != 0 else 1)|}
  | ECall {path= "clip"; args= [x; low; high]} ->
      let x = print_exp x in
      let low = print_exp low in
      let high = print_exp high in
      {%pla|((<#low#>) if (<#x#>) < (<#low#>) else ((<#high#>) if (<#x#>) > (<#high#>) else (<#x#>)))|}
  (* Inline math functions *)
  | ECall {path= "sin"; args= [x]} ->
      let x = print_exp x in
      {%pla|math.sin(<#x#>)|}
  | ECall {path= "cos"; args= [x]} ->
      let x = print_exp x in
      {%pla|math.cos(<#x#>)|}
  | ECall {path= "abs"; args= [x]} ->
      let x = print_exp x in
      {%pla|abs(<#x#>)|}
  | ECall {path= "exp"; args= [x]} ->
      let x = print_exp x in
      {%pla|math.exp(<#x#>)|}
  | ECall {path= "floor"; args= [x]} ->
      let x = print_exp x in
      {%pla|math.floor(<#x#>)|}
  | ECall {path= "ceil"; args= [x]} ->
      let x = print_exp x in
      {%pla|math.ceil(<#x#>)|}
  | ECall {path= "asin"; args= [x]} ->
      let x = print_exp x in
      {%pla|math.asin(<#x#>)|}
  | ECall {path= "acos"; args= [x]} ->
      let x = print_exp x in
      {%pla|math.acos(<#x#>)|}
  | ECall {path= "atan"; args= [x]} ->
      let x = print_exp x in
      {%pla|math.atan(<#x#>)|}
  | ECall {path= "atan2"; args= [a; b]} ->
      let a = print_exp a in
      let b = print_exp b in
      {%pla|math.atan2(<#a#>, <#b#>)|}
  | ECall {path= "min"; args= [a; b]} ->
      let a = print_exp a in
      let b = print_exp b in
      {%pla|min(<#a#>, <#b#>)|}
  | ECall {path= "max"; args= [a; b]} ->
      let a = print_exp a in
      let b = print_exp b in
      {%pla|max(<#a#>, <#b#>)|}
  | ECall {path= "tan"; args= [x]} ->
      let x = print_exp x in
      {%pla|math.tan(<#x#>)|}
  | ECall {path= "tanh"; args= [x]} ->
      let x = print_exp x in
      {%pla|math.tanh(<#x#>)|}
  | ECall {path= "sqrt"; args= [x]} ->
      let x = print_exp x in
      {%pla|math.sqrt(<#x#>)|}
  | ECall {path= "pow"; args= [a; b]} ->
      let a = print_exp a in
      let b = print_exp b in
      {%pla|math.pow(<#a#>, <#b#>)|}
  | ECall {path; args} ->
      let args = Pla.map_sep Pla.commaspace print_exp args in
      {%pla|<#path#s>(<#args#>)|}
  | EUnOp (op, e) ->
      let e = print_exp e in
      let op = uoperator op in
      {%pla|(<#op#><#e#>)|}
  | EOp (op, e1, e2) ->
      let se1 = print_exp e1 in
      let se2 = print_exp e2 in
      let op = operator op in
      {%pla|(<#se1#> <#op#> <#se2#>)|}
  | EIf {cond; then_; else_} ->
      let cond = print_exp cond in
      let then_ = print_exp then_ in
      let else_ = print_exp else_ in
      {%pla|(<#then_#> if <#cond#> else <#else_#>)|}
  | ETuple l ->
      let l = Pla.map_sep Pla.commaspace print_exp l in
      {%pla|[<#l#>]|}
  | EMember (e, m) ->
      let e = print_exp e in
      {%pla|<#e#>.<#m#s>|}
  | ETMember (e, i) ->
      let e = print_exp e in
      let m = i in
      {%pla|<#e#>[<#m#i>]|}
  | ERecord {path; elems} ->
      (* Generate class instantiation with keyword arguments *)
      let printElem (n, v) =
        let v = print_exp v in
        {%pla|<#n#s>=<#v#>|}
      in
      let elems = Pla.map_sep Pla.commaspace printElem elems in
      {%pla|<#path#s>(<#elems#>)|}

let rec print_lexp (e : lexp) =
  match e.l with
  | LWild ->
      Pla.string "_"
  | LId s ->
      Pla.string s
  | LMember (e, m) ->
      let e = print_lexp e in
      {%pla|<#e#>.<#m#s>|}
  | LIndex {e; index= {e= EInt i; _}} ->
      let e = print_lexp e in
      let index = i in
      {%pla|<#e#>[<#index#i>]|}
  | LIndex {e; index} ->
      let e = print_lexp e in
      let index = print_exp index in
      {%pla|<#e#>[<#index#>]|}
  | _ ->
      failwith "Python:print_lexp LTuple"

let print_dexp (e : dexp) =
  match e.d with DId (id, None) -> {%pla|<#id#s>|} | DId (id, Some dim) -> {%pla|<#id#s>[<#dim#i>]|}

let rec print_stmt (s : stmt) =
  match s.s with
  (* if the name is _ctx, create a class instance *)
  | StmtDecl (({d= DId ("_ctx", _); t= {t= TStruct {path; _}; _}; _} as lhs), None) ->
      let lhs = print_dexp lhs in
      {%pla|<#lhs#> = <#path#s>()|}
  (* needs allocation - create class instance directly *)
  | StmtDecl (({t= {t= TStruct {path; _}; _}; _} as lhs), None) ->
      let lhs = print_dexp lhs in
      {%pla|<#lhs#> = <#path#s>()|}
  (* declaration without value - initialize to None *)
  | StmtDecl (lhs, None) ->
      let lhs = print_dexp lhs in
      {%pla|<#lhs#> = None|}
  (* declaration with value *)
  | StmtDecl (lhs, Some rhs) ->
      let lhs = print_dexp lhs in
      let rhs = print_exp rhs in
      {%pla|<#lhs#> = <#rhs#>|}
  (* wildcard binding - just evaluate the expression *)
  | StmtBind ({l= LWild; _}, rhs) ->
      let rhs = print_exp rhs in
      {%pla|_ = <#rhs#>|}
  (* normal assignment *)
  | StmtBind (lhs, rhs) ->
      let lhs = print_lexp lhs in
      let rhs = print_exp rhs in
      {%pla|<#lhs#> = <#rhs#>|}
  (* return statement *)
  | StmtReturn e ->
      let e = print_exp e in
      {%pla|return <#e#>|}
  (* if without else *)
  | StmtIf (cond, then_, None) ->
      let e = print_exp cond in
      let then_ = print_stmt then_ in
      {%pla|if <#e#>:<#then_#+>|}
  (* if with else *)
  | StmtIf (cond, then_, Some else_) ->
      let cond = print_exp cond in
      let then_ = print_stmt then_ in
      let else_ = print_stmt else_ in
      {%pla|if <#cond#>:<#then_#+><#>else:<#else_#+>|}
  (* while loop *)
  | StmtWhile (cond, stmt) ->
      let cond = print_exp cond in
      let stmt = print_stmt stmt in
      {%pla|while <#cond#>:<#stmt#+>|}
  (* block of statements *)
  | StmtBlock stmts ->
      let stmt = Pla.map_sep_all Pla.newline print_stmt stmts in
      {%pla|<#stmt#>|}
  (* switch converted to if/elif/else chain *)
  | StmtSwitch (e1, cases, default) -> (
      let if_ =
        CCList.fold_right
          (fun (e2, body) else_ ->
            let cond = C.eeq e1 e2 in
            Some (C.sif cond body else_) )
          cases default
      in
      match if_ with None -> Pla.string "pass" | Some if_ -> print_stmt if_ )

let print_arg ({name; _} : param) = {%pla|<#name#s>|}

let print_function_def (def : function_def) =
  let name = def.name in
  let args = Pla.map_sep Pla.commaspace print_arg def.args in
  {%pla|def <#name#s>(<#args#>):|}

let print_body (body : stmt) =
  match body.s with
  | StmtBlock [] ->
      {%pla|<#>   pass|}
  | StmtBlock stmts ->
      let stmts = Pla.map_sep_all Pla.newline print_stmt stmts in
      {%pla|<#stmts#+>|}
  | _ ->
      let stmt = print_stmt body in
      {%pla|<#stmt#+>|}

let print_top_stmt (args : Util.Args.args) (t : top_stmt) =
  match t.top with
  | TopFunction (def, body) ->
      let def = print_function_def def in
      let body = print_body body in
      {%pla|<#def#><#body#><#><#>|}
  | TopExternal _ ->
      Pla.unit
  | TopType {path; members} ->
      (* Generate a class with __slots__ for better performance *)
      let member_names =
        CCList.map (fun (name, _, _, _) -> {%pla|'<#name#s>'|}) members |> Pla.join_sep Pla.commaspace
      in
      let getDefaultValue (t : type_) =
        match t.t with
        | TInt ->
            "0"
        | TInt16 ->
            "0"
        | TReal ->
            "0.0"
        | TFix16 ->
            "0.0"
        | TBool ->
            "False"
        | TString ->
            "\"\""
        | TStruct {path; _} ->
            path ^ "()"
        | TArray (size_opt, _) -> (
          match size_opt with Some size -> Printf.sprintf "[0.0] * %d" size | None -> "[]" )
        | TList _ ->
            "[]"
        | _ ->
            "None"
      in
      (* Generate __init__ with keyword arguments for record literals *)
      let init_params =
        CCList.map
          (fun (name, t, _, _) ->
            let default = getDefaultValue t in
            {%pla|<#name#s>=<#default#s>|} )
          members
        |> Pla.join_sep Pla.commaspace
      in
      let init_assignments =
        CCList.map (fun (name, _, _, _) -> {%pla|        self.<#name#s> = <#name#s>|}) members
        |> Pla.join_sep Pla.newline
      in
      {%pla|class <#path#s>:
    __slots__ = [<#member_names#>]
    def __init__(self, <#init_params#>):
<#init_assignments#>

|}
  | TopAlias _ ->
      Pla.unit
  | TopConstant (name, _, _, _, _) when args.test_mode ->
      {%pla|<#name#s> = {}<#>|}
  | TopConstant (name, _, _, rhs, _) ->
      let rhs = print_exp rhs in
      {%pla|<#name#s> = <#rhs#><#>|}

let print_prog (args : Util.Args.args) (t : top_stmt list) = Pla.map_join (print_top_stmt args) t

let getTemplateCode (args : Util.Args.args) =
  match args.template with
  | None ->
      (Pla.unit, Pla.unit)
  | Some "performance" ->
      T_performance.generatePython args
  | Some name ->
      Util.Error.raiseErrorMsg ("Unknown template '" ^ name ^ "' for Python")

let generate (args : Util.Args.args) (stmts : top_stmt list) =
  let file = Common.setExt ".py" args.output in
  let code = print_prog args stmts in
  let pre, post = getTemplateCode args in
  [({%pla|<#runtime#><#pre#><#code#><#post#>|}, file)]
