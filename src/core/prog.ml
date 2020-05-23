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
open Parser
open Util

type tag = Tags.tag

type type_d_ =
  | TVoid
  | TInt
  | TReal
  | TString
  | TBool
  | TFixed
  | TArray  of int * type_
  | TStruct of struct_descr
  | TTuple  of type_ list

and struct_descr =
  { path : string
  ; members : param list
  }

and param = string * type_ * Loc.t

and type_ =
  { t : type_d_
  ; loc : Loc.t
  }

type fun_type = type_ list * type_

type operator =
  | OpAdd
  | OpSub
  | OpMul
  | OpDiv
  | OpMod
  | OpLand
  | OpLor
  | OpBor
  | OpBand
  | OpBxor
  | OpLsh
  | OpRsh
  | OpEq
  | OpNe
  | OpLt
  | OpLe
  | OpGt
  | OpGe

type uoperator =
  | UOpNeg
  | UOpNot

type exp_d =
  | EUnit
  | EBool   of bool
  | EInt    of int
  | EReal   of float
  | EString of string
  | EId     of string
  | EIndex  of
      { e : exp
      ; index : exp
      }
  | EArray  of exp list
  | ECall   of
      { path : string
      ; args : exp list
      }
  | EUnOp   of uoperator * exp
  | EOp     of operator * exp * exp
  | EIf     of
      { cond : exp
      ; then_ : exp
      ; else_ : exp
      }
  | ETuple  of exp list
  | EMember of exp * string

and exp =
  { e : exp_d
  ; loc : Loc.t
  ; t : type_
  }

and lexp_d =
  | LWild
  | LId     of string
  | LMember of lexp * string
  | LIndex  of
      { e : lexp
      ; index : exp
      }
  | LTuple  of lexp list

and lexp =
  { l : lexp_d
  ; loc : Loc.t
  ; t : type_
  }

type dexp_d =
  | DWild
  | DId    of string * int option
  | DTuple of dexp list

and dexp =
  { d : dexp_d
  ; loc : Loc.t
  ; t : type_
  }

and stmt_d =
  | StmtDecl   of dexp
  | StmtBind   of lexp * exp
  | StmtReturn of exp
  | StmtBlock  of stmt list
  | StmtIf     of exp * stmt * stmt option
  | StmtWhile  of exp * stmt

and stmt =
  { s : stmt_d
  ; loc : Loc.t
  }

and function_def =
  { name : string
  ; args : param list
  ; t : type_ list * type_
  ; loc : Loc.t
  ; tags : tag list
  }

type top_stmt_d =
  | TopExternal of function_def * string
  | TopFunction of function_def * stmt
  | TopType     of struct_descr

and top_stmt =
  { top : top_stmt_d
  ; loc : Loc.t
  }

module Print = struct
  let rec print_type_ (t : type_) : Pla.t =
    match t.t with
    | TVoid -> Pla.string "void"
    | TInt -> Pla.string "int"
    | TReal -> Pla.string "real"
    | TString -> Pla.string "string"
    | TBool -> Pla.string "bool"
    | TFixed -> Pla.string "fixed"
    | TArray (dim, t) ->
        let t = print_type_ t in
        {pla|<#t#>[<#dim#i>]|pla}
    | TStruct { path; _ } -> {pla|struct <#path#s>|pla}
    | TTuple elems ->
        let elems = Pla.map_sep Pla.commaspace print_type_ elems in
        {pla|(<#elems#>)|pla}


  let print_operator op =
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


  let print_uoperator op =
    match op with
    | UOpNeg -> Pla.string "-"
    | UOpNot -> Pla.string "not"


  let rec print_exp (e : exp) =
    match e.e with
    | EUnit -> Pla.string "()"
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
        let op = print_uoperator op in
        {pla|(<#op#><#e#>)|pla}
    | EOp (op, e1, e2) ->
        let e1 = print_exp e1 in
        let e2 = print_exp e2 in
        let op = print_operator op in
        {pla|(<#e1#> <#op#> <#e2#>)|pla}
    | EIf { cond; then_; else_ } ->
        let cond = print_exp cond in
        let then_ = print_exp then_ in
        let else_ = print_exp else_ in
        {pla|(if <#cond#> then <#then_#> else <#else_#>)|pla}
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
        {pla|<#e#>[<#index#>]|pla}
    | LTuple l ->
        let l = Pla.map_sep Pla.commaspace print_lexp l in
        {pla|(<#l#>)|pla}


  let rec print_dexp (e : dexp) =
    let t = print_type_ e.t in
    match e.d with
    | DWild -> {pla|_ : <#t#>|pla}
    | DId (id, None) -> {pla|<#id#s> : <#t#>|pla}
    | DId (id, Some dim) -> {pla|<#id#s>[<#dim#i>] : <#t#>|pla}
    | DTuple l ->
        let l = Pla.map_sep Pla.commaspace print_dexp l in
        {pla|(<#l#>) : <#t#>|pla}


  let rec print_stmt s =
    match s.s with
    | StmtDecl lhs ->
        let lhs = print_dexp lhs in
        {pla|val <#lhs#>;|pla}
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
        {pla|if (<#e#>) <#then_#>|pla}
    | StmtIf (cond, then_, Some else_) ->
        let cond = print_exp cond in
        let then_ = print_stmt then_ in
        let else_ = print_stmt else_ in
        {pla|if (<#cond#>) <#then_#><#>else <#else_#>|pla}
    | StmtWhile (cond, stmt) ->
        let cond = print_exp cond in
        let stmt = print_stmt stmt in
        {pla|while (<#cond#>)<#stmt#+>|pla}
    | StmtBlock stmts ->
        let stmt = Pla.map_sep_all Pla.newline print_stmt stmts in
        {pla|{<#stmt#+>}|pla}


  let print_arg (n, t, _) =
    let t = print_type_ t in
    {pla|<#n#s> : <#t#>|pla}


  let print_function_def kind (def : function_def) =
    let name = def.name in
    let args = Pla.map_sep Pla.commaspace print_arg def.args in
    let tags = Tags.print_tags def.tags in
    let t = print_type_ (snd def.t) in
    {pla|<#kind#s> <#name#s>(<#args#>) : <#t#> <#tags#>|pla}


  let print_member (name, t, _) =
    let t = print_type_ t in
    {pla|<#name#s> : <#t#>;|pla}


  let print_body body =
    match body.s with
    | StmtBlock _ -> print_stmt body
    | _ ->
        let stmt = print_stmt body in
        {pla|{<#stmt#+><#>}|pla}


  let print_top_stmt t =
    match t.top with
    | TopFunction (def, body) ->
        let def = print_function_def "fun" def in
        let body = print_body body in
        {pla|<#def#> <#body#><#>|pla}
    | TopExternal (def, link) ->
        let def = print_function_def "external" def in
        {pla|<#def#> "<#link#s>"<#>|pla}
    | TopType { path = p; members } ->
        let members = Pla.map_sep_all Pla.newline print_member members in
        {pla|struct <#p#s> {<#members#+>}<#>|pla}


  let print_prog t = Pla.map_sep_all Pla.newline print_top_stmt t
end

module TypedToProg = struct
  module T = Typed
  module Cache = CCMap.Make (String)

  type state =
    { types : type_ Cache.t
    ; dummy : int
    }

  let path (p : Syntax.path) : string =
    match p with
    | { id; n = None; _ } -> id
    | { id; n = Some n; _ } -> n ^ "_" ^ id


  let list mapper (env : Env.in_top) (state : state) (l : 'a list) =
    let state, rev =
      List.fold_left
        (fun (state, acc) e ->
          let state, e = mapper env state e in
          state, e :: acc)
        (state, [])
        l
    in
    state, List.rev rev


  let option mapper (env : Env.in_top) (state : state) (l : 'a option) =
    match l with
    | None -> state, None
    | Some l ->
        let state, l = mapper env state l in
        state, Some l


  let rec getDim (t : Typed.type_) =
    match t.tx with
    | T.TELink t -> getDim t
    | T.TESize dim -> dim
    | _ -> failwith "invalid dimmension"


  let rec type_ (env : Env.in_top) (state : state) (t : Typed.type_) =
    let loc = t.loc in
    match t.tx with
    | T.TENoReturn -> state, { t = TVoid; loc }
    | T.TEUnbound _ -> failwith "untyped"
    | T.TEOption _ -> failwith "undecided type"
    | T.TEId { id = "unit"; n = None; _ } -> state, { t = TVoid; loc }
    | T.TEId { id = "int"; n = None; _ } -> state, { t = TInt; loc }
    | T.TEId { id = "real"; n = None; _ } -> state, { t = TReal; loc }
    | T.TEId { id = "fix16"; n = None; _ } -> state, { t = TFixed; loc }
    | T.TEId { id = "string"; n = None; _ } -> state, { t = TString; loc }
    | T.TEId { id = "bool"; n = None; _ } -> state, { t = TBool; loc }
    | T.TEId p ->
        let ps = path p in
        begin
          match Cache.find_opt ps state.types with
          | Some t -> state, t
          | None ->
              ( match Env.getType env p with
              | None -> failwith "unknown type"
              | Some { members; _ } ->
                  let members =
                    List.map (fun (name, (var : Env.var)) -> name, var.t, var.loc) (Env.Map.to_list members)
                  in
                  let state, members = type_list env state members in
                  let t = { t = TStruct { path = ps; members }; loc } in
                  let types = Cache.add ps t state.types in
                  { state with types }, t )
        end
    | T.TELink t -> type_ env state t
    | T.TEComposed ("array", [ t; dim ]) ->
        let state, t = type_ env state t in
        let dim = getDim dim in
        state, { t = TArray (dim, t); loc }
    | T.TEComposed ("tuple", elems) ->
        let state, elems = list type_ env state elems in
        state, { t = TTuple elems; loc }
    | T.TEComposed _ -> failwith "unknown composed"
    | T.TESize _ -> failwith "invalid input"


  and type_list (env : Env.in_top) (state : state) (l : (string * Typed.type_ * Loc.t) list) =
    let mapper env state ((n : string), (t : Typed.type_), (loc : Loc.t)) =
      let state, t = type_ env state t in
      state, (n, t, loc)
    in
    list mapper env state l


  let operator op =
    match op with
    | "+" -> OpAdd
    | "-" -> OpSub
    | "*" -> OpMul
    | "/" -> OpDiv
    | "%" -> OpMod
    | "&&" -> OpLand
    | "||" -> OpLor
    | "|" -> OpBor
    | "&" -> OpBand
    | "^" -> OpBxor
    | "<<" -> OpLsh
    | ">>" -> OpRsh
    | "==" -> OpEq
    | "<>" -> OpNe
    | "<" -> OpLt
    | "<=" -> OpLe
    | ">" -> OpGt
    | ">=" -> OpGe
    | _ -> failwith "unknown operator"


  let uoperator op =
    match op with
    | "-" -> UOpNeg
    | "not" -> UOpNot
    | _ -> failwith "unknown uoperator"


  let rec exp (env : Env.in_top) (state : state) (e : Typed.exp) =
    let loc = e.loc in
    let state, t = type_ env state e.t in
    match e.e with
    | EUnit -> state, { e = EUnit; t; loc }
    | EBool v -> state, { e = EBool v; t; loc }
    | EInt n -> state, { e = EInt n; t; loc }
    | EReal n -> state, { e = EReal n; t; loc }
    | EString s -> state, { e = EString s; t; loc }
    | EId id -> state, { e = EId id; t; loc }
    | EIndex { e; index } ->
        let state, e = exp env state e in
        let state, index = exp env state index in
        state, { e = EIndex { e; index }; t; loc }
    | EArray l ->
        let state, l = list exp env state l in
        state, { e = EArray l; t; loc }
    | ECall { path = p; args; _ } ->
        let p = path p in
        let state, args = list exp env state args in
        state, { e = ECall { path = p; args }; t; loc }
    | EUnOp (op, e) ->
        let op = uoperator op in
        let state, e = exp env state e in
        state, { e = EUnOp (op, e); t; loc }
    | EOp (op, e1, e2) ->
        let state, e1 = exp env state e1 in
        let state, e2 = exp env state e2 in
        let op = operator op in
        state, { e = EOp (op, e1, e2); t; loc }
    | EIf { cond; then_; else_ } ->
        let state, cond = exp env state cond in
        let state, then_ = exp env state then_ in
        let state, else_ = exp env state else_ in
        state, { e = EIf { cond; then_; else_ }; t; loc }
    | ETuple l ->
        let state, l = list exp env state l in
        state, { e = ETuple l; t; loc }
    | EMember (e, m) ->
        let state, e = exp env state e in
        state, { e = EMember (e, m); t; loc }


  let rec lexp (env : Env.in_top) (state : state) (e : Typed.lexp) =
    let loc = e.loc in
    let state, t = type_ env state e.t in
    match e.l with
    | LWild -> state, { l = LWild; t; loc }
    | LId s -> state, { l = LId s; t; loc }
    | LMember (e, m) ->
        let state, e = lexp env state e in
        state, { l = LMember (e, m); t; loc }
    | LIndex { e; index } ->
        let state, e = lexp env state e in
        let state, index = exp env state index in
        state, { l = LIndex { e; index }; t; loc }
    | LTuple l ->
        let state, l = list lexp env state l in
        state, { l = LTuple l; t; loc }


  let rec dexp (env : Env.in_top) (state : state) (e : Typed.dexp) =
    let loc = e.loc in
    let state, t = type_ env state e.t in
    match e.d with
    | DWild -> state, { d = DWild; t; loc }
    | DId (id, None) -> state, { d = DId (id, None); t; loc }
    | DId (id, Some dim) -> state, { d = DId (id, Some dim); t; loc }
    | DTuple l ->
        let state, l = list dexp env state l in
        state, { d = DTuple l; t; loc }


  let block (stmts : stmt list) : stmt =
    match stmts with
    | [] -> { s = StmtBlock []; loc = Loc.default }
    | [ s ] -> s
    | _ -> { s = StmtBlock stmts; loc = Loc.default }


  let rec stmt (env : Env.in_top) (state : state) (s : Typed.stmt) =
    let loc = s.loc in
    match s.s with
    | StmtVal lhs ->
        let state, lhs = dexp env state lhs in
        state, [ { s = StmtDecl lhs; loc } ]
    | StmtMem (_, _) -> state, []
    | StmtBind (lhs, rhs) ->
        let state, lhs = lexp env state lhs in
        let state, rhs = exp env state rhs in
        state, [ { s = StmtBind (lhs, rhs); loc } ]
    | StmtReturn e ->
        let state, e = exp env state e in
        state, [ { s = StmtReturn e; loc } ]
    | StmtIf (cond, then_, None) ->
        let state, cond = exp env state cond in
        let state, then_ = stmt env state then_ in
        state, [ { s = StmtIf (cond, block then_, None); loc } ]
    | StmtIf (cond, then_, Some else_) ->
        let state, cond = exp env state cond in
        let state, then_ = stmt env state then_ in
        let state, else_ = stmt env state else_ in
        state, [ { s = StmtIf (cond, block then_, Some (block else_)); loc } ]
    | StmtWhile (cond, s) ->
        let state, cond = exp env state cond in
        let state, s = stmt env state s in
        state, [ { s = StmtWhile (cond, block s); loc } ]
    | StmtBlock stmts ->
        let state, stmts = list stmt env state stmts in
        ( match List.flatten stmts with
        | [] -> state, [ { s = StmtBlock []; loc } ]
        | [ { s = StmtBlock subs; _ } ] -> state, [ { s = StmtBlock subs; loc } ]
        | [ s ] -> state, [ s ]
        | subs -> state, [ { s = StmtBlock subs; loc } ] )


  let arg (env : Env.in_top) (state : state) (n, (t : Typed.type_), loc) =
    let state, t = type_ env state t in
    state, (n, t, loc)


  let function_type env state t =
    match t with
    | args, ret ->
        let state, args = list type_ env state args in
        let state, ret = type_ env state ret in
        state, (args, ret)


  let rec function_def (env : Env.in_top) (state : state) (def : Typed.function_def) body =
    let name = path def.name in
    let state, args = list arg env state def.args in
    let state, t = function_type env state def.t in
    let state, body = stmt env state body in
    let body = block body in
    let state, next = next_def env state def.next in
    let stmt = { top = TopFunction ({ name; args; t; loc = def.loc; tags = def.tags }, body); loc = def.loc } in
    state, stmt :: next


  and next_def env state def_opt =
    match def_opt with
    | None -> state, []
    | Some (def, body) -> function_def env state def body


  let ext_function_def (env : Env.in_top) (state : state) (def : Typed.function_def) linkname =
    let name = path def.name in
    let state, args = list arg env state def.args in
    let state, t = function_type env state def.t in
    let state, next = next_def env state def.next in
    let stmt = { top = TopExternal ({ name; args; t; loc = def.loc; tags = def.tags }, linkname); loc = def.loc } in
    state, stmt :: next


  let top_stmt (env : Env.in_top) (state : state) (t : Typed.top_stmt) =
    match t.top with
    | TopFunction (def, body) ->
        let state, functions = function_def env state def body in
        state, functions
    | TopExternal (def, linkname) ->
        let state, functions = ext_function_def env state def linkname in
        state, functions
    | TopType { path = p; members } ->
        let p = path p in
        let state, members = type_list env state members in
        let struct_descr = { path = p; members } in
        let t = { t = TStruct struct_descr; loc = t.loc } in
        let types = Cache.add p t state.types in
        { state with types }, [ { top = TopType struct_descr; loc = t.loc } ]


  let top_stmt_list (env : Env.in_top) (state : state) (t : Typed.top_stmt list) = list top_stmt env state t

  let convert env stmts =
    let state = { types = Cache.empty; dummy = 0 } in
    let _, t = top_stmt_list env state stmts in
    List.flatten t
end

let convert = TypedToProg.convert
