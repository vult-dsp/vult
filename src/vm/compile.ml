open Core
open Prog
open Util
open Maps

type builtin =
  | Set
  | Get
  | Size
  | Abs
  | Exp
  | Log10
  | Sin
  | Cos
  | Tan
  | Sinh
  | Cosh
  | Tanh
  | Sqrt
  | Floor
  | Clip
  | Pow
  | Pi
  | Samplerate
  | Real
  | Fixed
  | Int
  | Eps
  | Random

type f =
  | F of int
  | B of builtin

type env =
  { functions : f Map.t
  ; locals : int Map.t
  ; lcount : int
  ; fcount : int
  }

let functions =
  [ "set", B Set
  ; "get", B Get
  ; "size", B Size
  ; "abs", B Abs
  ; "exp", B Exp
  ; "log10", B Log10
  ; "sin", B Sin
  ; "cos", B Cos
  ; "tan", B Tan
  ; "sinh", B Sinh
  ; "cosh", B Cosh
  ; "tanh", B Tanh
  ; "sqrt", B Sqrt
  ; "floor", B Floor
  ; "clip", B Clip
  ; "pow", B Pow
  ; "real", B Real
  ; "pi", B Pi
  ; "samplerate", B Samplerate
  ; "int", B Int
  ; "random", B Random
  ; "eps", B Eps
  ; "fix16", B Fixed
  ]
  |> Map.of_list


let default_env = { locals = Map.empty; lcount = 0; functions; fcount = 0 }

let addLocal env name = { env with locals = Map.add name env.lcount env.locals; lcount = env.lcount + 1 }

type op =
  | OpAdd
  | OpSub
  | OpDiv
  | OpMul
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

type rvalue_d =
  | RVoid
  | RInt    of int
  | RReal   of float
  | RBool   of bool
  | RString of string
  | RRef    of int * string
  | RObject of rvalue array
  | ROp     of op * rvalue * rvalue
  | RNot    of rvalue
  | RNeg    of rvalue
  | RIf     of rvalue * rvalue * rvalue
  | RMember of rvalue * int * string
  | RCall   of f * string * rvalue list
  | RIndex  of rvalue * rvalue

and rvalue =
  { r : rvalue_d
  ; loc : Loc.t
  }

type lvalue_d =
  | LVoid
  | LRef    of int * string
  | LTuple  of lvalue array
  | LMember of lvalue * int * string
  | LIndex  of lvalue * rvalue

and lvalue =
  { l : lvalue_d
  ; loc : Loc.t
  }

type instr_d =
  | Store  of lvalue * rvalue
  | Return of rvalue
  | If     of rvalue * instr list * instr list
  | While  of rvalue * instr list

and instr =
  { i : instr_d
  ; loc : Loc.t
  }

type segment =
  | External
  | Function of
      { name : string
      ; body : instr list
      ; locals : int
      ; n_args : int
      }

type segments = segment array [@@deriving show]

type bytecode =
  { table : f Map.t
  ; code : segments
  }

let list f (env : 'env) (l : 'e) =
  let env, i_rev =
    List.fold_left
      (fun (env, instr) d ->
        let env, i = f env d in
        env, i :: instr )
      (env, [])
      l
  in
  env, List.flatten (List.rev i_rev)


let getIndex name elems =
  let rec loop elems index =
    match elems with
    | [] -> failwith "index not found"
    | (current, _, _) :: _ when current = name -> index
    | _ :: t -> loop t (index + 1)
  in
  loop elems 0


let rec dexp_to_lexp (d : dexp) : lexp =
  let t = d.t in
  let loc = d.loc in
  match d.d with
  | DWild -> { l = LWild; t; loc }
  | DId (name, _) -> { l = LId name; t; loc }
  | DTuple l ->
      let l = List.map dexp_to_lexp l in
      { l = LTuple l; t; loc }


let rec compile_dexp (env : env) d =
  match d.d with
  | DWild -> env
  | DId (name, _) -> addLocal env name
  | DTuple l -> List.fold_left compile_dexp env l


let rec compile_lexp (env : env) (l : lexp) : lvalue =
  let loc = l.loc in
  match l.l with
  | LWild -> { l = LVoid; loc }
  | LId name ->
      let index =
        match Map.find_opt name env.locals with
        | Some index -> index
        | None -> failwith name
      in
      { l = LRef (index, name); loc }
  | LTuple l ->
      let l = List.map (compile_lexp env) l |> Array.of_list in
      { l = LTuple l; loc }
  | LMember (e, s) ->
      ( match e.t.t with
      | TStruct descr ->
          let index = getIndex s descr.members in
          let e = compile_lexp env e in
          { l = LMember (e, index, s); loc }
      | _ -> failwith "type error" )
  | LIndex { e; index } ->
      let e = compile_lexp env e in
      let index = compile_exp env index in
      { l = LIndex (e, index); loc }


and compile_exp (env : env) e : rvalue =
  let loc = e.loc in
  match e.e with
  | EUnit -> { r = RVoid; loc }
  | EBool v -> { r = RBool v; loc }
  | EInt v -> { r = RInt v; loc }
  | EReal v -> { r = RReal v; loc }
  | EFixed v -> { r = RReal v; loc }
  | EString v -> { r = RString v; loc }
  | EId id ->
      let index =
        match Map.find_opt id env.locals with
        | Some index -> index
        | None -> failwith id
      in
      { r = RRef (index, id); loc }
  | EOp (op, e1, e2) ->
      let e1 = compile_exp env e1 in
      let e2 = compile_exp env e2 in
      { r = makeOp op e1 e2; loc }
  | EIndex { e; index } ->
      let e1 = compile_exp env e in
      let e2 = compile_exp env index in
      { r = RIndex (e1, e2); loc }
  | EUnOp (UOpNeg, e) ->
      let e = compile_exp env e in
      { r = RNeg e; loc }
  | EUnOp (UOpNot, e) ->
      let e = compile_exp env e in
      { r = RNot e; loc }
  | EIf { cond; then_; else_ } ->
      let cond = compile_exp env cond in
      let then_ = compile_exp env then_ in
      let else_ = compile_exp env else_ in
      { r = RIf (cond, then_, else_); loc }
  | ETuple elems ->
      let elems = List.map (compile_exp env) elems in
      { r = RObject (Array.of_list elems); loc }
  | EArray elems ->
      let elems = List.map (compile_exp env) elems in
      { r = RObject (Array.of_list elems); loc }
  | EMember (e, m) ->
      ( match e.t.t with
      | TStruct descr ->
          let index = getIndex m descr.members in
          let e = compile_exp env e in
          { r = RMember (e, index, m); loc }
      | _ -> failwith "type error" )
  | ECall { path; args } ->
      let args = List.map (compile_exp env) args in
      ( match Map.find_opt path env.functions with
      | Some index -> { r = RCall (index, path, args); loc }
      | None ->
          print_endline ("Function not found " ^ path) ;
          { r = RVoid; loc } )


and makeOp op e1 e2 =
  match op with
  | OpAdd -> ROp (OpAdd, e1, e2)
  | OpSub -> ROp (OpSub, e1, e2)
  | OpMul -> ROp (OpMul, e1, e2)
  | OpDiv -> ROp (OpDiv, e1, e2)
  | OpMod -> ROp (OpMod, e1, e2)
  | OpLand -> ROp (OpLand, e1, e2)
  | OpLor -> ROp (OpLor, e1, e2)
  | OpBor -> ROp (OpBor, e1, e2)
  | OpBand -> ROp (OpBand, e1, e2)
  | OpBxor -> ROp (OpBxor, e1, e2)
  | OpLsh -> ROp (OpLsh, e1, e2)
  | OpRsh -> ROp (OpRsh, e1, e2)
  | OpEq -> ROp (OpEq, e1, e2)
  | OpNe -> ROp (OpNe, e1, e2)
  | OpLt -> ROp (OpLt, e1, e2)
  | OpLe -> ROp (OpLe, e1, e2)
  | OpGt -> ROp (OpGt, e1, e2)
  | OpGe -> ROp (OpGe, e1, e2)


let rec compile_stmt (env : env) (stmt : stmt) =
  let loc = stmt.loc in
  match stmt.s with
  | StmtDecl lhs ->
      let env = compile_dexp env lhs in
      env, []
  | StmtBind (lhs, rhs) ->
      let lhs = compile_lexp env lhs in
      let rhs = compile_exp env rhs in
      env, [ { i = Store (lhs, rhs); loc } ]
  | StmtReturn e ->
      let e = compile_exp env e in
      env, [ { i = Return e; loc } ]
  | StmtBlock stmts ->
      let env, instr = list compile_stmt env stmts in
      env, instr
  | StmtIf (cond, then_, Some else_) ->
      let cond = compile_exp env cond in
      let env, then_ = compile_stmt env then_ in
      let env, else_ = compile_stmt env else_ in
      env, [ { i = If (cond, then_, else_); loc } ]
  | StmtIf (cond, then_, None) ->
      let cond = compile_exp env cond in
      let env, then_ = compile_stmt env then_ in
      env, [ { i = If (cond, then_, []); loc } ]
  | StmtWhile (cond, body) ->
      let cond = compile_exp env cond in
      let env, body = compile_stmt env body in
      env, [ { i = While (cond, body); loc } ]


let getNOutputs (t : type_) =
  match t.t with
  | TVoid _
   |TInt
   |TReal
   |TString
   |TBool
   |TFixed
   |TArray _
   |TStruct _ ->
      1
  | TTuple elems -> List.length elems


let compile_top (env : env) (s : top_stmt) =
  match s.top with
  | TopExternal ({ name; _ }, _) ->
      let index = env.fcount in
      let functions = Map.add name (F index) env.functions in
      let env = { env with functions; fcount = env.fcount + 1 } in
      env, [ External ]
  | TopType _ -> env, []
  | TopFunction ({ name; args; _ }, body) ->
      let index = env.fcount in
      let functions = Map.add name (F index) env.functions in
      let env = { locals = Map.empty; lcount = 0; functions; fcount = env.fcount + 1 } in
      let env = List.fold_left (fun env (n, _, _) -> addLocal env n) env args in
      let env, body = compile_stmt env body in
      let n_args = List.length args in
      env, [ Function { name; body; locals = env.lcount - n_args; n_args } ]


let compile stmts : env * segment list = list compile_top default_env stmts

let print_op op =
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


let builtin b =
  match b with
  | Set -> "set"
  | Get -> "get"
  | Size -> "size"
  | Abs -> "abs"
  | Exp -> "exp"
  | Log10 -> "log10"
  | Sin -> "sin"
  | Cos -> "cos"
  | Tan -> "tan"
  | Sinh -> "sinh"
  | Cosh -> "cosh"
  | Tanh -> "tanh"
  | Sqrt -> "sqrt"
  | Floor -> "floor"
  | Clip -> "clip"
  | Pow -> "pow"
  | Samplerate -> "samplerate"
  | Pi -> "pi"
  | Real -> "real"
  | Fixed -> "fix16"
  | Int -> "int"
  | Random -> "random"
  | Eps -> "eps"


let f f =
  match f with
  | F i -> Pla.int i
  | B i -> Pla.string (builtin i)


let rec print_rvalue r =
  match r.r with
  | RVoid -> Pla.string "()"
  | RInt n -> Pla.int n
  | RReal n -> Pla.float n
  | RBool n -> Pla.string (if n then "true" else "false")
  | RString s -> Pla.string_quoted s
  | RRef (n, s) -> [%pla {|[<#n#i>:<#s#s>]|}]
  | RObject elems ->
      let elems = Pla.map_sep Pla.commaspace print_rvalue (Array.to_list elems) in
      [%pla {|{ <#elems#> }|}]
  | RIndex (e, i) ->
      let e = print_rvalue e in
      let i = print_rvalue i in
      [%pla {|<#e#>[<#i#>]|}]
  | RCall (i, s, args) ->
      let args = Pla.map_sep Pla.commaspace print_rvalue args in
      let i = f i in
      [%pla {|<#i#>:<#s#s>(<#args#>)|}]
  | RMember (e, i, s) ->
      let e = print_rvalue e in
      [%pla {|<#e#>.[<#i#i>:<#s#s>]|}]
  | RNot e ->
      let e = print_rvalue e in
      [%pla {|not(<#e#>)|}]
  | RNeg e ->
      let e = print_rvalue e in
      [%pla {|(-<#e#>)|}]
  | RIf (cond, then_, else_) ->
      let cond = print_rvalue cond in
      let then_ = print_rvalue then_ in
      let else_ = print_rvalue else_ in
      [%pla {|(if <#cond#> then <#then_#> else <#else_#>)|}]
  | ROp (op, e1, e2) ->
      let e1 = print_rvalue e1 in
      let e2 = print_rvalue e2 in
      let op = print_op op in
      [%pla {|<#e1#> <#op#> <#e2#>|}]


let rec print_lvalue (l : lvalue) =
  match l.l with
  | LVoid -> Pla.string "_"
  | LRef (n, s) -> [%pla {|[<#n#i>:<#s#s>]|}]
  | LTuple elems ->
      let elems = Pla.map_sep Pla.commaspace print_lvalue (Array.to_list elems) in
      [%pla {|{ <#elems#> }|}]
  | LMember (e, i, s) ->
      let e = print_lvalue e in
      [%pla {|<#e#>.[<#i#i>:<#s#s>]|}]
  | LIndex (e, i) ->
      let e = print_lvalue e in
      let i = print_rvalue i in
      [%pla {|<#e#>[<#i#>]|}]


let rec print_instr (i : instr) =
  match i.i with
  | Store (lvalue, rvalue) ->
      let lvalue = print_lvalue lvalue in
      let rvalue = print_rvalue rvalue in
      [%pla {|<#lvalue#> <- <#rvalue#>|}]
  | Return rvalue ->
      let rvalue = print_rvalue rvalue in
      [%pla {|return <#rvalue#>|}]
  | If (cond, then_, else_) ->
      let cond = print_rvalue cond in
      let then_ = print_instr_list then_ in
      let else_ = print_instr_list else_ in
      [%pla {|if <#cond#><#then_#+>else<#else_#+>|}]
  | While (cond, body) ->
      let cond = print_rvalue cond in
      let body = print_instr_list body in
      [%pla {|while <#cond#><#body#+>|}]


and print_instr_list stmts = Pla.map_sep_all Pla.newline print_instr stmts

let print_segment (s : segment) =
  match s with
  | Function { name; body; locals; n_args } ->
      let body = print_instr_list body in
      [%pla {|function <#name#s> : args = <#n_args#i>, locals = <#locals#i><#body#+>|}]
  | External -> Pla.string "external"


let print_segments s = Pla.map_sep_all Pla.newline print_segment (Array.to_list s)

let print_table t =
  let f (n, i) =
    let i = f i in
    [%pla {|<#i#>: <#n#s>|}]
  in
  let elems = List.sort (fun (_, n1) (_, n2) -> compare n1 n2) (Map.to_list t) in
  Pla.map_sep_all Pla.newline f elems


let print_bytecode b =
  let table = print_table b.table in
  let segments = print_segments b.code in
  [%pla {|<#table#><#><#segments#>|}]
