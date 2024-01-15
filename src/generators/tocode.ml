open Core
open Util.Maps
open Code

type context =
  { ext_names : string Map.t
  ; args : Util.Args.args
  }

let rec type_ (context : context) (t : Prog.type_) =
  match t with
  | { t = TReal; _ } when context.args.real = Fixed -> { t with t = TFix16 }
  | { t = TVoid (Some elems); _ } ->
    let elems = List.map (type_ context) elems in
    { t with t = TVoid (Some elems) }
  | { t = TArray (None, sub); _ } ->
    let sub = type_ context sub in
    { t with t = TArray (None, sub) }
  | { t = TArray (Some dim, sub); _ } ->
    let sub = type_ context sub in
    { t with t = TArray (Some dim, sub) }
  | { t = TStruct descr; _ } ->
    let descr = struct_descr context descr in
    { t with t = TStruct descr }
  | { t = TTuple elems; _ } ->
    let elems = List.map (type_ context) elems in
    { t with t = TTuple elems }
  | _ -> t


and struct_descr (context : context) (d : Prog.struct_descr) : Prog.struct_descr =
  let members = List.map (fun (name, t, tags, loc) -> name, type_ context t, tags, loc) d.members in
  { path = d.path; members }


let operator (op : Prog.operator) : operator =
  match op with
  | OpAdd -> Add
  | OpSub -> Sub
  | OpMul -> Mul
  | OpDiv -> Div
  | OpMod -> Mod
  | OpLand -> Land
  | OpLor -> Lor
  | OpBor -> Bor
  | OpBand -> Band
  | OpBxor -> Bxor
  | OpLsh -> Lsh
  | OpRsh -> Rsh
  | OpEq -> Eq
  | OpNe -> Ne
  | OpLt -> Lt
  | OpLe -> Le
  | OpGt -> Gt
  | OpGe -> Ge


let uoperator (op : Prog.uoperator) : uoperator =
  match op with
  | UOpNeg -> Neg
  | UOpNot -> Not


let getCallName (context : context) name =
  match Map.find_opt name context.ext_names with
  | Some name -> name
  | _ -> name


let rec exp_d (context : context) (e : Prog.exp_d) : exp_d =
  match e with
  | EUnit -> Unit
  | EBool v -> Bool v
  | EInt v -> Int v
  | EReal v when context.args.real = Fixed -> Fixed v
  | EReal v -> Real v
  | EFixed v -> Fixed v
  | EString s -> String s
  | EId id -> Id id
  | EIndex { e; index } ->
    let e = exp context e in
    let index = exp context index in
    Index { e; index }
  | EArray elems ->
    let elems = List.map (exp context) elems in
    Array elems
  | ECall { path; args } ->
    let args = List.map (exp context) args in
    Call { path = getCallName context path; args }
  | EUnOp (op, e) ->
    let op = uoperator op in
    let e = exp context e in
    UnOp (op, e)
  | EOp (op, e1, e2) ->
    let op = operator op in
    let e1 = exp context e1 in
    let e2 = exp context e2 in
    Op (op, e1, e2)
  | EIf { cond; then_; else_ } ->
    let cond = exp context cond in
    let then_ = exp context then_ in
    let else_ = exp context else_ in
    If { cond; then_; else_ }
  | ETuple elems ->
    let elems = List.map (exp context) elems in
    Tuple elems
  | EMember (e, n) ->
    let e = exp context e in
    Member (e, n)
  | ETMember (e, n) ->
    let e = exp context e in
    TMember (e, n)


and exp (context : context) (e : Prog.exp) : exp =
  let t = type_ context e.t in
  let e = exp_d context e.e in
  { e; t }


let rec lexp_d (context : context) (l : Prog.lexp_d) : lexp_d =
  match l with
  | LWild -> LWild
  | LId id -> LId id
  | LMember (le, n) ->
    let le = lexp context le in
    LMember (le, n)
  | LIndex { e; index } ->
    let e = lexp context e in
    let index = exp context index in
    LIndex (e, index)
  | LTuple _ -> failwith "lhs tuple"


and lexp (context : context) (l : Prog.lexp) : lexp =
  let t = type_ context l.t in
  let l = lexp_d context l.l in
  { l; t }


let rec dexp_d (d : Prog.dexp_d) : dexp_d =
  match d with
  | DWild -> failwith "There should not be wild declarations"
  | DId (id, dim) -> DId (id, dim)
  | DTuple _ -> failwith "There should not be tuple declarations"


and dexp (context : context) (d : Prog.dexp) : dexp =
  let t = type_ context d.t in
  let d = dexp_d d.d in
  { d; t }


type decl =
  | Nothing
  | Initialize of string * int option * type_
  | Declare of string * int option * type_

let findRemove env n =
  match Map.find_opt n env.decl with
  | Some (dim, t) ->
    let decl = Map.remove n env.decl in
    { env with decl }, Some (dim, t)
  | None -> env, None


let rec getInitRHS (t : type_) =
  match t.t with
  | Prog.TVoid _ -> Some { e = Unit; t }
  | TInt -> Some { e = Int 0; t }
  | TReal -> Some { e = Real 0.0; t }
  | TFix16 -> Some { e = Real 0.0; t }
  | TString -> Some { e = String ""; t }
  | TBool -> Some { e = Bool false; t }
  | TArray (Some size, t) -> (
    match getInitRHS t with
    | Some elem ->
      let elems = List.init size (fun _ -> elem) in
      Some { e = Array elems; t }
    | None -> None)
  | _ -> None


let generateInitializations env vars =
  let decl, env =
    List.fold_left
      (fun (acc, env) var ->
        match findRemove env var with
        | env, None -> acc, env
        | env, Some (dim, t) -> (var, dim, t) :: acc, env)
      ([], env)
      vars
  in
  let stmts = List.fold_left (fun acc (n, dim, t) -> StmtDecl ({ d = DId (n, dim); t }, getInitRHS t) :: acc) [] decl in
  env, stmts


let getNecessaryDeclarations (env : env) (e : exp) =
  let vars = GetVariables.from_exp e in
  generateInitializations env vars


let getPendingDeclarations (env : env) =
  let stmts = Map.fold (fun n (dim, t) acc -> StmtDecl ({ d = DId (n, dim); t }, getInitRHS t) :: acc) env.decl [] in
  { env with decl = Map.empty }, stmts


let rec getLDecl (env : env) (l : lexp) =
  match l with
  | { l = LId n; _ } -> (
    match findRemove env n with
    | env, Some (dim, t) -> env, Initialize (n, dim, t)
    | env, None -> env, Nothing)
  | { l = LWild; _ } -> env, Nothing
  | { l = LMember (l, _); _ } | { l = LIndex (l, _); _ } -> (
    match getLDecl env l with
    | env, Initialize (n, dim, t) -> env, Declare (n, dim, t)
    | env, ret -> env, ret)


let makeBlock stmts =
  match stmts with
  | [] -> StmtBlock []
  | [ h ] -> h
  | _ -> StmtBlock stmts


let rec stmt (context : context) (env : env) (s : Prog.stmt) =
  match s with
  | { s = StmtDecl { d = DId (n, dim); t; _ }; _ } ->
    let t = type_ context t in
    let decl = Map.add n (dim, t) env.decl in
    { env with decl }, []
  | { s = StmtDecl _; _ } -> failwith "there should not be tuples or wild"
  | { s = StmtBind ({ l = LWild; _ }, ({ e = ECall _; _ } as rhs)); _ } ->
    let rhs = exp context rhs in
    let env, stmts = getNecessaryDeclarations env rhs in
    env, stmts @ [ StmtBind ({ l = LWild; t = Prog.C.void_t }, rhs) ]
  | { s = StmtBind ({ l = LWild; _ }, _); _ } -> env, []
  | { s = StmtBind (lhs, rhs); _ } -> (
    let lhs = lexp context lhs in
    let rhs = exp context rhs in
    let env, stmts = getNecessaryDeclarations env rhs in
    match getLDecl env lhs with
    | env, Nothing -> env, stmts @ [ StmtBind (lhs, rhs) ]
    | env, Initialize (n, dim, t) -> env, stmts @ [ StmtDecl ({ d = DId (n, dim); t }, Some rhs) ]
    | env, Declare (n, dim, t) -> env, stmts @ [ StmtDecl ({ d = DId (n, dim); t }, getInitRHS t); StmtBind (lhs, rhs) ]
    )
  | { s = StmtReturn e; _ } ->
    let e = exp context e in
    env, [ StmtReturn e ]
  | { s = StmtIf (cond, then_, Some else_); _ } ->
    let env, stmts = getPendingDeclarations env in
    let cond = exp context cond in
    let env, then_ = stmt context env then_ in
    let env, else_ = stmt context env else_ in
    env, stmts @ [ StmtIf (cond, makeBlock then_, Some (makeBlock else_)) ]
  | { s = StmtIf (cond, then_, None); _ } ->
    let cond = exp context cond in
    let env, stmts = getPendingDeclarations env in
    let env, then_ = stmt context env then_ in
    env, stmts @ [ StmtIf (cond, makeBlock then_, None) ]
  | { s = StmtWhile (cond, body); _ } ->
    let cond = exp context cond in
    let env, stmts = getPendingDeclarations env in
    let env, body = stmt context env body in
    env, stmts @ [ StmtWhile (cond, makeBlock body) ]
  | { s = StmtBlock body; _ } ->
    let env, stmts =
      List.fold_left
        (fun (env, acc) s ->
          let env, stmts = stmt context env s in
          env, stmts :: acc)
        (env, [])
        body
    in
    env, List.flatten (List.rev stmts)


let rec tryCreateSwitchLoop id cases next =
  match next with
  | None -> Some (List.rev cases, None)
  | Some (StmtIf ({ e = Op (Eq, nid, ({ e = Int _; _ } as i)); _ }, stmt, next)) ->
    if compare id nid = 0 then tryCreateSwitchLoop id ((i, stmt) :: cases) next else None
  | Some def -> Some (List.rev cases, Some def)


let tryCreateSwitch e =
  match e with
  | StmtIf ({ e = Op (Eq, id, ({ e = Int _; _ } as i)); _ }, stmt, next) -> (
    match tryCreateSwitchLoop id [ i, stmt ] next with
    | Some ((_ :: _ :: _ as cases), def) -> StmtSwitch (id, cases, def)
    | _ -> e)
  | _ -> e


let makeSingleBlock stmts =
  match stmts with
  | [] -> StmtBlock []
  | [ stmt ] -> stmt
  | _ -> StmtBlock stmts


let rec createSwitch block =
  match block with
  | StmtBlock stmts ->
    let stmts = createSwitchList stmts in
    makeSingleBlock stmts
  | StmtIf (cond, then_, else_) -> (
    let block = StmtIf (cond, createSwitch then_, else_) in
    match tryCreateSwitch block with
    | StmtIf (cond, then_, Some else_) -> StmtIf (cond, then_, Some (createSwitch else_))
    | StmtSwitch (e, cases, def) ->
      StmtSwitch (e, List.map (fun (cond, body) -> cond, createSwitch body) cases, Option.map createSwitch def)
    | result -> result)
  | StmtWhile (cond, body) -> StmtWhile (cond, createSwitch body)
  | _ -> block


and createSwitchList stmts =
  match stmts with
  | [] -> []
  | h :: t -> createSwitch h :: createSwitchList t


let function_info (info : Prog.function_info) : function_info =
  { original_name = info.original_name; is_root = info.is_root }


let function_def (context : context) (def : Prog.function_def) : function_def =
  let name = def.name in
  let args = List.map (fun (name, t, _) -> name, type_ context t) def.args in
  let args_t, ret_t = def.t in
  let ret_t = type_ context ret_t in
  let args_t = List.map (type_ context) args_t in
  { name; args; t = args_t, ret_t; tags = def.tags; loc = def.loc; info = function_info def.info }


let top_stmt (context : context) (top : Prog.top_stmt) : top_stmt option =
  match top with
  | { top = TopFunction (def, _); _ } when Pparser.Ptags.has def.tags "placeholder" -> None
  | { top = TopFunction (def, body); loc } ->
    let _, body = stmt context default_env body in
    let def = function_def context def in
    Some { top = TopFunction (def, createSwitch (makeBlock body)); loc }
  | { top = TopType descr; loc } ->
    let descr = struct_descr context descr in
    Some { top = TopType descr; loc }
  | { top = TopAlias { path; alias_of }; loc } -> Some { top = TopAlias (path, alias_of); loc }
  | { top = TopExternal (def, name); loc } ->
    let def = function_def context def in
    Some { top = TopExternal (def, name); loc }


let registerExternalNames (stmts : Prog.top_stmt list) =
  List.fold_left
    (fun acc s ->
      match s with
      | Prog.{ top = TopExternal (def, Some name); _ } -> Map.add def.name name acc
      | _ -> acc)
    Map.empty
    stmts


let prog args stmts =
  let ext_names = registerExternalNames stmts in
  let context = { args; ext_names } in
  List.filter_map (top_stmt context) stmts
