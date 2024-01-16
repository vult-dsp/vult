open Core.Prog
open Util.Maps
open Code

type context =
  { ext_names : string Map.t
  ; args : Util.Args.args
  }

let rec type_ (context : context) (t : type_) =
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


and struct_descr (context : context) (d : struct_descr) : struct_descr =
  let members = List.map (fun (name, t, tags, loc) -> name, type_ context t, tags, loc) d.members in
  { path = d.path; members }


let getCallName (context : context) name =
  match Map.find_opt name context.ext_names with
  | Some name -> name
  | _ -> name


type decl =
  | Nothing
  | Initialize of string * int option * type_
  | Declare of string * int option * type_

let findRemove env n =
  match Map.find_opt n env.Code.decl with
  | Some (dim, t) ->
    let decl = Map.remove n env.Code.decl in
    Code.{ env with decl }, Some (dim, t)
  | None -> env, None


let rec getInitRHS (t : type_) =
  match t.t with
  | TVoid _ -> Some C.eunit
  | TInt -> Some (C.eint 0)
  | TReal -> Some (C.ereal 0.0)
  | TFix16 -> Some (C.efix16 0.0)
  | TString -> Some (C.estring "")
  | TBool -> Some (C.ebool false)
  | TArray (Some size, t) -> (
    match getInitRHS t with
    | Some elem ->
      let elems = List.init size (fun _ -> elem) in
      Some (C.earray elems t)
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
  let stmts = List.fold_left (fun acc (n, size, t) -> Code.StmtDecl (C.did ~size n t, getInitRHS t) :: acc) [] decl in
  env, stmts


let getNecessaryDeclarations (env : env) (e : exp) =
  let vars = GetVariables.from_exp e in
  generateInitializations env vars


let getPendingDeclarations (env : env) =
  let stmts = Map.fold (fun n (size, t) acc -> StmtDecl (C.did ~size n t, getInitRHS t) :: acc) env.decl [] in
  { env with decl = Map.empty }, stmts


let rec getLDecl (env : env) (l : lexp) =
  match l with
  | { l = LId n; _ } -> (
    match findRemove env n with
    | env, Some (dim, t) -> env, Initialize (n, dim, t)
    | env, None -> env, Nothing)
  | { l = LWild; _ } -> env, Nothing
  | { l = LMember (l, _); _ } -> (
    match getLDecl env l with
    | env, Initialize (n, dim, t) -> env, Declare (n, dim, t)
    | env, ret -> env, ret)
  | { l = LIndex { e; _ }; _ } -> (
    match getLDecl env e with
    | env, Initialize (n, dim, t) -> env, Declare (n, dim, t)
    | env, ret -> env, ret)
  | _ -> failwith "getLDecl: LTuple"


let makeBlock stmts =
  match stmts with
  | [] -> StmtBlock []
  | [ h ] -> h
  | _ -> StmtBlock stmts


let rec stmt (context : context) (env : env) (s : Core.Prog.stmt) =
  match s with
  | { s = StmtDecl { d = DId (n, dim); t; _ }; _ } ->
    let t = type_ context t in
    let decl = Map.add n (dim, t) env.decl in
    { env with decl }, []
  | { s = StmtBind ({ l = LWild; _ }, ({ e = ECall _; _ } as rhs)); _ } ->
    let env, stmts = getNecessaryDeclarations env rhs in
    env, stmts @ [ Code.StmtBind ({ l = LWild; t = C.void_t; loc = Util.Loc.default }, rhs) ]
  | { s = StmtBind ({ l = LWild; _ }, _); _ } -> env, []
  | { s = StmtBind (lhs, rhs); _ } -> (
    let env, stmts = getNecessaryDeclarations env rhs in
    match getLDecl env lhs with
    | env, Nothing -> env, stmts @ [ StmtBind (lhs, rhs) ]
    | env, Initialize (n, size, t) -> env, stmts @ [ StmtDecl (C.did ~size n t, Some rhs) ]
    | env, Declare (n, size, t) -> env, stmts @ [ StmtDecl (C.did ~size n t, getInitRHS t); StmtBind (lhs, rhs) ])
  | { s = StmtReturn e; _ } -> env, [ StmtReturn e ]
  | { s = StmtIf (cond, then_, Some else_); _ } ->
    let env, stmts = getPendingDeclarations env in
    let env, then_ = stmt context env then_ in
    let env, else_ = stmt context env else_ in
    env, stmts @ [ StmtIf (cond, makeBlock then_, Some (makeBlock else_)) ]
  | { s = StmtIf (cond, then_, None); _ } ->
    let env, stmts = getPendingDeclarations env in
    let env, then_ = stmt context env then_ in
    env, stmts @ [ StmtIf (cond, makeBlock then_, None) ]
  | { s = StmtWhile (cond, body); _ } ->
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
  | Some (StmtIf ({ e = EOp (OpEq, nid, ({ e = EInt _; _ } as i)); _ }, stmt, next)) ->
    if Compare.exp id nid = 0 then tryCreateSwitchLoop id ((i, stmt) :: cases) next else None
  | Some def -> Some (List.rev cases, Some def)


let tryCreateSwitch e =
  match e with
  | StmtIf ({ e = EOp (OpEq, id, ({ e = EInt _; _ } as i)); _ }, stmt, next) -> (
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


let function_info (info : Core.Prog.function_info) : function_info =
  { original_name = info.original_name; is_root = info.is_root }


let function_def (context : context) (def : Core.Prog.function_def) : function_def =
  let name = def.name in
  let args = List.map (fun (name, t, _) -> name, type_ context t) def.args in
  let args_t, ret_t = def.t in
  let ret_t = type_ context ret_t in
  let args_t = List.map (type_ context) args_t in
  { name; args; t = args_t, ret_t; tags = def.tags; loc = def.loc; info = function_info def.info }


let top_stmt (context : context) (top : Core.Prog.top_stmt) : top_stmt option =
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


let registerExternalNames (stmts : Core.Prog.top_stmt list) =
  List.fold_left
    (fun acc s ->
      match s with
      | Core.Prog.{ top = TopExternal (def, Some name); _ } -> Map.add def.name name acc
      | _ -> acc)
    Map.empty
    stmts


let prog args stmts =
  let ext_names = registerExternalNames stmts in
  let context = { args; ext_names } in
  List.filter_map (top_stmt context) stmts
