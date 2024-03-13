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
open Core
open Util.Maps

type code = Core.Prog.top_stmt list

type env =
  { decl : (int option * type_) Map.t
  ; dummy : int
  }

let default_env = { decl = Map.empty; dummy = 0 }

type context =
  { ext_names : string Map.t
  ; args : Util.Args.args
  }

let getExtCall (context : context) name =
  match Map.find_opt name context.ext_names with
  | Some name -> name
  | _ -> name


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
  let stmts = List.fold_left (fun acc (n, size, t) -> C.sdecl_init ~size n (getInitRHS t) t :: acc) [] decl in
  env, stmts


let getNecessaryDeclarations (env : env) (e : exp) =
  let vars = Core.Passes.GetVariables.in_exp e in
  generateInitializations env (Set.to_list vars)


let getPendingDeclarations (env : env) =
  let stmts = Map.fold (fun n (size, t) acc -> C.sdecl_init ~size n (getInitRHS t) t :: acc) env.decl [] in
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


module ApplyReplacements = struct
  let getReplacement code name (args : exp list) ret =
    let args_t = List.map (fun (e : exp) -> e.t) args in
    match Replacements.fun_to_fun code name args_t ret with
    | Some path -> path
    | None -> Replacements.keyword code name


  let exp =
    Mapper.make
    @@ fun (env : context) state (e : exp) ->
    match e with
    | { e = EId name; _ } ->
      let name = Replacements.keyword env.args.code name in
      state, { e with e = EId name }
    | { e = EOp (op, e1, e2); _ } -> (
      match Replacements.op_to_fun env.args.code op e1.t e2.t e.t with
      | Some path -> state, { e with e = ECall { path; args = [ e1; e2 ] } }
      | None -> state, e)
    | { e = ECall { path; args }; t = ret; _ } ->
      let path = getReplacement env.args.code path args ret in
      let path = getExtCall env path in
      state, { e with e = ECall { path; args } }
    | _ -> state, e


  let lexp =
    Mapper.make
    @@ fun (env : context) state (e : lexp) ->
    match e with
    | { l = LId name; _ } ->
      let name = Replacements.keyword env.args.code name in
      state, { e with l = LId name }
    | _ -> state, e


  let dexp =
    Mapper.make
    @@ fun (env : context) state (e : dexp) ->
    match e with
    | { d = DId (name, dim); _ } ->
      let name = Replacements.keyword env.args.code name in
      state, { e with d = DId (name, dim) }


  let param =
    Mapper.make
    @@ fun (env : context) state (p : param) ->
    let name = Replacements.keyword env.args.code p.name in
    state, { p with name }


  let mapper = { Mapper.identity with exp; lexp; dexp; param }
end

let makeBlock stmts =
  match stmts with
  | [] -> C.sblock []
  | [ h ] -> h
  | _ -> C.sblock stmts


let rec stmt (context : context) (env : env) (s : stmt) : 'a * stmt list =
  match s with
  | { s = StmtDecl ({ d = DId _; _ }, Some _); _ } -> env, [ s ]
  | { s = StmtDecl ({ d = DId (n, dim); t; _ }, None); _ } ->
    let decl = Map.add n (dim, t) env.decl in
    { env with decl }, []
  | { s = StmtBind ({ l = LWild; _ }, ({ e = ECall _; _ } as rhs)); _ } ->
    let env, stmts = getNecessaryDeclarations env rhs in
    env, stmts @ [ C.sbind_wild rhs ]
  | { s = StmtBind ({ l = LWild; _ }, _); _ } -> env, []
  | { s = StmtBind (lhs, rhs); _ } -> (
    let env, stmts = getNecessaryDeclarations env rhs in
    match getLDecl env lhs with
    | env, Nothing -> env, stmts @ [ C.sbind lhs rhs ]
    | env, Initialize (n, size, t) -> env, stmts @ [ C.sdecl ~size ~init:rhs n t ]
    | env, Declare (n, size, t) -> env, stmts @ [ C.sdecl_init ~size n (getInitRHS t) t; C.sbind lhs rhs ])
  | { s = StmtReturn e; _ } -> env, [ C.sreturn e ]
  | { s = StmtIf (cond, then_, Some else_); _ } ->
    let env, stmts = getPendingDeclarations env in
    let env, then_ = stmt context env then_ in
    let env, else_ = stmt context env else_ in
    env, stmts @ [ C.sif cond (makeBlock then_) (Some (makeBlock else_)) ]
  | { s = StmtIf (cond, then_, None); _ } ->
    let env, stmts = getPendingDeclarations env in
    let env, then_ = stmt context env then_ in
    env, stmts @ [ C.sif cond (makeBlock then_) None ]
  | { s = StmtWhile (cond, body); _ } ->
    let env, stmts = getPendingDeclarations env in
    let env, body = stmt context env body in
    env, stmts @ [ C.swhile cond (makeBlock body) ]
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
  | { s = StmtSwitch (cond, cases, default); _ } ->
    let env, stmts = getPendingDeclarations env in
    let env, cases_rev =
      List.fold_left
        (fun (env, acc) (e, case) ->
          let env, case = stmt context env case in
          env, (e, makeBlock case) :: acc)
        (env, [])
        cases
    in
    let env, default =
      match default with
      | None -> env, None
      | Some default ->
        let env, default = stmt context env default in
        env, Some (makeBlock default)
    in
    env, stmts @ [ C.sswitch cond (List.rev cases_rev) default ]


and stmt_list (context : context) (env : env) stmts =
  let env, stmts_rev =
    List.fold_left
      (fun (env, acc) s ->
        let env, stmts = stmt context env s in
        env, stmts :: acc)
      (env, [])
      stmts
  in
  env, List.rev stmts_rev


let rec tryCreateSwitchLoop id cases (next : stmt option) =
  match next with
  | None -> Some (List.rev cases, None)
  | Some { s = StmtIf ({ e = EOp (OpEq, nid, ({ e = EInt _; _ } as i)); _ }, stmt, next); _ } ->
    if Compare.exp id nid = 0 then tryCreateSwitchLoop id ((i, stmt) :: cases) next else None
  | Some def -> Some (List.rev cases, Some def)


let tryCreateSwitch e =
  match e with
  | { s = StmtIf ({ e = EOp (OpEq, id, ({ e = EInt _; _ } as i)); _ }, stmt, next); _ } -> (
    match tryCreateSwitchLoop id [ i, stmt ] next with
    | Some ((_ :: _ :: _ as cases), def) -> C.sswitch id cases def
    | _ -> e)
  | _ -> e


let makeSingleBlock stmts =
  match stmts with
  | [] -> C.sblock []
  | [ stmt ] -> stmt
  | _ -> C.sblock stmts


let rec createSwitch (block : stmt) =
  match block.s with
  | StmtBlock stmts ->
    let stmts = createSwitchList stmts in
    makeSingleBlock stmts
  | StmtIf (cond, then_, else_) -> (
    let block = C.sif cond (createSwitch then_) else_ in
    match tryCreateSwitch block with
    | { s = StmtIf (cond, then_, Some else_); _ } -> C.sif cond then_ (Some (createSwitch else_))
    | { s = StmtSwitch (e, cases, def); _ } ->
      C.sswitch e (List.map (fun (cond, body) -> cond, createSwitch body) cases) (Option.map createSwitch def)
    | result -> result)
  | StmtWhile (cond, body) -> C.swhile cond (createSwitch body)
  | _ -> block


and createSwitchList stmts =
  match stmts with
  | [] -> []
  | h :: t -> createSwitch h :: createSwitchList t


let function_info (info : Core.Prog.function_info) : function_info =
  { original_name = info.original_name; is_root = info.is_root }


let function_def (context : context) (def : Core.Prog.function_def) : function_def =
  let name = Replacements.keyword context.args.code def.name in
  let args = def.args in
  let args_t, ret_t = def.t in
  { name; args; t = args_t, ret_t; tags = def.tags; loc = def.loc; info = function_info def.info }


let top_stmt (context : context) (top : Core.Prog.top_stmt) : top_stmt option =
  match top with
  | { top = TopFunction (def, _); _ } when Pparser.Ptags.has def.tags "placeholder" -> None
  | { top = TopFunction (def, body); loc } ->
    let _, body = stmt context default_env body in
    let def = function_def context def in
    Some { top = TopFunction (def, createSwitch (makeBlock body)); loc }
  | { top = TopType descr; loc } -> Some { top = TopType descr; loc }
  | { top = TopAlias { path; alias_of }; loc } -> Some { top = TopAlias { path; alias_of }; loc }
  | { top = TopExternal (def, name); loc } ->
    let def = function_def context def in
    Some { top = TopExternal (def, name); loc }
  | { top = TopConstant (path, dims, t, e); loc } ->
    let path = Replacements.keyword context.args.code path in
    Some { top = TopConstant (path, dims, t, e); loc }


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
  let _, stmts = (Mapper.mapper_list Mapper.top_stmt) ApplyReplacements.mapper context (Mapper.defaultState ()) stmts in
  let stmts = List.flatten stmts in
  List.filter_map (top_stmt context) stmts
