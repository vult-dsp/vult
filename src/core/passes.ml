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

open Prog
open Util.Maps

type data =
  { repeat : bool
  ; ticks : (string, int) Hashtbl.t
  ; function_deps : Set.t Map.t
  ; type_deps : Set.t Map.t
  }

type env =
  { in_if_exp : bool
  ; in_function : bool
  ; bound_if : bool
  ; bound_call : bool
  ; bound_array : bool
  ; bound_record : bool
  ; current_function : function_def option
  ; current_type : struct_descr option
  ; args : Util.Args.args
  }

type enabled_disabled =
  | Enabled
  | Disabled

let default_data () : data =
  { repeat = false; ticks = Hashtbl.create 16; function_deps = Map.empty; type_deps = Map.empty }


let default_env args : env =
  { args
  ; in_if_exp = false
  ; in_function = false
  ; bound_if = false
  ; current_function = None
  ; bound_call = false
  ; bound_array = false
  ; bound_record = false
  ; current_type = None
  }


let reapply (state : data Mapper.state) =
  let data = Mapper.getData state in
  Mapper.setData state { data with repeat = true }


let currentFunction env =
  match env.current_function with
  | None -> failwith "not in a function"
  | Some { name; args = { name = ctx; t; _ } :: _; _ } -> name, ctx, t
  | Some _ -> failwith "function has no context"


let isValue (e : exp) =
  match e.e with
  | EReal _ | EInt _ | EBool _ | EFixed _ -> true
  | _ -> false


let getTick (env : env) (state : data Mapper.state) =
  let name =
    match env.current_function with
    | None -> ""
    | Some def -> def.name
  in
  let data = Mapper.getData state in
  match Hashtbl.find_opt data.ticks name with
  | None ->
    Hashtbl.add data.ticks name 1;
    0
  | Some n ->
    Hashtbl.replace data.ticks name (n + 1);
    n


module CollectDependencies = struct
  let initializeDeps map name =
    let set =
      match Map.find_opt name map with
      | None -> Set.empty
      | Some set -> set
    in
    Map.add name set map


  let addFunctionDep (state : data Mapper.state) name dep =
    let data = Mapper.getData state in
    let set =
      match Map.find_opt name data.function_deps with
      | None -> Set.empty
      | Some set -> set
    in
    let set = Set.add dep set in
    let function_deps = Map.add name set data.function_deps in
    let data = { data with function_deps } in
    Mapper.setData state data


  let addTypeDep (state : data Mapper.state) name dep =
    let data = Mapper.getData state in
    let set =
      match Map.find_opt name data.type_deps with
      | None -> Set.empty
      | Some set -> set
    in
    let set = Set.add dep set in
    let type_deps = Map.add name set data.type_deps in
    let data = { data with type_deps } in
    Mapper.setData state data


  let exp =
    Mapper.make
    @@ fun env state (e : exp) ->
    match e with
    | { e = ECall { path; _ }; _ } -> (
      match env.current_function with
      | None -> state, e
      | Some def ->
        let state = addFunctionDep state def.name path in
        state, e)
    | _ -> state, e


  let type_ =
    Mapper.make
    @@ fun env state (p : type_) ->
    match p with
    | { t = TStruct { path; _ }; _ } -> (
      match env.current_type with
      | None -> state, p
      | Some { path = name; _ } ->
        let state = addTypeDep state name path in
        state, p)
    | _ -> state, p


  let top_stmt =
    Mapper.makeExpander
    @@ fun _env state (top : top_stmt) ->
    let data = Mapper.getData state in
    match top with
    | { top = TopType { path; _ }; _ } ->
      let type_deps = initializeDeps data.type_deps path in
      let data = { data with type_deps } in
      Mapper.setData state data, [ top ]
    | { top = TopFunction ({ name; _ }, _); _ } ->
      let function_deps = initializeDeps data.function_deps name in
      let data = { data with function_deps } in
      Mapper.setData state data, [ top ]
    | _ -> state, [ top ]


  let mapper = { Mapper.identity with exp; type_; top_stmt }
end

module GetVariables = struct
  let exp =
    Mapper.make
    @@ fun _env (state : Set.t Mapper.state) (e : exp) ->
    match e with
    | { e = EId name; _ } ->
      let data = Mapper.getData state in
      Mapper.setData state (Set.add name data), e
    | _ -> state, e


  let lexp =
    Mapper.make
    @@ fun _env (state : Set.t Mapper.state) (e : lexp) ->
    match e with
    | { l = LId name; _ } ->
      let data = Mapper.getData state in
      Mapper.setData state (Set.add name data), e
    | _ -> state, e


  let dexp =
    Mapper.make
    @@ fun _env (state : Set.t Mapper.state) (e : dexp) ->
    match e with
    | { d = DId (name, _); _ } ->
      let data = Mapper.getData state in
      Mapper.setData state (Set.add name data), e


  let mapper = { Mapper.identity with exp; lexp; dexp }

  let in_exp (e : exp) =
    let state, _ = Mapper.exp mapper () (Mapper.defaultState Set.empty) e in
    Mapper.getData state


  let in_lexp (e : lexp) =
    let state, _ = Mapper.lexp mapper () (Mapper.defaultState Set.empty) e in
    Mapper.getData state


  let in_stmts (s : stmt list) =
    let state, _ = Mapper.mapper_list_expand Mapper.stmt mapper () (Mapper.defaultState Set.empty) s in
    Mapper.getData state
end

module Location = struct
  let top_stmt_env =
    Mapper.makeEnv
    @@ fun env (s : top_stmt) ->
    match s with
    | { top = TopFunction (def, _); _ } -> { env with current_function = Some def }
    | { top = TopType def; _ } -> { env with current_type = Some def }
    | _ -> env


  let exp_env =
    Mapper.makeEnv
    @@ fun env (e : exp) ->
    match e with
    | { e = EIf _; _ } -> { env with in_if_exp = true }
    | _ -> env


  let mapper = { Mapper.identity with top_stmt_env; exp_env }
end

module IfExpressions = struct
  (* Check if an expression is simple enough to remain as a ternary operator *)
  let rec isSimpleValue (e : exp) =
    match e.e with
    | EUnit | EBool _ | EInt _ | EReal _ | EFixed _ | EString _ -> true
    | EId _ | EMember _ -> true
    | EUnOp (_, e) -> isSimpleValue e
    | _ -> false


  let isSimpleIf (e : exp) =
    match e.e with
    | EIf { cond; then_; else_ } -> isSimpleValue cond && isSimpleValue then_ && isSimpleValue else_
    | _ -> false


  let stmt_env =
    Mapper.makeEnv
    @@ fun env (s : stmt) ->
    match s with
    | { s = StmtBind (_, { e = EIf _; _ }); _ } -> { env with bound_if = true }
    | { s = StmtReturn { e = EIf _; _ }; _ } -> { env with bound_if = true }
    | _ -> env


  let stmt =
    Mapper.makeExpander
    @@ fun _env state (s : stmt) ->
    match s with
    (* if (false) -> else *)
    | { s = StmtIf ({ e = EBool false; _ }, _, Some else_); _ } -> reapply state, [ else_ ]
    (* if (true) -> then *)
    | { s = StmtIf ({ e = EBool true; _ }, then_, _); _ } -> reapply state, [ then_ ]
    (* Convert else if (true) -> else*)
    | { s = StmtIf (cond, then_, Some { s = StmtIf ({ e = EBool true; _ }, else_, None); _ }); loc } ->
      reapply state, [ C.sif ~loc cond then_ (Some else_) ]
    | { s = StmtBind (lhs, { e = EIf { cond; then_; else_ }; _ }); loc } ->
      let then_ = { s = StmtBind (lhs, then_); loc } in
      let else_ = { s = StmtBind (lhs, else_); loc } in
      reapply state, [ { s = StmtIf (cond, then_, Some else_); loc } ]
    | { s = StmtReturn { e = EIf { cond; then_; else_ }; _ }; loc } ->
      let then_ = { s = StmtReturn then_; loc } in
      let else_ = { s = StmtReturn else_; loc } in
      reapply state, [ { s = StmtIf (cond, then_, Some else_); loc } ]
    | _ -> state, [ s ]


  let exp =
    Mapper.make
    @@ fun env state (e : exp) ->
    match e with
    (* Evaluates if-expressions with constant condition *)
    | { e = EIf { cond = { e = EBool cond; _ }; then_; else_ }; _ } ->
      ( reapply state
      , if cond then
          then_
        else
          else_ )
    (* Preserve simple if-expressions as ternary operators *)
    | { e = EIf _; _ } when isSimpleIf e -> state, e
    (* Bind if-expressions to a variable in function context *)
    | { e = EIf _; t; loc } when (not env.in_if_exp) && (not env.bound_if) && env.in_function ->
      let tick = getTick env state in
      let temp = "_if_temp_" ^ string_of_int tick in
      let temp_e = { e = EId temp; t; loc } in
      let decl_stmt = { s = StmtDecl ({ d = DId (temp, None); t; loc }, None); loc } in
      let bind_stmt = { s = StmtBind ({ l = LId temp; t; loc }, e); loc } in
      let state = Mapper.pushStmts state [ decl_stmt; bind_stmt ] in
      reapply state, temp_e
    | _ -> state, e


  let mapper enabled =
    if enabled = Enabled then
      { Mapper.identity with stmt; exp; stmt_env }
    else
      Mapper.identity
end

module LiteralRecords = struct
  let stmt_env =
    Mapper.makeEnv
    @@ fun env (s : stmt) ->
    match s with
    | { s = StmtBind (_, { e = ERecord _; _ }); _ } -> { env with bound_record = true }
    | _ -> env


  let top_stmt_env =
    Mapper.makeEnv
    @@ fun env (s : top_stmt) ->
    match s with
    | { top = TopConstant (_, _, _, { e = ERecord _; _ }, _); _ } -> { env with bound_record = true }
    | _ -> env


  let exp =
    Mapper.make
    @@ fun env state (e : exp) ->
    match e with
    (* Bind records to a variable in the context of functions *)
    | { e = ERecord _; t; loc } when (not env.in_if_exp) && (not env.bound_record) && env.in_function ->
      let tick = getTick env state in
      let temp = "_record_" ^ string_of_int tick in
      let temp_e = { e = EId temp; t; loc } in
      let decl_stmt = { s = StmtDecl ({ d = DId (temp, None); t; loc }, None); loc } in
      let bind_stmt = { s = StmtBind ({ l = LId temp; t; loc }, e); loc } in
      let state = Mapper.pushStmts state [ decl_stmt; bind_stmt ] in
      reapply state, temp_e
    | { e = ERecord _; t; loc } when (not env.in_if_exp) && not env.bound_record ->
      let tick = getTick env state in
      let temp = "_record_" ^ string_of_int tick in
      let temp_e = { e = EId temp; t; loc } in
      let constant_decl = { top = TopConstant (temp, None, t, e, None); loc } in
      let state = Mapper.pushTopStmts state [ constant_decl ] in
      reapply state, temp_e
    | _ -> state, e


  let mapper enabled =
    if enabled = Enabled then
      { Mapper.identity with exp; stmt_env; top_stmt_env }
    else
      Mapper.identity
end

module Markers = struct
  let top_stmt_env =
    Mapper.makeEnv
    @@ fun env (s : top_stmt) ->
    match s with
    | { top = TopFunction _; _ } -> { env with in_function = true }
    | _ -> env


  let mapper enabled =
    if enabled = Enabled then
      { Mapper.identity with top_stmt_env }
    else
      Mapper.identity
end

module LiteralArrays = struct
  let stmt_env =
    Mapper.makeEnv
    @@ fun env (s : stmt) ->
    match s with
    | { s = StmtBind (_, { e = EArray _; _ }); _ } -> { env with bound_array = true }
    | _ -> env


  let top_stmt_env =
    Mapper.makeEnv
    @@ fun env (s : top_stmt) ->
    match s with
    | { top = TopConstant (_, _, _, { e = EArray _; _ }, _); _ } -> { env with bound_array = true }
    | _ -> env


  let exp =
    Mapper.make
    @@ fun env state (e : exp) ->
    match e with
    (* Bind arrays to a variable *)
    | { e = EArray _; t; loc } when (not env.in_if_exp) && (not env.bound_array) && env.in_function ->
      let tick = getTick env state in
      let temp = "_array_" ^ string_of_int tick in
      let temp_e = { e = EId temp; t; loc } in
      let decl_stmt = { s = StmtDecl ({ d = DId (temp, None); t; loc }, None); loc } in
      let bind_stmt = { s = StmtBind ({ l = LId temp; t; loc }, e); loc } in
      let state = Mapper.pushStmts state [ decl_stmt; bind_stmt ] in
      reapply state, temp_e
    | _ -> state, e


  let mapper enabled =
    if enabled = Enabled then
      { Mapper.identity with exp; stmt_env; top_stmt_env }
    else
      Mapper.identity
end

module Tuples = struct
  let stmt_env =
    Mapper.makeEnv
    @@ fun env (s : stmt) ->
    match s with
    (* Mark bound multi-return functions as bound *)
    | { s = StmtBind (_, { e = ECall _; t = { t = TTuple _; _ }; _ }); _ } -> { env with bound_call = true }
    | _ -> env


  let exp =
    Mapper.make
    @@ fun env state (e : exp) ->
    match e with
    (* bind multi-return function calls *)
    | { e = ECall _; t = { t = TTuple elems; _ } as t; loc } when (not env.bound_call) && not env.in_if_exp ->
      let temp =
        CCList.map
          (fun (t : type_) ->
            let tick = getTick env state in
            "_call_temp_" ^ string_of_int tick, t)
          elems
      in
      let decl_stmt =
        CCList.map (fun (name, t) -> { s = StmtDecl ({ d = DId (name, None); t; loc }, None); loc }) temp
      in
      let temp_l = CCList.map (fun (name, t) -> { l = LId name; t; loc }) temp in
      let bind_stmt = { s = StmtBind ({ l = LTuple temp_l; t; loc }, e); loc } in
      let state = Mapper.pushStmts state (decl_stmt @ [ bind_stmt ]) in
      let temp_e = CCList.map (fun (name, t) -> { e = EId name; t; loc }) temp in
      reapply state, { e = ETuple temp_e; t; loc }
    | _ -> state, e


  let stmt =
    Mapper.makeExpander
    @@ fun env state (s : stmt) ->
    match s with
    (* split tuple assings *)
    | { s = StmtBind (({ l = LTuple l_elems; _ } as lhs), ({ e = ETuple r_elems; _ } as rhs)); loc } ->
      let l = GetVariables.in_lexp lhs in
      let r = GetVariables.in_exp rhs in
      let d = Set.inter l r in
      if Set.is_empty d then
        let bindings = CCList.map2 (fun l r -> { s = StmtBind (l, r); loc }) l_elems r_elems in
        reapply state, bindings
      else
        let temp_list = CCList.map (fun (l : lexp) -> "_t_temp_" ^ string_of_int (getTick env state), l.t) l_elems in
        let decl = CCList.map (fun (n, t) -> { s = StmtDecl ({ d = DId (n, None); loc; t }, None); loc }) temp_list in
        let bindings1 =
          CCList.map2
            (fun (l, _) (r : exp) -> { s = StmtBind ({ l = LId l; t = r.t; loc = r.loc }, r); loc })
            temp_list
            r_elems
        in
        let bindings2 =
          CCList.map2
            (fun (l : lexp) (r, _) -> { s = StmtBind (l, { e = EId r; t = l.t; loc = l.loc }); loc })
            l_elems
            temp_list
        in
        reapply state, decl @ bindings1 @ bindings2
    (* bind multi return calls to the context *)
    | { s = StmtBind (({ l = LTuple elems; _ } as lhs), ({ e = ECall { path; args = ctx :: _ }; loc = rloc; _ } as rhs))
      ; loc
      } ->
      let bindings =
        CCList.mapi
          (fun i (l : lexp) ->
            let r = { e = EMember (ctx, path ^ "_ret_" ^ string_of_int i); t = l.t; loc = l.loc } in
            { s = StmtBind (l, r); loc })
          elems
      in
      let s =
        { s = StmtBind ({ lhs with l = LWild }, { rhs with t = { t = TVoid None; const = false; loc = rloc } }); loc }
      in
      reapply state, s :: bindings
    (* multi return calls bound to a tuple variable *)
    | { s =
          StmtBind
            ( ({ l = LId _; t = { t = TTuple types; _ }; _ } as lhs)
            , ({ e = ECall { path; args = ctx :: _ }; loc = rloc; _ } as rhs) )
      ; loc
      } ->
      let tuple_elems =
        CCList.mapi (fun i (t : type_) -> { e = EMember (ctx, path ^ "_ret_" ^ string_of_int i); t; loc }) types
      in
      let s =
        { s = StmtBind ({ lhs with l = LWild }, { rhs with t = { t = TVoid None; const = false; loc = rloc } }); loc }
      in
      let binding = { s = StmtBind (lhs, { e = ETuple tuple_elems; t = rhs.t; loc = rhs.loc }); loc } in
      reapply state, s :: [ binding ]
    (* Remove the return type of calls bound to wild *)
    | { s = StmtBind ({ l = LWild; _ }, { e = ECall _; t = { t = TVoid _; _ }; _ }); _ } -> state, [ s ]
    | { s = StmtBind (({ l = LWild; _ } as lhs), ({ e = ECall _; loc = rloc; _ } as rhs)); loc } ->
      let s =
        { s =
            StmtBind ({ lhs with l = LWild }, { rhs with t = { t = TVoid (Some [ rhs.t ]); const = false; loc = rloc } })
        ; loc
        }
      in
      reapply state, [ s ]
    (* Bind returned tupples to the environment *)
    | { s = StmtReturn { e = ETuple elems; loc = eloc; _ }; loc } ->
      let name, ctx_name, ctx_t = currentFunction env in
      let ctx = { l = LId ctx_name; t = ctx_t; loc } in
      let bindings =
        CCList.mapi
          (fun i (r : exp) ->
            let l = { l = LMember (ctx, name ^ "_ret_" ^ string_of_int i); t = r.t; loc = r.loc } in
            { s = StmtBind (l, r); loc })
          elems
      in
      let s = { s = StmtReturn { e = EUnit; t = { t = TVoid None; const = false; loc }; loc = eloc }; loc } in
      reapply state, bindings @ [ s ]
    (* Bind returned single variable tuple to the environment *)
    | { s = StmtReturn ({ e = EId _; loc = eloc; t = { t = TTuple types; _ } } as ret); loc } ->
      let name, ctx_name, ctx_t = currentFunction env in
      let ctx = { l = LId ctx_name; t = ctx_t; loc } in
      let bindings =
        CCList.mapi
          (fun i (t : type_) ->
            let l = { l = LMember (ctx, name ^ "_ret_" ^ string_of_int i); t; loc } in
            { s = StmtBind (l, { ret with e = ETMember (ret, i) }); loc })
          types
      in
      let s = { s = StmtReturn { e = EUnit; t = { t = TVoid None; const = false; loc }; loc = eloc }; loc } in
      reapply state, bindings @ [ s ]
    | _ -> state, [ s ]


  let top_stmt =
    Mapper.makeExpander
    @@ fun _env state (top : top_stmt) ->
    match top with
    | { top = TopFunction (({ t = args_t, { t = TTuple elems; loc = tloc; _ }; _ } as def), body); loc } ->
      let def = { def with t = args_t, { t = TVoid (Some elems); const = false; loc = tloc } } in
      state, [ { top = TopFunction (def, body); loc } ]
    | _ -> state, [ top ]


  let mapper enabled =
    if enabled = Enabled then
      { Mapper.identity with stmt; stmt_env; exp; top_stmt }
    else
      Mapper.identity
end

module Builtin = struct
  let exp =
    Mapper.make
    @@ fun env state (e : exp) ->
    match e with
    | { e = ECall { path = "pi"; args = [] }; _ } -> reapply state, { e with e = EReal Float.pi }
    | { e = ECall { path = "exp"; args = [ { e = EReal v; _ } ] }; _ } -> reapply state, { e with e = EReal (exp v) }
    | { e = ECall { path = "exp"; args = [ { e = EFixed v; _ } ] }; _ } -> reapply state, { e with e = EFixed (exp v) }
    | { e = ECall { path = "sin"; args = [ { e = EReal v; _ } ] }; _ } -> reapply state, { e with e = EReal (sin v) }
    | { e = ECall { path = "sin"; args = [ { e = EFixed v; _ } ] }; _ } -> reapply state, { e with e = EFixed (sin v) }
    | { e = ECall { path = "cos"; args = [ { e = EReal v; _ } ] }; _ } -> reapply state, { e with e = EReal (cos v) }
    | { e = ECall { path = "cos"; args = [ { e = EFixed v; _ } ] }; _ } -> reapply state, { e with e = EFixed (cos v) }
    | { e = ECall { path = "abs"; args = [ { e = EReal v; _ } ] }; _ } ->
      reapply state, { e with e = EReal (Float.abs v) }
    | { e = ECall { path = "abs"; args = [ { e = EFixed v; _ } ] }; _ } ->
      reapply state, { e with e = EFixed (Float.abs v) }
    | { e = ECall { path = "sqrt"; args = [ { e = EReal v; _ } ] }; _ } -> reapply state, { e with e = EReal (sqrt v) }
    | { e = ECall { path = "sqrt"; args = [ { e = EFixed v; _ } ] }; _ } ->
      reapply state, { e with e = EFixed (sqrt v) }
    | { e = ECall { path = "not"; args = [ e1 ] }; loc; _ } ->
      reapply state, { e with e = EOp (OpEq, e1, { e = EBool false; t = { t = TBool; const = false; loc }; loc }) }
    | { e = ECall { path = "size"; args = [ { t = { t = TArray (Some size, _); _ }; _ } ] }; loc; _ } ->
      reapply state, { e with e = EInt size; loc }
    | { e = ECall { path = "length"; args = [ { e = EString str; _ } ] }; loc; _ } ->
      reapply state, { e with e = EInt (String.length str); loc }
    | { e = ECall { path = "samplerate"; args = [] }; _ } -> (
      match env.args.fs with
      | Some fs -> reapply state, { e with e = EReal fs }
      | None -> state, e)
    | _ -> state, e


  let mapper enabled =
    if enabled = Enabled then
      { Mapper.identity with exp }
    else
      Mapper.identity
end

module Cast = struct
  let exp =
    Mapper.make
    @@ fun env state (e : exp) ->
    match e with
    | { e = ECall { path = "fix16"; args = [ ({ t = { t = TFix16; _ }; _ } as e1) ] }; _ } -> reapply state, e1
    | { e = ECall { path = "real"; args = [ ({ t = { t = TReal; _ }; _ } as e1) ] }; _ } -> reapply state, e1
    | { e = ECall { path = "int"; args = [ ({ t = { t = TInt; _ }; _ } as e1) ] }; _ } -> reapply state, e1
    | { e = ECall { path = "bool"; args = [ ({ t = { t = TBool; _ }; _ } as e1) ] }; _ } -> reapply state, e1
    (* casting constant inputs *)
    | { e = ECall { path = "fix16"; args = [ ({ e = EReal v; _ } as e1) ] }; t; _ } ->
      reapply state, { e1 with e = EFixed v; t }
    | { e = ECall { path = "fix16"; args = [ ({ e = EInt i; _ } as e1) ] }; t; _ } ->
      reapply state, { e1 with e = EFixed (float_of_int i); t }
    | { e = ECall { path = "fix16"; args = [ ({ e = EBool b; _ } as e1) ] }; t; _ } ->
      ( reapply state
      , { e1 with
          e =
            EFixed
              (if b then
                 1.0
               else
                 0.0)
        ; t
        } )
    | { e = ECall { path = "real"; args = [ ({ e = EFixed v; _ } as e1) ] }; t; _ } ->
      reapply state, { e1 with e = EReal v; t }
    | { e = ECall { path = "real"; args = [ ({ e = EInt i; _ } as e1) ] }; t; _ } ->
      reapply state, { e1 with e = EReal (float_of_int i); t }
    | { e = ECall { path = "real"; args = [ ({ e = EBool b; _ } as e1) ] }; t; _ } ->
      ( reapply state
      , { e1 with
          e =
            EReal
              (if b then
                 1.0
               else
                 0.0)
        ; t
        } )
    | { e = ECall { path = "int"; args = [ ({ e = EFixed v; _ } as e1) ] }; t; _ } ->
      reapply state, { e1 with e = EInt (int_of_float v); t }
    | { e = ECall { path = "int"; args = [ ({ e = EReal v; _ } as e1) ] }; t; _ } ->
      reapply state, { e1 with e = EInt (int_of_float v); t }
    | { e = ECall { path = "int"; args = [ ({ e = EBool b; _ } as e1) ] }; t; _ } ->
      ( reapply state
      , { e1 with
          e =
            EInt
              (if b then
                 1
               else
                 0)
        ; t
        } )
    | { e = ECall { path = "bool"; args = [ ({ e = EFixed v; _ } as e1) ] }; t; _ } ->
      reapply state, { e1 with e = EBool (v <> 0.0); t }
    | { e = ECall { path = "bool"; args = [ ({ e = EReal v; _ } as e1) ] }; t; _ } ->
      reapply state, { e1 with e = EBool (v <> 0.0); t }
    | { e = ECall { path = "bool"; args = [ ({ e = EInt v; _ } as e1) ] }; t; _ } ->
      reapply state, { e1 with e = EBool (v <> 0); t }
    (* Convert real type *)
    | { e = EReal n; loc; _ } when env.args.real = Fixed -> reapply state, C.efix16 ~loc n
    | _ -> state, e


  let type_ =
    Mapper.make
    @@ fun env state (t : type_) ->
    match t with
    | { t = TReal; const; loc } when env.args.real = Fixed -> state, { t = TFix16; const; loc }
    | _ -> state, t


  let mapper enabled =
    if enabled = Enabled then
      { Mapper.identity with exp; type_ }
    else
      Mapper.identity
end

module Canonize = struct
  let compare_exp e1 e2 =
    match e1.e, e2.e with
    | EInt n1, EInt n2 -> compare n1 n2
    | EInt _, _ -> -1
    | EBool n1, EBool n2 -> compare n1 n2
    | EBool _, _ -> -1
    | EReal n1, EReal n2 -> compare n1 n2
    | EReal _, _ -> -1
    | _ -> compare e1 e2


  let exp =
    Mapper.make
    @@ fun _env state e ->
    match e with
    (* do not modify string addition *)
    | { e = EOp (OpAdd, { t = { t = TString; _ }; _ }, _); _ } -> state, e
    (* (e1 op e2) op n3 -> (e1 op (e2 op n3)) *)
    | { e = EOp (op1, { e = EOp (op2, e1, e2); _ }, n3); _ } when (op1 = OpAdd || op1 = OpMul) && op1 = op2 ->
      let loc2 = Util.Loc.merge e2.loc n3.loc in
      let loc1 = Util.Loc.merge e1.loc n3.loc in
      let n2 = { e = EOp (op1, e2, n3); t = e2.t; loc = loc2 } in
      let n1 = { e = EOp (op1, e1, n2); t = e1.t; loc = loc1 } in
      reapply state, n1
    (* (e2 op (e1 op n3)) -> (e1 op (e2 op n3)) *)
    | { e = EOp (op1, e2, ({ e = EOp (op2, e1, e3); _ } as n2)); _ } when (op1 = OpAdd || op1 = OpMul) && op1 = op2 ->
      if compare_exp e2 e1 > 0 then
        let n2 = { n2 with e = EOp (op2, e2, e3) } in
        reapply state, { e with e = EOp (op1, e1, n2) }
      else
        state, e
    | { e = EOp (op, e1, e2); _ } when op = OpAdd || op = OpMul ->
      if compare_exp e1 e2 > 0 then
        reapply state, { e with e = EOp (op, e2, e1) }
      else
        state, e
    (* e1 - e2 -> e1 + (-e2) *)
    | { e = EOp (OpSub, e1, e2); _ } ->
      reapply state, { e with e = EOp (OpAdd, e1, { e2 with e = EUnOp (UOpNeg, e2) }) }
    (* - (e1 * e2) -> (-e1) * e2 *)
    | { e = EUnOp (UOpNeg, { e = EOp (OpMul, e1, e2); _ }); _ } when isValue e1 ->
      reapply state, { e with e = EOp (OpMul, { e1 with e = EUnOp (UOpNeg, e1) }, e2) }
    (* - (e1 + e2) -> (-e1) + (-e2) *)
    | { e = EUnOp (UOpNeg, { e = EOp (OpAdd, e1, e2); _ }); _ } ->
      let e1 = { e1 with e = EUnOp (UOpNeg, e1) } in
      let e2 = { e2 with e = EUnOp (UOpNeg, e2) } in
      reapply state, { e with e = EOp (OpAdd, e1, e2) }
    | _ -> state, e


  let mapper enabled =
    if enabled = Enabled then
      { Mapper.identity with exp }
    else
      Mapper.identity
end

module Simplify = struct
  let evaluate t op e1 e2 =
    match e1, e2 with
    (* boolean *)
    | e, { e = EBool true; loc; _ } | { e = EBool true; loc; _ }, e -> (
      match op with
      | OpLand -> Some e
      | OpLor -> Some (C.ebool ~loc true)
      | _ -> None)
    | e, { e = EBool false; loc; _ } | { e = EBool false; loc; _ }, e -> (
      match op with
      | OpLand -> Some (C.ebool ~loc false)
      | OpLor -> Some e
      | _ -> None)
    (* arithmetic *)
    | { e = EReal n1; _ }, { e = EReal n2; _ } -> (
      match op with
      | OpAdd -> Some { e = EReal (n1 +. n2); t = e1.t; loc = Util.Loc.merge e1.loc e2.loc }
      | OpMul -> Some { e = EReal (n1 *. n2); t = e1.t; loc = Util.Loc.merge e1.loc e2.loc }
      | OpSub -> Some { e = EReal (n1 -. n2); t = e1.t; loc = Util.Loc.merge e1.loc e2.loc }
      | OpDiv -> Some { e = EReal (n1 /. n2); t = e1.t; loc = Util.Loc.merge e1.loc e2.loc }
      | OpEq -> Some { e = EBool (n1 = n2); t; loc = Util.Loc.merge e1.loc e2.loc }
      | OpNe -> Some { e = EBool (n1 <> n2); t; loc = Util.Loc.merge e1.loc e2.loc }
      | OpLe -> Some { e = EBool (n1 <= n2); t; loc = Util.Loc.merge e1.loc e2.loc }
      | OpLt -> Some { e = EBool (n1 < n2); t; loc = Util.Loc.merge e1.loc e2.loc }
      | OpGe -> Some { e = EBool (n1 >= n2); t; loc = Util.Loc.merge e1.loc e2.loc }
      | OpGt -> Some { e = EBool (n1 > n2); t; loc = Util.Loc.merge e1.loc e2.loc }
      | _ -> None)
    | { e = EFixed n1; _ }, { e = EFixed n2; _ } -> (
      match op with
      | OpAdd -> Some { e = EFixed (n1 +. n2); t = e1.t; loc = Util.Loc.merge e1.loc e2.loc }
      | OpMul -> Some { e = EFixed (n1 *. n2); t = e1.t; loc = Util.Loc.merge e1.loc e2.loc }
      | OpSub -> Some { e = EFixed (n1 -. n2); t = e1.t; loc = Util.Loc.merge e1.loc e2.loc }
      | OpDiv -> Some { e = EFixed (n1 /. n2); t = e1.t; loc = Util.Loc.merge e1.loc e2.loc }
      | OpEq -> Some { e = EBool (n1 = n2); t; loc = Util.Loc.merge e1.loc e2.loc }
      | OpNe -> Some { e = EBool (n1 <> n2); t; loc = Util.Loc.merge e1.loc e2.loc }
      | OpLe -> Some { e = EBool (n1 <= n2); t; loc = Util.Loc.merge e1.loc e2.loc }
      | OpLt -> Some { e = EBool (n1 < n2); t; loc = Util.Loc.merge e1.loc e2.loc }
      | OpGe -> Some { e = EBool (n1 >= n2); t; loc = Util.Loc.merge e1.loc e2.loc }
      | OpGt -> Some { e = EBool (n1 > n2); t; loc = Util.Loc.merge e1.loc e2.loc }
      | _ -> None)
    | { e = EInt n1; _ }, { e = EInt n2; _ } -> (
      match op with
      | OpAdd -> Some { e = EInt (n1 + n2); t = e1.t; loc = Util.Loc.merge e1.loc e2.loc }
      | OpMul -> Some { e = EInt (n1 * n2); t = e1.t; loc = Util.Loc.merge e1.loc e2.loc }
      | OpSub -> Some { e = EInt (n1 - n2); t = e1.t; loc = Util.Loc.merge e1.loc e2.loc }
      | OpDiv -> Some { e = EInt (n1 / n2); t = e1.t; loc = Util.Loc.merge e1.loc e2.loc }
      | OpEq -> Some { e = EBool (n1 = n2); t; loc = Util.Loc.merge e1.loc e2.loc }
      | OpNe -> Some { e = EBool (n1 <> n2); t; loc = Util.Loc.merge e1.loc e2.loc }
      | OpLe -> Some { e = EBool (n1 <= n2); t; loc = Util.Loc.merge e1.loc e2.loc }
      | OpLt -> Some { e = EBool (n1 < n2); t; loc = Util.Loc.merge e1.loc e2.loc }
      | OpGe -> Some { e = EBool (n1 >= n2); t; loc = Util.Loc.merge e1.loc e2.loc }
      | OpGt -> Some { e = EBool (n1 > n2); t; loc = Util.Loc.merge e1.loc e2.loc }
      | _ -> None)
    | ({ e = EReal 0.0 | EFixed 0.0; _ } as zero), e | e, ({ e = EReal 0.0 | EFixed 0.0; _ } as zero) -> (
      match op with
      | OpAdd -> Some e
      | OpMul -> Some zero
      | _ -> None)
    | _ -> None


  let exp =
    Mapper.make
    @@ fun _env state e ->
    match e with
    (* -(n) -> -n *)
    | { e = EUnOp (UOpNeg, ({ e = EReal n; _ } as e1)); _ } -> reapply state, { e1 with e = EReal (-.n) }
    | { e = EUnOp (UOpNeg, ({ e = EFixed n; _ } as e1)); _ } -> reapply state, { e1 with e = EFixed (-.n) }
    | { e = EUnOp (UOpNeg, ({ e = EInt n; _ } as e1)); _ } -> reapply state, { e1 with e = EInt (-n) }
    (* x - (-n) -> x + n *)
    | { e = EOp (OpSub, e1, ({ e = EReal n; _ } as e2)); _ } ->
      reapply state, { e with e = EOp (OpAdd, e1, { e2 with e = EReal (-.n) }) }
    | { e = EOp (OpSub, e1, ({ e = EFixed n; _ } as e2)); _ } ->
      reapply state, { e with e = EOp (OpAdd, e1, { e2 with e = EFixed (-.n) }) }
    | { e = EOp (OpSub, e1, ({ e = EInt n; _ } as e2)); _ } ->
      reapply state, { e with e = EOp (OpAdd, e1, { e2 with e = EInt (-n) }) }
    (* e1 / e2 -> e1 * (1.0 / e2) *)
    | { e = EOp (OpDiv, e1, ({ e = EReal n; _ } as e2)); _ } ->
      reapply state, { e with e = EOp (OpMul, e1, { e2 with e = EReal (1.0 /. n) }) }
    | { e = EOp (OpDiv, e1, ({ e = EFixed n; _ } as e2)); _ } ->
      reapply state, { e with e = EOp (OpMul, e1, { e2 with e = EFixed (1.0 /. n) }) }
    (* k1 * (k2 + e) -> k1 * k2 + k1 * e *)
    | { e =
          EOp
            ( OpMul
            , ({ e = EReal _ | EInt _ | EFixed _; loc = loc1; _ } as k1)
            , { e = EOp (OpAdd, ({ e = EReal _ | EInt _ | EFixed _; _ } as k2), e); loc = loc2; _ } )
      ; _
      } ->
      let loc = Util.Loc.merge loc1 loc2 in
      let e1 = { e = EOp (OpMul, k1, k2); loc; t = k1.t } in
      let e2 = { e = EOp (OpMul, k1, e); loc; t = k1.t } in
      reapply state, { e with e = EOp (OpAdd, e1, e2) }
    | { e = EOp (op1, e1, { e = EOp (op2, e2, e3); _ }); _ } when op1 = op2 -> (
      match evaluate e.t op1 e1 e2 with
      | Some en -> reapply state, { e with e = EOp (op1, en, e3) }
      | None -> state, e)
    | { e = EOp (op, e1, e2); _ } -> (
      match evaluate e.t op e1 e2 with
      | Some e -> reapply state, e
      | None -> state, e)
    | _ -> state, e


  let stmt =
    Mapper.makeExpander
    @@ fun _env state (s : stmt) ->
    match s with
    (* removes a = a *)
    | { s = StmtBind ({ l = LId name1; _ }, { e = EId name2; _ }); _ } when String.compare name1 name2 = 0 -> state, []
    (* removes else {} *)
    | { s = StmtIf (cond, then_, Some { s = StmtBlock []; _ }); _ } ->
      state, [ { s with s = StmtIf (cond, then_, None) } ]
    (* removes if (cond) {} *)
    | { s = StmtIf (_, { s = StmtBlock []; _ }, None); _ } -> state, []
    | _ -> state, [ s ]


  let mapper enabled =
    if enabled = Enabled then
      { Mapper.identity with exp; stmt }
    else
      Mapper.identity
end

module StrengthReduction = struct
  (* Helper functions to check if a number is a power of 2 *)
  let is_power_of_two n = n > 0 && n land (n - 1) = 0

  let log2 n =
    let rec loop acc x =
      if x = 1 then
        acc
      else
        loop (acc + 1) (x lsr 1)
    in
    loop 0 n


  (* Check if a floating point number is close to an integer *)
  let is_close_to_int f =
    let rounded = Float.round f in
    Float.abs (f -. rounded) < 1e-10


  (* Check if a floating point number is a power of 2 *)
  let is_float_power_of_two f =
    is_close_to_int f
    &&
    let i = int_of_float (Float.round f) in
    i > 0 && is_power_of_two i


  let float_log2 f =
    let i = int_of_float (Float.round f) in
    log2 i


  (* Main strength reduction for expressions *)
  let exp =
    Mapper.make
    @@ fun _env state e ->
    match e with
    (* ========== INTEGER ARITHMETIC OPTIMIZATIONS ========== *)

    (* x * 0 -> 0, x * 1 -> x (extend existing patterns) *)
    | { e = EOp (OpMul, _, { e = EInt 0; _ }); _ } | { e = EOp (OpMul, { e = EInt 0; _ }, _); _ } ->
      reapply state, { e with e = EInt 0 }
    | { e = EOp (OpMul, e1, { e = EInt 1; _ }); _ } -> reapply state, e1
    | { e = EOp (OpMul, { e = EInt 1; _ }, e2); _ } -> reapply state, e2
    (* x * 2^n -> x << n (for integer types) *)
    | { e = EOp (OpMul, e1, { e = EInt n; _ }); t = { t = TInt; _ }; loc } when is_power_of_two n ->
      let shift_amount = log2 n in
      reapply state, { e = EOp (OpLsh, e1, C.eint ~loc shift_amount); t = e.t; loc }
    | { e = EOp (OpMul, { e = EInt n; _ }, e2); t = { t = TInt; _ }; loc } when is_power_of_two n ->
      let shift_amount = log2 n in
      reapply state, { e = EOp (OpLsh, e2, C.eint ~loc shift_amount); t = e.t; loc }
    (* x / 2^n -> x >> n (for positive integers only, to preserve sign behavior) *)
    | { e = EOp (OpDiv, e1, { e = EInt n; _ }); t = { t = TInt; _ }; loc } when is_power_of_two n ->
      let shift_amount = log2 n in
      reapply state, { e = EOp (OpRsh, e1, C.eint ~loc shift_amount); t = e.t; loc }
    (* Removed x + x -> x * 2 transformation as it causes issues with constant propagation *)
    (* x - x -> 0 (preserve type) *)
    | { e = EOp (OpSub, e1, e2); _ } when Compare.exp e1 e2 = 0 -> (
      match e1.t.t with
      | TInt -> reapply state, C.eint ~loc:e.loc 0
      | TReal -> reapply state, C.ereal ~loc:e.loc 0.0
      | TFix16 -> reapply state, C.efix16 ~loc:e.loc 0.0
      | _ -> state, e)
    (* x * -1 -> -x *)
    | { e = EOp (OpMul, e1, { e = EInt -1; _ }); _ } -> reapply state, { e with e = EUnOp (UOpNeg, e1) }
    | { e = EOp (OpMul, { e = EInt -1; _ }, e2); _ } -> reapply state, { e with e = EUnOp (UOpNeg, e2) }
    (* x % 1 -> 0 *)
    | { e = EOp (OpMod, _, { e = EInt 1; _ }); loc; _ } -> reapply state, C.eint ~loc 0
    (* NOTE: x % 2^n -> x & (2^n - 1) optimization removed because Lua 5.1/LuaJIT
       doesn't support native bitwise operators, and modulo works correctly *)
    (* 0 / x -> 0 *)
    | { e = EOp (OpDiv, { e = EInt 0; _ }, _); loc; _ } -> reapply state, C.eint ~loc 0
    (* 0 - x -> -x *)
    | { e = EOp (OpSub, { e = EInt 0; _ }, e2); _ } -> reapply state, { e with e = EUnOp (UOpNeg, e2) }
    (* ========== FLOATING POINT ARITHMETIC OPTIMIZATIONS ========== *)
    (* x * 0.0 -> 0.0, x * 1.0 -> x *)
    | { e = EOp (OpMul, _, { e = EReal 0.0; _ }); _ } | { e = EOp (OpMul, { e = EReal 0.0; _ }, _); _ } ->
      reapply state, { e with e = EReal 0.0 }
    | { e = EOp (OpMul, e1, { e = EReal 1.0; _ }); _ } -> reapply state, e1
    | { e = EOp (OpMul, { e = EReal 1.0; _ }, e2); _ } -> reapply state, e2
    (* x * -1.0 -> -x *)
    | { e = EOp (OpMul, e1, { e = EReal -1.0; _ }); _ } -> reapply state, { e with e = EUnOp (UOpNeg, e1) }
    | { e = EOp (OpMul, { e = EReal -1.0; _ }, e2); _ } -> reapply state, { e with e = EUnOp (UOpNeg, e2) }
    (* 0.0 / x -> 0.0 *)
    | { e = EOp (OpDiv, { e = EReal 0.0; _ }, _); loc; _ } -> reapply state, C.ereal ~loc 0.0
    (* 0.0 - x -> -x *)
    | { e = EOp (OpSub, { e = EReal 0.0; _ }, e2); _ } -> reapply state, { e with e = EUnOp (UOpNeg, e2) }
    (* Removed x * 2.0 -> x + x transformation as it interferes with constant folding *)
    (* Removed x * 0.5 <-> x / 2.0 transformations as they create cycles with other passes *)
    (* Removed problematic power-of-2 real multiplication transformation *)
    (* ========== FIXED POINT ARITHMETIC OPTIMIZATIONS ========== *)
    (* Similar patterns for fixed point (TFix16) *)
    | { e = EOp (OpMul, _, { e = EFixed 0.0; _ }); _ } | { e = EOp (OpMul, { e = EFixed 0.0; _ }, _); _ } ->
      reapply state, { e with e = EFixed 0.0 }
    | { e = EOp (OpMul, e1, { e = EFixed 1.0; _ }); _ } -> reapply state, e1
    | { e = EOp (OpMul, { e = EFixed 1.0; _ }, e2); _ } ->
      reapply state, e2 (* Removed x * 2.0 -> x + x for fixed point as it interferes with constant folding *)
    (* x * -1.0 -> -x (fixed) *)
    | { e = EOp (OpMul, e1, { e = EFixed -1.0; _ }); _ } -> reapply state, { e with e = EUnOp (UOpNeg, e1) }
    | { e = EOp (OpMul, { e = EFixed -1.0; _ }, e2); _ } -> reapply state, { e with e = EUnOp (UOpNeg, e2) }
    (* 0.0 / x -> 0.0 (fixed) *)
    | { e = EOp (OpDiv, { e = EFixed 0.0; _ }, _); loc; _ } -> reapply state, C.efix16 ~loc 0.0
    (* 0.0 - x -> -x (fixed) *)
    | { e = EOp (OpSub, { e = EFixed 0.0; _ }, e2); _ } -> reapply state, { e with e = EUnOp (UOpNeg, e2) }
    (* ========== DIVISION BY 1 OPTIMIZATIONS ========== *)
    (* x / 1 -> x (for all numeric types) *)
    | { e = EOp (OpDiv, e1, { e = EInt 1; _ }); _ } -> reapply state, e1
    | { e = EOp (OpDiv, e1, { e = EReal 1.0; _ }); _ } -> reapply state, e1
    | { e = EOp (OpDiv, e1, { e = EFixed 1.0; _ }); _ } -> reapply state, e1
    (* ========== FUNCTION CALL OPTIMIZATIONS ========== *)
    (* pow(x, 2) -> x * x *)
    | { e = ECall { path = "pow"; args = [ x; { e = EReal 2.0; _ } ] }; _ } ->
      reapply state, { e with e = EOp (OpMul, x, x) }
    | { e = ECall { path = "pow"; args = [ x; { e = EInt 2; _ } ] }; _ } ->
      reapply state, { e with e = EOp (OpMul, x, x) }
    | { e = ECall { path = "pow"; args = [ x; { e = EFixed 2.0; _ } ] }; _ } ->
      reapply state, { e with e = EOp (OpMul, x, x) }
    (* pow(x, 3) -> x * x * x *)
    | { e = ECall { path = "pow"; args = [ x; { e = EReal 3.0; _ } ] }; _ } ->
      let x_squared = { e with e = EOp (OpMul, x, x) } in
      reapply state, { e with e = EOp (OpMul, x, x_squared) }
    | { e = ECall { path = "pow"; args = [ x; { e = EInt 3; _ } ] }; _ } ->
      let x_squared = { e with e = EOp (OpMul, x, x) } in
      reapply state, { e with e = EOp (OpMul, x, x_squared) }
    (* pow(x, 4) -> (x * x) * (x * x) *)
    | { e = ECall { path = "pow"; args = [ x; { e = EReal 4.0; _ } ] }; _ } ->
      let x_squared = { e with e = EOp (OpMul, x, x) } in
      reapply state, { e with e = EOp (OpMul, x_squared, x_squared) }
    | { e = ECall { path = "pow"; args = [ x; { e = EInt 4; _ } ] }; _ } ->
      let x_squared = { e with e = EOp (OpMul, x, x) } in
      reapply state, { e with e = EOp (OpMul, x_squared, x_squared) }
    (* pow(x, 0.5) -> sqrt(x) *)
    | { e = ECall { path = "pow"; args = [ x; { e = EReal 0.5; _ } ] }; _ } ->
      reapply state, { e with e = ECall { path = "sqrt"; args = [ x ] } }
    (* pow(x, -1) -> 1.0 / x *)
    | { e = ECall { path = "pow"; args = [ x; { e = EReal -1.0; _ } ] }; loc; _ } ->
      reapply state, { e with e = EOp (OpDiv, C.ereal ~loc 1.0, x) }
    | { e = ECall { path = "pow"; args = [ x; { e = EInt -1; _ } ] }; loc; _ } ->
      reapply state, { e with e = EOp (OpDiv, C.ereal ~loc 1.0, x) }
    (* pow(x, -2) -> 1.0 / (x * x) *)
    | { e = ECall { path = "pow"; args = [ x; { e = EReal -2.0; _ } ] }; loc; _ } ->
      let x_squared = { e with e = EOp (OpMul, x, x) } in
      reapply state, { e with e = EOp (OpDiv, C.ereal ~loc 1.0, x_squared) }
    | { e = ECall { path = "pow"; args = [ x; { e = EInt -2; _ } ] }; loc; _ } ->
      let x_squared = { e with e = EOp (OpMul, x, x) } in
      reapply state, { e with e = EOp (OpDiv, C.ereal ~loc 1.0, x_squared) }
    (* pow(x, 1) -> x *)
    | { e = ECall { path = "pow"; args = [ x; { e = EReal 1.0; _ } ] }; _ } -> reapply state, x
    | { e = ECall { path = "pow"; args = [ x; { e = EInt 1; _ } ] }; _ } -> reapply state, x
    | { e = ECall { path = "pow"; args = [ x; { e = EFixed 1.0; _ } ] }; _ } -> reapply state, x
    (* pow(x, 0) -> 1 *)
    | { e = ECall { path = "pow"; args = [ _; { e = EReal 0.0; _ } ] }; loc; _ } -> reapply state, C.ereal ~loc 1.0
    | { e = ECall { path = "pow"; args = [ _; { e = EInt 0; _ } ] }; loc; _ } -> reapply state, C.eint ~loc 1
    | { e = ECall { path = "pow"; args = [ _; { e = EFixed 0.0; _ } ] }; loc; _ } ->
      reapply state, C.efix16 ~loc 1.0 (* ========== MATHEMATICAL IDENTITIES ========== *)
    (* abs(abs(x)) -> abs(x) *)
    | { e = ECall { path = "abs"; args = [ { e = ECall { path = "abs"; args = [ x ] }; _ } ] }; _ } ->
      reapply state, { e with e = ECall { path = "abs"; args = [ x ] } }
    (* sqrt(x * x) -> abs(x) (mathematically correct) *)
    | { e = ECall { path = "sqrt"; args = [ { e = EOp (OpMul, x1, x2); _ } ] }; _ } when Compare.exp x1 x2 = 0 ->
      reapply state, { e with e = ECall { path = "abs"; args = [ x1 ] } }
    (* Removed log(exp(x)) <-> exp(log(x)) transformations as they may create cycles *)
    (* Removed sin/cos constant optimizations as they conflict with existing Builtin pass *)

    (* ========== AUDIO-SPECIFIC OPTIMIZATIONS ========== *)
    (* tanh(x) where x is very small -> x (linear approximation) *)
    | { e = ECall { path = "tanh"; args = [ { e = EReal x; _ } ] }; _ } when Float.abs x < 0.1 ->
      reapply state, { e with e = EReal x }
    | { e = ECall { path = "tanh"; args = [ { e = EFixed x; _ } ] }; _ } when Float.abs x < 0.1 ->
      reapply state, { e with e = EFixed x } (* ========== BITWISE OPERATIONS OPTIMIZATIONS ========== *)
    (* x & 0 -> 0 *)
    | { e = EOp (OpBand, _, { e = EInt 0; _ }); _ } | { e = EOp (OpBand, { e = EInt 0; _ }, _); _ } ->
      reapply state, { e with e = EInt 0 }
    (* x | 0 -> x *)
    | { e = EOp (OpBor, e1, { e = EInt 0; _ }); _ } -> reapply state, e1
    | { e = EOp (OpBor, { e = EInt 0; _ }, e2); _ } -> reapply state, e2
    (* x ^ 0 -> x *)
    | { e = EOp (OpBxor, e1, { e = EInt 0; _ }); _ } -> reapply state, e1
    | { e = EOp (OpBxor, { e = EInt 0; _ }, e2); _ } -> reapply state, e2
    (* x ^ x -> 0 *)
    | { e = EOp (OpBxor, e1, e2); _ } when Compare.exp e1 e2 = 0 -> reapply state, C.eint ~loc:e.loc 0
    (* x << 0 -> x, x >> 0 -> x *)
    | { e = EOp (OpLsh, e1, { e = EInt 0; _ }); _ } -> reapply state, e1
    | { e = EOp (OpRsh, e1, { e = EInt 0; _ }); _ } -> reapply state, e1
    (* x & x -> x, x | x -> x *)
    | { e = EOp (OpBand, e1, e2); _ } when Compare.exp e1 e2 = 0 -> reapply state, e1
    | { e = EOp (OpBor, e1, e2); _ } when Compare.exp e1 e2 = 0 -> reapply state, e1
    (* x & -1 -> x (all bits set) *)
    | { e = EOp (OpBand, e1, { e = EInt -1; _ }); _ } -> reapply state, e1
    | { e = EOp (OpBand, { e = EInt -1; _ }, e2); _ } -> reapply state, e2
    (* ========== NEGATION OPTIMIZATIONS ========== *)
    (* -(-x) -> x *)
    | { e = EUnOp (UOpNeg, { e = EUnOp (UOpNeg, x); _ }); _ } -> reapply state, x
    (* not(not(x)) -> x *)
    | { e = EUnOp (UOpNot, { e = EUnOp (UOpNot, x); _ }); _ } -> reapply state, x
    (* ========== BOOLEAN LOGIC OPTIMIZATIONS ========== *)
    (* x && true -> x *)
    | { e = EOp (OpLand, e1, { e = EBool true; _ }); _ } -> reapply state, e1
    | { e = EOp (OpLand, { e = EBool true; _ }, e2); _ } -> reapply state, e2
    (* x && false -> false *)
    | { e = EOp (OpLand, _, { e = EBool false; loc; _ }); _ } -> reapply state, C.ebool ~loc false
    | { e = EOp (OpLand, { e = EBool false; loc; _ }, _); _ } -> reapply state, C.ebool ~loc false
    (* x || false -> x *)
    | { e = EOp (OpLor, e1, { e = EBool false; _ }); _ } -> reapply state, e1
    | { e = EOp (OpLor, { e = EBool false; _ }, e2); _ } -> reapply state, e2
    (* x || true -> true *)
    | { e = EOp (OpLor, _, { e = EBool true; loc; _ }); _ } -> reapply state, C.ebool ~loc true
    | { e = EOp (OpLor, { e = EBool true; loc; _ }, _); _ } -> reapply state, C.ebool ~loc true
    (* ========== COMPARISON SELF-IDENTITIES ========== *)
    (* x == x -> true (int only, floats have NaN) *)
    | { e = EOp (OpEq, e1, e2); loc; _ } when Compare.exp e1 e2 = 0 && e1.t.t = TInt -> reapply state, C.ebool ~loc true
    (* x != x -> false (int only) *)
    | { e = EOp (OpNe, e1, e2); loc; _ } when Compare.exp e1 e2 = 0 && e1.t.t = TInt ->
      reapply state, C.ebool ~loc false
    (* x < x -> false, x > x -> false *)
    | { e = EOp (OpLt, e1, e2); loc; _ } when Compare.exp e1 e2 = 0 -> reapply state, C.ebool ~loc false
    | { e = EOp (OpGt, e1, e2); loc; _ } when Compare.exp e1 e2 = 0 -> reapply state, C.ebool ~loc false
    (* x <= x -> true, x >= x -> true (int only) *)
    | { e = EOp (OpLe, e1, e2); loc; _ } when Compare.exp e1 e2 = 0 && e1.t.t = TInt -> reapply state, C.ebool ~loc true
    | { e = EOp (OpGe, e1, e2); loc; _ } when Compare.exp e1 e2 = 0 && e1.t.t = TInt -> reapply state, C.ebool ~loc true
    (* ========== MIN/MAX OPTIMIZATIONS ========== *)
    (* min(x, x) -> x, max(x, x) -> x *)
    | { e = ECall { path = "min"; args = [ x1; x2 ] }; _ } when Compare.exp x1 x2 = 0 -> reapply state, x1
    | { e = ECall { path = "max"; args = [ x1; x2 ] }; _ } when Compare.exp x1 x2 = 0 -> reapply state, x1
    (* No optimization found *)
    | _ -> state, e


  let mapper enabled =
    if enabled = Enabled then
      { Mapper.identity with exp }
    else
      Mapper.identity
end

module Sort = struct
  let dependencies = Location.mapper |> Mapper.seq CollectDependencies.mapper

  let rec split types functions externals constants stmts =
    match stmts with
    | [] -> CCList.rev types, CCList.rev functions, CCList.rev externals, CCList.rev constants
    | ({ top = TopType { path; _ }; _ } as h) :: t -> split ((path, h) :: types) functions externals constants t
    | ({ top = TopAlias { path; _ }; _ } as h) :: t -> split ((path, h) :: types) functions externals constants t
    | ({ top = TopFunction ({ name; _ }, _); _ } as h) :: t ->
      split types ((name, h) :: functions) externals constants t
    | ({ top = TopExternal _; _ } as h) :: t -> split types functions (h :: externals) constants t
    | ({ top = TopConstant _; _ } as h) :: t -> split types functions externals (h :: constants) t


  let rec sort deps table visited sorted stmts =
    match stmts with
    | [] -> CCList.rev sorted
    | { top = TopType { path = name; _ }; _ } :: t
     |{ top = TopAlias { path = name; _ }; _ } :: t
     |{ top = TopFunction ({ name; _ }, _); _ } :: t
     |{ top = TopExternal ({ name; _ }, _); _ } :: t
     |{ top = TopConstant (name, _, _, _, _); _ } :: t ->
      let visited, sorted = pullIn deps table visited sorted name in
      sort deps table visited sorted t


  and pullIn deps table visited sorted name =
    if Set.mem name visited then
      visited, sorted
    else
      match Map.find_opt name deps with
      | None -> (
        let visited = Set.add name visited in
        match Map.find_opt name table with
        | Some stmt -> visited, stmt :: sorted
        | None -> visited, sorted)
      | Some dep_set ->
        let visited = Set.add name visited in
        let missing = Set.filter (fun name -> not (Set.mem name visited)) dep_set in
        let visited, sorted =
          Set.fold (fun name (visited, sorted) -> pullIn deps table visited sorted name) missing (visited, sorted)
        in
        let stmt = Map.find name table in
        visited, stmt :: sorted


  let getDependencies args prog =
    let state, _ = Mapper.prog dependencies (default_env args) (Mapper.defaultState (default_data ())) prog in
    let data = Mapper.getData state in
    data.type_deps, data.function_deps


  let run args prog =
    let type_deps, function_deps = getDependencies args prog in
    let types, functions, externals, constants = split [] [] [] [] prog in
    let type_table = Map.of_list types in
    let functions_table = Map.of_list functions in
    let types = sort type_deps type_table Set.empty [] (CCList.map snd types) in
    let functions = sort function_deps functions_table Set.empty [] (CCList.map snd functions) in
    types @ constants @ externals @ functions
end

let passes =
  Location.mapper
  |> Mapper.seq (Markers.mapper Enabled)
  |> Mapper.seq (Canonize.mapper Enabled)
  |> Mapper.seq (StrengthReduction.mapper Enabled)
  |> Mapper.seq (Simplify.mapper Enabled)
  |> Mapper.seq (Builtin.mapper Enabled)
  |> Mapper.seq (IfExpressions.mapper Enabled)
  |> Mapper.seq (Tuples.mapper Enabled)
  |> Mapper.seq (Cast.mapper Enabled)
  |> Mapper.seq (LiteralArrays.mapper Enabled)
  |> Mapper.seq (LiteralRecords.mapper Enabled)


let rec apply env state prog n =
  if n > 20 then
    failwith "too many repeats"
  else
    match prog with
    | [] -> state, []
    | h :: t ->
      let state, h = Mapper.top_stmt passes env state h in
      let data = Mapper.getData state in
      if data.repeat then
        let data = { data with repeat = false } in
        let state, h = apply env (Mapper.setData state data) h (n + 1) in
        apply env state (h @ t) (n + 1)
      else
        let state, t = apply env state t 0 in
        state, h @ t


let run args (prog : prog) : prog =
  let _, prog = apply (default_env args) (Mapper.defaultState (default_data ())) prog 0 in
  let prog = Sort.run args prog in
  prog


let simplifyExp (e : exp) : exp =
  let rec loop n env state e =
    if n > 20 then
      failwith "too many repeats"
    else
      let state, e = Mapper.exp passes env state e in
      let data = Mapper.getData state in
      if data.repeat then
        let data = { data with repeat = false } in
        loop (n + 1) env (Mapper.setData state data) e
      else
        e
  in
  loop 0 (default_env Util.Args.default_arguments) (Mapper.defaultState (default_data ())) e
