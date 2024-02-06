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
    let state, _ = Mapper.mapper_list Mapper.stmt mapper () (Mapper.defaultState Set.empty) s in
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
    | { e = EIf { cond = { e = EBool cond; _ }; then_; else_ }; _ } -> reapply state, if cond then then_ else else_
    (* Bind if-expressions to a variable *)
    | { e = EIf _; t; loc } when (not env.in_if_exp) && not env.bound_if ->
      let tick = getTick env state in
      let temp = "_if_temp_" ^ string_of_int tick in
      let temp_e = { e = EId temp; t; loc } in
      let decl_stmt = { s = StmtDecl ({ d = DId (temp, None); t; loc }, None); loc } in
      let bind_stmt = { s = StmtBind ({ l = LId temp; t; loc }, e); loc } in
      let state = Mapper.pushStmts state [ decl_stmt; bind_stmt ] in
      reapply state, temp_e
    | _ -> state, e


  let mapper enabled = if enabled = Enabled then { Mapper.identity with stmt; exp; stmt_env } else Mapper.identity
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
    | { top = TopConstant (_, _, _, { e = ERecord _; _ }); _ } -> { env with bound_record = true }
    | _ -> env


  let exp =
    Mapper.make
    @@ fun env state (e : exp) ->
    match e with
    (* Bind arrays to a variable *)
    | { e = ERecord _; t; loc } when (not env.in_if_exp) && not env.bound_record ->
      let tick = getTick env state in
      let temp = "_record_" ^ string_of_int tick in
      let temp_e = { e = EId temp; t; loc } in
      let decl_stmt = { s = StmtDecl ({ d = DId (temp, None); t; loc }, None); loc } in
      let bind_stmt = { s = StmtBind ({ l = LId temp; t; loc }, e); loc } in
      let state = Mapper.pushStmts state [ decl_stmt; bind_stmt ] in
      reapply state, temp_e
    | _ -> state, e


  let mapper enabled =
    if enabled = Enabled then { Mapper.identity with exp; stmt_env; top_stmt_env } else Mapper.identity
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
    | { top = TopConstant (_, _, _, { e = EArray _; _ }); _ } -> { env with bound_array = true }
    | _ -> env


  let exp =
    Mapper.make
    @@ fun env state (e : exp) ->
    match e with
    (* Bind arrays to a variable *)
    | { e = EArray _; t; loc } when (not env.in_if_exp) && not env.bound_array ->
      let tick = getTick env state in
      let temp = "_array_" ^ string_of_int tick in
      let temp_e = { e = EId temp; t; loc } in
      let decl_stmt = { s = StmtDecl ({ d = DId (temp, None); t; loc }, None); loc } in
      let bind_stmt = { s = StmtBind ({ l = LId temp; t; loc }, e); loc } in
      let state = Mapper.pushStmts state [ decl_stmt; bind_stmt ] in
      reapply state, temp_e
    | _ -> state, e


  let mapper enabled =
    if enabled = Enabled then { Mapper.identity with exp; stmt_env; top_stmt_env } else Mapper.identity
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
        List.map
          (fun (t : type_) ->
            let tick = getTick env state in
            "_call_temp_" ^ string_of_int tick, t)
          elems
      in
      let decl_stmt = List.map (fun (name, t) -> { s = StmtDecl ({ d = DId (name, None); t; loc }, None); loc }) temp in
      let temp_l = List.map (fun (name, t) -> { l = LId name; t; loc }) temp in
      let bind_stmt = { s = StmtBind ({ l = LTuple temp_l; t; loc }, e); loc } in
      let state = Mapper.pushStmts state (decl_stmt @ [ bind_stmt ]) in
      let temp_e = List.map (fun (name, t) -> { e = EId name; t; loc }) temp in
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
      if Set.is_empty d then (
        let bindings = List.map2 (fun l r -> { s = StmtBind (l, r); loc }) l_elems r_elems in
        reapply state, bindings)
      else (
        let temp_list = List.map (fun (l : lexp) -> "_t_temp_" ^ string_of_int (getTick env state), l.t) l_elems in
        let decl = List.map (fun (n, t) -> { s = StmtDecl ({ d = DId (n, None); loc; t }, None); loc }) temp_list in
        let bindings1 =
          List.map2
            (fun (l, _) (r : exp) -> { s = StmtBind ({ l = LId l; t = r.t; loc = r.loc }, r); loc })
            temp_list
            r_elems
        in
        let bindings2 =
          List.map2
            (fun (l : lexp) (r, _) -> { s = StmtBind (l, { e = EId r; t = l.t; loc = l.loc }); loc })
            l_elems
            temp_list
        in
        reapply state, decl @ bindings1 @ bindings2)
    (* bind multi return calls to the context *)
    | { s = StmtBind (({ l = LTuple elems; _ } as lhs), ({ e = ECall { path; args = ctx :: _ }; loc = rloc; _ } as rhs))
      ; loc
      } ->
      let bindings =
        List.mapi
          (fun i (l : lexp) ->
            let r = { e = EMember (ctx, path ^ "_ret_" ^ string_of_int i); t = l.t; loc = l.loc } in
            { s = StmtBind (l, r); loc })
          elems
      in
      let s = { s = StmtBind ({ lhs with l = LWild }, { rhs with t = { t = TVoid None; loc = rloc } }); loc } in
      reapply state, s :: bindings
    (* multi return calls bound to a tuple variable *)
    | { s =
          StmtBind
            ( ({ l = LId _; t = { t = TTuple types; _ }; _ } as lhs)
            , ({ e = ECall { path; args = ctx :: _ }; loc = rloc; _ } as rhs) )
      ; loc
      } ->
      let tuple_elems =
        List.mapi (fun i (t : type_) -> { e = EMember (ctx, path ^ "_ret_" ^ string_of_int i); t; loc }) types
      in
      let s = { s = StmtBind ({ lhs with l = LWild }, { rhs with t = { t = TVoid None; loc = rloc } }); loc } in
      let binding = { s = StmtBind (lhs, { e = ETuple tuple_elems; t = rhs.t; loc = rhs.loc }); loc } in
      reapply state, s :: [ binding ]
    (* Remove the return type of calls bound to wild *)
    | { s = StmtBind ({ l = LWild; _ }, { e = ECall _; t = { t = TVoid _; _ }; _ }); _ } -> state, [ s ]
    | { s = StmtBind (({ l = LWild; _ } as lhs), ({ e = ECall _; loc = rloc; _ } as rhs)); loc } ->
      let s =
        { s = StmtBind ({ lhs with l = LWild }, { rhs with t = { t = TVoid (Some [ rhs.t ]); loc = rloc } }); loc }
      in
      reapply state, [ s ]
    (* Bind returned tupples to the environment *)
    | { s = StmtReturn { e = ETuple elems; loc = eloc; _ }; loc } ->
      let name, ctx_name, ctx_t = currentFunction env in
      let ctx = { l = LId ctx_name; t = ctx_t; loc } in
      let bindings =
        List.mapi
          (fun i (r : exp) ->
            let l = { l = LMember (ctx, name ^ "_ret_" ^ string_of_int i); t = r.t; loc = r.loc } in
            { s = StmtBind (l, r); loc })
          elems
      in
      let s = { s = StmtReturn { e = EUnit; t = { t = TVoid None; loc }; loc = eloc }; loc } in
      reapply state, bindings @ [ s ]
    (* Bind returned single variable tuple to the environment *)
    | { s = StmtReturn ({ e = EId _; loc = eloc; t = { t = TTuple types; _ } } as ret); loc } ->
      let name, ctx_name, ctx_t = currentFunction env in
      let ctx = { l = LId ctx_name; t = ctx_t; loc } in
      let bindings =
        List.mapi
          (fun i (t : type_) ->
            let l = { l = LMember (ctx, name ^ "_ret_" ^ string_of_int i); t; loc } in
            { s = StmtBind (l, { ret with e = ETMember (ret, i) }); loc })
          types
      in
      let s = { s = StmtReturn { e = EUnit; t = { t = TVoid None; loc }; loc = eloc }; loc } in
      reapply state, bindings @ [ s ]
    | _ -> state, [ s ]


  let top_stmt =
    Mapper.makeExpander
    @@ fun _env state (top : top_stmt) ->
    match top with
    | { top = TopFunction (({ t = args_t, { t = TTuple elems; loc = tloc }; _ } as def), body); loc } ->
      let def = { def with t = args_t, { t = TVoid (Some elems); loc = tloc } } in
      state, [ { top = TopFunction (def, body); loc } ]
    | _ -> state, [ top ]


  let mapper enabled =
    if enabled = Enabled then { Mapper.identity with stmt; stmt_env; exp; top_stmt } else Mapper.identity
end

module Builtin = struct
  let exp =
    Mapper.make
    @@ fun env state (e : exp) ->
    match e with
    | { e = ECall { path = "pi"; args = [] }; _ } -> reapply state, { e with e = EReal Float.pi }
    | { e = ECall { path = "not"; args = [ e1 ] }; loc; _ } ->
      reapply state, { e with e = EOp (OpEq, e1, { e = EBool false; t = { t = TBool; loc }; loc }) }
    | { e = ECall { path = "size"; args = [ { t = { t = TArray (Some size, _); _ }; _ } ] }; loc; _ } ->
      reapply state, { e with e = EInt size; loc }
    | { e = ECall { path = "length"; args = [ { e = EString str; _ } ] }; loc; _ } ->
      reapply state, { e with e = EInt (String.length str); loc }
    | { e = ECall { path = "samplerate"; args = [] }; _ } -> (
      match env.args.fs with
      | Some fs -> reapply state, { e with e = EReal fs }
      | None -> state, e)
    | _ -> state, e


  let mapper enabled = if enabled = Enabled then { Mapper.identity with exp } else Mapper.identity
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
      reapply state, { e1 with e = EFixed (if b then 1.0 else 0.0); t }
    | { e = ECall { path = "real"; args = [ ({ e = EFixed v; _ } as e1) ] }; t; _ } ->
      reapply state, { e1 with e = EReal v; t }
    | { e = ECall { path = "real"; args = [ ({ e = EInt i; _ } as e1) ] }; t; _ } ->
      reapply state, { e1 with e = EReal (float_of_int i); t }
    | { e = ECall { path = "real"; args = [ ({ e = EBool b; _ } as e1) ] }; t; _ } ->
      reapply state, { e1 with e = EReal (if b then 1.0 else 0.0); t }
    | { e = ECall { path = "int"; args = [ ({ e = EFixed v; _ } as e1) ] }; t; _ } ->
      reapply state, { e1 with e = EInt (int_of_float v); t }
    | { e = ECall { path = "int"; args = [ ({ e = EReal v; _ } as e1) ] }; t; _ } ->
      reapply state, { e1 with e = EInt (int_of_float v); t }
    | { e = ECall { path = "int"; args = [ ({ e = EBool b; _ } as e1) ] }; t; _ } ->
      reapply state, { e1 with e = EInt (if b then 1 else 0); t }
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
    | { t = TReal; loc } when env.args.real = Fixed -> state, { t = TFix16; loc }
    | _ -> state, t


  let mapper enabled = if enabled = Enabled then { Mapper.identity with exp; type_ } else Mapper.identity
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
      if compare_exp e2 e1 > 0 then (
        let n2 = { n2 with e = EOp (op2, e2, e3) } in
        reapply state, { e with e = EOp (op1, e1, n2) })
      else
        state, e
    | { e = EOp (op, e1, e2); _ } when op = OpAdd || op = OpMul ->
      if compare_exp e1 e2 > 0 then reapply state, { e with e = EOp (op, e2, e1) } else state, e
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


  let mapper enabled = if enabled = Enabled then { Mapper.identity with exp } else Mapper.identity
end

module Simplify = struct
  let evaluate op e1 e2 =
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
      | _ -> None)
    | { e = EFixed n1; _ }, { e = EFixed n2; _ } -> (
      match op with
      | OpAdd -> Some { e = EFixed (n1 +. n2); t = e1.t; loc = Util.Loc.merge e1.loc e2.loc }
      | OpMul -> Some { e = EFixed (n1 *. n2); t = e1.t; loc = Util.Loc.merge e1.loc e2.loc }
      | OpSub -> Some { e = EFixed (n1 -. n2); t = e1.t; loc = Util.Loc.merge e1.loc e2.loc }
      | OpDiv -> Some { e = EFixed (n1 /. n2); t = e1.t; loc = Util.Loc.merge e1.loc e2.loc }
      | _ -> None)
    | { e = EInt n1; _ }, { e = EInt n2; _ } -> (
      match op with
      | OpAdd -> Some { e = EInt (n1 + n2); t = e1.t; loc = Util.Loc.merge e1.loc e2.loc }
      | OpMul -> Some { e = EInt (n1 * n2); t = e1.t; loc = Util.Loc.merge e1.loc e2.loc }
      | OpSub -> Some { e = EInt (n1 - n2); t = e1.t; loc = Util.Loc.merge e1.loc e2.loc }
      | OpDiv -> Some { e = EInt (n1 / n2); t = e1.t; loc = Util.Loc.merge e1.loc e2.loc }
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
      match evaluate op1 e1 e2 with
      | Some en -> reapply state, { e with e = EOp (op1, en, e3) }
      | None -> state, e)
    | { e = EOp (op, e1, e2); _ } -> (
      match evaluate op e1 e2 with
      | Some e -> reapply state, e
      | None -> state, e)
    | _ -> state, e


  let mapper enabled = if enabled = Enabled then { Mapper.identity with exp } else Mapper.identity
end

module Sort = struct
  let dependencies = Location.mapper |> Mapper.seq CollectDependencies.mapper

  let rec split types functions externals constants stmts =
    match stmts with
    | [] -> List.rev types, List.rev functions, List.rev externals, List.rev constants
    | ({ top = TopType { path; _ }; _ } as h) :: t -> split ((path, h) :: types) functions externals constants t
    | ({ top = TopAlias { path; _ }; _ } as h) :: t -> split ((path, h) :: types) functions externals constants t
    | ({ top = TopFunction ({ name; _ }, _); _ } as h) :: t ->
      split types ((name, h) :: functions) externals constants t
    | ({ top = TopExternal _; _ } as h) :: t -> split types functions (h :: externals) constants t
    | ({ top = TopConstant _; _ } as h) :: t -> split types functions externals (h :: constants) t


  let rec sort deps table visited sorted stmts =
    match stmts with
    | [] -> List.rev sorted
    | { top = TopType { path = name; _ }; _ } :: t
    | { top = TopAlias { path = name; _ }; _ } :: t
    | { top = TopFunction ({ name; _ }, _); _ } :: t
    | { top = TopExternal ({ name; _ }, _); _ } :: t
    | { top = TopConstant (name, _, _, _); _ } :: t ->
      let visited, sorted = pullIn deps table visited sorted name in
      sort deps table visited sorted t


  and pullIn deps table visited sorted name =
    if Set.mem name visited then
      visited, sorted
    else (
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
        visited, stmt :: sorted)


  let getDependencies args prog =
    let state, _ = Mapper.prog dependencies (default_env args) (Mapper.defaultState (default_data ())) prog in
    let data = Mapper.getData state in
    data.type_deps, data.function_deps


  let run args prog =
    let type_deps, function_deps = getDependencies args prog in
    let types, functions, externals, constants = split [] [] [] [] prog in
    let type_table = Map.of_list types in
    let functions_table = Map.of_list functions in
    let types = sort type_deps type_table Set.empty [] (List.map snd types) in
    let functions = sort function_deps functions_table Set.empty [] (List.map snd functions) in
    types @ constants @ externals @ functions
end

let passes =
  Location.mapper
  |> Mapper.seq (Canonize.mapper Enabled)
  |> Mapper.seq (Simplify.mapper Enabled)
  |> Mapper.seq (Builtin.mapper Enabled)
  |> Mapper.seq (IfExpressions.mapper Enabled)
  |> Mapper.seq (Tuples.mapper Enabled)
  |> Mapper.seq (Cast.mapper Enabled)
  |> Mapper.seq (LiteralArrays.mapper Enabled)
  |> Mapper.seq (LiteralRecords.mapper Enabled)


let rec apply env state prog n =
  if n > 10 then
    failwith "too many repeats"
  else (
    match prog with
    | [] -> state, []
    | h :: t ->
      let state, h = Mapper.top_stmt passes env state h in
      let data = Mapper.getData state in
      if data.repeat then (
        let data = { data with repeat = false } in
        let state, h = apply env (Mapper.setData state data) h (n + 1) in
        apply env state (h @ t) (n + 1))
      else (
        let state, t = apply env state t 0 in
        state, h @ t))


let run args (prog : prog) : prog =
  let _, prog = apply (default_env args) (Mapper.defaultState (default_data ())) prog 0 in
  let prog = Sort.run args prog in
  prog


let simplifyExp (e : exp) : exp =
  let rec loop n env state e =
    if n > 10 then
      failwith "too many repeats"
    else (
      let state, e = Mapper.exp passes env state e in
      let data = Mapper.getData state in
      if data.repeat then (
        let data = { data with repeat = false } in
        loop (n + 1) env (Mapper.setData state data) e)
      else
        e)
  in
  loop 0 (default_env Util.Args.default_arguments) (Mapper.defaultState (default_data ())) e
