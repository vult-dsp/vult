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

open Prog
module Set = CCSet.Make (String)

type state =
  { repeat : bool
  ; ticks : (string, int) Hashtbl.t
  ; stmt_acc : stmt list
  ; dummy : int
  }

type env =
  { in_if_exp : bool
  ; bound_if : bool
  ; current_function : string
  ; dummy : int
  }

let default_state : state = { repeat = false; dummy = 0; ticks = Hashtbl.create 16; stmt_acc = [] }

let default_env : env = { dummy = 0; in_if_exp = false; bound_if = false; current_function = "" }

let reapply state = { state with repeat = true }

let setStmts (state : state) stmts = { state with stmt_acc = stmts @ state.stmt_acc }

let getStmts (state : state) = { state with stmt_acc = [] }, state.stmt_acc

let getTick (env : env) (state : state) =
  match Hashtbl.find_opt state.ticks env.current_function with
  | None ->
      Hashtbl.add state.ticks env.current_function 1 ;
      0
  | Some n ->
      Hashtbl.replace state.ticks env.current_function (n + 1) ;
      n


module GetVariables = struct
  let exp =
    Mapper.make
    @@ fun _env (state : Set.t) (e : exp) ->
    match e with
    | { e = EId name; _ } -> Set.add name state, e
    | _ -> state, e


  let lexp =
    Mapper.make
    @@ fun _env (state : Set.t) (e : lexp) ->
    match e with
    | { l = LId name; _ } -> Set.add name state, e
    | _ -> state, e


  let mapper = { Mapper.identity with exp; lexp }

  let in_exp (e : exp) =
    let state, _ = Mapper.exp mapper () Set.empty e in
    state


  let in_lexp (e : lexp) =
    let state, _ = Mapper.lexp mapper () Set.empty e in
    state
end

module Location = struct
  let top_stmt_env =
    Mapper.makeEnv
    @@ fun env (s : top_stmt) ->
    match s with
    | { top = TopFunction ({ name; _ }, _); _ } -> { env with current_function = name }
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
    | { e = EIf _; t; loc } when (not env.in_if_exp) && not env.bound_if ->
        let tick = getTick env state in
        let temp = "_if_temp_" ^ string_of_int tick in
        let temp_e = { e = EId temp; t; loc } in
        let decl_stmt = { s = StmtDecl { d = DId (temp, None); t; loc }; loc } in
        let bind_stmt = { s = StmtBind ({ l = LId temp; t; loc }, e); loc } in
        let state = setStmts state [ decl_stmt; bind_stmt ] in
        reapply state, temp_e
    | _ -> state, e


  let mapper = { Mapper.identity with stmt; exp; stmt_env }
end

module Tuples = struct
  let stmt =
    Mapper.makeExpander
    @@ fun env state (s : stmt) ->
    match s with
    | { s = StmtDecl { d = DTuple elems; _ }; loc } ->
        let stmts = List.map (fun d -> { s = StmtDecl d; loc }) elems in
        reapply state, stmts
    | { s = StmtDecl { d = DWild; _ }; _ } -> reapply state, []
    | { s = StmtBind (({ l = LTuple l_elems; _ } as lhs), ({ e = ETuple r_elems; _ } as rhs)); loc } ->
        let l = GetVariables.in_lexp lhs in
        let r = GetVariables.in_exp rhs in
        let d = Set.inter l r in
        if Set.is_empty d then
          let bindings = List.map2 (fun l r -> { s = StmtBind (l, r); loc }) l_elems r_elems in
          state, bindings
        else
          let temp_list = List.map (fun (l : lexp) -> "_t_temp_" ^ string_of_int (getTick env state), l.t) l_elems in
          let decl = List.map (fun (n, t) -> { s = StmtDecl { d = DId (n, None); loc; t }; loc }) temp_list in
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
          state, decl @ bindings1 @ bindings2
    | _ -> state, [ s ]


  let mapper = { Mapper.identity with stmt }
end

module Not = struct
  let exp =
    Mapper.make
    @@ fun _env state (e : exp) ->
    match e with
    | { e = ECall { path = "not"; args = [ e1 ] }; loc; _ } ->
        reapply state, { e with e = EOp (OpEq, e1, { e = EBool false; t = { t = TBool; loc }; loc }) }
    | _ -> state, e


  let mapper = { Mapper.identity with exp }
end

module Simplify = struct
  let exp =
    Mapper.make
    @@ fun _env state e ->
    match e with
    | _ -> state, e


  let mapper = { Mapper.identity with exp }
end

module PrependStmts = struct
  let stmt =
    Mapper.makeExpander
    @@ fun _env state (s : stmt) ->
    match s with
    | _ ->
        let state, pre = getStmts state in
        state, pre @ [ s ]


  let mapper = { Mapper.identity with stmt }
end

let mapper =
  Simplify.mapper
  |> Mapper.seq Not.mapper
  |> Mapper.seq IfExpressions.mapper
  |> Mapper.seq Tuples.mapper
  |> Mapper.seq PrependStmts.mapper


let apply env state prog =
  let rec loop state prog n =
    if n > 20 then
      prog
    else
      let state, prog = Mapper.prog mapper env state prog in
      if state.repeat then
        loop { state with repeat = false } prog (n + 1)
      else
        prog
  in
  loop state prog 0


let run (prog : prog) : prog =
  let prog = apply default_env default_state prog in
  prog
