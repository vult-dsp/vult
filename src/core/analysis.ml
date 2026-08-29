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

(* An identifier followed by member selections. Indices are excluded because
   evaluating one can have observable effects. *)
module Path = struct
  type t = string list

  let rec of_exp (e : exp) : t option =
    match e.e with
    | EId name ->
        Some [name]
    | EMember (base, member) ->
        Option.map (fun base -> base @ [member]) (of_exp base)
    | _ ->
        None

  let rec of_lexp (l : lexp) : t option =
    match l.l with
    | LId name ->
        Some [name]
    | LMember (base, member) ->
        Option.map (fun base -> base @ [member]) (of_lexp base)
    | LWild | LIndex _ | LTuple _ ->
        None

  (* Rebuilds the expression that reads the location written by [l]. *)
  let rec exp_of_lexp (l : lexp) : exp option =
    match l.l with
    | LId name ->
        Some {e= EId name; t= l.t; loc= l.loc}
    | LMember (base, member) ->
        Option.map (fun base -> {e= EMember (base, member); t= l.t; loc= l.loc}) (exp_of_lexp base)
    | LWild | LIndex _ | LTuple _ ->
        None

  let rec is_prefix (a : t) (b : t) =
    match (a, b) with [], _ -> true | x :: a, y :: b -> String.equal x y && is_prefix a b | _ :: _, [] -> false

  (* Overlapping storage: writing [q] also changes [q.a], but [q.a] and [q.b]
     are disjoint. *)
  let may_alias (a : t) (b : t) = is_prefix a b || is_prefix b a

  let equal (a : t) (b : t) = CCList.equal String.equal a b
end

(* Folds [f] over [s] and every nested statement, parents first. [f] sees only
   the statement itself, never its children. *)
let rec fold_stmt f acc (s : stmt) =
  let acc = f acc s in
  match s.s with
  | StmtDecl _ | StmtBind _ | StmtReturn _ ->
      acc
  | StmtBlock stmts ->
      CCList.fold_left (fold_stmt f) acc stmts
  | StmtIf (_, then_, else_) ->
      let acc = fold_stmt f acc then_ in
      Option.fold ~none:acc ~some:(fold_stmt f acc) else_
  | StmtWhile (_, body) ->
      fold_stmt f acc body
  | StmtSwitch (_, cases, default) ->
      let acc = CCList.fold_left (fun acc (_, body) -> fold_stmt f acc body) acc cases in
      Option.fold ~none:acc ~some:(fold_stmt f acc) default

module GetVariables = struct
  let exp =
    Mapper.make
    @@ fun _env (state : Set.t Mapper.state) (e : exp) ->
    match e with
    | {e= EId name; _} ->
        let data = Mapper.getData state in
        (Mapper.setData state (Set.add name data), e)
    | _ ->
        (state, e)

  let lexp =
    Mapper.make
    @@ fun _env (state : Set.t Mapper.state) (e : lexp) ->
    match e with
    | {l= LId name; _} ->
        let data = Mapper.getData state in
        (Mapper.setData state (Set.add name data), e)
    | _ ->
        (state, e)

  let dexp =
    Mapper.make
    @@ fun _env (state : Set.t Mapper.state) (e : dexp) ->
    match e with
    | {d= DId (name, _); _} ->
        let data = Mapper.getData state in
        (Mapper.setData state (Set.add name data), e)

  let mapper = {Mapper.identity with exp; lexp; dexp}

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
