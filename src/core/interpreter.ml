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
open Util
module Map = CCMap.Make (String)

type env =
  { functions : int Map.t
  ; locals : int Map.t
  ; lcount : int
  ; fcount : int
  }

let default_env = { locals = Map.empty; lcount = 0; functions = Map.empty; fcount = 0 }

let addLocal env name = { env with locals = Map.add name env.lcount env.locals; lcount = env.lcount + 1 }

type rvalue_d =
  | RVoid
  | RInt    of int
  | RReal   of float
  | RBool   of bool
  | RString of string
  | RRef    of int
  | RIndex  of rvalue * rvalue
  | RAdd    of rvalue * rvalue
  | RSub    of rvalue * rvalue
  | RDiv    of rvalue * rvalue
  | RMul    of rvalue * rvalue
  | RMod    of rvalue * rvalue
  | RLand   of rvalue * rvalue
  | RLor    of rvalue * rvalue
  | RBor    of rvalue * rvalue
  | RBand   of rvalue * rvalue
  | RBxor   of rvalue * rvalue
  | RLsh    of rvalue * rvalue
  | RRsh    of rvalue * rvalue
  | REq     of rvalue * rvalue
  | RNe     of rvalue * rvalue
  | RLt     of rvalue * rvalue
  | RLe     of rvalue * rvalue
  | RGt     of rvalue * rvalue
  | RGe     of rvalue * rvalue
  | RNot    of rvalue
  | RNeg    of rvalue
  | RIf     of rvalue * rvalue * rvalue
  | RTuple  of rvalue list
  | RArray  of rvalue array
  | RMember of rvalue * int
  | RCall   of int * rvalue list

and rvalue =
  { r : rvalue_d
  ; t : type_
  ; loc : Loc.t
  }

type lvalue_d =
  | LVoid
  | LRef    of int
  | LTuple  of lvalue list
  | LMember of lvalue * int
  | LIndex  of lvalue * rvalue

and lvalue =
  { l : lvalue_d
  ; t : type_
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
      ; index : int
      }

let list f (env : 'env) (l : 'e) =
  let env, i_rev =
    List.fold_left
      (fun (env, instr) d ->
        let env, i = f env d in
        env, i :: instr)
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
  let t = l.t in
  match l.l with
  | LWild -> { l = LVoid; t; loc }
  | LId name ->
      let index = Map.find name env.locals in
      { l = LRef index; t; loc }
  | LTuple l -> { l = LTuple (List.map (compile_lexp env) l); t; loc }
  | LMember (e, s) ->
      begin
        match e.t.t with
        | TStruct descr ->
            let index = getIndex s descr.members in
            let e = compile_lexp env e in
            { l = LMember (e, index); t; loc }
        | _ -> failwith "type error"
      end
  | LIndex { e; index } ->
      let e = compile_lexp env e in
      let index = compile_exp env index in
      { l = LIndex (e, index); t; loc }


and compile_exp (env : env) e : rvalue =
  let loc = e.loc in
  let t = e.t in
  match e.e with
  | EUnit -> { r = RVoid; t; loc }
  | EBool v -> { r = RBool v; t; loc }
  | EInt v -> { r = RInt v; t; loc }
  | EReal v -> { r = RReal v; t; loc }
  | EString v -> { r = RString v; t; loc }
  | EId id ->
      let index = Map.find id env.locals in
      { r = RRef index; t; loc }
  | EOp (op, e1, e2) ->
      let e1 = compile_exp env e1 in
      let e2 = compile_exp env e2 in
      { r = makeOp op e1 e2; t; loc }
  | EIndex { e; index } ->
      let e1 = compile_exp env e in
      let e2 = compile_exp env index in
      { r = RIndex (e1, e2); t; loc }
  | EUnOp (UOpNeg, e) ->
      let e = compile_exp env e in
      { r = RNeg e; t; loc }
  | EUnOp (UOpNot, e) ->
      let e = compile_exp env e in
      { r = RNot e; t; loc }
  | EIf { cond; then_; else_ } ->
      let cond = compile_exp env cond in
      let then_ = compile_exp env then_ in
      let else_ = compile_exp env else_ in
      { r = RIf (cond, then_, else_); t; loc }
  | ETuple elems ->
      let elems = List.map (compile_exp env) elems in
      { r = RTuple elems; t; loc }
  | EArray elems ->
      let elems = List.map (compile_exp env) elems in
      { r = RArray (Array.of_list elems); t; loc }
  | EMember (e, m) ->
      begin
        match e.t.t with
        | TStruct descr ->
            let index = getIndex m descr.members in
            let e = compile_exp env e in
            { r = RMember (e, index); t; loc }
        | _ -> failwith "type error"
      end
  | ECall { path; args } ->
      let args = List.map (compile_exp env) args in
      let index = Map.find path env.functions in
      { r = RCall (index, args); t; loc }


and makeOp op e1 e2 =
  match op with
  | OpAdd -> RAdd (e1, e2)
  | OpSub -> RAdd (e1, e2)
  | OpMul -> RMul (e1, e2)
  | OpDiv -> RDiv (e1, e2)
  | OpMod -> RMod (e1, e2)
  | OpLand -> RLand (e1, e2)
  | OpLor -> RLor (e1, e2)
  | OpBor -> RBor (e1, e2)
  | OpBand -> RBand (e1, e2)
  | OpBxor -> RBxor (e1, e2)
  | OpLsh -> RLsh (e1, e2)
  | OpRsh -> RRsh (e1, e2)
  | OpEq -> REq (e1, e2)
  | OpNe -> RNe (e1, e2)
  | OpLt -> RLt (e1, e2)
  | OpLe -> RLe (e1, e2)
  | OpGt -> RGt (e1, e2)
  | OpGe -> RGe (e1, e2)


let rec compile_stmt (env : env) (stmt : stmt) =
  let loc = stmt.loc in
  match stmt.s with
  | StmtDecl (lhs, None) ->
      let env = compile_dexp env lhs in
      env, []
  | StmtDecl (lhs, Some rhs) ->
      let env = compile_dexp env lhs in
      let lhs = compile_lexp env (dexp_to_lexp lhs) in
      let rhs = compile_exp env rhs in
      env, [ { i = Store (lhs, rhs); loc } ]
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


let compile_top (env : env) (s : top_stmt) =
  match s.top with
  | TopExternal ({ name; _ }, _) ->
      let index = env.fcount in
      let functions = Map.add name index env.functions in
      let env = { env with functions; fcount = env.fcount + 1 } in
      env, [ External ]
  | TopType _ -> env, []
  | TopFunction ({ name; args; _ }, body) ->
      let index = env.fcount in
      let functions = Map.add name index env.functions in
      let env = { locals = Map.empty; lcount = 0; functions; fcount = env.fcount + 1 } in
      let env = List.fold_left (fun env (n, _, _) -> addLocal env n) env args in
      let env, body = compile_stmt env body in
      env, [ Function { name; body; locals = env.lcount; index } ]


let compile stmts =
  let env, functions = list compile_top default_env stmts in
  env.functions, functions


let load (prog : top_stmt list) =
  let _table, _code = compile prog in
  ()
