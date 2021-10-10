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
open Core
open Util.Maps

type tag = Prog.tag

type type_ =
  | Void
  | Int
  | Real
  | String
  | Bool
  | Fixed
  | Array  of int * type_
  | Struct of struct_descr
  | Tuple  of type_ list

and struct_descr =
  { path : string
  ; members : param list
  }

and param = string * type_

type operator =
  | Add
  | Sub
  | Mul
  | Div
  | Mod
  | Land
  | Lor
  | Bor
  | Band
  | Bxor
  | Lsh
  | Rsh
  | Eq
  | Ne
  | Lt
  | Le
  | Gt
  | Ge

type uoperator =
  | Neg
  | Not

type exp_d =
  | Unit
  | Bool   of bool
  | Int    of int
  | Real   of float
  | String of string
  | Id     of string
  | Index  of
      { e : exp
      ; index : exp
      }
  | Array  of exp list
  | Call   of
      { path : string
      ; args : exp list
      }
  | UnOp   of uoperator * exp
  | Op     of operator * exp * exp
  | If     of
      { cond : exp
      ; then_ : exp
      ; else_ : exp
      }
  | Tuple  of exp list
  | Member of exp * string

and exp =
  { e : exp_d
  ; t : type_
  }

and lexp_d =
  | LWild
  | LId     of string
  | LMember of lexp * string
  | LIndex  of lexp * exp

and lexp =
  { l : lexp_d
  ; t : type_
  }

type dexp_d = DId of string * int option

and dexp =
  { d : dexp_d
  ; t : type_
  }

type function_info =
  { original_name : string option
  ; is_root : bool
  }

type stmt =
  | StmtDecl   of dexp * exp option
  | StmtBind   of lexp * exp
  | StmtReturn of exp
  | StmtBlock  of stmt list
  | StmtIf     of exp * stmt * stmt option
  | StmtWhile  of exp * stmt

and function_def =
  { name : string
  ; args : param list
  ; t : type_ list * type_
  ; tags : tag list
  ; loc : Util.Loc.t
  ; info : function_info
  }

type top_stmt =
  | TopExternal of function_def * string
  | TopFunction of function_def * stmt
  | TopType     of struct_descr
  | TopDecl     of dexp * exp

type code = top_stmt list

type env =
  { decl : (int option * type_) Map.t
  ; dummy : int
  }

let default_env = { decl = Map.empty; dummy = 0 }

module Convert = struct
  let rec type_ (t : Prog.type_) =
    match t with
    | { t = TVoid; _ } -> Void
    | { t = TInt; _ } -> Int
    | { t = TReal; _ } -> Real
    | { t = TString; _ } -> String
    | { t = TBool; _ } -> Bool
    | { t = TFixed; _ } -> Fixed
    | { t = TArray (dim, sub); _ } ->
        let sub = type_ sub in
        Array (dim, sub)
    | { t = TStruct descr; _ } ->
        let descr = struct_descr descr in
        Struct descr
    | { t = TTuple _; _ } -> failwith "no tuples"


  and struct_descr (d : Prog.struct_descr) : struct_descr =
    let members = List.map (fun (name, t, _) -> name, type_ t) d.members in
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


  let rec exp_d (e : Prog.exp_d) : exp_d =
    match e with
    | EUnit -> Unit
    | EBool v -> Bool v
    | EInt v -> Int v
    | EReal v -> Real v
    | EString s -> String s
    | EId id -> Id id
    | EIndex { e; index } ->
        let e = exp e in
        let index = exp index in
        Index { e; index }
    | EArray elems ->
        let elems = List.map exp elems in
        Array elems
    | ECall { path; args } ->
        let args = List.map exp args in
        Call { path; args }
    | EUnOp (op, e) ->
        let op = uoperator op in
        let e = exp e in
        UnOp (op, e)
    | EOp (op, e1, e2) ->
        let op = operator op in
        let e1 = exp e1 in
        let e2 = exp e2 in
        Op (op, e1, e2)
    | EIf { cond; then_; else_ } ->
        let cond = exp cond in
        let then_ = exp then_ in
        let else_ = exp else_ in
        If { cond; then_; else_ }
    | ETuple elems ->
        let elems = List.map exp elems in
        Tuple elems
    | EMember (e, n) ->
        let e = exp e in
        Member (e, n)


  and exp (e : Prog.exp) : exp =
    let t = type_ e.t in
    let e = exp_d e.e in
    { e; t }


  let rec lexp_d (l : Prog.lexp_d) : lexp_d =
    match l with
    | LWild -> LWild
    | LId id -> LId id
    | LMember (le, n) ->
        let le = lexp le in
        LMember (le, n)
    | LIndex { e; index } ->
        let e = lexp e in
        let index = exp index in
        LIndex (e, index)
    | LTuple _ -> failwith "lhs tuple"


  and lexp (l : Prog.lexp) : lexp =
    let t = type_ l.t in
    let l = lexp_d l.l in
    { l; t }


  let rec dexp_d (d : Prog.dexp_d) : dexp_d =
    match d with
    | DWild -> failwith "There should not be wild declarations"
    | DId (id, dim) -> DId (id, dim)
    | DTuple _ -> failwith "There should not be tuple declarations"


  and dexp (d : Prog.dexp) : dexp =
    let t = type_ d.t in
    let d = dexp_d d.d in
    { d; t }


  type decl =
    | Nothing
    | Initialize of string * int option * type_
    | Declare    of string * int option * type_

  let findRemove env n =
    match Map.find_opt n env.decl with
    | Some (dim, t) ->
        let decl = Map.remove n env.decl in
        { env with decl }, Some (dim, t)
    | None -> env, None


  let getInitRHS (t : type_) =
    match t with
    | Void -> Some { e = Unit; t }
    | Int -> Some { e = Int 0; t }
    | Real -> Some { e = Real 0.0; t }
    | Fixed -> Some { e = Real 0.0; t }
    | String -> Some { e = String ""; t }
    | Bool -> Some { e = Bool false; t }
    | _ -> None


  let getPendingDeclarations (env : env) =
    let stmts = Map.fold (fun n (dim, t) acc -> StmtDecl ({ d = DId (n, dim); t }, getInitRHS t) :: acc) env.decl [] in
    { env with decl = Map.empty }, stmts


  let rec getLDecl (env : env) (l : lexp) =
    match l with
    | { l = LId n; _ } ->
        ( match findRemove env n with
        | env, Some (dim, t) -> env, Initialize (n, dim, t)
        | env, None -> env, Nothing )
    | { l = LWild; _ } -> env, Nothing
    | { l = LMember (l, _); _ }
     |{ l = LIndex (l, _); _ } ->
        ( match getLDecl env l with
        | env, Initialize (n, dim, t) -> env, Declare (n, dim, t)
        | env, ret -> env, ret )


  let makeBlock stmts =
    match stmts with
    | [] -> StmtBlock []
    | [ h ] -> h
    | _ -> StmtBlock stmts


  let rec stmt (env : env) (s : Prog.stmt) =
    match s with
    | { s = StmtDecl { d = DId (n, dim); t; _ }; _ } ->
        let t = type_ t in
        let decl = Map.add n (dim, t) env.decl in
        { env with decl }, []
    | { s = StmtDecl _; _ } -> failwith "there should not be tuples or wild"
    | { s = StmtBind ({ l = LWild; _ }, rhs); _ } ->
        let rhs = exp rhs in
        env, [ StmtBind ({ l = LWild; t = Void }, rhs) ]
    | { s = StmtBind (lhs, rhs); _ } ->
        let lhs = lexp lhs in
        let rhs = exp rhs in
        ( match getLDecl env lhs with
        | env, Nothing -> env, [ StmtBind (lhs, rhs) ]
        | env, Initialize (n, dim, t) -> env, [ StmtDecl ({ d = DId (n, dim); t }, Some rhs) ]
        | env, Declare (n, dim, t) -> env, [ StmtDecl ({ d = DId (n, dim); t }, getInitRHS t); StmtBind (lhs, rhs) ] )
    | { s = StmtReturn e; _ } ->
        let e = exp e in
        env, [ StmtReturn e ]
    | { s = StmtIf (cond, then_, Some else_); _ } ->
        let env, stmts = getPendingDeclarations env in
        let cond = exp cond in
        let env, then_ = stmt env then_ in
        let env, else_ = stmt env else_ in
        env, stmts @ [ StmtIf (cond, makeBlock then_, Some (makeBlock else_)) ]
    | { s = StmtIf (cond, then_, None); _ } ->
        let env, stmts = getPendingDeclarations env in
        let cond = exp cond in
        let env, then_ = stmt env then_ in
        env, stmts @ [ StmtIf (cond, makeBlock then_, None) ]
    | { s = StmtWhile (cond, body); _ } ->
        let env, stmts = getPendingDeclarations env in
        let cond = exp cond in
        let env, body = stmt env body in
        env, stmts @ [ StmtWhile (cond, makeBlock body) ]
    | { s = StmtBlock body; _ } ->
        let env, stmts =
          List.fold_left
            (fun (env, acc) s ->
              let env, stmts = stmt env s in
              env, stmts :: acc)
            (env, [])
            body
        in
        env, List.flatten (List.rev stmts)


  let function_info (info : Prog.function_info) : function_info =
    { original_name = info.original_name; is_root = info.is_root }


  let function_def (def : Prog.function_def) : function_def =
    let name = def.name in
    let args = List.map (fun (name, t, _) -> name, type_ t) def.args in
    let args_t, ret_t = def.t in
    let ret_t = type_ ret_t in
    let args_t = List.map type_ args_t in
    { name; args; t = args_t, ret_t; tags = def.tags; loc = def.loc; info = function_info def.info }


  let top_stmt (top : Prog.top_stmt) : top_stmt =
    match top with
    | { top = TopFunction (def, body); _ } ->
        let _, body = stmt default_env body in
        let def = function_def def in
        TopFunction (def, makeBlock body)
    | { top = TopType descr; _ } ->
        let descr = struct_descr descr in
        TopType descr
    | { top = TopExternal (def, name); _ } ->
        let def = function_def def in
        TopExternal (def, name)


  let prog stmts = List.map top_stmt stmts
end
