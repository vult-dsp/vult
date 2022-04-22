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
  | Void of type_ list option
  | Int
  | Real
  | String
  | Bool
  | Fixed
  | Array of int * type_
  | Struct of struct_descr
  | Tuple of type_ list

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
  | Bool of bool
  | Int of int
  | Real of float
  | Fixed of float
  | String of string
  | Id of string
  | Index of
      { e : exp
      ; index : exp
      }
  | Array of exp list
  | Call of
      { path : string
      ; args : exp list
      }
  | UnOp of uoperator * exp
  | Op of operator * exp * exp
  | If of
      { cond : exp
      ; then_ : exp
      ; else_ : exp
      }
  | Tuple of exp list
  | Member of exp * string

and exp =
  { e : exp_d
  ; t : type_
  }

and lexp_d =
  | LWild
  | LId of string
  | LMember of lexp * string
  | LIndex of lexp * exp

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
  | StmtDecl of dexp * exp option
  | StmtBind of lexp * exp
  | StmtReturn of exp
  | StmtBlock of stmt list
  | StmtIf of exp * stmt * stmt option
  | StmtWhile of exp * stmt

and function_def =
  { name : string
  ; args : param list
  ; t : type_ list * type_
  ; tags : tag list
  ; loc : Util.Loc.t
  ; info : function_info
  }

type top_stmt =
  | TopExternal of function_def * string option
  | TopFunction of function_def * stmt
  | TopType of struct_descr
  | TopDecl of dexp * exp

type code = top_stmt list

type env =
  { decl : (int option * type_) Map.t
  ; dummy : int
  }

let default_env = { decl = Map.empty; dummy = 0 }

module Convert = struct
  type context =
    { ext_names : string Map.t
    ; args : Util.Args.args
    }

  let rec type_ (context : context) (t : Prog.type_) =
    match t with
    | { t = TVoid None; _ } -> Void None
    | { t = TVoid (Some elems); _ } ->
      let elems = List.map (type_ context) elems in
      Void (Some elems)
    | { t = TInt; _ } -> Int
    | { t = TReal; _ } when context.args.real = Fixed -> Fixed
    | { t = TReal; _ } -> Real
    | { t = TString; _ } -> String
    | { t = TBool; _ } -> Bool
    | { t = TFixed; _ } -> Fixed
    | { t = TArray (dim, sub); _ } ->
      let sub = type_ context sub in
      Array (dim, sub)
    | { t = TStruct descr; _ } ->
      let descr = struct_descr context descr in
      Struct descr
    | { t = TTuple _; loc } -> Util.Error.raiseError "no tuples" loc

  and struct_descr (context : context) (d : Prog.struct_descr) : struct_descr =
    let members = List.map (fun (name, t, _) -> name, type_ context t) d.members in
    { path = d.path; members }
  ;;

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
  ;;

  let uoperator (op : Prog.uoperator) : uoperator =
    match op with
    | UOpNeg -> Neg
    | UOpNot -> Not
  ;;

  let getCallName (context : context) name =
    match Map.find_opt name context.ext_names with
    | Some name -> name
    | _ -> name
  ;;

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

  and exp (context : context) (e : Prog.exp) : exp =
    let t = type_ context e.t in
    let e = exp_d context e.e in
    { e; t }
  ;;

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
  ;;

  let rec dexp_d (d : Prog.dexp_d) : dexp_d =
    match d with
    | DWild -> failwith "There should not be wild declarations"
    | DId (id, dim) -> DId (id, dim)
    | DTuple _ -> failwith "There should not be tuple declarations"

  and dexp (context : context) (d : Prog.dexp) : dexp =
    let t = type_ context d.t in
    let d = dexp_d d.d in
    { d; t }
  ;;

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
  ;;

  let getInitRHS (t : type_) =
    match t with
    | Void _ -> Some { e = Unit; t }
    | Int -> Some { e = Int 0; t }
    | Real -> Some { e = Real 0.0; t }
    | Fixed -> Some { e = Real 0.0; t }
    | String -> Some { e = String ""; t }
    | Bool -> Some { e = Bool false; t }
    | _ -> None
  ;;

  let getPendingDeclarations (env : env) =
    let stmts = Map.fold (fun n (dim, t) acc -> StmtDecl ({ d = DId (n, dim); t }, getInitRHS t) :: acc) env.decl [] in
    { env with decl = Map.empty }, stmts
  ;;

  let rec getLDecl (env : env) (l : lexp) =
    match l with
    | { l = LId n; _ } ->
      (match findRemove env n with
      | env, Some (dim, t) -> env, Initialize (n, dim, t)
      | env, None -> env, Nothing)
    | { l = LWild; _ } -> env, Nothing
    | { l = LMember (l, _); _ } | { l = LIndex (l, _); _ } ->
      (match getLDecl env l with
      | env, Initialize (n, dim, t) -> env, Declare (n, dim, t)
      | env, ret -> env, ret)
  ;;

  let makeBlock stmts =
    match stmts with
    | [] -> StmtBlock []
    | [ h ] -> h
    | _ -> StmtBlock stmts
  ;;

  let rec stmt (context : context) (env : env) (s : Prog.stmt) =
    match s with
    | { s = StmtDecl { d = DId (n, dim); t; _ }; _ } ->
      let t = type_ context t in
      let decl = Map.add n (dim, t) env.decl in
      { env with decl }, []
    | { s = StmtDecl _; _ } -> failwith "there should not be tuples or wild"
    | { s = StmtBind ({ l = LWild; _ }, ({ e = ECall _; _ } as rhs)); _ } ->
      let env, stmts = getPendingDeclarations env in
      let rhs = exp context rhs in
      env, stmts @ [ StmtBind ({ l = LWild; t = Void None }, rhs) ]
    | { s = StmtBind ({ l = LWild; _ }, _); _ } -> env, []
    | { s = StmtBind (lhs, rhs); _ } ->
      let lhs = lexp context lhs in
      let rhs = exp context rhs in
      (match getLDecl env lhs with
      | env, Nothing -> env, [ StmtBind (lhs, rhs) ]
      | env, Initialize (n, dim, t) -> env, [ StmtDecl ({ d = DId (n, dim); t }, Some rhs) ]
      | env, Declare (n, dim, t) -> env, [ StmtDecl ({ d = DId (n, dim); t }, getInitRHS t); StmtBind (lhs, rhs) ])
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
      let env, stmts = getPendingDeclarations env in
      let cond = exp context cond in
      let env, then_ = stmt context env then_ in
      env, stmts @ [ StmtIf (cond, makeBlock then_, None) ]
    | { s = StmtWhile (cond, body); _ } ->
      let env, stmts = getPendingDeclarations env in
      let cond = exp context cond in
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
  ;;

  let function_info (info : Prog.function_info) : function_info =
    { original_name = info.original_name; is_root = info.is_root }
  ;;

  let function_def (context : context) (def : Prog.function_def) : function_def =
    let name = def.name in
    let args = List.map (fun (name, t, _) -> name, type_ context t) def.args in
    let args_t, ret_t = def.t in
    let ret_t = type_ context ret_t in
    let args_t = List.map (type_ context) args_t in
    { name; args; t = args_t, ret_t; tags = def.tags; loc = def.loc; info = function_info def.info }
  ;;

  let top_stmt (context : context) (top : Prog.top_stmt) : top_stmt option =
    match top with
    | { top = TopFunction (def, _); _ } when Pparser.Ptags.has def.tags "placeholder" -> None
    | { top = TopFunction (def, body); _ } ->
      let _, body = stmt context default_env body in
      let def = function_def context def in
      Some (TopFunction (def, makeBlock body))
    | { top = TopType descr; _ } ->
      let descr = struct_descr context descr in
      Some (TopType descr)
    | { top = TopExternal (def, name); _ } ->
      let def = function_def context def in
      Some (TopExternal (def, name))
  ;;

  let registerExternalNames (stmts : Prog.top_stmt list) =
    List.fold_left
      (fun acc s ->
        match s with
        | Prog.{ top = TopExternal (def, Some name); _ } -> Map.add def.name name acc
        | _ -> acc)
      Map.empty
      stmts
  ;;

  let prog args stmts =
    let ext_names = registerExternalNames stmts in
    let context = { args; ext_names } in
    List.filter_map (top_stmt context) stmts
  ;;
end
