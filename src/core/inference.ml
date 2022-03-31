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
open Util
open Pparser
open Env
open Typed

module ToProg = struct
  open Prog
  module T = Typed
  open Util.Maps

  type state =
    { types : type_ Map.t
    ; dummy : int
    }

  let path (p : Syntax.path) : string =
    match p with
    | { id; n = None; _ } -> id
    | { id; n = Some n; _ } -> n ^ "_" ^ id
  ;;

  let list mapper (env : Env.in_top) (state : state) (l : 'a list) =
    let state, rev =
      List.fold_left
        (fun (state, acc) e ->
          let state, e = mapper env state e in
          state, e :: acc)
        (state, [])
        l
    in
    state, List.rev rev
  ;;

  let rec getDim (t : Typed.type_) =
    match t.tx with
    | T.TELink t -> getDim t
    | T.TESize dim -> dim
    | _ -> Error.raiseError "The size of the array could not be inferred. Please add type annotation." t.loc
  ;;

  let rec type_ (env : Env.in_top) (state : state) (t : Typed.type_) =
    let loc = t.loc in
    match t.tx with
    | T.TENoReturn -> state, { t = TVoid None; loc }
    | T.TEUnbound _ -> Error.raiseError "The type could not be infered. Please add type annotation." t.loc
    | T.TEOption _ -> Error.raiseError "undecided type" t.loc
    | T.TEId { id = "unit"; n = None; _ } -> state, { t = TVoid None; loc }
    | T.TEId { id = "int"; n = None; _ } -> state, { t = TInt; loc }
    | T.TEId { id = "real"; n = None; _ } -> state, { t = TReal; loc }
    | T.TEId { id = "fix16"; n = None; _ } -> state, { t = TFixed; loc }
    | T.TEId { id = "string"; n = None; _ } -> state, { t = TString; loc }
    | T.TEId { id = "bool"; n = None; _ } -> state, { t = TBool; loc }
    | T.TEId p ->
      let ps = path p in
      (match Map.find_opt ps state.types with
      | Some t -> state, t
      | None ->
        (match Env.getType env p with
        | None -> failwith "unknown type"
        | Some { descr = Enum _; _ } -> state, { t = TInt; loc }
        | Some { descr = Record members; _ } ->
          let members =
            List.map (fun (name, (var : Env.var)) -> name, var.t, var.loc) (Env.Map.to_list members)
            |> List.sort (fun (n1, _, _) (n2, _, _) -> compare n1 n2)
          in
          let state, members = type_list env state members in
          let t = { t = TStruct { path = ps; members }; loc } in
          let types = Map.add ps t state.types in
          { state with types }, t
        | Some { descr = Simple; _ } -> failwith "Type does not have members"))
    | T.TELink t -> type_ env state t
    | T.TEComposed ("array", [ t; dim ]) ->
      let state, t = type_ env state t in
      let dim = getDim dim in
      state, { t = TArray (dim, t); loc }
    | T.TEComposed ("tuple", elems) ->
      let state, elems = list type_ env state elems in
      state, { t = TTuple elems; loc }
    | T.TEComposed _ -> failwith "unknown composed"
    | T.TESize _ -> failwith "invalid input"

  and type_list (env : Env.in_top) (state : state) (l : (string * Typed.type_ * Loc.t) list) =
    let mapper env state ((n : string), (t : Typed.type_), (loc : Loc.t)) =
      let state, t = type_ env state t in
      state, (n, t, loc)
    in
    list mapper env state l
  ;;

  let operator op =
    match op with
    | "+" -> OpAdd
    | "-" -> OpSub
    | "*" -> OpMul
    | "/" -> OpDiv
    | "%" -> OpMod
    | "&&" -> OpLand
    | "||" -> OpLor
    | "|" -> OpBor
    | "&" -> OpBand
    | "^" -> OpBxor
    | "<<" -> OpLsh
    | ">>" -> OpRsh
    | "==" -> OpEq
    | "<>" -> OpNe
    | "<" -> OpLt
    | "<=" -> OpLe
    | ">" -> OpGt
    | ">=" -> OpGe
    | _ -> failwith "unknown operator"
  ;;

  let uoperator op =
    match op with
    | "-" -> UOpNeg
    | "not" -> UOpNot
    | _ -> failwith "unknown uoperator"
  ;;

  let rec exp (env : Env.in_top) (state : state) (e : Typed.exp) =
    let loc = e.loc in
    let state, t = type_ env state e.t in
    match e.e with
    | EUnit -> state, { e = EUnit; t; loc }
    | EBool v -> state, { e = EBool v; t; loc }
    | EInt n -> state, { e = EInt n; t; loc }
    | EReal n -> state, { e = EReal n; t; loc }
    | EFixed n -> state, { e = EFixed n; t; loc }
    | EString s -> state, { e = EString s; t; loc }
    | EId id -> state, { e = EId id; t; loc }
    | EIndex { e; index } ->
      let state, e = exp env state e in
      let state, index = exp env state index in
      state, { e = EIndex { e; index }; t; loc }
    | EArray l ->
      let state, l = list exp env state l in
      state, { e = EArray l; t; loc }
    | ECall { path = p; args; _ } ->
      let p = path p in
      let state, args = list exp env state args in
      state, { e = ECall { path = p; args }; t; loc }
    | EUnOp (op, e) ->
      let op = uoperator op in
      let state, e = exp env state e in
      state, { e = EUnOp (op, e); t; loc }
    | EOp (op, e1, e2) ->
      let state, e1 = exp env state e1 in
      let state, e2 = exp env state e2 in
      let op = operator op in
      state, { e = EOp (op, e1, e2); t; loc }
    | EIf { cond; then_; else_ } ->
      let state, cond = exp env state cond in
      let state, then_ = exp env state then_ in
      let state, else_ = exp env state else_ in
      state, { e = EIf { cond; then_; else_ }; t; loc }
    | ETuple l ->
      let state, l = list exp env state l in
      state, { e = ETuple l; t; loc }
    | EMember (e, m) ->
      let state, e = exp env state e in
      state, { e = EMember (e, m); t; loc }
  ;;

  let rec lexp (env : Env.in_top) (state : state) (e : Typed.lexp) =
    let loc = e.loc in
    let state, t = type_ env state e.t in
    match e.l with
    | LWild -> state, { l = LWild; t; loc }
    | LId s -> state, { l = LId s; t; loc }
    | LMember (e, m) ->
      let state, e = lexp env state e in
      state, { l = LMember (e, m); t; loc }
    | LIndex { e; index } ->
      let state, e = lexp env state e in
      let state, index = exp env state index in
      state, { l = LIndex { e; index }; t; loc }
    | LTuple l ->
      let state, l = list lexp env state l in
      state, { l = LTuple l; t; loc }
  ;;

  let rec dexp (env : Env.in_top) (state : state) (e : Typed.dexp) =
    let loc = e.loc in
    let state, t = type_ env state e.t in
    match e.d with
    | DWild -> state, { d = DWild; t; loc }
    | DId (id, None) -> state, { d = DId (id, None); t; loc }
    | DId (id, Some dim) -> state, { d = DId (id, Some dim); t; loc }
    | DTuple l ->
      let state, l = list dexp env state l in
      state, { d = DTuple l; t; loc }
  ;;

  let block (stmts : stmt list) : stmt =
    match stmts with
    | [] -> { s = StmtBlock []; loc = Loc.default }
    | [ s ] -> s
    | _ -> { s = StmtBlock stmts; loc = Loc.default }
  ;;

  let rec stmt (env : Env.in_top) (state : state) (s : Typed.stmt) =
    let loc = s.loc in
    match s.s with
    | StmtVal lhs ->
      let state, lhs = dexp env state lhs in
      state, [ { s = StmtDecl lhs; loc } ]
    | StmtMem (_, _) -> state, []
    | StmtBind (lhs, rhs) ->
      let state, lhs = lexp env state lhs in
      let state, rhs = exp env state rhs in
      state, [ { s = StmtBind (lhs, rhs); loc } ]
    | StmtReturn e ->
      let state, e = exp env state e in
      state, [ { s = StmtReturn e; loc } ]
    | StmtIf (cond, then_, None) ->
      let state, cond = exp env state cond in
      let state, then_ = stmt env state then_ in
      state, [ { s = StmtIf (cond, block then_, None); loc } ]
    | StmtIf (cond, then_, Some else_) ->
      let state, cond = exp env state cond in
      let state, then_ = stmt env state then_ in
      let state, else_ = stmt env state else_ in
      state, [ { s = StmtIf (cond, block then_, Some (block else_)); loc } ]
    | StmtWhile (cond, s) ->
      let state, cond = exp env state cond in
      let state, s = stmt env state s in
      state, [ { s = StmtWhile (cond, block s); loc } ]
    | StmtBlock stmts ->
      let state, stmts = list stmt env state stmts in
      (match List.flatten stmts with
      | [] -> state, [ { s = StmtBlock []; loc } ]
      | [ { s = StmtBlock subs; _ } ] -> state, [ { s = StmtBlock subs; loc } ]
      | [ s ] -> state, [ s ]
      | subs -> state, [ { s = StmtBlock subs; loc } ])
  ;;

  let arg (env : Env.in_top) (state : state) (n, (t : Typed.type_), loc) =
    let state, t = type_ env state t in
    state, (n, t, loc)
  ;;

  let function_type env state t =
    match t with
    | args, ret ->
      let state, args = list type_ env state args in
      let state, ret = type_ env state ret in
      state, (args, ret)
  ;;

  let rec function_def (env : Env.in_top) (state : state) (def : Typed.function_def) body =
    let name = path def.name in
    let state, args = list arg env state def.args in
    let state, t = function_type env state def.t in
    let state, body = stmt env state body in
    let body = block body in
    let state, next = next_def env state def.next in
    let original_name = Some (Pla.print (Syntax.print_path def.name)) in
    let info = { original_name; is_root = def.is_root } in
    let stmt = { top = TopFunction ({ name; args; t; loc = def.loc; tags = def.tags; info }, body); loc = def.loc } in
    state, stmt :: next

  and next_def env state def_opt =
    match def_opt with
    | None -> state, []
    | Some (def, body) -> function_def env state def body
  ;;

  let ext_function_def (env : Env.in_top) (state : state) (def : Typed.function_def) (linkname : string option) =
    let name = path def.name in
    let state, args = list arg env state def.args in
    let state, t = function_type env state def.t in
    let state, next = next_def env state def.next in
    let original_name = Some (Pla.print (Syntax.print_path def.name)) in
    let info = { original_name; is_root = false } in
    let stmt =
      { top = TopExternal ({ name; args; t; loc = def.loc; tags = def.tags; info }, linkname); loc = def.loc }
    in
    state, stmt :: next
  ;;

  let top_stmt (env : Env.in_top) (state : state) (t : Typed.top_stmt) =
    match t.top with
    | TopFunction (def, body) ->
      let state, functions = function_def env state def body in
      state, functions
    | TopExternal (def, linkname) ->
      let state, functions = ext_function_def env state def linkname in
      state, functions
    | TopType { path = p; members } ->
      let p = path p in
      let state, members = type_list env state members in
      let struct_descr = { path = p; members } in
      let t = { t = TStruct struct_descr; loc = t.loc } in
      let types = Map.add p t state.types in
      { state with types }, [ { top = TopType struct_descr; loc = t.loc } ]
    | TopEnum _ -> state, []
  ;;

  let top_stmt_list (env : Env.in_top) (state : state) (t : Typed.top_stmt list) = list top_stmt env state t

  let main env stmts =
    let state = { types = Map.empty; dummy = 0 } in
    let _, t = top_stmt_list env state stmts in
    List.flatten t
  ;;

  let isType s =
    match s with
    | { top = TopType _; _ } -> true
    | _ -> false
  ;;

  let convert env stmts =
    let stmts = main env stmts in
    let types, functions = List.partition isType stmts in
    let initializers = CCList.map Initializer.(createInitFunction RefObject) types in
    env, types @ initializers @ functions
  ;;
end

let context_name = "_ctx"

let pickLoc (t1 : type_) (t2 : type_) : unit =
  if t1.loc == Loc.default then t1.loc <- t2.loc else if t2.loc == Loc.default then t2.loc <- t1.loc
;;

let linkType ~from ~into =
  into.tx <- TELink from;
  pickLoc from into;
  true
;;

let rec unlink (t : type_) =
  match t.tx with
  | TELink t -> unlink t
  | _ -> t
;;

let path_string (p : Syntax.path) : string =
  match p with
  | { id; n = None; _ } -> id
  | { id; n = Some n; _ } -> n ^ "_" ^ id
;;

let constrainOption l1 l2 =
  let l2_ = List.filter (fun e2 -> List.exists (fun e1 -> compare_type_ e1 e2 = 0) l1) l2 in
  let l1_ = List.filter (fun e1 -> List.exists (fun e2 -> compare_type_ e2 e1 = 0) l2_) l1 in
  match l1_ with
  | [] -> failwith "cannot unify the two options"
  | [ t ] -> t
  | l -> { tx = TEOption l; loc = Loc.default }
;;

let rec pickOption original l tt =
  let rec loop l =
    match l with
    | [] -> false
    | h :: t -> if unify h tt then linkType ~from:tt ~into:original else loop t
  in
  loop l

and unify (t1 : type_) (t2 : type_) =
  if t1 == t2
  then true
  else (
    match t1.tx, t2.tx with
    | TEId t1, TEId t2 -> Pparser.Syntax.compare_path t1 t2 = 0
    | TESize t1, TESize t2 -> t1 = t2
    | TEComposed (n1, e1), TEComposed (n2, e2) when n1 = n2 && List.length e1 = List.length e2 ->
      List.for_all2 unify e1 e2
    (* follow the links *)
    | TELink tlink, _ -> unify tlink t2
    | _, TELink tlink -> unify t1 tlink
    | TENoReturn, _ -> linkType ~from:t2 ~into:t1
    | _, TENoReturn -> linkType ~from:t1 ~into:t2
    (* replace any unbound *)
    | TEUnbound _, _ -> linkType ~from:t2 ~into:t1
    | _, TEUnbound _ -> linkType ~from:t1 ~into:t2
    (* types with alternatives *)
    | TEOption l1, TEOption l2 ->
      let t3 = constrainOption l1 l2 in
      let _ = linkType ~from:t3 ~into:t2 in
      linkType ~from:t3 ~into:t1
    | TEOption l, _ -> pickOption t1 l t2
    | _, TEOption l -> pickOption t2 l t1
    | TEId _, _ -> false
    | TESize _, _ -> false
    | TEComposed _, _ -> false)
;;

let unifyRaise (loc : Loc.t) (t1 : type_) (t2 : type_) : unit =
  (* TODO: improve unify error reporting for tuples *)
  let raise = true in
  if not (unify t1 t2)
  then (
    let msg =
      let t1 = print_type_ t1 in
      let t2 = print_type_ t2 in
      Pla.print [%pla {|This expression has type '<#t2#>' but '<#t1#>' was expected|}]
    in
    if raise
    then Error.raiseError msg loc
    else (
      print_endline (Loc.to_string loc);
      print_endline msg))
;;

let rec type_in_m (env : in_module) (t : Syntax.type_) =
  match t with
  | { t = STId path; loc } ->
    let found = Env.lookTypeInModule env path loc in
    { tx = TEId found.path; loc }
  | { t = STSize n; loc } -> { tx = TESize n; loc }
  | { t = STComposed (name, l); loc } ->
    let l = List.map (type_in_m env) l in
    { tx = TEComposed (name, l); loc }
;;

let type_in_c (env : Env.in_context) (t : Syntax.type_) = type_in_m (Env.exitContext env) t
let type_in_f (env : Env.in_func) (t : Syntax.type_) = type_in_c (Env.exitFunction env) t

let applyFunction loc (args_t_in : type_ list) (ret : type_) (args : exp list) =
  let rec loop (args_t : type_ list) args =
    match args_t, args with
    | [], _ :: _ ->
      let required = Pla.map_sep Pla.commaspace print_type_ args_t_in in
      let msg = Pla.print [%pla {|Extra arguments in function call. Expecting: (<#required#>)|}] in
      Error.raiseError msg loc
    | _ :: _, [] ->
      let required = Pla.map_sep Pla.commaspace print_type_ args_t_in in
      let msg = Pla.print [%pla {|Missing arguments in function call. Expecting: (<#required#>)|}] in
      Error.raiseError msg loc
    | [], [] -> ret
    | h :: args_t, (ht : exp) :: args ->
      unifyRaise ht.loc h ht.t;
      loop args_t args
  in
  loop args_t_in args
;;

let addContextArg (env : Env.in_func) instance (f : Env.f) args loc =
  if Env.isFunctionActive f
  then (
    let cpath = Env.getContext env in
    let fpath = Env.getFunctionContext f in
    if Syntax.compare_path cpath fpath = 0
    then (
      let t = C.path loc fpath in
      let e = { e = EId context_name; t; loc } in
      env, e :: args)
    else (
      let instance =
        let number =
          Printf.sprintf
            "%.2x%.2x"
            (0xFF land Hashtbl.hash (path_string fpath))
            (0xFF land Hashtbl.hash (path_string cpath))
        in
        match instance with
        | Some i -> i ^ "_" ^ number
        | None ->
          let n = Env.getFunctionTick env in
          "inst_" ^ string_of_int n ^ number
      in
      let t = C.path loc fpath in
      let ctx_t = C.path loc cpath in
      let env = Env.addVar env unify instance t Inst loc in
      let e = { e = EMember ({ e = EId context_name; t = ctx_t; loc }, instance); loc; t } in
      env, e :: args))
  else env, args
;;

let rec exp (env : Env.in_func) (e : Syntax.exp) : Env.in_func * exp =
  match e with
  | { e = SEUnit; loc } ->
    let t = C.unit ~loc in
    env, { e = EUnit; t; loc }
  | { e = SEBool value; loc } ->
    let t = C.bool ~loc in
    env, { e = EBool value; t; loc }
  | { e = SEInt value; loc } ->
    let t = C.int ~loc in
    env, { e = EInt value; t; loc }
  | { e = SEReal value; loc } ->
    let t = C.real ~loc in
    env, { e = EReal (float_of_string value); t; loc }
  | { e = SEFixed value; loc } ->
    let t = C.fix16 ~loc in
    let value = String.sub value 0 (String.length value - 1) in
    env, { e = EFixed (float_of_string value); t; loc }
  | { e = SEString value; loc } ->
    let t = C.string ~loc in
    env, { e = EString value; t; loc }
  | { e = SEGroup e; _ } -> exp env e
  | { e = SEId name; loc } ->
    let var = Env.lookVar env name loc in
    let t = var.t in
    let e =
      match var.kind with
      | Val -> { e = EId name; t; loc }
      | Mem | Inst ->
        let ctx = Env.getContext env in
        let ctx_t = C.path loc ctx in
        { e = EMember ({ e = EId context_name; t = ctx_t; loc }, name); t; loc }
    in
    env, e
  | { e = SEIndex { e; index }; loc } ->
    let env, e = exp env e in
    let env, index = exp env index in
    let t = C.unbound Loc.default in
    unifyRaise e.loc (C.array t) e.t;
    unifyRaise index.loc (C.int ~loc:Loc.default) index.t;
    env, { e = EIndex { e; index }; t; loc }
  | { e = SEArray []; _ } -> failwith "empty array"
  | { e = SEArray (h :: t); loc } ->
    let env, h = exp env h in
    let env, t_rev, size =
      List.fold_left
        (fun (env, acc, size) e ->
          let env, e = exp env e in
          unifyRaise e.loc h.t e.t;
          env, e :: acc, size + 1)
        (env, [], 1)
        t
    in
    let t = C.array ~size:(C.size ~loc size) h.t in
    env, { e = EArray (h :: List.rev t_rev); t; loc }
  | { e = SETuple l; loc } ->
    let env, l = exp_list env l in
    let t = C.tuple ~loc (List.map (fun (e : exp) -> e.t) l) in
    env, { e = ETuple l; t; loc }
  | { e = SEIf { cond; then_; else_ }; loc } ->
    let env, cond = exp env cond in
    let env, then_ = exp env then_ in
    let env, else_ = exp env else_ in
    let t = then_.t in
    unifyRaise cond.loc (C.bool ~loc) cond.t;
    unifyRaise else_.loc then_.t else_.t;
    env, { e = EIf { cond; then_; else_ }; t; loc }
  | { e = SECall { instance; path; args }; loc } ->
    let env, args = exp_list env args in
    let f = Env.lookFunctionCall env path loc in
    let args_t, ret = f.t in
    let t = applyFunction e.loc args_t ret args in
    let env, args = addContextArg env instance f args loc in
    env, { e = ECall { instance = None; path = f.path; args }; t; loc }
  | { e = SEOp (op, e1, e2); loc } ->
    let env, e1 = exp env e1 in
    let env, e2 = exp env e2 in
    let f = Env.lookOperator env op in
    let args_t, ret = f.t in
    let t = applyFunction e.loc args_t ret [ e1; e2 ] in
    env, { e = EOp (op, e1, e2); t; loc }
  | { e = SEUnOp (op, e); loc } ->
    let env, e = exp env e in
    let f = Env.lookOperator env ("u" ^ op) in
    let args_t, ret = f.t in
    let t = applyFunction e.loc args_t ret [ e ] in
    env, { e = EUnOp (op, e); t; loc }
  | { e = SEMember (e, m); loc } ->
    let env, e = exp env e in
    (match (unlink e.t).tx with
    | TEId path ->
      (match Env.lookType env path with
      | { path; descr = Record members; _ } ->
        (match Map.find m members with
        | None -> Error.raiseError ("The field '" ^ m ^ "' is not part of the type '" ^ pathString path ^ "'") loc
        | Some { t; _ } -> env, { e = EMember (e, m); t; loc })
      | _ -> failwith "Not a record type")
    | _ -> failwith "exp: invalid access to type")
  | { e = SEEnum path; loc } ->
    let type_path, tloc, index = Env.lookEnum env path loc in
    let t = C.path tloc type_path in
    env, { e = EInt index; t; loc }

and exp_list (env : Env.in_func) (l : Syntax.exp list) : Env.in_func * exp list =
  let env, rev_l =
    List.fold_left
      (fun (env, acc) e ->
        let env, e = exp env e in
        env, e :: acc)
      (env, [])
      l
  in
  env, List.rev rev_l

and lexp (env : Env.in_func) (e : Syntax.lexp) : Env.in_func * lexp =
  match e with
  | { l = SLWild; loc } ->
    let t = C.noreturn loc in
    env, { l = LWild; t; loc }
  | { l = SLId name; loc } ->
    let var = Env.lookVar env name loc in
    let t = var.t in
    let e =
      match var.kind with
      | Val -> { l = LId name; t; loc }
      | Mem | Inst ->
        let ctx = Env.getContext env in
        let ctx_t = C.path loc ctx in
        { l = LMember ({ l = LId context_name; t = ctx_t; loc }, name); t; loc }
    in
    env, e
  | { l = SLGroup e; _ } -> lexp env e
  | { l = SLTuple elems; loc } ->
    let env, elems =
      List.fold_left
        (fun (env, acc) e ->
          let env, e = lexp env e in
          env, e :: acc)
        (env, [])
        (List.rev elems)
    in
    let t_elems = List.map (fun (e : lexp) -> e.t) elems in
    let t = C.tuple ~loc t_elems in
    env, { l = LTuple elems; t; loc }
  | { l = SLIndex { e; index }; loc } ->
    let env, e = lexp env e in
    let env, index = exp env index in
    let t = C.unbound loc in
    unifyRaise index.loc (C.int ~loc) index.t;
    unifyRaise e.loc (C.array ~loc t) e.t;
    env, { l = LIndex { e; index }; t; loc }
  | { l = SLMember (e, m); loc } ->
    let env, e = lexp env e in
    (match (unlink e.t).tx with
    | TEId path ->
      (match Env.lookType env path with
      | { path; descr = Record members; _ } ->
        (match Map.find m members with
        | None -> Error.raiseError ("The field '" ^ m ^ "' is not part of the type '" ^ pathString path ^ "'") loc
        | Some { t; _ } -> env, { l = LMember (e, m); t; loc })
      | _ -> failwith "Not a record type")
    | _ -> failwith "lexp: invalid access to type")

and dexp (env : Env.in_func) (e : Syntax.dexp) (kind : var_kind) : Env.in_func * dexp =
  match e with
  | { d = SDWild; loc } ->
    let t = C.noreturn loc in
    env, { d = DWild; t; loc }
  | { d = SDTuple l; loc } ->
    let env, l =
      List.fold_left
        (fun (env, acc) e ->
          let env, e = dexp env e kind in
          env, e :: acc)
        (env, [])
        (List.rev l)
    in
    let t = C.tuple ~loc (List.map (fun (e : dexp) -> e.t) l) in
    env, { d = DTuple l; t; loc }
  | { d = SDGroup e; _ } -> dexp env e kind
  | { d = SDTyped (e, t); _ } ->
    let env, e = dexp env e kind in
    let t = type_in_f env t in
    unifyRaise e.loc t e.t;
    env, e
  | { d = SDId (name, dims); loc } ->
    let t =
      match dims with
      | Some size -> C.array ~loc ~size:(C.size ~loc size) (C.unbound loc)
      | None -> C.unbound loc
    in
    let env = Env.addVar env unify name t kind loc in
    env, { d = DId (name, dims); t; loc }
;;

let rec dexp_to_lexp (d : Syntax.dexp) : Syntax.lexp =
  let loc = d.loc in
  match d.d with
  | SDTuple l ->
    let l = List.map dexp_to_lexp l in
    { l = SLTuple l; loc }
  | SDWild -> { l = SLWild; loc }
  | SDId (name, _) -> { l = SLId name; loc }
  | SDGroup e -> dexp_to_lexp e
  | SDTyped (e, _) -> dexp_to_lexp e
;;

let stmt_block (stmts : stmt list) =
  match stmts with
  | [ s ] -> s
  | _ -> { s = StmtBlock stmts; loc = Loc.default }
;;

let makeIterWhile name id_loc value body loc =
  let open Syntax in
  let int_type = { t = STId { id = "int"; n = None; loc = id_loc }; loc } in
  let dlhs = { d = SDTyped ({ d = SDId (name, None); loc = id_loc }, int_type); loc = id_loc } in
  let lhs = { l = SLId name; loc = id_loc } in
  let rhs = { e = SEId name; loc = id_loc } in
  let decl = { s = SStmtVal (dlhs, Some { e = SEInt 0; loc }); loc } in
  let incr = { s = SStmtBind (lhs, { e = SEOp ("+", rhs, { e = SEInt 1; loc }); loc }); loc } in
  let new_body = { s = SStmtBlock [ body; incr ]; loc } in
  let cond = { e = SEOp ("<", rhs, value); loc } in
  let while_s = { s = SStmtWhile (cond, new_body); loc } in
  { s = SStmtBlock [ decl; while_s ]; loc }
;;

let rec stmt (env : Env.in_func) (return : type_) (s : Syntax.stmt) : Env.in_func * stmt list =
  match s with
  | { s = SStmtError; _ } -> env, []
  | { s = SStmtBlock stmts; loc } ->
    let env = Env.pushScope env in
    let env, stmts = stmt_list env return stmts in
    let env = Env.popScope env in
    env, [ { s = StmtBlock stmts; loc } ]
  | { s = SStmtVal (lhs, None); loc } ->
    let env, lhs = dexp env lhs Val in
    env, [ { s = StmtVal lhs; loc } ]
  | { s = SStmtVal (lhs, Some rhs); loc } ->
    let env, dlhs = dexp env lhs Val in
    let env, lhs = lexp env (dexp_to_lexp lhs) in
    let env, rhs = exp env rhs in
    unifyRaise rhs.loc dlhs.t rhs.t;
    env, [ { s = StmtVal dlhs; loc }; { s = StmtBind (lhs, rhs); loc } ]
  | { s = SStmtMem (lhs, None, tag); loc } ->
    let env, lhs = dexp env lhs Mem in
    env, [ { s = StmtMem (lhs, tag); loc } ]
  | { s = SStmtMem (lhs, Some rhs, tag); loc } ->
    let env, dlhs = dexp env lhs Mem in
    let env, lhs = lexp env (dexp_to_lexp lhs) in
    let env, rhs = exp env rhs in
    unifyRaise rhs.loc lhs.t rhs.t;
    env, [ { s = StmtMem (dlhs, tag); loc }; { s = StmtBind (lhs, rhs); loc } ]
  | { s = SStmtBind (lhs, rhs); loc } ->
    let env, lhs = lexp env lhs in
    let env, rhs = exp env rhs in
    unifyRaise rhs.loc lhs.t rhs.t;
    env, [ { s = StmtBind (lhs, rhs); loc } ]
  | { s = SStmtReturn e; loc } ->
    let env, e = exp env e in
    unifyRaise e.loc return e.t;
    env, [ { s = StmtReturn e; loc } ]
  | { s = SStmtIf (cond, then_, else_); loc } ->
    let env, cond = exp env cond in
    unifyRaise cond.loc (C.bool ~loc) cond.t;
    let env, then_ = stmt env return then_ in
    let env, else_ = stmt_opt env return else_ in
    env, [ { s = StmtIf (cond, stmt_block then_, else_); loc } ]
  | { s = SStmtWhile (cond, s); loc } ->
    let env, cond = exp env cond in
    unifyRaise cond.loc (C.bool ~loc) cond.t;
    let env, s = stmt env return s in
    env, [ { s = StmtWhile (cond, stmt_block s); loc } ]
  | { s = SStmtIter { id = name, id_loc; value; body }; loc } ->
    let while_s = makeIterWhile name id_loc value body loc in
    stmt env return while_s

and stmt_opt env return s =
  match s with
  | None -> env, None
  | Some s ->
    let env, s = stmt env return s in
    env, Some (stmt_block s)

and stmt_list env return l =
  let env, l_rev =
    List.fold_left
      (fun (env, acc) s ->
        let env, s = stmt env return s in
        env, s :: acc)
      (env, [])
      l
  in
  env, List.flatten (List.rev l_rev)
;;

let addGeneratedFunctions tags name next =
  if Ptags.has tags "wave"
  then (
    let code = Pla.print [%pla {|fun <#name#s>_samples() @[placeholder] : int|}] in
    let def = Parse.parseFunctionSpec code in
    Some ({ def with next }, Syntax.{ s = SStmtBlock []; loc = Loc.default }))
  else next
;;

let getOptType env loc (t : Syntax.type_ option) =
  match t with
  | None -> C.unbound loc
  | Some t -> type_in_c env t
;;

let getReturnType env loc (t : Syntax.type_ option) =
  match t with
  | None -> C.noreturn loc
  | Some t -> type_in_c env t
;;

let convertArguments env (args : Syntax.arg list) : arg list =
  List.map (fun (name, t, loc) -> name, getOptType env loc t, loc) args
;;

let registerMultiReturnMem (env : Env.in_context) name t loc =
  let _, ret = t in
  match unlink ret with
  | { tx = TEComposed ("tuple", elems); _ } ->
    let names = List.mapi (fun i t -> path_string name ^ "_ret_" ^ string_of_int i, t) elems in
    List.fold_left (fun env (name, t) -> Env.addReturnVar env name t loc) env names
  | _ -> env
;;

let isRoot (args : Args.args) path =
  let s_path = Pla.print (Syntax.print_path path) in
  List.mem s_path args.roots
;;

let rec function_def (iargs : Args.args) (env : Env.in_context) ((def : Syntax.function_def), (body : Syntax.stmt))
    : Env.in_context * (function_def * stmt)
  =
  let ret = getReturnType env def.loc def.t in
  let args = convertArguments env def.args in
  let env, path, t = Env.enterFunction env def.name args ret def.loc in
  let env, body = stmt env ret body in
  let env = Env.exitFunction env in
  let next = addGeneratedFunctions def.tags def.name def.next in
  let env, next = function_def_opt iargs env next in
  let env = registerMultiReturnMem env path t def.loc in
  let is_root = isRoot iargs path in
  env, ({ name = path; args; t; loc = def.loc; tags = def.tags; next; is_root }, stmt_block body)

and function_def_opt (iargs : Args.args) (env : Env.in_context) def_opt =
  match def_opt with
  | None -> env, None
  | Some def_body ->
    let env, def_body = function_def iargs env def_body in
    env, Some def_body
;;

let ext_function (iargs : Args.args) (env : Env.in_context) (def : Syntax.ext_def) : Env.in_context * function_def =
  let ret = getOptType env def.loc def.t in
  let args = convertArguments env def.args in
  let env, path, t = Env.enterFunction env def.name args ret def.loc in
  let env = Env.exitFunction env in
  let next = addGeneratedFunctions def.tags def.name None in
  let env, next = function_def_opt iargs env next in
  env, { name = path; args; t; loc = def.loc; tags = def.tags; next; is_root = false }
;;

let getContextArgument (context : Env.context) loc : arg option =
  match context with
  | Some (p, { descr = Record members; _ }) ->
    if Map.is_empty members
    then None
    else (
      let ctx_t = C.path loc p in
      Some (context_name, ctx_t, loc))
  | _ -> None
;;

let insertContextArgument (env : Env.in_context) (def : function_def) : function_def =
  match getContextArgument env.context def.loc with
  | None -> def
  | Some arg ->
    let rec loop next =
      match next with
      | Some (def, body) ->
        let next = loop def.next in
        Some ({ def with args = arg :: def.args; next }, body)
      | None -> None
    in
    let next = loop def.next in
    { def with args = arg :: def.args; next }
;;

let rec top_stmt (iargs : Args.args) (env : Env.in_module) (s : Syntax.top_stmt) : Env.in_module * top_stmt =
  match s with
  | { top = STopError; _ } -> failwith "Parser error"
  | { top = STopFunction (def, body); _ } ->
    let env = Env.createContextForFunction env def.name def.loc in
    let env, (def, body) = function_def iargs env (def, body) in
    let def = insertContextArgument env def in
    let env = Env.exitContext env in
    env, { top = TopFunction (def, body); loc = def.loc }
  | { top = STopExternal (def, link_name); _ } ->
    let env = Env.createContextForExternal env in
    let env, def = ext_function iargs env def in
    let env = Env.exitContext env in
    env, { top = TopExternal (def, link_name); loc = def.loc }
  | { top = STopType { name; members }; loc } ->
    let members = List.map (fun (name, t, loc) -> name, type_in_m env t, loc) members in
    let members = List.sort (fun (n1, _, _) (n2, _, _) -> compare n1 n2) members in
    let env = Env.addType env name members loc in
    let path = Env.getPath env.m name loc in
    env, { top = TopType { path; members }; loc }
  | { top = STopEnum { name; members }; loc } ->
    let env = Env.addEnum env name members loc in
    let path = Env.getPath env.m name loc in
    env, { top = TopEnum { path; members }; loc }

and top_stmt_list (iargs : Args.args) (env : Env.in_module) (s : Syntax.top_stmt list) : Env.in_module * top_stmt list =
  let env, rev_s =
    List.fold_left
      (fun (env, acc) s ->
        let env, s = top_stmt iargs env s in
        env, s :: acc)
      (env, [])
      s
  in
  env, rev_s
;;

let getTypesFromModule m =
  Map.fold
    (fun _ t s ->
      match t.descr with
      | Record members when Map.is_empty members -> s
      | Record _ -> t :: s
      | Simple | Enum _ -> s)
    []
    m.Env.types
;;

let createTypes (env : Env.in_top) =
  let types =
    Map.fold
      (fun _ m s ->
        let types = getTypesFromModule m in
        types @ s)
      []
      env.modules
  in
  (* sort the types *)
  let types =
    types |> List.filter (fun (t : Env.t) -> t.generated) |> List.sort (fun (a : Env.t) b -> compare a.index b.index)
  in
  List.map
    (fun (t : Env.t) ->
      match t.descr with
      | Record members ->
        let members = Map.fold (fun _ (var : Env.var) s -> (var.name, var.t, var.loc) :: s) [] members in
        let members = List.sort (fun (n1, _, _) (n2, _, _) -> compare n1 n2) members in
        { top = TopType { path = t.path; members }; loc = t.loc }
      | Enum _ | Simple -> failwith "There should not be other than records here")
    types
;;

module Set = Set.Make (struct
  type t = path

  let compare = Syntax.compare_path
end)

let rec createExistingTypeSet stmts : Set.t =
  match stmts with
  | [] -> Set.empty
  | { top = TopType { path; _ }; _ } :: t -> Set.add path (createExistingTypeSet t)
  | _ :: t -> createExistingTypeSet t
;;

let removeExistingTypes set types =
  let f s =
    match s with
    | { top = TopType { path; _ }; _ } when Set.mem path set -> false
    | _ -> true
  in
  List.filter f types
;;

let infer_single (iargs : Args.args) (env : Env.in_top) (h : Parse.parsed_file) : Env.in_top * Prog.top_stmt list =
  let set = createExistingTypeSet (createTypes env) in
  let env = Env.enterModule env h.name in
  let env, stmt = top_stmt_list iargs env h.stmts in
  let env = Env.exitModule env in
  let types = removeExistingTypes set (createTypes env) in
  ToProg.convert env (stmt @ types)
;;

let infer (iargs : Args.args) (parsed : Parse.parsed_file list) : Env.in_top * Prog.top_stmt list =
  let env, stmts =
    List.fold_left
      (fun (env, acc) (h : Parse.parsed_file) ->
        let env = Env.enterModule env h.name in
        let env, stmt = top_stmt_list iargs env h.stmts in
        let env = Env.exitModule env in
        env, stmt @ acc)
      (Env.empty (), [])
      parsed
  in
  let types = createTypes env in
  ToProg.convert env (types @ List.rev stmts)
;;
