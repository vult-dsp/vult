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
open Util
open Pparser
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


let rec getDim (t : Typed.type_) =
  match t.tx with
  | T.TELink t -> getDim t
  | T.TESize dim -> dim
  | _ -> Error.raiseError "The size of the array could not be inferred. Please add a type annotation." t.loc


let rec type_ ?(const = false) (env : Env.in_top) (state : state) (t : Typed.type_) =
  let const = const || Typed.isTypeConst t in
  let loc = t.loc in
  match t.tx with
  | T.TENoReturn -> state, { t = TVoid None; const; loc }
  | T.TEUnbound _ -> Error.raiseError "The type could not be infered. Please add a type annotation." t.loc
  | T.TEOption _ -> Error.raiseError "undecided type" t.loc
  | T.TEId { id = "unit"; n = None; _ } -> state, { t = TVoid None; const; loc }
  | T.TEId { id = "int"; n = None; _ } -> state, { t = TInt; const; loc }
  | T.TEId { id = "real"; n = None; _ } -> state, { t = TReal; const; loc }
  | T.TEId { id = "fix16"; n = None; _ } -> state, { t = TFix16; const; loc }
  | T.TEId { id = "string"; n = None; _ } -> state, { t = TString; const; loc }
  | T.TEId { id = "bool"; n = None; _ } -> state, { t = TBool; const; loc }
  | T.TEId p -> (
    let ps = path p in
    match Map.find_opt ps state.types with
    | Some t -> state, { t with const }
    | None -> (
      match Env.getType env p with
      | None -> failwith "unknown type"
      | Some { descr = Enum _; _ } -> state, { t = TInt; const; loc }
      | Some { descr = Record members; _ } ->
        let members =
          List.map (fun (name, (var : Env.var)) -> name, var.t, var.tags, var.loc) (Env.Map.to_list members)
          |> List.sort (fun (n1, _, _, _) (n2, _, _, _) -> String.compare n1 n2)
        in
        let state, members = type_list env state members in
        let t = { t = TStruct { path = ps; members }; loc; const = true } in
        let types = Map.add ps t state.types in
        { state with types }, t
      | Some { descr = Simple; _ } -> failwith "Type does not have members"
      | Some { descr = Alias _; _ } -> failwith ""))
  | T.TELink t -> type_ ~const env state t
  | T.TEComposed ("array", [ t; dim ]) ->
    let state, t = type_ ~const env state t in
    let dim = getDim dim in
    state, { t = TArray (Some dim, t); const; loc }
  | T.TEComposed ("array", [ t ]) ->
    let state, t = type_ ~const env state t in
    state, { t = TArray (None, t); const; loc }
  | T.TEComposed ("tuple", elems) ->
    let state, elems = list (type_ ~const) env state elems in
    state, { t = TTuple elems; const; loc }
  | T.TEComposed (name, _) -> Error.raiseError ("Unknown composed type '" ^ name ^ "'.") t.loc
  | T.TESize _ -> Error.raiseError "Invalid type description." t.loc


and type_list (env : Env.in_top) (state : state) (l : (string * Typed.type_ * Ptags.tags * Loc.t) list) =
  let mapper env state ((n : string), (t : Typed.type_), (tags : Ptags.tags), (loc : Loc.t)) =
    let state, t = type_ env state t in
    state, (n, t, tags, loc)
  in
  list mapper env state l


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


let uoperator op =
  match op with
  | "-" -> UOpNeg
  | "not" -> UOpNot
  | _ -> failwith "unknown uoperator"


let rec exp (env : Env.in_top) (state : state) (e : Typed.exp) : state * Prog.exp =
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
  | EConst id ->
    let id = path id in
    state, { e = EId id; t; loc }
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
  | ERecord { path = p; elems } ->
    let p = path p in
    let state, elems_rev =
      List.fold_left
        (fun (state, acc) (n, v) ->
          let state, v = exp env state v in
          state, (n, v) :: acc)
        (state, [])
        elems
    in
    let sorting =
      match t with
      | { t = TStruct { members; _ }; _ } -> List.mapi (fun i (name, _, _, _) -> name, i) members
      | _ -> failwith "This should be a record"
    in
    let numbered = List.map (fun (n, v) -> snd (List.find (fun (id, _) -> id = n) sorting), (n, v)) elems_rev in
    let sorted = List.sort (fun (i1, _) (i2, _) -> compare i1 i2) numbered |> List.map snd in
    state, { e = ERecord { path = p; elems = sorted }; t; loc }


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


let dexp (env : Env.in_top) (state : state) id dims t loc =
  let state, t = type_ env state t in
  match id, dims with
  | id, None -> state, { d = DId (id, None); t; loc }
  | id, Some dim -> state, { d = DId (id, Some dim); t; loc }


let block (stmts : stmt list) : stmt =
  match stmts with
  | [] -> { s = StmtBlock []; loc = Loc.default }
  | [ s ] -> s
  | _ -> { s = StmtBlock stmts; loc = Loc.default }


let rec flattenTupleDeclarations env state (l : T.dexp list) =
  let state, l =
    List.fold_left
      (fun (state, acc) (d : T.dexp) ->
        match d.d with
        | DWild -> state, acc
        | DTuple elems ->
          let state, inner = flattenTupleDeclarations env state elems in
          state, List.rev inner @ acc
        | DId (id, dims) ->
          let state, d = dexp env state id dims d.t d.loc in
          state, d :: acc)
      (state, [])
      l
  in
  state, List.rev l


let rec stmt (env : Env.in_top) (state : state) (s : Typed.stmt) =
  let loc = s.loc in
  match s.s with
  | StmtVal { d = DWild; _ } -> state, []
  | StmtVal { d = DTuple elems; _ } ->
    let state, dexp_elems = flattenTupleDeclarations env state elems in
    let decls = List.map (fun lhs -> { s = StmtDecl (lhs, None); loc }) dexp_elems in
    state, decls
  | StmtVal { d = DId (id, dims); t; loc } ->
    let state, lhs = dexp env state id dims t loc in
    state, [ { s = StmtDecl (lhs, None); loc } ]
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
  | StmtBlock stmts -> (
    let state, stmts = list stmt env state stmts in
    match List.flatten stmts with
    | [] -> state, [ { s = StmtBlock []; loc } ]
    | [ { s = StmtBlock subs; _ } ] -> state, [ { s = StmtBlock subs; loc } ]
    | [ s ] -> state, [ s ]
    | subs -> state, [ { s = StmtBlock subs; loc } ])


let arg (env : Env.in_top) (state : state) ({ name; t; loc } : Typed.arg) =
  let const = Typed.isTypeConst t = true in
  let state, t = type_ env state t in
  state, { name; t; const; loc }


let function_type env state t =
  match t with
  | args, ret ->
    let state, args = list type_ env state args in
    let state, ret = type_ env state ret in
    state, (args, ret)


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


let ext_function_def (env : Env.in_top) (state : state) (def : Typed.function_def) (linkname : string option) =
  let name = path def.name in
  let state, args = list arg env state def.args in
  let state, t = function_type env state def.t in
  let state, next = next_def env state def.next in
  let original_name = Some (Pla.print (Syntax.print_path def.name)) in
  let info = { original_name; is_root = false } in
  let stmt = { top = TopExternal ({ name; args; t; loc = def.loc; tags = def.tags; info }, linkname); loc = def.loc } in
  state, stmt :: next


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
    let t = { t = TStruct struct_descr; loc = t.loc; const = false } in
    let types = Map.add p t state.types in
    { state with types }, [ { top = TopType struct_descr; loc = t.loc } ]
  | TopEnum _ -> state, []
  | TopAlias { path = p; alias_of } ->
    let p = path p in
    let alias_of = path alias_of in
    state, [ { top = TopAlias { path = p; alias_of }; loc = t.loc } ]
  | TopConstant (p, dim, t, e) ->
    let p = path p in
    let state, e = exp env state e in
    let state, t = type_ env state t in
    state, [ { top = TopConstant (p, dim, t, e); loc = t.loc } ]


let top_stmt_list (env : Env.in_top) (state : state) (t : Typed.top_stmt list) = list top_stmt env state t

let main env stmts =
  let state = { types = Map.empty; dummy = 0 } in
  let _, t = top_stmt_list env state stmts in
  List.flatten t


let isType s =
  match s with
  | { top = TopType _; _ } -> true
  | { top = TopAlias _; _ } -> true
  | _ -> false


let getInitializersFromModule table m =
  List.fold_left (fun s (key, t) -> Map.add (path key) (path t) s) table m.Env.init


let createInitizerTable (env : Env.in_top) =
  Env.Map.fold (fun _ m s -> getInitializersFromModule s m) Map.empty env.modules


let convert (iargs : Args.args) env stmts =
  let stmts = main env stmts in
  let types, functions = List.partition isType stmts in
  let custom_initializers = createInitizerTable env in
  let initializers = CCList.map Initializer.(createInitFunction custom_initializers iargs) types in
  let serializers = Serializer.createSerializers types in
  env, types @ initializers @ serializers @ functions
