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

(* S-expression printer for Syntax AST *)

open Util
open Syntax

let escape_string (s : string) : string =
  let buf = Buffer.create (String.length s) in
  String.iter
    (fun c ->
      match c with
      | '"' -> Buffer.add_string buf "\\\""
      | '\\' -> Buffer.add_string buf "\\\\"
      | '\n' -> Buffer.add_string buf "\\n"
      | '\r' -> Buffer.add_string buf "\\r"
      | '\t' -> Buffer.add_string buf "\\t"
      | c -> Buffer.add_char buf c)
    s;
  Buffer.contents buf


let tag_name (t : Ptags.tag) : string =
  match t.g with
  | Ptags.TagId name -> name
  | Ptags.TagCall { name; _ } -> name
  | Ptags.TagInt i -> string_of_int i
  | Ptags.TagBool b -> string_of_bool b
  | Ptags.TagReal f -> string_of_float f
  | Ptags.TagString s -> s


let rec path (p : path) : string =
  match p.n with
  | None -> Printf.sprintf "(path %s)" p.id
  | Some n -> Printf.sprintf "(path %s %s)" p.id n


and type_ (t : type_) : string = Printf.sprintf "(type %s %s)" (type_d t.t) (Loc.to_string t.loc)

and type_d (t : type_d) : string =
  match t with
  | STUnbound -> "unbound"
  | STId p -> Printf.sprintf "(id %s)" (path p)
  | STGenericType id -> Printf.sprintf "(generic_type %s)" id
  | STSize n -> Printf.sprintf "(size %d)" n
  | STComposed (name, types) ->
    let types_str = String.concat " " (List.map type_ types) in
    Printf.sprintf "(composed %s %s)" name types_str


and exp (e : exp) : string = Printf.sprintf "(exp %s %s)" (exp_d e.e) (Loc.to_string e.loc)

and exp_d (e : exp_d) : string =
  match e with
  | SEBool b -> Printf.sprintf "(bool %b)" b
  | SEInt i -> Printf.sprintf "(int %s)" i
  | SEReal r -> Printf.sprintf "(real %s)" r
  | SEFixed f -> Printf.sprintf "(fixed %s)" f
  | SEString s -> Printf.sprintf "(string \"%s\")" (escape_string s)
  | SEId id -> Printf.sprintf "(id %s)" id
  | SEIndex { e; index } -> Printf.sprintf "(index %s %s)" (exp e) (exp index)
  | SEArray exprs ->
    let exprs_str = String.concat " " (List.map exp exprs) in
    Printf.sprintf "(array %s)" exprs_str
  | SECall { instance; path = p; args } ->
    let instance_str =
      match instance with
      | None -> "none"
      | Some (name, None) -> Printf.sprintf "(instance %s)" name
      | Some (name, Some e) -> Printf.sprintf "(instance %s %s)" name (exp e)
    in
    let args_str = String.concat " " (List.map exp args) in
    Printf.sprintf "(call %s %s %s)" instance_str (path p) args_str
  | SEUnOp (op, e) -> Printf.sprintf "(unop %s %s)" op (exp e)
  | SEOp (op, e1, e2) -> Printf.sprintf "(op %s %s %s)" op (exp e1) (exp e2)
  | SEIf { cond; then_; else_ } -> Printf.sprintf "(if %s %s %s)" (exp cond) (exp then_) (exp else_)
  | SETuple exprs ->
    let exprs_str = String.concat " " (List.map exp exprs) in
    Printf.sprintf "(tuple %s)" exprs_str
  | SEMember (e, m) -> Printf.sprintf "(member %s %s)" (exp e) m
  | SEGroup e -> Printf.sprintf "(group %s)" (exp e)
  | SERecord { path = p; elems } ->
    let elems_str = String.concat " " (List.map (fun (p, e) -> Printf.sprintf "(%s %s)" (path p) (exp e)) elems) in
    Printf.sprintf "(record %s %s)" (path p) elems_str
  | SENamed (e1, e2) -> Printf.sprintf "(named %s %s)" (exp e1) (exp e2)


and pattern (p : pattern) : string = Printf.sprintf "(pattern %s %s)" (pattern_d p.p) (Loc.to_string p.loc)

and pattern_d (p : pattern_d) : string =
  match p with
  | SPWild -> "wild"
  | SPBool b -> Printf.sprintf "(bool %b)" b
  | SPInt i -> Printf.sprintf "(int %s)" i
  | SPReal r -> Printf.sprintf "(real %s)" r
  | SPFixed f -> Printf.sprintf "(fixed %s)" f
  | SPString s -> Printf.sprintf "(string \"%s\")" (escape_string s)
  | SPId id -> Printf.sprintf "(id %s)" id
  | SPTuple patterns ->
    let patterns_str = String.concat " " (List.map pattern patterns) in
    Printf.sprintf "(tuple %s)" patterns_str
  | SPGroup p -> Printf.sprintf "(group %s)" (pattern p)
  | SPMember (p, m) -> Printf.sprintf "(member %s %s)" (pattern p) m


and lexp (l : lexp) : string = Printf.sprintf "(lexp %s %s)" (lexp_d l.l) (Loc.to_string l.loc)

and lexp_d (l : lexp_d) : string =
  match l with
  | SLWild -> "wild"
  | SLId id -> Printf.sprintf "(id %s)" id
  | SLMember (e, m) -> Printf.sprintf "(member %s %s)" (lexp e) m
  | SLIndex { e; index } -> Printf.sprintf "(index %s %s)" (lexp e) (exp index)
  | SLGroup e -> Printf.sprintf "(group %s)" (lexp e)
  | SLTuple lexps ->
    let lexps_str = String.concat " " (List.map lexp lexps) in
    Printf.sprintf "(tuple %s)" lexps_str


and dexp (d : dexp) : string = Printf.sprintf "(dexp %s %s)" (dexp_d d.d) (Loc.to_string d.loc)

and dexp_d (d : dexp_d) : string =
  match d with
  | SDWild -> "wild"
  | SDId (id, None) -> Printf.sprintf "(id %s)" id
  | SDId (id, Some n) -> Printf.sprintf "(id %s %d)" id n
  | SDTuple dexps ->
    let dexps_str = String.concat " " (List.map dexp dexps) in
    Printf.sprintf "(tuple %s)" dexps_str
  | SDGroup e -> Printf.sprintf "(group %s)" (dexp e)
  | SDTyped (e, t) -> Printf.sprintf "(typed %s %s)" (dexp e) (type_ t)


and stmt (s : stmt) : string = Printf.sprintf "(stmt %s %s)" (stmt_d s.s) (Loc.to_string s.loc)

and stmt_d (s : stmt_d) : string =
  match s with
  | SStmtError -> "error"
  | SStmtVal (d, None) -> Printf.sprintf "(val %s)" (dexp d)
  | SStmtVal (d, Some e) -> Printf.sprintf "(val %s %s)" (dexp d) (exp e)
  | SStmtMem (d, None, tags) ->
    let tags_str = String.concat " " (List.map (fun t -> Printf.sprintf "\"%s\"" (tag_name t)) tags) in
    Printf.sprintf "(mem %s (%s))" (dexp d) tags_str
  | SStmtMem (d, Some e, tags) ->
    let tags_str = String.concat " " (List.map (fun t -> Printf.sprintf "\"%s\"" (tag_name t)) tags) in
    Printf.sprintf "(mem %s %s (%s))" (dexp d) (exp e) tags_str
  | SStmtBind (l, e) -> Printf.sprintf "(bind %s %s)" (lexp l) (exp e)
  | SStmtReturn e -> Printf.sprintf "(return %s)" (exp e)
  | SStmtIf (cond, then_, None) -> Printf.sprintf "(if %s %s)" (exp cond) (stmt then_)
  | SStmtIf (cond, then_, Some else_) -> Printf.sprintf "(if %s %s %s)" (exp cond) (stmt then_) (stmt else_)
  | SStmtWhile (cond, body) -> Printf.sprintf "(while %s %s)" (exp cond) (stmt body)
  | SStmtIter { id = name, _; value; body } -> Printf.sprintf "(iter %s %s %s)" name (exp value) (stmt body)
  | SStmtMatch { e = e_; cases } ->
    let cases_str = String.concat " " (List.map (fun (p, s) -> Printf.sprintf "(%s %s)" (pattern p) (stmt s)) cases) in
    Printf.sprintf "(match %s %s)" (exp e_) cases_str
  | SStmtBlock stmts ->
    let stmts_str = String.concat " " (List.map stmt stmts) in
    Printf.sprintf "(block %s)" stmts_str


and generic_param (p : generic_param) : string =
  match p with
  | GParamFunction (name, None) -> Printf.sprintf "(gparam_func %s)" name
  | GParamFunction (name, Some t) -> Printf.sprintf "(gparam_func %s %s)" name (type_ t)
  | GParamType name -> Printf.sprintf "(gparam_type %s)" name
  | GParamConstant (name, t) -> Printf.sprintf "(gparam_const %s %s)" name (type_ t)


and function_def (f : function_def) : string =
  let generic_params_str =
    match f.generic_params with
    | [] -> ""
    | params -> Printf.sprintf " (generic_params %s)" (String.concat " " (List.map generic_param params))
  in
  let args_str =
    String.concat
      " "
      (List.map
         (fun (n, t, _) ->
           Printf.sprintf
             "(%s %s)"
             n
             (match t with
             | None -> "untyped"
             | Some t -> type_ t))
         f.args)
  in
  let ret_type_str =
    match f.t with
    | None -> "untyped"
    | Some t -> type_ t
  in
  let tags_str = String.concat " " (List.map (fun t -> Printf.sprintf "\"%s\"" (tag_name t)) f.tags) in
  Printf.sprintf
    "(fun %s%s (args %s) (ret %s) (tags %s) %s)"
    f.name
    generic_params_str
    args_str
    ret_type_str
    tags_str
    (stmt f.body)


and ext_def (e : ext_def) : string =
  let args_str =
    String.concat
      " "
      (List.map
         (fun (n, t, _) ->
           Printf.sprintf
             "(%s %s)"
             n
             (match t with
             | None -> "untyped"
             | Some t -> type_ t))
         e.args)
  in
  let ret_type_str =
    match e.t with
    | None -> "untyped"
    | Some t -> type_ t
  in
  let tags_str = String.concat " " (List.map (fun t -> Printf.sprintf "\"%s\"" (tag_name t)) e.tags) in
  Printf.sprintf "(extern %s (args %s) (ret %s) (tags %s))" e.name args_str ret_type_str tags_str


and top_stmt (s : top_stmt) : string = Printf.sprintf "(top %s %s)" (top_stmt_d s.top) (Loc.to_string s.loc)

and top_stmt_d (s : top_stmt_d) : string =
  match s with
  | STopError -> "error"
  | STopFunction f ->
    let next_str =
      match f.next with
      | None -> ""
      | Some next -> Printf.sprintf " (next %s)" (function_def next)
    in
    Printf.sprintf "(function %s%s)" (function_def f) next_str
  | STopExternal (e, link_name) ->
    let link_str =
      match link_name with
      | None -> "none"
      | Some name -> Printf.sprintf "\"%s\"" name
    in
    Printf.sprintf "(external %s %s)" (ext_def e) link_str
  | STopType { name; members } ->
    let members_str =
      String.concat
        " "
        (List.map
           (fun (n, t, tags, _) ->
             let tags_str = String.concat " " (List.map (fun t -> Printf.sprintf "\"%s\"" (tag_name t)) tags) in
             Printf.sprintf "(%s %s (%s))" n (type_ t) tags_str)
           members)
    in
    Printf.sprintf "(type %s %s)" name members_str
  | STopEnum { name; members } ->
    let members_str = String.concat " " (List.map (fun (n, _) -> n) members) in
    Printf.sprintf "(enum %s %s)" name members_str
  | STopConstant (d, e) -> Printf.sprintf "(constant %s %s)" (dexp d) (exp e)


let parsed_file (f : Parse.parsed_file) : string =
  let stmts_str = String.concat "\n" (List.map top_stmt f.stmts) in
  Printf.sprintf "(file %s\n%s)" f.name stmts_str
