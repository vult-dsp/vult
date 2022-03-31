(*
   The MIT License (MIT)

   Copyright (c) 2021 Leonardo Laguna Ruiz

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

let index_tick = ref 0
let resetTick () = index_tick := 0

let getTick () =
  let n = !index_tick in
  let () = incr index_tick in
  n
;;

let getInitRHS (t : type_) =
  match t with
  | { t = TVoid _; loc } -> { e = EUnit; t; loc }
  | { t = TInt; loc } -> { e = EInt 0; t; loc }
  | { t = TReal; loc } -> { e = EReal 0.0; t; loc }
  | { t = TFixed; loc } -> { e = EReal 0.0; t; loc }
  | { t = TString; loc } -> { e = EString ""; t; loc }
  | { t = TBool; loc } -> { e = EBool false; t; loc }
  | { t = TStruct { path; _ }; loc } -> { e = ECall { path = path ^ "_init"; args = [] }; t; loc }
  | _ -> failwith "Not a simple type"
;;

type cstyle =
  | NewObject
  | RefObject

let rec initStatement (cstyle : cstyle) lhs rhs (t : type_) =
  match t with
  | { t = TVoid _; loc } ->
    let rhs = getInitRHS t in
    { s = StmtBind (lhs, rhs); loc }
  | { t = TInt; loc } ->
    let rhs = getInitRHS t in
    { s = StmtBind (lhs, rhs); loc }
  | { t = TReal; loc } ->
    let rhs = getInitRHS t in
    { s = StmtBind (lhs, rhs); loc }
  | { t = TFixed; loc } ->
    let rhs = getInitRHS t in
    { s = StmtBind (lhs, rhs); loc }
  | { t = TString; loc } ->
    let rhs = getInitRHS t in
    { s = StmtBind (lhs, rhs); loc }
  | { t = TBool; loc } ->
    let rhs = getInitRHS t in
    { s = StmtBind (lhs, rhs); loc }
  | { t = TTuple _; _ } -> failwith "tuples"
  | { t = TStruct { path; _ }; loc } when cstyle = RefObject ->
    let rhs = { e = ECall { path = path ^ "_init"; args = [ rhs ] }; t; loc } in
    { s = StmtBind ({ l = LWild; loc; t = { t = TVoid None; loc = Loc.default } }, rhs); loc }
  | { t = TStruct { path; _ }; loc } ->
    let rhs = { e = ECall { path = path ^ "_init"; args = [] }; t; loc } in
    { s = StmtBind (lhs, rhs); loc }
  | { t = TArray (size, subt); loc } when cstyle = RefObject ->
    let i = "i_" ^ string_of_int (getTick ()) in
    let int_t = { t = TInt; loc } in
    let index = { e = EId i; t = int_t; loc } in
    let one = { e = EInt 1; t = int_t; loc } in
    let cond = { e = EOp (OpLt, index, { e = EInt size; t = int_t; loc }); t; loc } in
    let bind =
      let lhs = { l = LIndex { e = lhs; index }; t = subt; loc } in
      let rhs = { e = EIndex { e = rhs; index }; t = subt; loc } in
      initStatement cstyle lhs rhs subt
    in
    let plus_one = { e = EOp (OpAdd, index, one); t = int_t; loc } in
    let incr = { s = StmtBind ({ l = LId i; t = int_t; loc }, plus_one); loc } in
    let body = { s = StmtBlock [ bind; incr ]; loc } in
    let loop = { s = StmtWhile (cond, body); loc } in
    let decl = { s = StmtDecl { d = DId (i, None); t = int_t; loc }; loc } in
    let init = { s = StmtBind ({ l = LId i; t = int_t; loc }, { e = EInt 0; t = int_t; loc }); loc } in
    { s = StmtBlock [ decl; init; loop ]; loc }
  | { t = TArray (size, subt); loc } ->
    let i = "i_" ^ string_of_int (getTick ()) in
    let int_t = { t = TInt; loc } in
    let index = { e = EId i; t = int_t; loc } in
    let one = { e = EInt 1; t = int_t; loc } in
    let cond = { e = EOp (OpLt, index, { e = EInt size; t = int_t; loc }); t; loc } in
    let rhs_temp = { e = EId "temp"; t; loc } in
    let lhs_temp = { l = LId "temp"; t; loc } in
    let bind =
      let lhs = { l = LIndex { e = lhs_temp; index }; t = subt; loc } in
      let rhs = { e = EIndex { e = rhs_temp; index }; t = subt; loc } in
      initStatement cstyle lhs rhs subt
    in
    let plus_one = { e = EOp (OpAdd, index, one); t = int_t; loc } in
    let incr = { s = StmtBind ({ l = LId i; t = int_t; loc }, plus_one); loc } in
    let body = { s = StmtBlock [ bind; incr ]; loc } in
    let loop = { s = StmtWhile (cond, body); loc } in
    let decl = { s = StmtDecl { d = DId (i, None); t = int_t; loc }; loc } in
    let decl_array = { s = StmtDecl { d = DId ("temp", None); t; loc }; loc } in
    let init = { s = StmtBind ({ l = LId i; t = int_t; loc }, { e = EInt 0; t = int_t; loc }); loc } in
    let transfer = { s = StmtBind (lhs, rhs_temp); loc } in
    { s = StmtBlock [ decl_array; decl; init; loop; transfer ]; loc }
;;

let createInitFunction (cstyle : cstyle) stmt =
  let () = resetTick () in
  match stmt with
  (* generation for c-style code using pointers *)
  | { top = TopType struct_t; loc } when cstyle = RefObject ->
    let name = struct_t.path ^ "_init" in
    let this_type = { t = TStruct struct_t; loc = Loc.default } in
    let void_type = { t = TVoid None; loc = Loc.default } in
    let lctx = { l = LId "_ctx"; t = this_type; loc } in
    let ectx = { e = EId "_ctx"; t = this_type; loc } in
    let stmts =
      List.map
        (fun (var, (t : type_), _) ->
          let lhs = { l = LMember (lctx, var); t; loc = t.loc } in
          let rhs = { e = EMember (ectx, var); t; loc = t.loc } in
          initStatement cstyle lhs rhs t)
        struct_t.members
    in
    let body = { s = StmtBlock stmts; loc } in
    let args, t = [ "_ctx", this_type, loc ], ([ this_type ], void_type) in
    { top = TopFunction ({ name; args; t; loc; tags = []; info = default_info }, body); loc }
  | { top = TopType struct_t; loc } ->
    let name = struct_t.path ^ "_init" in
    let this_type = { t = TStruct struct_t; loc = Loc.default } in
    let lctx = { l = LId "_ctx"; t = this_type; loc } in
    let ectx = { e = EId "_ctx"; t = this_type; loc } in
    let stmts =
      List.map
        (fun (var, (t : type_), _) ->
          let lhs = { l = LMember (lctx, var); t; loc = t.loc } in
          let rhs = { e = EMember (ectx, var); t; loc = t.loc } in
          initStatement cstyle lhs rhs t)
        struct_t.members
    in
    let new_ctx = { s = StmtDecl { d = DId ("_ctx", None); t = this_type; loc }; loc } in
    let return = { s = StmtReturn ectx; loc } in
    let body = { s = StmtBlock ((new_ctx :: stmts) @ [ return ]); loc } in
    let args, t = [], ([], this_type) in
    { top = TopFunction ({ name; args; t; loc; tags = []; info = default_info }, body); loc }
  | _ -> failwith "not a type"
;;
