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


let rec getInitRHS (t : type_) =
  match t with
  | { t = TVoid _; _ } -> C.eunit
  | { t = TInt; loc; _ } -> C.eint ~loc 0
  | { t = TReal; loc; _ } -> C.ereal ~loc 0.0
  | { t = TFix16; loc; _ } -> C.efix16 ~loc 0.0
  | { t = TString; loc; _ } -> C.estring ~loc ""
  | { t = TBool; loc; _ } -> C.ebool ~loc false
  | { t = TStruct { path; _ }; loc; _ } -> C.ecall ~loc (path ^ "_init") [] t
  | { t = TArray (Some size, at); loc; _ } ->
    let v = getInitRHS at in
    let elems = List.init size (fun _ -> v) in
    C.earray ~loc elems t
  | _ -> failwith "Not a simple type"


type cstyle =
  | NewObject
  | RefObject

let rec initStatement (cstyle : cstyle) lhs rhs (t : type_) =
  match t with
  | { t = TVoid _; loc; _ } ->
    let rhs = getInitRHS t in
    { s = StmtBind (lhs, rhs); loc }
  | { t = TInt; loc; _ } ->
    let rhs = getInitRHS t in
    { s = StmtBind (lhs, rhs); loc }
  | { t = TReal; loc; _ } ->
    let rhs = getInitRHS t in
    { s = StmtBind (lhs, rhs); loc }
  | { t = TFix16; loc; _ } ->
    let rhs = getInitRHS t in
    { s = StmtBind (lhs, rhs); loc }
  | { t = TString; loc; _ } ->
    let rhs = getInitRHS t in
    { s = StmtBind (lhs, rhs); loc }
  | { t = TBool; loc; _ } ->
    let rhs = getInitRHS t in
    { s = StmtBind (lhs, rhs); loc }
  | { t = TTuple _; _ } -> failwith "tuples"
  | { t = TStruct { path; _ }; loc; _ } when cstyle = RefObject ->
    let rhs = { e = ECall { path = path ^ "_init"; args = [ rhs ] }; t; loc } in
    { s = StmtBind ({ l = LWild; loc; t = C.void_t }, rhs); loc }
  | { t = TStruct { path; _ }; loc; _ } ->
    let rhs = { e = ECall { path = path ^ "_alloc"; args = [] }; t; loc } in
    { s = StmtBind (lhs, rhs); loc }
  | { t = TArray (Some size, subt); loc; _ } when cstyle = RefObject ->
    let i = "i_" ^ string_of_int (getTick ()) in
    let int_t = C.int_t in
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
    let decl = { s = StmtDecl ({ d = DId (i, None); t = int_t; loc }, None); loc } in
    let init = { s = StmtBind ({ l = LId i; t = int_t; loc }, { e = EInt 0; t = int_t; loc }); loc } in
    { s = StmtBlock [ decl; init; loop ]; loc }
  | { t = TArray (Some size, subt); loc; _ } ->
    let i = "i_" ^ string_of_int (getTick ()) in
    let int_t = C.int_t in
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
    let decl = { s = StmtDecl ({ d = DId (i, None); t = int_t; loc }, None); loc } in
    let decl_array = { s = StmtDecl ({ d = DId ("temp", None); t; loc }, None); loc } in
    let init = { s = StmtBind ({ l = LId i; t = int_t; loc }, { e = EInt 0; t = int_t; loc }); loc } in
    let transfer = { s = StmtBind (lhs, rhs_temp); loc } in
    { s = StmtBlock [ decl_array; decl; init; loop; transfer ]; loc }
  | { t = TArray (None, _); _ } -> failwith "initStatement: Array without size"


let customInitializerCall (custom_initializers : string Util.Maps.Map.t) name ectx void_type loc =
  match Util.Maps.Map.find_opt name custom_initializers with
  | None -> []
  | Some path ->
    [ { s = StmtBind ({ l = LWild; t = void_type; loc }, { e = ECall { path; args = [ ectx ] }; t = void_type; loc })
      ; loc
      }
    ]


let initializerType (iargs : Args.args) =
  match iargs.code with
  | NoCode -> NewObject
  | CppCode -> RefObject
  | JSCode -> NewObject
  | LuaCode -> NewObject
  | WLCode -> NewObject
  | JavaCode -> NewObject


let createInitFunction custom_initializers (iargs : Args.args) stmt =
  let () = resetTick () in
  let cstyle = initializerType iargs in
  match stmt with
  (* Generation for c-style code using references *)
  | { top = TopType struct_t; loc } when cstyle = RefObject ->
    let name = struct_t.path ^ "_init" in
    let this_type = { t = TStruct struct_t; loc = Loc.default; const = false } in
    let void_type = C.void_t in
    let lctx = { l = LId "_ctx"; t = this_type; loc } in
    let ectx = { e = EId "_ctx"; t = this_type; loc } in
    let stmts =
      List.map
        (fun (var, (t : type_), _, _) ->
          let lhs = { l = LMember (lctx, var); t; loc = t.loc } in
          let rhs = { e = EMember (ectx, var); t; loc = t.loc } in
          initStatement cstyle lhs rhs t)
        struct_t.members
    in
    let custom_initializer = customInitializerCall custom_initializers struct_t.path ectx void_type loc in
    let body = { s = StmtBlock (stmts @ custom_initializer); loc } in
    let args, t = [ { name = "_ctx"; t = this_type; const = false; loc } ], ([ this_type ], void_type) in
    { top = TopFunction ({ name; args; t; loc; tags = []; info = default_info }, body); loc }
  (* Initialization of alias c-style *)
  | { top = TopAlias { path; alias_of }; loc } when cstyle = RefObject ->
    let name = path ^ "_init" in
    let this_type = { t = TStruct { path; members = [] }; loc = Loc.default; const = false } in
    let void_type = C.void_t in
    let call =
      { e = ECall { path = alias_of ^ "_init"; args = [ { e = EId "_ctx"; t = this_type; loc } ] }; loc; t = void_type }
    in
    let bind = { s = StmtBind ({ l = LWild; loc; t = void_type }, call); loc } in
    let body = { s = StmtBlock [ bind ]; loc } in
    let args, t = [ { name = "_ctx"; t = this_type; const = false; loc } ], ([ this_type ], void_type) in
    { top = TopFunction ({ name; args; t; loc; tags = []; info = default_info }, body); loc }
  (* Generate initializers that return a value *)
  | { top = TopType struct_t; loc } ->
    let name = struct_t.path ^ "_alloc" in
    let this_type = { t = TStruct struct_t; loc = Loc.default; const = true } in
    let void_type = C.void_t in
    let lctx = { l = LId "_ctx"; t = this_type; loc } in
    let ectx = { e = EId "_ctx"; t = this_type; loc } in
    let stmts =
      List.map
        (fun (var, (t : type_), _, _) ->
          let lhs = { l = LMember (lctx, var); t; loc = t.loc } in
          let rhs = { e = EMember (ectx, var); t; loc = t.loc } in
          initStatement cstyle lhs rhs t)
        struct_t.members
    in
    let custom_initializer = customInitializerCall custom_initializers struct_t.path ectx void_type loc in
    let new_ctx = { s = StmtDecl ({ d = DId ("_ctx", None); t = this_type; loc }, None); loc } in
    let return = { s = StmtReturn ectx; loc } in
    let body = { s = StmtBlock ((new_ctx :: stmts) @ custom_initializer @ [ return ]); loc } in
    let args, t = [], ([], this_type) in
    { top = TopFunction ({ name; args; t; loc; tags = []; info = default_info }, body); loc }
  | { top = TopAlias { path; alias_of }; loc } ->
    let name = path ^ "_alloc" in
    let this_type = { t = TStruct { path; members = [] }; loc = Loc.default; const = true } in
    let void_type = C.void_t in
    let call = { e = ECall { path = alias_of ^ "_alloc"; args = [] }; loc; t = void_type } in
    let body = { s = StmtReturn call; loc } in
    let args, t = [ { name = "_ctx"; t = this_type; const = false; loc } ], ([], this_type) in
    { top = TopFunction ({ name; args; t; loc; tags = []; info = default_info }, body); loc }
  | _ ->
    print_endline (Pla.print (Prog.Print.print_top_stmt stmt));
    failwith "not a type"
