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

type ('data, 'kind) mapper_func = ('data -> 'kind -> 'data * 'kind) option

type ('data, 'kind) expand_func = ('data -> 'kind -> 'data * 'kind list) option

let apply (mapper : ('data, 'kind) mapper_func) (data : 'data) (kind : 'kind) : 'data * 'kind =
  match mapper with
  | Some f -> f data kind
  | None -> data, kind


let make (mapper : 'data -> 'kind -> 'data * 'kind) : ('data, 'kind) mapper_func = Some mapper

let ( |-> ) : ('data, 'value) mapper_func -> ('data, 'value) mapper_func -> ('data, 'value) mapper_func =
 fun mapper1 mapper2 ->
  let mapper3 state exp =
    let state', exp' = apply mapper1 state exp in
    apply mapper2 state' exp'
  in
  Some mapper3


let list mapper_app mapper state el =
  let state', rev_el =
    List.fold_left
      (fun (s, acc) e ->
        let s', e' = mapper_app mapper s e in
        s', e' :: acc)
      (state, [])
      el
  in
  state', List.rev rev_el


type 'state mapper =
  { type_ : ('state, type_) mapper_func
  ; exp : ('state, exp) mapper_func
  ; lexp : ('state, lexp) mapper_func
  ; dexp : ('state, dexp) mapper_func
  ; fun_type : ('state, fun_type) mapper_func
  ; param : ('state, param) mapper_func
  ; struct_descr : ('state, struct_descr) mapper_func
  ; stmt : ('state, stmt) expand_func
  ; top_stmt : ('state, top_stmt) expand_func
  }

let rec type_ (mapper : 'state mapper) (state : 'state) (t : type_) : 'state * type_ =
  let loc = t.loc in
  match t with
  | { t = TVoid; _ } -> apply mapper.type_ state { t = TVoid; loc }
  | { t = TInt; _ } -> apply mapper.type_ state { t = TInt; loc }
  | { t = TReal; _ } -> apply mapper.type_ state { t = TReal; loc }
  | { t = TString; _ } -> apply mapper.type_ state { t = TString; loc }
  | { t = TBool; _ } -> apply mapper.type_ state { t = TBool; loc }
  | { t = TFixed; _ } -> apply mapper.type_ state { t = TFixed; loc }
  | { t = TArray (dim, t1); _ } ->
      let state, t1 = type_ mapper state t1 in
      apply mapper.type_ state { t = TArray (dim, t1); loc }
  | { t = TTuple elems; _ } ->
      let state, elems = (list type_) mapper state elems in
      apply mapper.type_ state { t = TTuple elems; loc }
  | { t = TStruct s; _ } ->
      let state, s = struct_descr mapper state s in
      apply mapper.type_ state { t = TStruct s; loc }


and struct_descr (mapper : 'state mapper) (state : 'state) (s : struct_descr) : 'state * struct_descr =
  match s with
  | { path; members } ->
      let state, members = (list param) mapper state members in
      apply mapper.struct_descr state { path; members }


and param (mapper : 'state mapper) (state : 'state) (p : param) : 'state * param =
  let name, t, loc = p in
  let state, t = type_ mapper state t in
  apply mapper.param state (name, t, loc)


let rec exp (mapper : 'state mapper) (state : 'state) (e : exp) : 'state * exp =
  let loc = e.loc in
  let state, t = type_ mapper state e.t in
  match e with
  | { e = EUnit; _ } -> apply mapper.exp state { e = EUnit; t; loc }
  | { e = EBool b; _ } -> apply mapper.exp state { e = EBool b; t; loc }
  | { e = EInt n; _ } -> apply mapper.exp state { e = EInt n; t; loc }
  | { e = EReal n; _ } -> apply mapper.exp state { e = EReal n; t; loc }
  | { e = EString n; _ } -> apply mapper.exp state { e = EString n; t; loc }
  | { e = EId n; _ } -> apply mapper.exp state { e = EId n; t; loc }
  | { e = EIndex { e; index }; _ } ->
      let state, e = exp mapper state e in
      let state, index = exp mapper state index in
      apply mapper.exp state { e = EIndex { e; index }; t; loc }
  | { e = EArray elems; _ } ->
      let state, elems = list exp mapper state elems in
      apply mapper.exp state { e = EArray elems; t; loc }
  | { e = ECall { path; args }; _ } ->
      let state, args = list exp mapper state args in
      apply mapper.exp state { e = ECall { path; args }; t; loc }
  | { e = EUnOp (op, e1); _ } ->
      let state, e1 = exp mapper state e1 in
      apply mapper.exp state { e = EUnOp (op, e1); t; loc }
  | { e = EOp (op, e1, e2); _ } ->
      let state, e1 = exp mapper state e1 in
      let state, e2 = exp mapper state e2 in
      apply mapper.exp state { e = EOp (op, e1, e2); t; loc }
  | { e = EIf { cond; then_; else_ }; _ } ->
      let state, cond = exp mapper state cond in
      let state, then_ = exp mapper state then_ in
      let state, else_ = exp mapper state else_ in
      apply mapper.exp state { e = EIf { cond; then_; else_ }; t; loc }
  | { e = ETuple elems; _ } ->
      let state, elems = list exp mapper state elems in
      apply mapper.exp state { e = ETuple elems; t; loc }
  | { e = EMember (e1, n); _ } ->
      let state, e1 = exp mapper state e1 in
      apply mapper.exp state { e = EMember (e1, n); t; loc }


let rec lexp (mapper : 'state mapper) (state : 'state) (e : lexp) : 'state * lexp =
  let loc = e.loc in
  let state, t = type_ mapper state e.t in
  match e with
  | { l = LWild; _ } -> apply mapper.lexp state { l = LWild; t; loc }
  | { l = LId n; _ } -> apply mapper.lexp state { l = LId n; t; loc }
  | { l = LMember (e1, n); _ } ->
      let state, e1 = lexp mapper state e1 in
      apply mapper.lexp state { l = LMember (e1, n); t; loc }
  | { l = LIndex { e; index }; _ } ->
      let state, e = lexp mapper state e in
      let state, index = exp mapper state index in
      apply mapper.lexp state { l = LIndex { e; index }; t; loc }
  | { l = LTuple elems; _ } ->
      let state, elems = list lexp mapper state elems in
      apply mapper.lexp state { l = LTuple elems; t; loc }


let rec dexp (mapper : 'state mapper) (state : 'state) (e : dexp) : 'state * dexp =
  let loc = e.loc in
  let state, t = type_ mapper state e.t in
  match e with
  | { d = DWild; _ } -> apply mapper.dexp state { d = DWild; t; loc }
  | { d = DId (n, dim); _ } -> apply mapper.dexp state { d = DId (n, dim); t; loc }
  | { d = DTuple elems; _ } ->
      let state, elems = list dexp mapper state elems in
      apply mapper.dexp state { d = DTuple elems; t; loc }
