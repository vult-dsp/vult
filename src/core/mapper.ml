(*
The MIT License (MIT)

Copyright (c) 2014 Leonardo Laguna Ruiz, Carl Jönsson

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

open TypesVult
open Scope

type binding_info =
   | FunctionInfo
   | TypeInfo
         [@@deriving show,eq,ord]

module BindingInfo =
struct
   type t    = identifier
   type v    = binding_info
   type kind = scope_kind
   let compare     = compare_identifier
   let string_t _  = ""
   let string_v _  = ""
   let lookup_cond = (* Only scapes local scopes *)
      function | Some(LocalScope) -> true | _ -> false
end

module BindingsScope = Scope(BindingInfo)

(** Used to track the scope in all traversers *)
module State = struct
   type 'a t =
      {
         scope   : BindingsScope.t;
         data    : 'a;
         revisit : bool;
         counter : int;
      }

   (** Returns a traversing state *)
   let createState (data:'a) : 'a t =
      { scope = BindingsScope.empty; data = data; revisit = false; counter = 0 }

   (** Sets the data for the traversing state *)
   let setState (s:'a t) (data:'a) : 'a t =
      { s with data = data }

   (** Gets the data for the traversing state *)
   let getState (s:'a t) : 'a =
      s.data

   (** Creates a new state keepin the internal data *)
   let deriveState (s:'a t) (data:'b) : 'b t =
      { scope = s.scope; data = data; revisit = false; counter = 0 }

   (** Adds a name to the scope *)
   let pushScope (s:'a t) (name:identifier) (kind:scope_kind) : 'a t =
      (*Printf.printf " - Entering to scope '%s'\n" (joinSep "." name);*)
      { s with scope = BindingsScope.enter s.scope name (Some(kind)) }

   (** Removes the last name from the scope *)
   let popScope (s:'a t) : 'a t =
      { s with scope = BindingsScope.exit s.scope }

   (** Returns the current scope *)
   let getScope (s:'a t) : identifier =
      BindingsScope.getCurrentPath s.scope |> List.flatten
end

type ('data,'kind) mapper_func = 'data -> 'kind -> 'data * 'kind

(** Makes a chain of mappers. E.g. foo |-> bar will apply first foo then bar. *)
let (|->) : ('data,'value) mapper_func -> ('data,'value) mapper_func -> ('data,'value) mapper_func =
   fun mapper1 mapper2 ->
   fun state exp ->
      let state',exp' = mapper1 state exp in
      mapper2 state' exp'

type 'a mapper =
   {
      type_exp : ('a, type_exp)  mapper_func;
      typed_id : ('a, typed_id)  mapper_func;
      exp      : ('a, exp)       mapper_func;
      lhs_exp  : ('a, lhs_exp)   mapper_func;
      val_decl : ('a, val_decl)  mapper_func;
      stmt     : ('a, stmt)      mapper_func;
      revisit  : 'a -> 'a * bool;
   }

let default_mapper =
   {
      type_exp = (fun state e -> state,e);
      typed_id = (fun state e -> state,e);
      exp      = (fun state e -> state,e);
      lhs_exp  = (fun state e -> state,e);
      val_decl = (fun state e -> state,e);
      stmt     = (fun state e -> state,e);
      revisit  = (fun state -> state,false);
   }
(** Applies any mapper to a list *)
let mapper_list mapper_app =
   fun mapper state el ->
      let state,rev_el =
         List.fold_left
            (fun (s,acc) e ->
                let s',e' = mapper_app mapper s e in
                s',e'::acc)
            (state,[]) el
      in state, (List.rev rev_el)

(** Applies any mapper to an option value *)
let mapper_opt mapper_app =
   fun mapper state e_opt ->
      match e_opt with
      | None    -> state, None
      | Some(e) ->
         let state',e' = mapper_app mapper state e in
         state',Some(e')

let mapper_reapply mapper_app =
   fun mapper (state,exp) ->
      let state',revisit = mapper.revisit state in
      if revisit then
         mapper_app mapper state' exp
      else
         state,exp

let rec map_type_exp (mapper:'a mapper) (state:'a) (te:type_exp) : 'a * type_exp =
   let map_type_exp_list = mapper_list map_type_exp in
   match te with
   | TUnit(_)
   | TWild(_)
   | TId(_,_) -> mapper.type_exp state te
   | TTuple(el,attr) ->
      let state',el' = map_type_exp_list mapper state el in
      mapper.type_exp state' (TTuple(el',attr))
   | TComposed(id,el,attr) ->
      let state',el' = map_type_exp_list mapper state el in
      mapper.type_exp state' (TComposed(id,el',attr))
   | TSignature(el,attr) ->
      let state',el' = map_type_exp_list mapper state el in
      mapper.type_exp state' (TSignature(el',attr))

let rec map_typed_id (mapper:'a mapper) (state:'a) (t:typed_id) : 'a * typed_id =
   let reapply = mapper_reapply map_typed_id in
   match t with
   | SimpleId(_,_) ->
      mapper.typed_id state t
   | TypedId(id,tp,atte) ->
      let state',tp' = map_type_exp mapper state tp in
      mapper.typed_id state' (TypedId(id,tp',atte))
      |> reapply mapper

let rec map_lhs_exp (mapper:'state mapper) (state:'state) (exp:lhs_exp) : 'state * lhs_exp =
   let reapply          = mapper_reapply map_lhs_exp in
   let map_lhs_exp_list = mapper_list map_lhs_exp in
   match exp with
   | LWild(_)
   | LId(_) -> mapper.lhs_exp state exp
   | LTyped(e,tp,attr) ->
      let state',e'  = map_lhs_exp  mapper state e in
      let state',tp' = map_type_exp mapper state' tp in
      mapper.lhs_exp state' (LTyped(e',tp',attr))
      |> reapply mapper
   | LTuple(elems,attr) ->
      let state',elems' = map_lhs_exp_list mapper state elems in
      mapper.lhs_exp state' (LTuple(elems,attr))
      |> reapply mapper

let map_val_decl (mapper:'state mapper) (state:'state) (v:val_decl) : 'state * val_decl =
   let id,tp,attr = v in
   let state',tp' = map_type_exp mapper state tp in
   state',(id,tp',attr)

(** Traverses the expression in a bottom-up fashion *)
let rec map_exp (mapper:'state mapper) (state:'state) (exp:exp) : 'state * exp =
   let reapply = mapper_reapply map_exp in
   match exp with
   | PUnit(_)
   | PBool(_,_)
   | PInt(_,_)
   | PReal(_,_)
   | PId(_,_)
   | PEmpty -> mapper.exp state exp
   | PUnOp(op,e,loc) ->
      let state1,ne = map_exp mapper state e in
      mapper.exp state1 (PUnOp(op,ne,loc))
      |> reapply mapper
   | PBinOp(op,e1,e2,loc) ->
      let state1,ne1 = map_exp mapper state e1 in
      let state2,ne2 = map_exp mapper state1 e2 in
      mapper.exp state2 (PBinOp(op,ne1,ne2,loc))
      |> reapply mapper
   | PCall(inst,name,args,attr) ->
      let state1,nargs = map_exp_list mapper state args in
      mapper.exp state1 (PCall(inst,name,nargs,attr))
      |> reapply mapper
   | PIf(cond,then_,else_,attr) ->
      let state1,ncond  = map_exp mapper state cond in
      let state2,nthen_ = map_exp mapper state1 then_ in
      let state3,nelse_ = map_exp mapper state2 else_ in
      mapper.exp state3 (PIf(ncond,nthen_,nelse_,attr))
      |> reapply mapper
   | PGroup(e,attr) ->
      let state1,ne = map_exp mapper state e in
      mapper.exp state1 (PGroup(ne,attr))
      |> reapply mapper
   | PTuple(el,attr) ->
      let state1,nel = map_exp_list mapper state el in
      mapper.exp state1 (PTuple(nel,attr))
      |> reapply mapper
   | PSeq(id,stmt,attr) ->
      let state',stmt' = map_stmt mapper state stmt in
      mapper.exp state' (PSeq(id,stmt',attr))
      |> reapply mapper

and map_exp_list mapper = fun state exp -> (mapper_list map_exp) mapper state exp

and map_stmt (mapper:'state mapper) (state:'state) (stmt:stmt) : 'state * stmt  =
   let reapply       = mapper_reapply map_stmt in
   match stmt with
   | StmtVal(lhs,rhs,attr) ->
      let state',lhs' = map_lhs_exp mapper state lhs in
      let state',rhs' = (mapper_opt map_exp) mapper state' rhs in
      mapper.stmt state' (StmtVal(lhs',rhs',attr))
      |> reapply mapper
   | StmtMem(lhs,init,rhs,attr) ->
      let state',lhs'   = map_lhs_exp mapper state lhs in
      let state',init'  = (mapper_opt map_exp) mapper state' init in
      let state',rhs'   = (mapper_opt map_exp) mapper state' rhs in
      mapper.stmt state' (StmtMem(lhs',init',rhs',attr))
      |> reapply mapper
   | StmtTable(id,elems,attr) ->
      let state',elems' = (mapper_list map_exp) mapper state elems in
      mapper.stmt state' (StmtTable(id,elems',attr))
      |> reapply mapper
   | StmtReturn(e,attr) ->
      let state',e' = map_exp mapper state e in
      mapper.stmt state' (StmtReturn(e',attr))
      |> reapply mapper
   | StmtBind(lhs,rhs,attr) ->
      let state',lhs' = map_lhs_exp mapper state lhs in
      let state',rhs' = map_exp mapper state' rhs in
      mapper.stmt state' (StmtBind(lhs',rhs',attr))
      |> reapply mapper
   | StmtType(name,args,members,attr) ->
      let state',args'    = (mapper_list map_typed_id) mapper state args in
      let state',members' = (mapper_list map_val_decl) mapper state' members in
      mapper.stmt state' (StmtType(name,args',members',attr))
      |> reapply mapper
   | StmtAliasType(name,args,tp,attr) ->
      let state',args' = (mapper_list map_typed_id) mapper state args in
      let state',tp'   = map_type_exp mapper state' tp in
      mapper.stmt state' (StmtAliasType(name,args',tp',attr))
      |> reapply mapper
   | StmtEmpty ->
      mapper.stmt state StmtEmpty
      |> reapply mapper
   | StmtWhile(cond,stmts,attr) ->
      let state',cond'  = map_exp mapper state cond in
      let state',stmts' = map_stmt mapper state' stmts in
      (state',(StmtWhile(cond',stmts',attr)))
      |> reapply mapper
   | StmtIf(cond,then_,Some(else_),attr) ->
      let state',cond'  = map_exp mapper state cond in
      let state',then_' = map_stmt mapper state' then_ in
      let state',else_' = map_stmt mapper state' else_ in
      (state',(StmtIf(cond',then_',Some(else_'),attr)))
      |> reapply mapper
   | StmtIf(cond,then_,None,attr) ->
      let state',cond'  = map_exp mapper state cond in
      let state',then_' = map_stmt mapper state' then_ in
      (state',(StmtIf(cond',then_',None,attr)))
      |> reapply mapper
   | StmtFun(name,args,body,ret,attr) ->
      let state',args' = (mapper_list map_typed_id) mapper state args in
      let state',body' = map_stmt mapper state' body in
      let state',ret'  = (mapper_opt map_type_exp) mapper state' ret in
      (state',(StmtFun(name,args',body',ret',attr)))
      |> reapply mapper
   | StmtBlock(name,stmts,attr) ->
      let state',stmts' = map_stmt_list mapper state stmts in
      (state',(StmtBlock(name,stmts',attr)))
      |> reapply mapper

and map_stmt_list mapper = fun state stmt -> (mapper_list map_stmt) mapper state stmt






