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
open Env

type ('data,'kind) mapper_func = 'data -> 'kind -> 'data * 'kind

(** Makes a chain of mappers. E.g. foo |-> bar will apply first foo then bar. *)
let (|->) : ('data,'value) mapper_func -> ('data,'value) mapper_func -> ('data,'value) mapper_func =
   fun mapper1 mapper2 ->
   fun state exp ->
      let state',exp' = mapper1 state exp in
      mapper2 state' exp'

type 'a mapper =
   {
      type_exp : ('a, type_exp)   mapper_func;
      typed_id : ('a, typed_id)   mapper_func;
      exp      : ('a, exp)        mapper_func;
      lhs_exp  : ('a, lhs_exp)    mapper_func;
      val_decl : ('a, val_decl)   mapper_func;
      stmt     : ('a, stmt)       mapper_func;
      attr     : ('a, attr)       mapper_func;
      id       : ('a, id)         mapper_func;
   }

let default_mapper =
   {
      type_exp = (fun state e -> state,e);
      typed_id = (fun state e -> state,e);
      exp      = (fun state e -> state,e);
      lhs_exp  = (fun state e -> state,e);
      val_decl = (fun state e -> state,e);
      stmt     = (fun state e -> state,e);
      attr     = (fun state e -> state,e);
      id       = (fun state e -> state,e);
   }

(** Merge two mapper functions. This is a little bit weird but it will be
    improved when mapper functions are optional. *)
let seqMapperFunc default a b =
   if a == default then b else
   if b == default then a
   else a |-> b

(** Merges two mappers *)
let seq (a:'a mapper) (b:'a mapper) : 'a mapper =
   {
      type_exp = seqMapperFunc default_mapper.type_exp   a.type_exp b.type_exp;
      typed_id = seqMapperFunc default_mapper.typed_id   a.typed_id b.typed_id;
      exp      = seqMapperFunc default_mapper.exp        a.exp      b.exp;
      lhs_exp  = seqMapperFunc default_mapper.lhs_exp    a.lhs_exp  b.lhs_exp;
      val_decl = seqMapperFunc default_mapper.val_decl   a.val_decl b.val_decl;
      stmt     = seqMapperFunc default_mapper.stmt       a.stmt     b.stmt;
      attr     = seqMapperFunc default_mapper.attr       a.attr     b.attr;
      id       = seqMapperFunc default_mapper.id         a.id       b.id;
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

let map_id (mapper:'a mapper) (state:'a) (id:id) : 'a * id =
   mapper.id state id

let map_attr (mapper:'a mapper) (state:'a) (attr:attr) : 'a * attr =
   mapper.attr state attr

let rec map_type_exp (mapper:'a mapper) (state:'a) (te:type_exp) : 'a * type_exp =
   let map_type_exp_list = mapper_list map_type_exp in
   match te with
   | TUnit(attr) ->
      let state',attr' = map_attr mapper state attr in
      state',TUnit(attr')
   | TWild(attr) ->
      let state',attr' = map_attr mapper state attr in
      state',TWild(attr')
   | TId(id,attr) ->
      let state',id'   = map_id mapper state id in
      let state',attr' = map_attr mapper state' attr in
      mapper.type_exp state (TId(id',attr'))
   | TTuple(el,attr) ->
      let state',el'   = map_type_exp_list mapper state el in
      let state',attr' = map_attr mapper state' attr in
      mapper.type_exp state' (TTuple(el',attr'))
   | TComposed(id,el,attr) ->
      let state',id'   = map_id mapper state id in
      let state',el'   = map_type_exp_list mapper state' el in
      let state',attr' = map_attr mapper state' attr in
      mapper.type_exp state' (TComposed(id',el',attr'))
   | TSignature(el,attr) ->
      let state',el'   = map_type_exp_list mapper state el in
      let state',attr' = map_attr mapper state' attr in
      mapper.type_exp state' (TSignature(el',attr'))

let rec map_typed_id (mapper:'a mapper) (state:'a) (t:typed_id) : 'a * typed_id =
   match t with
   | SimpleId(id,attr) ->
      let state',id'   = map_id mapper state id in
      let state',attr' = map_attr mapper state' attr in
      mapper.typed_id state (SimpleId(id',attr'))
   | TypedId(id,tp,attr) ->
      let state',id'   = map_id mapper state id in
      let state',tp'   = map_type_exp mapper state' tp in
      let state',attr' = map_attr mapper state' attr in
      mapper.typed_id state' (TypedId(id',tp',attr'))

let rec map_lhs_exp (mapper:'state mapper) (state:'state) (exp:lhs_exp) : 'state * lhs_exp =
   let map_lhs_exp_list = mapper_list map_lhs_exp in
   match exp with
   | LWild(attr) ->
      let state',attr' = map_attr mapper state attr in
      mapper.lhs_exp state' (LWild(attr'))
   | LId(id,attr) ->
      let state',id'   = map_id mapper state id in
      let state',attr' = map_attr mapper state' attr in
      mapper.lhs_exp state' (LId(id',attr'))
   | LTyped(e,tp,attr) ->
      let state',e'    = map_lhs_exp  mapper state e in
      let state',tp'   = map_type_exp mapper state' tp in
      let state',attr' = map_attr mapper state' attr in
      mapper.lhs_exp state' (LTyped(e',tp',attr'))
   | LTuple(elems,attr) ->
      let state',elems' = map_lhs_exp_list mapper state elems in
      let state',attr'  = map_attr mapper state' attr in
      mapper.lhs_exp state' (LTuple(elems',attr'))

let map_val_decl (mapper:'state mapper) (state:'state) (v:val_decl) : 'state * val_decl =
   let id,tp,attr   = v in
   let state',id'   = map_id mapper state id in
   let state',tp'   = map_type_exp mapper state' tp in
   let state',attr' = map_attr mapper state' attr in
   state',(id',tp',attr')

(** Traverses the expression in a bottom-up fashion *)
let rec map_exp (mapper:'state mapper) (state:'state) (exp:exp) : 'state * exp =
   match exp with
   | PUnit(attr) ->
      let state',attr' = map_attr mapper state attr in
      mapper.exp state' (PUnit(attr'))
   | PBool(b,attr) ->
      let state',attr' = map_attr mapper state attr in
      mapper.exp state' (PBool(b,attr'))
   | PInt(i,attr) ->
      let state',attr' = map_attr mapper state attr in
      mapper.exp state' (PInt(i,attr'))
   | PReal(r,attr) ->
      let state',attr' = map_attr mapper state attr in
      mapper.exp state' (PReal(r,attr'))
   | PId(id,attr) ->
      let state',id'   = map_id mapper state id in
      let state',attr' = map_attr mapper state' attr in
      mapper.exp state' (PId(id',attr'))
   | PEmpty -> mapper.exp state exp
   | PUnOp(op,e,attr) ->
      let state',e' = map_exp mapper state e in
      let state',attr' = map_attr mapper state' attr in
      mapper.exp state' (PUnOp(op,e',attr'))
   | PBinOp(op,e1,e2,attr) ->
      let state',e1'   = map_exp mapper state e1 in
      let state',e2'   = map_exp mapper state' e2 in
      let state',attr' = map_attr mapper state' attr in
      mapper.exp state' (PBinOp(op,e1',e2',attr'))
   | PCall(inst,name,args,attr) ->
      let state',inst' = (mapper_opt map_id) mapper state inst in
      let state',name' = map_id mapper state' name in
      let state',args' = map_exp_list mapper state' args in
      let state',attr' = map_attr mapper state' attr in
      mapper.exp state' (PCall(inst',name',args',attr'))
   | PIf(cond,then_,else_,attr) ->
      let state',cond'  = map_exp mapper state cond in
      let state',then_' = map_exp mapper state' then_ in
      let state',else_' = map_exp mapper state' else_ in
      let state',attr'  = map_attr mapper state' attr in
      mapper.exp state' (PIf(cond',then_',else_',attr'))
   | PGroup(e,attr) ->
      let state',e'     = map_exp mapper state e in
      let state',attr'  = map_attr mapper state' attr in
      mapper.exp state' (PGroup(e',attr'))
   | PTuple(el,attr) ->
      let state',el'    = map_exp_list mapper state el in
      let state',attr'  = map_attr mapper state' attr in
      mapper.exp state' (PTuple(el',attr'))
   | PSeq(id,stmt,attr) ->
      let state',id'   = (mapper_opt map_id) mapper state id in
      let state',stmt' = map_stmt mapper state' stmt in
      let state',attr' = map_attr mapper state' attr in
      mapper.exp state' (PSeq(id',stmt',attr'))

and map_exp_list mapper = fun state exp -> (mapper_list map_exp) mapper state exp

and map_stmt (mapper:'state mapper) (state:'state) (stmt:stmt) : 'state * stmt =
   match stmt with
   | StmtVal(lhs,rhs,attr) ->
      let state',lhs'  = map_lhs_exp mapper state lhs in
      let state',rhs'  = (mapper_opt map_exp) mapper state' rhs in
      let state',attr' = map_attr mapper state' attr in
      mapper.stmt state' (StmtVal(lhs',rhs',attr'))
   | StmtMem(lhs,init,rhs,attr) ->
      let state',lhs'  = map_lhs_exp mapper state lhs in
      let state',init' = (mapper_opt map_exp) mapper state' init in
      let state',rhs'  = (mapper_opt map_exp) mapper state' rhs in
      let state',attr' = map_attr mapper state' attr in
      mapper.stmt state' (StmtMem(lhs',init',rhs',attr'))
   | StmtTable(id,elems,attr) ->
      let state',id'    = map_id mapper state id in
      let state',elems' = (mapper_list map_exp) mapper state' elems in
      let state',attr'  = map_attr mapper state' attr in
      mapper.stmt state' (StmtTable(id',elems',attr'))
   | StmtReturn(e,attr) ->
      let state',e'    = map_exp mapper state e in
      let state',attr' = map_attr mapper state' attr in
      mapper.stmt state' (StmtReturn(e',attr'))
   | StmtBind(lhs,rhs,attr) ->
      let state',lhs'  = map_lhs_exp mapper state lhs in
      let state',rhs'  = map_exp mapper state' rhs in
      let state',attr' = map_attr mapper state' attr in
      mapper.stmt state' (StmtBind(lhs',rhs',attr'))
   | StmtType(name,args,members,attr) ->
      let state',name'    = map_id mapper state name in
      let state',args'    = (mapper_list map_typed_id) mapper state' args in
      let state',members' = (mapper_list map_val_decl) mapper state' members in
      let state',attr'    = map_attr mapper state' attr in
      mapper.stmt state' (StmtType(name',args',members',attr'))
   | StmtAliasType(name,args,tp,attr) ->
      let state',name' = map_id mapper state name in
      let state',args' = (mapper_list map_typed_id) mapper state args in
      let state',tp'   = map_type_exp mapper state' tp in
      let state',attr' = map_attr mapper state' attr in
      mapper.stmt state' (StmtAliasType(name',args',tp',attr'))
   | StmtEmpty ->
      mapper.stmt state StmtEmpty
   | StmtWhile(cond,stmts,attr) ->
      let state',cond'  = map_exp mapper state cond in
      let state',attr'  = map_attr mapper state' attr in
      mapper.stmt state' (StmtWhile(cond',stmts,attr'))
      |> map_stmt_subs mapper
   | StmtIf(cond,then_,Some(else_),attr) ->
      let state',cond'  = map_exp mapper state cond in
      let state',attr'  = map_attr mapper state' attr in
      mapper.stmt state' (StmtIf(cond',then_,Some(else_),attr'))
      |> map_stmt_subs mapper
   | StmtIf(cond,then_,None,attr) ->
      let state',cond'  = map_exp mapper state cond in
      let state',attr'  = map_attr mapper state' attr in
      mapper.stmt state' (StmtIf(cond',then_,None,attr'))
      |> map_stmt_subs mapper
   | StmtFun(name,args,body,ret,attr) ->
      let state'       = Env.enter state name in
      let state',name' = map_id mapper state' name in
      let state',args' = (mapper_list map_typed_id) mapper state' args in
      let state',ret'  = (mapper_opt map_type_exp) mapper state' ret in
      let state',attr' = map_attr mapper state' attr in
      let state',stmt' = mapper.stmt state' (StmtFun(name',args',body,ret',attr'))
         |> map_stmt_subs mapper
      in
      let state'       = Env.exit state' in
      state', stmt'
   | StmtBlock(name,stmts,attr) ->
      let state',name'  = (mapper_opt map_id) mapper state name in
      let state',attr'  = map_attr mapper state' attr in
      mapper.stmt state' (StmtBlock(name',stmts,attr'))
      |> map_stmt_subs mapper

and map_stmt_list mapper = fun state stmt -> (mapper_list map_stmt) mapper state stmt

and map_stmt_subs (mapper:'state mapper) (state,stmt:('state * stmt)) : 'state * stmt =
   match stmt with
   | StmtVal(_,_,_)
   | StmtMem(_,_,_,_)
   | StmtTable(_,_,_)
   | StmtReturn(_,_)
   | StmtBind(_,_,_)
   | StmtType(_,_,_,_)
   | StmtAliasType(_,_,_,_)
   | StmtEmpty -> state, stmt
   | StmtWhile(cond,stmts,attr) ->
      let state',stmts' = map_stmt mapper state stmts in
      (state',(StmtWhile(cond,stmts',attr)))
   | StmtIf(cond,then_,Some(else_),attr) ->
      let state',then_' = map_stmt mapper state then_ in
      let state',else_' = map_stmt mapper state' else_ in
      (state',(StmtIf(cond,then_',Some(else_'),attr)))
   | StmtIf(cond,then_,None,attr) ->
      let state',then_' = map_stmt mapper state then_ in
      (state',(StmtIf(cond,then_',None,attr)))
   | StmtFun(name,args,body,ret,attr) ->
      let state',body' = map_stmt mapper state body in
      (state',(StmtFun(name,args,body',ret,attr)))
   | StmtBlock(name,stmts,attr) ->
      let state',stmts' = map_stmt_list mapper state stmts in
      (state',(StmtBlock(name,stmts',attr)))




