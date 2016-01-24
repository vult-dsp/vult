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
open VEnv

type ('data,'kind) mapper_func = ('data -> 'kind -> 'data * 'kind) option

type ('data,'kind) expand_func = ('data -> 'kind -> 'data * 'kind list) option

let apply (mapper:('data,'kind) mapper_func) (data:'data) (kind:'kind) : 'data * 'kind =
   match mapper with
   | Some(f) -> f data kind
   | None    -> data, kind

let applyExpander (mapper:('data,'kind) expand_func) (data:'data) (kind:'kind) : 'data * 'kind list =
   match mapper with
   | Some(f) -> f data kind
   | None    -> data, [kind]

let applyExpanderList (mapper:('data,'kind) expand_func) (data:'data) (kind_list:'kind list) : 'data * 'kind list =
   let state', rev_exp_list =
      List.fold_left
         (fun (s,acc) k ->
            let s', kl = applyExpander mapper s k in
            (s', kl::acc)
         ) (data,[]) kind_list
   in state', rev_exp_list |> List.rev |> List.flatten

let rec applyExpanderListIter (mapper:('data,'kind) expand_func) (data:'data) (kind_list:'kind list) : 'data * 'kind list =
   let data',kind_list' = applyExpanderList mapper data kind_list in
   (* A little bit inefficient and dangerous but good for now *)
   if List.length kind_list' <> List.length kind_list then
      applyExpanderListIter mapper data' kind_list'
   else
      data',kind_list'

let make (mapper:'data -> 'kind -> 'data * 'kind) : ('data,'kind) mapper_func =
   Some(mapper)

let makeExpander (mapper:'data -> 'kind -> 'data * 'kind list) : ('data,'kind) expand_func =
   Some(mapper)

(** Makes a chain of mappers. E.g. foo |-> bar will apply first foo then bar. *)
let (|->) : ('data,'value) mapper_func -> ('data,'value) mapper_func -> ('data,'value) mapper_func =
   fun mapper1 mapper2 ->
      let mapper3 =
         fun state exp ->
            let state', exp' = apply mapper1 state exp in
            apply mapper2 state' exp'
      in Some(mapper3)

let (|*>) : ('data,'value) expand_func -> ('data,'value) expand_func -> ('data,'value) expand_func =
   fun mapper1 mapper2 ->
      let mapper3 =
         fun state exp ->
            let state', exp_list  = applyExpander mapper1 state exp in
            let state', exp_list' = applyExpanderListIter mapper2 state' exp_list in
            state', exp_list'
      in Some(mapper3)

type 'a mapper =
   {
      vtype_c  : ('a, VType.vtype) mapper_func;
      typed_id : ('a, typed_id)   mapper_func;
      exp      : ('a, exp)        mapper_func;
      lhs_exp  : ('a, lhs_exp)    mapper_func;
      val_decl : ('a, val_decl)   mapper_func;
      stmt     : ('a, stmt)       mapper_func;
      attr     : ('a, attr)       mapper_func;
      id       : ('a, id)         mapper_func;

      stmt_x   : ('a, stmt)      expand_func;
   }

let default_mapper =
   {
      vtype_c  = None;
      typed_id = None;
      exp      = None;
      lhs_exp  = None;
      val_decl = None;
      stmt     = None;
      attr     = None;
      id       = None;

      stmt_x   = None;
   }

(** Merge two mapper functions. This is a little bit weird but it will be
    improved when mapper functions are optional. *)
let seqMapperFunc a b =
   if a = None then b else
   if b = None then a
   else a |-> b

let seqExpandFunc a b =
   if a = None then b else
   if b = None then a
   else a |*> b

(** Merges two mappers *)
let seq (b:'a mapper) (a:'a mapper) : 'a mapper =
   {
      vtype_c  = seqMapperFunc a.vtype_c  b.vtype_c;
      typed_id = seqMapperFunc a.typed_id b.typed_id;
      exp      = seqMapperFunc a.exp      b.exp;
      lhs_exp  = seqMapperFunc a.lhs_exp  b.lhs_exp;
      val_decl = seqMapperFunc a.val_decl b.val_decl;
      stmt     = seqMapperFunc a.stmt     b.stmt;
      attr     = seqMapperFunc a.attr     b.attr;
      id       = seqMapperFunc a.id       b.id;

      stmt_x   = seqExpandFunc a.stmt_x   b.stmt_x;

   }

(** Applies any mapper to a list *)
let mapper_list mapper_app =
   fun mapper state el ->
      let state',rev_el =
         List.fold_left
            (fun (s,acc) e ->
                let s',e' = mapper_app mapper s e in
                s',e'::acc)
            (state,[]) el
      in state', (List.rev rev_el)

(** Applies any mapper to an option value *)
let mapper_opt mapper_app =
   fun mapper state e_opt ->
      match e_opt with
      | None    -> state, None
      | Some(e) ->
         let state',e' = mapper_app mapper state e in
         state',Some(e')

let map_id (mapper:'a mapper) (state:'a) (id:id) : 'a * id =
   apply mapper.id state id

let map_attr (mapper:'a mapper) (state:'a) (attr:attr) : 'a * attr =
   apply mapper.attr state attr

let rec map_vtype_c (mapper:'a mapper) (state:'a) (te:VType.vtype) : 'a * VType.vtype =
   let map_vtype_list = mapper_list map_vtype in
   match te with
   | VType.TUnbound(_,_,_) ->
      state,te
   | VType.TId(id,loc) ->
      let state',id'   = map_id mapper state id in
      apply mapper.vtype_c state' (VType.TId(id',loc))
   | VType.TComposed(id,el,loc) ->
      let state',id'   = map_id mapper state id in
      let state',el'   = map_vtype_list mapper state' el in
      apply mapper.vtype_c state' (VType.TComposed(id',el',loc))
   | VType.TArrow(e1,e2,loc) ->
      let state',e1'   = map_vtype mapper state e1 in
      let state',e2'   = map_vtype mapper state' e2 in
      apply mapper.vtype_c state' (VType.TArrow(e1',e2',loc))
   | VType.TLink(tp) ->
      let state',tp'   = map_vtype mapper state tp in
      apply mapper.vtype_c state' (VType.TLink(tp'))
   | VType.TExpAlt(elems) ->
      let state', elems' = mapper_list map_vtype mapper state elems in
      apply mapper.vtype_c state' (VType.TExpAlt(elems'))

and map_vtype (mapper:'a mapper) (state:'a) (te:VType.vtype ref) : 'a * VType.vtype ref =
   let state',tp = map_vtype_c mapper state !te in
   te := tp;
   state', te

let rec map_typed_id (mapper:'a mapper) (state:'a) (t:typed_id) : 'a * typed_id =
   match t with
   | SimpleId(id,attr) ->
      let state',id'   = map_id mapper state id in
      let state',attr' = map_attr mapper state' attr in
      apply mapper.typed_id state' (SimpleId(id',attr'))
   | TypedId(id,tp,attr) ->
      let state',id'   = map_id mapper state id in
      let state',tp'   = map_vtype mapper state' tp in
      let state',attr' = map_attr mapper state' attr in
      apply mapper.typed_id state' (TypedId(id',tp',attr'))

let rec map_lhs_exp (mapper:'state mapper) (state:'state) (exp:lhs_exp) : 'state * lhs_exp =
   let map_lhs_exp_list = mapper_list map_lhs_exp in
   match exp with
   | LWild(attr) ->
      let state',attr' = map_attr mapper state attr in
      apply mapper.lhs_exp state' (LWild(attr'))
   | LId(id,tp,attr) ->
      let state',id'   = map_id mapper state id in
      let state',tp'   = (mapper_opt map_vtype) mapper state' tp in
      let state',attr' = map_attr mapper state' attr in
      apply mapper.lhs_exp state' (LId(id',tp',attr'))
   | LTyped(e,tp,attr) ->
      let state',e'    = map_lhs_exp  mapper state e in
      let state',tp'   = map_vtype mapper state' tp in
      let state',attr' = map_attr mapper state' attr in
      apply mapper.lhs_exp state' (LTyped(e',tp',attr'))
   | LTuple(elems,attr) ->
      let state',elems' = map_lhs_exp_list mapper state elems in
      let state',attr'  = map_attr mapper state' attr in
      apply mapper.lhs_exp state' (LTuple(elems',attr'))
   | LGroup(e,attr) ->
      let state',attr'  = map_attr mapper state attr in
      let state',e' = map_lhs_exp mapper state' e in
      apply mapper.lhs_exp state' (LGroup(e',attr'))

and map_lhs_exp_list mapper = fun state exp -> (mapper_list map_lhs_exp) mapper state exp

let map_val_decl (mapper:'state mapper) (state:'state) (v:val_decl) : 'state * val_decl =
   let id,tp,attr   = v in
   let state',id'   = map_id mapper state id in
   let state',tp'   = map_vtype mapper state' tp in
   let state',attr' = map_attr mapper state' attr in
   state',(id',tp',attr')

(** Traverses the expression in a bottom-up fashion *)
let rec map_exp (mapper:'state mapper) (state:'state) (exp:exp) : 'state * exp =
   match exp with
   | PUnit(attr) ->
      let state',attr' = map_attr mapper state attr in
      apply mapper.exp state' (PUnit(attr'))
   | PBool(b,attr) ->
      let state',attr' = map_attr mapper state attr in
      apply mapper.exp state' (PBool(b,attr'))
   | PInt(i,attr) ->
      let state',attr' = map_attr mapper state attr in
      apply mapper.exp state' (PInt(i,attr'))
   | PReal(r,attr) ->
      let state',attr' = map_attr mapper state attr in
      apply mapper.exp state' (PReal(r,attr'))
   | PId(id,attr) ->
      let state',id'   = map_id mapper state id in
      let state',attr' = map_attr mapper state' attr in
      apply mapper.exp state' (PId(id',attr'))
   | PEmpty -> apply mapper.exp state exp
   | PUnOp(op,e,attr) ->
      let state',e' = map_exp mapper state e in
      let state',attr' = map_attr mapper state' attr in
      apply mapper.exp state' (PUnOp(op,e',attr'))
   | POp(op,args,attr) ->
      let state',args' = map_exp_list mapper state args in
      let state',attr' = map_attr mapper state' attr in
      apply mapper.exp state' (POp(op,args',attr'))
   | PCall(inst,name,args,attr) ->
      let state',inst' = (mapper_opt map_id) mapper state inst in
      let state',name' = map_id mapper state' name in
      let state',args' = map_exp_list mapper state' args in
      let state',attr' = map_attr mapper state' attr in
      apply mapper.exp state' (PCall(inst',name',args',attr'))
   | PIf(cond,then_,else_,attr) ->
      let state',cond'  = map_exp mapper state cond in
      let state',then_' = map_exp mapper state' then_ in
      let state',else_' = map_exp mapper state' else_ in
      let state',attr'  = map_attr mapper state' attr in
      apply mapper.exp state' (PIf(cond',then_',else_',attr'))
   | PGroup(e,attr) ->
      let state',e'     = map_exp mapper state e in
      let state',attr'  = map_attr mapper state' attr in
      apply mapper.exp state' (PGroup(e',attr'))
   | PTuple(el,attr) ->
      let state',el'    = map_exp_list mapper state el in
      let state',attr'  = map_attr mapper state' attr in
      apply mapper.exp state' (PTuple(el',attr'))
   | PSeq(id,stmt,attr) ->
      let state',id'   = (mapper_opt map_id) mapper state id in
      let state',stmt' = map_stmt mapper state' stmt in
      let state',attr' = map_attr mapper state' attr in
      apply mapper.exp state' (PSeq(id',stmt',attr'))

and map_exp_list mapper = fun state exp -> (mapper_list map_exp) mapper state exp

and map_stmt (mapper:'state mapper) (state:'state) (stmt:stmt) : 'state * stmt =
   match stmt with
   | StmtVal(lhs,rhs,attr) ->
      let state',lhs'  = map_lhs_exp mapper state lhs in
      let state',rhs'  = (mapper_opt map_exp) mapper state' rhs in
      let state',attr' = map_attr mapper state' attr in
      apply mapper.stmt state' (StmtVal(lhs',rhs',attr'))
   | StmtMem(lhs,init,rhs,attr) ->
      let state',lhs'  = map_lhs_exp mapper state lhs in
      let state',init' = (mapper_opt map_exp) mapper state' init in
      let state',rhs'  = (mapper_opt map_exp) mapper state' rhs in
      let state',attr' = map_attr mapper state' attr in
      apply mapper.stmt state' (StmtMem(lhs',init',rhs',attr'))
   | StmtTable(id,elems,attr) ->
      let state',id'    = map_id mapper state id in
      let state',elems' = (mapper_list map_exp) mapper state' elems in
      let state',attr'  = map_attr mapper state' attr in
      apply mapper.stmt state' (StmtTable(id',elems',attr'))
   | StmtReturn(e,attr) ->
      let state',e'    = map_exp mapper state e in
      let state',attr' = map_attr mapper state' attr in
      apply mapper.stmt state' (StmtReturn(e',attr'))
   | StmtBind(lhs,rhs,attr) ->
      let state',lhs'  = map_lhs_exp mapper state lhs in
      let state',rhs'  = map_exp mapper state' rhs in
      let state',attr' = map_attr mapper state' attr in
      apply mapper.stmt state' (StmtBind(lhs',rhs',attr'))
   | StmtType(name,members,attr) ->
      let state',name' = map_vtype mapper state name in
      let state',members' = (mapper_list map_val_decl) mapper state' members in
      let state',attr'    = map_attr mapper state' attr in
      apply mapper.stmt state' (StmtType(name',members',attr'))
   | StmtAliasType(name,tp,attr) ->
      let state',name' = map_vtype mapper state name in
      let state',tp'   = map_vtype mapper state' tp in
      let state',attr' = map_attr mapper state' attr in
      apply mapper.stmt state' (StmtAliasType(name',tp',attr'))
   | StmtEmpty ->
      apply mapper.stmt state StmtEmpty
   | StmtWhile(cond,stmts,attr) ->
      let state',cond' = map_exp mapper state cond in
      let state',attr' = map_attr mapper state' attr in
      apply mapper.stmt state' (StmtWhile(cond',stmts,attr'))
      |> map_stmt_subs mapper
   | StmtIf(cond,then_,Some(else_),attr) ->
      let state',cond' = map_exp mapper state cond in
      let state',attr' = map_attr mapper state' attr in
      apply mapper.stmt state' (StmtIf(cond',then_,Some(else_),attr'))
      |> map_stmt_subs mapper
   | StmtIf(cond,then_,None,attr) ->
      let state',cond' = map_exp mapper state cond in
      let state',attr' = map_attr mapper state' attr in
      apply mapper.stmt state' (StmtIf(cond',then_,None,attr'))
      |> map_stmt_subs mapper
   | StmtFun(name,args,body,ret,attr) ->
      let state'       = Env.enter `Function state name in
      let state',name' = map_id mapper state' name in
      let state',args' = (mapper_list map_typed_id) mapper state' args in
      let state',ret'  = (mapper_opt map_vtype) mapper state' ret in
      let state',attr' = map_attr mapper state' attr in
      let state',stmt' =
         apply mapper.stmt state' (StmtFun(name',args',body,ret',attr'))
         |> map_stmt_subs mapper
      in
      let state'       = Env.exit `Function state' in
      state', stmt'
   | StmtExternal(name,args,ret,attr) ->
      let state'       = Env.enter `Function state name in
      let state',name' = map_id mapper state' name in
      let state',args' = (mapper_list map_typed_id) mapper state' args in
      let state',ret'  = map_vtype mapper state' ret in
      let state',attr' = map_attr mapper state' attr in
      let state',stmt' =
         apply mapper.stmt state' (StmtExternal(name',args',ret',attr'))
         |> map_stmt_subs mapper
      in
      let state'       = Env.exit `Function state' in
      state', stmt'
   | StmtBlock(name,stmts,attr) ->
      let state',name' = (mapper_opt map_id) mapper state name in
      let state',attr' = map_attr mapper state' attr in
      apply mapper.stmt state' (StmtBlock(name',stmts,attr'))
      |> map_stmt_subs mapper

and map_stmt_list mapper = fun state stmts ->
   let state', stmts' = (mapper_list map_stmt) mapper state stmts in
   let state', stmts' = applyExpanderListIter mapper.stmt_x state' stmts' in
   state', stmts'

and map_stmt_subs (mapper:'state mapper) (state,stmt:('state * stmt)) : 'state * stmt =
   match stmt with
   | StmtVal _
   | StmtMem _
   | StmtTable _
   | StmtReturn _
   | StmtBind _
   | StmtType _
   | StmtAliasType _
   | StmtExternal _
   | StmtEmpty -> state, stmt
   | StmtWhile(cond,stmts,attr) ->
      let state',stmts' = map_stmt mapper state stmts in
      state',(StmtWhile(cond,stmts',attr))
   | StmtIf(cond,then_,Some(else_),attr) ->
      let state',then_' = map_stmt mapper state then_ in
      let state',else_' = map_stmt mapper state' else_ in
      state',(StmtIf(cond,then_',Some(else_'),attr))
   | StmtIf(cond,then_,None,attr) ->
      let state',then_' = map_stmt mapper state then_ in
      state',(StmtIf(cond,then_',None,attr))
   | StmtFun(name,args,body,ret,attr) ->
      let state',body'  = map_stmt mapper state body in
      state',(StmtFun(name,args,body',ret,attr))
   | StmtBlock(name,stmts,attr) ->
      let state',stmts' = map_stmt_list mapper state stmts in
      state',(StmtBlock(name,stmts',attr))




