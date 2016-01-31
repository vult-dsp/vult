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

(** Transformations and optimizations of the syntax tree *)

open TypesVult
open VEnv
open Common

module PassData = struct

   type t =
      {
         gen_init_ctx : IdSet.t; (** Context for which a init function has been generated *)
         add_ctx      : IdSet.t;
         repeat       : bool;
      }

   let hasInitFunction (t:t) (id:id) : bool =
      IdSet.mem id t.gen_init_ctx

   let hasContextArgument (t:t) (id:id) : bool =
      IdSet.mem id t.add_ctx

   let markInitFunction (t:t) (id:id) : t =
      { t with gen_init_ctx = IdSet.add id t.gen_init_ctx }

   let markContextArgument (t:t) (id:id) : t =
      { t with add_ctx = IdSet.add id t.add_ctx }

   let reapply (t:t) : t =
      { t with repeat = true }

   let reset (t:t) : t =
      { t with repeat = false }

   let shouldReapply (t:t) : bool =
      t.repeat

   let empty = { gen_init_ctx = IdSet.empty; repeat = false; add_ctx = IdSet.empty }

end

let reapply (state:PassData.t Env.t) : PassData.t Env.t =
   let data = Env.get state in
   Env.set state (PassData.reapply data)

let reset (state:PassData.t Env.t) : PassData.t Env.t =
   let data = Env.get state in
   Env.set state (PassData.reset data)

let shouldReapply (state:PassData.t Env.t) : bool =
   PassData.shouldReapply (Env.get state)

module InsertContext = struct

   let stmt : (PassData.t Env.t,stmt) Mapper.mapper_func =
      Mapper.make @@ fun state stmt ->
         match stmt with
         | StmtFun(name,args,body,rettype,attr) ->
            let data = Env.get state in
            if Env.isActive state name && not (PassData.hasContextArgument data name) then
               let context = Env.getContext state name in
               let arg0 = TypedId(["_ctx"],ref (VType.TId(context,None)),attr) in
               let data' = PassData.markContextArgument data name in
               Env.set state data', StmtFun(name,arg0::args,body,rettype,attr)
            else
               state, stmt
         | _ -> state, stmt

   let exp : ('a Env.t,exp) Mapper.mapper_func =
      Mapper.make @@ fun state exp ->
         match exp with
         | PCall(Some(id),kind,args,attr) ->
            let context = Env.getContext state kind in
            let typ = ref (VType.TId(context,None)) in
            state,PCall(None,kind,PId("_ctx"::id,{ attr with typ = Some(typ) })::args,attr)
         | PId(id,attr) when Env.isLocalInstanceOrMem state id ->
            state, PId("_ctx"::id,attr)
         | _ -> state,exp

   let lhs_exp : ('a Env.t,lhs_exp) Mapper.mapper_func =
      Mapper.make @@ fun state exp ->
         match exp with
         | LId(id,tp,attr) when Env.isLocalInstanceOrMem state id ->
            state, LId("_ctx"::id,tp,attr)
         | _ -> state,exp

   let stmt_x : ('a Env.t, stmt) Mapper.expand_func =
      Mapper.makeExpander @@ fun state stmt ->
         match stmt with
         | StmtMem _ -> state, []
         | _ -> state, [stmt]

   let mapper =
      { Mapper.default_mapper with Mapper.stmt = stmt; Mapper.exp = exp; Mapper.lhs_exp = lhs_exp; Mapper.stmt_x = stmt_x }
end


(** Splits mem declarations with binding to two statements *)
module SplitMem = struct

   let stmt_x : ('a Env.t,stmt) Mapper.expand_func =
      Mapper.makeExpander @@ fun state stmt ->
         match stmt with
         | StmtMem(lhs,init,Some(rhs),attr) ->
            reapply state, [ StmtMem(lhs,init,None,attr); StmtBind(lhs,rhs,attr) ]
         | _ -> state, [stmt]

   let mapper =
      { Mapper.default_mapper with Mapper.stmt_x = stmt_x }

end

module CreateInitFunction = struct

   module StmtSet = Set.Make(struct type t = stmt let compare = compare_stmt end)

   let rec getInitFunctioName (id:id) : id =
      match id with
      | [] -> failwith "getInitFunctioName: empty id"
      | [last] -> [ last^"_init" ]
      | h::t -> h :: (getInitFunctioName t)

   let rec getFunctioTypeName (id:id) : id =
      match id with
      | [] -> failwith "getFunctioTypeName: empty id"
      | [last] -> [ last^"_type" ]
      | h::t -> h :: (getFunctioTypeName t)

   let rec getInitValue (tp:VType.t) : exp =
      match tp with
      | { contents = VType.TId(["real"],_) } -> PReal(0.0,{ emptyAttr with typ = Some(tp)})
      | { contents = VType.TId(["int"],_) } -> PInt(0,{ emptyAttr with typ = Some(tp)})
      | { contents = VType.TId(["bool"],_) } -> PBool(false,{ emptyAttr with typ = Some(tp)})
      | { contents = VType.TId(name,_) } ->
         PCall(None,getInitFunctioName name,[],{ emptyAttr with typ = Some(tp)})
      | { contents = VType.TLink(tp) } ->
         getInitValue tp
      | _ -> failwith "getInitValue"

   let getContextIfPossible state tp =
      match tp with
      | { contents = VType.TId(tp_name,_) } -> (try ref (VType.TId(Env.getContext state tp_name,None)) with | _ -> tp )
      | _ -> tp

   let generateInitFunction (ctx:id) (member_set:IdTypeSet.t) : stmt =
      let ctx_name = ["_ctx"] in
      let typ = ref (VType.TId(ctx,None)) in
      let new_stmts_set =
         IdTypeSet.fold
            (fun (name,tp) acc ->
               let new_stmt = StmtBind(LId(ctx_name @ name,Some(tp),{ emptyAttr with typ = Some(tp) }), getInitValue tp, emptyAttr) in
               StmtSet.add new_stmt acc)
            member_set StmtSet.empty
      in
      let attr = { emptyAttr with typ = Some(typ) } in
      let return_stmt = StmtReturn(PId(ctx_name,attr),emptyAttr) in
      let ctx_decl = StmtVal(LId(ctx_name,Some(typ),attr),None,emptyAttr) in
      let stmts = StmtSet.fold (fun a acc -> a::acc) new_stmts_set [return_stmt] in
      StmtFun(getInitFunctioName ctx, [], StmtBlock(None, ctx_decl::stmts, emptyAttr), Some(typ), emptyAttr)

   let generateContextType (ctx:id) (member_set:IdTypeSet.t) : stmt =
      let members =
         IdTypeSet.fold
            (fun (name,tp) acc ->
               (name, tp, emptyAttr) :: acc
            ) member_set []
      in
      StmtType(ref (VType.TId(ctx,None)),members,emptyAttr)

   let generateInitFunctionWrapper (state:'a Env.t) (name:id) : stmt =
      let ctx = Env.getContext state name in
      let typ = ref (VType.TId(ctx,None)) in
      let attr = { emptyAttr with typ = Some(typ) } in
      StmtFun(getInitFunctioName name,
         [],
         StmtReturn(PCall(None,getInitFunctioName ctx,[],attr),emptyAttr),
         Some(typ), emptyAttr)

   let generateTypeAlias (state:'a Env.t) (name:id) : stmt =
      let ctx  = Env.getContext state name in
      let typ  = ref (VType.TId(ctx,None)) in
      let name_type = ref (VType.TId(getFunctioTypeName name,None)) in
      StmtAliasType(name_type,typ,emptyAttr)

   let stmt_x : ('a Env.t,stmt) Mapper.expand_func =
      Mapper.makeExpander @@ fun state stmt ->
         match stmt with
         | StmtFun(name,_,_,_,_) ->
            let data = Env.get state in
            if Env.isActive state name && not (PassData.hasInitFunction data name) then
               let ctx     = Env.getContext state name in
               let init_fn = generateInitFunctionWrapper state name in
               let type_fn = generateTypeAlias state name in
               if PassData.hasInitFunction data ctx then
                  let data'   = PassData.markInitFunction data name in
                  Env.set state data', [type_fn; init_fn; stmt]
               else
                  let mem_vars, instances = Env.getMemAndInstances state name in
                  let member_set =
                     IdTypeSet.fold
                        (fun (name,tp) acc ->
                           let context = getContextIfPossible state tp in
                           let member = name, context in
                           IdTypeSet.add member acc)
                        (IdTypeSet.union mem_vars instances) IdTypeSet.empty
                  in
                  let init_funct = generateInitFunction ctx member_set in
                  let type_def   = generateContextType ctx member_set in
                  let data'      = PassData.markInitFunction data ctx in
                  let data'      = PassData.markInitFunction data' name in
                  Env.set state data', [type_def; type_fn; init_funct; init_fn; stmt]
            else
               state, [stmt]

         | _ -> state, [stmt]

   let mapper =
      { Mapper.default_mapper with Mapper.stmt_x = stmt_x }

end

module SimplifyTupleAssign = struct

   let makeTmp i = ["_tmp_" ^ (string_of_int i)]

   let makeValBind lhs rhs = StmtVal(lhs,Some(rhs),emptyAttr)

   let makeBind lhs rhs = StmtBind(lhs,rhs,emptyAttr)

   let createAssignments kind lhs rhs =
      let lhs_id = GetIdentifiers.fromLhsExpList lhs in
      let rhs_id = GetIdentifiers.fromExpList rhs in
      if IdSet.is_empty (IdSet.inter lhs_id rhs_id) then
         List.map2 (fun a b -> kind a b) lhs rhs
      else
         let stmts1 =
            List.mapi (fun i a ->
               let attr = GetAttr.fromExp a in
               makeValBind (LId(makeTmp i, attr.typ, attr)) a)
            rhs
         in
         let stmts2 =
            List.mapi (fun i a ->
               let attr = GetAttr.fromLhsExp a in
               kind a (PId(makeTmp i,attr)))
            lhs
         in
         stmts1 @ stmts2

   let stmt_x : ('a Env.t,stmt) Mapper.expand_func =
      Mapper.makeExpander @@ fun state stmt ->
         match stmt with
         | StmtVal(LTuple(lhs,_),None,attr) ->
            let stmts = List.map (fun a -> StmtVal(a,None,attr)) lhs in
            reapply state, stmts

         | StmtVal(LTuple(lhs,_),Some(PTuple(rhs,_)),_) when List.length lhs = List.length rhs ->
            let stmts = createAssignments makeValBind lhs rhs in
            reapply state, stmts

         | StmtBind(LTuple(lhs,_),PTuple(rhs,_),_) when List.length lhs = List.length rhs ->
            let stmts = createAssignments makeBind lhs rhs in
            reapply state, stmts

         | StmtBind(LTuple(_,_),PTuple(_,_),_) ->
            failwith "SimplifyTupleAssign.stmt_x: this error should be catched by the type checker"

         | _ -> state, [stmt]

   let mapper =
      { Mapper.default_mapper with Mapper.stmt_x = stmt_x }

end

module LHSTupleBinding = struct

   let stmt_x : ('a Env.t,stmt) Mapper.expand_func =
      Mapper.makeExpander @@ fun state stmt ->
         match stmt with
         | StmtVal(LTuple(_,_),Some(PId(_,_)),_) -> state, [stmt]
         | StmtVal(LTuple(_,_),Some(PTuple(_,_)),_) -> state, [stmt]
         | StmtBind(LTuple(_,_),PId(_,_),_) -> state, [stmt]
         | StmtBind(LTuple(_,_),PTuple(_,_),_) -> state, [stmt]
         | StmtBind((LTuple(_,_) as lhs),rhs,attr) ->
            let tick, state' = Env.tick state in
            let tmp_name = ["_tmp_"^(string_of_int tick)] in
            let tmp = PId(tmp_name,attr) in
            let typ = (GetAttr.fromExp rhs).typ in
            let ltmp = LId(tmp_name,typ,attr) in
            reapply state', [StmtVal(ltmp,Some(rhs),attr); StmtBind(lhs,tmp,attr)]
         | StmtVal((LTuple(_,_) as lhs),Some(rhs),attr) ->
            let tick, state' = Env.tick state in
            let tmp_name = ["_tmp_"^(string_of_int tick)] in
            let tmp = PId(tmp_name,attr) in
            let typ = (GetAttr.fromExp rhs).typ in
            let ltmp = LId(tmp_name,typ,attr) in
            reapply state', [StmtVal(ltmp,Some(rhs),attr); StmtVal(lhs,Some(tmp),attr)]
         | _ -> state, [stmt]

   let mapper =
      { Mapper.default_mapper with Mapper.stmt_x = stmt_x }

end

module ReportUnboundType = struct

   let reportError (name:id) (attr:attr) =
      let msg = Printf.sprintf "The type of variable '%s' cannot be infered. Add a type annotation." (idStr name) in
      Error.raiseError msg attr.loc

   let isUnbound attr =
      match attr.typ with
      | None -> false
      | Some(typ) -> VType.isUnbound  typ

   let lhs_exp : ('a Env.t,lhs_exp) Mapper.mapper_func =
      Mapper.make @@ fun state exp ->
         match exp with
         | LId(id,None,attr) ->
            reportError id attr
         | LId(id,Some(t),attr) when VType.isUnbound t ->
            reportError id attr
         | _ -> state, exp

   let exp : ('a Env.t,exp) Mapper.mapper_func =
      Mapper.make @@ fun state exp ->
         match exp with
         | PId(id,attr) when isUnbound attr ->
            reportError id attr
         | _ -> state, exp

   let typed_id : ('a Env.t,typed_id) Mapper.mapper_func =
      Mapper.make @@ fun state t ->
         match t with
         | TypedId(id,typ,attr) when VType.isUnbound typ ->
            reportError id attr
         | _ -> state, t

   let mapper = { Mapper.default_mapper with Mapper.lhs_exp = lhs_exp; exp = exp; typed_id = typed_id; }

end


(* Basic transformations *)
let inferPass (state,stmts) =
   let stmts,state,_ = Inference.inferStmtList state Inference.NoType stmts in
   state,stmts

let pass1_mapper =
   ReportUnboundType.mapper
   |> Mapper.seq SplitMem.mapper
   |> Mapper.seq InsertContext.mapper
   |> Mapper.seq SimplifyTupleAssign.mapper
   |> Mapper.seq LHSTupleBinding.mapper
   |> Mapper.seq CreateInitFunction.mapper

let rec pass1 (state,stmts) =
   let state',stmts' = Mapper.map_stmt_list pass1_mapper state stmts in
   if shouldReapply state' then
      let _ = print_endline "Reapplying mapper" in
      pass1 (reset state',stmts')
   else
      state',stmts'

let applyTransformations (results:parser_results) =
   let module_name =
      results.file
      |> Filename.basename
      |> Filename.chop_extension
      |> String.capitalize
      |> fun a -> [a]
   in
   let initial_state =
      Env.empty module_name PassData.empty
   in

   let passes stmts =
      (initial_state,stmts)
      |> inferPass
      |> pass1
      |> snd
   in

   let new_stmts =
      try
         CCError.map passes results.presult
      with
      | Error.VError(e) -> `Error([e])
   in
   { results with presult = new_stmts }
