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
      }

   let hasInitFunction (t:t) (id:id) : bool =
      IdSet.mem id t.gen_init_ctx

   let markInitFunction (t:t) (id:id) : t =
      { gen_init_ctx = IdSet.add id t.gen_init_ctx }

   let empty = { gen_init_ctx = IdSet.empty }

end


module GetAttr = struct

   let fromExp (e:exp) : attr =
      match e with
      | PUnit(attr)
      | PBool(_,attr)
      | PInt(_,attr)
      | PReal(_,attr)
      | PId(_,attr)
      | PUnOp(_,_,attr)
      | POp(_,_,attr)
      | PCall(_,_,_,attr)
      | PIf(_,_,_,attr)
      | PGroup(_,attr)
      | PTuple(_,attr)
      | PSeq(_,_,attr) -> attr
      | PEmpty -> emptyAttr
end

module InsertContext = struct

   let stmt : (PassData.t Env.t,stmt) Mapper.mapper_func =
      Mapper.make @@ fun state stmt ->
         match stmt with
         | StmtFun(name,args,body,rettype,attr) ->
            if Env.isActive state name then
               let context = Env.getContext state name in
               let arg0 = TypedId(["$ctx"],ref (VType.TId(context,None)),attr) in
               state,StmtFun(name,arg0::args,body,rettype,attr)
            else
               state, stmt
         | _ -> state, stmt

   let exp : ('a Env.t,exp) Mapper.mapper_func =
      Mapper.make @@ fun state exp ->
         match exp with
         | PCall(Some(id),kind,args,attr) ->
            state,PCall(None,kind,PId(id,attr)::args,attr)
         | PId(id,attr) when Env.isLocalInstanceOrMem state id ->
            state, PId("$ctx"::id,attr)
         | _ -> state,exp

   let lhs_exp : ('a Env.t,lhs_exp) Mapper.mapper_func =
      Mapper.make @@ fun state exp ->
         match exp with
         | LId(id,tp,attr) when Env.isLocalInstanceOrMem state id ->
            state, LId("$ctx"::id,tp,attr)
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
            state, [ StmtMem(lhs,init,None,attr); StmtBind(lhs,rhs,attr) ]
         | _ -> state, [stmt]

   let mapper =
      { Mapper.default_mapper with Mapper.stmt_x = stmt_x }

end
(*
module CreateInitFunction = struct

   module StmtSet = Set.Make(struct type t = stmt let compare = compare_stmt end)

   let rec getInitFunctioName (id:id) : id =
      match id with
      | [] -> failwith "getInitFunctioName: empty id"
      | [last] -> [ last^"_init" ]
      | h::t -> h :: (getInitFunctioName t)

   let getInitValue (tp:VType.t) : exp =
      match tp with
      | { contents = VType.TId(["real"],_) } -> PReal(0.0,emptyAttr)
      | { contents = VType.TId(["int"],_) } -> PInt(0,emptyAttr)
      | _ -> PReal(0.0,emptyAttr)

   let callInitFunction state (tp:VType.t) : exp =
      match tp with
      | { contents = VType.TId(name,_) } ->
         let fun_ctx = Env.getContext state name in
         PCall(None,getInitFunctioName fun_ctx,[],emptyAttr)
      | _ -> failwith "CreateInitFunction.callInitFunction: cannot initialize this yet"

   let generateInitFunction (state:'a Env.t) (name:id) : stmt =
      let mem_vars, instances = Env.getMemAndInstances state name in
      let ctx = Env.getContext state name in
      let ctx_name = ["$ctx"] in
      let new_stmts_set =
         IdTypeSet.fold
            (fun (name,tp) acc ->
               let new_stmt = StmtBind(LId(ctx_name @ name,Some(tp),emptyAttr), getInitValue tp, emptyAttr) in
               StmtSet.add new_stmt acc)
            mem_vars StmtSet.empty
      in
      let new_stmts_set' =
         IdTypeSet.fold
            (fun (name,tp) acc ->
               let new_stmt = StmtBind(LId(ctx_name @ name, Some(tp), emptyAttr), callInitFunction state tp, emptyAttr) in
               StmtSet.add new_stmt acc)
            instances new_stmts_set
      in
      let return_stmt = StmtReturn(PId(ctx_name,emptyAttr),emptyAttr) in
      let ctx_decl = StmtVal(LId(ctx_name,Some(ref (VType.TId(ctx,None))),emptyAttr),None,emptyAttr) in
      let stmts = StmtSet.fold (fun a acc -> a::acc) new_stmts_set' [return_stmt] in
      StmtFun(getInitFunctioName ctx, [], StmtBlock(None, ctx_decl::stmts, emptyAttr), None, emptyAttr)

   let generateContextType (state:'a Env.t) (name:id) : stmt =
      let mem_vars, instances = Env.getMemAndInstances state name in
      let ctx = Env.getContext state name in
      let members =
         IdTypeSet.fold
            (fun (name,tp) acc ->
               let member = name, tp, emptyAttr in
               member :: acc)
            (IdTypeSet.union mem_vars instances) []
      in
      StmtType(ctx,[],members,emptyAttr)

   let generateInitFunctionWrapper (state:'a Env.t) (name:id) : stmt =
      let ctx = Env.getContext state name in
      StmtFun(getInitFunctioName name,
         [],
         StmtReturn(PCall(None,getInitFunctioName ctx,[],emptyAttr),emptyAttr),
         None, emptyAttr)


   let stmt_x : ('a Env.t,stmt) Mapper.expand_func =
      Mapper.makeExpander @@ fun state stmt ->
         match stmt with
         | StmtFun(name,_,_,_,_) ->
            if Env.isActive state name then
               let data = Env.get state in
               let ctx = Env.getContext state name in
               let init_fn = generateInitFunctionWrapper state name in
               if PassData.hasInitFunction data ctx then
                  state, [stmt; init_fn]
               else
                  let init_funct = generateInitFunction state name in
                  let type_def   = generateContextType state name in
                  let data'      = PassData.markInitFunction data ctx in
                  Env.set state data', [type_def; stmt; init_funct; init_fn]
            else
               state, [stmt]

         | _ -> state, [stmt]

   let mapper =
      { Mapper.default_mapper with Mapper.stmt_x = stmt_x }

end
*)
module SimplifyTupleAssign = struct

   let makeTmp i = ["$tmp_" ^ (string_of_int i)]

   let makeValBind lhs rhs = StmtVal(lhs,Some(rhs),emptyAttr)

   let makeBind lhs rhs = StmtBind(lhs,rhs,emptyAttr)

   let createAssignments kind lhs rhs =
      let lhs_id = GetIdentifiers.fromLhsExpList lhs in
      let rhs_id = GetIdentifiers.fromExpList rhs in
      if IdSet.is_empty (IdSet.inter lhs_id rhs_id) then
         List.map2 (fun a b -> kind a b) lhs rhs
      else
         let stmts1 =
            List.mapi
               (fun i a ->
                  let attr = GetAttr.fromExp a in
                  kind (LId(makeTmp i, attr.typ, emptyAttr)) a)
               rhs
         in
         let stmts2 = List.mapi (fun i a -> kind a (PId(makeTmp i,emptyAttr))) lhs in
         stmts1 @ stmts2

   let stmt_x : ('a Env.t,stmt) Mapper.expand_func =
      Mapper.makeExpander @@ fun state stmt ->
         match stmt with
         | StmtVal(LTuple(lhs,_),None,attr) ->
            let stmts = List.map (fun a -> StmtVal(a,None,attr)) lhs in
            state, stmts

         | StmtVal(LTuple(lhs,_),Some(PTuple(rhs,_)),_) when List.length lhs = List.length rhs ->
            let stmts = createAssignments makeValBind lhs rhs in
            state, stmts

         | StmtBind(LTuple(lhs,_),PTuple(rhs,_),_) when List.length lhs = List.length rhs ->
            let stmts = createAssignments makeBind lhs rhs in
            state, stmts

         | StmtBind(LTuple(_,_),PTuple(_,_),_) ->
            failwith "SimplifyTupleAssign.stmt_x: this error should be catched by the type checker"

         | _ -> state, [stmt]

   let mapper =
      { Mapper.default_mapper with Mapper.stmt_x = stmt_x }

end

(* Basic transformations *)
let inferPass (state,stmts) =
   let stmts,state,_ = Inference.inferStmtList state None stmts in
   state,stmts

let pass1 (state,stmts) =
   let mapper =
      SplitMem.mapper
      (*|> Mapper.seq CollectContext.mapper*)
      |> Mapper.seq InsertContext.mapper
      |> Mapper.seq SimplifyTupleAssign.mapper
   in
   Mapper.map_stmt_list mapper state stmts

(*let pass2 (state,stmts) =
   let mapper =
      CreateInitFunction.mapper
   in
   Mapper.map_stmt_list mapper state stmts
*)
let dump (state,stmts) =
   Env.dump state;
   state,stmts

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
      |> dump
      (*|> pass2
      |> dump*)
      |> snd
   in

   let new_stmts =
      try
         CCError.map passes results.presult
      with
      | Error.VError(e) -> `Error([e])
   in
   { results with presult = new_stmts }
