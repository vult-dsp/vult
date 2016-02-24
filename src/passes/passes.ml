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

type pass_options =
   {
      pass1 : bool;
      pass2 : bool;
      pass3 : bool;
   }

let default_options =
   {
      pass1 = true;
      pass2 = true;
      pass3 = true;
   }


module PassData = struct

   type t =
      {
         gen_init_ctx : IdSet.t; (** Context for which a init function has been generated *)
         add_ctx      : IdSet.t;
         used_tuples  : TypeSet.t;
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

   let addTuples (t:t) (tup:TypeSet.t) : t =
      { t with used_tuples = TypeSet.union tup t.used_tuples }

   let getTuples (t:t) : TypeSet.t =
      t.used_tuples

   let empty =
      {
         gen_init_ctx = IdSet.empty;
         repeat       = false;
         add_ctx      = IdSet.empty;
         used_tuples  = TypeSet.empty;
      }

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

   let generateInitFunction (ctx:id) (init_fun:id option) (member_set:IdTypeSet.t) : stmt =
      let ctx_name = ["_ctx"] in
      let typ = ref (VType.TId(ctx,None)) in
      let ctx_lid = PId(ctx_name,{ emptyAttr with typ = Some(typ)}) in
      (* Generates bindings for all members *)
      let new_stmts_set =
         IdTypeSet.fold
            (fun (name,tp) acc ->
               let new_stmt = StmtBind(LId(ctx_name @ name,Some(tp),{ emptyAttr with typ = Some(tp) }), getInitValue tp, emptyAttr) in
               StmtSet.add new_stmt acc)
            member_set StmtSet.empty
      in
      let attr = { emptyAttr with typ = Some(typ) } in
      (* Generates a call to the initialization function if there's one*)
      let init_fun_call =
         match init_fun with
         | Some(init_fun_name) -> [StmtBind(LWild(emptyAttr),PCall(None,init_fun_name,[ctx_lid],{emptyAttr with typ=Some(VType.Constants.unit_type)}),emptyAttr)]
         | None -> []
      in
      let return_stmt = StmtReturn(PId(ctx_name,attr),emptyAttr) in
      let ctx_decl = StmtVal(LId(ctx_name,Some(typ),attr),None,emptyAttr) in
      let stmts = StmtSet.fold (fun a acc -> a::acc) new_stmts_set (init_fun_call@[return_stmt]) in
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
         StmtReturn(PCall(None,getInitFunctioName ctx,[],attr),attr),
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
                  let init_fun   = Env.getInitFunction state name in
                  let init_funct = generateInitFunction ctx init_fun member_set in
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
         | StmtBind((LTuple(_,lhs_attr) as lhs),rhs,attr) ->
            let tick, state' = Env.tick state in
            let tmp_name = ["_tplbind_"^(string_of_int tick)] in
            let rhs_attr = GetAttr.fromExp rhs in
            let tmp = PId(tmp_name,rhs_attr) in
            let typ = rhs_attr.typ in
            let ltmp = LId(tmp_name,typ,lhs_attr) in
            reapply state', [StmtVal(ltmp,Some(rhs),attr); StmtBind(lhs,tmp,attr)]
         | StmtVal((LTuple(_,lhs_attr) as lhs),Some(rhs),attr) ->
            let tick, state' = Env.tick state in
            let tmp_name = ["_tplbind_"^(string_of_int tick)] in
            let rhs_attr = GetAttr.fromExp rhs in
            let tmp = PId(tmp_name,rhs_attr) in
            let typ = rhs_attr.typ in
            let ltmp = LId(tmp_name,typ,lhs_attr) in
            reapply state', [StmtVal(ltmp,Some(rhs),attr); StmtVal(lhs,Some(tmp),attr)]
         | StmtReturn((PTuple(_,rhs_attr) as rhs),attr) ->
            let tick, state' = Env.tick state in
            let tmp_name = ["_tplbind_"^(string_of_int tick)] in
            let tmp = PId(tmp_name,rhs_attr) in
            let typ = rhs_attr.typ in
            let ltmp = LId(tmp_name,typ,rhs_attr) in
            reapply state', [StmtVal(ltmp,Some(rhs),attr);StmtReturn(tmp,attr)]
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

module CreateTypesForTuples = struct

   let vtype_c : (TypeSet.t Env.t, VType.vtype) Mapper.mapper_func =
      Mapper.make @@ fun state t ->
         match t with
         | VType.TComposed(["tuple"],_,_) ->
            let tupl = Env.get state in
            Env.set state (TypeSet.add (ref t) tupl), t
         | _ -> state, t

   let getTuples_mapper = { Mapper.default_mapper with Mapper.vtype_c = vtype_c }

   let makeTypeDeclaration (t:VType.t) : stmt =
      match !t with
      | VType.TComposed(["tuple"],types,_) ->
         let elems = List.mapi (fun i a -> ["field_"^(string_of_int i)],a,emptyAttr) types in
         let name_str = VType.getTupleName t in
         let name = VType.TId([name_str],None) in
         StmtType(ref name,elems,emptyAttr)
      | _ -> failwith "CreateTypesForTuples.makeTypeDeclaration: there should be only tuples here"

   let stmt_x : ('a Env.t,stmt) Mapper.expand_func =
      Mapper.makeExpander @@ fun state stmt ->
         match stmt with
         | StmtFun(_,_,_,_,_) ->
            let data_env,_ = Mapper.map_stmt getTuples_mapper (Env.empty [] TypeSet.empty) stmt in
            let new_tuples = Env.get data_env in
            let data       = Env.get state in
            let current    = PassData.getTuples data in
            let not_in_set = TypeSet.diff new_tuples current in
            if not (TypeSet.is_empty not_in_set) then
               let decl =
                  TypeSet.fold
                     (fun a acc ->
                        let type_decl = makeTypeDeclaration a in
                        type_decl::acc)
                  not_in_set
                  []
               in
               let data' = PassData.addTuples data not_in_set in
               Env.set state data', decl@[stmt]
            else
               state, [stmt]
         | _ -> state, [stmt]

   let mapper = { Mapper.default_mapper with Mapper.stmt_x = stmt_x }

end

module Simplify = struct

   let rec getOpElements (op:string) (elems: exp list) : bool * exp list =
      match elems with
      | [] -> false,[]
      | POp(op',sub,_) :: t when op' = op ->
         let _, t' = getOpElements op t in
         true, sub @ t'
      | h :: t ->
         let found, t' = getOpElements op t in
         found, h :: t'

   let isNum (e:exp) : bool =
      match e with
      | PInt _
      | PReal _ -> true
      | _ -> false

   let negNum (e:exp) : exp =
      match e with
      | PInt(value,attr) -> PInt(-value,attr)
      | PReal(value,attr) -> PReal(-.value,attr)
      | _ -> failwith "Simplify.negNum: not a number"


   let exp : ('a Env.t,exp) Mapper.mapper_func =
      Mapper.make @@ fun state exp ->
         match exp with
         | POp("/",[e1;PReal(value,attr)],attr2) ->
            reapply state, POp("*",[e1;PReal(1.0 /. value,attr)],attr2)
         | POp("-",[e1;e2],attr) when isNum e2 ->
            reapply state, POp("+",[e1;negNum e2],attr)
         (* Collapses trees of sums and multiplications *)
         | POp(op,elems,attr) when op = "+" || op = "*" ->
            let found, elems' = getOpElements op elems in
            let state' = if found then reapply state else state in
            state', POp(op,elems',attr)
         (* Simplifies unary minus *)
         | PUnOp("-",e1,_) when isNum e1 ->
            reapply state, negNum e1

         | _ -> state, exp

   let mapper = { Mapper.default_mapper with Mapper.exp = exp }

end

(* Basic transformations *)
let inferPass (state,stmts) =
   let stmts,state,_ = Inference.inferStmtList state Inference.NoType stmts in
   state,stmts

let pass1 =
   ReportUnboundType.mapper
   |> Mapper.seq SplitMem.mapper
   |> Mapper.seq SimplifyTupleAssign.mapper
   |> Mapper.seq LHSTupleBinding.mapper

let pass2 =
   Simplify.mapper

let pass3 =
   InsertContext.mapper
   |> Mapper.seq CreateInitFunction.mapper
   |> Mapper.seq CreateTypesForTuples.mapper

let rec applyPass apply pass (state,stmts) =
   if apply then
      let state',stmts' = Mapper.map_stmt_list pass state stmts in
      if shouldReapply state' then
         applyPass apply pass (reset state',stmts')
      else
         state',stmts'
   else
      state,stmts

let applyTransformations ?(options=default_options) (results:parser_results) =
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
      |> applyPass options.pass1 pass1
      |> applyPass options.pass2 pass2
      |> applyPass options.pass3 pass3
      |> snd
   in

   let new_stmts =
      try
         CCError.map passes results.presult
      with
      | Error.VError(e) -> `Error([e])
   in
   { results with presult = new_stmts }
