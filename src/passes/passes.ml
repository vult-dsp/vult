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
      pass4 : bool;
   }

let default_options =
   {
      pass1 = true;
      pass2 = true;
      pass3 = true;
      pass4 = true;
   }

module PassData = struct

   type t =
      {
         gen_init_ctx : PathSet.t; (** Context for which a init function has been generated *)
         add_ctx      : PathSet.t;
         used_tuples  : TypeSet.t;
         repeat       : bool;
         args         : arguments;
      }

   let hasInitFunction (t:t) (path:path) : bool =
      PathSet.mem path t.gen_init_ctx

   let hasContextArgument (t:t) (path:path) : bool =
      PathSet.mem path t.add_ctx

   let markInitFunction (t:t) (path:path) : t =
      { t with gen_init_ctx = PathSet.add path t.gen_init_ctx }

   let markContextArgument (t:t) (path:path) : t =
      { t with add_ctx = PathSet.add path t.add_ctx }

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

   let empty args =
      {
         gen_init_ctx = PathSet.empty;
         repeat       = false;
         add_ctx      = PathSet.empty;
         used_tuples  = TypeSet.empty;
         args         = args;
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

let newState (state:'a Env.t) (data:'b) : 'b Env.t =
   Env.derive state data

let restoreState (original:'a Env.t) (current:'b Env.t) : 'a Env.t * 'b =
   let current_data = Env.get current in
   let original_data = Env.get original in
   Env.derive current original_data, current_data

module InsertContext = struct

   let stmt : (PassData.t Env.t,stmt) Mapper.mapper_func =
      Mapper.make "InsertContext.stmt" @@ fun state stmt ->
      match stmt with
      | StmtFun(name,args,body,rettype,attr) ->
         let data     = Env.get state in
         let path,_,_ = Env.lookupRaise Scope.Function state name attr.loc in
         if Env.isActive state name && not (PassData.hasContextArgument data path) then
            let ctx_full = Env.getContext state name in
            let ctx      = Env.pathFromCurrent state ctx_full in
            let arg0     = TypedId(["_ctx"],ref (VType.TId(ctx,None)),attr) in
            let data'    = PassData.markContextArgument data path in
            Env.set state data', StmtFun(name,arg0::args,body,rettype,attr)
         else
            state, stmt
      | _ -> state, stmt

   let exp : ('a Env.t,exp) Mapper.mapper_func =
      Mapper.make "InsertContext.exp" @@ fun state exp ->
      match exp with
      | PCall(Some(id),kind,args,attr) ->
         let Path(context) = Env.getContext state kind in
         let typ = ref (VType.TId(context,None)) in
         state,PCall(None,kind,PId("_ctx"::id,{ attr with typ = Some(typ) })::args,attr)
      | PId(id,attr) when Env.isLocalInstanceOrMem state id ->
         state, PId("_ctx"::id,attr)
      | _ -> state,exp

   let lhs_exp  : ('a Env.t,lhs_exp) Mapper.mapper_func =
      Mapper.make "InsertContext.lhs_exp" @@ fun state exp ->
      match exp with
      | LId(id,tp,attr) when Env.isLocalInstanceOrMem state id ->
         state, LId("_ctx"::id,tp,attr)
      | _ -> state,exp

   let stmt_x : ('a Env.t, stmt) Mapper.expand_func =
      Mapper.makeExpander "InsertContext.stmt_x" @@ fun state stmt ->
      match stmt with
      | StmtMem _ -> state, []
      | _ -> state, [stmt]

   let mapper =
      { Mapper.default_mapper with Mapper.stmt = stmt; Mapper.exp = exp; Mapper.lhs_exp = lhs_exp; Mapper.stmt_x = stmt_x }
end


(** Splits mem declarations with binding to two statements *)
module SplitMem = struct

   let stmt_x : ('a Env.t,stmt) Mapper.expand_func =
      Mapper.makeExpander "SplitMem.stmt_x" @@ fun state stmt ->
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
      let typedAttr = { emptyAttr with typ = Some(tp)} in
      match !tp with
      | VType.TId(["real"],_) -> PReal(0.0,typedAttr)
      | VType.TId(["int"],_)  -> PInt(0,typedAttr)
      | VType.TId(["bool"],_) -> PBool(false,typedAttr)
      | VType.TId(name,_)     -> PCall(None,getInitFunctioName name,[],typedAttr)
      | VType.TComposed(["array"],[sub;{ contents = VType.TInt(size,_) }],_) ->
         let sub_init    = getInitValue sub in
         let intTypeAttr = {emptyAttr with typ = Some(VType.Constants.int_type)} in
         PCall(None,["makeArray"],[PInt(size,intTypeAttr);sub_init],typedAttr)
      | VType.TLink(tp) -> getInitValue tp
      | _ -> failwith "getInitValue"

   let getContextIfPossible state tp =
      match tp with
      | { contents = VType.TId(tp_name,_) } ->
         begin
            try
               let context_path = Env.getContext state tp_name in
               let context      = Env.pathFromCurrent state context_path in
               ref (VType.TId(context,None))
            with | _ -> tp
         end
      | _ -> tp

   let generateInitFunction (ctx:id) (init_fun:id option) (member_set:IdTypeSet.t) : stmt =
      let ctx_name = ["_ctx"] in
      let typ = ref (VType.TId(ctx,None)) in
      let ctx_lid = PId(ctx_name,{ emptyAttr with typ = Some(typ)}) in
      (* Generates bindings for all members *)
      let new_stmts_set =
         IdTypeSet.fold
            (fun (name,tp) acc ->
                let typedAttr = { emptyAttr with typ = Some(tp) } in
                let lhs       = LId(ctx_name @ name,Some(tp),typedAttr) in
                let new_stmt  = StmtBind(lhs, getInitValue tp, emptyAttr) in
                StmtSet.add new_stmt acc)
            member_set StmtSet.empty
      in
      let attr = { emptyAttr with typ = Some(typ) } in
      (* Generates a call to the initialization function if there's one*)
      let init_fun_call =
         match init_fun with
         | Some(init_fun_name) ->
            let unitAttr = { emptyAttr with typ=Some(VType.Constants.unit_type)} in
            let callExp  = PCall(None,init_fun_name,[ctx_lid],unitAttr) in
            [StmtBind(LWild(emptyAttr),callExp,emptyAttr)]
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
      let ctx_path = Env.getContext state name in
      let ctx = Env.pathFromCurrent state ctx_path in
      let typ = ref (VType.TId(ctx,None)) in
      let attr = { emptyAttr with typ = Some(typ) } in
      StmtFun(getInitFunctioName name,
              [],
              StmtReturn(PCall(None,getInitFunctioName ctx,[],attr),attr),
              Some(typ), emptyAttr)

   let generateTypeAlias (state:'a Env.t) (name:id) : stmt =
      let ctx_path  = Env.getContext state name in
      let ctx       = Env.pathFromCurrent state ctx_path in
      let typ       = ref (VType.TId(ctx,None)) in
      let name_type = ref (VType.TId(getFunctioTypeName name,None)) in
      StmtAliasType(name_type,typ,emptyAttr)

   let stmt_x : ('a Env.t,stmt) Mapper.expand_func =
      Mapper.makeExpander "CreateInitFunction.stmt_x" @@ fun state stmt ->
      match stmt with
      | StmtFun(name,_,_,_,attr) ->
         let data = Env.get state in
         let path,_,_ = Env.lookupRaise Scope.Function state name attr.loc in
         if Env.isActive state name && not (PassData.hasInitFunction data path) then
            let ctx_path = Env.getContext state name in
            let ctx     = Env.pathFromCurrent state ctx_path in
            let init_fn = generateInitFunctionWrapper state name in
            let type_fn = generateTypeAlias state name in
            if PassData.hasInitFunction data ctx_path then
               let data'   = PassData.markInitFunction data path in
               Env.set state data', [type_fn; init_fn; stmt]
            else
               let mem_inst = Env.getMemAndInstances state name in
               let member_set =
                  IdTypeSet.fold
                     (fun (name,tp) acc ->
                         let context = getContextIfPossible state tp in
                         let member = name, context in
                         IdTypeSet.add member acc)
                     mem_inst IdTypeSet.empty
               in
               let init_fun   = Env.getInitFunction state name in
               let init_funct = generateInitFunction ctx init_fun member_set in
               let type_def   = generateContextType ctx member_set in
               let data'      = PassData.markInitFunction data ctx_path in
               let data'      = PassData.markInitFunction data' path in
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
      Mapper.makeExpander "SimplifyTupleAssign.stmt_x" @@ fun state stmt ->
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
      Mapper.makeExpander "LHSTupleBinding.stmt_x" @@ fun state stmt ->
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
      Mapper.make "ReportUnboundType.lhs_exp" @@ fun state exp ->
      match exp with
      | LId(id,None,attr) ->
         reportError id attr
      | LId(id,Some(t),attr) when VType.isUnbound t ->
         reportError id attr
      | _ -> state, exp

   let exp : ('a Env.t,exp) Mapper.mapper_func =
      Mapper.make "ReportUnboundType.exp" @@ fun state exp ->
      match exp with
      | PId(id,attr) when isUnbound attr ->
         reportError id attr
      | _ -> state, exp

   let typed_id : ('a Env.t,typed_id) Mapper.mapper_func =
      Mapper.make "ReportUnboundType.typed_id" @@ fun state t ->
      match t with
      | TypedId(id,typ,attr) when VType.isUnbound typ ->
         reportError id attr
      | _ -> state, t

   let mapper = { Mapper.default_mapper with Mapper.lhs_exp = lhs_exp; exp = exp; typed_id = typed_id; }

end

module CreateTypesForTuples = struct

   (* This mapper is used to collect a set of tuples *)
   module TupleSet = struct
      let vtype_c : (TypeSet.t Env.t, VType.vtype) Mapper.mapper_func =
         Mapper.make "CreateTypesForTuples.vtype_c" @@ fun state t ->
         match t with
         | VType.TComposed(["tuple"],_,_) ->
            let tupl = Env.get state in
            Env.set state (TypeSet.add (ref t) tupl), t
         | _ -> state, t

      let mapper = Mapper.{ default_mapper with vtype_c }
   end

   let makeTypeDeclaration (t:VType.t) : stmt =
      match !t with
      | VType.TComposed(["tuple"],types,_) ->
         let elems = List.mapi (fun i a -> ["field_"^(string_of_int i)],a,emptyAttr) types in
         StmtType(t,elems,emptyAttr)
      | _ -> failwith "CreateTypesForTuples.makeTypeDeclaration: there should be only tuples here"

   let stmt_x : ('a Env.t,stmt) Mapper.expand_func =
      Mapper.makeExpander "CreateTypesForTuples.stmt_x" @@ fun state stmt ->
      match stmt with
      | StmtFun _ ->
         let data_env,_ = Mapper.map_stmt TupleSet.mapper (Env.empty TypeSet.empty) stmt in
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

   let mapper = Mapper.{ default_mapper with stmt_x }

end

module Simplify = struct

   (** Returns the sub elements of an operator, e.g. a+(b+c) -> [a,b,c] *)
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

   let isZero (e:exp) : bool =
      match e with
      | PInt(0,_)
      | PReal(0.0,_) -> true
      | _ -> false

   let isOne (e:exp) : bool =
      match e with
      | PInt(1,_)
      | PReal(1.0,_) -> true
      | _ -> false

   let minusOne attr (typ:VType.t) : exp =
      match !typ with
      | VType.TId(["int"],_) -> PInt(-1,attr)
      | VType.TId(["real"],_) -> PReal(-1.0,attr)
      | _ -> failwith "Simplify.minusOne: invalid numeric value"

   let applyOp (op:string) (e1:exp) (e2:exp) : exp =
      match op,e1,e2 with
      | "+",PInt(n1,attr),PInt(n2,_) -> PInt(n1+n2,attr)
      | "*",PInt(n1,attr),PInt(n2,_) -> PInt(n1*n2,attr)
      | "+",PReal(n1,attr),PReal(n2,_) -> PReal(n1+.n2,attr)
      | "*",PReal(n1,attr),PReal(n2,_) -> PReal(n1*.n2,attr)
      | _ -> failwith "Simplify.applyOp: invalid operation"

   let rec simplifyElems op (elems: exp list) : bool  * exp list =
      let constants,other = List.partition isNum elems in
      match constants with
      | []  -> false, elems
      | [c] when isZero c && op = "*" -> false, [c]
      | [c] when isOne c && op = "*" -> false, other
      | [c] when isZero c && op = "+" -> false, other
      | [_] -> false, elems
      | h :: t ->
         let c = List.fold_left (applyOp op) h t in
         true, c :: other

   let negNum (e:exp) : exp =
      match e with
      | PInt(value,attr) -> PInt(-value,attr)
      | PReal(value,attr) -> PReal(-.value,attr)
      | _ -> failwith "Simplify.negNum: not a number"

   let exp : ('a Env.t,exp) Mapper.mapper_func =
      Mapper.make "Simplify.exp" @@ fun state exp ->
      match exp with
      | POp("/",[e1;PReal(value,attr)],attr2) ->
         reapply state, POp("*",[e1;PReal(1.0 /. value,attr)],attr2)
      | POp("-",[e1;e2],attr) when isNum e2 ->
         reapply state, POp("+",[e1;negNum e2],attr)
      | POp("-",[e1;(PUnOp("-",e2,_))],attr) ->
         reapply state, POp("+",[e1;e2],attr)
      | POp("-",[e1;e2],attr) ->
         reapply state, POp("+",[e1;PUnOp("-",e2,attr)],attr)
      | PUnOp("-",POp("*",elems,({typ = Some(t)} as attr)),_) when List.exists isNum elems ->
         let minus = minusOne attr t in
         reapply state, POp("*",minus::elems,attr)

      (* Collapses trees of sums and multiplications *)
      | POp(op,elems,attr) when op = "+" || op = "*" ->
         let found, elems' = getOpElements op elems in
         let simpl, elems' = simplifyElems op elems' in
         let state' = if found || simpl then reapply state else state in
         state', POp(op,elems',attr)
      (* Simplifies unary minus *)
      | PUnOp("-",e1,_) when isNum e1 ->
         reapply state, negNum e1

      | _ -> state, exp

   let mapper = { Mapper.default_mapper with Mapper.exp = exp }

end

module OtherErrors = struct

   let exp : ('a Env.t,exp) Mapper.mapper_func =
      Mapper.make "OtherErrors.exp" @@ fun state exp ->
      match exp with
      | PReal(v,attr) ->
         let () =
            let data = Env.get state in
            if (v > 32767.0 || v < -32768.0) && data.PassData.args.real = "fixed" then
               let msg = Printf.sprintf "This value '%f' cannot be represented with fixed-point numbers" v in
               Error.raiseError msg (attr.loc)
         in
         state,PReal(v,attr)
      | _ -> state, exp

   let mapper = { Mapper.default_mapper with Mapper.exp = exp }

end

module ProcessArrays = struct

   let getArraySize (typ_opt:VType.t option) : int =
      match typ_opt with
      | Some(typ) ->
         begin match typ with
            | { contents = VType.TComposed(["array"],[_;{ contents = VType.TInt(n,_)}],_)} -> n
            | _ -> failwith "ProcessArrays.getArraySize: the argument is not an array"
         end
      | _ -> failwith "ProcessArrays.getArraySize: type inference should have put a type here"

   let exp : ('a Env.t,exp) Mapper.mapper_func =
      Mapper.make "ProcessArrays.exp" @@ fun state exp ->
      match exp with
      | PCall(None,["size"],[arr],attr) ->
         let arr_attr = GetAttr.fromExp arr in
         let size = getArraySize arr_attr.typ in
         state, PInt(size,attr)
      | _ ->
         state, exp

   let mapper = { Mapper.default_mapper with Mapper.exp = exp }

end

module SimplifyIfExp = struct

   (** This mapper is used to bind the if expressions to a variable *)
   module BindIfExp = struct

      let exp : (stmt list Env.t,exp) Mapper.mapper_func =
         Mapper.make "BindIfExp.exp" @@ fun state exp ->
         match exp with
         | PIf(_,_,_,attr) when not (Env.insideIf state) ->
            let n,state' = Env.tick state in
            let var_name = "_if_"^(string_of_int n) in
            let exp'     = PId([var_name],attr) in
            let lhs      = LId([var_name],attr.typ,attr) in
            let stmt     = StmtVal(lhs,Some(exp),emptyAttr) in
            let acc      = Env.get state' in
            let state'   = Env.set state' (stmt::acc) in
            state',exp'
         | _ -> state,exp

      let mapper = { Mapper.default_mapper with Mapper.exp = exp }

   end

   let stmt_x : ('a Env.t,stmt) Mapper.expand_func =
      Mapper.makeExpander "SimplifyIfExp.stmt_x" @@ fun state stmt ->
      match stmt with
      | StmtBind(lhs,PIf(cond,then_,else_,ifattr),attr) ->
         reapply state,[StmtIf(cond,StmtBind(lhs,then_,ifattr),Some(StmtBind(lhs,else_,ifattr)),attr)]
      | StmtVal(lhs,Some(PIf(cond,then_,else_,ifattr)),attr) ->
         let decl = StmtVal(lhs,None,attr) in
         let if_ = StmtIf(cond,StmtBind(lhs,then_,ifattr),Some(StmtBind(lhs,else_,ifattr)),attr) in
         reapply state,[decl;if_]
      | StmtBind(lhs,rhs,attr) ->
         let acc       = newState state [] in
         let acc',rhs' = Mapper.map_exp_to_stmt BindIfExp.mapper acc rhs in
         let state',acc_stmts = restoreState state acc' in
         let stmts'    = StmtBind(lhs,rhs',attr)::acc_stmts in
         state', List.rev stmts'
      | StmtVal(lhs,Some(rhs),attr) ->
         let acc       = newState state [] in
         let acc',rhs' = Mapper.map_exp_to_stmt BindIfExp.mapper acc rhs in
         let state',acc_stmts = restoreState state acc' in
         let stmts'    = StmtVal(lhs,Some(rhs'),attr)::acc_stmts in
         state', List.rev stmts'
      | StmtReturn(e,attr) ->
         let acc       = newState state [] in
         let acc',e' = Mapper.map_exp_to_stmt BindIfExp.mapper acc e in
         let state',acc_stmts = restoreState state acc' in
         let stmts'    = StmtReturn(e',attr)::acc_stmts in
         state', List.rev stmts'
      | _ -> state, [stmt]

   let mapper = { Mapper.default_mapper with Mapper.stmt_x = stmt_x }

end

module ReplaceFunctionNames = struct

   let exp : ('a Env.t,exp) Mapper.mapper_func =
      Mapper.make "ReplaceFunctionNames.exp" @@ fun state exp ->
      match exp with
      | PCall(name,fname,args,attr) ->
         let Path(path),_,t = Env.lookupRaise Scope.Function state fname attr.loc in
         let final_name =
            match t.Scope.ext_fn with
            | Some(n) -> [n]
            | None -> path
         in
         state, PCall(name,final_name,args,attr)
      | _ -> state,exp

   let stmt : (PassData.t Env.t,stmt) Mapper.mapper_func =
      Mapper.make "ReplaceFunctionNames.stmt" @@ fun state stmt ->
      match stmt with
      | StmtFun([_],args,body,rettype,attr) ->
         let Path(path) = Env.currentScope state in
         state, StmtFun(path,args,body,rettype,attr)
      | _ ->
         state, stmt

   let vtype_c : (PassData.t Env.t,VType.vtype) Mapper.mapper_func =
      Mapper.make "ReplaceFunctionNames.vtype_c" @@ fun state typ ->
      match typ with
      | VType.TId(id,optloc) ->
         let loc =
            match optloc with
            | Some(loc) -> loc
            | None -> Loc.default
         in
         let Path(type_path),_,_ = Env.lookupRaise Scope.Type state id loc in
         state, VType.TId(type_path,optloc)
      | _ -> state, typ

   let mapper = Mapper.{ default_mapper with exp; stmt; vtype_c }

end

module ReturnReferences = struct

   let unitAttr attr = { attr with typ = Some(VType.Constants.unit_type)}

   let isSimpleType (typ:VType.t) : bool =
      match !typ with
      | VType.TId(["real"],_) -> true
      | VType.TId(["int"],_) -> true
      | VType.TId(["bool"],_) -> true
      | VType.TId(["unit"],_) -> true
      | _ -> false

   (** This mapper is used to bind the if expressions to a variable *)
   module BindFunctionCalls = struct

      let exp : (stmt list Env.t,exp) Mapper.mapper_func =
         Mapper.make "BindFunctionCalls.exp" @@ fun state exp ->
         match exp with
         | PCall(_,_,_,({ typ = Some(typ) } as attr)) when not (isSimpleType typ) ->
            let n,state' = Env.tick state in
            let var_name = "_if_"^(string_of_int n) in
            let exp'     = PId([var_name],attr) in
            let lhs      = LId([var_name],attr.typ,attr) in
            let stmt     = StmtVal(lhs,Some(exp),emptyAttr) in
            let acc      = Env.get state' in
            let state'   = Env.set state' (stmt::acc) in
            state',exp'
         | _ -> state,exp

      let mapper = { Mapper.default_mapper with Mapper.exp = exp }

   end

   let stmt : (PassData.t Env.t,stmt) Mapper.mapper_func =
      Mapper.make "ReturnReferences.stmt" @@ fun state stmt ->
      let data = Env.get state in
      if not data.PassData.args.ccode then
         state, stmt
      else
         match stmt with
         | StmtFun(name,args,body,Some(rettype),attr) when not (isSimpleType rettype) ->
            let output = TypedId(["_output_"],rettype,emptyAttr) in
            let stmt' = StmtFun(name,args@[output],body,Some(VType.Constants.unit_type),attr) in
            state, stmt'
         | StmtBind(LId(lhs,Some(typ),lattr),PCall(inst,name,args,attr),battr) when not (isSimpleType typ) ->
            let arg = PId(lhs,lattr) in
            let fixed_attr = unitAttr attr in
            state, StmtBind(LWild(fixed_attr),PCall(inst,name,args@[arg],attr),battr)
         | StmtVal(LId(lhs,Some(typ),lattr),Some(PCall(inst,name,args,attr)),battr) when not (isSimpleType typ) ->
            let arg = PId(lhs,lattr) in
            let fixed_attr = unitAttr attr in
            state, StmtVal(LWild(fixed_attr),Some(PCall(inst,name,args@[arg],attr)),battr)
         | _ -> state, stmt

   let stmt_x : ('a Env.t,stmt) Mapper.expand_func =
      Mapper.makeExpander "ReturnReferences.stmt_x" @@ fun state stmt ->
      let data = Env.get state in
      if not data.PassData.args.ccode then
         state, [stmt]
      else
         match stmt with
         | StmtBind(_,PCall(_,_,_,_),_) ->
            state, [stmt]
         | StmtVal(_,Some(PCall(_,_,_,_)),_) ->
            state, [stmt]
         | StmtBind(lhs,rhs,attr) ->
            let acc       = newState state [] in
            let acc',rhs' = Mapper.map_exp_to_stmt BindFunctionCalls.mapper acc rhs in
            let state',acc_stmts = restoreState state acc' in
            let state' = if CCList.is_empty acc_stmts then state' else reapply state' in
            let stmts' = StmtBind(lhs,rhs',attr) :: acc_stmts in
            state', List.rev stmts'
         | StmtVal(lhs,Some(rhs),attr) ->
            let acc       = newState state [] in
            let acc',rhs' = Mapper.map_exp_to_stmt BindFunctionCalls.mapper acc rhs in
            let state',acc_stmts = restoreState state acc' in
            let state' = if CCList.is_empty acc_stmts then state' else reapply state' in
            let stmts' = StmtVal(lhs,Some(rhs'),attr)::acc_stmts in
            state', List.rev stmts'
         | StmtReturn(e,attr) ->
            let eattr = GetAttr.fromExp e in
            begin match eattr.typ with
               | Some(typ) when not (isSimpleType typ) ->
                  let stmt' = StmtBind(LId(["_output_"],Some(typ),attr),e,attr) in
                  reapply state, [stmt';StmtReturn(PUnit(unitAttr eattr),attr)]
               | _ ->
                  let acc     = newState state [] in
                  let acc',e' = Mapper.map_exp_to_stmt BindFunctionCalls.mapper acc e in
                  let state',acc_stmts = restoreState state acc' in
                  let state' = if CCList.is_empty acc_stmts then state' else reapply state' in
                  let stmts' = StmtReturn(e',attr)::acc_stmts in
                  state', List.rev stmts'
            end
         | _ -> state, [stmt]


   let mapper = Mapper.{ default_mapper with stmt; stmt_x }


end


module UnlinkTypes = struct

   let vtype_c : (PassData.t Env.t,VType.vtype) Mapper.mapper_func =
      Mapper.make "UnlinkTypes.vtype_c" @@ fun state typ ->
      match typ with
      | VType.TLink(t) -> state, !t
      | _ -> state, typ

   let mapper = { Mapper.default_mapper with Mapper.vtype_c = vtype_c }

end

(* Basic transformations *)
let inferPass name (state,stmts) =
   let state' = Env.enter Scope.Module state name emptyAttr in
   let stmts,state',_ = Inference.inferStmtList state' Inference.NoType stmts in
   let state' = Env.exit state' in
   state',stmts

let pass1 =
   ReportUnboundType.mapper
   |> Mapper.seq UnlinkTypes.mapper
   |> Mapper.seq SplitMem.mapper
   |> Mapper.seq SimplifyTupleAssign.mapper
   |> Mapper.seq LHSTupleBinding.mapper
   |> Mapper.seq SimplifyIfExp.mapper

let pass2 =
   Simplify.mapper
   |> Mapper.seq ProcessArrays.mapper

let pass3 =
   InsertContext.mapper
   |> Mapper.seq CreateInitFunction.mapper
   |> Mapper.seq CreateTypesForTuples.mapper
   |> Mapper.seq OtherErrors.mapper

let pass4 =
   ReplaceFunctionNames.mapper
   |> Mapper.seq ReturnReferences.mapper


let rec applyPassRepeat name apply pass pass_name (state,stmts) =
   if Mapper.log then print_endline ("Running "^pass_name);
   if apply then
      let state',stmts' = Mapper.map_stmt_list pass state stmts in
      if shouldReapply state' then
         applyPassRepeat name apply pass pass_name (reset state',stmts')
      else
         state',stmts'
   else
      state,stmts

let applyPass name apply pass pass_name (state,stmts) =
   let state' = Env.enter Scope.Module state name emptyAttr in
   let state', stmts' = applyPassRepeat name apply pass pass_name (state',stmts) in
   let state' = Env.exit state' in
   (*print_endline (Env.show_full state');*)
   state',stmts'



let passes (name:id) (options:pass_options) (env,stmts) =
   (env,stmts)
   |> inferPass name
   |> applyPass name options.pass1 pass1 "pass 1"
   |> applyPass name options.pass2 pass2 "pass 2"
   |> applyPass name options.pass3 pass3 "pass 3"
   |> applyPass name options.pass4 pass4 "pass 4"

let apply env options (results:parser_results) =
   let module_name =
      results.file
      |> moduleName
      |> fun a -> [a]
   in
   passes module_name options (env,results.presult)


let applyTransformations args ?(options=default_options) (results:parser_results list) =
   let env = Env.empty (PassData.empty args) in
   let _,stmts_list =
      List.fold_left
         (fun (env,acc) stmts ->
             let env',stmts' = apply env options stmts in
             (*print_endline "-------------";
               print_endline (Env.show env');
               print_endline "-------------";*)
             env', stmts'::acc
         )
         (env,[])
         results
   in
   List.rev stmts_list

let applyTransformationsSingle args ?(options=default_options) (results:parser_results) =
   let env = Env.empty (PassData.empty args) in
   let _,stmts' = apply env options results in
   stmts'
