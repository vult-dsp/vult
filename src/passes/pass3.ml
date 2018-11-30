(*
The MIT License (MIT)

Copyright (c) 2014 Leonardo Laguna Ruiz

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

open PassCommon
open Env
open Prog
open Args
open Maps


module InsertContext = struct

   let makeTypeOfCtx (ctx:Id.path) : Typ.t =
      match ctx with
      | Id.Path id -> ref (Typ.TId(id, None))

   let getContextType state =
      let Id.Path(fn_name) = Env.currentScope state in
      let ctx_full = Env.getContext state fn_name in
      makeTypeOfCtx ctx_full

   let stmt : (PassData.t Env.t, stmt) Mapper.mapper_func =
      Mapper.make "InsertContext.stmt" @@ fun state stmt ->
      match stmt with
      | StmtFun(name,args,body,rettype,attr) ->
         let data     = Env.get state in
         let path, _, _ = Env.lookupRaise Scope.Function state name attr.loc in
         if Env.isActive state name && not (PassData.hasContextArgument data path) then
            let ctx_full = Env.getContext state name in
            let ctx_typ  = makeTypeOfCtx ctx_full in
            let ctx      = Env.pathFromCurrent state ctx_full in
            let typ      = [ref (Typ.TId(ctx, None))] in
            let arg0     = TypedId(["_ctx"], typ @ [ctx_typ], ContextArg, attr) in
            let data'    = PassData.markContextArgument data path in
            Env.set state data', StmtFun(name,arg0::args,body,rettype,attr)
         else
            state, stmt
      | _ -> state, stmt

   let exp : ('a Env.t,exp) Mapper.mapper_func =
      Mapper.make "InsertContext.exp" @@ fun state exp ->
      match exp with
      | PCall(Some(id), kind, args, attr) ->
         let Id.Path(fn_name) = Env.currentScope state in
         let Id.Path(context) = Env.getContext state fn_name in
         let typ = ref (Typ.TId(context, None)) in
         state, PCall(None, kind, PId("_ctx" :: id, { attr with typ = Some(typ) }) :: args, attr)
      | PId(id, attr) when Env.isLocalInstanceOrMem state id ->
         state, PId("_ctx" :: id, attr)
      | _ -> state, exp

   let lhs_exp  : ('a Env.t,lhs_exp) Mapper.mapper_func =
      Mapper.make "InsertContext.lhs_exp" @@ fun state exp ->
      match exp with
      | LId(id, Some tp, attr) when Env.isLocalInstanceOrMem state id ->
         let ctx_typ  = getContextType state in
         state, LId("_ctx" :: id, Some (tp @ [ctx_typ]), attr)
      | LIndex(id, tp, index, attr) when Env.isLocalInstanceOrMem state id ->
         state, LIndex("_ctx" :: id, tp, index, attr)
      | _ -> state,exp

   let stmt_x : ('a Env.t, stmt) Mapper.expand_func =
      Mapper.makeExpander "InsertContext.stmt_x" @@ fun state stmt ->
      match stmt with
      | StmtMem _ -> state, []
      | _ -> state, [stmt]

   let mapper =
      { Mapper.default_mapper with Mapper.stmt = stmt; Mapper.exp = exp; Mapper.lhs_exp = lhs_exp; Mapper.stmt_x = stmt_x }
end

module CreateInitFunction = struct

   module StmtSet = Set.Make(struct type t = stmt let compare = compare_stmt end)

   let rec getInitFunctioName (id:Id.t) : Id.t =
      match id with
      | [] -> failwith "getInitFunctioName: empty id"
      | [last] -> [ last ^ "_init" ]
      | h :: t -> h :: (getInitFunctioName t)

   let rec getFunctioTypeName (id:Id.t) : Id.t =
      match id with
      | [] -> failwith "getFunctioTypeName: empty id"
      | [last] -> [ last ^ "_type" ]
      | h :: t -> h :: (getFunctioTypeName t)

   let rec getInitValue (tp:Typ.t) : exp =
      let typedAttr = { emptyAttr with typ = Some(tp)} in
      match !tp with
      | Typ.TId(["real"], _) -> PReal(0.0,typedAttr)
      | Typ.TId(["int"], _)  -> PInt(0,typedAttr)
      | Typ.TId(["abstract"], _) -> PInt(0,typedAttr)
      | Typ.TId(["bool"], _) -> PBool(false,typedAttr)
      | Typ.TId(name, _)     -> PCall(None,getInitFunctioName name, [],typedAttr)
      | Typ.TComposed(["array"], [sub; { contents = Typ.TInt(size, _) }], _) ->
         let sub_init    = getInitValue sub in
         let intTypeAttr = {emptyAttr with typ = Some(Typ.Const.int_type)} in
         PCall(None, ["makeArray"], [PInt(size,intTypeAttr);sub_init],typedAttr)
      | Typ.TComposed(["tuple"],types, _) ->
         let elems = List.map getInitValue types in
         PTuple(elems,typedAttr)
      | Typ.TLink(tp) -> getInitValue tp
      | _ -> failwith "getInitValue"

   let getContextIfPossible state tp =
      match tp with
      | { contents = Typ.TId(tp_name, _) } ->
         begin
            try
               let context_path = Env.getContext state tp_name in
               let context      = Env.pathFromCurrent state context_path in
               ref (Typ.TId(context,None))
            with | _ -> tp
         end
      | _ -> tp

   let generateInitFunction (ctx:Id.t) (init_fun:Id.t option) (member_set:IdTypeSet.t) : stmt =
      let ctx_name = ["_ctx"] in
      let ctx_typ = ref (Typ.TId(ctx,None)) in
      let ctx_lid = PId(ctx_name, { emptyAttr with typ = Some(ctx_typ)}) in
      (* Generates bindings for all members *)
      let new_stmts_set =
         IdTypeSet.fold
            (fun (name,tp) acc ->
                let typedAttr = { emptyAttr with typ = Some(tp) } in
                let lhs       = LId(ctx_name @ name, Some [tp; ctx_typ],typedAttr) in
                let new_stmt  = StmtBind(lhs, getInitValue tp, emptyAttr) in
                StmtSet.add new_stmt acc)
            member_set StmtSet.empty
      in
      let attr = { emptyAttr with typ = Some ctx_typ } in
      (* Generates a call to the initialization function if there's one*)
      let init_fun_call =
         match init_fun with
         | Some(init_fun_name) ->
            let unitAttr = { emptyAttr with typ=Some(Typ.Const.unit_type)} in
            let callExp  = PCall(None,init_fun_name, [ctx_lid],unitAttr) in
            [StmtBind(LWild(emptyAttr),callExp,emptyAttr)]
         | None -> []
      in
      let return_stmt = StmtReturn(PId(ctx_name, attr), emptyAttr) in
      let ctx_decl = StmtVal(LId(ctx_name, Some [ctx_typ], attr), None, emptyAttr) in
      let stmts = StmtSet.fold (fun a acc -> a :: acc) new_stmts_set (init_fun_call @ [return_stmt]) in
      StmtFun(getInitFunctioName ctx, [], StmtBlock(None, ctx_decl :: stmts, emptyAttr), Some ctx_typ, emptyAttr)

   let generateContextType (ctx:Id.t) (member_set:IdTypeSet.t) : stmt =
      let members =
         IdTypeSet.fold
            (fun (name,tp) acc ->
                (name, [tp], emptyAttr) :: acc
            ) member_set []
      in
      StmtType(ref (Typ.TId(ctx,None)), members, emptyAttr)

   let generateInitFunctionWrapper (state:'a Env.t) (name:Id.t) : stmt =
      let ctx_path = Env.getContext state name in
      let ctx = Env.pathFromCurrent state ctx_path in
      let typ = ref (Typ.TId(ctx,None)) in
      let attr = { emptyAttr with typ = Some(typ) } in
      StmtFun(getInitFunctioName name,
              [],
              StmtReturn(PCall(None,getInitFunctioName ctx, [],attr),attr),
              Some(typ), emptyAttr)

   let generateTypeAlias (state:'a Env.t) (name:Id.t) : stmt =
      let ctx_path  = Env.getContext state name in
      let ctx       = Env.pathFromCurrent state ctx_path in
      let typ       = ref (Typ.TId(ctx,None)) in
      let name_type = ref (Typ.TId(getFunctioTypeName name,None)) in
      StmtAliasType(name_type,typ,emptyAttr)

   let stmt_x : ('a Env.t,stmt) Mapper.expand_func =
      Mapper.makeExpander "CreateInitFunction.stmt_x" @@ fun state stmt ->
      match stmt with
      | StmtFun(name, _, _, _,attr) ->
         let data = Env.get state in
         let path, _, _ = Env.lookupRaise Scope.Function state name attr.loc in
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
               let state'     = Env.set state data' in
               state', [type_def; type_fn; init_funct; init_fn; stmt]
         else
            state, [stmt]

      | _ -> state, [stmt]

   let mapper =
      { Mapper.default_mapper with Mapper.stmt_x = stmt_x }

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

let run =
   InsertContext.mapper
   |> Mapper.seq CreateInitFunction.mapper
   |> Mapper.seq OtherErrors.mapper