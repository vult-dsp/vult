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
open Common
open Args

module ReplaceFunctionNames = struct

   let exp : ('a Env.t, exp) Mapper.mapper_func =
      Mapper.make "ReplaceFunctionNames.exp" @@ fun state exp ->
      match exp with
      | PCall(name,fname,args,attr) when attr.ext_fn = None ->
         let Id.Path(path), _, t = Env.lookupRaise Scope.Function state fname attr.loc in
         let final_name, attr =
            match !(t.Scope.ext_fn) with
            | Some(n) -> [n], { attr with ext_fn = Some n}
            | None -> path, attr
         in
         state, PCall(name, final_name, args, attr)
      | _ -> state,exp

   let stmt : (PassData.t Env.t,stmt) Mapper.mapper_func =
      Mapper.make "ReplaceFunctionNames.stmt" @@ fun state stmt ->
      match stmt with
      | StmtFun([_],args,body,rettype,attr) ->
         let Id.Path(path) = Env.currentScope state in
         state, StmtFun(path,args,body,rettype,attr)
      | _ ->
         state, stmt

   let vtype_c : (PassData.t Env.t,Typ.vtype) Mapper.mapper_func =
      Mapper.make "ReplaceFunctionNames.vtype_c" @@ fun state typ ->
      match typ with
      | Typ.TId(id,optloc) ->
         let loc =
            match optloc with
            | Some(loc) -> loc
            | None -> Loc.default
         in
         let Id.Path(type_path), _, _ = Env.lookupRaise Scope.Type state id loc in
         state, Typ.TId(type_path,optloc)
      | _ -> state, typ

   let mapper = Mapper.{ default_mapper with exp; stmt; vtype_c }

end

module ReturnReferences = struct

   let unitAttr attr = { attr with typ = Some(Typ.Const.unit_type)}

   let stmt : (PassData.t Env.t,stmt) Mapper.mapper_func =
      Mapper.make "ReturnReferences.stmt" @@ fun state stmt ->
      let data = Env.get state in
      let args = data.PassData.args in
      if not (args.code = CCode) then
         state, stmt
      else
         match stmt with
         | StmtFun(name, args, body, Some(rettype), attr) when not (Typ.isSimpleType rettype) ->
            let output = TypedId(["_output_"], [rettype], OutputArg,emptyAttr) in
            let stmt' = StmtFun(name, args @ [output], body, Some(Typ.Const.unit_type), attr) in
            state, stmt'
         | _ -> state, stmt

   let stmt_x : ('a Env.t,stmt) Mapper.expand_func =
      Mapper.makeExpander "ReturnReferences.stmt_x" @@ fun state stmt ->
      let data = Env.get state in
      let args = data.PassData.args in
      if not (args.code = CCode) then
         state, [stmt]
      else
         match stmt with
         (* regular case a = foo() *)
         | StmtBind(LId(lhs, Some(typ), lattr), PCall(inst, name, args, attr), battr) when not (Typ.isSimpleType (Typ.first typ)) ->
            let arg = PId(lhs, lattr) in
            let fixed_attr = unitAttr attr in
            state, [StmtBind(LWild(fixed_attr), PCall(inst, name, args @ [arg], fixed_attr), battr)]
         (* special case _ = foo() when the return is no simple value *)
         | StmtBind(LWild(wattr),PCall(inst,name,args,attr),battr) when not (Typ.isSimpleOpType wattr.typ) ->
            let i,state' = Env.tick state in
            let tmp_name = "_unused_" ^ (string_of_int i) in
            let arg = PId([tmp_name], wattr) in
            let fixed_attr = unitAttr attr in
            state', [StmtVal(LId([tmp_name],Typ.makeListOpt wattr.typ,wattr),None,battr);StmtBind(LWild(fixed_attr),PCall(inst,name,args@[arg],fixed_attr),battr)]
         (* special case _ = a when a is not simple value *)
         | StmtBind(LWild(wattr), e,battr) when not (Typ.isSimpleOpType wattr.typ) ->
            let i,state' = Env.tick state in
            let tmp_name = "_unused_" ^ (string_of_int i) in
            state', [StmtVal(LId([tmp_name],Typ.makeListOpt wattr.typ,wattr),None,battr);StmtBind(LId([tmp_name],Typ.makeListOpt wattr.typ,wattr), e,battr)]
         | StmtBind(_,PCall(_, _, _, _), _) ->
            state, [stmt]
         | StmtVal(LId(lhs,Some(typ),lattr),Some(PCall(inst,name,args,attr)),battr) when not (Typ.isSimpleType (Typ.first typ)) ->
            let arg = PId(lhs,lattr) in
            let fixed_attr = unitAttr attr in
            state, [StmtVal(LId(lhs,Some(typ),lattr),None,battr);StmtBind(LWild(fixed_attr),PCall(inst,name,args@[arg],fixed_attr),battr)]
         | StmtVal(_,Some(PCall(_, _, _, _)), _) ->
            state, [stmt]
         | StmtReturn(e,attr) ->
            let eattr = GetAttr.fromExp e in
            if not (Typ.isSimpleOpType eattr.typ) then
               let stmt' = StmtBind(LId(["_output_"],Typ.makeListOpt eattr.typ,eattr), e,attr) in
               reapply state, [stmt';StmtReturn(PUnit(unitAttr eattr),attr)]
            else
               state, [stmt]

         | _ -> state, [stmt]


   let mapper = Mapper.{ default_mapper with stmt; stmt_x }


end

module DummySimplifications = struct

   let exp : (PassData.t Env.t,exp) Mapper.mapper_func =
      Mapper.make "DummySimplifications.exp" @@ fun state exp ->
      match exp with
      | PCall(None, ["wrap_array"], [e], _) ->
         let args = (Env.get state).PassData.args in
         if args.code = JSCode || args.code = LuaCode then
            state, e
         else
            state, exp

      | PCall(None, ["not"], [e], attr) ->
         let args = (Env.get state).PassData.args in
         if args.code = JSCode || args.code = LuaCode then
            state, POp("==", [e; PBool(false, attr)], attr)
         else
            state, exp
      | _ -> state, exp

   let stmt : (PassData.t Env.t,stmt) Mapper.mapper_func =
      Mapper.make "DummySimplifications.stmt" @@ fun state stmt ->
      match stmt with
      | StmtVal(LWild(wattr),Some(rhs),attr) ->
         state, StmtBind(LWild(wattr),rhs,attr)
      | StmtIf(cond, then_, Some(StmtBlock(_, [], _)), attr) ->
         state, StmtIf(cond, then_, None, attr)
      | StmtIf(cond, StmtBlock(_, [], _), Some(else_), attr) ->
         let cod_attr = GetAttr.fromExp cond in
         state, StmtIf(PCall(None, ["not"], [cond], cod_attr), else_, None, attr)

      | _ -> state, stmt

   let stmt_x : ('a Env.t,stmt) Mapper.expand_func =
      Mapper.makeExpander "DummySimplifications.stmt_x" @@ fun state stmt ->
      match stmt with
      | StmtVal(LWild(_),None, _) ->
         state, []
      | StmtBind(LId(lhs, _, _), PId(rhs, _), _ ) when compare lhs rhs = 0 ->
         reapply state, []
      | _ -> state, [stmt]

   let mapper = Mapper.{ default_mapper with stmt; stmt_x; exp }

end

let run =
   ReplaceFunctionNames.mapper
   |> Mapper.seq ReturnReferences.mapper
   |> Mapper.seq DummySimplifications.mapper