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

open Env
open Prog
open Maps

module GetIdentifiers = struct

   let lhs_exp : ('a Env.t, lhs_exp) Mapper.mapper_func =
      Mapper.make "GetIdentifiers.lhs_exp" @@ fun state exp ->
      match exp with
      | LId(id, _, _) ->
         Env.set state (IdSet.add id (Env.get state)), exp
      | _ -> state, exp

   let exp : ('a Env.t, exp) Mapper.mapper_func =
      Mapper.make "GetIdentifiers.exp" @@ fun state exp ->
      match exp with
      | PId(id, _) ->
         Env.set state (IdSet.add id (Env.get state)), exp
      | _ -> state, exp

   let mapper = { Mapper.default_mapper with Mapper.exp = exp; Mapper.lhs_exp = lhs_exp }

   let dummy_env = Env.empty IdSet.empty

   let fromExp (exp:exp) : IdSet.t =
      let s, _ = Mapper.map_exp mapper dummy_env exp in
      Env.get s

   let fromExpList (exp:exp list) : IdSet.t =
      let s, _ = Mapper.map_exp_list mapper dummy_env exp in
      Env.get s

   let fromLhsExp (exp:lhs_exp) : IdSet.t =
      let s, _ = Mapper.map_lhs_exp mapper dummy_env exp in
      Env.get s

   let fromLhsExpList (exp:lhs_exp list) : IdSet.t =
      let s, _ = Mapper.map_lhs_exp_list mapper dummy_env exp in
      Env.get s

end

module GetLocation = struct

   let attr : (Loc.t Env.t, attr) Mapper.mapper_func =
      Mapper.make "GetLocation.attr" @@ fun state attr ->
      let s = Env.get state in
      Env.set state (Loc.merge s attr.loc), attr

   let mapper = { Mapper.default_mapper with Mapper.attr = attr }

   let fromExp (e:exp) : Loc.t =
      let dummy_env = Env.empty Loc.default in
      let s, _ = Mapper.map_exp mapper dummy_env e in
      Env.get s

   let fromStmt (stmt:stmt) : Loc.t =
      let dummy_env = Env.empty Loc.default in
      let s, _ = Mapper.map_stmt mapper dummy_env stmt in
      Env.get s

   let fromLhsExp (e:lhs_exp) : Loc.t =
      let dummy_env = Env.empty Loc.default in
      let s, _ = Mapper.map_lhs_exp mapper dummy_env e in
      Env.get s

   let fromType (e:Typ.t) : Loc.t = Typ.location e

end

module GetAttr = struct

   let fromExp (e:exp) : attr =
      match e with
      | PUnit(attr)
      | PBool(_, attr)
      | PInt(_, attr)
      | PReal(_, attr)
      | PString(_, attr)
      | PId(_, attr)
      | PIndex(_, _, attr)
      | PArray(_, attr)
      | PUnOp(_, _, attr)
      | POp(_, _, attr)
      | PCall(_, _, _, attr)
      | PIf(_, _, _, attr)
      | PGroup(_, attr)
      | PTuple(_, attr)
      | PSeq(_, _, attr) -> attr
      | PEmpty -> emptyAttr

   let fromLhsExp (e:lhs_exp) : attr =
      match e with
      | LWild(attr)
      | LId(_, _, attr)
      | LTuple(_, attr)
      | LTyped(_, _, attr)
      | LGroup(_, attr) -> attr
      | LIndex(_, _, _, attr) -> attr

end


module GetDependencies = struct

   let exp : ('a Env.t, exp) Mapper.mapper_func =
      Mapper.make "GetIdentifiers.exp" @@ fun state exp ->
      match exp with
      | PId([id; _], _) ->
         Env.set state (IdSet.add [id] (Env.get state)), exp
      | PCall(_, [id; _], _, _) ->
         Env.set state (IdSet.add [id] (Env.get state)), exp
      | _ -> state, exp

   let mapper = Mapper.{ default_mapper with exp }

   let fromStmts (stmts:stmt list) : IdSet.t =
      let s, _ = Mapper.map_stmt_list mapper (Env.empty IdSet.empty) stmts in
      Env.get s

end