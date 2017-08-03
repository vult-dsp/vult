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


module CollectTuples = struct

   let vtype_c : ('a Env.t, Typ.vtype) Mapper.mapper_func =
      Mapper.make "CollectTuples.vtype_c" @@ fun state t ->
      match t with
      | Typ.TComposed(["tuple"],_,_) ->
         let data       = Env.get state in
         let data' = PassData.addTuple data (ref t) in
         Env.set state data', t
      | _ -> state, t

   let mapper = Mapper.{ default_mapper with vtype_c }

end

module ReportUnsupportedTypes = struct

   let reportUnsupportedArray (typ:Typ.t) (name:Id.t) (attr:attr) =
      let msg = Printf.sprintf "The type '%s' of variable '%s' is not supported. Arrays can only contain basic types." (PrintProg.typeStr typ) (idStr name) in
      Error.raiseError msg attr.loc

   let isComplexArray typ =
      if Typ.isArray typ then
         let t, _ = Typ.arrayTypeAndSize typ in
         not (Typ.isSimpleType t)
      else false

   let lhs_exp : ('a Env.t,lhs_exp) Mapper.mapper_func =
      Mapper.make "ReportUnsupportedTypes.lhs_exp" @@ fun state exp ->
      match exp with
      | LId(id,Some(t),attr) when isComplexArray t ->
         reportUnsupportedArray t id attr
      | _ -> state, exp

   let exp : ('a Env.t,exp) Mapper.mapper_func =
      Mapper.make "ReportUnsupportedTypes.exp" @@ fun state exp ->
      let attr = GetAttr.fromExp exp in
      match attr.typ with
      | Some(t) when Typ.isUnbound t ->
         let msg = Printf.sprintf "The type of this expression could not be inferred. Add a type annotation." in
         Error.raiseError msg (attr.loc)
      | Some(t) when isComplexArray t ->
         let msg = Printf.sprintf "The type '%s' of this expression is not supported. Arrays can only contain basic types." (PrintProg.typeStr t) in
         Error.raiseError msg attr.loc
      | _ ->
         state, exp

   let typed_id : ('a Env.t,typed_id) Mapper.mapper_func =
      Mapper.make "ReportUnsupportedTypes.typed_id" @@ fun state t ->
      match t with
      | TypedId(id,typ,_,attr) when isComplexArray typ ->
         reportUnsupportedArray typ id attr
      | _ -> state, t

   let mapper = Mapper.{ default_mapper with lhs_exp; exp; typed_id; }

end

let run =
   CollectTuples.mapper
   |> Mapper.seq ReportUnsupportedTypes.mapper