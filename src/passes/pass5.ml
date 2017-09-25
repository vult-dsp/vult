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
      | Typ.TComposed(["tuple"], _, _) ->
         let data       = Env.get state in
         let data' = PassData.addTuple data (ref t) in
         Env.set state data', t
      | _ -> state, t

   let mapper = Mapper.{ default_mapper with vtype_c }

end

module ReportUnsupportedTypes = struct

   let reportUnsupportedArray (typ:Typ.t) (name:Id.t) (attr:attr) =
      let msg = Printf.sprintf "The type '%s' of variable '%s' is not supported. Arrays can only contain basic types." (PrintProg.typeStr typ) (Id.show name) in
      Error.raiseError msg attr.loc

   let isComplexArray typ =
      if Typ.isArray typ then
         let t, _ = Typ.arrayTypeAndSize typ in
         not (Typ.isSimpleType t)
      else false

   let lhs_exp : ('a Env.t, lhs_exp) Mapper.mapper_func =
      Mapper.make "ReportUnsupportedTypes.lhs_exp" @@ fun state exp ->
      match exp with
      | LId(id,Some t,attr) when isComplexArray (Typ.first t) ->
         reportUnsupportedArray (Typ.first t) id attr
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
      | TypedId(id, typ, _,attr) when isComplexArray (Typ.first typ) ->
         reportUnsupportedArray (Typ.first typ) id attr
      | _ -> state, t

   let mapper = Mapper.{ default_mapper with lhs_exp; exp; typed_id; }

end

module SimplifyFixed = struct

   (* Creates a multiplication with the given terms if
      the list has more than two terms
   *)
   let makeMult elems attr =
      match elems with
      | [] -> failwith "SimplifyFixed.makeMult: invalid input"
      | [e] -> e
      | _ -> POp("*", elems, attr)

   let fixSign sign (e:exp) : exp =
      if sign then
         match e with
         | PInt(value,attr) -> PInt(-value,attr)
         | PReal(value,attr) -> PReal(-.value,attr)
         | _ -> PUnOp("-", e,GetAttr.fromExp e)
      else e

   let powers n =
      match n with
      | 2 -> 1
      | 4 -> 2
      | 8 -> 3
      | 16 -> 4
      | 32 -> 5
      | 64 -> 6
      | 128 -> 7
      | 256 -> 8
      | 512 -> 9
      | 1024 -> 10
      | 2048 -> 11
      | 4096 -> 12
      | 8192 -> 13
      | 16384 -> 14
      | _ -> failwith "invalid input"

   let rec isPowerOfTwo (n:int) (value:float) : int option =
      if n > 16384 then None
      else
      if value = (float_of_int n) then
         Some (powers n)
      else isPowerOfTwo (2 * n) value

   let isNum (e:exp) : bool =
      match e with
      | PInt _
      | PReal _
      | PBool _ -> true
      | _ -> false

   let rec find e acc attr =
      match e with
      | [] -> false, makeMult acc attr
      (* multiply / divide by an int power of two *)
      | (PInt(n, iattr) as h) :: t ->
         let sign = n < 0 in
         let nn = abs n in
         begin match isPowerOfTwo 2 (float_of_int nn) with
            | Some p -> true, fixSign sign (POp("<<", [(makeMult (acc @ t) attr); PInt(p, iattr)], attr))
            | None -> find t (h::acc) attr
         end
      (* multiply / divide by an int power of two *)
      | (PReal(n, iattr) as h) :: t ->
         let sign = n < 0.0 in
         let div = abs_float n < 1.0 in
         let nn = if div then 1.0 /. (abs_float n) else abs_float n in
         begin match isPowerOfTwo 2 nn with
            | Some p ->
               let op = if div then ">>" else "<<" in
               true, fixSign sign (POp(op, [(makeMult (acc @ t) attr); PInt(p, iattr)], attr))
            | None -> find t (h::acc) attr
         end
      | h :: t -> find t (h::acc) attr

   let exp : ('a Env.t,exp) Mapper.mapper_func =
      Mapper.make "Simplify.exp" @@ fun state exp ->
      let data = Env.get state in
      let args = data.PassData.args in
      if args.Args.real = "fixed" then
         match exp with
         | POp("*", elems, attr) ->
            let constants, other = List.partition isNum elems in
            let found, new_exp = find constants other attr in
            if found then
               reapply state, new_exp
            else
               state, exp
         | _ -> state, exp
      else
         state, exp


   let mapper = Mapper.{ default_mapper with exp }

end

let run =
   CollectTuples.mapper
   |> Mapper.seq ReportUnsupportedTypes.mapper
   |> Mapper.seq SimplifyFixed.mapper