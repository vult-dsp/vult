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
open Maps

module CollectTuples = struct
   let vtype_c : ('a Env.t, Typ.vtype) Mapper.mapper_func =
      Mapper.make "CollectTuples.vtype_c"
      @@ fun state t ->
      match t with
      | Typ.TComposed ([ "tuple" ], _, _) ->
         let data = Env.get state in
         let data' = PassData.addTuple data (ref t) in
         Env.set state data', t
      | _ -> state, t
   ;;

   let mapper = Mapper.{ default_mapper with vtype_c }
end

module ReportUnsupportedTypes = struct
   let exp : ('a Env.t, exp) Mapper.mapper_func =
      Mapper.make "ReportUnsupportedTypes.exp"
      @@ fun state exp ->
      let attr = GetAttr.fromExp exp in
      match attr.typ with
      | Some t when Typ.isUnbound t ->
         let msg = Printf.sprintf "The type of this expression could not be inferred. Add a type annotation." in
         Error.raiseError msg attr.loc
      | _ -> state, exp
   ;;

   let mapper = Mapper.{ default_mapper with exp }
end

module SimplifyFixed = struct
   (* Creates a multiplication with the given terms if
      the list has more than two terms
   *)
   let makeMult elems attr =
      match elems with
      | [] -> failwith "SimplifyFixed.makeMult: invalid input"
      | [ e ] -> e
      | _ -> POp ("*", elems, attr)
   ;;

   let fixSign sign (e : exp) : exp =
      if sign
      then (
         match e with
         | PInt (value, attr) -> PInt (-value, attr)
         | PReal (value, p, attr) -> PReal (-.value, p, attr)
         | _ -> PUnOp ("-", e, GetAttr.fromExp e))
      else e
   ;;

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
   ;;

   let rec isPowerOfTwo (n : int) (value : float) : int option =
      if n > 16384 then None else if value = float_of_int n then Some (powers n) else isPowerOfTwo (2 * n) value
   ;;

   let isNum (e : exp) : bool =
      match e with
      | PInt _ | PReal _ | PBool _ -> true
      | _ -> false
   ;;

   let rec find is_fixed e acc attr =
      match e with
      | [] -> false, makeMult acc attr
      (* multiply / divide by an int power of two *)
      | (PInt (n, iattr) as h) :: t ->
         let sign = n < 0 in
         let nn = abs n in
         (match isPowerOfTwo 2 (float_of_int nn) with
          | Some p -> true, fixSign sign (POp ("<<", [ makeMult (acc @ t) attr; PInt (p, iattr) ], attr))
          | None -> find is_fixed t (h :: acc) attr)
      (* multiply / divide by an int power of two *)
      | (PReal (n, p, iattr) as h) :: t ->
         if is_fixed || p = Fix16
         then (
            let sign = n < 0.0 in
            let div = abs_float n < 1.0 in
            let nn = if div then 1.0 /. abs_float n else abs_float n in
            match isPowerOfTwo 2 nn with
            | Some p ->
               let op = if div then ">>" else "<<" in
               true, fixSign sign (POp (op, [ makeMult (acc @ t) attr; PInt (p, iattr) ], attr))
            | None -> find is_fixed t (h :: acc) attr)
         else find is_fixed t (h :: acc) attr
      | h :: t -> find is_fixed t (h :: acc) attr
   ;;

   let exp : ('a Env.t, exp) Mapper.mapper_func =
      Mapper.make "Simplify.exp"
      @@ fun state exp ->
      match exp with
      | POp ("*", elems, attr) ->
         let data = Env.get state in
         let args = data.PassData.args in
         if args.code <> LuaCode
         then (
            let is_fixed = args.Args.real = "fixed" in
            let constants, other = List.partition isNum elems in
            let found, new_exp = find is_fixed constants other attr in
            if found then reapply state, new_exp else state, exp)
         else state, exp
      | _ -> state, exp
   ;;

   let mapper = Mapper.{ default_mapper with exp }
end

module MarkUsedFunctions = struct
   module MarkAllCalls = struct
      let exp : ('a Env.t, exp) Mapper.mapper_func =
         Mapper.make "MarkUsedFunctions.MarkAllCalls.exp"
         @@ fun state exp ->
         match exp with
         | PCall (_, name, _, _) ->
            let data = Env.get state in
            let data = PassData.markAsUsed data name NotRoot in
            let state = { state with data } in
            state, exp
         | _ -> state, exp
      ;;

      let mapper = Mapper.{ default_mapper with exp }
   end

   let markType state name root =
      try
         let (Id.Path ctx) = Env.getContext state name in
         let data = PassData.markAsUsed (Env.get state) ctx root in
         let state = { state with data } in
         state
      with
      | Not_found -> state
   ;;

   let stmt : (PassData.t Env.t, stmt) Mapper.mapper_func =
      Mapper.make "MarkUsedFunctions.stmt"
      @@ fun state stmt ->
      match stmt with
      | StmtFun (name, args, body, ret, attr) ->
         let data = Env.get state in
         (match IdMap.find name data.PassData.used_code with
          | Keep _ | NotUsed -> state, stmt
          | Used root ->
             let state, body = Mapper.map_stmt MarkAllCalls.mapper state body in
             let data = PassData.markToKeep (Env.get state) name root in
             let state = { state with data } in
             let state = markType state name root in
             reapply state, StmtFun (name, args, body, ret, { attr with used = Keep root })
          | exception Not_found ->
             (match Env.getContext state name with
              | Id.Path ctx ->
                 (match IdMap.find ctx data.PassData.used_code with
                  | Used used ->
                     let state, body = Mapper.map_stmt MarkAllCalls.mapper state body in
                     let data = PassData.markToKeep (Env.get state) name used in
                     let state = { state with data } in
                     reapply state, StmtFun (name, args, body, ret, { attr with used = Keep used })
                  | _ -> state, stmt
                  | exception Not_found -> state, stmt)
              | exception Not_found -> state, stmt))
      | StmtType (({ contents = TId (name, _) } as lhs), rhs, attr) ->
         let data = Env.get state in
         (match IdMap.find name data.PassData.used_code with
          | Keep _ | NotUsed -> state, stmt
          | Used root ->
             (*let state, body = Mapper.map_stmt MarkAllCalls.mapper state body in
               let data  = PassData.markToKeep (Env.get state) name root in
               let state = { state with data } in*)
             let data = PassData.markToKeep (Env.get state) name root in
             let state = { state with data } in
             reapply state, StmtType (lhs, rhs, { attr with used = Keep root })
          | exception Not_found -> state, stmt)
      | StmtConst (lhs, rhs, ({ fun_src = Some fname } as attr)) ->
         let data = Env.get state in
         (match IdMap.find fname data.PassData.used_code with
          | Used _ | Keep _ -> state, StmtConst (lhs, rhs, { attr with used = Keep NotRoot })
          | NotUsed -> state, stmt
          | exception Not_found -> state, stmt)
      | _ -> state, stmt
   ;;

   let mapper = Mapper.{ default_mapper with stmt }
end

module MakeMac = struct
   let isRealAttr attr : bool =
      match attr.typ with
      | None -> failwith "the type is not defined"
      | Some t -> Typ.isRealType t
   ;;

   let exp : ('a Env.t, exp) Mapper.mapper_func =
      Mapper.make "MakeMac.exp"
      @@ fun state exp ->
      let data = Env.get state in
      if data.PassData.args.mac
      then (
         match exp with
      (*
            | POp("+",[PUnOp("-", c, _); POp("*",[a; b], _)], attr) when isRealAttr attr ->
            reapply state, PCall(NoInst,["msu"],[c; a; b], attr)
            | POp("+",[POp("*",[a; b], _); PUnOp("-", c, _)], attr) when isRealAttr attr ->
            reapply state, PCall(NoInst,["msu"],[c; a; b], attr)

            | POp("+", PUnOp("-", c, _) :: POp("*",[a; b], _) :: rest, attr) when isRealAttr attr ->
            reapply state, POp("+", PCall(NoInst,["msu"],[c; a; b], attr) :: rest, attr)
            | POp("+", POp("*",[a; b], _) :: PUnOp("-", c, _) :: rest, attr) when isRealAttr attr ->
            reapply state, POp("+", PCall(NoInst,["msu"],[c; a; b], attr) :: rest, attr)
         *)
         | POp ("+", c :: POp ("*", [ a; b ], _) :: rest, attr) when isRealAttr attr ->
            reapply state, POp ("+", PCall (NoInst, [ "mac" ], [ c; a; b ], attr) :: rest, attr)
         | POp ("+", POp ("*", [ a; b ], _) :: c :: rest, attr) when isRealAttr attr ->
            reapply state, POp ("+", PCall (NoInst, [ "mac" ], [ c; a; b ], attr) :: rest, attr)
         | POp ("+", [ c; POp ("*", [ a; b ], _) ], attr) when isRealAttr attr ->
            reapply state, PCall (NoInst, [ "mac" ], [ c; a; b ], attr)
         | POp ("+", [ POp ("*", [ a; b ], _); c ], attr) when isRealAttr attr ->
            reapply state, PCall (NoInst, [ "mac" ], [ c; a; b ], attr)
         | _ -> state, exp)
      else state, exp
   ;;

   let mapper = Mapper.{ default_mapper with exp }
end

module SortExp = struct
   let exp : ('a Env.t, exp) Mapper.mapper_func =
      Mapper.make "MakeMac.exp"
      @@ fun state exp ->
      match exp with
      | POp ((("+" | "*") as op), args, attr) -> state, POp (op, List.fast_sort compare_exp args, attr)
      | _ -> state, exp
   ;;

   let mapper = Mapper.{ default_mapper with exp }
end

let run =
   CollectTuples.mapper
   |> Mapper.seq ReportUnsupportedTypes.mapper
   |> Mapper.seq SimplifyFixed.mapper
   |> Mapper.seq MarkUsedFunctions.mapper
   |> Mapper.seq SortExp.mapper
   |> Mapper.seq MakeMac.mapper
;;
