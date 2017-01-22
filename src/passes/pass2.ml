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
open VEnv
open TypesVult

let getInterpEnv state =
   let env = (Env.get state).PassData.interp_env in
   match Env.currentScope state with
   | Path([]) -> env
   | Path(module_::_) -> Interpreter.Env.enterModule env [module_]

module Evaluate = struct

   let isConst exp =
      match exp with
      | PInt _ | PReal _ | PBool _ -> true
      | _ -> false

   let exp : ('a Env.t,exp) Mapper.mapper_func =
      Mapper.make "Simplify.exp" @@ fun state exp ->
      match exp with
      | PCall(None,_,args,_) when List.for_all isConst args ->
         let env = getInterpEnv state in
         let exp' = Interpreter.evalExp env exp in
         state, exp'
      (*| PReal(v, attr) ->
         begin
            match classify_float v with
            | FP_normal -> state, exp
            | FP_subnormal -> state, exp
            | FP_zero -> state, exp
            | FP_infinite -> state, PReal(3.40282347E+38, attr)
            | FP_nan -> failwith "real number evaluates to nan"
         end*)
      | _ -> state, exp

   let mapper = Mapper.{ default_mapper with exp = exp }

end

module MakeTables = struct

   let getFloat x =
      match x with
      | PReal(value,_) -> value
      | _ -> failwith "The result of the evaluation is not a float"

   let makeFloat x =
      PReal(x,emptyAttr)

   let getCoefficients l =
      match l with
      | [x1; x2; x3] -> x1, x2, x3
      | _ -> failwith "the curve fitting returned more than three points"

   let checkRealReturn typ : bool =
      match typ with
      | None -> failwith "the type is not defined"
      | Some(t) -> VType.isRealType t

   let getTableIndividualParams loc args =
      let remaining, found = Attributes.(getParameterList loc args [(["size"],Int); (["min"],Real); (["max"],Real)]) in
      match remaining with
      | _::_ ->
         let params_s =  List.map (fun (id,_) -> PrintTypes.identifierStr id) remaining |> String.concat ", " in
         let msg = "The following arguments are unknown for the current anotation: "^ params_s in
         Error.raiseError msg loc
      | [] ->
         match found with
         | [AInt(size,_); AReal(min,_); AReal(max,_)] ->
            (int_of_string size), (float_of_string min), (float_of_string max)
         | _ ->
            let msg = "The annotation 'table' requires specific parameters. e.g. 'table(size=128,min=0.0,max=1.0)'" in
            Error.raiseError msg loc

   let rec getTableParams (attr:attr_exp list) =
      match attr with
      | [] -> None
      | AFun(name,args,loc)::_ when name = ["table"] ->
         Some(getTableIndividualParams loc args)
      | _::t -> getTableParams t

   let makeTableDecl fname name data =
      let t = VType.Constants.real_type in
      let ta = ref (VType.TComposed(["array"], [t; ref (VType.TInt(List.length data, None))], None)) in
      let attr_array = { emptyAttr with typ = Some(ta) } in
      let varname = [String.concat "_" (fname @ name)] in
      let arr = PArray((List.map makeFloat data |> Array.of_list), emptyAttr) in
      StmtVal(LId(varname, Some(ta), attr_array), Some(arr), { emptyAttr with const = true})

   let evaluateFunction env name x =
      let exp = PCall(None, name, [PReal(x, emptyAttr)], emptyAttr) in
      let value = Interpreter.evalExp env exp in
      getFloat value

   let calculateTables env name size min max =
      let delta = (max -. min) /. (float_of_int (size - 1)) in
      let rec loop index xx acc0 acc1 acc2 =
         if index < 0 then
            [
               makeTableDecl name ["x0_"] xx;
               makeTableDecl name ["c0_"] acc0;
               makeTableDecl name ["c1_"] acc1;
               makeTableDecl name ["c2_"] acc2
            ]
         else
            let r_index = float_of_int index in
            let xx_point = min +. delta *. r_index in
            let x =
               [ min +. delta *. r_index;
                 min +. delta *. (r_index +. 0.5);
                 min +. delta *. (r_index +. 1.0); ]
            in
            let y = List.map (fun a -> evaluateFunction env name a) x in
            let c0, c1, c2 = Fitting.fit x y |> getCoefficients in
            loop (index-1) (xx_point::xx) (c0::acc0) (c1::acc1) (c2::acc2)
      in
      loop (size - 1) [] [] [] []

   let stmt_x : ('a Env.t,stmt) Mapper.expand_func =
      Mapper.makeExpander "MakeTables.stmt_x" @@ fun state stmt ->
      match stmt with
      | StmtFun(name, args, _body, ret, attr) ->
         begin
            match getTableParams attr.exp, args with
            | None, _ -> state, [stmt]
            | Some(size, min, max), [_] when checkRealReturn ret ->
               let env = getInterpEnv state in
               let result = calculateTables env name size min max in
               state, result @ [stmt]
            | Some _, _::_ ->
               let msg = "This annotation can only be applied to functions of one argument" in
               Error.raiseError msg attr.loc
            | Some _,_ ->
               let msg = "This annotation can only be applied to functions returning 'real'" in
               Error.raiseError msg attr.loc
         end
      | _ -> state, [stmt]

   let mapper = Mapper.{ default_mapper with stmt_x }

end

let run =
   Pass1.Simplify.mapper
   |> Mapper.seq Evaluate.mapper
   |> Mapper.seq MakeTables.mapper
