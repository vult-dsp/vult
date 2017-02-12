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

   let int_type = VType.Constants.int_type
   let real_type = VType.Constants.real_type
   let attr_int = { emptyAttr with typ = Some(int_type) }
   let attr_real = { emptyAttr with typ = Some(real_type) }

   let array_type size =
      ref (VType.TComposed(["array"], [real_type; ref (VType.TInt(size, None))], None))

   let attr_array size =
      { emptyAttr with typ = Some(array_type size) }

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

   let rec removeTableParams (attr:attr_exp list) =
      match attr with
      | [] -> []
      | AFun(name,_,_)::t when name = ["table"] ->
         removeTableParams t
      | h::t -> h :: (removeTableParams t)

   let getInputVar arg =
      match arg with
      | [SimpleId(id,_,_)]
      | [TypedId(id,_,_,_)] -> PId(id,attr_real)
      | _ -> failwith "invalid input variable"

   let makeVarName (fname:id) (var:id) : id =
      [String.concat "_" (fname @ var)]

   let makeTableDecl fname name data =
      let varname = makeVarName fname name in
      let arr = PArray((List.map makeFloat data |> Array.of_list), emptyAttr) in
      let size = List.length data in
      StmtVal(LId(varname, Some(array_type size), attr_array size), Some(arr), { emptyAttr with const = true})

   let makeNewBody fname size min max input =
      let lindex = LId(["index"],Some(int_type), attr_int) in
      let rindex = PId(["index"], attr_int) in
      let getCoeff a =
         PCall(None,["get"], [PCall(None,["wrap_array"], [PId(makeVarName fname [a], attr_array size)], attr_real); rindex], attr_real)
      in
      let initial_index = PReal(((float_of_int size) -. 1.0) /. (max -. min),attr_real) in
      StmtBlock(
         None,
         [
            StmtVal(lindex,None,emptyAttr);
            StmtBind(lindex,
                     PCall(None,["clip"],
                           [
                              PCall(None,["int"],
                                    [POp("*",
                                         [
                                            initial_index;
                                            POp("-",[input; PReal(min,attr_real)],attr_real)
                                         ],attr_real)],attr_int);
                              PInt(0,attr_int);
                              PInt(size-1,attr_int);
                           ],attr_int),
                     emptyAttr);
            StmtReturn(
               POp("+",
                   [getCoeff "c0";
                    POp("*",[input;
                             (POp("+",
                                  [getCoeff "c1";
                                   POp("*",
                                       [getCoeff "c2";input],attr_real)],attr_real))],
                        attr_real)],attr_real),emptyAttr)
         ], emptyAttr)

   let evaluateFunction env (name:id) (x:float) =
      match name with
      | [_; fname] ->
         let exp = PCall(None, [fname], [PReal(x, emptyAttr)], emptyAttr) in
         let value = Interpreter.evalExp env exp in
         getFloat value
      | _ -> failwith "evaluateFunction: the function should be a full path"

   let calculateTables env name size min max =
      let map x x0 x1 y0 y1 = (x -. x0) *. (y1 -. y0) /. (x1 -. x0) +. y0 in
      let map_x x = map x 0. (float_of_int size) min max in
      let rec loop index xx acc0 acc1 acc2 =
         if index < 0 then
            [
               makeTableDecl name ["c0"] acc0;
               makeTableDecl name ["c1"] acc1;
               makeTableDecl name ["c2"] acc2
            ]
         else
            let r_index = float_of_int index in
            let xx_point = map_x r_index in
            let x =
               [ xx_point;
                 map_x (r_index +. 0.5);
                 map_x (r_index +. 1.0); ]
            in
            let y = List.map (fun a -> evaluateFunction env name a) x in
            let c0, c1, c2 = Fitting.fit x y |> getCoefficients in
            loop (index-1) (xx_point::xx) (c0::acc0) (c1::acc1) (c2::acc2)
      in
      loop size [] [] [] []

   let stmt_x : ('a Env.t,stmt) Mapper.expand_func =
      Mapper.makeExpander "MakeTables.stmt_x" @@ fun state stmt ->
      match stmt with
      | StmtFun(name, args, _, ret, attr) ->
         begin
            match getTableParams attr.exp, args with
            | None, _ -> state, [stmt]
            | Some(size, min, max), [_] when checkRealReturn ret ->
               let env    = getInterpEnv state in
               let Path(path) = Env.currentScope state in
               let full_path = path@name in
               let result = calculateTables env full_path size min max in
               let attr'  = { attr with exp = removeTableParams attr.exp } in
               let var    = getInputVar args in
               let body'  = makeNewBody full_path size min max var in
               reapply state, result @ [StmtFun(name, args, body', ret, attr')]
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
