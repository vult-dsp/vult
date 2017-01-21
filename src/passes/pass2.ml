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

module Evaluate = struct

   let isConst exp =
      match exp with
      | PInt _ | PReal _ | PBool _ -> true
      | _ -> false

   let getInterpEnv state =
      let env = (Env.get state).PassData.interp_env in
      match Env.currentScope state with
      | Path([]) -> env
      | Path(module_::_) -> Interpreter.Env.enterModule env [module_]

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

let run =
   Pass1.Simplify.mapper
   |> Mapper.seq Evaluate.mapper