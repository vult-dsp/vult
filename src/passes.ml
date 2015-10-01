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
open Env

module ConstantSimplification = struct
   let biOpReal (op:string) : float -> float -> float =
      match op with
      | "+" -> ( +. )
      | "-" -> ( -. )
      | "*" -> ( *. )
      | "/" -> ( /. )
      | _ -> failwith (Printf.sprintf "biOpReal: Unknown operator %s" op)

   let biOpInt (op:string) : int -> int -> int =
      match op with
      | "+" -> ( + )
      | "-" -> ( - )
      | "*" -> ( * )
      | "/" -> ( / )
      | _ -> failwith (Printf.sprintf "biOpReal: Unknown operator %s" op)

   let exp : ('a,exp) Mapper.mapper_func =
      fun state e ->
         match e with
         | PBinOp(op,PInt(v1,_),PInt(v2,_),attr) ->
            let f = biOpInt op in
            state,PInt(f v1 v2,attr)
         | PBinOp(op,PInt(v1,_),PReal(v2,_),attr) ->
            let f = biOpReal op in
            state,PReal(f (float_of_int v1) v2,attr)
         | PBinOp(op,PReal(v1,_),PInt(v2,_),attr) ->
            let f = biOpReal op in
            state,PReal(f v1 (float_of_int v2),attr)
         | PBinOp(op,PReal(v1,_),PReal(v2,_),attr) ->
            let f = biOpReal op in
            state,PReal(f v1 v2,attr)
         | _ -> state,e

   let mapper =
      { Mapper.default_mapper with Mapper.exp = exp }
end


module CollectContext = struct

   let stmt : (env,stmt) Mapper.mapper_func =
      fun state stmt ->
         match stmt with
         | StmtFun(name,_,_,_,attr) when attr.fun_and ->
            let env =
               {
                  state with
                  context = FunctionContex.addToCurrent state.context name
               }
            in
            env,stmt
         | StmtFun(name,_,_,_,attr) ->
            let env =
               {
                  state with
                  context = FunctionContex.addToNew state.context name
               }
            in
            env,stmt
         | _ -> state,stmt

   let mapper =
      { Mapper.default_mapper with Mapper.stmt = stmt }

end


(* Basic transformations *)
let basicPasses (state,stmts) =
   let basic_mapper =
      Mapper.seq CollectContext.mapper ConstantSimplification.mapper
   in
   Mapper.map_stmt_list basic_mapper state stmts

let applyTransformations (results:parser_results) =
   let initial_state = Env.empty in

   let passes stmts =
      (initial_state,[StmtBlock(None,stmts,makeAttr Loc.default)])
      |> basicPasses
      |> snd
   in

   let new_stmts = CCError.map passes results.presult in
   { results with presult = new_stmts }
