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
open TypesUtil
open PassesUtil
open PassesCode
open PassesBasic
open PassesInline
open PassesSequence

let applyOn cond f data =
   if cond data then
      f data
   else
      data

let returnRemovalON (state,_) = state.data.options.simplify_return
let inlineON        (state,_) = state.data.options.inline
let finalizeON      (state,_) = state.data.options.finalize
let basicON         (state,_) = state.data.options.basic
let codegenOn       (state,_) = state.data.options.codegen
let interpreterOn   (state,_) = state.data.options.interpreter

let applyTransformations (options:options) (results:parser_results) =
   let module_name = Filename.basename results.file |> Filename.chop_extension in
   let initial_state =
      {
         counter         = 0;
         functions       = IdentifierMap.empty;
         types           = IdentifierMap.empty;
         function_weight = IdentifierMap.empty;
         options         = options;
         function_mem    = IdentifierMap.empty;
         instances       = IdentifierMap.empty;
         type_function   = IdentifierMap.empty;
         type_mapping    = IdentifierMap.empty;
      } |> createState
   in

   let passes stmts =
      (initial_state,[StmtBlock(None,stmts,default_loc)])
      |> applyOn basicON         basicPasses
      |> applyOn returnRemovalON removalOfSequencesPasses
      |> applyOn inlineON        inliningPasses
      |> applyOn finalizeON      (finalPasses module_name)
      |> applyOn codegenOn       codeGenPasses
      |> applyOn interpreterOn   interpreterPasses
      |> snd
   in

   let new_stmts = CCError.map passes results.presult in
   { results with presult = new_stmts }

