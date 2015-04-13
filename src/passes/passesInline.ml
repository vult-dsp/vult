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

open TypesVult
open TypesUtil
open PassesUtil

(** Changes appends the given prefix to all named_ids *)
let prefixAllNamedIds : ('data,exp) transformation =
   fun state exp ->
      match exp with
      | PId(name,type_exp,loc) -> state,PId(prefixId state.data name,type_exp,loc)
      | _ -> state,exp

(** inlines the given function call by preparing the assignments and replacing the statements *)
let inlineFunctionCall (state:'data tstate) (call_name:identifier) (ftype:identifier) (args:exp list) loc : exp list =
   let function_def = IdentifierMap.find ftype state.data.functions in
   match function_def with
   | StmtFun(_,fargs,fbody,type_exp,_,_) ->
      let prefix = (joinSep "_" call_name)^"_" in
      let fargs_prefixed = List.map (fun a -> a |> getNameFromNamedId |> prefixId prefix) fargs in
      let prefix_state = deriveState state prefix in
      let _,fbody_prefixed = TypesUtil.traverseBottomExp None prefixAllNamedIds prefix_state fbody in
      let new_decl = List.map (fun a-> StmtVal(PId(a,None,loc),None,loc)) fargs_prefixed in
      let new_assignments = List.map2 (fun a b -> StmtBind(PId(a,None,loc),b,loc)) fargs_prefixed args in
      [appendBlocks (new_decl@new_assignments@[fbody_prefixed])]
   | _ -> failwith "inlineFunctionCall: Invalid function declaration"

(** Main traverser/expander to inline function calls *)
let inlineStmts : ('data,exp) expander =
   fun state exp ->
      match exp with
      | PCall(optname,fname,args,loc,_) ->
         begin
            match optname,(lookupFunctionName state.data.functions state fname) with
            | None,_   ->
               state,[exp]
            | _,None   ->
               state,[exp]
            | Some(name),Some(full_name) ->
               let weight = IdentifierMap.find full_name state.data.function_weight in
               if weight > state.data.options.inline_weight then
                  state,[exp]
               else begin
                  let new_stmts = inlineFunctionCall state name full_name args loc in
                  match new_stmts with
                  | [] -> state,[]
                  | _  -> state,[PSeq(None,new_stmts,loc)]
               end
         end
      | _ -> state,[exp]

(** Inlines functions into functions *)
let inlineFunctionBodies (state:'data tstate) (exp_list:exp list) : 'data tstate * exp list =
   let inlineFunctionBody name fun_decl (functions,weigths) =
      match fun_decl with
      | StmtFun(fname,fargs,fbody,type_exp,active,loc) ->
         let _,new_fbody     = expandStmt None inlineStmts state fbody in
         let new_fbody_block = appendBlocks new_fbody in
         let weight          = getExpWeight new_fbody_block in
         let new_functions   = IdentifierMap.add name (StmtFun(fname,fargs,new_fbody_block,type_exp,active,loc)) functions in
         let new_weigths     = IdentifierMap.add name weight weigths in
         new_functions,new_weigths
      | _ -> functions,weigths
   in
   let new_functions,new_weigths =
      IdentifierMap.fold inlineFunctionBody state.data.functions (IdentifierMap.empty,IdentifierMap.empty) in
   let new_state =
      { state.data with
        functions = new_functions;
        function_weight = new_weigths;
      }
   in (setState state new_state),exp_list


(* Inlining *)
let inliningPasses state =
   state
   |+> TypesUtil.foldAsTransformation None collectFunctionDefinitions
   |+> inlineFunctionBodies
   |+> TypesUtil.expandStmtList (Some(skipFun)) inlineStmts
   |+> TypesUtil.foldAsTransformation None collectFunctionDefinitions

