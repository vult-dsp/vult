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


(** Negates a condition*)
let notCondition (exp:exp) =
   match exp with
   | PCall(_,["not"],[exp],_,_) -> exp
   | _ ->
      let loc = getExpLocation exp in
      PCall(None,["not"],[exp],loc,[])

(** Splits if statemtents containing else in order to apply simplifications. e.g. if(a) stmt1; else stmt2; -> if(a) stmt1; if(!a) stmt2; *)
let splitIfWithTwoReturns : ('data,exp) expander =
   fun state exp ->
      match exp with
      | StmtIf(cond,then_exp,Some(else_exp),loc) ->
         begin
            match hasReturn then_exp, hasReturn else_exp with
            | false,false -> state,[StmtIf(cond,then_exp,Some(else_exp),loc)]
            | true,false  -> state,(StmtIf(cond,then_exp,None,loc)::[else_exp])
            | false,true  -> state,(StmtIf(notCondition cond,else_exp,None,loc)::[then_exp])
            | true,true   -> state,[StmtIf(cond,then_exp,None,loc);StmtIf(notCondition cond,else_exp,None,loc)]
         end
      | _ -> state,[exp]

(** Wrapps changes return a; -> if(true) return a; This is useful to apply transformations *)
let wrapSimpleReturn : ('data,exp) traverser =
   fun state stmt ->
      match stmt with
      | StmtReturn(e,loc) -> state,StmtIf(PBool(true,loc),StmtReturn(e,loc),None,loc)
      | _ -> state,stmt

(** Returns true if there is a return statement inside an if expression *)
let hasIfStmtWithReturnList (stmts:exp list) : bool =
   let fold_function state stmt =
      match stmt with
      | StmtIf(_,then_exp,None,_)  ->
         setState state (state.data || hasReturn then_exp)
      | StmtIf(_,then_exp,Some(else_exp),_)  ->
         setState state (state.data || hasReturn then_exp || hasReturn else_exp)
      | _ -> state
   in
   foldDownExpList (Some(skipPSeq)) fold_function (createState false) stmts
   |> getState

(** Replaces the return statements by bindings to the return variable *)
let rec replaceReturn ret_var stmts =
   let replace e = match e with | StmtReturn(e,loc) -> StmtBind(ret_var,e,loc) | _ -> e in
   match stmts with
   | StmtBlock(name,block_stmts,loc)  -> StmtBlock(name,List.map replace block_stmts,loc)
   | _ -> replace stmts

(** Main transformation that eliminates the returns (assumes that return -> if(true) goto :end_of_function ) *)
let rec simplifyReturnPaths (ret_var:exp) (stmts:exp list) : exp list =
   match stmts with
   | [] -> []
   | StmtIf(cond,then_stmts,None,loc)::[] when hasReturn then_stmts ->
      [StmtIf(cond,replaceReturn ret_var then_stmts,None,loc)]
   | StmtIf(cond,then_stmts,None,loc)::t when hasReturn then_stmts ->
      let new_t = simplifyReturnPaths ret_var t in
      [StmtIf(notCondition cond,StmtBlock(None,new_t,loc),Some(replaceReturn ret_var then_stmts),loc)]
   | h::t -> h::(simplifyReturnPaths ret_var t)

(** Transforms if(a){} if(!a){} -> if(a) {} else {} *)
let rec collapseUnnecessaryIf (stmts:exp list) : exp list =
   match stmts with
   | [] -> []
   | StmtIf(cond1,StmtIf(cond2,then_stmt2,_,loc2),else_stmt,loc1)::t
      when compareExp cond1 cond2 = 0 ->
      collapseUnnecessaryIf (StmtIf(cond1,then_stmt2,else_stmt,loc1)::t)
   | StmtIf(cond1,then_stmt1,None,loc1)::StmtIf(cond2,then_stmt2,None,loc2)::t
      when compareExp (notCondition cond1) cond2 = 0 ->
      collapseUnnecessaryIf (StmtIf(cond1,then_stmt1,Some(then_stmt2),loc1)::t)
   | h::t -> h::(collapseUnnecessaryIf t)

(** Applies the return elimination to each if-statement *)
let simplifyReturn : ('data,exp) traverser =
   fun state stmt ->
      match stmt with
      | StmtIf(cond,then_stmt,Some(else_stmt),loc)
         when hasReturn then_stmt || hasReturn else_stmt ->
         let new_then =
            simplifyReturnPaths state.data (expandBlockOrSeq then_stmt)
            |> appendBlocksList |> fst
            |> collapseUnnecessaryIf
         in
         let new_else =
            simplifyReturnPaths state.data (expandBlockOrSeq else_stmt)
            |> appendBlocksList |> fst
            |> collapseUnnecessaryIf
         in
         state,StmtIf(cond,appendBlocks new_then,Some(appendBlocks new_else),loc)
      | StmtIf(cond,then_stmt,None,loc) when hasReturn then_stmt ->
         let new_then =
            simplifyReturnPaths state.data (expandBlockOrSeq then_stmt)
            |> appendBlocksList |> fst
            |> collapseUnnecessaryIf
         in
         state,StmtIf(cond,appendBlocks new_then,None,loc)
      | _ -> state,stmt

(** Applies elimination of return statements to PSeq. Considers a return as a variable binding followed by a goto *)
let simplifyReturnInPSeq : ('data,exp) traverser =
   fun state stmt ->
      match stmt with
      | PSeq(name,pseq_stmts,loc) when hasIfStmtWithReturnList pseq_stmts ->
         let var  = PId(["_return_value"],None,loc) in
         let decl = StmtVal(var,None,loc) in
         let inner_state = deriveState state var in
         let _,simp_stmts = traverseBottomExpList None simplifyReturn inner_state pseq_stmts in
         let new_stmts =
            simplifyReturnPaths var simp_stmts
            |> appendBlocksList |> fst
            |> collapseUnnecessaryIf
         in
         let ret_stmt = StmtReturn(var,loc) in
         let new_stmts,loc = appendBlocksList (decl::new_stmts@[ret_stmt]) in
         state,PSeq(name,new_stmts,loc)
      | _ -> state,stmt

(** Removes nested block created by the transformations *)
let removeUnnecessaryBlocks : ('data,exp) traverser =
   fun state stmt ->
      match stmt with
      | StmtBlock(_,[h],_) -> state,h
      | _ -> state,stmt

(** Given a condition that we know is true, evaluates the if-statements using that condition *)
let evaluateCertainConditions : ('data,exp) traverser =
   fun state stmt ->
      match stmt with
      | StmtIf(cond,then_stmt,_,loc) when compareExp state.data cond = 0 ->
         state,then_stmt
      | StmtIf(cond,_,Some(else_stmt),loc) when compareExp state.data (notCondition cond) = 0 ->
         state,else_stmt
      | _ -> state,stmt

(** Removes if-statements with empty blocks *)
let removeEmptyIfConditions : ('data,exp) traverser =
   fun state stmt ->
      match stmt with
      | StmtIf(cond,StmtBlock(_,[],_),Some(else_stmt),loc) ->
         state,StmtIf(notCondition cond,else_stmt,None,loc)
      | StmtIf(cond,then_stmt,Some(StmtBlock(_,[],_)),loc) ->
         state,StmtIf(cond,then_stmt,None,loc)
      | _ -> state,stmt

(** Changes if(!a) stmt1 else stmt2 -> if(a) stmt2 else stmt1 *)
let removeSwapedIfCondition : ('data,exp) traverser =
   fun state stmt ->
      match stmt with
      | StmtIf(PCall(_,["not"],[exp],_,_),then_stmt,Some(else_stmt),loc)->
         state,StmtIf(exp,else_stmt,Some(then_stmt),loc)
      | _ -> state,stmt

(** Simplifies dummy if-statements created by the transformations. e.g. if(true) ... or if(a) { if(!a) ...}  *)
let removeUnnecesaryIfConditions : ('data,exp) traverser =
   fun state stmt ->
      match stmt with
      | StmtIf(PBool(true,_),then_stmt,_,loc) -> state,then_stmt
      | StmtIf(cond,then_stmt,Some(else_stmt),loc) ->
         let inner_state1 = deriveState state cond in
         let _,nthen_stmt =
            traverseBottomExp None evaluateCertainConditions inner_state1 then_stmt in
         let inner_state2 = deriveState state (notCondition cond) in
         let _,nelse_stmt =
            traverseBottomExp None evaluateCertainConditions inner_state2 else_stmt in
         state,StmtIf(cond,nthen_stmt,Some(nelse_stmt),loc)
      | StmtIf(cond,then_stmt,None,loc) ->
         let inner_state = deriveState state cond in
         let _,nthen_stmt =
            traverseBottomExp None evaluateCertainConditions inner_state then_stmt in
         state,StmtIf(cond,nthen_stmt,None,loc)
      | _ -> state,stmt

(* Return removal *)
let removalOfSequencesPasses state =
   state
   |+> TypesUtil.expandStmtList None splitIfWithTwoReturns
   |+> TypesUtil.traverseBottomExpList (Some(skipIfStmt)) wrapSimpleReturn
   |+> TypesUtil.traverseBottomExpList None simplifyReturnInPSeq
   |+> TypesUtil.traverseBottomExpList None
      (removeUnnecessaryBlocks
       |-> removeUnnecesaryIfConditions
       |-> removeEmptyIfConditions
       |-> removeSwapedIfCondition)


