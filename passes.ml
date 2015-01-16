
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

open Types
open CCList
open TypesUtil


(** Generic type of transformations *)
type ('data,'value) transformation = 'data -> 'value -> 'data * 'value

(** Generic type of expanders *)
type ('data,'value) expander = 'data -> 'value -> 'data * 'value list

(** Generic type of folders *)
type ('data,'value) folder = 'data -> 'value -> 'data


(** Makes a chain of transformations. E.g. foo |-> bar will apply first foo then bar. *)
let (|->) : ('data,'value) transformation -> ('data,'value) transformation -> ('data,'value) transformation =
   fun a b ->
   fun state exp ->
      let new_state,new_exp = a state exp in
      b new_state new_exp

(** Pipes a pair (state,value) into transformation functions *)
let (|+>) : ('state * 'value) -> ('state, 'value) transformation -> ('state * 'value) =
   fun (state,value) transformation ->
      transformation state value

(** Options to control the transformations *)
type options =
   {
      inline          : bool;
      inline_weight   : int;
      simplify_return : bool;
      imperativize    : bool;
      finalize        : bool;
   }

let opt_full_transform =
   {
      inline          = true;
      inline_weight   = 10;
      simplify_return = true;
      imperativize    = true;
      finalize        = true;
   }

let opt_simple_transform =
   {
      inline          = false;
      inline_weight   = 10;
      simplify_return = false;
      imperativize    = false;
      finalize        = false;
   }

(** Traversing state one *)
type traversing_state =
   {
      functions       : parse_exp NamedIdMap.t;
      function_weight : int NamedIdMap.t;
      counter         : int;
      options         : options;
   }
(* ======================= *)

(** Transforms mem x=0; -> mem x; x=0; *)
let separateBindAndDeclaration : ('data,parse_exp) expander =
   fun state stmt ->
      match stmt with
      | StmtMem(lhs,init,Some(rhs),loc) ->
         let stmts = [StmtMem(lhs,init,None,loc); StmtBind(lhs,rhs,loc)]  in
         state,stmts
      | StmtVal(lhs,Some(rhs),loc) ->
         let stmts = [StmtVal(lhs,None,loc); StmtBind(lhs,rhs,loc)]  in
         state,stmts
      | _ -> state,[stmt]

(* ======================= *)

let rec map3 f a b c =
   match a,c,b with
   | [],[],[] -> []
   | h1::t1,h2::t2,h3::t3 -> (f h1 h2 h3)::(map3 f t1 t2 t3)
   | _ -> failwith "map3: Different number of elements in lists"

(** Transforms val x,y; -> val x; val y; *)
let makeSingleDeclaration : ('data,parse_exp) expander =
   fun state stmt ->
      match stmt with
      | StmtVal(PTuple(elems,_),None,loc) ->
         let stmts = List.map (fun a -> StmtVal(a,None,loc)) elems in
         state,stmts
      | StmtMem(PTuple(elems,_),None,None,loc) ->
         let stmts = List.map (fun a -> StmtMem(a,None,None,loc)) elems in
         state,stmts
      | StmtMem(PTuple(elems,_),Some(PTuple(init,_)),None,loc) ->
         let stmts = List.map2 (fun a b -> StmtMem(a,Some(b),None,loc)) elems init in
         state,stmts
      | _ -> state,[stmt]

(* ======================= *)

(** Adds a default name to all function calls. e.g. foo(x) ->  _inst_0:foo(x) *)
let nameFunctionCalls : ('data,parse_exp) transformation =
   fun state exp ->
      match exp with
      | PCall(SimpleId(name,loc),args,floc,attr) ->
         let inst = "_i"^(string_of_int state.counter) in
         let new_state = {state with counter = state.counter+1} in
         new_state,PCall(NamedId([inst],name,loc,loc),args,floc,attr)
      | _ -> state,exp

(* ======================= *)

(** Changes (x) -> x *)
let removeGroups : ('data,parse_exp) transformation =
   fun state exp ->
      match exp with
      | PGroup(e,_) ->
         state,e
      | _ -> state,exp

(* ======================= *)

(** Transforms all operators into function calls *)
let operatorsToFunctionCalls : ('data,parse_exp) transformation =
   fun state exp ->
      match exp with
      | PUnOp(op,e,loc) ->
         state,PCall(NamedId(["_"],["'"^op^"'"],loc,loc),[e],loc,[])
      | PBinOp(op,e1,e2,loc) ->
         state,PCall(NamedId(["_"],["'"^op^"'"],loc,loc),[e1;e2],loc,[])
      | _ -> state,exp

(* ======================= *)

(** Changes (a,b) = (c,d) -> a=c; b=d. If not possible uses temporary variables like (a,b) =  (b,a) -> tmp1=a;tmp2=b; b=tmp1; a=tmp2 *)
let simplifyTupleAssign : ('data,parse_exp) expander =
   fun state exp ->
      match exp with
      | StmtBind(PTuple(lhs,loc1),PTuple(rhs,loc2),loc) ->
         let lhs_id = TypesUtil.getIdsInExpList lhs in
         let rhs_id = TypesUtil.getIdsInExpList rhs in
         let common = Set.inter ~eq:TypesUtil.compareName lhs_id rhs_id in
         begin
            match common with
            | [] -> state,List.map2 (fun a b -> StmtBind(a,b,loc)) lhs rhs
            | _  ->
               let init = state.counter in
               let tmp_vars  = List.mapi (fun i _ -> SimpleId(["_tpl"^(string_of_int (i+init))],loc)) lhs in
               let tmp_e     = List.map (fun a -> PId(a)) tmp_vars in
               let to_tmp    = List.map2 (fun a b -> StmtBind(a,b,loc)) tmp_e rhs in
               let from_tmp  = List.map2 (fun a b -> StmtBind(a,b,loc)) lhs tmp_e in
               let decl = List.map (fun a -> StmtVal(PId(a),None,loc)) tmp_vars in
               let new_state = { state with counter = init+(List.length lhs)} in
               new_state,decl@to_tmp@from_tmp
         end
      | _ -> state,[exp]


(* ======================= *)

(** True if the attributes contains SimpleBinding *)
let isSimpleBinding attr = List.exists (fun a->a=SimpleBinding) attr

(** Creates bindings for all function calls in an expression *)
let bindFunctionAndIfExpCallsInExp : (int * parse_exp list,parse_exp) transformation =
   fun data exp ->
      match exp with
      | PIf(_,_,_,loc) ->
         let count,stmts = data in
         let tmp_var = SimpleId(["_tmp"^(string_of_int count)],loc) in
         let decl = StmtVal(PId(tmp_var),None,loc) in
         let bind_stmt = StmtBind(PId(tmp_var),exp,loc) in
         (count+1,[bind_stmt;decl]@stmts),PId(tmp_var)
      | PCall(name,args,loc,attr) when not (isSimpleBinding attr) ->
         let count,stmts = data in
         let tmp_var = SimpleId(["_tmp"^(string_of_int count)],loc) in
         let decl = StmtVal(PId(tmp_var),None,loc) in
         let bind_stmt = StmtBind(PId(tmp_var),PCall(name,args,loc,SimpleBinding::attr),loc) in
         (count+1,[bind_stmt;decl]@stmts),PId(tmp_var)
      | _ -> data,exp

(** Binds all function calls to a variable. e.g. foo(bar(x)) -> tmp1 = bar(x); tmp2 = foo(tmp1); tmp2; *)
let bindFunctionAndIfExpCalls : ('data,parse_exp) expander  =
   fun state stmt ->
      match stmt with
      | StmtBind(lhs,PCall(name,args,loc1,attr),loc) ->
         let (count,stmts),new_args = TypesUtil.traverseBottomExpList None bindFunctionAndIfExpCallsInExp (state.counter,[]) args in
         {state with counter = count},(List.rev (StmtBind(lhs,PCall(name,new_args,loc1,attr),loc)::stmts))
      | StmtBind(lhs,rhs,loc) ->
         let (count,stmts),new_rhs = TypesUtil.traverseBottomExp None bindFunctionAndIfExpCallsInExp (state.counter,[]) rhs in
         {state with counter = count},(List.rev (StmtBind(lhs,new_rhs,loc)::stmts))
      | StmtReturn(e,loc) ->
         let (count,stmts),new_e = TypesUtil.traverseBottomExp None bindFunctionAndIfExpCallsInExp (state.counter,[]) e in
         {state with counter = count},(List.rev (StmtReturn(new_e,loc)::stmts))
      | StmtIf(cond,then_stmts,else_stmts,loc) ->
         let (count,stmts),new_cond = TypesUtil.traverseBottomExp None bindFunctionAndIfExpCallsInExp (state.counter,[]) cond in
         {state with counter = count},(List.rev (StmtIf(new_cond,then_stmts,else_stmts,loc)::stmts))

      | _ -> state,[stmt]

(* ======================= *)

(** Wraps the values of an if expression into return statements *)
let wrapIfExpValues : ('data,parse_exp) transformation =
   fun state exp ->
      match exp with
      | PIf(cond,then_exp,else_exp,loc) ->
         state,PIf(cond,PSeq([StmtReturn(then_exp,loc)],loc),PSeq([StmtReturn(else_exp,loc)],loc),loc)
      | _ -> state,exp


(* ======================= *)

(** Adds all function definitions to a map in the state and also the weight of the function *)
let collectFunctionDefinitions : ('data,parse_exp) folder =
   fun state exp ->
      match exp with
      | StmtFun(name,args,stmts,loc) ->
         let simple_name = removeNamedIdType name in
         let weight = getExpWeight stmts in
         (*let _ = Printf.printf "*** Adding function '%s' with weight %i\n" (namedIdStr simple_name) weight in*)
         { state with
           functions = NamedIdMap.add simple_name exp state.functions;
           function_weight = NamedIdMap.add simple_name weight state.function_weight }
      | _ -> state

(* ======================= *)

(** Changes appends the given prefix to all named_ids *)
let prefixAllNamedIds : ('data,parse_exp) transformation =
   fun prefix exp ->
      match exp with
      | PId(name) -> prefix,PId(prefixNamedId prefix name)
      | _ -> prefix,exp

(** inlines the given function call by preparing the assignments and replacing the statements *)
let inlineFunctionCall (state:'data) (name:named_id) (args:parse_exp list) loc : parse_exp list =
   let call_name,ftype = getFunctionTypeAndName name in
   let fname = SimpleId(ftype,default_loc) in
   let function_def = NamedIdMap.find fname state.functions in
   match function_def with
   | StmtFun(_,fargs,fbody,_) ->
      let prefix = (joinSep "_" call_name)^"_" in
      let fargs_prefixed = List.map (prefixNamedId prefix) fargs in
      let _,fbody_prefixed = TypesUtil.traverseBottomExp None prefixAllNamedIds prefix fbody in
      let new_decl = List.map (fun a-> StmtVal(PId(a),None,loc)) fargs_prefixed in
      let new_assignments = List.map2 (fun a b -> StmtBind(PId(a),b,loc)) fargs_prefixed args in
      [appendBlocks (new_decl@new_assignments@[fbody_prefixed])]
   | _ -> failwith "inlineFunctionCall: Invalid function declaration"

(** Main traverser/expander to inline function calls *)
let inlineStmts : ('data,parse_exp) expander =
   fun state exp ->
      match exp with
      | PCall(fname_full,args,loc,_) ->
         let name,ftype = getFunctionTypeAndName fname_full in
         let fname = SimpleId(ftype,default_loc) in
         if name = ["_"] || not (NamedIdMap.mem fname state.functions)  then
            state,[exp]
         else
            let weight = NamedIdMap.find fname state.function_weight in
            if weight > state.options.inline_weight then
               state,[exp]
            else begin
               let new_stmts = inlineFunctionCall state fname_full args loc in
               state,[PSeq(new_stmts,loc)]
            end
      | _ -> state,[exp]

(* ======================= *)

let isReturn : ('data,parse_exp) folder =
   fun state e ->
      match e with
      | StmtReturn(_,_) -> true
      | _ -> state

let isIfStmt : ('data,parse_exp) folder =
   fun state e ->
      match e with
      | StmtIf(_,_,_,_) -> true
      | _ -> state

let skipPSeq (e:parse_exp) : bool =
   match e with
   | PSeq(_,_) -> false
   | _ -> true

let skipFun stmt =
   match stmt with
   | StmtFun(_,_,_,_) -> false
   | _ -> true

let skipBlock stmt =
   match stmt with
   | StmtBlock(_,_) -> false
   | _ -> true

let skipPSeq stmt =
   match stmt with
   | PSeq(_,_) -> false
   | _ -> true

let skipIfStmt stmt =
   match stmt with
   | StmtIf(_) -> false
   | _ -> true

let hasReturn (stmt:parse_exp) : bool =
   foldTopExp (Some(skipPSeq)) isReturn false stmt

let hasReturnList (stmts:parse_exp list) : bool =
   foldTopExpList (Some(skipPSeq)) isReturn false stmts

let hasIfStmtList (stmts:parse_exp list) : bool =
   foldTopExpList (Some(skipPSeq)) isIfStmt false stmts

(** Returns true if there is a return statement inside an if expression *)
let hasIfStmtWithReturnList (stmts:parse_exp list) : bool =
   let fold_function state stmt =
      match stmt with
      | StmtIf(_,then_exp,None,_)  ->
         state || hasReturn then_exp
      | StmtIf(_,then_exp,Some(else_exp),_)  ->
         state || hasReturn then_exp || hasReturn else_exp
      | _ -> state
   in foldDownExpList (Some(skipPSeq)) fold_function false stmts

(** Returns Some(e,stmts) if the sequence has a single path until it returns *)
let rec isSinglePathStmtList (acc:parse_exp list) (stmts:parse_exp list) : (parse_exp * parse_exp list) option =
   match stmts with
   | [] -> None
   | [StmtReturn(e,_)] -> Some(e,List.rev acc)
   | h::_ when hasReturn h -> None
   | h::t -> isSinglePathStmtList (h::acc) t

(** Transforms x = {return y;}; -> x = y;  and _ = { stmts; } -> stmts *)
let simplifySequenceBindings : ('data,parse_exp) traverser =
   fun state exp ->
      match exp with
      | PSeq([PSeq(stmts,loc1)],loc2) ->
         state,PSeq(stmts,mergeLocations loc1 loc2)
      | PSeq([StmtBlock(stmts,loc1)],loc2) ->
         state,PSeq(stmts,mergeLocations loc1 loc2)
      | StmtBlock([StmtBlock(stmts,loc1)],loc2) ->
         state,StmtBlock(stmts,mergeLocations loc1 loc2)
      | StmtBind(lhs,PSeq(stmts,loc_s),loc) ->
         begin
            match isSinglePathStmtList [] stmts with
            | Some(e,rem_stmts) -> state,StmtBlock(rem_stmts@[StmtBind(lhs,e,loc)],loc_s)
            | None -> state,exp
         end
      | _ -> state,exp

(* ======================= *)

(** Removes all the mem statememts *)
let rec removeAllMem : ('data,parse_exp) expander =
   fun state exp ->
      match exp with
      | StmtMem(PId(name),_,_,_) when NamedIdMap.mem name state ->
         state,[]
      | StmtMem(PId(name),_,_,_)->
         state,[exp]
      | _ -> state,[exp]

(** Removes all the val statememts *)
let rec removeAllVal : ('data,parse_exp) expander =
   fun state exp ->
      match exp with
      | StmtVal(PId(name),_,_) when NamedIdMap.mem name state ->
         state,[]
      | StmtVal(PId(name),_,_)->
         state,[exp]
      | _ -> state,[exp]

(** collects all non repeated mem statements *)
let collectMemDecl : ('data,parse_exp) folder =
   fun state exp ->
      match exp with
      | StmtMem(PId(name),_,_,_) when NamedIdMap.mem name state ->
         state
      | StmtMem(PId(name),_,_,_)->
         (NamedIdMap.add name exp state)
      | _ -> state

(** collects all val statements *)
let collectValDecl : ('data,parse_exp) folder =
   fun state exp ->
      match exp with
      | StmtVal(PId(name),_,_) when NamedIdMap.mem name state ->
         state
      | StmtVal(PId(name),_,_)->
         (NamedIdMap.add name exp state)
      | _ -> state

(** Removes duplicated mem declarations from StmtSequence and moves to the top the val *)
let relocateMemAndVal : ('data,parse_exp) traverser =
   fun state exp ->
      match exp with
      | StmtFun(name,args,body,loc) ->
         let mem_decl_map = TypesUtil.foldTopExp (Some(skipFun)) collectMemDecl NamedIdMap.empty body in
         let _,new_body   = TypesUtil.expandStmt (Some(skipFun)) removeAllMem mem_decl_map body in
         let mem_decl     = NamedIdMap.fold (fun _ a acc -> a::acc) mem_decl_map [] in
         state,StmtFun(name,args,appendBlocks (mem_decl@new_body),loc)
      | StmtBlock(stmts,loc) ->
         let skip a = skipPSeq a && skipBlock a in
         let val_decl_map = TypesUtil.foldTopExpList (Some(skip)) collectValDecl NamedIdMap.empty stmts in
         let _,new_stmts  = TypesUtil.expandStmtList (Some(skip)) removeAllVal val_decl_map stmts in
         let val_decl     = NamedIdMap.fold (fun _ a acc -> a::acc) val_decl_map [] in
         state,StmtBlock(val_decl@new_stmts,loc)
      | PSeq(stmts,loc) ->
         let skip a = skipPSeq a && skipBlock a in
         let val_decl_map = TypesUtil.foldTopExpList (Some(skip)) collectValDecl NamedIdMap.empty stmts in
         let _,new_stmts  = TypesUtil.expandStmtList (Some(skip)) removeAllVal val_decl_map stmts in
         let val_decl     = NamedIdMap.fold (fun _ a acc -> a::acc) val_decl_map [] in
         state,PSeq(val_decl@new_stmts,loc)
      | _ -> state,exp

(* ======================= *)
(** Changes if(cond,e1,e2) -> if(cond,{|return e1|},{|return e2|})*)
let makeIfStatement : ('data,parse_exp) traverser =
   fun state exp ->
      match exp with
      | StmtBind(lhs,PIf(cond,then_exp,else_exp,iloc),bloc) ->
         state,StmtIf(cond,StmtBind(lhs,then_exp,iloc),Some(StmtBind(lhs,else_exp,bloc)),iloc)
      | _ -> state,exp

(* ======================= *)

(** Negates a condition*)
let notCondition exp =
   match exp with
   | PCall(SimpleId(["'!'"],_),[exp],_,_) -> exp
   | _ ->
      let loc = getExpLocation exp in
      PCall(SimpleId(["'!'"],loc),[exp],loc,[])

(** Splits if statemtents containing else in order to apply simplifications. e.g. if(a) stmt1; else stmt2; -> if(a) stmt1; if(!a) stmt2; *)
let splitIfWithTwoReturns : ('data,parse_exp) expander =
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

(** Wrapps changes return a; -> if(true) return a; This is useful to apply transforations *)
let wrapSimpleReturn : ('data,parse_exp) traverser =
   fun state stmt ->
      match stmt with
      | StmtReturn(e,loc) -> state,StmtIf(PBool(true,loc),StmtReturn(e,loc),None,loc)
      | _ -> state,stmt

(** Replaces the return statements by bindings to the return variable *)
let rec replaceReturn ret_var stmts =
   let replace e = match e with | StmtReturn(e,loc) -> StmtBind(ret_var,e,loc) | _ -> e in
   match stmts with
   | StmtBlock(block_stmts,loc)  -> StmtBlock(List.map replace block_stmts,loc)
   | _ -> replace stmts

(** Main transformation that eliminates the returns (assumes that return -> if(true) goto :end_of_function ) *)
let rec simplifyReturnPaths ret_var (stmts:parse_exp list) : parse_exp list =
   match stmts with
   | [] -> []
   | StmtIf(cond,then_stmts,None,loc)::[] when hasReturn then_stmts ->
      [StmtIf(cond,replaceReturn ret_var then_stmts,None,loc)]
   | StmtIf(cond,then_stmts,None,loc)::t when hasReturn then_stmts ->
      let new_t = simplifyReturnPaths ret_var t in
      [StmtIf(notCondition cond,StmtBlock(new_t,loc),Some(replaceReturn ret_var then_stmts),loc)]
   | h::t -> h::(simplifyReturnPaths ret_var t)

(** Transforms if(a){} if(!a){} -> if(a) {} else {} *)
let rec collapseUnnecessaryIf (stmts:parse_exp list) : parse_exp list =
   match stmts with
   | [] -> []
   | StmtIf(cond1,StmtIf(cond2,then_stmt2,_,loc2),else_stmt,loc1)::t
      when compareExp cond1 cond2 = 0 ->
      collapseUnnecessaryIf (StmtIf(cond1,then_stmt2,else_stmt,loc1)::t)
   | StmtIf(cond1,then_stmt1,None,loc1)::StmtIf(cond2,then_stmt2,None,loc2)::t
      when compareExp (notCondition cond1) cond2 = 0 ->
      collapseUnnecessaryIf (StmtIf(cond1,then_stmt1,Some(then_stmt2),loc1)::t)
   | h::t -> h::(collapseUnnecessaryIf t)

(** Removes nested block created by the transformations *)
let removeUnnecessaryBlocks : ('data,parse_exp) traverser =
   fun state stmt ->
      match stmt with
      | StmtBlock([h],_) -> state,h
      | _ -> state,stmt

(** Given a condition that we know is true, evaluates the if statements using that condition *)
let evaluateCertainConditions : ('data,parse_exp) traverser =
   fun known_cond stmt ->
      match stmt with
      | StmtIf(cond,then_stmt,_,loc) when compareExp known_cond cond = 0 ->
         known_cond,then_stmt
      | StmtIf(cond,_,Some(else_stmt),loc) when compareExp known_cond (notCondition cond) = 0 ->
         known_cond,else_stmt
      | _ -> known_cond,stmt

(** Simplifies dummy if statements created by the transfrmations. e.g. if(true) ... or if(a) { if(!a) ...}  *)
let removeUnnecesaryIfConditions : ('data,parse_exp) traverser =
   fun state stmt ->
      match stmt with
      | StmtIf(PBool(true,_),then_stmt,_,loc) -> state,then_stmt
      | StmtIf(cond,then_stmt,Some(else_stmt),loc) ->
         let _,nthen_stmt= traverseBottomExp None evaluateCertainConditions cond then_stmt in
         let _,nelse_stmt= traverseBottomExp None evaluateCertainConditions (notCondition cond) else_stmt in
         state,StmtIf(cond,nthen_stmt,Some(nelse_stmt),loc)
      | StmtIf(cond,then_stmt,None,loc) ->
         let _,nthen_stmt= traverseBottomExp None evaluateCertainConditions cond then_stmt in
         state,StmtIf(cond,nthen_stmt,None,loc)
      | _ -> state,stmt

(** Changes if(!a) stmt1 else stmt2 -> if(a) stmt2 else stmt1 *)
let removeSwapedIfCondition : ('data,parse_exp) traverser =
   fun state stmt ->
      match stmt with
      | StmtIf(PCall(SimpleId(["'!'"],_),[exp],_,_),then_stmt,Some(else_stmt),loc)->
         state,StmtIf(exp,else_stmt,Some(then_stmt),loc)
      | _ -> state,stmt

(** Removes if statements with empty blocks *)
let removeEmptyIfConditions : ('data,parse_exp) traverser =
   fun state stmt ->
      match stmt with
      | StmtIf(cond,StmtBlock([],_),Some(else_stmt),loc) ->
         state,StmtIf(notCondition cond,else_stmt,None,loc)
      | StmtIf(cond,then_stmt,Some(StmtBlock([],_)),loc) ->
         state,StmtIf(cond,then_stmt,None,loc)
      | _ -> state,stmt

(** Applies the return elimination to each if statement *)
let simplifyReturn : ('data,parse_exp) traverser =
   fun var stmt ->
      match stmt with
      | StmtIf(cond,then_stmt,Some(else_stmt),loc) when hasReturn then_stmt || hasReturn else_stmt ->
         let new_then =
            simplifyReturnPaths var (expandBlockOrSeq then_stmt)
            |> appendBlocksList |> fst
            |> collapseUnnecessaryIf
         in
         let new_else =
            simplifyReturnPaths var (expandBlockOrSeq else_stmt)
            |> appendBlocksList |> fst
            |> collapseUnnecessaryIf
         in
         var,StmtIf(cond,appendBlocks new_then,Some(appendBlocks new_else),loc)
      | StmtIf(cond,then_stmt,None,loc) when hasReturn then_stmt ->
         let new_then =
            simplifyReturnPaths var (expandBlockOrSeq then_stmt)
            |> appendBlocksList |> fst
            |> collapseUnnecessaryIf
         in
         var,StmtIf(cond,appendBlocks new_then,None,loc)
      | _ -> var,stmt

(** Applies elimination of return statements to PSeq. Considers a return as a variable binding followed by a goto *)
let simplifyReturnInPSeq : ('data,parse_exp) traverser =
   fun state stmt ->
      match stmt with
      | PSeq(pseq_stmts,loc) when hasIfStmtWithReturnList pseq_stmts ->
         let var  = PId(SimpleId(["_return_value"],loc)) in
         let decl = StmtVal(var,None,loc) in
         let _,simp_stmts = traverseBottomExpList None simplifyReturn var pseq_stmts in
         let new_stmts =
            simplifyReturnPaths var simp_stmts
            |> appendBlocksList |> fst
            |> collapseUnnecessaryIf
         in
         let ret_stmt = StmtReturn(var,loc) in
         let new_stmts,loc = appendBlocksList (decl::new_stmts@[ret_stmt]) in
         state,PSeq(new_stmts,loc)
      | _ -> state,stmt

(* ======================= *)

(** Takes a fold function and wrap it as it was a transformation so it can be chained with |+> *)
let foldAsTransformation (f:('data,parse_exp) folder) (state:'date) (exp_list:parse_exp list) : 'data * parse_exp list =
   let new_state = TypesUtil.foldTopExpList None f state exp_list in
   new_state,exp_list

(** Inlines functions into functions *)
let inlineFunctionBodies (state:'data) (exp_list:parse_exp list) : 'data * parse_exp list =
   let inlineFunctionBody name fun_decl (functions,weigths) =
      match fun_decl with
      | StmtFun(fname,fargs,fbody,loc) ->
         let _,new_fbody = expandStmt None inlineStmts state fbody in
         let new_fbody_block = appendBlocks new_fbody in
         let weight = getExpWeight new_fbody_block in
         let new_functions = NamedIdMap.add name (StmtFun(fname,fargs,new_fbody_block,loc)) functions in
         let new_weigths = NamedIdMap.add name weight weigths in
         new_functions,new_weigths
      | _ -> functions,weigths
   in
   let new_functions,new_weigths = NamedIdMap.fold inlineFunctionBody state.functions (NamedIdMap.empty,NamedIdMap.empty) in
   { state with functions = new_functions; function_weight = new_weigths; },exp_list

(** Wrapps all the statements into a function called __main__ and calls it *)
let makeFunAndCall state stmts =
   let fcall = SimpleId(["__main__"],default_loc) in
   state,[StmtFun(fcall,[],appendBlocks stmts,default_loc); StmtReturn(PCall(fcall,[],default_loc,[]),default_loc)]

let applyOn cond f data =
   if cond data then
      f data
   else
      data

let returnRemovalON (state,_) = state.options.simplify_return
let inlineON        (state,_) = state.options.inline
let imperativizeON  (state,_) = state.options.imperativize
let finalizeON      (state,_) = state.options.finalize

let applyTransformations (options:options) (results:parser_results) =
   let initial_state =
      {
         counter         = 0;
         functions       = NamedIdMap.empty;
         function_weight = NamedIdMap.empty;
         options         = options;
      }
   in
   (* Basic transformations *)
   let basicPasses state =
      state
      |+> TypesUtil.traverseTopExpList None (removeGroups|->nameFunctionCalls|->operatorsToFunctionCalls|->wrapIfExpValues)
      |+> TypesUtil.expandStmtList None separateBindAndDeclaration
      |+> TypesUtil.expandStmtList None makeSingleDeclaration
      |+> TypesUtil.expandStmtList None bindFunctionAndIfExpCalls
      |+> TypesUtil.expandStmtList None simplifyTupleAssign
   in
   (* Return removal *)
   let removalOfReturnPasses state =
      state
      |+> TypesUtil.expandStmtList None splitIfWithTwoReturns
      |+> TypesUtil.traverseBottomExpList (Some(skipIfStmt)) wrapSimpleReturn
      |+> TypesUtil.traverseBottomExpList None simplifyReturnInPSeq
      |+> TypesUtil.traverseBottomExpList None
         (removeUnnecessaryBlocks
         |-> removeUnnecesaryIfConditions
         |-> removeEmptyIfConditions
         |-> removeSwapedIfCondition)
   in
   (* Inlining *)
   let inliningPasses state =
      state
      |+> foldAsTransformation collectFunctionDefinitions
      |+> inlineFunctionBodies
      |+> TypesUtil.expandStmtList (Some(skipFun)) inlineStmts
      |+> foldAsTransformation collectFunctionDefinitions
   in
   (* Used for imperative transformation *)
   let imperativePasses state =
      state
      |+> TypesUtil.traverseBottomExpList None makeIfStatement
   in
   (* Last preparations *)
   let finalPasses state =
      state
      |+> TypesUtil.traverseBottomExpList None simplifySequenceBindings
      |+> makeFunAndCall
      |+> TypesUtil.traverseBottomExpList None relocateMemAndVal
   in
   let passes stmts =
      (initial_state,[StmtBlock(stmts,default_loc)])
      |> basicPasses
      |> applyOn returnRemovalON removalOfReturnPasses
      |> applyOn inlineON        inliningPasses
      |> applyOn imperativizeON  imperativePasses
      |> applyOn finalizeON      finalPasses
      |> snd
   in

   let new_stmts = CCError.map passes results.presult in
   { results with presult = new_stmts }


