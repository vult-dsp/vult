
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

(** Default inline weight *)
let default_inline_weight = 10

(** Traversing state one *)
type state_t1 =
   {
      functions       : parse_exp NamedIdMap.t;
      function_weight : int NamedIdMap.t;
      counter         : int;
      inline_weight   : int;
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
               let tmp_vars  = List.mapi (fun i _ -> SimpleId(["_tpl"^(string_of_int (i+init))],default_loc)) lhs in
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
         let tmp_var = SimpleId(["_tmp"^(string_of_int count)],default_loc) in
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
            if weight > state.inline_weight then
               state,[exp]
            else begin
               let new_stmts = inlineFunctionCall state fname_full args loc in
               state,[PSeq(new_stmts,loc)]
            end
      | _ -> state,[exp]

(* ======================= *)

let isReturn state e =
   match e with
   | StmtReturn(_,_) -> true
   | _ -> state

let hasReturn (stmt:parse_exp) : bool =
   let skipPSeq e =
      match e with
      | PSeq(_,_) -> false
      | _ -> true
   in foldTopExp (Some(skipPSeq)) isReturn false stmt

let hasReturnList (stmts:parse_exp list) : bool =
   let skipPSeq e =
      match e with
      | PSeq(_,_) -> false
      | _ -> true
   in foldTopExpList (Some(skipPSeq)) isReturn false stmts

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

let skipFun stmt =
   match stmt with
   | StmtFun(_,_,_,_) -> false
   | _ -> true

(** collects all non repeated mem statements *)
let collectMemDecl : ('data,parse_exp) folder =
   fun state exp ->
      match exp with
      | StmtMem(PId(name),_,_,_) when NamedIdMap.mem name state ->
         state
      | StmtMem(PId(name),_,_,_)->
         (NamedIdMap.add name exp state)
      | _ -> state

(** Removes duplicated mem declarations  from StmtSequence *)
let removeDuplicateMemStmts : ('data,parse_exp) traverser =
   fun state exp ->
      match exp with
      | StmtFun(name,args,body,loc) ->
         let mem_decl_map = TypesUtil.foldTopExp (Some(skipFun)) collectMemDecl NamedIdMap.empty body in
         let _,new_body = TypesUtil.expandStmt (Some(skipFun)) removeAllMem mem_decl_map body in
         let mem_decl = NamedIdMap.fold (fun _ a acc -> a::acc) mem_decl_map [] in
         state,StmtFun(name,args,appendBlocks (mem_decl@new_body),loc)
      | _ -> state,exp
(* ======================= *)

let makeIfStatement : ('data,parse_exp) traverser =
   fun state exp ->
      match exp with
      | StmtBind(lhs,PIf(cond,then_exp,else_exp,iloc),bloc) ->
         state,StmtIf(cond,StmtBind(lhs,then_exp,iloc),Some(StmtBind(lhs,else_exp,bloc)),iloc)
      | _ -> state,exp

(* ======================= *)

let notCondition exp = PCall(SimpleId(["'!'"],default_loc),[exp],default_loc,[])

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


(* ======================= *)

(** Takes a fold function and wrap it as it was a transformation so it can be chained with |+> *)
let foldAsTransformation (f:('data,parse_exp) folder) (state:'date) (exp_list:parse_exp list) : 'data * parse_exp list =
   let new_state = TypesUtil.foldTopExpList None f state exp_list in
   new_state,exp_list

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

let makeFunAndCall state stmts =
   let fcall = SimpleId(["__main__"],default_loc) in
   state,[StmtFun(fcall,[],appendBlocks stmts,default_loc); StmtReturn(PCall(fcall,[],default_loc,[]),default_loc)]

let applyTransformations (results:parser_results) =
   let initial_state =
      {
         counter         = 0;
         functions       = NamedIdMap.empty;
         function_weight = NamedIdMap.empty;
         inline_weight   = default_inline_weight;
      }
   in
   let passes stmts =
      (initial_state,[StmtBlock(stmts,default_loc)])
      (* Basic transformations *)
      |+> TypesUtil.traverseTopExpList None (removeGroups|->nameFunctionCalls|->operatorsToFunctionCalls|->wrapIfExpValues)
      |+> TypesUtil.expandStmtList None separateBindAndDeclaration
      |+> TypesUtil.expandStmtList None makeSingleDeclaration
      |+> TypesUtil.expandStmtList None bindFunctionAndIfExpCalls
      |+> TypesUtil.expandStmtList None simplifyTupleAssign
      |+> TypesUtil.expandStmtList None splitIfWithTwoReturns
      (* Inlining *)
      |+> foldAsTransformation collectFunctionDefinitions
      |+> inlineFunctionBodies
      |+> TypesUtil.expandStmtList (Some(skipFun)) inlineStmts
      |+> foldAsTransformation collectFunctionDefinitions
      (* Used for imperative transformation *)
      |+> TypesUtil.traverseBottomExpList None makeIfStatement
      |+> TypesUtil.traverseBottomExpList None simplifySequenceBindings
      (* Las preparations *)
      |+> makeFunAndCall
      |+> TypesUtil.traverseTopExpList None removeDuplicateMemStmts
      |> snd
   in
   let new_stmts = CCError.map passes results.presult in
   { results with presult = new_stmts }


