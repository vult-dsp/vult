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

(** Provides passes that perform simple transformations *)

open TypesVult
open TypesUtil
open PassesUtil

(** Changes (x) -> x *)
let removeGroups : ('data,exp) transformation =
   fun state exp ->
      match exp with
      | PGroup(e,_) ->
         state,e
      | _ -> state,exp

(** Transforms x:foo() - PTyped(x,foo(),_) -> PCall(x,foo,...) *)
let makeTypedIdNamedCall : ('data,exp) traverser =
   fun state exp ->
      match exp with
      | PId(id,Some(PCall(None,fname,args,loc,attr)),_) ->
         state,PCall(Some(id),fname,args,loc,attr)
      | PTyped(PId(id,None,_),PCall(None,fname,args,loc,attr),_) ->
         state,PCall(Some(id),fname,args,loc,attr)
      | _ -> state,exp

(** Adds a default name to all function calls. e.g. foo(x) ->  _inst_0:foo(x) *)
let nameFunctionCalls : ('data,exp) transformation =
   fun state exp ->
      match exp with
      | PCall(None,name,args,floc,attr) ->
         let inst = "_i"^(string_of_int state.data.counter) in
         (incState state),PCall(Some([inst]),name,args,floc,attr)
      | _ -> state,exp

(** Transforms all operators into function calls *)
let operatorsToFunctionCalls : ('data,exp) transformation =
   fun state exp ->
      match exp with
      | PUnOp(op,e,loc) ->
         state,PCall(None,["'"^op^"'"],[e],loc,[])
      | PBinOp(op,e1,e2,loc) ->
         state,PCall(None,["'"^op^"'"],[e1;e2],loc,[])
      | _ -> state,exp

(** Transforms mem x=0; -> mem x; x=0; *)
let separateBindAndDeclaration : ('data,exp) expander =
   fun state stmt ->
      match stmt with
      | StmtMem(lhs,init,Some(rhs),loc) ->
         let stmts = [StmtMem(lhs,init,None,loc); StmtBind(lhs,rhs,loc)]  in
         state,stmts
      | StmtVal(lhs,Some(rhs),loc) ->
         let stmts = [StmtVal(lhs,None,loc); StmtBind(lhs,rhs,loc)]  in
         state,stmts
      | _ -> state,[stmt]

(** Transforms val x,y; -> val x; val y; *)
let makeSingleDeclaration : ('data,exp) expander =
   fun state stmt ->
      match stmt with
      | StmtVal(PTuple(elems,_),None,loc) ->
         (* TODO: split the type *)
         let stmts = List.map (fun a -> StmtVal(a,None,loc)) elems in
         state,stmts
      | StmtMem(PTuple(elems,_),None,None,loc) ->
         let stmts = List.map (fun a -> StmtMem(a,None,None,loc)) elems in
         state,stmts
      | StmtMem(PTuple(elems,_),Some(PTuple(init,_)),None,loc) ->
         let stmts = List.map2 (fun a b -> StmtMem(a,Some(b),None,loc)) elems init in
         state,stmts
      | _ -> state,[stmt]

(** True if the attributes contains SimpleBinding *)
let isSimpleBinding (attr:call_attributes) : bool =
   List.exists (fun a->a=SimpleBinding) attr

(** Creates bindings for all function calls in an expression *)
let bindFunctionAndIfExpCallsInExp : (int * exp list,exp) transformation =
   fun state exp ->
      match exp with
      | PIf(_,_,_,loc) ->
         let count,stmts = getState state in
         let tmp_var     = ["_tmp"^(string_of_int count)] in
         let decl        = StmtVal(PId(tmp_var,None,loc),None,loc) in
         let bind_stmt   = StmtBind(PId(tmp_var,None,loc),exp,loc) in
         let new_data    = count+1,[bind_stmt;decl]@stmts in
         (setState state new_data),PId(tmp_var,None,loc)
      | PCall(name,fname,args,loc,attr) when not (isSimpleBinding attr) ->
         let count,stmts = getState state in
         let var_type    = getFunctionType state fname in
         let tmp_var     = ["_tmp"^(string_of_int count)] in
         let decl        = StmtVal(PId(tmp_var,var_type,loc),None,loc) in
         let bind_stmt   = StmtBind(PId(tmp_var,var_type,loc),PCall(name,fname,args,loc,SimpleBinding::attr),loc) in
         let new_data    = count+1,[bind_stmt;decl]@stmts in
         (setState state new_data),PId(tmp_var,None,loc)
      | _ -> state,exp

(** Binds all function calls to a variable. e.g. foo(bar(x)) -> tmp1 = bar(x); tmp2 = foo(tmp1); tmp2; *)
let bindFunctionAndIfExpCalls : ('data,exp) expander  =
   fun state stmt ->
      match stmt with
      | StmtBind(lhs,PCall(name,fname,args,loc1,attr),loc) ->
         let inner_state        = deriveState state (state.data.counter,[]) in
         let ret_state,new_args = TypesUtil.traverseBottomExpList None bindFunctionAndIfExpCallsInExp inner_state args in
         let count,stmts        = getState ret_state in
         let new_state          = {state.data with counter = count} in
         (setState state new_state),(List.rev (StmtBind(lhs,PCall(name,fname,new_args,loc1,attr),loc)::stmts))
      | StmtBind(lhs,rhs,loc) ->
         let inner_state        = deriveState state (state.data.counter,[]) in
         let ret_state,new_rhs  = TypesUtil.traverseBottomExp None bindFunctionAndIfExpCallsInExp inner_state rhs in
         let count,stmts        = getState ret_state in
         let new_state          = {state.data with counter = count} in
         (setState state new_state),(List.rev (StmtBind(lhs,new_rhs,loc)::stmts))
      | StmtReturn(e,loc) ->
         let inner_state = deriveState state (state.data.counter,[]) in
         let ret_state,new_e = TypesUtil.traverseBottomExp None bindFunctionAndIfExpCallsInExp inner_state e in
         let count,stmts = getState ret_state in
         let new_state = {state.data with counter = count} in
         (setState state new_state),(List.rev (StmtReturn(new_e,loc)::stmts))
      | StmtIf(cond,then_stmts,else_stmts,loc) ->
         let inner_state = deriveState state (state.data.counter,[]) in
         let ret_state,new_cond = TypesUtil.traverseBottomExp None bindFunctionAndIfExpCallsInExp inner_state cond in
         let count,stmts = getState ret_state in
         let new_state = {state.data with counter = count} in
         (setState state new_state),(List.rev (StmtIf(new_cond,then_stmts,else_stmts,loc)::stmts))

      | PIf(cond,then_,else_,loc) ->
         let inner_state1 = deriveState state (state.data.counter,[]) in
         let ret_state1,new_then_ = TypesUtil.traverseBottomExp None bindFunctionAndIfExpCallsInExp inner_state1 then_ in
         let count1,then_stmts = getState ret_state1 in
         let inner_state2 = deriveState state (count1,[]) in
         let ret_state2,new_else_ = TypesUtil.traverseBottomExp None bindFunctionAndIfExpCallsInExp inner_state2 else_ in
         let count2,else_stmts = getState ret_state2 in
         let then_exp =
            match then_stmts with
            | [] -> new_then_
            | _  -> PSeq(None,List.rev (StmtReturn(new_then_,loc)::then_stmts),loc)
         in
         let else_exp =
            match else_stmts with
            | [] -> new_else_
            | _  -> PSeq(None,List.rev (StmtReturn(new_else_,loc)::else_stmts),loc)
         in
         let new_state = {state.data with counter = count2} in
         (setState state new_state),[PIf(cond,then_exp,else_exp,loc)]
      | _ -> state,[stmt]

(** Changes (a,b) = (c,d) -> a=c; b=d. If not possible uses temporary variables like (a,b) =  (b,a) -> tmp1=a;tmp2=b; b=tmp1; a=tmp2 *)
let simplifyTupleAssign : ('data,exp) expander =
   fun state exp ->
      match exp with
      | StmtBind(PTuple(lhs,loc1),PTuple(rhs,loc2),loc) ->
         let lhs_id = TypesUtil.getIdsInExpList lhs in
         let rhs_id = TypesUtil.getIdsInExpList rhs in
         let common = CCList.Set.inter lhs_id rhs_id in
         begin
            match common with
            | [] -> state,List.map2 (fun a b -> StmtBind(a,b,loc)) lhs rhs
            | _  ->
               let init = state.data.counter in
               let tmp_vars  = List.mapi (fun i _ -> ["_tpl"^(string_of_int (i+init))]) lhs in
               let tmp_e     = List.map (fun a -> PId(a,None,loc)) tmp_vars in
               let to_tmp    = List.map2 (fun a b -> StmtBind(a,b,loc)) tmp_e rhs in
               let from_tmp  = List.map2 (fun a b -> StmtBind(a,b,loc)) lhs tmp_e in
               let decl = List.map (fun a -> StmtVal(PId(a,None,loc),None,loc)) tmp_vars in
               let ret_state = { state.data with counter = init+(List.length lhs)} in
               (setState state ret_state),decl@to_tmp@from_tmp
         end
      | _ -> state,[exp]

(** Returns Some(e,stmts) if the sequence has a single path until it returns *)
let rec isSinglePathStmtList (acc:exp list) (stmts:exp list) : (exp * exp list) option =
   match stmts with
   | [] -> None
   | [StmtReturn(e,_)] -> Some(e,List.rev acc)
   | h::_ when hasReturn h -> None
   | h::t -> isSinglePathStmtList (h::acc) t

(** Transforms x = {return y;}; -> x = y;  and _ = {stmts;} -> stmts *)
let simplifySequenceBindings : ('data,exp) traverser =
   fun state exp ->
      match exp with
      | PSeq(name,[PSeq(_,stmts,loc1)],loc2) ->
         state,PSeq(name,stmts,mergeLocations loc1 loc2)
      | PSeq(name,[StmtBlock(_,stmts,loc1)],loc2) ->
         state,PSeq(name,stmts,mergeLocations loc1 loc2)
      | StmtBlock(name,[StmtBlock(_,stmts,loc1)],loc2) ->
         state,StmtBlock(name,stmts,mergeLocations loc1 loc2)
      | StmtBind(lhs,PSeq(name,stmts,loc_s),loc) ->
         begin
            match isSinglePathStmtList [] stmts with
            | Some(e,rem_stmts) -> state,StmtBlock(name,rem_stmts@[StmtBind(lhs,e,loc)],loc_s)
            | None -> state,exp
         end
      | _ -> state,exp

(** Removes all the mem statements *)
let rec removeAllMem : ('data,exp) expander =
   fun state exp ->
      match exp with
      | StmtMem(PId(name,_,_),_,_,_) when IdentifierMap.mem name state.data ->
         state,[]
      | StmtMem(PId(name,_,_),_,_,_)->
         state,[exp]
      | _ -> state,[exp]

(** collects all non repeated mem statements *)
let collectMemDecl : ('data,exp) folder =
   fun state exp ->
      match exp with
      | StmtMem(PId(name,_,_),_,_,_) when IdentifierMap.mem name state.data ->
         state
      | StmtMem(PId(name,_,_),_,_,_)->
         setState state (IdentifierMap.add name exp state.data)
      | _ -> state

let collectMemInFunctions : ('data,'value) folder =
   fun state exp ->
      match exp with
      | StmtMem(elems,_,_,_) ->
         let names = getIdAsExp elems in
         addMemToFunction state names
      | _ -> state

let collectFunctionInstances : ('data,'value) folder =
   fun state exp ->
      match exp with
      | PCall(Some(iname),fname,_,_,_) ->
         addInstanceToFunction state iname fname
      | _ -> state

(** collects all val statements *)
let collectValDecl : ('data,exp) folder =
   fun state exp ->
      match exp with
      | StmtVal(PId(name,_,_),_,_) when IdentifierMap.mem name state.data ->
         state
      | StmtVal(PId(name,_,_),_,_)->
         setState state (IdentifierMap.add name exp state.data)
      | _ -> state

(** Removes all the val statements *)
let rec removeAllVal : ('data,exp) expander =
   fun state exp ->
      match exp with
      | StmtVal(PId(name,_,_),_,_) when IdentifierMap.mem name state.data ->
         state,[]
      | StmtVal(PId(name,_,_),_,_)->
         state,[exp]
      | _ -> state,[exp]

let isBasicType (name:identifier) : bool =
   match name with
   | ["real"]
   | ["bool"]
   | ["int"] -> true
   | _ -> false

(** Takes a list of option identifiers representing the types,returns some if all are the same *)
let checkAllSameTypes (elems:identifier option list) : identifier option =
   match elems with
   | [Some(elem)] -> Some(elem)
   | Some(h)::t ->
      begin
         let rec loop l first =
            match l with
            | [] -> Some(first)
            | Some(hh)::tt when hh=first -> loop tt first
            | _ -> None
         in loop t h
      end
   | _ -> None

let rec replaceSimplifiedTypeInMember (mappings: identifier IdentifierMap.t) (member:val_decl) : val_decl =
   let valname,tp,loc = member in
   match tp with
   | PId(name,_,_) when isBasicType name -> member
   | PId(name,None,iloc) ->
      begin
         match mapfindOption name mappings with
         | Some(new_name) -> valname,PId(new_name,None,iloc),loc
         | _-> failwith ("replaceSimplifiedTypeInMember: unknown type mapping"^(identifierStr name))
      end
   | PTuple(elems,iloc) ->
      begin
         let elem_ids    =
            List.map (fun a ->
               match a with
               | PId(name,_,_) -> name
               | _ -> failwith "replaceSimplifiedTypeInMember: invalid type")
            elems
         in
         let found_elems = List.map (fun a -> mapfindOption a mappings) elem_ids in
         match checkAllSameTypes found_elems with
         | Some(new_name) ->  valname,PId(new_name,None,iloc),loc
         | _ -> failwith "replaceSimplifiedTypeInMember: incorrect mix of types (better error message needed)"
      end
   | _ -> failwith "replaceSimplifiedTypeInMember: invalid type"

(** Takes a type and the mapping of simplified types and replaces all occurrences *)
let replaceSimplifiedTypes (mappings: identifier IdentifierMap.t) (tp:exp) : exp =
   match tp with
   | StmtType(name,[],Some(members),None,loc) ->
      let new_members = List.map (replaceSimplifiedTypeInMember mappings) members in
      StmtType(name,[],Some(new_members),None,loc)
   | _ -> failwith "replaceSimplifiedTypes: Invalid type"

let generateTypeNameForInstance (ids:identifier list) : exp option =
   match ids with
   | [] -> None
   | _ ->
      let s =
         ids
         |> List.map (joinSep "_")
         |> List.sort compare
         |> List.map (fun a->PId(["_auto_"^a],None,default_loc))
      in
      match s with
      | [h] -> Some(h)
      | _   -> Some(PTuple(s,default_loc))

let rec createTypeForFunction (state:pass_state tstate) (fname:identifier) : exp option =
   if isActiveFunction state fname |> not then
      None
   else
      let instances = lookupFunctionDefault state.data.instances    state fname IdentifierMap.empty in
      let mems      = lookupFunctionDefault state.data.function_mem state fname [] in
      let mem_pairs = List.map (fun a -> getIdAndType a) mems in
      let inst_pais =
         IdentifierMap.fold
            (fun name types acc ->
               let non_static = List.filter (fun a -> isActiveFunction state a) types in
               match generateTypeNameForInstance non_static with
               | None -> acc
               | Some(inst_type) -> (name,inst_type)::acc)
            instances []
      in
      let members = List.map (fun (a,b)-> a,b,default_loc) (mem_pairs@inst_pais) in
      Some(StmtType(generateTypeName fname,[],Some(members),None,default_loc))

let createTypes : ('data,exp) folder =
   fun state e ->
      match e with
      | StmtFun(name,_,_,_,_,_) ->
         let full_name = getScope state in
         begin
            match createTypeForFunction state name with
            | None -> state
            | Some(type_decl) ->
               addTypeOfFunction state full_name type_decl
         end
      | _ -> state

(** Removes duplicated mem declarations from StmtSequence and moves to the top the val *)
let relocateMemAndVal : ('data,exp) traverser =
   fun state exp ->
      match exp with
      | StmtFun(name,args,body,type_exp,active,loc) ->
         let inner_state = deriveState state IdentifierMap.empty in
         let mem_decl_map =
            TypesUtil.foldTopExp (Some(skipFun)) collectMemDecl inner_state body
         in
         let _,new_body   =
            TypesUtil.expandStmt (Some(skipFun)) removeAllMem mem_decl_map body in
         let mem_decl     = IdentifierMap.fold (fun _ a acc -> a::acc) mem_decl_map.data [] in
         state,StmtFun(name,args,appendBlocks (mem_decl@new_body),type_exp,active,loc)
      | StmtBlock(name,stmts,loc) ->
         let skip a = skipPSeq a && skipBlock a in
         let inner_state  = deriveState state IdentifierMap.empty in
         let val_decl_map =
            TypesUtil.foldTopExpList (Some(skip)) collectValDecl inner_state stmts in
         let _,new_stmts  =
            TypesUtil.expandStmtList (Some(skip)) removeAllVal val_decl_map stmts in
         let val_decl     = IdentifierMap.fold (fun _ a acc -> a::acc) val_decl_map.data [] in
         state,StmtBlock(name,val_decl@new_stmts,loc)
      | PSeq(name,stmts,loc) ->
         let skip a = skipPSeq a && skipBlock a in
         let inner_state  = deriveState state IdentifierMap.empty in
         let val_decl_map =
            TypesUtil.foldTopExpList (Some(skip)) collectValDecl inner_state stmts in
         let _,new_stmts  =
            TypesUtil.expandStmtList (Some(skip)) removeAllVal val_decl_map stmts in
         let val_decl     = IdentifierMap.fold (fun _ a acc -> a::acc) val_decl_map.data [] in
         state,PSeq(name,val_decl@new_stmts,loc)
      | _ -> state,exp

let getTypeName (tp:exp) : identifier =
   match tp with
   | StmtType(name,_,_,_,_) -> name
   | _ -> failwith "getTypeName: invalid type"

let renameType (tp:exp) (name:identifier) =
   match tp with
   | StmtType(_,[],Some(members),None,loc) -> StmtType(name,[],Some(members),None,loc)
   | _ -> failwith "renameType: invalid type"

let addTypeMapping (current_name:identifier) (new_type_name:identifier) (mapping:identifier IdentifierMap.t) =
   (*let _ = Printf.printf "Type %s maps to %s\n" (identifierStr current_name) (identifierStr new_type_name) in*)
   IdentifierMap.add current_name new_type_name mapping

(** Used to sort the members of a type *)
let compareMemberName (a,_,_) (b,_,_) = compare a b

(** Returns Some if the type expression are the same or if they can be merged *)
let mergeTypeExp (t1:exp) (t2:exp) : exp option =
   match t1,t2 with
   | PId(id1,None,_),PId(id2,None,_) when id1 = id2 -> Some(t1)
   | PId(id1,None,_),PTuple(elem,_)
      when List.exists (fun a -> compare_exp a t1 = 0) elem -> Some(t2)
   | PTuple(elem,_),PId(id1,None,_)
      when List.exists (fun a -> compare_exp a t2 = 0) elem -> Some(t1)
   | _ -> None

(** Merges a list of members (pre-sorted). If it was possible to merge them returns Some. *)
let rec matchTypeMembers (members1:val_decl list) (members2:val_decl list) (acc:val_decl list) : val_decl list option =
   match members1,members2 with
   | [],[] -> Some(acc)
   | (n1,t1,_)::r1,(n2,t2,_)::r2 when n1 = n2 ->
      begin
         match mergeTypeExp t1 t2 with
         | Some(new_type) -> matchTypeMembers r1 r2 ((n1,new_type,default_loc)::acc)
         | _ -> None
      end
   | _ -> None

(** This function merges two types if they have the same members. This function will appropriately handle
    instances with more than one type. It returns Some if the types could be merged. *)
let mergeTypes (t1:exp) (t2:exp) : exp option =
   match t1,t2 with
   | StmtType(name1,[],Some(members1),None,loc1),StmtType(name2,[],Some(members2),None,loc2) ->
      let members1_s = List.sort compareMemberName members1 in
      let members2_s = List.sort compareMemberName members2 in
      begin
         match matchTypeMembers members1_s members2_s [] with
         | Some(new_members) -> Some(StmtType(name1,[],Some(new_members),None,default_loc))
         | _ -> None
      end
   | _ -> None

(** Tries to merge the type with any of the types in the list.*)
let rec pushMergeType (count:int) (mapping:identifier IdentifierMap.t) (tp:exp) (current_types: exp list) (acc: exp list) : int * exp list * (identifier IdentifierMap.t) =
   match current_types with
   | []   ->
      let new_type_name = ["_auto_type"^(string_of_int count)] in
      let current_name  = getTypeName tp in
      let new_type      = renameType tp new_type_name in
      let new_mapping   = addTypeMapping current_name new_type_name mapping in
      (count+1),new_type::acc,new_mapping
   | h::t ->
      begin
         match mergeTypes h tp with
         | None           -> pushMergeType count mapping tp t (h::acc)
         | Some(new_type) ->
            let current_name  = getTypeName tp in
            let new_type_name = getTypeName new_type in
            let new_mapping   = addTypeMapping current_name new_type_name mapping in
            count,new_type::t@acc,new_mapping
      end

let simplifyTypes (state:pass_state tstate) (exp_list:exp list) : pass_state tstate * exp list =
   let _,simple_types,mapping = IdentifierMap.fold
      (fun fname value (count,types,mapping) ->  pushMergeType count mapping value types []) state.data.type_function (0,[],IdentifierMap.empty)
   in
   let final_types = List.map (replaceSimplifiedTypes mapping) simple_types in
   (*let _ = print_endline "Final types" in
   let _ = List.iter (fun a -> print_endline (PrintTypes.expressionStr a)) final_types in*)
   { state with data = { state.data with type_mapping = mapping } },final_types@exp_list

let nameLocalScopes : ('data,exp) traverser =
   fun state exp ->
      match exp with
      | PSeq(None,stmts,loc) ->
         let name = ["_scope_"^string_of_int(state.data.counter)] in
         incState state,PSeq(Some(name),stmts,loc)
      | StmtBlock(None,stmts,loc) ->
         let name = ["_scope_"^string_of_int(state.data.counter)] in
         incState state,StmtBlock(Some(name),stmts,loc)
      | StmtFun(name,args,StmtBlock(Some(_),stmts,loc1),ret,active,loc) ->
         state,StmtFun(name,args,StmtBlock(None,stmts,loc1),ret,active,loc)
      | StmtFun(name,args,PSeq(Some(_),stmts,loc1),ret,active,loc) ->
         state,StmtFun(name,args,PSeq(None,stmts,loc1),ret,active,loc)
      | _ -> state,exp

let markActiveFunctions : ('data,exp) traverser =
   fun state exp ->
      match exp with
      | StmtFun(name,args,body,ret,_,loc) when isActiveFunction state name ->
         state,StmtFun(name,args,body,ret,true,loc)
      | StmtFun(name,args,body,ret,_,loc) ->
         state,StmtFun(name,args,body,ret,false,loc)
      | _ -> state,exp

(** Wraps all the statements into a function called __main__ and calls it *)
let makeFunAndCall name state stmts =
   let fcall = ["__"^name^"__"] in
   state,[StmtFun(fcall,[],appendBlocks stmts,None,false,default_loc); StmtReturn(PCall(Some(fcall),fcall,[],default_loc,[]),default_loc)]


(* Basic transformations *)
let basicPasses state =
   state
   |+> TypesUtil.traverseTopExpList None
      (removeGroups
      |-> makeTypedIdNamedCall
      |-> nameFunctionCalls
      |-> operatorsToFunctionCalls)
   |+> TypesUtil.expandStmtList None separateBindAndDeclaration
   |+> TypesUtil.expandStmtList None makeSingleDeclaration
   |+> TypesUtil.expandStmtList None bindFunctionAndIfExpCalls
   |+> TypesUtil.expandStmtList None simplifyTupleAssign

(* Last preparations *)
let finalPasses module_name state =
   state
   |+> TypesUtil.traverseBottomExpList None simplifySequenceBindings
   |+> makeFunAndCall module_name
   |+> TypesUtil.traverseBottomExpList None relocateMemAndVal
   |+> TypesUtil.foldAsTransformation None
      (collectMemInFunctions |*> collectFunctionInstances)
   |+> TypesUtil.foldAsTransformation None createTypes
   |+> simplifyTypes
   |+> TypesUtil.traverseBottomExpList None (nameLocalScopes|->markActiveFunctions)
