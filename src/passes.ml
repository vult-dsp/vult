
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


(** Generic type of transformations *)
type ('data,'value) transformation = 'data tstate -> 'value -> 'data tstate * 'value

(** Generic type of expanders *)
type ('data,'value) expander = 'data tstate -> 'value -> 'data tstate * 'value list

(** Generic type of folders *)
type ('data,'value) folder = 'data tstate -> 'value -> 'data tstate


(** Makes a chain of transformations. E.g. foo |-> bar will apply first foo then bar. *)
let (|->) : ('data,'value) transformation -> ('data,'value) transformation -> ('data,'value) transformation =
   fun a b ->
   fun state exp ->
      let new_state,new_exp = a state exp in
      b new_state new_exp

(** Makes a chain of fold functions. E.g. foo |*> bar will apply first foo then bar. *)
let (|*>) : ('data,'value) folder -> ('data,'value) folder -> ('data,'value) folder =
   fun a b ->
   fun state exp ->
      let new_state = a state exp in
      b new_state exp

(** Pipes a pair (state,value) into transformation functions *)
let (|+>) : ('state tstate * 'value) -> ('state, 'value) transformation -> ('state tstate * 'value) =
   fun (state,value) transformation ->
      transformation state value

(** Options to control the transformations *)
type options =
   {
      basic           : bool;
      inline          : bool;
      inline_weight   : int;
      simplify_return : bool;
      imperativize    : bool;
      finalize        : bool;
   }

let opt_full_transform =
   {
      basic           = true;
      inline          = true;
      inline_weight   = 10;
      simplify_return = true;
      imperativize    = true;
      finalize        = true;
   }

let opt_simple_transform =
   {
      basic           = true;
      inline          = false;
      inline_weight   = 10;
      simplify_return = false;
      imperativize    = false;
      finalize        = false;
   }

let opt_no_transform =
   {
      basic           = false;
      inline          = false;
      inline_weight   = 10;
      simplify_return = false;
      imperativize    = false;
      finalize        = false;
   }

(** Traversing state one *)
type pass_state =
   {
      functions       : exp IdentifierMap.t;
      function_weight : int IdentifierMap.t;
      counter         : int;
      options         : options;
      function_mem    : exp list IdentifierMap.t;
      instances       : (identifier list IdentifierMap.t) IdentifierMap.t;
      active_function : bool IdentifierMap.t;
   }

(** Search a function in a table starting in the current scope and returns an Some if found *)
let lookupFunction (table:'a IdentifierMap.t) (state:pass_state tstate) (fname:identifier) : 'a option =
   let current_scope = state.scope |> List.flatten in
   let rec loop rev_scope =
      let full_name = (List.rev rev_scope)@fname in
      if IdentifierMap.mem full_name table then
         Some(IdentifierMap.find full_name table)
      else
         match rev_scope with
         | []   -> None
         | _::t -> loop t
   in loop current_scope

(** Search a function in a table starting in the current scope and returns the default value if not found *)
let lookupFunctionDefault (table:'a IdentifierMap.t) (state:pass_state tstate) (fname:identifier) (default:'a) : 'a =
   match lookupFunction table state fname with
   | None    -> default
   | Some(a) -> a

(** Registers a mem declaration in the current scope *)
let addMemToFunction (s:pass_state tstate) (names:exp list) =
   let scope = getScope s in
   (*let names_string = List.map PrintTypes.expressionStr names in
   let _ = Printf.printf "Adding mem %s to function %s\n" (joinSep ", " names_string) (identifierStr scope) in*)
   if IdentifierMap.mem scope s.data.function_mem then
      let current = IdentifierMap.find scope s.data.function_mem in
      let new_map = IdentifierMap.add scope (current@names) s.data.function_mem in
      { s with data = { s.data with function_mem = new_map } }
   else
      let new_map = IdentifierMap.add scope names s.data.function_mem in
      { s with data = { s.data with function_mem = new_map } }

(** Registers an instance in the current scope *)
let addInstanceToFunction (s:pass_state tstate) (name:identifier) (fname:identifier) =
   let scope             = getScope s in
   let instances_for_fun = mapfindDefault scope s.data.instances IdentifierMap.empty in
   let current_instance  = mapfindDefault name instances_for_fun [] in
   if List.exists (fun a->a=fname) current_instance then
      s
   else
      let _ = Printf.printf "Adding insance '%s' of funtcion '%s' to '%s'\n" (identifierStr name) (identifierStr fname) (identifierStr scope) in
      let new_instances     = IdentifierMap.add name (fname::current_instance) instances_for_fun in
      let new_inst_for_fun  = IdentifierMap.add scope new_instances s.data.instances in
      { s with data = { s.data with instances = new_inst_for_fun } }

let rec isActiveFunction (state: pass_state tstate) (name:identifier) : bool =
   (* this function can be cached by adding a pass that calculats every function *)
   (isMemFunction state name) || (isMemInstanceFunction state name)


and isMemFunction (state: pass_state tstate) (name:identifier) : bool =
   match lookupFunction state.data.function_mem state name with
   | None     -> false
   | Some([]) -> false
   | _        -> true

and isMemInstanceFunction (state:pass_state tstate) (name:identifier) : bool =
   let instances_for_fun = mapfindDefault name state.data.instances IdentifierMap.empty in
   IdentifierMap.fold (fun key types acc -> List.exists (isActiveFunction state) types ) instances_for_fun false

(* ======================= *)

(** Changes (x) -> x *)
let removeGroups : ('data,exp) transformation =
   fun state exp ->
      match exp with
      | PGroup(e,_) ->
         state,e
      | _ -> state,exp

(* ======================= *)

(** Adds a default name to all function calls. e.g. foo(x) ->  _inst_0:foo(x) *)
let nameFunctionCalls : ('data,exp) transformation =
   fun state exp ->
      match exp with
      | PCall(None,name,args,floc,attr) ->
         let inst = "_i"^(string_of_int state.data.counter) in
         let ret_state = { state.data with counter = state.data.counter+1} in
         (setState state ret_state),PCall(Some([inst]),name,args,floc,attr)
      | _ -> state,exp

(* ======================= *)

(** Transforms all operators into function calls *)
let operatorsToFunctionCalls : ('data,exp) transformation =
   fun state exp ->
      match exp with
      | PUnOp(op,e,loc) ->
         state,PCall(None,["'"^op^"'"],[e],loc,[])
      | PBinOp(op,e1,e2,loc) ->
         state,PCall(None,["'"^op^"'"],[e1;e2],loc,[])
      | _ -> state,exp

(* ======================= *)

(** Returns the name of the type that is declared for a function *)
let generateTypeName (id:identifier) : identifier =
   ["_auto_"^(joinSep "_" id)]

let generateTypeNameForInstance (ids:identifier list) : identifier option =
   match ids with
   | [] -> None
   | _ ->
      let s =
         ids
         |> List.map (joinSep "_")
         |> List.sort compare
         |> joinSep "_" in
      Some(["_auto_"^s])

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

(** Returs the name and type if an expression PId, fails on any other case *)
let getIdAndType (e:exp) =
   match e with
   | PId(name,Some(tp),_) -> name,tp
   | PId(name,None,loc) -> name,PId(["real"],None,loc)
   | _ -> failwith "getIdAndType: not expected mem declaration"

(** Once we have created the types based on two function calls this function merges them*)
let mergeTypes (t1:exp) (t2:exp) : exp =
   match t1,t2 with
   | StmtType(name1,[],Some(members1),None,loc1),StmtType(name2,[],Some(members2),None,loc2) ->
      let name = generateTypeNameForInstance [name1;name2] in
      let member_cmp (_,a,_) (_,b,_) = compare a b in
      (* TODO: Add check for equal types *)
      let members = CCList.sort_uniq ~cmp:member_cmp (members1@members2) in
      let loc = mergeLocations loc1 loc2 in
      StmtType(CCOpt.get_exn name,[],Some(members),None,loc)
   | _ -> failwith "mergeTypes: cannot merge these types"

let rec createTypeForFunction (state:pass_state tstate) (fname:identifier) : exp list =
   if isActiveFunction state fname |> not then
      []
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
               | Some(inst_type) -> (name,PId(inst_type,None,default_loc))::acc)
            instances []
      in
      let inst_types = IdentifierMap.fold
            (fun name types acc -> (createTypeForFunction state name)@acc)
            instances []
      in
      let merged_type =
         match inst_types with
         | [] -> []
         | h::t -> [List.fold_left mergeTypes h t]
      in
      let members = List.map (fun (a,b)-> a,b,default_loc) (mem_pairs@inst_pais) in
      StmtType(generateTypeName fname,[],Some(members),None,default_loc)::merged_type

let createTypes : ('data,exp) expander =
   fun state e ->
      match e with
      | StmtFun(name,_,_,_,_) ->
         let type_decl = createTypeForFunction state name in
         state,e::type_decl
      | _ -> state,[e]

(* ======================= *)

(** Transforms x:foo() - PTyped(x,foo(),_) -> PCall(x,foo,...) *)
let makeTypedIdNamedCall : ('data,exp) traverser =
   fun state exp ->
      match exp with
      | PId(id,Some(PCall(None,fname,args,loc,attr)),_) ->
         state,PCall(Some(id),fname,args,loc,attr)
      | PTyped(PId(id,None,_),PCall(None,fname,args,loc,attr),_) ->
         state,PCall(Some(id),fname,args,loc,attr)
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

(* ======================= *)

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

(* ======================= *)

(** True if the attributes contains SimpleBinding *)
let isSimpleBinding (attr:call_attributes) : bool =
   List.exists (fun a->a=SimpleBinding) attr

(** Creates bindings for all function calls in an expression *)
let bindFunctionAndIfExpCallsInExp : (int * exp list,exp) transformation =
   fun state exp ->
      match exp with
      | PIf(_,_,_,loc) ->
         let count,stmts = getState state in
         let tmp_var = ["_tmp"^(string_of_int count)] in
         let decl = StmtVal(PId(tmp_var,None,loc),None,loc) in
         let bind_stmt = StmtBind(PId(tmp_var,None,loc),exp,loc) in
         let new_data = count+1,[bind_stmt;decl]@stmts in
         (setState state new_data),PId(tmp_var,None,loc)
      | PCall(name,fname,args,loc,attr) when not (isSimpleBinding attr) ->
         let count,stmts = getState state in
         let tmp_var = ["_tmp"^(string_of_int count)] in
         let decl = StmtVal(PId(tmp_var,None,loc),None,loc) in
         let bind_stmt = StmtBind(PId(tmp_var,None,loc),PCall(name,fname,args,loc,SimpleBinding::attr),loc) in
         let new_data = count+1,[bind_stmt;decl]@stmts in
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
            | _  -> PSeq(List.rev (StmtReturn(new_then_,loc)::then_stmts),loc)
         in
         let else_exp =
            match else_stmts with
            | [] -> new_else_
            | _  -> PSeq(List.rev (StmtReturn(new_else_,loc)::else_stmts),loc)
         in
         let new_state = {state.data with counter = count2} in
         (setState state new_state),[PIf(cond,then_exp,else_exp,loc)]
      | _ -> state,[stmt]

(* ======================= *)

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


let isReturn : ('data,exp) folder =
   fun state e ->
      match e with
      | StmtReturn(_,_) -> setState state true
      | _ -> state

let isIfStmt : ('data,exp) folder =
   fun state e ->
      match e with
      | StmtIf(_,_,_,_) -> setState state true
      | _ -> state

let skipPSeq (e:exp) : bool =
   match e with
   | PSeq(_,_) -> false
   | _ -> true

let skipFun (stmt:exp) : bool =
   match stmt with
   | StmtFun(_,_,_,_,_) -> false
   | _ -> true

let skipBlock (stmt:exp) : bool =
   match stmt with
   | StmtBlock(_,_) -> false
   | _ -> true

let skipIfStmt (stmt:exp) : bool =
   match stmt with
   | StmtIf(_) -> false
   | _ -> true

let skipPIf (stmt:exp) : bool =
   match stmt with
   | PIf(_) -> false
   | _ -> true

let rec map3 f a b c =
   match a,c,b with
   | [],[],[] -> []
   | h1::t1,h2::t2,h3::t3 -> (f h1 h2 h3)::(map3 f t1 t2 t3)
   | _ -> failwith "map3: Different number of elements in lists"

let hasReturn (stmt:exp) : bool =
   foldTopExp (Some(skipPSeq)) isReturn (createState false) stmt
   |> getState

let hasReturnList (stmts:exp list) : bool =
   foldTopExpList (Some(skipPSeq)) isReturn (createState false) stmts
   |> getState

let hasIfStmtList (stmts:exp list) : bool =
   foldTopExpList (Some(skipPSeq)) isIfStmt (createState false) stmts
   |> getState

(* ======================= *)

(** Adds all function definitions to a map in the state and also the weight of the function *)
let collectFunctionDefinitions : ('data,exp) folder =
   fun state exp ->
      match exp with
      | StmtFun(name,args,stmts,type_exp,loc) ->
         let weight = getExpWeight stmts in
         (*let _ = Printf.printf "*** Adding function '%s' with weight %i\n" (identifierStr name) weight in*)
         let ret_state =
            {
               state.data with
               functions = IdentifierMap.add name exp state.data.functions;
               function_weight = IdentifierMap.add name weight state.data.function_weight
            }
         in setState state ret_state
      | _ -> state

(* ======================= *)

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
   | StmtFun(_,fargs,fbody,type_exp,_) ->
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
            match optname,(IdentifierMap.mem fname state.data.functions) with
            | None,_   ->
               state,[exp]
            | _,false   ->
               state,[exp]
            | Some(name),_ ->
               let weight = IdentifierMap.find fname state.data.function_weight in
               if weight > state.data.options.inline_weight then
                  state,[exp]
               else begin
                  let new_stmts = inlineFunctionCall state name fname args loc in
                  match new_stmts with
                  | [] -> state,[]
                  | _  -> state,[PSeq(new_stmts,loc)]
               end
         end
      | _ -> state,[exp]

(* ======================= *)

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

(** Returns Some(e,stmts) if the sequence has a single path until it returns *)
let rec isSinglePathStmtList (acc:exp list) (stmts:exp list) : (exp * exp list) option =
   match stmts with
   | [] -> None
   | [StmtReturn(e,_)] -> Some(e,List.rev acc)
   | h::_ when hasReturn h -> None
   | h::t -> isSinglePathStmtList (h::acc) t

(** Transforms x = {return y;}; -> x = y;  and _ = { stmts; } -> stmts *)
let simplifySequenceBindings : ('data,exp) traverser =
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
let rec removeAllMem : ('data,exp) expander =
   fun state exp ->
      match exp with
      | StmtMem(PId(name,_,_),_,_,_) when IdentifierMap.mem name state.data ->
         state,[]
      | StmtMem(PId(name,_,_),_,_,_)->
         state,[exp]
      | _ -> state,[exp]

(** Removes all the val statememts *)
let rec removeAllVal : ('data,exp) expander =
   fun state exp ->
      match exp with
      | StmtVal(PId(name,_,_),_,_) when IdentifierMap.mem name state.data ->
         state,[]
      | StmtVal(PId(name,_,_),_,_)->
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

(** collects all val statements *)
let collectValDecl : ('data,exp) folder =
   fun state exp ->
      match exp with
      | StmtVal(PId(name,_,_),_,_) when IdentifierMap.mem name state.data ->
         state
      | StmtVal(PId(name,_,_),_,_)->
         setState state (IdentifierMap.add name exp state.data)
      | _ -> state

(** Removes duplicated mem declarations from StmtSequence and moves to the top the val *)
let relocateMemAndVal : ('data,exp) traverser =
   fun state exp ->
      match exp with
      | StmtFun(name,args,body,type_exp,loc) ->
         let inner_state = deriveState state IdentifierMap.empty in
         let mem_decl_map =
            TypesUtil.foldTopExp (Some(skipFun)) collectMemDecl inner_state body
         in
         let _,new_body   =
            TypesUtil.expandStmt (Some(skipFun)) removeAllMem mem_decl_map body in
         let mem_decl     = IdentifierMap.fold (fun _ a acc -> a::acc) mem_decl_map.data [] in
         state,StmtFun(name,args,appendBlocks (mem_decl@new_body),type_exp,loc)
      | StmtBlock(stmts,loc) ->
         let skip a = skipPSeq a && skipBlock a in
         let inner_state  = deriveState state IdentifierMap.empty in
         let val_decl_map =
            TypesUtil.foldTopExpList (Some(skip)) collectValDecl inner_state stmts in
         let _,new_stmts  =
            TypesUtil.expandStmtList (Some(skip)) removeAllVal val_decl_map stmts in
         let val_decl     = IdentifierMap.fold (fun _ a acc -> a::acc) val_decl_map.data [] in
         state,StmtBlock(val_decl@new_stmts,loc)
      | PSeq(stmts,loc) ->
         let skip a = skipPSeq a && skipBlock a in
         let inner_state  = deriveState state IdentifierMap.empty in
         let val_decl_map =
            TypesUtil.foldTopExpList (Some(skip)) collectValDecl inner_state stmts in
         let _,new_stmts  =
            TypesUtil.expandStmtList (Some(skip)) removeAllVal val_decl_map stmts in
         let val_decl     = IdentifierMap.fold (fun _ a acc -> a::acc) val_decl_map.data [] in
         state,PSeq(val_decl@new_stmts,loc)
      | _ -> state,exp

(* ======================= *)
(** Changes if(cond,e1,e2) -> if(cond,{|return e1|},{|return e2|})*)
let makeIfStatement : ('data,exp) traverser =
   fun state exp ->
      match exp with
      | StmtBind(lhs,PIf(cond,then_exp,else_exp,iloc),bloc) ->
         state,StmtIf(cond,StmtBind(lhs,then_exp,iloc),Some(StmtBind(lhs,else_exp,bloc)),iloc)
      | _ -> state,exp

(* ======================= *)

(** Negates a condition*)
let notCondition (exp:exp) =
   match exp with
   | PCall(_,["'!'"],[exp],_,_) -> exp
   | _ ->
      let loc = getExpLocation exp in
      PCall(None,["'!'"],[exp],loc,[])

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

(** Wrapps changes return a; -> if(true) return a; This is useful to apply transforations *)
let wrapSimpleReturn : ('data,exp) traverser =
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
let rec simplifyReturnPaths (ret_var:exp) (stmts:exp list) : exp list =
   match stmts with
   | [] -> []
   | StmtIf(cond,then_stmts,None,loc)::[] when hasReturn then_stmts ->
      [StmtIf(cond,replaceReturn ret_var then_stmts,None,loc)]
   | StmtIf(cond,then_stmts,None,loc)::t when hasReturn then_stmts ->
      let new_t = simplifyReturnPaths ret_var t in
      [StmtIf(notCondition cond,StmtBlock(new_t,loc),Some(replaceReturn ret_var then_stmts),loc)]
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

(** Removes nested block created by the transformations *)
let removeUnnecessaryBlocks : ('data,exp) traverser =
   fun state stmt ->
      match stmt with
      | StmtBlock([h],_) -> state,h
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

(** Simplifies dummy if-statements created by the transfrmations. e.g. if(true) ... or if(a) { if(!a) ...}  *)
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

(** Changes if(!a) stmt1 else stmt2 -> if(a) stmt2 else stmt1 *)
let removeSwapedIfCondition : ('data,exp) traverser =
   fun state stmt ->
      match stmt with
      | StmtIf(PCall(_,["'!'"],[exp],_,_),then_stmt,Some(else_stmt),loc)->
         state,StmtIf(exp,else_stmt,Some(then_stmt),loc)
      | _ -> state,stmt

(** Removes if-statements with empty blocks *)
let removeEmptyIfConditions : ('data,exp) traverser =
   fun state stmt ->
      match stmt with
      | StmtIf(cond,StmtBlock([],_),Some(else_stmt),loc) ->
         state,StmtIf(notCondition cond,else_stmt,None,loc)
      | StmtIf(cond,then_stmt,Some(StmtBlock([],_)),loc) ->
         state,StmtIf(cond,then_stmt,None,loc)
      | _ -> state,stmt

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
      | PSeq(pseq_stmts,loc) when hasIfStmtWithReturnList pseq_stmts ->
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
         state,PSeq(new_stmts,loc)
      | _ -> state,stmt

(* ======================= *)

(** Inlines functions into functions *)
let inlineFunctionBodies (state:'data tstate) (exp_list:exp list) : 'data tstate * exp list =
   let inlineFunctionBody name fun_decl (functions,weigths) =
      match fun_decl with
      | StmtFun(fname,fargs,fbody,type_exp,loc) ->
         let _,new_fbody     = expandStmt None inlineStmts state fbody in
         let new_fbody_block = appendBlocks new_fbody in
         let weight          = getExpWeight new_fbody_block in
         let new_functions   = IdentifierMap.add name (StmtFun(fname,fargs,new_fbody_block,type_exp,loc)) functions in
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

(** Wrapps all the statements into a function called __main__ and calls it *)
let makeFunAndCall state stmts =
   let fcall = ["__main__"] in
   state,[StmtFun(fcall,[],appendBlocks stmts,None,default_loc); StmtReturn(PCall(None,fcall,[],default_loc,[]),default_loc)]

let applyOn cond f data =
   if cond data then
      f data
   else
      data

let returnRemovalON (state,_) = state.data.options.simplify_return
let inlineON        (state,_) = state.data.options.inline
let imperativizeON  (state,_) = state.data.options.imperativize
let finalizeON      (state,_) = state.data.options.finalize
let basicON         (state,_) = state.data.options.basic

let applyTransformations (options:options) (results:parser_results) =
   let initial_state =
      {
         counter         = 0;
         functions       = IdentifierMap.empty;
         function_weight = IdentifierMap.empty;
         options         = options;
         function_mem    = IdentifierMap.empty;
         instances       = IdentifierMap.empty;
         active_function = IdentifierMap.empty;
      } |> createState
   in
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
      |+> TypesUtil.foldAsTransformation collectFunctionDefinitions
      |+> inlineFunctionBodies
      |+> TypesUtil.expandStmtList (Some(skipFun)) inlineStmts
      |+> TypesUtil.foldAsTransformation collectFunctionDefinitions
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
      |+> TypesUtil.foldAsTransformation
         (collectMemInFunctions
         |*> collectFunctionInstances)
      |+> TypesUtil.expandStmtList None createTypes
   in
   let passes stmts =
      (initial_state,[StmtBlock(stmts,default_loc)])
      |> applyOn basicON         basicPasses
      |> applyOn returnRemovalON removalOfReturnPasses
      |> applyOn inlineON        inliningPasses
      |> applyOn imperativizeON  imperativePasses
      |> applyOn finalizeON      finalPasses
      |> snd
   in

   let new_stmts = CCError.map passes results.presult in
   { results with presult = new_stmts }

