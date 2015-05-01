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
open Graphs

(** Removes the instance name for every function call that is static *)
let removeNamesFromStaticFunctions : (pass_state,exp) traverser =
   fun state exp ->
      match exp with
      | PCall(_,fname,args,loc,attr) when isActiveFunction state fname |> not ->
         state,PCall(None,fname,args,loc,attr)
      | PCall(Some(id),fname,args,loc,attr) ->
         state,PCall(None,fname,PUnOp("p&",PId(id,None,loc),loc)::args,loc,attr)
      | _ -> state,exp

(** Creates a table of replacements from the given list of identifiers *)
let createStateReplacements (ids:identifier list) =
   let fold s id =
      let rep = "_st_"::id in
      (*let _ = Printf.printf "Adding replacement %s -> %s\n" (identifierStr id) (identifierStr rep) in*)
      IdentifierMap.add id rep s
   in
   List.fold_left (fun s a -> fold s a) IdentifierMap.empty ids

(** Replaces all identifiers using the given replacements table *)
let replaceIds (replacements:identifier IdentifierMap.t) : ('state,exp) traverser =
   fun state exp ->
      match exp with
      | PId(id,tp,loc) when IdentifierMap.mem id replacements ->
         let new_id = IdentifierMap.find id replacements in
         state,PId(new_id,tp,loc)
      | _ -> state,exp

(** Replaces all references to mem variables to members of the input state *)
let replaceMemAccess : (pass_state,exp) traverser =
   fun state exp ->
      match exp with
      | StmtFun(name,args,body,ret,_,loc) when isActiveFunction state name ->
         let scope         = getScope state in
         let mem_names     =
            getMemDeclarations state scope
            |> List.map (fun a -> getIdAndType a|>fst)
         in
         let instance_names = getInstanceNames state scope in
         let replacements   = createStateReplacements (mem_names@instance_names) in
         let _,new_body     = traverseBottomExp (Some(skipFun)) (replaceIds replacements) (createState ()) body in
         state,StmtFun(name,args,new_body,ret,true,loc)
      | _ -> state,exp

(** Sets the instance name as argument to the function *)
let makeInstanceArgument (module_name:string) : (pass_state,exp) traverser =
   fun state exp ->
      match exp with
      | StmtFun(name,args,body,ret,true,loc) ->
         begin
            match getFinalType module_name state name with
            | Some(ftype) ->
               let arg = NamedId(["_st_"],PId(ftype,None,loc),loc) in
               state,StmtFun(name,arg::args,body,ret,true,loc)
            | _ ->
               let arg = SimpleId(["_st_"],loc) in
               state,StmtFun(name,arg::args,body,ret,true,loc)
         end
      | _ -> state,exp

(** Changes all the function calls  their full name *)
let makeCallsFullName : (pass_state,exp) traverser =
   fun state exp ->
      match exp with
      | PCall(name,fname,args,loc,attr) ->
         begin
            match lookupFunctionName state.data.functions state fname with
            |  Some(full_name) ->
               let flat_name = joinSep "_" full_name in
               state,PCall(name,[flat_name],args,loc,attr)
            | _ -> state,exp
         end
      | _ -> state,exp

let makeFunDeclFullName : (pass_state,exp) traverser =
   fun state exp ->
      match exp with
      | StmtFun(name,args,body,ret,active,loc) ->
         let full_name = getScope state |> joinSep "_" in
         state,StmtFun([full_name],args,body,ret,active,loc)
      | _ -> state,exp

(** Returns the dependencies of a type declaration *)
let returnTypeDependencies (tp:exp) : identifier list =
   match tp with
   | StmtType(_,_,members,_) ->
      List.map (fun (_,t,_) -> getIdAndType t |> fst) members
   | StmtAliasType(_,_,t,_) -> [getIdAndType t |> fst]
   | _ -> []

(** Takes a list of type declarations and returns it sorted based on their dependencies *)
let sortTypes (types:exp list) : exp list =
   let vertex,edges,decls = List.fold_left (fun (vertex,edges,decls) (a:exp) ->
         let name = getTypeName a in
         let dependencies =
            returnTypeDependencies a
            |> List.filter (fun a -> isBuiltinType a |> not)
         in
         let _ = Hashtbl.add edges name dependencies in
         let _ = Hashtbl.add decls name a in
         (name::vertex),edges,decls
      ) ([],Hashtbl.create 100,Hashtbl.create 100) types
   in
   let g = Graphs.createGraph vertex edges in
   let components = calculateComponents g in
   let sorted_names = List.flatten components in
   List.map (fun name -> Hashtbl.find decls name) sorted_names

let removeSubFunctions_expander : (unit,exp) expander =
   fun state exp ->
      match exp with
      | StmtFun(_,_,_,_,_,_) -> state,[]
      | _ -> state,[exp]

let removeSubFunctions state stmts =
   let apply stmt =
      match stmt with
      | StmtFun(name,args,body,ret,active,loc) ->
         let new_body = TypesUtil.expandStmt None removeSubFunctions_expander (createState ()) body |> snd in
         StmtFun(name,args,makeStmtBlock loc new_body,ret,active,loc)
      | _ -> stmt
   in
   state, List.map apply stmts

let flattenDefinitions state stmts =
   let function_definitions =
      IdentifierMap.to_list state.data.functions
      |> List.map snd
   in
   let type_definitions =
      IdentifierMap.to_list state.data.types
      |> List.map snd
      |> sortTypes
   in
   state,type_definitions@function_definitions

let clearFunctionDefinitions state stmts =
   { state with data = { state.data with functions = IdentifierMap.empty; function_weight = IdentifierMap.empty } }, stmts

let codeGenPasses module_name state =
   state
   |+> TypesUtil.foldAsTransformation None collectFunctionDefinitions
   |+> TypesUtil.traverseBottomExpList None
      (removeNamesFromStaticFunctions
       |-> replaceMemAccess
       |-> makeInstanceArgument module_name
       |-> makeCallsFullName
       |-> makeFunDeclFullName)
   (* Collects again the functions calls in order to move them to the top scope *)
   |+> clearFunctionDefinitions
   |+> TypesUtil.foldAsTransformation None
      (collectFunctionDefinitions
      |*> collectTypeDefinitions
      |*> collectTableDefinitions)
   |+> flattenDefinitions
   |+> removeSubFunctions

