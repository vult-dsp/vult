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

let removeNamesFromStaticFunctions : (pass_state,exp) traverser =
   fun state exp ->
      match exp with
      | PCall(_,fname,args,loc,attr) when isActiveFunction state fname |> not ->
         state,PCall(None,fname,args,loc,attr)
      | PCall(Some(id),fname,args,loc,attr) ->
         state,PCall(None,fname,PUnOp("p&",PId(id,None,loc),loc)::args,loc,attr)
      | _ -> state,exp


let createStateReplacements (ids:identifier list) =
   let fold s id =
      let rep = "_st_"::id in
      (*let _ = Printf.printf "Adding replacement %s -> %s\n" (identifierStr id) (identifierStr rep) in*)
      IdentifierMap.add id rep s
   in
   List.fold_left (fun s a -> fold s a) IdentifierMap.empty ids

let replaceIds (replacements:identifier IdentifierMap.t) : ('state,exp) traverser =
   fun state exp ->
      match exp with
      | PId(id,tp,loc) when IdentifierMap.mem id replacements ->
         let new_id = IdentifierMap.find id replacements in
         state,PId(new_id,tp,loc)
      | _ -> state,exp

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

let makeInstanceArgument : (pass_state,exp) traverser =
   fun state exp ->
      match exp with
      | StmtFun(name,args,body,ret,true,loc) ->
         begin
            match getFinalType state name with
            | Some(ftype) ->
               let arg = NamedId(["_st_"],PId(ftype,None,loc),loc) in
               state,StmtFun(name,arg::args,body,ret,true,loc)
            | _ ->
               let arg = SimpleId(["_st_"],loc) in
               state,StmtFun(name,arg::args,body,ret,true,loc)
         end
      | _ -> state,exp

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

(** Changes if(cond,e1,e2) -> if(cond,{|return e1|},{|return e2|})*)
let makeIfStatement : ('data,exp) traverser =
   fun state exp ->
      match exp with
      | StmtBind(lhs,PIf(cond,then_exp,else_exp,iloc),bloc) ->
         state,StmtIf(cond,StmtBind(lhs,then_exp,iloc),Some(StmtBind(lhs,else_exp,bloc)),iloc)
      | _ -> state,exp

let codeGenPasses state =
   state
   |+> TypesUtil.foldAsTransformation None collectFunctionDefinitions
   |+> TypesUtil.traverseBottomExpList None makeIfStatement
   |+> TypesUtil.traverseBottomExpList None
      (removeNamesFromStaticFunctions
      |->replaceMemAccess
      |->makeInstanceArgument
      |->makeCallsFullName)

