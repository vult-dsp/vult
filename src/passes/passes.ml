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
open VEnv

open PassCommon


module CreateTupleTypes = struct

   type 'a dependencies = ('a * 'a list) list

   let getSubTuples (t:VType.t) : VType.t list =
      VType.getSubTypes t |> List.filter VType.isTuple

   let makeTypeDeclaration (t:VType.t) : stmt =
      match !t with
      | VType.TComposed(["tuple"],types,_) ->
         let elems = List.mapi (fun i a -> ["field_"^(string_of_int i)],a,emptyAttr) types in
         StmtType(t,elems,emptyAttr)
      | _ -> failwith "CreateTupleTypes.makeTypeDeclaration: there should be only tuples here"

   let rec getDeclarations dependencies visited remaining : VType.t dependencies=
      match remaining with
      | [] ->
         Hashtbl.fold (fun a b acc -> (a,b)::acc) dependencies []
      | h::t when TypeSet.mem h visited ->
         getDeclarations dependencies visited t
      | h::t ->
         let sub = getSubTuples h in
         let visited' = TypeSet.add h visited in
         let () = Hashtbl.add dependencies h sub in
         getDeclarations dependencies visited' (sub@t)

   let rec checkCircularDepedencies components =
      match components with
      | [] -> ()
      | [_]::t -> checkCircularDepedencies t
      | types::_ ->
         let types_str = List.map PrintTypes.typeStr types |> String.concat ", " in
         let msg = "The following tuple types have circular dependencies: " ^ types_str in
         Error.raiseErrorMsg msg

   let run (state,stmts) =
      let data = Env.get state in
      let tuples = TypeSet.elements (PassData.getTuples data) |> List.map VType.unlink in
      let dependencies = getDeclarations (Hashtbl.create 8) TypeSet.empty tuples in
      let components = Components.components dependencies in
      let sorted = List.map List.hd components in
      let decl = List.map makeTypeDeclaration sorted in
      state, decl @ stmts

end

(* Basic transformations *)
let inferPass (name:id) (state,stmts) =
   let state' = Env.enter Scope.Module state name emptyAttr in
   let stmts,state',_ = Inference.inferStmtList state' Inference.NoType stmts in
   let state' = Env.exit state' in
   state', stmts

let interPass (name:id) (state,stmts) =
   let data = Env.get state in
   Interpreter.Env.addModule data.PassData.interp_env name;
   let env' = Interpreter.Env.enterModule data.PassData.interp_env name in
   Interpreter.loadStmts env' stmts;
   state, stmts


let rec applyPassRepeat name apply pass pass_name (state,stmts) =
   if Mapper.log then print_endline ("Running "^pass_name);
   if apply then
      let state',stmts' = Mapper.map_stmt_list pass state stmts in
      if shouldReapply state' then
         applyPassRepeat name apply pass pass_name (reset state',stmts')
      else
         state',stmts'
   else
      state,stmts

let applyPass name apply pass pass_name (state,stmts) =
   let state' = Env.enter Scope.Module state name emptyAttr in
   let state', stmts' = applyPassRepeat name apply pass pass_name (state',stmts) in
   let state' = Env.exit state' in
   state',stmts'

let passes (name:id) (options:pass_options) (env,stmts) =
   (env,stmts)
   |> inferPass name
   |> interPass name
   |> applyPass name options.pass1 Pass1.run "pass 1"
   |> applyPass name options.pass2 Pass2.run "pass 2"
   |> applyPass name options.pass3 Pass3.run "pass 3"
   |> applyPass name options.pass4 Pass4.run "pass 4"
   |> applyPass name options.pass5 Pass5.run "pass 5"
   |> CreateTupleTypes.run

let apply env options (results:parser_results) =
   let module_name = [moduleName results.file] in
   passes module_name options (env,results.presult)


let applyTransformations args ?(options=default_options) (results:parser_results list) =
   let env = Env.empty (PassData.empty args) in
   let _,stmts_list =
      List.fold_left
         (fun (env,acc) stmts ->
             let env', stmts' = apply env options stmts in
             let result' = { stmts with presult = stmts' } in
             env', result'::acc
         )
         (env,[])
         results
   in
   List.rev stmts_list

let applyTransformationsSingle args ?(options=default_options) (results:parser_results) =
   let env = Env.empty (PassData.empty args) in
   let _,stmts' = apply env options results in
   { results with presult = stmts' }
