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

open Prog
open Env
open Maps
open PassCommon

module CreateTupleTypes = struct
   type 'a dependencies = ('a * 'a list) list

   let getSubTuples (t : Typ.t) : Typ.t list = Typ.getSubTypes t |> List.filter Typ.isTuple

   let makeTypeDeclaration (t : Typ.t) : stmt =
      match !t with
      | Typ.TComposed ([ "tuple" ], types, _) ->
         let elems = List.mapi (fun i a -> [ "field_" ^ string_of_int i ], a, emptyAttr) types in
         StmtType (t, elems, emptyAttr)
      | _ -> failwith "CreateTupleTypes.makeTypeDeclaration: there should be only tuples here"
   ;;

   let rec getDeclarations dependencies visited remaining : Typ.t dependencies =
      match remaining with
      | [] -> Hashtbl.fold (fun a b acc -> (a, b) :: acc) dependencies []
      | h :: t when TypeSet.mem h visited -> getDeclarations dependencies visited t
      | h :: t ->
         let sub = getSubTuples h in
         let visited' = TypeSet.add h visited in
         let () = Hashtbl.add dependencies h sub in
         getDeclarations dependencies visited' (sub @ t)
   ;;

   let rec checkCircularDepedencies components =
      match components with
      | [] -> ()
      | [ _ ] :: t -> checkCircularDepedencies t
      | types :: _ ->
         let types_str = List.map PrintProg.typeStr types |> String.concat ", " in
         let msg = "The following tuple types have circular dependencies: " ^ types_str in
         Error.raiseErrorMsg msg
   ;;

   let run state =
      let data = Env.get state in
      let tuples = TypeSet.elements (PassData.getTuples data) |> List.map Typ.unlink in
      let dependencies = getDeclarations (Hashtbl.create 8) TypeSet.empty tuples in
      let components = Components.components dependencies in
      let sorted = List.map List.hd components in
      let decl = List.map makeTypeDeclaration sorted in
      decl
   ;;
end

(* Basic transformations *)
let inferPass (name : Id.t) (state, stmts) =
   let state' = Env.enter Scope.Module state name emptyAttr in
   let stmts, state', _ = Inference.inferStmtList state' Inference.NoType stmts in
   let state' = Env.exit state' in
   state', stmts
;;

let foldPassAll pass state (results : parser_results list) =
   let state, rev =
      List.fold_left
         (fun (state, acc) result ->
             let name = [ moduleName result.file ] in
             let state, presult = pass name (state, result.presult) in
             state, { result with presult } :: acc)
         (state, [])
         results
   in
   state, List.rev rev
;;

let interPass (name : Id.t) (state, stmts) =
   let data = Env.get state in
   Interpreter.Env.addModule data.PassData.interp_env name;
   let env' = Interpreter.Env.enterModule data.PassData.interp_env name in
   Interpreter.loadStmts env' stmts;
   state, stmts
;;

let rec applyPassLog apply pass pass_name (state, stmts) =
   if Mapper.log then print_endline ("Running " ^ pass_name);
   if apply then Mapper.map_stmt_list pass state stmts else state, stmts
;;

let applyPass name apply pass pass_name (state, stmts) =
   let state' = Env.enter Scope.Module state name emptyAttr in
   let state', stmts' = applyPassLog apply pass pass_name (state', stmts) in
   let state' = Env.exit state' in
   state', stmts'
;;

let inferPassAll = foldPassAll inferPass

let pass1All options =
   foldPassAll (fun name (state, stmts) -> applyPass name options.pass1 Pass1.run "pass 1" (state, stmts))
;;

let pass2All options =
   foldPassAll (fun name (state, stmts) -> applyPass name options.pass2 Pass2.run "pass 2" (state, stmts))
;;

let pass3All options =
   foldPassAll (fun name (state, stmts) -> applyPass name options.pass3 Pass3.run "pass 3" (state, stmts))
;;

let pass4All options =
   foldPassAll (fun name (state, stmts) -> applyPass name options.pass4 Pass4.run "pass 4" (state, stmts))
;;

let pass5All options =
   foldPassAll (fun name (state, stmts) -> applyPass name options.pass5 Pass5.run "pass 5" (state, stmts))
;;

let interAll = foldPassAll (fun name (state, stmts) -> interPass name (state, stmts))

let rec exhaustPass pass (env, results) =
   let env, results = pass env results in
   if shouldReapply env then exhaustPass pass (reset env, results) else env, results
;;

let getExtensions (args : Args.args) =
   match args with
   | { code = LuaCode; template = "vcv-prototype" } -> Some `VCVPrototype
   | _ -> None
;;

let passesAll args ?(options = default_options) results =
   let extensions = getExtensions args in
   let env = Env.empty ~extensions (PassData.empty args) in
   (env, results)
   |> exhaustPass inferPassAll
   |> exhaustPass (pass1All options)
   |> exhaustPass interAll
   |> exhaustPass (pass2All options)
   |> exhaustPass (pass3All options)
   |> exhaustPass (pass4All options)
   |> exhaustPass (pass5All options)
;;

let applyTransformations args ?(options = default_options) (results : parser_results list) =
   let env, stmts_list = passesAll args ~options results in
   let data = Env.get env in
   let used = data.used_code in
   if options.tuples
   then (
      let tuples = { presult = CreateTupleTypes.run env; file = "" } in
      tuples :: stmts_list, used)
   else stmts_list, used
;;

let applyTransformationsSingle args ?(options = default_options) (results : parser_results) =
   let env, stmts' = passesAll args ~options [ results ] in
   let stmts' = List.map (fun a -> a.presult) stmts' |> List.flatten in
   let tuples = CreateTupleTypes.run env in
   { results with presult = tuples @ stmts' }
;;
