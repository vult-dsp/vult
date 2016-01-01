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
open Scope
open TypesUtil

exception CheckerError of Error.t

module ScopeInfo =
struct
   type t    = identifier
   type v    = type_exp
   type kind = scope_kind
   let compare     = compare_identifier
   let string_t _  = ""
   let string_v _  = ""
end

module CheckerScope = Scope(ScopeInfo)
module StringMap    = Map.Make(String)

(** Checks that the type is a numeric, otherwise raises CheckerError *)
let expectNumeric (t:type_exp) : type_exp =
   match t with
   | TId(["int"],_)  -> t
   | TId(["real"],_) -> t
   | _ ->
      let msg   = Printf.sprintf "Expecting a numeric (int, real) expression but got '%s'" (PrintTypes.typeStr t) in
      let error = Error.makeError msg (getFullTypeLocation t) in
      raise (CheckerError(error))

(** Checks that the type is a bool, otherwise raises CheckerError *)
let expectBoolean (t:type_exp) : type_exp =
   match t with
   | TId(["bool"],_) -> t
   | _ ->
      let msg   = Printf.sprintf "Expecting a 'bool' expression but got '%s'" (PrintTypes.typeStr t) in
      let error = Error.makeError msg (getFullTypeLocation t) in
      raise (CheckerError(error))

(*let expectBooleanPair ((state:CheckerScope.t),(t:type_exp)) : CheckerScope.t *  type_exp * Loc.t =
   expectBoolean state t*)

(** Checks that both types are numeric, otherwise raises CheckerError *)
let arithmeticFunction (t1:type_exp) (t2:type_exp) : type_exp =
   match t1,t2 with
   | TId(["int"],_),TId(["int"],_)   -> t1
   | TId(["real"],_),TId(["int"],_)  -> t1
   | TId(["int"],_),TId(["real"],_)  -> t2
   | TId(["real"],_),TId(["real"],_) -> t1
   | TId(["int"],_),_
   | TId(["real"],_),_ ->
      let msg   = Printf.sprintf "Expecting a numeric (int, real) expression but got '%s'" (PrintTypes.typeStr t2) in
      let error = Error.makeError msg (getFullTypeLocation t2) in
      raise (CheckerError(error))
   | _,TId(["int"],_)
   | _,TId(["real"],_) ->
      let msg   = Printf.sprintf "Expecting a numeric (int, real) expression but got '%s'" (PrintTypes.typeStr t1) in
      let error = Error.makeError msg (getFullTypeLocation t1) in
      raise (CheckerError(error))
   | _ ->
      let msg   = Printf.sprintf "Arithmetic operations cannot be applied to types '%s' and '%s" (PrintTypes.typeStr t1) (PrintTypes.typeStr t2) in
      let loc   = Loc.merge (getFullTypeLocation t1) (getFullTypeLocation t2) in
      let error = Error.makeError msg loc in
      raise (CheckerError(error))

(** Succeeds if both types are the same *)
let sameType (t1:type_exp) (t2:type_exp) : type_exp =
   if compare_type_exp t1 t2 = 0 then
      t1
   else
      let msg   = Printf.sprintf "Expecting a expression of type '%s' but got '%s'" (PrintTypes.typeStr t1) (PrintTypes.typeStr t2) in
      let error = Error.makeError msg (getFullTypeLocation t2) in
      raise (CheckerError(error))

let comparisonFunction (t1:type_exp) (t2:type_exp) : type_exp =
   if compare_type_exp t1 t2 = 0 then
      TId(["bool"],makeAttr Loc.default)
   else
      let msg   = Printf.sprintf "Cannot compare an expression of type '%s' with an expression of type '%s'" (PrintTypes.typeStr t1) (PrintTypes.typeStr t2) in
      let error = Error.makeError msg (getFullTypeLocation t2) in
      raise (CheckerError(error))

let unop_table =
   [
      "-",expectNumeric;
      "!",expectBoolean;
   ]
   |> List.fold_left (fun s (op,f) -> StringMap.add op f s) StringMap.empty

let binop_table =
   [
      "+",arithmeticFunction;
      "-",arithmeticFunction;
      "/",arithmeticFunction;
      "*",arithmeticFunction;
      "==",comparisonFunction;
      "<>",comparisonFunction;
      "<",comparisonFunction;
      ">",comparisonFunction;
      "<=",comparisonFunction;
      ">=",comparisonFunction;
   ]
   |> List.fold_left (fun s (op,f) -> StringMap.add op f s) StringMap.empty

let matchTypes (t1:type_exp) (t2:type_exp) : type_exp =
   match t1,t2 with
   | TWild(_),TWild(_) -> t1
   | TWild(_),_        -> t2
   | _,TWild(_)        -> t1
   | TUnit(_),TUnit(_) -> t1
   | TId(id1,_),TId(id2,_) when id1 = id2 ->
      t1

let rec checkLhsExp (exp:lhs_exp) : type_exp =
   match exp with
   | LWild(attr) -> TWild(attr)
   | LId(_,attr) -> TWild(attr)
   | LTuple(elems,attr) ->
      let tl = List.map checkLhsExp elems in
      TTuple(tl,attr)
   | LTyped(e1,t,attr) ->
      let e1t = checkLhsExp e1 in
      (* match the types*)
      t


let rec checkExp (state:CheckerScope.t) (exp:exp) : CheckerScope.t *  type_exp * Loc.t =
   match exp with
   | PUnit(attr)   -> state,TUnit(attr),       attr.loc
   | PBool(_,attr) -> state,TId(["bool"],attr),attr.loc
   | PInt(_,attr)  -> state,TId(["int"],attr), attr.loc
   | PReal(_,attr) -> state,TId(["real"],attr),attr.loc
   | PId(id,attr)  ->
      begin
         match CheckerScope.lookup state id with
         | Some(id_type) -> state,id_type,attr.loc
         | _ ->
            let msg   = Printf.sprintf "The identifier '%s' is not declared" (identifierStr id) in
            let error = Error.makeError msg attr.loc in
            raise (CheckerError(error))
      end
   | PUnOp(op,e1,attr) when StringMap.mem op unop_table ->
      let new_state,e1_type,loc1 = checkExp state e1 in
      let op_f = StringMap.find op unop_table in
      let nt   = op_f e1_type in
      new_state,nt,(Loc.merge attr.loc loc1)
   | PUnOp(op,_,attr) ->
      let msg   = Printf.sprintf "Unknown operator '%s'" op in
      let error = Error.makeError msg attr.loc in
      raise (CheckerError(error))
   | PBinOp(op,e1,e2,attr) when StringMap.mem op binop_table ->
      let new_state1,e1_type,loc1 = checkExp state e1 in
      let new_state2,e2_type,loc2 = checkExp new_state1 e2 in
      let op_f = StringMap.find op binop_table in
      let nt   = op_f e1_type e2_type in
      new_state2,nt,(Loc.merge3 attr.loc loc1 loc2)
   | PBinOp(op,_,_,attr) ->
      let msg   = Printf.sprintf "Unknown operator '%s'" op in
      let error = Error.makeError msg attr.loc in
      raise (CheckerError(error))
   | PIf(cond,e1,e2,attr) ->
      let new_state,cond_type,loc1 = checkExp state cond in
      let _ = expectBoolean cond_type in
      let new_state1,e1_type,loc2 = checkExp new_state e1 in
      let new_state2,e2_type,loc3 = checkExp new_state1 e2 in
      let loc = Loc.merge (Loc.merge3 loc1 loc2 loc3) attr.loc in
      let nt  = sameType e1_type e2_type in
      new_state2,nt,loc
   | PGroup(e1,attr) ->
      let new_state,e1_type,loc1 = checkExp state e1 in
      new_state,e1_type,Loc.merge attr.loc loc1
   | PTuple(elems,attr) ->
      let new_state,elems_type,loc1 = checkExpList state elems in
      let loc = Loc.merge loc1 attr.loc in
      let t = TTuple(elems_type,makeAttr loc) in
      new_state,t,loc
   | PSeq(_,_,_) -> failwith "Cannot check PSeq yet."
   | PCall(_,_,_,_) -> failwith "Cannot check PCall yet."
   | PEmpty -> state,TUnit(makeAttr Loc.default),Loc.default

and checkExpList (state:CheckerScope.t) (el:exp list) : CheckerScope.t *  type_exp list * Loc.t =
   let new_state,acc,loc =
      List.fold_left (fun (s,acc,loc) e -> let ns,t,nloc = checkExp state e in ns,t::acc,(Loc.merge loc nloc)) (state,[],Loc.default) el in
   new_state,List.rev acc,loc


let empty = CheckerScope.empty

let parseCheck s =
   try
      let e = ParserVult.parseExp s in
      let _ ,t,loc = checkExp empty e in
      (PrintTypes.typeStr t),(Loc.to_string loc)
   with
   | CheckerError(error) ->
      "-",Error.reportErrorString [|s|] error

