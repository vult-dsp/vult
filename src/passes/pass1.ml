(*
   The MIT License (MIT)

   Copyright (c) 2014 Leonardo Laguna Ruiz

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

open PassCommon
open Env
open Prog
open Common
open Maps

module UnlinkTypes = struct
   let vtype_c : (PassData.t Env.t, Typ.vtype) Mapper.mapper_func =
      Mapper.make "UnlinkTypes.vtype_c"
      @@ fun state typ ->
      match typ with
      | Typ.TLink t -> state, !t
      | _ -> state, typ
   ;;

   let mapper = Mapper.{ default_mapper with vtype_c }
end

module ReportUnboundTypes = struct
   let reportUnbound (name : Id.t) (attr : attr) =
      let msg = Printf.sprintf "The type of variable '%s' cannot be inferred. Add a type annotation." (Id.show name) in
      Error.raiseError msg attr.loc
   ;;

   let lhs_exp : ('a Env.t, lhs_exp) Mapper.mapper_func =
      Mapper.make "ReportUnboundTypes.lhs_exp"
      @@ fun state exp ->
      match exp with
      | LId (id, None, attr) -> reportUnbound id attr
      | LId (id, Some t, attr) when Typ.isUnbound t -> reportUnbound id attr
      | _ -> state, exp
   ;;

   let exp : ('a Env.t, exp) Mapper.mapper_func =
      Mapper.make "ReportUnboundTypes.exp"
      @@ fun state exp ->
      match exp with
      | PId (id, ({ typ = Some t } as attr)) when Typ.isUnbound t -> reportUnbound id attr
      | _ ->
         let attr = GetAttr.fromExp exp in
         (match attr.typ with
          | Some t when Typ.isUnbound t ->
             let msg = Printf.sprintf "The type of this expression could not be inferred. Add a type annotation." in
             Error.raiseError msg attr.loc
          | _ -> state, exp)
   ;;

   let typed_id : ('a Env.t, typed_id) Mapper.mapper_func =
      Mapper.make "ReportUnboundTypes.typed_id"
      @@ fun state t ->
      match t with
      | TypedId (id, typ, _, attr) when Typ.isUnbound (Typ.first typ) -> reportUnbound id attr
      | _ -> state, t
   ;;

   let mapper = Mapper.{ default_mapper with lhs_exp; exp; typed_id }
end

(** Splits mem declarations with binding to two statements *)
module SplitMem = struct
   let stmt_x : ('a Env.t, stmt) Mapper.expand_func =
      Mapper.makeExpander "SplitMem.stmt_x"
      @@ fun state stmt ->
      match stmt with
      | StmtMem (lhs, Some rhs, attr) -> reapply state, [ StmtMem (lhs, None, attr); StmtBind (lhs, rhs, attr) ]
      | StmtVal (lhs, Some rhs, attr) -> reapply state, [ StmtVal (lhs, None, attr); StmtBind (lhs, rhs, attr) ]
      | _ -> state, [ stmt ]
   ;;

   let mapper = Mapper.{ default_mapper with stmt_x }
end

(** Checks that a variable was not declared as mem and val at the same time *)
module ConflictingDeclarations = struct
   let rec checkLhs env lhs : unit =
      match lhs with
      | LWild _ -> ()
      | LTuple (elems, _) -> List.iter (checkLhs env) elems
      | LTyped (lhs, _, _) -> checkLhs env lhs
      | LGroup (lhs, _) -> checkLhs env lhs
      | LIndex (id, _, _, attr) | LId (id, _, attr) ->
         (match Env.lookupVariable env id with
          | Some decl ->
             if Loc.isSameLoc attr.loc decl.Scope.loc
             then ()
             else (
                let msg =
                   Printf.sprintf
                      "Redefinition of symbol '%s'. Previously defined at %s"
                      (Id.show id)
                      (Loc.to_string_readable decl.Scope.loc)
                in
                Error.raiseError msg attr.loc)
          | _ -> ())
   ;;

   let stmt : ('a Env.t, stmt) Mapper.mapper_func =
      Mapper.make "ConflictingDeclarations.stmt"
      @@ fun state stmt ->
      match stmt with
      | StmtVal (lhs, _, _) ->
         let () = checkLhs state lhs in
         state, stmt
      | _ -> state, stmt
   ;;

   let mapper = Mapper.{ default_mapper with stmt }
end

module Simplify = struct
   (** Returns the sub elements of an operator, e.g. a+(b+c) -> [a, b, c] *)
   let rec getOpElements (op : string) (elems : exp list) : bool * exp list =
      match elems with
      | [] -> false, []
      | POp (op', sub, _) :: t when op' = op ->
         let _, t' = getOpElements op t in
         true, sub @ t'
      | h :: t ->
         let found, t' = getOpElements op t in
         found, h :: t'
   ;;

   let isNum (e : exp) : bool =
      match e with
      | PInt _ | PReal _ | PBool _ -> true
      | _ -> false
   ;;

   let isZero (e : exp) : bool =
      match e with
      | PInt (0, _) | PReal (0.0, _, _) -> true
      | _ -> false
   ;;

   let isOne (e : exp) : bool =
      match e with
      | PInt (1, _) | PReal (1.0, _, _) -> true
      | _ -> false
   ;;

   let isTrue (e : exp) : bool =
      match e with
      | PBool (true, _) -> true
      | _ -> false
   ;;

   let isFalse (e : exp) : bool =
      match e with
      | PBool (false, _) -> true
      | _ -> false
   ;;

   let minusOne attr (typ : Typ.t) : exp =
      match !typ with
      | Typ.TId ([ "int" ], _) -> PInt (-1, attr)
      | Typ.TId ([ "real" ], _) -> PReal (-1.0, Float, attr)
      | Typ.TId ([ "fix16" ], _) -> PReal (-1.0, Fix16, attr)
      | _ -> failwith "Simplify.minusOne: invalid numeric value"
   ;;

   let applyOp (op : string) (e1 : exp) (e2 : exp) : exp =
      match op, e1, e2 with
      | "+", PInt (n1, attr), PInt (n2, _) -> PInt (n1 + n2, attr)
      | "*", PInt (n1, attr), PInt (n2, _) -> PInt (n1 * n2, attr)
      | "+", PReal (n1, p1, attr), PReal (n2, p2, _) when p1 = p2 -> PReal (n1 +. n2, p1, attr)
      | "*", PReal (n1, p1, attr), PReal (n2, p2, _) when p1 = p2 -> PReal (n1 *. n2, p1, attr)
      | "||", PBool (n1, attr), PBool (n2, _) -> PBool (n1 || n2, attr)
      | "&&", PBool (n1, attr), PBool (n2, _) -> PBool (n1 && n2, attr)
      | _ -> failwith "Simplify.applyOp: invalid operation on"
   ;;

   let rec simplifyElems op (elems : exp list) : bool * exp list =
      let constants, other = List.partition isNum elems in
      match constants with
      | [] -> false, elems
      | [ c ] when isZero c && op = "*" -> false, [ c ]
      | [ c ] when isOne c && op = "*" -> false, other
      | [ c ] when isZero c && op = "+" -> false, other
      | [ c ] when isTrue c && op = "||" -> false, [ c ]
      | [ c ] when isFalse c && op = "||" -> false, other
      | [ c ] when isTrue c && op = "&&" -> false, other
      | [ c ] when isFalse c && op = "&&" -> false, [ c ]
      | [ _ ] -> false, elems
      | h :: t ->
         let c = List.fold_left (applyOp op) h t in
         true, c :: other
   ;;

   let negNum (e : exp) : exp =
      match e with
      | PInt (value, attr) -> PInt (-value, attr)
      | PReal (value, p, attr) -> PReal (-.value, p, attr)
      | _ -> failwith "Simplify.negNum: not a number"
   ;;

   let exp : ('a Env.t, exp) Mapper.mapper_func =
      Mapper.make "Simplify.exp"
      @@ fun state exp ->
      match exp with
      | POp ("/", [ e1; PReal (value, p, attr) ], attr2) ->
         reapply state, POp ("*", [ e1; PReal (1.0 /. value, p, attr) ], attr2)
      | POp ("-", [ e1; e2 ], attr) when isNum e2 -> reapply state, POp ("+", [ e1; negNum e2 ], attr)
      | POp ("-", [ e1; PUnOp ("-", e2, _) ], attr) -> reapply state, POp ("+", [ e1; e2 ], attr)
      | POp ("-", [ e1; e2 ], attr) -> reapply state, POp ("+", [ e1; PUnOp ("-", e2, attr) ], attr)
      | PUnOp ("-", POp ("*", elems, ({ typ = Some t } as attr)), _) when List.exists isNum elems ->
         let minus = minusOne attr t in
         reapply state, POp ("*", minus :: elems, attr)
      (* Collapses trees of sums and multiplications *)
      | POp (op, elems, attr) when op = "+" || op = "*" || op = "||" || op = "&&" ->
         let found, elems' = getOpElements op elems in
         let simpl, elems' = simplifyElems op elems' in
         let state' = if found || simpl then reapply state else state in
         let exp' =
            match elems' with
            | [] -> failwith "Passes.Simplify.exp"
            | [ e ] -> e
            | _ -> POp (op, elems', attr)
         in
         state', exp'
      (* Simplifies unary minus *)
      | PUnOp ("-", e1, _) when isNum e1 -> reapply state, negNum e1
      (* Simplifies constant condition *)
      | PIf (cond, then_, else_, _) ->
         if isTrue cond then reapply state, then_ else if isFalse cond then reapply state, else_ else state, exp
      | _ -> state, exp
   ;;

   let stmt_x : ('a Env.t, stmt) Mapper.expand_func =
      Mapper.makeExpander "Simplify.stmt_x"
      @@ fun state stmt ->
      match stmt with
      | StmtIf (PBool (true, _), then_, _, _) -> reapply state, [ then_ ]
      | StmtIf (PBool (false, _), _, Some else_, _) -> reapply state, [ else_ ]
      | StmtIf (PBool (false, _), _, None, _) -> reapply state, []
      | _ -> state, [ stmt ]
   ;;

   let mapper = Mapper.{ default_mapper with exp; stmt_x }
end

module SimplifyIfExp = struct
   let isSimpleCond (e : exp) : bool =
      match e with
      | PBool _ | PId _ -> true
      | _ -> false
   ;;

   (** This mapper is used to bind the if expressions to a variable *)
   module BindIfExp = struct
      let exp : (stmt list Env.t, exp) Mapper.mapper_func =
         Mapper.make "BindIfExp.exp"
         @@ fun state exp ->
         match exp with
         | PIf (_, _, _, attr) when not (Env.insideIf state) ->
            let n, state' = Env.tick state in
            let var_name = "_if_" ^ string_of_int n in
            let exp' = PId ([ var_name ], attr) in
            let lhs = LId ([ var_name ], attr.typ, attr) in
            let decl = StmtVal (lhs, None, emptyAttr) in
            let bind = StmtBind (lhs, exp, emptyAttr) in
            let acc = Env.get state' in
            let state' = Env.set state' (bind :: decl :: acc) in
            state', exp'
         | _ -> state, exp
      ;;

      let mapper = { Mapper.default_mapper with Mapper.exp }
   end

   let stmt_x : ('a Env.t, stmt) Mapper.expand_func =
      Mapper.makeExpander "SimplifyIfExp.stmt_x"
      @@ fun state stmt ->
      match stmt with
      | StmtIf (cond, then_, else_, attr) when not (isSimpleCond cond) ->
         let n, state' = Env.tick state in
         let var_name = "_cond_" ^ string_of_int n in
         let cond_attr = GetAttr.fromExp cond in
         let lhs = LId ([ var_name ], cond_attr.typ, cond_attr) in
         let cond' = PId ([ var_name ], cond_attr) in
         let decl = StmtVal (lhs, None, attr) in
         let bind = StmtBind (lhs, cond, attr) in
         reapply state', [ decl; bind; StmtIf (cond', then_, else_, attr) ]
      | StmtBind (lhs, PIf (cond, then_, else_, ifattr), attr) ->
         reapply state, [ StmtIf (cond, StmtBind (lhs, then_, ifattr), Some (StmtBind (lhs, else_, ifattr)), attr) ]
      | StmtVal (lhs, Some (PIf (cond, then_, else_, ifattr)), attr) ->
         let decl = StmtVal (lhs, None, attr) in
         let if_ = StmtIf (cond, StmtBind (lhs, then_, ifattr), Some (StmtBind (lhs, else_, ifattr)), attr) in
         reapply state, [ decl; if_ ]
      | StmtBind (lhs, rhs, attr) ->
         let acc = newState state [] in
         let acc', rhs' = Mapper.map_exp_to_stmt BindIfExp.mapper acc rhs in
         let state', acc_stmts = restoreState state acc' in
         let stmts' = StmtBind (lhs, rhs', attr) :: acc_stmts in
         let state' = if acc_stmts <> [] then reapply state' else state' in
         state', List.rev stmts'
      | StmtVal (lhs, Some rhs, attr) ->
         let acc = newState state [] in
         let acc', rhs' = Mapper.map_exp_to_stmt BindIfExp.mapper acc rhs in
         let state', acc_stmts = restoreState state acc' in
         let stmts' = StmtBind (lhs, rhs', attr) :: StmtVal (lhs, None, attr) :: acc_stmts in
         let state' = if acc_stmts <> [] then reapply state' else state' in
         state', List.rev stmts'
      | StmtReturn (e, attr) ->
         let acc = newState state [] in
         let acc', e' = Mapper.map_exp_to_stmt BindIfExp.mapper acc e in
         let state', acc_stmts = restoreState state acc' in
         let stmts' = StmtReturn (e', attr) :: acc_stmts in
         let state' = if acc_stmts <> [] then reapply state' else state' in
         state', List.rev stmts'
      | _ -> state, [ stmt ]
   ;;

   let mapper = Mapper.{ default_mapper with stmt_x }
end

module BindComplexExpressions = struct
   let makeTmp tick i = [ "_tmp_" ^ string_of_int tick ^ "_" ^ string_of_int i ]
   let makeValBind lhs rhs = StmtVal (lhs, Some rhs, emptyAttr)
   let makeBind lhs rhs = StmtBind (lhs, rhs, emptyAttr)

   let isUselessBind lhs rhs =
      match lhs, rhs with
      | LId _, PId _ -> true
      | _ -> false
   ;;

   let hasLessThanOneBinding stmts = List.length stmts < 3

   let createAssignments tick kind lhs rhs =
      let lhs_id = GetIdentifiers.fromLhsExpList lhs in
      let rhs_id = GetIdentifiers.fromExpList rhs in
      if IdSet.is_empty (IdSet.inter lhs_id rhs_id)
      then List.map2 (fun a b -> kind a b) lhs rhs
      else (
         let stmts1 =
            List.mapi
               (fun i a ->
                   let attr = GetAttr.fromExp a in
                   makeValBind (LId (makeTmp tick i, attr.typ, attr)) a)
               rhs
         in
         let stmts2 =
            List.mapi
               (fun i a ->
                   let attr = GetAttr.fromLhsExp a in
                   kind a (PId (makeTmp tick i, attr)))
               lhs
         in
         stmts1 @ stmts2)
   ;;

   module BindComplexHelper = struct
      let exp : (stmt list Env.t, exp) Mapper.mapper_func =
         Mapper.make "BindComplexHelper.exp"
         @@ fun state exp ->
         match exp with
         | PTuple (_, attr) ->
            let n, state' = Env.tick state in
            let var_name = "_tuple_" ^ string_of_int n in
            let exp' = PId ([ var_name ], attr) in
            let lhs = LId ([ var_name ], attr.typ, attr) in
            let decl = StmtVal (lhs, None, emptyAttr) in
            let bind = StmtBind (lhs, exp, emptyAttr) in
            let acc = Env.get state' in
            let state' = Env.set state' (bind :: decl :: acc) in
            state', exp'
         | PArray (_, attr) ->
            let n, state' = Env.tick state in
            let var_name = "_array_" ^ string_of_int n in
            let exp' = PId ([ var_name ], attr) in
            let lhs = LId ([ var_name ], attr.typ, attr) in
            let decl = StmtVal (lhs, None, emptyAttr) in
            let bind = StmtBind (lhs, exp, emptyAttr) in
            let acc = Env.get state' in
            let state' = Env.set state' (bind :: decl :: acc) in
            state', exp'
         | _ -> state, exp
      ;;

      let mapper = { Mapper.default_mapper with Mapper.exp }
   end

   let stmt_x : ('a Env.t, stmt) Mapper.expand_func =
      Mapper.makeExpander "BindComplexExpressions.stmt_x"
      @@ fun state stmt ->
      match stmt with
      (* avoids rebinding complex expressions *)
      | StmtReturn (PId (_, _), _) -> state, [ stmt ]
      (* simplify tuple assigns  *)
      | StmtVal (LTuple (lhs, _), None, attr) ->
         let stmts = List.map (fun a -> StmtVal (a, None, attr)) lhs in
         reapply state, stmts
      | StmtVal (LTuple (lhs, _), Some (PTuple (rhs, _)), _) when List.length lhs = List.length rhs ->
         let tick, state' = Env.tick state in
         let stmts = createAssignments tick makeValBind lhs rhs in
         reapply state', stmts
      | StmtBind (LTuple (lhs, _), PTuple (rhs, _), _) when List.length lhs = List.length rhs ->
         let tick, state' = Env.tick state in
         let stmts = createAssignments tick makeBind lhs rhs in
         reapply state', stmts
      | StmtBind (lhs, rhs, attr) ->
         let acc = newState state [] in
         let acc', rhs' = Mapper.map_exp_to_stmt BindComplexHelper.mapper acc rhs in
         let state', acc_stmts = restoreState state acc' in
         if isUselessBind lhs rhs' && hasLessThanOneBinding acc_stmts
         then state, [ stmt ]
         else (
            let stmts' = StmtBind (lhs, rhs', attr) :: acc_stmts in
            let state' = if acc_stmts <> [] then reapply state' else state' in
            state', List.rev stmts')
      | StmtVal (lhs, Some rhs, attr) ->
         let acc = newState state [] in
         let acc', rhs' = Mapper.map_exp_to_stmt BindComplexHelper.mapper acc rhs in
         let state', acc_stmts = restoreState state acc' in
         if isUselessBind lhs rhs' && hasLessThanOneBinding acc_stmts
         then state, [ stmt ]
         else (
            let stmts' = StmtVal (lhs, None, attr) :: StmtBind (lhs, rhs', attr) :: acc_stmts in
            let state' = if acc_stmts <> [] then reapply state' else state' in
            state', List.rev stmts')
      | StmtReturn (e, attr) ->
         let acc = newState state [] in
         let acc', e' = Mapper.map_exp_to_stmt BindComplexHelper.mapper acc e in
         let state', acc_stmts = restoreState state acc' in
         let stmts' = StmtReturn (e', attr) :: acc_stmts in
         let state' = if acc_stmts <> [] then reapply state' else state' in
         state', List.rev stmts'
      | _ -> state, [ stmt ]
   ;;

   let mapper = Mapper.{ default_mapper with stmt_x }
end

module ProcessArrays = struct
   let getArraySize (typ_opt : Typ.t option) : int =
      match typ_opt with
      | Some typ ->
         (match typ with
          | { contents = Typ.TComposed ([ "array" ], [ _; { contents = Typ.TInt (n, _) } ], _) } -> n
          | _ -> failwith "ProcessArrays.getArraySize: the argument is not an array")
      | _ -> failwith "ProcessArrays.getArraySize: type inference should have put a type here"
   ;;

   let exp : (PassData.t Env.t, exp) Mapper.mapper_func =
      Mapper.make "ProcessArrays.exp"
      @@ fun state exp ->
      match exp with
      | PCall (NoInst, [ "size" ], [ arr ], attr) ->
         let arr_attr = GetAttr.fromExp arr in
         let size = getArraySize arr_attr.typ in
         state, PInt (size, attr)
      | _ -> state, exp
   ;;

   let mapper = Mapper.{ default_mapper with exp }
end

module ChangeTupleReturnCalls = struct
   let createReturnTuple inst fname typ =
      match !typ with
      | Typ.TComposed (_, elems, _) ->
         let telems =
            List.mapi
               (fun i typ ->
                   let id = Inference.varReturnName fname i in
                   PCall (inst, id, [], { emptyAttr with typ = Some typ }))
               elems
         in
         PTuple (telems, { emptyAttr with typ = Some typ })
      | _ -> failwith "this should be a tuple"
   ;;

   let stmt_x : ('a Env.t, stmt) Mapper.expand_func =
      Mapper.makeExpander "ChangeTupleReturnCalls.stmt_x"
      @@ fun state stmt ->
      match stmt with
      | StmtBind (LWild _, PCall _, _) -> state, [ stmt ]
      | StmtBind (lhs, (PCall (inst, fname, _, { typ = Some typ }) as rhs), attr) when Typ.isTuple typ ->
         let tuple = createReturnTuple inst fname typ in
         reapply state, [ StmtBind (LWild emptyAttr, rhs, attr); StmtBind (lhs, tuple, attr) ]
      | _ -> state, [ stmt ]
   ;;

   let mapper = Mapper.{ default_mapper with stmt_x }
end

module ApplyFixCast = struct
   let exp : (PassData.t Env.t, exp) Mapper.mapper_func =
      Mapper.make "ApplyFixCast.exp"
      @@ fun state exp ->
      match exp with
      | PCall (_, [ "fix16" ], [ PReal (n, Float, fattr) ], _) -> state, PReal (n, Fix16, fattr)
      | _ -> state, exp
   ;;

   let mapper = Mapper.{ default_mapper with exp }
end

let run =
   UnlinkTypes.mapper
   |> Mapper.seq ReportUnboundTypes.mapper
   |> Mapper.seq SplitMem.mapper
   |> Mapper.seq Simplify.mapper
   |> Mapper.seq SimplifyIfExp.mapper
   |> Mapper.seq ChangeTupleReturnCalls.mapper
   |> Mapper.seq BindComplexExpressions.mapper
   |> Mapper.seq ProcessArrays.mapper
   |> Mapper.seq ConflictingDeclarations.mapper
   |> Mapper.seq ApplyFixCast.mapper
;;
