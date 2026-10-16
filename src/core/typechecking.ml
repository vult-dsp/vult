(*
   The MIT License (MIT)

   Copyright (c) 2014-2024 Leonardo Laguna Ruiz

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
open Util
open Pparser
open Env
open Typed

let context_name = Env.context_name

(* Helper functions to create common contexts *)
let normal_context = {Env.in_constant= false; in_generic_arg= false}

let constant_context = {Env.in_constant= true; in_generic_arg= false}

let generic_arg_context = {Env.in_constant= false; in_generic_arg= true}

(* Check if a syntax statement contains a return statement *)
let rec syntax_has_return (stmt : Syntax.stmt) : bool =
  match stmt.s with
  | SStmtReturn _ ->
      true
  | SStmtBlock stmts ->
      CCList.exists syntax_has_return stmts
  | SStmtIf (_, then_stmt, else_opt) ->
      syntax_has_return then_stmt || Option.fold ~none:false ~some:syntax_has_return else_opt
  | SStmtWhile (_, body) | SStmtIter {body; _} ->
      syntax_has_return body
  | SStmtMatch {first_case; other_cases; _} ->
      CCList.exists (fun (_, case) -> syntax_has_return case) (first_case :: other_cases)
  | SStmtError | SStmtVal _ | SStmtMem _ | SStmtBind _ ->
      false

(* The type with the bindings of its variables followed *)
let resolve (env : env) (t : type_) : type_ = Subst.walk env.subst t

let printType (env : env) (t : type_) : Pla.t = Subst.printType env.subst t

let isConst (env : env) (t : type_) : bool = Subst.isConst env.subst t

let setTypeMut (env : env) (t : type_) : env = {env with subst= Subst.setMut env.subst t}

let unifyConstness (env : env) (t1 : type_) (t2 : type_) : env = {env with subst= Subst.unifyConstness env.subst t1 t2}

(* Unifies the types, returning None when they do not unify *)
let unify ?(bind = false) (env : env) (t1 : type_) (t2 : type_) : env option =
  Option.map (fun subst -> {env with subst}) (Subst.unify ~bind env.subst t1 t2)

let pathString = Typed.pathString

(* Why two types failed to unify. Tuples are diagnosed element by element so that the error can
   name the offending element instead of printing both tuples in full. *)
type unify_failure =
  | TypeMismatch
  | TupleSizeMismatch of {expected: int; got: int}
  | TupleElementMismatch of {index: int; expected: type_; got: type_}

(* Returns the index and types of the first pair of elements that do not unify, unifying the
   elements before it. *)
let findTupleElementMismatch ~bind (env : env) (expected : type_ list) (got : type_ list) : env * unify_failure =
  let rec loop env index expected got =
    match (expected, got) with
    | e :: expected, g :: got -> (
      match unify ~bind env e g with
      | Some env ->
          loop env (index + 1) expected got
      | None ->
          (env, TupleElementMismatch {index; expected= e; got= g}) )
    | [], _ | _, [] ->
        (env, TypeMismatch)
  in
  loop env 0 expected got

(* Called after [unify] failed. Re-unifying the elements only fails at the element that caused the
   original failure. Returns the environment where the elements before it are unified, which is
   used to print the types. *)
let diagnoseUnifyFailure ~bind (env : env) (expected : type_) (got : type_) : env * unify_failure =
  match ((resolve env expected).tx, (resolve env got).tx) with
  | TEComposed ("tuple", e1), TEComposed ("tuple", e2) ->
      let expected_n = CCList.length e1 in
      let got_n = CCList.length e2 in
      if expected_n <> got_n then (env, TupleSizeMismatch {expected= expected_n; got= got_n})
      else findTupleElementMismatch ~bind env e1 e2
  | _ ->
      (env, TypeMismatch)

let typeMismatchMessage (env : env) (expected : type_) (got : type_) : string =
  let expected = printType env expected in
  let got = printType env got in
  Pla.print {%pla|This expression has type '<#got#>' but '<#expected#>' was expected|}

let quotedNames (names : string list) : string = String.concat ", " (CCList.map (fun n -> "'" ^ n ^ "'") names)

let noRecordTypeMessage (members : string list) : string =
  "The type of this value cannot be determined: no record type has the members " ^ quotedNames members
  ^ " with the types they are used with."

(* Uses the member [m] of a value whose type is not known yet. The type of the value is guessed
   among the record types that have the member. Returns the type of the member. *)
let useGuessedMember (env : env) ~(read : bool) (value : type_) (value_loc : Loc.t) (m : string) (loc : Loc.t) :
    env * type_ =
  (* The member of a value that is written shares its constness, like the members of known records *)
  let env, t = Env.fresh env (C.unbound loc) in
  let t = if read then t else {t with const= value.const} in
  let use : Subst.member_use = {name= m; t; owner= value; read} in
  match Env.recordCandidates env (fun members -> Option.is_some (Env.Map.find m members)) with
  | [] ->
      Error.raiseError ("No record type has a member named '" ^ m ^ "'.") loc
  | candidates -> (
    match Subst.guessType env.subst value RecordGuess candidates [use] value_loc with
    | Ok subst ->
        ({env with subst}, t)
    | Error (NoRecordType members) ->
        Error.raiseError (noRecordTypeMessage members) loc
    | Error (Mismatch | NoCommonAlternative _) ->
        Error.raiseError (noRecordTypeMessage [m]) loc )

(* Unifies [t1] (the expected type) with [t2] (the type found at [loc]), raising an error if they
   do not match. When [t2] belongs to a tuple literal, [elem_locs] holds the locations of its
   elements, which are used to point at the offending element. *)
let unifyRaise ?(bind = false) ?(elem_locs = []) (env : env) (loc : Loc.t) (t1 : type_) (t2 : type_) : env =
  match Subst.unifyResult ~bind env.subst t1 t2 with
  | Ok subst ->
      {env with subst}
  | Error (NoCommonAlternative {message; loc}) ->
      Error.raiseError message loc
  | Error (NoRecordType members) ->
      Error.raiseError (noRecordTypeMessage members) loc
  | Error Mismatch -> (
    match diagnoseUnifyFailure ~bind env t1 t2 with
    | env, TypeMismatch ->
        Error.raiseError (typeMismatchMessage env t1 t2) loc
    | _, TupleSizeMismatch {expected; got} ->
        let msg = Pla.print {%pla|This expression has <#got#i> elements but <#expected#i> were expected|} in
        Error.raiseError msg loc
    | env, TupleElementMismatch {index; expected; got} -> (
        let position = index + 1 in
        let element_msg =
          let expected = printType env expected in
          let got = printType env got in
          Pla.print {%pla|Element <#position#i> of this tuple has type '<#got#>' but '<#expected#>' was expected|}
        in
        match CCList.nth_opt elem_locs index with
        | Some elem_loc ->
            Error.raiseError element_msg elem_loc
        | None ->
            let tuple_msg = typeMismatchMessage env t1 t2 in
            Error.raiseError (element_msg ^ ". " ^ tuple_msg) loc ) )

(* The type of a declared variable: an array when its dimension is given *)
let declaredType (loc : Loc.t) (dims : int option) (s : supply) : supply * type_ =
  match dims with
  | Some n ->
      let s, size = C.size ~loc n s in
      let s, elem = C.unbound loc s in
      C.array ~loc ~size elem s
  | None ->
      C.unbound loc s

(* Like [unifyRaise] with an expected type that is created with fresh variables *)
let unifyFreshRaise (env : env) (loc : Loc.t) (make : type_ fresh) (t2 : type_) : env =
  let env, t1 = Env.fresh env make in
  unifyRaise env loc t1 t2

(* Like [unifyRaise] but takes the expression itself, so that tuple literals can report the
   location of the element that does not match. *)
let unifyExpRaise ?(bind = false) (env : env) (expected : type_) (e : exp) : env =
  let elem_locs = match e.e with ETuple elems -> CCList.map (fun (e : exp) -> e.loc) elems | _ -> [] in
  unifyRaise ~bind ~elem_locs env e.loc expected e.t

(* Convert a type with a mapping from generic parameter names to their unbound types *)
let rec type_in_m_with_generic_mapping (env : env) (generic_type_map : (string * type_) list) (t : Syntax.type_) :
    env * type_ =
  match t with
  | {t= STUnbound; loc} ->
      Env.fresh env (C.unbound loc)
  | {t= STGenericType id; loc} -> (
    (* Explicit generic type parameter - look up in the mapping *)
    match CCList.assoc_opt ~eq:String.equal id generic_type_map with
    | Some unbound_type ->
        (env, unbound_type)
    | None ->
        Error.raiseError (Printf.sprintf "Generic type parameter '%s' is not declared in the function" id) loc )
  | {t= STId path; loc} -> (
    match path with
    | {id; n= None; _} -> (
      (* Check if this is a generic type parameter *)
      match CCList.assoc_opt ~eq:String.equal id generic_type_map with
      | Some unbound_type ->
          (env, unbound_type)
      | None ->
          (* Regular type lookup *)
          let found = Env.lookType env path loc in
          Env.fresh env (C.path_t loc found.path) )
    | _ ->
        (* Regular type lookup *)
        let found = Env.lookType env path loc in
        Env.fresh env (C.path_t loc found.path) )
  | {t= STSize n; loc} ->
      let () =
        if n = 0 then
          let msg = "Empty arrays are not supported" in
          Error.raiseError msg loc
      in
      Env.fresh env (C.size ~loc n)
  | {t= STComposed (name, l); loc} ->
      let env, l = CCList.fold_left_map (fun env t -> type_in_m_with_generic_mapping env generic_type_map t) env l in
      Env.fresh env (C.withConst loc (TEComposed (name, l)))

(* Helper to create a mapping from generic parameter names to unbound types *)
let createGenericTypeMapping (env : env) (generic_params : string list) (loc : Loc.t) : env * (string * type_) list =
  let fresh env name =
    let env, t = Env.fresh env (C.unbound loc) in
    (env, (name, t))
  in
  CCList.fold_left_map fresh env generic_params

(* Legacy wrapper for backwards compatibility - creates fresh mapping each time *)
let type_in_m_with_generics (env : env) (generic_params : string list) (t : Syntax.type_) : env * type_ =
  let env, generic_type_map = createGenericTypeMapping env generic_params t.loc in
  type_in_m_with_generic_mapping env generic_type_map t

let type_in_m (env : env) (t : Syntax.type_) : env * type_ = type_in_m_with_generics env [] t

let checkArrayDimensions (env : env) (t : type_) =
  match (resolve env t).tx with
  | TEComposed ("array", [_]) ->
      Error.raiseError "Array type declaration missing size. Use 'array(type, size)' format (e.g., 'array(real, 10)')"
        t.loc
  | _ ->
      ()

(* The type is converted outside the context, and the environment continues with the variables it
   created *)
let type_in_c (env : env) (t : Syntax.type_) : env * type_ =
  let outside, t = type_in_m (Env.exitContext env) t in
  ({env with subst= outside.subst}, t)

let type_in_f (env : env) (t : Syntax.type_) : env * type_ =
  let outside, t = type_in_c (Env.exitFunction env) t in
  ({env with subst= outside.subst}, t)

(* The ordering operators are typed as 'a -> 'a -> bool so that they accept every numeric type,
   which also lets strings through. No backend agrees on what ordering two strings means, so they
   are rejected here. Equality and inequality stay available. *)
let checkOrderedOperands (env : env) (op : string) (e1 : exp) (e2 : exp) (loc : Loc.t) : unit =
  let isString (e : exp) = match (resolve env e.t).tx with TEId {id= "string"; n= None; _} -> true | _ -> false in
  match op with
  | ("<" | ">" | "<=" | ">=") when isString e1 || isString e2 ->
      Error.raiseError
        ("Strings cannot be compared with '" ^ op ^ "'. Only '==' and '<>' are available for strings.")
        loc
  | _ ->
      ()

let applyFunction (env : env) loc (args_t_in : type_ list) (ret : type_) (args_in : exp list) : env * type_ =
  let rec loop env (args_t : type_ list) args =
    match (args_t, args) with
    | [], _ :: _ ->
        let required_n = CCList.length args_t_in in
        let got_n = CCList.length args_in in
        let loc = Loc.mergeList loc (CCList.map (fun (e : exp) -> e.loc) args_in) in
        let msg = Pla.print {%pla|Extra arguments in function call. Expecting <#required_n#i> but got <#got_n#i>.|} in
        Error.raiseError msg loc
    | _ :: _, [] ->
        let required_n = CCList.length args_t_in in
        let got_n = CCList.length args_in in
        let loc = Loc.mergeList loc (CCList.map (fun (e : exp) -> e.loc) args_in) in
        let msg = Pla.print {%pla|Missing arguments in function call. Expecting <#required_n#i> but got <#got_n#i>.|} in
        Error.raiseError msg loc
    | [], [] ->
        (env, ret)
    | h :: args_t, (ht : exp) :: args ->
        let env = unifyRaise env ht.loc h ht.t in
        loop env args_t args
  in
  loop env args_t_in args_in

let rec markExpMutable (env : env) (exp : exp) loc : env =
  match exp.e with
  | EId name when String.equal name context_name -> (
    match Env.contextArgument (Env.getCurrentFunction env) with Some {arg_t; _} -> setTypeMut env arg_t | None -> env )
  | EId name -> (
    match Env.lookVar env name loc with var -> setTypeMut env var.t | exception Error.Errors _ -> env )
  | EMember (e, _) ->
      markExpMutable env e loc
  | EIndex {e; _} ->
      markExpMutable env e loc
  | _ ->
      env

let propagateVariability (env : env) loc (args : Typed.arg list option) (exp_args : exp list) : env =
  match args with
  | None ->
      env
  | Some args ->
      (* Only propagate if the lists have the same length to avoid iter2 failures.
       Length mismatches can happen with external functions or context arguments. *)
      if CCList.length args = CCList.length exp_args then
        CCList.fold_left2
          (fun env (arg : arg) (exp : exp) -> if isConst env arg.t then env else markExpMutable env exp loc)
          env args exp_args
      else env

(* A parameter of a generic call whose argument is typechecked in a second pass *)
type pending_generic_param = PendingArg of arg * Syntax.exp | ReadyParam of Typed.call_param

(* Pairs the parameters of a generic function with the arguments of a call to it *)
let pairGenericParams (generic_func : Typed.generic_function) (args : Syntax.exp list) (loc : Loc.t) :
    (Typed.param_slot * Syntax.exp) list =
  let rec pair slots args acc =
    match (slots, args) with
    | [], [] ->
        Some (CCList.rev acc)
    | slot :: slots, arg :: args ->
        pair slots args ((slot, arg) :: acc)
    | [], _ :: _ | _ :: _, [] ->
        None
  in
  match pair generic_func.params args [] with
  | Some pairs ->
      pairs
  | None ->
      let explicit_count =
        CCList.count (function Typed.SlotGeneric _ -> true | Typed.SlotArg _ -> false) generic_func.params
      in
      let function_count = CCList.length generic_func.args in
      Error.raiseError
        (Printf.sprintf
           "Generic function '%s' expects %d arguments (%d explicit generic parameters + %d function parameters) but \
            got %d"
           generic_func.name (CCList.length generic_func.params) explicit_count function_count (CCList.length args) )
        loc

(* A value of an enumeration. When several enumerations have a value with this name, the
   enumeration is guessed from how the value is used. *)
let enumValue (env : env) (name : string) ((first, others) : Env.enum_values) (loc : Loc.t) : env * exp =
  match others with
  | [] ->
      let env, t = Env.fresh env (C.path_t first.value_loc first.enum_path) in
      (env, {e= EInt first.index; t; loc})
  | _ :: _ -> (
      let values = first :: others in
      let env, t = Env.fresh env (C.unbound loc) in
      let candidates =
        CCList.map (fun (v : Env.enum_value) : Subst.candidate -> {candidate_path= v.enum_path; members= []}) values
      in
      match Subst.guessType env.subst t EnumGuess candidates [] loc with
      | Ok subst ->
          let values = CCList.map (fun (v : Env.enum_value) -> (v.enum_path, v.index)) values in
          ({env with subst}, {e= EEnum {name; values}; t; loc})
      | Error _ ->
          failwith "Internal error in enumValue: a new variable cannot fail to be guessed" )

let rec addContextArg (env : env) instance (f : Env.f) args loc =
  match Env.activeContextArgument env f with
  | None ->
      (env, args)
  | Some callee -> (
      let caller = Env.currentContextArgument env loc in
      let cpath = caller.context_path in
      let fpath = callee.context_path in
      let ctx_t = caller.arg_t in
      let fctx_t = callee.arg_t in
      let is_ctx_mutable = not (isConst env fctx_t) in
      match (Syntax.compare_path cpath fpath, instance) with
      | 0, None ->
          let e = {e= EId context_name; t= fctx_t; loc} in
          let env = if is_ctx_mutable then markExpMutable env e loc else env in
          (env, e :: args)
      | 0, Some _ ->
          let msg =
            Pla.print
              {%pla|This function belongs to the same instance and it must not be called on a different instance.|}
          in
          Error.raiseError msg loc
      (* no instance name provided *)
      | _, None ->
          let number =
            Printf.sprintf "%.2x%.2x"
              (0xFF land Hashtbl.hash (pathString fpath))
              (0xFF land Hashtbl.hash (pathString cpath))
          in
          let rec generateName env =
            let env, n = Env.nextFunctionTick env in
            let name = "inst_" ^ string_of_int n ^ number in
            if checkMemExists env name || Env.checkConstantExists env name then generateName env else (env, name)
          in
          let env, name = generateName env in
          let env = Env.addVar env name fctx_t Env.LocalInst loc in
          let e = {e= EMember ({e= EId context_name; t= ctx_t; loc}, name); loc; t= fctx_t} in
          let env = if is_ctx_mutable then markExpMutable env e loc else env in
          (env, e :: args)
      (* intance without subscripts *)
      | _, Some (name, None) ->
          let env = Env.addVar env name fctx_t Env.LocalInst loc in
          let e = {e= EMember ({e= EId context_name; t= ctx_t; loc}, name); loc; t= fctx_t} in
          let env = if is_ctx_mutable then markExpMutable env e loc else env in
          (env, e :: args)
      (* array of instances *)
      | _, Some (name, Some index) ->
          let env, index = exp ~context:normal_context env index in
          let env = unifyFreshRaise env index.loc (C.int ~loc:Loc.default) index.t in
          let env, t = Env.fresh env (C.array ~loc fctx_t) in
          let env = Env.addVar env name t Env.LocalInst loc in
          let e = {e= EMember ({e= EId context_name; t= ctx_t; loc}, name); loc; t= fctx_t} in
          let e = {e= EIndex {e; index}; loc; t= fctx_t} in
          let env = if is_ctx_mutable then markExpMutable env e loc else env in
          (env, e :: args) )

and call ~(context : Env.expr_context) (env : env) instance path args loc eloc =
  (* First check if this is a generic function *)
  match Env.lookupGeneric env path with
  | Some _ when context.in_constant ->
      Error.raiseError "Generic functions cannot be called in constant expressions" loc
  | Some generic_func ->
      (* This is a generic call - handle instantiation *)
      (* NOTE: Don't process args with exp_list yet - generic_call will handle them *)
      generic_call env instance path generic_func args loc eloc
  | None -> (
    (* Try regular function lookup first *)
    match Env.tryLookFunctionCall env path with
    | _, Some f when context.in_constant && Env.isFunctionActive env f ->
        Error.raiseError "Functions with memory variables cannot be called in constant expressions" loc
    | env, Some f ->
        (* Regular function call *)
        let env, args = exp_list ~context env args in
        let args_t, ret = f.t in
        let env, t = applyFunction env eloc args_t ret args in
        let env = propagateVariability env loc f.args args in
        let env, args = addContextArg env instance f args loc in
        (env, {e= ECall {instance= None; path= f.path; args}; t; loc})
    | env, None -> (
      (* Check if this might be a companion function of a generic *)
      match Env.lookupGenericByCompanion env path with
      | Some _ when context.in_constant ->
          Error.raiseError "Companion functions of generic functions cannot be called in constant expressions" loc
      | Some parent_generic ->
          (* This is a companion call - create EGenCompanionCall for later processing *)
          let env, processed_args = exp_list env args in
          (* For now, we use noreturn as the return type - it will be resolved during instantiation *)
          let env, t = Env.fresh env (C.noreturn loc) in
          let parent_path : path = {id= parent_generic.name; n= path.Syntax.n; loc= path.Syntax.loc} in
          (* Extract just the instance name from the Syntax instance type *)
          let instance_name = Option.map fst instance in
          ( env
          , { e=
                EGenCompanionCall
                  { instance= instance_name
                  ; companion_name= path.Syntax.id
                  ; parent_generic_path= parent_path
                  ; args= processed_args }
            ; t
            ; loc } )
      | None ->
          Error.raiseError ("A function with the name '" ^ Env.pathString path ^ "' could not be found") loc ) )

and generic_call (env : env) (instance : (string * Syntax.exp option) option) (generic_path : Syntax.path)
    (generic_func : Typed.generic_function) (args : Syntax.exp list) (_ : Loc.t) (eloc : Loc.t) : env * exp =
  let pairs = pairGenericParams generic_func args eloc in
  (* The explicit generic arguments are processed first, in the context of generic arguments
     (which allows function references) *)
  let env, pending =
    CCList.fold_left_map
      (fun env (slot, arg) ->
        match slot with
        | Typed.SlotGeneric param ->
            let env, value = exp ~context:generic_arg_context env arg in
            (env, ReadyParam (Typed.CallGeneric {param; value}))
        | Typed.SlotArg decl ->
            (env, PendingArg (decl, arg)) )
      env pairs
  in
  (* Check each constant generic argument against its declared parameter type. This applies to
     literal (specialized) and variable (non-specialized) arguments alike. A fresh copy of the
     declared type is unified so the generic definition itself is never mutated. *)
  let env =
    CCList.fold_left
      (fun env pending_param ->
        match pending_param with
        | ReadyParam (Typed.CallGeneric {param= Typed.GParamConstant (_, param_type, _); value}) ->
            let (env, _), fresh_param_type = Env.copyType (env, Subst.emptyCopyMapping) param_type in
            unifyRaise env value.loc fresh_param_type value.t
        | ReadyParam (Typed.CallGeneric {param= Typed.GParamType _ | Typed.GParamFunction _; _})
        | ReadyParam (Typed.CallArg _)
        | PendingArg _ ->
            env )
      env pending
  in
  (* Process regular function arguments with normal context *)
  let env, params =
    CCList.fold_left_map
      (fun env pending_param ->
        match pending_param with
        | ReadyParam param ->
            (env, param)
        | PendingArg (arg, e) ->
            let env, value = exp ~context:normal_context env e in
            (env, Typed.CallArg {arg; value}) )
      env pending
  in
  let processed_function_args = Typed.callArgs params in
  (* Create fresh copies of the generic function's argument types for unification. This allows
     the types to be constrained by unification without polluting the original. *)
  let generic_args =
    CCList.filter_map (function Typed.CallArg {arg; _} -> Some arg.t | Typed.CallGeneric _ -> None) params
  in
  let (env, mapping), fresh_arg_types = Env.copyTypes (env, Subst.emptyCopyMapping) generic_args in
  let (env, _), fresh_ret_type = Env.copyType (env, mapping) (snd generic_func.t) in
  (* Unify argument types to constrain the fresh types *)
  let env =
    CCList.fold_left2
      (fun env fresh_t (value : exp) -> match unify env fresh_t value.t with Some env -> env | None -> env)
      env fresh_arg_types processed_function_args
  in
  (* Return type is now constrained through unification (may still be unbound if type depends on *)
  (* context not yet available - that's fine, it will be resolved by further unification) *)
  let env, t = applyFunction env eloc fresh_arg_types fresh_ret_type processed_function_args in
  (* Note: We don't propagate variability here because generic_func.args types haven't been
     marked mutable yet at this point. The mutability is determined LATER during instantiation
     when the body is processed (e.g., m[...] = value marks m mutable). Variability propagation
     happens in process_exp_instantiation after instantiation, using the specialized function's
     args which ARE properly marked as mutable. *)
  (* Return EGenCall - instantiation will happen during post-processing when types are fully resolved *)
  ( env
  , { e=
        EGenCall
          { instance= Option.map fst instance (* Extract just the name *)
          ; generic_path (* Use full path for correct module-qualified lookup *)
          ; params }
    ; t
    ; loc= eloc } )

and exp ?(context = normal_context) (env : env) (e : Syntax.exp) : env * exp =
  match e with
  | {e= SEBool value; loc} ->
      let env, t = Env.fresh env (C.bool ~loc) in
      (env, {e= EBool value; t; loc})
  | {e= SEInt value; loc} ->
      let env, t = Env.fresh env (C.int ~loc) in
      let value =
        match int_of_string_opt value with
        | Some n ->
            n
        | None ->
            Error.raiseError ("The integer '" ^ value ^ "' is out of range") loc
      in
      (env, {e= EInt value; t; loc})
  | {e= SEReal value; loc} ->
      let env, t = Env.fresh env (C.real ~loc) in
      (env, {e= EReal (float_of_string value); t; loc})
  | {e= SEFixed value; loc} ->
      let env, t = Env.fresh env (C.fix16 ~loc) in
      let value = String.sub value 0 (String.length value - 1) in
      (env, {e= EFixed (float_of_string value); t; loc})
  | {e= SEString value; loc} ->
      let env, t = Env.fresh env (C.string ~loc) in
      (env, {e= EString value; t; loc})
  | {e= SEGroup e; _} ->
      exp ~context env e
  | {e= SEId name; loc} when not (Syntax.startsUppercase name) -> (
      let name_path : path = {id= name; n= None; loc} in
      match Env.lookupExpressionSymbol env name_path context with
      | env, ExprVariable var ->
          let t = var.t in
          let env, e =
            match var.kind with
            | Val ->
                (Env.countRead env name, {e= EId name; t; loc})
            | Const ->
                let m = Env.getCurrentModule env in
                (env, {e= EConst {id= name; n= Some m.name; loc}; t; loc})
            | Mem _ | Inst ->
                let ctx = Env.getContext env in
                let env, ctx_t = Env.fresh env (C.path_t loc ctx) in
                (env, {e= EMember ({e= EId context_name; t= ctx_t; loc}, name); t; loc})
          in
          (env, e)
      | env, ExprEnum values ->
          enumValue env name values loc
      | env, ExprFunction f ->
          if context.in_generic_arg then
            (* In template argument context, allow function references *)
            let args_t, ret = f.t in
            let env, t = Env.fresh env (C.withConst loc (TEFunction (args_t, ret))) in
            (env, {e= EId name; t; loc}) (* Return function reference *)
          else
            (* In regular context, functions must be called with parentheses *)
            Error.raiseError ("Function '" ^ name ^ "' must be called with parentheses (e.g., '" ^ name ^ "()')") loc
      | _, ExprType _ ->
          (* Types in expression context are not directly supported - treat as error *)
          Error.raiseError
            ("Type '" ^ name ^ "' cannot be used as a value. Use it in variable declarations or type annotations")
            loc
      | _, ExprNotFound ->
          Error.raiseError ("Undefined symbol '" ^ name ^ "'. Check spelling or ensure it's declared before use") loc )
  | {e= SEIndex {e; index}; loc} ->
      let env, e = exp ~context env e in
      let env, index = exp ~context env index in
      let env, t = Env.fresh env (C.unbound Loc.default) in
      (* Allow indexing on arrays and lists *)
      let env = unifyFreshRaise env e.loc (C.indexable t) e.t in
      let env = unifyFreshRaise env index.loc (C.int ~loc:Loc.default) index.t in
      (* if the type is a builtin (a value) do not unify the constness *)
      let env = if (not context.in_constant) && not (Env.isBuiltinType env t) then unifyConstness env t e.t else env in
      (env, {e= EIndex {e; index}; t; loc})
  | {e= SEArray []; loc} ->
      Error.raiseError "Empty array literal '[]' is not supported. Specify array elements or use array type declaration"
        loc
  | {e= SEArray (h :: t); loc} ->
      let env, h = exp ~context env h in
      let env, t_rev, size =
        CCList.fold_left
          (fun (env, acc, size) e ->
            let env, e = exp ~context env e in
            let env = unifyRaise env e.loc h.t e.t in
            (env, e :: acc, size + 1) )
          (env, [], 1) t
      in
      let env, size = Env.fresh env (C.size ~loc size) in
      let env, t = Env.fresh env (C.array ~size h.t) in
      (env, {e= EArray (h :: CCList.rev t_rev); t; loc})
  | {e= SETuple l; loc} ->
      let env, l = exp_list ~context env l in
      let env, t = Env.fresh env (C.tuple ~loc (CCList.map (fun (e : exp) -> e.t) l)) in
      (env, {e= ETuple l; t; loc})
  | {e= SEIf {cond; then_; else_}; loc} ->
      let env, cond = exp ~context env cond in
      let env, then_ = exp ~context env then_ in
      let env, else_ = exp ~context env else_ in
      let t = then_.t in
      let env = unifyFreshRaise env cond.loc (C.bool ~loc) cond.t in
      let env = unifyExpRaise env then_.t else_ in
      (env, {e= EIf {cond; then_; else_}; t; loc})
  (* we need to add a special case for int() in order to support conversion of enumerations *)
  | {e= SECall {instance= None; path= {id= "int"; n= None; _} as path; args= [arg] as args}; loc}
    when not context.in_constant -> (
      let env, arg = exp ~context env arg in
      match arg with
      | {e= EInt n; loc; _} ->
          let env, t = Env.fresh env (C.int ~loc) in
          (env, {e= EInt n; t; loc})
      | {e= EEnum _; loc; _} ->
          (* The enumeration is decided by the other uses of the value *)
          let env, t = Env.fresh env (C.int ~loc) in
          (env, {e= ECall {instance= None; path; args= [arg]}; t; loc})
      | {loc= arg_loc; t; _} -> (
        (* A value whose type is an enumeration *)
        match (resolve env t).tx with
        | TEId tpath when Env.isEnum env tpath ->
            let env, t = Env.fresh env (C.int ~loc:arg_loc) in
            (env, {e= ECall {instance= None; path; args= [arg]}; t; loc= arg_loc})
        | _ ->
            call ~context env None path args loc e.loc ) )
  | {e= SECall {instance; path; args}; loc} ->
      call ~context env instance path args loc e.loc
  | {e= SEOp (op, e1, e2); loc} ->
      let env, e1 = exp ~context env e1 in
      let env, e2 = exp ~context env e2 in
      let () = checkOrderedOperands env op e1 e2 loc in
      let env, f = if context.in_constant then Env.lookOperatorInModule env op else Env.lookOperator env op in
      let args_t, ret = f.t in
      let env, t = applyFunction env e.loc args_t ret [e1; e2] in
      (env, {e= EOp (op, e1, e2); t; loc})
  | {e= SEUnOp (op, e); loc} ->
      let env, e = exp ~context env e in
      let env, f =
        if context.in_constant then Env.lookOperatorInModule env ("u" ^ op) else Env.lookOperator env ("u" ^ op)
      in
      let args_t, ret = f.t in
      let env, t = applyFunction env e.loc args_t ret [e] in
      (env, {e= EUnOp (op, e); t; loc})
  | {e= SEMember (e1, m); loc} -> (
    (* First, try to interpret this as an enum reference if e1 is an SEId or SEEnum *)
    match e1 with
    | {e= SEId module_name; _} when Syntax.startsUppercase module_name -> (
        (* First check if this is a module name - try module-qualified access *)
        let const_path = Syntax.{id= m; n= Some module_name; loc} in
        let env, results = Env.lookupPath env const_path in
        match results with
        | _ :: _ -> (
          (* Found something in module - check what it is *)
          match Env.findVar results with
          | Some var when var.kind = Const ->
              let t = var.t in
              (env, {e= EConst const_path; t; loc})
          | Some var ->
              Error.raiseError
                ( "Found '" ^ module_name ^ "." ^ m ^ "' but it's not a constant (it's a "
                ^ ( match var.kind with
                  | Val ->
                      "variable"
                  | Mem _ ->
                      "memory"
                  | Inst ->
                      "instance"
                  | Const ->
                      "constant" )
                ^ ")" )
                loc
          | None -> (
            (* Check for function or enum *)
            match Env.findFunction results with
            | Some _ ->
                Error.raiseError
                  ( "'" ^ module_name ^ "." ^ m ^ "' is a function, not a constant. Use function call syntax: "
                  ^ module_name ^ "." ^ m ^ "(args)" )
                  loc
            | None -> (
              match Env.findEnum results with
              | Some values ->
                  enumValue env m values loc
              | None ->
                  Error.raiseError
                    ("Found '" ^ module_name ^ "." ^ m ^ "' but it's not a constant, function, or enum")
                    loc ) ) )
        | [] ->
            (* Module not found - check if it's an actual module name or just not found *)
            Error.raiseError
              ("Module '" ^ module_name ^ "' not found. Check that the module is included or spelled correctly")
              loc )
    | _ -> (
        (* For non-SEId expressions, use normal member access *)
        let env, e1 = exp ~context env e1 in
        match (resolve env e1.t).tx with
        | TEId path -> (
          match Env.lookType env path loc with
          | {path; descr= Record members; _} -> (
            match Map.find m members with
            | None ->
                Error.raiseError ("The field '" ^ m ^ "' is not part of the type '" ^ pathString path ^ "'") loc
            | Some {t; _} ->
                let env, t = Env.refreshConstness env t in
                (* if the type is a builtin (a value) do not unify the constness *)
                let env =
                  if (not context.in_constant) && not (Env.isBuiltinType env t) then unifyConstness env t e1.t else env
                in
                (env, {e= EMember (e1, m); t; loc}) )
          | _ ->
              let t = Pla.print (printType env e1.t) in
              let e = Pla.print (Typed.print_exp e1) in
              Error.raiseError
                ("The expression '" ^ e ^ "' of type '" ^ t ^ "' does not have a member '" ^ m ^ "'.")
                loc )
        | TEUnbound _ when not context.in_constant ->
            let env, t = useGuessedMember env ~read:true e1.t e1.loc m loc in
            (env, {e= EMember (e1, m); t; loc})
        | _ ->
            let t = Pla.print (printType env e1.t) in
            let e = Pla.print (Typed.print_exp e1) in
            Error.raiseError ("The expression '" ^ e ^ "' of type '" ^ t ^ "' does not have a member '" ^ m ^ "'.") loc
        ) )
  | {e= SEId id; loc} -> (
      (* This case handles uppercase identifiers (enum constructors) *)
      let id_path : path = {id; n= None; loc} in
      match Env.lookupExpressionSymbol env id_path context with
      | env, ExprEnum values ->
          enumValue env id values loc
      | _, ExprNotFound ->
          Error.raiseError ("Undefined symbol '" ^ id ^ "'. Check spelling or ensure it's declared before use") loc
      (* Only names starting with an uppercase letter reach this point: they are read as
         enumeration values, so whatever this one does resolve to cannot be used here. *)
      | _, ((ExprVariable _ | ExprFunction _ | ExprType _) as symbol) ->
          let kind = match symbol with ExprFunction _ -> "a function" | ExprType _ -> "a type" | _ -> "a variable" in
          Error.raiseError
            ( "'" ^ id ^ "' is " ^ kind
            ^ ", but a name that starts with an uppercase letter is read as an enumeration value. Rename it to '"
            ^ String.uncapitalize_ascii id ^ "' or use a module-qualified enumeration value like 'MyModule.Value'." )
            loc )
  | {e= SERecord {path= None; elems}; loc} ->
      anonymousRecord ~context env elems loc
  | {e= SERecord {path= Some path; elems}; loc} -> (
      let t = Env.lookType env path loc in
      match t with
      | {descr= Record members; _} ->
          let env, elems_rev =
            CCList.fold_left
              (fun (env, acc) (id, v) ->
                let env, v = exp ~context env v in
                let id, id_loc =
                  match id with
                  | Syntax.{id; n= None; loc} ->
                      (id, loc)
                  | {loc; _} ->
                      Error.raiseError ("The name '" ^ pathString id ^ "' is not a valid member of a data type.") loc
                in
                match Env.Map.find id members with
                | None ->
                    Error.raiseError ("The name '" ^ id ^ "' does not belong to type '" ^ pathString path ^ "'.") id_loc
                | Some var ->
                    let env = unifyRaise env v.loc var.t v.t in
                    (env, (id, v) :: acc) )
              (env, []) elems
          in
          let elems = CCList.sort (fun (id1, _) (id2, _) -> String.compare id1 id2) elems_rev in
          let () = checkRecordMembers (Env.Map.fold (fun name _ acc -> name :: acc) [] members) elems loc in
          let env, record_t = Env.fresh env (C.path_t loc t.path) in
          (env, {e= ERecord {elems}; t= record_t; loc})
      | _ ->
          Error.raiseError ("The path '" ^ pathString path ^ "' is not a type.") loc )
  | {e= SETypeIntrinsic (intrinsic_name, _); loc} when context.in_constant ->
      Error.raiseError ("The type intrinsic '" ^ intrinsic_name ^ "' cannot be used in constant expressions") loc
  | {e= SETypeIntrinsic (intrinsic_name, type_param); loc} ->
      (* Type intrinsics - convert to typed representation *)
      (* The type is unbound here and will be resolved during generic instantiation *)
      let intrinsic =
        match intrinsic_name with
        | "typedefault" ->
            Typed.TypeDefault
        | "typemax" ->
            Typed.TypeMax
        | "typemin" ->
            Typed.TypeMin
        | _ ->
            Error.raiseError ("Unknown type intrinsic: " ^ intrinsic_name) loc
      in
      (* Type is unbound - will be resolved during generic specialization *)
      let env, t = Env.fresh env (C.unbound loc) in
      (env, {e= ETypeIntrinsic {intrinsic; type_param}; t; loc})

(* Every member of a record value has to be given once. [elems] are sorted by name. *)
and checkRecordMembers (members : string list) (elems : (string * exp) list) (loc : Loc.t) : unit =
  let names = CCList.map fst elems in
  let () =
    match CCList.find_opt (fun (id, _) -> CCList.count (String.equal id) names > 1) elems with
    | Some (id, (v : exp)) ->
        Error.raiseError ("The member '" ^ id ^ "' is given more than once.") v.loc
    | None ->
        ()
  in
  match CCList.filter (fun member -> not (CCList.mem ~eq:String.equal member names)) members with
  | [] ->
      ()
  | missing ->
      let missing = CCList.sort String.compare missing in
      let plural = match missing with [_] -> "member " | _ -> "members " in
      Error.raiseError ("No value was given for the " ^ plural ^ quotedNames missing ^ ".") loc

(* A record without the name of its type. The type is guessed among the records that have exactly
   the given members. *)
and anonymousRecord ~(context : Env.expr_context) (env : env) (elems : (Syntax.path * Syntax.exp) list) (loc : Loc.t) :
    env * exp =
  let env, elems =
    CCList.fold_left_map
      (fun env ((id : Syntax.path), v) ->
        match id with
        | {id; n= None; _} ->
            let env, v = exp ~context env v in
            (env, (id, v))
        | {loc; _} ->
            Error.raiseError ("The name '" ^ pathString id ^ "' is not a valid member of a data type.") loc )
      env elems
  in
  let elems = CCList.sort (fun (id1, _) (id2, _) -> String.compare id1 id2) elems in
  let names = CCList.map fst elems in
  let () = checkRecordMembers names elems loc in
  let hasExactly members =
    Env.Map.fold (fun name _ acc -> name :: acc) [] members
    |> CCList.sort String.compare |> CCList.equal String.equal names
  in
  let env, t = Env.fresh env (C.unbound loc) in
  let uses = CCList.map (fun (name, (v : exp)) : Subst.member_use -> {name; t= v.t; owner= t; read= false}) elems in
  match Env.recordCandidates env hasExactly with
  | [] ->
      Error.raiseError ("No record type has exactly the members " ^ quotedNames names ^ ".") loc
  | candidates -> (
    match Subst.guessType env.subst t RecordGuess candidates uses loc with
    | Ok subst ->
        ({env with subst}, {e= ERecord {elems}; t; loc})
    | Error (NoRecordType members) ->
        Error.raiseError (noRecordTypeMessage members) loc
    | Error (Mismatch | NoCommonAlternative _) ->
        Error.raiseError (noRecordTypeMessage names) loc )

and exp_list ?(context = normal_context) (env : env) (l : Syntax.exp list) : env * exp list =
  let env, rev_l =
    CCList.fold_left
      (fun (env, acc) e ->
        let env, e = exp ~context env e in
        (env, e :: acc) )
      (env, []) l
  in
  (env, CCList.rev rev_l)

and lexp ?(const = false) (env : env) (e : Syntax.lexp) : env * lexp =
  match e with
  | {l= SLWild; loc} ->
      let env, t = Env.fresh env (C.noreturn loc) in
      (env, {l= LWild; t; loc})
  | {l= SLId name; loc} ->
      let var = Env.lookVar env name loc in
      let t = var.t in
      let env = if const then env else setTypeMut env t in
      let env, e =
        match var.kind with
        | Val ->
            (env, {l= LId name; t; loc})
        | Mem _ | Inst ->
            let ctx = Env.getContext env in
            let env, ctx_t = Env.fresh env (C.path_t loc ctx) in
            (env, {l= LMember ({l= LId context_name; t= ctx_t; loc}, name); t; loc})
        | Const ->
            Error.raiseError ("Cannot assign to constant '" ^ name ^ "'. Constants are read-only after declaration") loc
      in
      (env, e)
  | {l= SLGroup e; _} ->
      lexp ~const env e
  | {l= SLTuple elems; loc} ->
      let env, elems =
        CCList.fold_left
          (fun (env, acc) e ->
            let env, e = lexp ~const env e in
            (env, e :: acc) )
          (env, []) (CCList.rev elems)
      in
      let t_elems = CCList.map (fun (e : lexp) -> e.t) elems in
      let env, t = Env.fresh env (C.tuple ~loc t_elems) in
      (env, {l= LTuple elems; t; loc})
  | {l= SLIndex {e; index}; loc} ->
      let env = countPartialWrite env e in
      let env, e = lexp ~const env e in
      let env, index = exp env index in
      let env, t = Env.fresh env (C.unbound loc) in
      let env = unifyFreshRaise env index.loc (C.int ~loc) index.t in
      let env = unifyFreshRaise env e.loc (C.array ~fixed:false ~loc t) e.t in
      (env, {l= LIndex {e; index}; t; loc})
  | {l= SLMember (e, m); loc} -> (
      let env = countPartialWrite env e in
      let env, e = lexp ~const env e in
      match (resolve env e.t).tx with
      | TEId path -> (
        match Env.lookType env path loc with
        | {path; descr= Record members; _} -> (
          match Map.find m members with
          | None ->
              Error.raiseError ("The field '" ^ m ^ "' is not part of the type '" ^ pathString path ^ "'") loc
          | Some {t; _} ->
              let env, t = Env.refreshConstness env t in
              (* if the type is a builtin (a value) do not unify the constness *)
              let t = if not (Env.isBuiltinType env t) then {t with const= e.t.const} else t in
              (env, {l= LMember (e, m); t; loc}) )
        | _ ->
            let t = Pla.print (printType env e.t) in
            let e = Pla.print (Typed.print_lexp e) in
            Error.raiseError ("The expression '" ^ e ^ "' of type '" ^ t ^ "' does not have a member '" ^ m ^ "'.") loc
        )
      | TEUnbound _ ->
          let env, t = useGuessedMember env ~read:false e.t e.loc m loc in
          (env, {l= LMember (e, m); t; loc})
      | _ ->
          let t = Pla.print (printType env e.t) in
          let e = Pla.print (Typed.print_lexp e) in
          Error.raiseError ("The expression '" ^ e ^ "' of type '" ^ t ^ "' does not have a member '" ^ m ^ "'.") loc )

and markInitialized (env : env) (e : dexp) : env =
  match e.d with
  | DId (name, _) ->
      Env.markInitialized env name
  | DTuple elems ->
      CCList.fold_left markInitialized env elems
  | DWild ->
      env

(* Counts the write of an element or member of a variable, which only matters for the ones that
   refer to another value *)
and countPartialWrite (env : env) (e : Syntax.lexp) : env =
  match e.l with
  | SLId name ->
      Env.countPartialWrite env name
  | SLGroup e ->
      countPartialWrite env e
  | SLWild | SLMember _ | SLIndex _ | SLTuple _ ->
      env

and dexp (env : env) (e : Syntax.dexp) (kind : Env.local_kind) : env * dexp =
  match e with
  | {d= SDWild; loc} ->
      let env, t = Env.fresh env (C.noreturn loc) in
      (env, {d= DWild; t; loc})
  | {d= SDTuple l; loc} ->
      let env, l =
        CCList.fold_left
          (fun (env, acc) e ->
            let env, e = dexp env e kind in
            (env, e :: acc) )
          (env, []) (CCList.rev l)
      in
      let env, t = Env.fresh env (C.tuple ~loc (CCList.map (fun (e : dexp) -> e.t) l)) in
      (env, {d= DTuple l; t; loc})
  | {d= SDGroup e; _} ->
      dexp env e kind
  | {d= SDTyped (e, t); _} ->
      let env, e = dexp env e kind in
      let env, t = type_in_f env t in
      checkArrayDimensions env t ;
      let env = unifyRaise ~bind:true env e.loc t e.t in
      (env, e)
  | {d= SDId (name, dims); loc} ->
      let env, t = Env.fresh env (declaredType loc dims) in
      let env = Env.addVar env name t kind loc in
      (env, {d= DId (name, dims); t; loc})

let rec dexp_to_lexp (d : Syntax.dexp) : Syntax.lexp =
  let loc = d.loc in
  match d.d with
  | SDTuple l ->
      let l = CCList.map dexp_to_lexp l in
      {l= SLTuple l; loc}
  | SDWild ->
      {l= SLWild; loc}
  | SDId (name, _) ->
      {l= SLId name; loc}
  | SDGroup e ->
      dexp_to_lexp e
  | SDTyped (e, _) ->
      dexp_to_lexp e

let stmt_block (stmts : stmt list) = match stmts with [s] -> s | _ -> {s= StmtBlock stmts; loc= Loc.default}

let makeIterWhile (env : env) name id_loc value body loc : env * Syntax.stmt =
  let env, tick = Env.nextFunctionTick env in
  let itname = name ^ "__" ^ string_of_int tick in
  let env = Env.addLoopCounter env ~counter:itname ~written:name in
  let open Syntax in
  let int_type = {t= STId {id= "int"; n= None; loc= id_loc}; loc} in
  let dlhs = {d= SDTyped ({d= SDId (itname, None); loc= id_loc}, int_type); loc= id_loc} in
  let lhs = {l= SLId itname; loc= id_loc} in
  let rhs = {e= SEId itname; loc= id_loc} in
  let decl = {s= SStmtVal (dlhs, Some {e= SEInt "0"; loc}); loc} in
  let incr = {s= SStmtBind (lhs, {e= SEOp ("+", rhs, {e= SEInt "1"; loc}); loc}); loc} in
  let new_body = Syntax.ReaplaceId.inStmt [(name, itname)] {s= SStmtBlock [body; incr]; loc} in
  let cond = {e= SEOp ("<", rhs, value); loc} in
  let while_s = {s= SStmtWhile (cond, new_body); loc} in
  (env, {s= SStmtBlock [decl; while_s]; loc})

let makeIfOfMatch env e first_case other_cases =
  let rec makeComparison (e : Syntax.exp) (p : Syntax.pattern) =
    let makeEq e1 e2 = Syntax.{e= SEOp ("==", e1, e2); loc= e1.loc} in
    let makeAnd e1 e2 = Syntax.{e= SEOp ("&&", e1, e2); loc= e1.loc} in
    match (e, p) with
    | _, {p= SPWild; loc} ->
        Syntax.{e= SEBool true; loc}
    | {e= SEGroup e; _}, _ ->
        makeComparison e p
    | e, {p= SPGroup p; _} ->
        makeComparison e p
    | {e= SETuple elems; _}, {p= SPTuple patterns; loc} ->
        if CCList.length elems = CCList.length patterns then
          let conds = CCList.map2 (fun e p -> makeComparison e p) elems patterns in
          CCList.fold_right makeAnd conds Syntax.{e= SEBool true; loc}
        else
          let msg =
            "The pattern cannot be matched with the input expression because it has different number of elements."
          in
          let loc = Loc.mergeList Loc.default @@ CCList.map (fun (p : Syntax.pattern) -> p.loc) patterns in
          Error.raiseError msg loc
    | {e= SETuple _; _}, {loc; _} ->
        let msg =
          "The pattern cannot be matched with the input expression because it has different number of elements."
        in
        Error.raiseError msg loc
    | _, {p= SPTuple patterns; _} ->
        let loc = Loc.mergeList Loc.default @@ CCList.map (fun (p : Syntax.pattern) -> p.loc) patterns in
        let msg =
          "The pattern cannot be matched with the input expression because it has different number of elements."
        in
        Error.raiseError msg loc
    | _, {p= SPBool b; loc} ->
        makeEq e Syntax.{e= SEBool b; loc}
    | _, {p= SPInt i; loc} ->
        makeEq e Syntax.{e= SEInt i; loc}
    | _, {p= SPReal f; loc} ->
        makeEq e Syntax.{e= SEReal f; loc}
    | _, {p= SPFixed f; loc} ->
        makeEq e Syntax.{e= SEFixed f; loc}
    | _, {p= SPString s; loc} ->
        makeEq e Syntax.{e= SEString s; loc}
    | _, {p= SPId id; loc} -> (
        (* Handle enum constructor and constant patterns *)
        let id_path : path = {id; n= None; loc} in
        (* Only a function creates fresh variables when it is found, and functions are rejected *)
        match snd (Env.lookupExpressionSymbol env id_path normal_context) with
        | ExprEnum _ ->
            (* Enum constructor: compare with the enum value itself *)
            makeEq e Syntax.{e= SEId id; loc}
        | ExprVariable var when var.kind = Const ->
            (* Constant: create a constant reference for comparison *)
            makeEq e Syntax.{e= SEId id; loc}
        | _ ->
            Error.raiseError ("Pattern '" ^ id ^ "' is not a valid enum constructor or constant") loc )
    | _, {p= SPMember ({p= SPId module_name; _}, variant_name); loc} -> (
        (* Handle qualified enum constructor patterns like Button.Push *)
        let id_path : path = {id= variant_name; n= Some module_name; loc} in
        match snd (Env.lookupExpressionSymbol env id_path normal_context) with
        | ExprEnum _ ->
            makeEq e Syntax.{e= SEMember (Syntax.{e= SEId module_name; loc}, variant_name); loc}
        | _ ->
            Error.raiseError ("Pattern '" ^ module_name ^ "." ^ variant_name ^ "' is not a valid enum constructor") loc
        )
    | _, {p= SPMember _; loc} ->
        Error.raiseError "Invalid qualified pattern. Only Module.Variant patterns are supported" loc
  in
  let makeCase (p, case) else_ =
    let cond = makeComparison e p in
    Syntax.{s= SStmtIf (cond, case, else_); loc= cond.loc}
  in
  let rec chain case rest =
    match rest with [] -> makeCase case None | next :: rest -> makeCase case (Some (chain next rest))
  in
  chain first_case other_cases

let resolveTypeIntrinsicInline = Typed.resolveTypeIntrinsicInline

let resolveTypeIntrinsicsInExp = Typed.resolveTypeIntrinsicsInExp

let resolveTypeIntrinsicsInStmt = Typed.resolveTypeIntrinsicsInStmt

let substituteConstantsInStmt = Typed.substituteConstantsInStmt

(* Type substitution version of stmt for processing specialized function bodies *)
let rec stmt_with_type_substitution (env : env) (type_substitution_map : (string * type_) list) (return : type_)
    (s : Syntax.stmt) : env * stmt list =
  (* Use a modified dexp function that substitutes concrete types for generic parameters *)
  let rec dexp_with_substitution env dexp kind =
    match dexp with
    | {Syntax.d= Syntax.SDTuple l; loc} ->
        (* Processed from the last element, like [dexp], but kept in source order *)
        let env, l_rev =
          CCList.fold_left_map
            (fun env e ->
              let env, e = dexp_with_substitution env e kind in
              (env, e) )
            env (CCList.rev l)
        in
        let l = CCList.rev l_rev in
        let env, t = Env.fresh env (C.tuple ~loc (CCList.map (fun (e : dexp) -> e.t) l)) in
        (env, {d= DTuple l; t; loc})
    | {Syntax.d= Syntax.SDGroup e; _} ->
        dexp_with_substitution env e kind
    | {Syntax.d= Syntax.SDTyped (e, t); _} ->
        let env, e = dexp_with_substitution env e kind in
        let env, t = type_in_m_with_substitution env type_substitution_map t in
        checkArrayDimensions env t ;
        let env = unifyRaise ~bind:true env e.loc t e.t in
        (env, e)
    | {Syntax.d= Syntax.SDId (name, dims); loc} ->
        let env, t = Env.fresh env (declaredType loc dims) in
        let env = Env.addVar env name t kind loc in
        (env, {d= DId (name, dims); t; loc})
    | {Syntax.d= Syntax.SDWild; loc} ->
        let env, t = Env.fresh env (C.noreturn loc) in
        (env, {d= DWild; t; loc})
  in
  (* Use the same stmt function but with our type-substituting dexp *)
  let env, stmts = stmt_generic env dexp_with_substitution return s in
  (* Resolve any type intrinsics in the resulting statements *)
  let resolved_map = CCList.map (fun (name, t) -> (name, Subst.zonk env.subst t)) type_substitution_map in
  let stmts = CCList.map (resolveTypeIntrinsicsInStmt resolved_map) stmts in
  (env, stmts)

and type_in_m_with_substitution (env : env) (type_substitution_map : (string * type_) list) (t : Syntax.type_) :
    env * type_ =
  match t with
  | {t= STUnbound; loc} ->
      Env.fresh env (C.unbound loc)
  | {t= STGenericType id; loc} -> (
    (* Generic type parameter - substitute with concrete type *)
    match CCList.assoc_opt ~eq:String.equal id type_substitution_map with
    | Some concrete_type ->
        (env, concrete_type)
    | None ->
        Error.raiseError (Printf.sprintf "Generic type parameter '%s' has no concrete type binding" id) loc )
  | {t= STId path; loc} -> (
    match path with
    | {id; n= None; _} -> (
      (* Check if this is a generic type parameter that should be substituted *)
      match CCList.assoc_opt ~eq:String.equal id type_substitution_map with
      | Some concrete_type ->
          (env, concrete_type)
      | None ->
          (* Regular type lookup *)
          let found = Env.lookType env path loc in
          Env.fresh env (C.path_t loc found.path) )
    | _ ->
        (* Module qualified path - regular lookup *)
        let found = Env.lookType env path loc in
        Env.fresh env (C.path_t loc found.path) )
  | {t= STSize n; loc} ->
      let () =
        if n = 0 then
          let msg = "Empty arrays are not supported" in
          Error.raiseError msg loc
      in
      Env.fresh env (C.size ~loc n)
  | {t= STComposed (name, l); loc} ->
      let env, l = CCList.fold_left_map (fun env t -> type_in_m_with_substitution env type_substitution_map t) env l in
      Env.fresh env (C.withConst loc (TEComposed (name, l)))

(* Generic-aware version of stmt for processing generic function bodies *)
and stmt_with_generics (env : env) (generic_params : string list) (return : type_) (s : Syntax.stmt) : env * stmt list =
  (* Use a modified dexp function that uses generic-aware type resolution *)
  let rec dexp_generic env dexp kind =
    match dexp with
    | {Syntax.d= Syntax.SDTuple l; loc} ->
        (* Processed from the last element, like [dexp], but kept in source order *)
        let env, l_rev =
          CCList.fold_left_map
            (fun env e ->
              let env, e = dexp_generic env e kind in
              (env, e) )
            env (CCList.rev l)
        in
        let l = CCList.rev l_rev in
        let env, t = Env.fresh env (C.tuple ~loc (CCList.map (fun (e : dexp) -> e.t) l)) in
        (env, {d= DTuple l; t; loc})
    | {Syntax.d= Syntax.SDGroup e; _} ->
        dexp_generic env e kind
    | {Syntax.d= Syntax.SDTyped (e, t); _} ->
        let env, e = dexp_generic env e kind in
        let env, t = type_in_m_with_generics env generic_params t in
        checkArrayDimensions env t ;
        let env = unifyRaise ~bind:true env e.loc t e.t in
        (env, e)
    | {Syntax.d= Syntax.SDId (name, dims); loc} ->
        let env, t = Env.fresh env (declaredType loc dims) in
        let env = Env.addVar env name t kind loc in
        (env, {d= DId (name, dims); t; loc})
    | {Syntax.d= Syntax.SDWild; loc} ->
        let env, t = Env.fresh env (C.noreturn loc) in
        (env, {d= DWild; t; loc})
  in
  (* Use the same stmt function but with our generic-aware dexp *)
  stmt_generic env dexp_generic return s

and stmt_generic (env : env) (dexp_func : env -> Syntax.dexp -> Env.local_kind -> env * dexp) (return : type_)
    (s : Syntax.stmt) : env * stmt list =
  match s with
  | {s= SStmtError; _} ->
      (env, [])
  | {s= SStmtBlock stmts; loc} ->
      let env, stmts = Env.withScope env (fun env -> stmt_list_generic env dexp_func return stmts) in
      (env, [{s= StmtBlock stmts; loc}])
  | {s= SStmtVal (lhs, None); loc} ->
      let env, lhs = dexp_func env lhs LocalVal in
      (env, [{s= StmtVal lhs; loc}])
  | {s= SStmtVal (lhs, Some rhs); loc} ->
      let env, dlhs = dexp_func env lhs LocalVal in
      let env = markInitialized env dlhs in
      let env, lhs = lexp ~const:true env (dexp_to_lexp lhs) in
      let env, rhs = exp env rhs in
      let env = unifyRaise ~bind:true env lhs.loc dlhs.t lhs.t in
      let env = unifyExpRaise ~bind:true env dlhs.t rhs in
      (env, [{s= StmtVal dlhs; loc}; {s= StmtBind (lhs, rhs); loc}])
  | {s= SStmtMem (lhs, None, tags); loc} ->
      let env, lhs = dexp_func env lhs (LocalMem tags) in
      (env, [{s= StmtMem (lhs, tags); loc}])
  | {s= SStmtMem (lhs, Some rhs, tags); loc} ->
      let env, dlhs = dexp_func env lhs (LocalMem tags) in
      let env, lhs = lexp env (dexp_to_lexp lhs) in
      let env, rhs = exp env rhs in
      let env = unifyExpRaise ~bind:true env lhs.t rhs in
      (env, [{s= StmtMem (dlhs, tags); loc}; {s= StmtBind (lhs, rhs); loc}])
  (* The remaining statements have no declaration of their own, so they are handled exactly like
     in [stmt], recursing through [stmt_generic] so that any nested declaration keeps using the
     generic-aware [dexp_func]. *)
  | {s= SStmtBind (lhs, rhs); loc} ->
      let env, lhs = lexp env lhs in
      let env, rhs = exp env rhs in
      let env = unifyExpRaise ~bind:true env lhs.t rhs in
      (env, [{s= StmtBind (lhs, rhs); loc}])
  | {s= SStmtReturn e; loc} ->
      let env, e = exp env e in
      let env = unifyExpRaise env return e in
      (env, [{s= StmtReturn e; loc}])
  | {s= SStmtIf (cond, then_, else_); loc} ->
      let env, cond = exp env cond in
      let env = unifyFreshRaise env cond.loc (C.bool ~loc) cond.t in
      let env, then_ = stmt_generic env dexp_func return then_ in
      let env, else_ =
        match else_ with
        | None ->
            (env, None)
        | Some else_ ->
            let env, else_ = stmt_generic env dexp_func return else_ in
            (env, Some (stmt_block else_))
      in
      (env, [{s= StmtIf (cond, stmt_block then_, else_); loc}])
  | {s= SStmtWhile (cond, body); loc} ->
      let env, cond = exp env cond in
      let env = unifyFreshRaise env cond.loc (C.bool ~loc) cond.t in
      let env, body = stmt_generic env dexp_func return body in
      (env, [{s= StmtWhile (cond, stmt_block body); loc}])
  | {s= SStmtIter {id= name, id_loc; value; body}; loc} ->
      let env, while_s = makeIterWhile env name id_loc value body loc in
      stmt_generic env dexp_func return while_s
  | {s= SStmtMatch {e; first_case; other_cases}; _} ->
      let if_stmt = makeIfOfMatch env e first_case other_cases in
      stmt_generic env dexp_func return if_stmt

and stmt_list_generic (env : env) (dexp_func : env -> Syntax.dexp -> Env.local_kind -> env * dexp) (return : type_)
    (stmts : Syntax.stmt list) : env * stmt list =
  CCList.fold_left_map (fun env s -> stmt_generic env dexp_func return s) env stmts
  |> fun (env, nested_lists) -> (env, CCList.flatten nested_lists)

(* The statements of regular functions are checked like the generic bodies, with the regular
   declarations *)
let rec stmt (env : env) (return : type_) (s : Syntax.stmt) : env * stmt list = stmt_generic env dexp return s

and stmt_opt env return s =
  match s with
  | None ->
      (env, None)
  | Some s ->
      let env, s = stmt env return s in
      (env, Some (stmt_block s))

and stmt_list env return l =
  let env, l_rev =
    CCList.fold_left
      (fun (env, acc) s ->
        let env, s = stmt env return s in
        (env, s :: acc) )
      (env, []) l
  in
  (env, CCList.flatten (CCList.rev l_rev))

let addGeneratedFunctions tags name next =
  if Ptags.has tags "wave" then
    let code = Pla.print {%pla|fun <#name#s>_samples() : int @[placeholder] {}|} in
    let def = Parse.parseFunctionDecl code in
    Some {def with next}
  else if Ptags.has tags "wavetable" then
    let samples = Pla.print {%pla|fun <#name#s>_samples() : int @[placeholder] {}|} in
    let code1 = Pla.print {%pla|fun <#name#s>_raw_c0(i:int) : real @[placeholder] {}|} in
    let code2 = Pla.print {%pla|fun <#name#s>_raw_c1(i:int) : real @[placeholder] {}|} in
    let samples = Parse.parseFunctionDecl samples in
    let def1 = Parse.parseFunctionDecl code1 in
    let def2 = Parse.parseFunctionDecl code2 in
    Some {def1 with next= Some {def2 with next= Some {samples with next}}}
  else next

let getOptType (env : env) loc (t : Syntax.type_ option) : env * type_ =
  match t with None -> Env.fresh env (C.unbound loc) | Some t -> type_in_c env t

let getReturnType (env : env) (t : Syntax.type_ option) : env * type_ option =
  match t with
  | None ->
      (env, None)
  | Some t ->
      let env, t = type_in_c env t in
      (env, Some t)

let convertArguments (env : env) (args : Syntax.arg list) : env * arg list =
  let convert env (name, t, loc) =
    let env, t = getOptType env loc t in
    (env, {name; t; loc})
  in
  CCList.fold_left_map convert env args

(* Convert arguments using a pre-created generic type mapping *)
let convertArgumentWithGenericMapping (env : env) (generic_type_map : (string * type_) list)
    ((name, t, loc) : Syntax.arg) : env * arg =
  let env, t =
    match t with
    | None ->
        Env.fresh env (C.unbound loc)
    | Some t ->
        type_in_m_with_generic_mapping env generic_type_map t
  in
  (env, {name; t; loc})

let registerMultiReturnMem (env : env) name t loc =
  let _, ret = t in
  match resolve env ret with
  | {tx= TEComposed ("tuple", elems); _} ->
      let names = CCList.mapi (fun i t -> (pathString name ^ "_ret_" ^ string_of_int i, t)) elems in
      CCList.fold_left (fun env (name, t) -> Env.addReturnVar env name t loc) env names
  | _ ->
      env

let isRoot (args : Args.args) path =
  let s_path = Pla.print (Syntax.print_path path) in
  CCList.mem s_path args.roots

(* When generating Pure Data externals, the tagged functions are the objects of the
   library: they are roots without needing the -root flag. *)
let isPdRoot (args : Args.args) tags =
  match args.template with
  | Some "pd" ->
      Ptags.has tags "pdtilde" || Ptags.has tags "pd" || Ptags.has tags "pdmessage"
  | Some _ | None ->
      false

let customInitializer (env : env) tags name = if Ptags.has tags "init" then Env.addCustomInitFunction env name else env

(* The type the return statements of a function are checked against: the declared one, so that a
   mismatch is reported at the return statement, or a placeholder inferred from them *)
let bodyReturnType (env : env) (specified_ret : type_ option) (loc : Loc.t) : env * type_ =
  match specified_ret with Some t -> (env, t) | None -> Env.fresh env (C.noreturn loc)

let reportReturnTypeMismatch (env : env) is_placeholder loc (specified_ret : type_ option) (body : Syntax.stmt)
    (ret : type_) : env =
  match (specified_ret, Subst.boundTo env.subst ret) with
  | None, None ->
      (* The return type is only bound by the return statements: a function without them returns
         unit. Fixing it here prevents the first caller from turning it into a function of any
         type. *)
      unifyFreshRaise env loc (C.unit ~loc) ret
  | None, Some _ ->
      env
  | Some t, _ ->
      (* A placeholder function has no body to return from, and a unit function has nothing to return *)
      let returns_unit = match (resolve env t).tx with TEId {id= "unit"; n= None; _} -> true | _ -> false in
      if (not is_placeholder) && (not returns_unit) && not (syntax_has_return body) then
        let t = Pla.print (printType env t) in
        Error.raiseError ("This function is expected to have type '" ^ t ^ "' but nothing was returned.") loc
      else env

(* The arguments of a function with an empty body are dictated by its callers, like the 'noteOn'
   handler required by the templates, so they are not reported when unused *)
let isStub (body : Syntax.stmt) : bool = match body.s with SStmtBlock [] -> true | _ -> false

let rec function_def (iargs : Args.args) (env : env) (def : Syntax.function_def) : env * (function_def * stmt) =
  let env, specified_ret = getReturnType env def.t in
  let env, ret = bodyReturnType env specified_ret def.loc in
  let env, args = convertArguments env def.args in
  let env, path, t = Env.enterFunction env def.name args ret def.loc in
  let env, body = stmt env ret def.body in
  let env = if isStub def.body then env else Env.reportUnusedArguments env in
  let env = Env.exitFunction env in
  let next = addGeneratedFunctions def.tags def.name def.next in
  let env, next = function_def_opt iargs env next in
  let env = registerMultiReturnMem env path t def.loc in
  let env = customInitializer env def.tags path in
  let is_root = isRoot iargs path || isPdRoot iargs def.tags in
  let is_placeholder = Ptags.has def.tags "placeholder" in
  let env = reportReturnTypeMismatch env is_placeholder def.loc specified_ret def.body ret in
  (env, ({name= path; args; t; loc= def.loc; tags= def.tags; next; is_root}, stmt_block body))

and function_def_opt (iargs : Args.args) (env : env) def_opt =
  match def_opt with
  | None ->
      (env, None)
  | Some def ->
      let env = Env.addAliasToContext env def.name def.loc in
      let env, def_body = function_def iargs env def in
      (env, Some def_body)

(* Marks the arguments listed in the tag @[mutable] as mutable or constant *)
let applyMutableTag (env : env) (args : Typed.arg list) (tags : Typed.tag list) : env =
  match Ptags.getArguments tags "mutable" with
  | None ->
      env
  | Some [] ->
      env
  | Some vars ->
      CCList.fold_left
        (fun env (arg : arg) ->
          match CCList.find_opt (fun (n, _, _) -> String.compare n arg.name = 0) vars with
          | Some (_, {g= TagBool mut; _}, _) ->
              {env with subst= Subst.setConst env.subst arg.t (not mut)}
          | _ ->
              env )
        env args

let ext_function (iargs : Args.args) (env : env) (def : Syntax.ext_def) : env * function_def =
  let env, ret = getOptType env def.loc def.t in
  let env, args = convertArguments env def.args in
  let env = applyMutableTag env args def.tags in
  let env, path, t = Env.enterFunction env def.name args ret def.loc in
  let env = Env.exitFunction env in
  let next = addGeneratedFunctions def.tags def.name None in
  let env, next = function_def_opt iargs env next in
  (env, {name= path; args; t; loc= def.loc; tags= def.tags; next; is_root= false})

let getContextArgument (env : env) (path : path) loc : env * arg option =
  match Env.getCurrentContext env with
  | Some context when not (Map.is_empty (Env.contextMembers env context)) -> (
      let m = Env.getCurrentModule env in
      let f =
        match Map.find path.id m.functions with
        | Some f ->
            f
        | None ->
            failwith "getContextArgument: function not found"
      in
      match Env.contextArgument f with
      | Some {arg_t; _} ->
          let members = Env.contextMembers env context in
          let env = Env.Map.fold (fun _ (var : var) env -> unifyConstness env arg_t var.t) env members in
          (env, Some {name= context_name; t= arg_t; loc})
      | None ->
          (env, None) )
  | Some _ | None ->
      (env, None)

let insertContextArgument (env : env) (def : function_def) : env * function_def =
  (* Check if the function already has a _ctx argument *)
  let already_has_ctx = match def.args with {name; _} :: _ when String.equal name context_name -> true | _ -> false in
  if already_has_ctx then (env, def)
  else
    match getContextArgument env def.name def.loc with
    | env, None ->
        (env, def)
    | env, Some arg ->
        let rec loop (next : (function_def * stmt) option) : (function_def * stmt) option =
          match next with
          | Some (def, body) ->
              let next = loop def.next in
              Some ({def with args= arg :: def.args; next}, body)
          | None ->
              None
        in
        let next = loop def.next in
        (env, {def with args= arg :: def.args; next})

(* A constant declaration only names one value, so the parenthesized and annotated forms are
   unwrapped to reach the identifier that gives the constant its name and dimensions. *)
let rec constantDecl (d : Syntax.dexp) : (string * int option) option =
  match d.d with
  | SDId (name, dims) ->
      Some (name, dims)
  | SDGroup e | SDTyped (e, _) ->
      constantDecl e
  | SDWild | SDTuple _ ->
      None

let rec top_dexp (env : env) (d : Syntax.dexp) =
  match d with
  | {d= SDId (name, dims); loc} ->
      let env, t = Env.fresh env (declaredType loc dims) in
      (*let env = Env.addVar env name t kind loc in*)
      (env, {d= DId (name, dims); t; loc})
  | {d= SDGroup e; _} ->
      top_dexp env e
  | {d= SDTyped (e, t); _} ->
      let env, e = top_dexp env e in
      let env, t = type_in_m env t in
      checkArrayDimensions env t ;
      let env = unifyRaise ~bind:true env e.loc t e.t in
      (env, e)
  | {d= SDWild; loc} ->
      Error.raiseError "A constant needs a name." loc
  | {d= SDTuple _; loc} ->
      Error.raiseError "A constant can only declare one value." loc

let convert_generic_param (env : env) (param : Syntax.generic_param) : env * Typed.generic_param =
  match param with
  | Syntax.GParamFunction (name, type_opt) ->
      (* Validate function parameter name *)
      if String.length name = 0 then Error.raiseError "Generic function parameter name cannot be empty" Loc.default ;
      (* For function templates, we'll determine the actual type during instantiation *)
      (* The type_opt can be used as a constraint later *)
      let env, converted_type =
        match type_opt with
        | None ->
            (env, None)
        | Some t ->
            let env, t' = type_in_m env t in
            (* Validate that the constraint type is actually a function type *)
            ( match (resolve env t').tx with
            | TEFunction (_, _) ->
                ()
            | _ ->
                Error.raiseError
                  (Printf.sprintf "Generic function parameter '%s' type constraint must be a function type, got %s" name
                     (Pla.print (printType env t')) )
                  Loc.default ) ;
            (* Store the constraint type, but actual function type determined at instantiation *)
            (env, Some t')
      in
      (env, Typed.GParamFunction (name, converted_type))
  | Syntax.GParamType name ->
      (* Validate type parameter name *)
      if String.length name = 0 then Error.raiseError "Template type parameter name cannot be empty" Loc.default ;
      (env, Typed.GParamType name)
  | Syntax.GParamConstant (name, type_expr, loc) ->
      (* Validate constant parameter name *)
      if String.length name = 0 then Error.raiseError "Generic constant parameter name cannot be empty" Loc.default ;
      let env, converted_type = type_in_m env type_expr in
      (* Validate that the type is a valid constant type (allow unbound for inference) *)
      ( match (resolve env converted_type).tx with
      | TEId {id= "int" | "real" | "bool" | "string"; _} ->
          ()
      | TEUnbound _ ->
          () (* Allow unbound types - will be inferred from call site *)
      | _ ->
          Error.raiseError
            (Printf.sprintf "Generic constant parameter '%s' must have type int, real, bool, or string, got %s" name
               (Pla.print (printType env converted_type)) )
            loc ) ;
      (env, Typed.GParamConstant (name, converted_type, loc))

let create_generic_function (env : env) (def : Syntax.function_def) : env * Typed.generic_function =
  (* Validate generic function has generic parameters *)
  if CCList.length def.generic_params = 0 then
    Error.raiseError (Printf.sprintf "Function '%s' marked as template but has no generic parameters" def.name) def.loc ;
  (* Check for duplicate generic parameter names *)
  let param_names =
    CCList.map
      (function
        | Syntax.GParamFunction (name, _) ->
            name
        | Syntax.GParamType name ->
            name
        | Syntax.GParamConstant (name, _, _) ->
            name )
      def.generic_params
  in
  let unique_names = CCList.sort_uniq ~cmp:String.compare param_names in
  if CCList.length unique_names <> CCList.length param_names then
    Error.raiseError (Printf.sprintf "Generic function '%s' has duplicate generic parameter names" def.name) def.loc ;
  (* Note: Type parameter names like 'a can have the same base name as function arguments
     like 'a' since they are in separate namespaces (type vs value). *)
  (* Extract just the type parameter names for context *)
  let type_param_names =
    CCList.filter_map (function Syntax.GParamType name -> Some name | _ -> None) def.generic_params
  in
  (* Create a shared mapping from type parameter names to unbound types *)
  (* This ensures all occurrences of the same parameter use the same unbound type *)
  let env, generic_type_map = createGenericTypeMapping env type_param_names def.loc in
  let convertParam env = function
    | Syntax.SlotArg arg ->
        let env, arg = convertArgumentWithGenericMapping env generic_type_map arg in
        (env, Typed.SlotArg arg)
    | Syntax.SlotGeneric param ->
        let env, param = convert_generic_param env param in
        (env, Typed.SlotGeneric param)
  in
  let env, params = CCList.fold_left_map convertParam env def.params in
  let args = CCList.filter_map (function Typed.SlotArg arg -> Some arg | Typed.SlotGeneric _ -> None) params in
  (* Process return type with the same generic type mapping *)
  let env, inferred_ret =
    match def.t with
    | Some ret_type ->
        type_in_m_with_generic_mapping env generic_type_map ret_type
    | None ->
        Env.fresh env (C.noreturn def.loc)
  in
  (* Create function type from regular arguments only (exclude template params) *)
  let arg_types = CCList.map (fun (arg : Typed.arg) -> arg.t) args in
  (* Capture the type index at definition time - this ensures specialized types appear near the generic's position *)
  let env, type_index = Env.nextTypeIndex env in
  ( env
  , { name= def.name
    ; args
    ; params
    ; t= (arg_types, inferred_ret)
    ; type_param_map= generic_type_map
    ; body= def.body
    ; next= def.next
    ; loc= def.loc
    ; tags= def.tags
    ; type_index } )

let has_generic_params (def : Syntax.function_def) : bool = not (CCList.is_empty def.generic_params)

let rec top_stmt (iargs : Args.args) (env : env) (s : Syntax.top_stmt) : env * top_stmt list =
  match s with
  | {top= STopError; _} ->
      failwith "Parser error"
  | {top= STopFunction def; loc} when has_generic_params def ->
      (* Store generic function and emit placeholder to mark where specializations go *)
      let env, generic_func = create_generic_function env def in
      let env = Env.addGeneric env generic_func in
      (env, [{top= TopGenericPlaceholder def.name; loc}])
  | {top= STopFunction def; _} ->
      let env = Env.createContextForFunction env def.name def.loc in
      let env, (def, body) = function_def iargs env def in
      let env, def = insertContextArgument env def in
      let env = Env.exitContext env in
      (* Generic function instantiation is now handled in toprog.ml when processing EGenCall *)
      (env, [{top= TopFunction (def, body); loc= def.loc}])
  | {top= STopExternal (def, link_name); _} ->
      let env = Env.createContextForExternal env in
      let env, def = ext_function iargs env def in
      let env = Env.exitContext env in
      (env, [{top= TopExternal (def, link_name); loc= def.loc}])
  | {top= STopType {name; members}; loc} ->
      let convertMember env (name, t, tags, loc) =
        let env, t = type_in_m env t in
        (env, (name, t, tags, loc))
      in
      let env, members = CCList.fold_left_map convertMember env members in
      let members = CCList.sort (fun (n1, _, _, _) (n2, _, _, _) -> compare n1 n2) members in
      let env = Env.addType env name members loc in
      let m = Env.getCurrentModule env in
      let path = Env.getPath m name loc in
      (env, [{top= TopType {path; members}; loc}])
  | {top= STopEnum {name; members}; loc} ->
      let env = Env.addEnum env name members loc in
      let m = Env.getCurrentModule env in
      let path = Env.getPath m name loc in
      (env, [{top= TopEnum {path; members}; loc}])
  | {top= STopConstant (d, e); loc} -> (
    match constantDecl d with
    | None ->
        let env, _ = top_dexp env d in
        (env, [])
    | Some (name, dim) ->
        let env, d = top_dexp env d in
        let env, e = exp ~context:constant_context env e in
        let env = unifyRaise env e.loc d.t e.t in
        let m = Env.getCurrentModule env in
        let path = Env.getPath m name loc in
        let env = Env.addConstant env name d.t loc in
        (env, [{top= TopConstant (path, dim, d.t, e, None); loc}]) )

and top_stmt_list (iargs : Args.args) (env : env) (s : Syntax.top_stmt list) : env * top_stmt list =
  let env, rev_groups =
    CCList.fold_left
      (fun (env, acc) s ->
        let env, stmt_list = top_stmt iargs env s in
        (env, stmt_list :: acc) )
      (env, []) s
  in
  (* Return the statements in source order *)
  (env, CCList.concat (CCList.rev rev_groups))

let getTypesFromModule m =
  Map.fold
    (fun _ t s ->
      match t.descr with
      | Record members when Map.is_empty members ->
          s
      | Record _ ->
          t :: s
      | Alias _ ->
          t :: s
      | Simple | Enum _ ->
          s )
    [] m.Env.types

let createTypes (env : env) =
  let types =
    Map.fold
      (fun _ m s ->
        let types = getTypesFromModule m in
        types @ s )
      [] env.modules
  in
  (* sort the types *)
  let types =
    types
    |> CCList.filter (fun (t : Env.t) -> t.generated)
    |> CCList.sort (fun (a : Env.t) b -> compare a.index b.index)
  in
  CCList.map
    (fun (t : Env.t) ->
      match t.descr with
      | Record members ->
          let members = Map.fold (fun _ (var : Env.var) s -> (var.name, var.t, var.tags, var.loc) :: s) [] members in
          let members = CCList.sort (fun (n1, _, _, _) (n2, _, _, _) -> compare n1 n2) members in
          {top= TopType {path= t.path; members}; loc= t.loc}
      | Alias (path, alias_of) ->
          {top= TopAlias {path; alias_of}; loc= t.loc}
      | Enum _ | Simple ->
          failwith "There should not be other than records here" )
    types

module Set = Set.Make (struct
  type t = path

  let compare = Syntax.compare_path
end)

let rec createExistingTypeSet stmts : Set.t =
  match stmts with
  | [] ->
      Set.empty
  | {top= TopType {path; _}; _} :: t ->
      Set.add path (createExistingTypeSet t)
  | _ :: t ->
      createExistingTypeSet t

let removeExistingTypes set types =
  let f s = match s with {top= TopType {path; _}; _} when Set.mem path set -> false | _ -> true in
  CCList.filter f types

let guessFailureError (failure : Subst.guess_failure) : Error.t =
  match failure with
  | AmbiguousType {kind= RecordGuess; candidates; loc} ->
      let names = quotedNames (CCList.map Env.pathString candidates) in
      Error.makeError
        ( "The type of this value cannot be decided: it can be any of the record types " ^ names
        ^ ". Add a type annotation." )
        loc
  | AmbiguousType {kind= EnumGuess; candidates; loc} ->
      let names = quotedNames (CCList.map Env.pathString candidates) in
      Error.makeError
        ( "The enumeration of this value cannot be decided: it can belong to any of " ^ names
        ^ ". Add a type annotation." )
        loc
  | UndeterminedRecord {members; loc} ->
      Error.makeError (noRecordTypeMessage members) loc

(* Decides the types of the values that were guessed from the members used on them. It is done once
   the whole program is typechecked, since the uses that follow a function can decide the types of
   its arguments. *)
let resolveRecordGuesses (env : env) : env =
  match Subst.resolveGuesses env.subst with
  | subst, [] ->
      {env with subst}
  | _, failures ->
      raise (Error.Errors (CCList.map guessFailureError failures))

(* Instantiate the elaborator with the pieces of the typechecker it needs *)
module Elab = Elaboration.Make (struct
  let unify env t1 t2 = unify env t1 t2

  let unifyRaise ?bind env loc t1 t2 = unifyRaise ?bind env loc t1 t2

  let function_def_opt = function_def_opt

  let stmt_with_type_substitution = stmt_with_type_substitution

  let insertContextArgument = insertContextArgument

  let propagateVariability = propagateVariability
end)

(* Typechecks and elaborates a single file against an already-elaborated environment.
   Used to evaluate expressions (e.g. -eval) in the context of an existing program:
   generic calls reuse the specializations the program already created. *)
let typecheck_single (iargs : Args.args) (env : env) (h : Parse.parsed_file) : env * top_stmt list =
  let set = createExistingTypeSet (createTypes env) in
  let env = Env.enterModule env h.name in
  let env, stmt = top_stmt_list iargs env h.stmts in
  let env = Env.exitModule env in
  let env = resolveRecordGuesses env in
  let env, stmt = Elab.elaborate ~reuse_existing:true iargs env [(h.name, stmt)] in
  let env = Env.zonk env in
  let stmt = Subst.zonkProgram env.subst stmt in
  let types = removeExistingTypes set (createTypes env) in
  (env, stmt @ types)

(* Type checking only - does NOT elaborate generics.
   Returns typed AST with EGenCall nodes preserved.
   Caller should call Elaboration.elaborate before code generation. *)
let extensionOfString (s : string) : Env.extension option =
  match s with "vcv-prototype" -> Some Env.VCVPrototype | _ -> None

let getExtensions (args : Args.args) : Env.extension list =
  (* Implicit extensions from template selection *)
  let implicit =
    match (args.code, args.template) with Args.LuaCode, Some "vcv-prototype" -> [Env.VCVPrototype] | _ -> []
  in
  (* Explicit extensions from -extension flags *)
  let explicit = CCList.filter_map extensionOfString args.extensions in
  (* Deduplicate *)
  CCList.sort_uniq ~cmp:compare (implicit @ explicit)

let typecheck (iargs : Args.args) (parsed : Parse.parsed_file list) : env * (string * top_stmt list) list =
  let extensions = getExtensions iargs in
  let env, module_stmts =
    CCList.fold_left
      (fun (env, acc) (h : Parse.parsed_file) ->
        let env = Env.enterModule env h.name in
        let env, stmt = top_stmt_list iargs env h.stmts in
        let env = Env.exitModule env in
        (env, (h.name, stmt) :: acc) )
      (Env.empty ~extensions (), [])
      parsed
  in
  let env = resolveRecordGuesses env in
  let module_stmts = CCList.rev module_stmts in
  Tablecheck.check env.subst iargs.includes module_stmts ;
  (env, module_stmts)

(* Type checking with elaboration - for backwards compatibility.
   This is equivalent to calling typecheck followed by Elab.elaborate. *)
let typecheck_and_elaborate (iargs : Args.args) (parsed : Parse.parsed_file list) : env * top_stmt list =
  let env, module_stmts = Util.Profile.time "Typechecking: infer" (fun () -> typecheck iargs parsed) in
  let env, elaborated = Util.Profile.time "Typechecking: elaborate" (fun () -> Elab.elaborate iargs env module_stmts) in
  let env = Util.Profile.time "Typechecking: resolve env" (fun () -> Env.zonk env) in
  let elaborated =
    Util.Profile.time "Typechecking: resolve program" (fun () -> Subst.zonkProgram env.subst elaborated)
  in
  let types = createTypes env in
  (env, types @ elaborated)
