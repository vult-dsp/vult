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

let context_name = "_ctx"

(* Helper functions to create common contexts *)
let normal_context = {Env.in_constant= false; in_generic_arg= false}

let constant_context = {Env.in_constant= true; in_generic_arg= false}

let generic_arg_context = {Env.in_constant= false; in_generic_arg= true}

(* Check if a syntax statement contains mem declarations *)
let rec syntax_has_mem (stmt : Syntax.stmt) : bool =
  match stmt.s with
  | SStmtMem _ ->
      true
  | SStmtBlock stmts ->
      CCList.exists syntax_has_mem stmts
  | SStmtIf (_, then_stmt, else_opt) ->
      syntax_has_mem then_stmt || Option.fold ~none:false ~some:syntax_has_mem else_opt
  | SStmtWhile (_, body) ->
      syntax_has_mem body
  | _ ->
      false

let pickLoc (t1 : type_) (t2 : type_) : unit =
  if t1.loc == Loc.default then t1.loc <- t2.loc else if t2.loc == Loc.default then t2.loc <- t1.loc

let linkType ~from ~into =
  into.tx <- TELink from ;
  pickLoc from into ;
  true

let unlink = Typed.unlink

let pathString = Typed.pathString

(* Tries to unity the given type with all the elements of the set, if they can be unified we increase the counter *)
let rec pushTypeToSet (set : (type_ * int) list) (elem : type_) =
  match (set, elem) with
  | [], _ ->
      [(elem, 1)]
  | (({tx= TEComposed (n1, e1); _} as h), count) :: t, {tx= TEComposed (n2, e2); _}
    when n2 = n1 && CCList.length e1 = CCList.length e2 ->
      if unify h elem then (h, count + 1) :: t else (h, count) :: pushTypeToSet t elem
  (* Don't try to unify composed types with different arguments *)
  | (({tx= TEComposed (n1, _); _} as h), count) :: t, {tx= TEComposed (n2, _); _} when n2 = n1 ->
      (h, count) :: pushTypeToSet t elem
  | (h, count) :: t, _ ->
      if unify h elem then (h, count + 1) :: t else (h, count) :: pushTypeToSet t elem

and constrainOption loc l1 l2 =
  let set = CCList.fold_left pushTypeToSet (CCList.map (fun e -> (e, 1)) l1) l2 in
  let final_set = CCList.filter_map (fun (e, n) -> if n > 1 then Some e else None) set in
  match final_set with
  | [] ->
      let t1 = Pla.map_sep Pla.commaspace Typed.print_type_ l1 in
      let t2 = Pla.map_sep Pla.commaspace Typed.print_type_ l2 in
      let msg =
        Pla.print {%pla|None of the following types: <#t1#>, matches with any of the following types <#t2#>. |}
      in
      Error.raiseError msg loc
  | [t] ->
      t
  | l ->
      {tx= TEOption l; loc= Loc.default; const= C.const ()}

and pickOption original l tt =
  let rec loop l =
    match l with [] -> false | h :: t -> if unify h tt then linkType ~from:tt ~into:original else loop t
  in
  loop l

and unifyConstnessValue (t1 : constness) (t2 : constness) =
  if t1 == t2 then ()
  else
    match (t1.c, t2.c) with
    | TECLink tl, _ ->
        unifyConstnessValue tl t2
    | _, TECLink tl ->
        unifyConstnessValue t1 tl
    | TEConst _, _ ->
        t1.c <- TECLink t2
    | _, TEConst _ ->
        t2.c <- TECLink t1
    | TEMut _, TEMut _ ->
        ()

and unifyConstness (t1 : type_) (t2 : type_) =
  unifyConstnessValue t1.const t2.const ;
  match (t1.tx, t2.tx) with
  | TELink tlink, _ ->
      unifyConstness tlink t2
  | _, TELink tlink ->
      unifyConstness t1 tlink
  | _ ->
      ()

and unify ?(bind = false) (t1 : type_) (t2 : type_) =
  if t1 == t2 then true
  else (
    (* transfer memory use to determine constness *)
    if bind then unifyConstnessValue t1.const t2.const ;
    match (t1.tx, t2.tx) with
    | TEId t1, TEId t2 ->
        Pparser.Syntax.compare_path t1 t2 = 0
    | TESize t1, TESize t2 ->
        t1 = t2
    | TEFunction (arg1, ret1), TEFunction (arg2, ret2) ->
        CCList.for_all2 unify arg1 arg2 && unify ret1 ret2
    | TEFunction _, _ ->
        false
    | _, TEFunction _ ->
        false
    (* special case for arrays without dimensions *)
    | TEComposed ("array", [e1; _]), TEComposed ("array", [e2])
    | TEComposed ("array", [e1]), TEComposed ("array", [e2; _]) ->
        unify e1 e2
    | TEComposed (n1, e1), TEComposed (n2, e2) when n1 = n2 && CCList.length e1 = CCList.length e2 ->
        CCList.for_all2 unify e1 e2
    (* follow the links *)
    | TELink tlink, _ ->
        unify tlink t2
    | _, TELink tlink ->
        unify t1 tlink
    | TENoReturn, _ ->
        linkType ~from:t2 ~into:t1
    | _, TENoReturn ->
        linkType ~from:t1 ~into:t2
    (* replace any unbound *)
    | TEUnbound None, TEUnbound _ ->
        linkType ~from:t1 ~into:t2
    | TEUnbound _, TEUnbound None ->
        linkType ~from:t2 ~into:t1
    | TEUnbound _, _ ->
        linkType ~from:t2 ~into:t1
    | _, TEUnbound _ ->
        linkType ~from:t1 ~into:t2
    (* types with alternatives *)
    | TEOption l1, TEOption l2 ->
        let t3 = constrainOption t2.loc l1 l2 in
        let _ = linkType ~from:t3 ~into:t2 in
        linkType ~from:t3 ~into:t1
    | TEOption l, _ ->
        pickOption t1 l t2
    | _, TEOption l ->
        pickOption t2 l t1
    | TEId _, _ ->
        false
    | TESize _, _ ->
        false
    | TEComposed _, _ ->
        false )

let unifyRaise ?(bind = false) (loc : Loc.t) (t1 : type_) (t2 : type_) : unit =
  (* TODO: improve unify error reporting for tuples *)
  let raise = true in
  if not (unify ~bind t1 t2) then
    let msg =
      let t1 = print_type_ t1 in
      let t2 = print_type_ t2 in
      Pla.print {%pla|This expression has type '<#t2#>' but '<#t1#>' was expected|}
    in
    if raise then Error.raiseError msg loc
    else (
      print_endline (Loc.to_string loc) ;
      print_endline msg )

(* Convert a type with a mapping from generic parameter names to their unbound types *)
let rec type_in_m_with_generic_mapping (env : env) (generic_type_map : (string * type_) list) (t : Syntax.type_) =
  match t with
  | {t= STUnbound; loc} ->
      {tx= TEUnbound None; loc; const= C.const ()}
  | {t= STGenericType id; loc} -> (
    (* Explicit generic type parameter - look up in the mapping *)
    match CCList.assoc_opt ~eq:String.equal id generic_type_map with
    | Some unbound_type ->
        unbound_type
    | None ->
        Error.raiseError (Printf.sprintf "Generic type parameter '%s' is not declared in the function" id) loc )
  | {t= STId path; loc} -> (
    match path with
    | {id; n= None; _} -> (
      (* Check if this is a generic type parameter *)
      match CCList.assoc_opt ~eq:String.equal id generic_type_map with
      | Some unbound_type ->
          unbound_type
      | None ->
          (* Regular type lookup *)
          let found = Env.lookType env path loc in
          {tx= TEId found.path; loc; const= C.const ()} )
    | _ ->
        (* Regular type lookup *)
        let found = Env.lookType env path loc in
        {tx= TEId found.path; loc; const= C.const ()} )
  | {t= STSize n; loc} ->
      let () =
        if n = 0 then
          let msg = "Empty arrays are not supported" in
          Error.raiseError msg loc
      in
      {tx= TESize n; loc; const= C.const ()}
  | {t= STComposed (name, l); loc} ->
      let l = CCList.map (type_in_m_with_generic_mapping env generic_type_map) l in
      {tx= TEComposed (name, l); loc; const= C.const ()}

(* Helper to create a mapping from generic parameter names to unbound types *)
let createGenericTypeMapping (generic_params : string list) (loc : Loc.t) : (string * type_) list =
  CCList.map (fun name -> (name, {tx= TEUnbound None; loc; const= C.const ()})) generic_params

(* Legacy wrapper for backwards compatibility - creates fresh mapping each time *)
let type_in_m_with_generics (env : env) (generic_params : string list) (t : Syntax.type_) =
  let generic_type_map = createGenericTypeMapping generic_params t.loc in
  type_in_m_with_generic_mapping env generic_type_map t

let type_in_m (env : env) (t : Syntax.type_) = type_in_m_with_generics env [] t

let rec checkArrayDimensions (t : type_) =
  match t.tx with
  | TEComposed ("array", [_]) ->
      Error.raiseError "Array type declaration missing size. Use 'array(type, size)' format (e.g., 'array(real, 10)')"
        t.loc
  | TEComposed ("array", [_; _]) ->
      ()
  | TELink t ->
      checkArrayDimensions t
  | _ ->
      ()

let type_in_c (env : env) (t : Syntax.type_) = type_in_m (Env.exitContext env) t

let type_in_f (env : env) (t : Syntax.type_) = type_in_c (Env.exitFunction env) t

let applyFunction loc (args_t_in : type_ list) (ret : type_) (args_in : exp list) =
  let rec loop (args_t : type_ list) args =
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
        ret
    | h :: args_t, (ht : exp) :: args ->
        unifyRaise ht.loc h ht.t ; loop args_t args
  in
  loop args_t_in args_in

let rec markExpMutable env exp loc =
  match exp.e with
  | EId name -> (
    match Env.lookVar env name loc with var -> Typed.setTypeMut var.t | exception Error.Errors _ -> () )
  | EMember (e, _) ->
      markExpMutable env e loc
  | EIndex {e; _} ->
      markExpMutable env e loc
  | _ ->
      ()

let propagateVariability env loc (args : Typed.arg list option) (exp_args : exp list) =
  match args with
  | None ->
      ()
  | Some args ->
      (* Only propagate if the lists have the same length to avoid iter2 failures.
       Length mismatches can happen with external functions or context arguments. *)
      if CCList.length args = CCList.length exp_args then
        CCList.iter2
          (fun (arg : arg) (exp : exp) -> if isTypeConst arg.t = false then markExpMutable env exp loc)
          args exp_args

let typeToMangledName = Typed.typeToMangledName

let rec addContextArg (env : env) instance (f : Env.f) args loc =
  if Env.isFunctionActive f then (
    let cpath = Env.getContext env in
    let fpath = Env.getFunctionContext f in
    (* get the context type of the current function *)
    let ctx_t =
      let f = Env.getCurrentFunction env in
      match Env.lookVarInScopes f.locals context_name with
      | Some var ->
          var.t
      | None ->
          failwith "context var not declared"
    in
    (* get the context type of the function we are calling *)
    let fctx_t =
      match Env.lookVarInScopes f.locals context_name with
      | Some var ->
          var.t
      | None ->
          failwith "context var not declared"
    in
    let is_ctx_mutable = isTypeConst fctx_t = false in
    match (Syntax.compare_path cpath fpath, instance) with
    | 0, None ->
        let e = {e= EId context_name; t= fctx_t; loc} in
        let () = if is_ctx_mutable then markExpMutable env e loc in
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
        let rec generateName () =
          let n = Env.getFunctionTick env in
          let name = "inst_" ^ string_of_int n ^ number in
          if checkMemExists env name || Env.checkConstantExists env name then generateName () else name
        in
        let name = generateName () in
        let env = Env.addVar env unify name fctx_t Inst loc in
        let e = {e= EMember ({e= EId context_name; t= ctx_t; loc}, name); loc; t= fctx_t} in
        let () = if is_ctx_mutable then markExpMutable env e loc in
        (env, e :: args)
    (* intance without subscripts *)
    | _, Some (name, None) ->
        let env = Env.addVar env unify name fctx_t Inst loc in
        let e = {e= EMember ({e= EId context_name; t= ctx_t; loc}, name); loc; t= fctx_t} in
        let () = if is_ctx_mutable then markExpMutable env e loc in
        (env, e :: args)
    (* array of instances *)
    | _, Some (name, Some index) ->
        let env, index = exp ~context:normal_context env index in
        unifyRaise index.loc (C.int ~loc:Loc.default) index.t ;
        let t = C.array ~loc fctx_t in
        let env = Env.addVar env unify name t Inst loc in
        let e = {e= EMember ({e= EId context_name; t= ctx_t; loc}, name); loc; t= fctx_t} in
        let e = {e= EIndex {e; index}; loc; t= fctx_t} in
        let () = if is_ctx_mutable then markExpMutable env e loc in
        (env, e :: args) )
  else (env, args)

and call (env : env) instance path args loc eloc =
  (* First check if this is a generic function *)
  match Env.lookupGeneric env path with
  | Some generic_func ->
      (* This is a generic call - handle instantiation *)
      (* NOTE: Don't process args with exp_list yet - generic_call will handle them *)
      generic_call env instance path generic_func args loc eloc
  | None -> (
    (* Try regular function lookup first *)
    match Env.tryLookFunctionCall env path with
    | Some f ->
        (* Regular function call *)
        let env, args = exp_list env args in
        let args_t, ret = f.t in
        let t = applyFunction eloc args_t ret args in
        let () = propagateVariability env loc f.args args in
        let env, args = addContextArg env instance f args loc in
        (env, {e= ECall {instance= None; path= f.path; args}; t; loc})
    | None -> (
      (* Check if this might be a companion function of a generic *)
      match Env.lookupGenericByCompanion env path with
      | Some parent_generic ->
          (* This is a companion call - create EGenCompanionCall for later processing *)
          let env, processed_args = exp_list env args in
          (* For now, we use noreturn as the return type - it will be resolved during instantiation *)
          let t = C.noreturn loc in
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
          (* Function not found - raise the standard error *)
          let _ = Env.lookFunctionCall env path loc in
          (* lookFunctionCall will raise, so this is unreachable *)
          failwith "Unreachable" ) )

and generic_call (env : env) (instance : (string * Syntax.exp option) option) (generic_path : Syntax.path)
    (generic_func : Typed.generic_function) (args : Syntax.exp list) (_ : Loc.t) (eloc : Loc.t) : env * exp =
  (* Count only explicit generic parameters (exclude implicit type parameters) *)
  let explicit_generic_param_count =
    CCList.count
      (function
        | Typed.GParamType _ ->
            false (* Implicit type parameters - inferred from function args *)
        | _ ->
            true (* Explicit parameters - require explicit arguments *) )
      generic_func.generic_params
  in
  let function_param_count = CCList.length generic_func.args in
  let total_expected = CCList.length generic_func.param_order in
  let total_provided = CCList.length args in
  if total_provided < total_expected then
    Error.raiseError
      (Printf.sprintf
         "Generic function '%s' expects %d arguments (%d explicit generic parameters + %d function parameters) but got \
          %d"
         generic_func.name total_expected explicit_generic_param_count function_param_count total_provided )
      eloc ;
  if total_provided > total_expected then
    Error.raiseError
      (Printf.sprintf
         "Generic function '%s' expects %d arguments (%d explicit generic parameters + %d function parameters) but got \
          %d"
         generic_func.name total_expected explicit_generic_param_count function_param_count total_provided )
      eloc ;
  (* Split arguments using param_order to handle interleaved params *)
  let args_array = Array.of_list args in
  let rec split_args (pos : int) (gen_acc : Syntax.exp list) (arg_acc : Syntax.exp list) (order : Typed.param_kind list)
      : Syntax.exp list * Syntax.exp list =
    match order with
    | [] ->
        (List.rev gen_acc, List.rev arg_acc)
    | Typed.PKGeneric _ :: rest ->
        split_args (pos + 1) (args_array.(pos) :: gen_acc) arg_acc rest
    | Typed.PKArg _ :: rest ->
        split_args (pos + 1) gen_acc (args_array.(pos) :: arg_acc) rest
  in
  let explicit_generic_args, function_args = split_args 0 [] [] generic_func.param_order in
  (* Process explicit template arguments with template argument context (allows function references) *)
  let env, processed_explicit_generic_args = exp_list ~context:generic_arg_context env explicit_generic_args in
  (* Process regular function arguments with normal context *)
  let env, processed_function_args = exp_list ~context:normal_context env function_args in
  (* Create fresh copies of the generic function's argument types for unification *)
  (* This allows the types to be constrained by unification without polluting the original *)
  let generic_func_arg_types, generic_func_ret_type = generic_func.t in
  let all_orig_types = generic_func_arg_types @ [generic_func_ret_type] in
  let all_fresh_types = Typed.copy_types_preserving_sharing all_orig_types in
  let fresh_arg_types, fresh_ret_type =
    match CCList.rev all_fresh_types with
    | last :: rest ->
        (CCList.rev rest, last)
    | [] ->
        failwith "copy_types_preserving_sharing returned empty list"
  in
  (* Unify argument types to constrain the fresh types *)
  CCList.iter2
    (fun fresh_t (arg : Typed.exp) ->
      let _ = unify fresh_t arg.t in
      () )
    fresh_arg_types processed_function_args ;
  (* Return type is now constrained through unification (may still be unbound if type depends on *)
  (* context not yet available - that's fine, it will be resolved by further unification) *)
  let t = applyFunction eloc fresh_arg_types fresh_ret_type processed_function_args in
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
          ; args= processed_function_args
          ; explicit_args= processed_explicit_generic_args }
    ; t
    ; loc= eloc } )

and exp ?(context = normal_context) ?(in_constant_context = false) (env : env) (e : Syntax.exp) : env * exp =
  (* Convert legacy in_constant_context parameter to new context *)
  let context = if in_constant_context then {context with in_constant= true} else context in
  match e with
  | {e= SEBool value; loc} ->
      let t = C.bool ~loc in
      (env, {e= EBool value; t; loc})
  | {e= SEInt value; loc} ->
      let t = C.int ~loc in
      (env, {e= EInt (int_of_string value); t; loc})
  | {e= SEReal value; loc} ->
      let t = C.real ~loc in
      (env, {e= EReal (float_of_string value); t; loc})
  | {e= SEFixed value; loc} ->
      let t = C.fix16 ~loc in
      let value = String.sub value 0 (String.length value - 1) in
      (env, {e= EFixed (float_of_string value); t; loc})
  | {e= SEString value; loc} ->
      let t = C.string ~loc in
      (env, {e= EString value; t; loc})
  | {e= SEGroup e; _} ->
      exp ~context env e
  | {e= SEId name; loc} when not (String.equal (String.capitalize_ascii name) name) -> (
      let name_path : path = {id= name; n= None; loc} in
      match Env.lookupExpressionSymbol env name_path context with
      | ExprVariable var ->
          let t = var.t in
          let e =
            match var.kind with
            | Val ->
                {e= EId name; t; loc}
            | Const ->
                let m = Env.getCurrentModule env in
                {e= EConst {id= name; n= Some m.name; loc}; t; loc}
            | Mem _ | Inst ->
                let ctx = Env.getContext env in
                let ctx_t = C.path_t loc ctx in
                {e= EMember ({e= EId context_name; t= ctx_t; loc}, name); t; loc}
          in
          (env, e)
      | ExprEnum (type_path, tloc, index) ->
          let t = C.path_t tloc type_path in
          (env, {e= EInt index; t; loc})
      | ExprFunction f ->
          if context.in_generic_arg then
            (* In template argument context, allow function references *)
            let args_t, ret = f.t in
            let t = {tx= TEFunction (args_t, ret); const= C.const (); loc} in
            (env, {e= EId name; t; loc}) (* Return function reference *)
          else
            (* In regular context, functions must be called with parentheses *)
            Error.raiseError ("Function '" ^ name ^ "' must be called with parentheses (e.g., '" ^ name ^ "()')") loc
      | ExprType _ ->
          (* Types in expression context are not directly supported - treat as error *)
          Error.raiseError
            ("Type '" ^ name ^ "' cannot be used as a value. Use it in variable declarations or type annotations")
            loc
      | ExprNotFound ->
          Error.raiseError ("Undefined symbol '" ^ name ^ "'. Check spelling or ensure it's declared before use") loc )
  | {e= SEIndex {e; index}; loc} ->
      let env, e = exp ~context env e in
      let env, index = exp ~context env index in
      let t = C.unbound Loc.default in
      (* Allow indexing on arrays and lists *)
      unifyRaise e.loc (C.indexable t) e.t ;
      unifyRaise index.loc (C.int ~loc:Loc.default) index.t ;
      (* if the type is a builtin (a value) do not unify the constness *)
      let () = if (not context.in_constant) && not (Env.isBuiltinType t) then unifyConstness t e.t in
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
            unifyRaise e.loc h.t e.t ;
            (env, e :: acc, size + 1) )
          (env, [], 1) t
      in
      let t = C.array ~size:(C.size ~loc size) h.t in
      (env, {e= EArray (h :: CCList.rev t_rev); t; loc})
  | {e= SETuple l; loc} ->
      let env, l = exp_list ~context env l in
      let t = C.tuple ~loc (CCList.map (fun (e : exp) -> e.t) l) in
      (env, {e= ETuple l; t; loc})
  | {e= SEIf {cond; then_; else_}; loc} ->
      let env, cond = exp ~context env cond in
      let env, then_ = exp ~context env then_ in
      let env, else_ = exp ~context env else_ in
      let t = then_.t in
      unifyRaise cond.loc (C.bool ~loc) cond.t ;
      unifyRaise else_.loc then_.t else_.t ;
      (env, {e= EIf {cond; then_; else_}; t; loc})
  (* we need to add a special case for int() in order to support conversion of enumerations *)
  | {e= SECall {instance= None; path= {id= "int"; n= None; _} as path; args= [arg] as args}; loc}
    when not context.in_constant -> (
      let env, arg = exp ~context env arg in
      match arg with
      | {e= EInt n; loc; _} ->
          (env, {e= EInt n; t= Typed.C.int ~loc; loc})
      | {e= EId _; loc; t= {tx= TEId tpath; _}} -> (
        match Env.lookType env tpath loc with
        | {descr= Enum _; _} ->
            (env, {e= ECall {instance= None; path; args= [arg]}; t= Typed.C.int ~loc; loc})
        | _ ->
            call env None path args loc e.loc )
      | _ ->
          call env None path args loc e.loc )
  | {e= SENamed ({e= SEIndex {e= {e= SEId instance; _}; index}; _}, {e= SECall {instance= None; path; args}; loc}); _}
    when not in_constant_context ->
      call env (Some (instance, Some index)) path args loc e.loc
  | {e= SENamed ({e= SEId instance; _}, {e= SECall {instance= None; path; args}; loc}); _} when not in_constant_context
    ->
      call env (Some (instance, None)) path args loc e.loc
  | {e= SENamed (_e1, _e2); _} when in_constant_context ->
      failwith "top_exp: Inference SENamed"
  | {e= SENamed (e1, ({e= SECall _; _} as e2)); _} ->
      let e1 = Pla.print (Syntax.Print.exp e1) in
      let e2 = Pla.print (Syntax.Print.exp e2) in
      failwith ("Inference SENamed: " ^ e1 ^ " : " ^ e2)
  | {e= SENamed (_, _); loc} ->
      (* Handle case where second part is not a function call *)
      Error.raiseError "Invalid instance call syntax. After ':' you must have a function call (e.g., 'name:foo()')" loc
  | {e= SECall {instance; path; args}; loc} when in_constant_context ->
      (* Check if the function has memory declarations *)
      let f = Env.lookFunctionCall env path loc in
      let function_has_mem = Env.isFunctionActive f in
      if function_has_mem then
        Error.raiseError "Functions with memory variables cannot be called in constant expressions" loc
      else call env instance path args loc e.loc
  | {e= SECall {instance; path; args}; loc} ->
      call env instance path args loc e.loc
  | {e= SEOp (op, e1, e2); loc} ->
      let env, e1 = exp ~context env e1 in
      let env, e2 = exp ~context env e2 in
      let f = if context.in_constant then Env.lookOperatorInModule env op else Env.lookOperator env op in
      let args_t, ret = f.t in
      let t = applyFunction e.loc args_t ret [e1; e2] in
      (env, {e= EOp (op, e1, e2); t; loc})
  | {e= SEUnOp (op, e); loc} ->
      let env, e = exp ~context env e in
      let f =
        if context.in_constant then Env.lookOperatorInModule env ("u" ^ op) else Env.lookOperator env ("u" ^ op)
      in
      let args_t, ret = f.t in
      let t = applyFunction e.loc args_t ret [e] in
      (env, {e= EUnOp (op, e); t; loc})
  | {e= SEMember (e1, m); loc} -> (
    (* First, try to interpret this as an enum reference if e1 is an SEId or SEEnum *)
    match e1 with
    | {e= SEId module_name; _} when String.equal (String.capitalize_ascii module_name) module_name -> (
        (* First check if this is a module name - try module-qualified access *)
        let const_path = Syntax.{id= m; n= Some module_name; loc} in
        let results = Env.lookupPath env const_path in
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
              | Some (type_path, tloc, index) ->
                  let t = C.path_t tloc type_path in
                  (env, {e= EInt index; t; loc})
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
        match (unlink e1.t).tx with
        | TEId path -> (
          match Env.lookType env path loc with
          | {path; descr= Record members; _} -> (
            match Map.find m members with
            | None ->
                Error.raiseError ("The field '" ^ m ^ "' is not part of the type '" ^ pathString path ^ "'") loc
            | Some {t; _} ->
                let t = refreshConstness t in
                (* if the type is a builtin (a value) do not unify the constness *)
                let () = if (not in_constant_context) && not (Env.isBuiltinType t) then unifyConstness t e1.t in
                (env, {e= EMember (e1, m); t; loc}) )
          | _ ->
              let t = Pla.print (Typed.print_type_ e1.t) in
              let e = Pla.print (Typed.print_exp e1) in
              Error.raiseError
                ("The expression '" ^ e ^ "' of type '" ^ t ^ "' does not have a member '" ^ m ^ "'.")
                loc )
        | _ ->
            let t = Pla.print (Typed.print_type_ e1.t) in
            let e = Pla.print (Typed.print_exp e1) in
            Error.raiseError ("The expression '" ^ e ^ "' of type '" ^ t ^ "' does not have a member '" ^ m ^ "'.") loc
        ) )
  | {e= SEId id; loc} -> (
      (* This case handles uppercase identifiers (enum constructors) *)
      let id_path : path = {id; n= None; loc} in
      match Env.lookupExpressionSymbol env id_path context with
      | ExprEnum (type_path, tloc, index) ->
          let t = C.path_t tloc type_path in
          (env, {e= EInt index; t; loc})
      | ExprNotFound ->
          Error.raiseError ("Undefined symbol '" ^ id ^ "'. Check spelling or ensure it's declared before use") loc
      | _ ->
          Error.raiseError
            ("Symbol '" ^ id ^ "' is not an enumeration value. Use enumeration constructors like 'MyEnum.Value'")
            loc )
  | {e= SERecord {path; elems}; loc} -> (
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
                    unifyRaise v.loc var.t v.t ;
                    (env, (id, v) :: acc) )
              (env, []) elems
          in
          let elems = CCList.sort (fun (id1, _) (id2, _) -> String.compare id1 id2) elems_rev in
          (env, {e= ERecord {path= t.path; elems}; t= Typed.C.path_t loc t.path; loc})
      | _ ->
          Error.raiseError ("The path '" ^ pathString path ^ "' is not a type.") loc )
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
      let t = C.unbound loc in
      (env, {e= ETypeIntrinsic {intrinsic; type_param}; t; loc})

and exp_list ?(context = normal_context) ?(in_constant_context = false) (env : env) (l : Syntax.exp list) :
    env * exp list =
  (* Convert legacy in_constant_context parameter to new context *)
  let context = if in_constant_context then {context with in_constant= true} else context in
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
      let t = C.noreturn loc in
      (env, {l= LWild; t; loc})
  | {l= SLId name; loc} ->
      let var = Env.lookVar env name loc in
      let t = var.t in
      if not const then setTypeMut t ;
      let e =
        match var.kind with
        | Val ->
            {l= LId name; t; loc}
        | Mem _ | Inst ->
            let ctx = Env.getContext env in
            let ctx_t = C.path_t loc ctx in
            {l= LMember ({l= LId context_name; t= ctx_t; loc}, name); t; loc}
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
      let t = C.tuple ~loc t_elems in
      (env, {l= LTuple elems; t; loc})
  | {l= SLIndex {e; index}; loc} ->
      let env, e = lexp ~const env e in
      let env, index = exp env index in
      let t = C.unbound loc in
      unifyRaise index.loc (C.int ~loc) index.t ;
      unifyRaise e.loc (C.array ~fixed:false ~loc t) e.t ;
      (env, {l= LIndex {e; index}; t; loc})
  | {l= SLMember (e, m); loc} -> (
      let env, e = lexp ~const env e in
      match (unlink e.t).tx with
      | TEId path -> (
        match Env.lookType env path loc with
        | {path; descr= Record members; _} -> (
          match Map.find m members with
          | None ->
              Error.raiseError ("The field '" ^ m ^ "' is not part of the type '" ^ pathString path ^ "'") loc
          | Some {t; _} ->
              let t = refreshConstness t in
              (* if the type is a builtin (a value) do not unify the constness *)
              let t = if not (Env.isBuiltinType t) then {t with const= e.t.const} else t in
              (env, {l= LMember (e, m); t; loc}) )
        | _ ->
            let t = Pla.print (Typed.print_type_ e.t) in
            let e = Pla.print (Typed.print_lexp e) in
            Error.raiseError ("The expression '" ^ e ^ "' of type '" ^ t ^ "' does not have a member '" ^ m ^ "'.") loc
        )
      | _ ->
          let t = Pla.print (Typed.print_type_ e.t) in
          let e = Pla.print (Typed.print_lexp e) in
          Error.raiseError ("The expression '" ^ e ^ "' of type '" ^ t ^ "' does not have a member '" ^ m ^ "'.") loc )

and dexp (env : env) (e : Syntax.dexp) (kind : var_kind) : env * dexp =
  match e with
  | {d= SDWild; loc} ->
      let t = C.noreturn loc in
      (env, {d= DWild; t; loc})
  | {d= SDTuple l; loc} ->
      let env, l =
        CCList.fold_left
          (fun (env, acc) e ->
            let env, e = dexp env e kind in
            (env, e :: acc) )
          (env, []) (CCList.rev l)
      in
      let t = C.tuple ~loc (CCList.map (fun (e : dexp) -> e.t) l) in
      (env, {d= DTuple l; t; loc})
  | {d= SDGroup e; _} ->
      dexp env e kind
  | {d= SDTyped (e, t); _} ->
      let env, e = dexp env e kind in
      let t = type_in_f env t in
      checkArrayDimensions t ; unifyRaise ~bind:true e.loc t e.t ; (env, e)
  | {d= SDId (name, dims); loc} ->
      let t =
        match dims with Some size -> C.array ~loc ~size:(C.size ~loc size) (C.unbound loc) | None -> C.unbound loc
      in
      let env = Env.addVar env unify name t kind loc in
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

let makeIterWhile (env : env) name id_loc value body loc =
  let tick = Env.getFunctionTick env in
  let itname = name ^ "__" ^ string_of_int tick in
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
  {s= SStmtBlock [decl; while_s]; loc}

let makeIfOfMatch env e cases =
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
        match Env.lookupExpressionSymbol env id_path normal_context with
        | ExprEnum (_, _, _) ->
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
        match Env.lookupExpressionSymbol env id_path normal_context with
        | ExprEnum (_, _, _) ->
            makeEq e Syntax.{e= SEMember (Syntax.{e= SEId module_name; loc}, variant_name); loc}
        | _ ->
            Error.raiseError ("Pattern '" ^ module_name ^ "." ^ variant_name ^ "' is not a valid enum constructor") loc
        )
    | _, {p= SPMember _; loc} ->
        Error.raiseError "Invalid qualified pattern. Only Module.Variant patterns are supported" loc
  in
  let if_stmt =
    CCList.fold_right
      (fun (p, case) else_ ->
        let cond = makeComparison e p in
        Some Syntax.{s= SStmtIf (cond, case, else_); loc= cond.loc} )
      cases None
  in
  match if_stmt with None -> failwith "makeIfOfMatch" | Some stmt -> stmt

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
        let env, l =
          CCList.fold_left_map
            (fun env e ->
              let env, e = dexp_with_substitution env e kind in
              (env, e) )
            env (CCList.rev l)
        in
        let t = C.tuple ~loc (CCList.map (fun (e : dexp) -> e.t) l) in
        (env, {d= DTuple l; t; loc})
    | {Syntax.d= Syntax.SDGroup e; _} ->
        dexp_with_substitution env e kind
    | {Syntax.d= Syntax.SDTyped (e, t); _} ->
        let env, e = dexp_with_substitution env e kind in
        let t = type_in_m_with_substitution env type_substitution_map t in
        checkArrayDimensions t ; unifyRaise ~bind:true e.loc t e.t ; (env, e)
    | {Syntax.d= Syntax.SDId (name, dims); loc} ->
        let t =
          match dims with Some size -> C.array ~loc ~size:(C.size ~loc size) (C.unbound loc) | None -> C.unbound loc
        in
        let env = Env.addVar env unify name t kind loc in
        (env, {d= DId (name, dims); t; loc})
    | {Syntax.d= Syntax.SDWild; loc} ->
        let t = C.noreturn loc in
        (env, {d= DWild; t; loc})
  in
  (* Use the same stmt function but with our type-substituting dexp *)
  let env, stmts = stmt_generic env dexp_with_substitution return s in
  (* Resolve any type intrinsics in the resulting statements *)
  let stmts = CCList.map (resolveTypeIntrinsicsInStmt type_substitution_map) stmts in
  (env, stmts)

and type_in_m_with_substitution (env : env) (type_substitution_map : (string * type_) list) (t : Syntax.type_) =
  match t with
  | {t= STUnbound; loc} ->
      {tx= TEUnbound None; loc; const= C.const ()}
  | {t= STGenericType id; loc} -> (
    (* Generic type parameter - substitute with concrete type *)
    try
      let concrete_type = CCList.assoc ~eq:String.equal id type_substitution_map in
      concrete_type
    with Not_found ->
      Error.raiseError (Printf.sprintf "Generic type parameter '%s' has no concrete type binding" id) loc )
  | {t= STId path; loc} -> (
    match path with
    | {id; n= None; _} -> (
      (* Check if this is a generic type parameter that should be substituted *)
      try
        let concrete_type = CCList.assoc ~eq:String.equal id type_substitution_map in
        concrete_type
      with Not_found ->
        (* Regular type lookup *)
        let found = Env.lookType env path loc in
        {tx= TEId found.path; loc; const= C.const ()} )
    | _ ->
        (* Module qualified path - regular lookup *)
        let found = Env.lookType env path loc in
        {tx= TEId found.path; loc; const= C.const ()} )
  | {t= STSize n; loc} ->
      let () =
        if n = 0 then
          let msg = "Empty arrays are not supported" in
          Error.raiseError msg loc
      in
      {tx= TESize n; loc; const= C.const ()}
  | {t= STComposed (name, l); loc} ->
      let l = CCList.map (type_in_m_with_substitution env type_substitution_map) l in
      {tx= TEComposed (name, l); loc; const= C.const ()}

(* Generic-aware version of stmt for processing generic function bodies *)
and stmt_with_generics (env : env) (generic_params : string list) (return : type_) (s : Syntax.stmt) : env * stmt list =
  (* Use a modified dexp function that uses generic-aware type resolution *)
  let rec dexp_generic env dexp kind =
    match dexp with
    | {Syntax.d= Syntax.SDTuple l; loc} ->
        let env, l =
          CCList.fold_left_map
            (fun env e ->
              let env, e = dexp_generic env e kind in
              (env, e) )
            env (CCList.rev l)
        in
        let t = C.tuple ~loc (CCList.map (fun (e : dexp) -> e.t) l) in
        (env, {d= DTuple l; t; loc})
    | {Syntax.d= Syntax.SDGroup e; _} ->
        dexp_generic env e kind
    | {Syntax.d= Syntax.SDTyped (e, t); _} ->
        let env, e = dexp_generic env e kind in
        let t = type_in_m_with_generics env generic_params t in
        checkArrayDimensions t ; unifyRaise ~bind:true e.loc t e.t ; (env, e)
    | {Syntax.d= Syntax.SDId (name, dims); loc} ->
        let t =
          match dims with Some size -> C.array ~loc ~size:(C.size ~loc size) (C.unbound loc) | None -> C.unbound loc
        in
        let env = Env.addVar env unify name t kind loc in
        (env, {d= DId (name, dims); t; loc})
    | {Syntax.d= Syntax.SDWild; loc} ->
        let t = C.noreturn loc in
        (env, {d= DWild; t; loc})
  in
  (* Use the same stmt function but with our generic-aware dexp *)
  stmt_generic env dexp_generic return s

and stmt_generic (env : env) (dexp_func : env -> Syntax.dexp -> var_kind -> env * dexp) (return : type_)
    (s : Syntax.stmt) : env * stmt list =
  match s with
  | {s= SStmtError; _} ->
      (env, [])
  | {s= SStmtBlock stmts; loc} ->
      let env = Env.pushScope env in
      let env, stmts = stmt_list_generic env dexp_func return stmts in
      let env = Env.popScope env in
      (env, [{s= StmtBlock stmts; loc}])
  | {s= SStmtVal (lhs, None); loc} ->
      let env, lhs = dexp_func env lhs Val in
      (env, [{s= StmtVal lhs; loc}])
  | {s= SStmtVal (lhs, Some rhs); loc} ->
      let env, dlhs = dexp_func env lhs Val in
      let env, lhs = lexp env (dexp_to_lexp lhs) in
      let env, rhs = exp env rhs in
      unifyRaise ~bind:true lhs.loc dlhs.t lhs.t ;
      unifyRaise ~bind:true rhs.loc dlhs.t rhs.t ;
      (env, [{s= StmtVal dlhs; loc}; {s= StmtBind (lhs, rhs); loc}])
  | {s= SStmtMem (lhs, None, tags); loc} ->
      let env, lhs = dexp_func env lhs (Mem tags) in
      (env, [{s= StmtMem (lhs, tags); loc}])
  | {s= SStmtMem (lhs, Some rhs, tags); loc} ->
      let env, dlhs = dexp_func env lhs (Mem tags) in
      let env, lhs = lexp env (dexp_to_lexp lhs) in
      let env, rhs = exp env rhs in
      unifyRaise ~bind:true rhs.loc lhs.t rhs.t ;
      (env, [{s= StmtMem (dlhs, tags); loc}; {s= StmtBind (lhs, rhs); loc}])
  | _ ->
      (* For other statements, use the regular dexp instead of the generic one *)
      let rec normal_stmt (env : env) (return : type_) (s : Syntax.stmt) : env * stmt list =
        match s with
        | {s= SStmtError; _} ->
            (env, [])
        | {s= SStmtBlock stmts; loc} ->
            let env = Env.pushScope env in
            let env, stmts =
              CCList.fold_left_map (fun env s -> normal_stmt env return s) env stmts
              |> fun (env, nested_lists) -> (env, CCList.flatten nested_lists)
            in
            let env = Env.popScope env in
            (env, [{s= StmtBlock stmts; loc}])
        | {s= SStmtReturn e; loc} ->
            let env, e = exp env e in
            unifyRaise e.loc return e.t ;
            (env, [{s= StmtReturn e; loc}])
        | {s= SStmtBind (lhs, rhs); loc} ->
            let env, lhs = lexp env lhs in
            let env, rhs = exp env rhs in
            unifyRaise ~bind:true rhs.loc lhs.t rhs.t ;
            (env, [{s= StmtBind (lhs, rhs); loc}])
        | {s= SStmtIf (cond, then_, else_); loc} ->
            let env, cond = exp env cond in
            let env, then_stmts = normal_stmt env return then_ in
            let env, else_stmts =
              match else_ with
              | None ->
                  (env, [])
              | Some else_stmt ->
                  let env, else_stmts = normal_stmt env return else_stmt in
                  (env, else_stmts)
            in
            let then_stmt = stmt_block then_stmts in
            let else_stmt_opt = match else_stmts with [] -> None | _ -> Some (stmt_block else_stmts) in
            (env, [{s= StmtIf (cond, then_stmt, else_stmt_opt); loc}])
        | {s= SStmtWhile (cond, body); loc} ->
            let env, cond = exp env cond in
            let env, body_stmts = normal_stmt env return body in
            let body = stmt_block body_stmts in
            (env, [{s= StmtWhile (cond, body); loc}])
        | _ ->
            failwith "Unhandled statement type in generic processing"
      in
      normal_stmt env return s

and stmt_list_generic (env : env) (dexp_func : env -> Syntax.dexp -> var_kind -> env * dexp) (return : type_)
    (stmts : Syntax.stmt list) : env * stmt list =
  CCList.fold_left_map (fun env s -> stmt_generic env dexp_func return s) env stmts
  |> fun (env, nested_lists) -> (env, CCList.flatten nested_lists)

let rec stmt (env : env) (return : type_) (s : Syntax.stmt) : env * stmt list =
  match s with
  | {s= SStmtError; _} ->
      (env, [])
  | {s= SStmtBlock stmts; loc} ->
      let env = Env.pushScope env in
      let env, stmts = stmt_list env return stmts in
      let env = Env.popScope env in
      (env, [{s= StmtBlock stmts; loc}])
  | {s= SStmtVal (lhs, None); loc} ->
      let env, lhs = dexp env lhs Val in
      (env, [{s= StmtVal lhs; loc}])
  | {s= SStmtVal (lhs, Some rhs); loc} ->
      let env, dlhs = dexp env lhs Val in
      let env, lhs = lexp ~const:true env (dexp_to_lexp lhs) in
      let env, rhs = exp env rhs in
      unifyRaise ~bind:true lhs.loc dlhs.t lhs.t ;
      unifyRaise ~bind:true rhs.loc dlhs.t rhs.t ;
      (env, [{s= StmtVal dlhs; loc}; {s= StmtBind (lhs, rhs); loc}])
  | {s= SStmtMem (lhs, None, tags); loc} ->
      let env, lhs = dexp env lhs (Mem tags) in
      (env, [{s= StmtMem (lhs, tags); loc}])
  | {s= SStmtMem (lhs, Some rhs, tags); loc} ->
      let env, dlhs = dexp env lhs (Mem tags) in
      let env, lhs = lexp env (dexp_to_lexp lhs) in
      let env, rhs = exp env rhs in
      unifyRaise ~bind:true rhs.loc lhs.t rhs.t ;
      (env, [{s= StmtMem (dlhs, tags); loc}; {s= StmtBind (lhs, rhs); loc}])
  | {s= SStmtBind (lhs, rhs); loc} ->
      let env, lhs = lexp env lhs in
      let env, rhs = exp env rhs in
      unifyRaise ~bind:true rhs.loc lhs.t rhs.t ;
      (env, [{s= StmtBind (lhs, rhs); loc}])
  | {s= SStmtReturn e; loc} ->
      let env, e = exp env e in
      unifyRaise e.loc return e.t ;
      (env, [{s= StmtReturn e; loc}])
  | {s= SStmtIf (cond, then_, else_); loc} ->
      let env, cond = exp env cond in
      unifyRaise cond.loc (C.bool ~loc) cond.t ;
      let env, then_ = stmt env return then_ in
      let env, else_ = stmt_opt env return else_ in
      (env, [{s= StmtIf (cond, stmt_block then_, else_); loc}])
  | {s= SStmtWhile (cond, s); loc} ->
      let env, cond = exp env cond in
      unifyRaise cond.loc (C.bool ~loc) cond.t ;
      let env, s = stmt env return s in
      (env, [{s= StmtWhile (cond, stmt_block s); loc}])
  | {s= SStmtIter {id= name, id_loc; value; body}; loc} ->
      let while_s = makeIterWhile env name id_loc value body loc in
      stmt env return while_s
  | {s= SStmtMatch {e; cases}; _} ->
      let if_stmt = makeIfOfMatch env e cases in
      stmt env return if_stmt

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

let getOptType env loc (t : Syntax.type_ option) = match t with None -> C.unbound loc | Some t -> type_in_c env t

let getReturnType env (t : Syntax.type_ option) = match t with None -> None | Some t -> Some (type_in_c env t)

let convertArguments env (args : Syntax.arg list) : arg list =
  CCList.map (fun (name, t, loc) -> {name; t= getOptType env loc t; loc}) args

(* Convert arguments using a pre-created generic type mapping *)
let convertArgumentsWithGenericMapping env (generic_type_map : (string * type_) list) (args : Syntax.arg list) :
    arg list =
  let getOptTypeWithMapping env loc (t : Syntax.type_ option) =
    match t with None -> C.unbound loc | Some t -> type_in_m_with_generic_mapping env generic_type_map t
  in
  CCList.map (fun (name, t, loc) -> {name; t= getOptTypeWithMapping env loc t; loc}) args

(* Legacy wrapper that creates a fresh mapping - kept for backwards compatibility *)
let convertArgumentsWithGenerics env (generic_params : string list) (args : Syntax.arg list) : arg list =
  let loc = match args with (_, _, loc) :: _ -> loc | [] -> Loc.default in
  let generic_type_map = createGenericTypeMapping generic_params loc in
  convertArgumentsWithGenericMapping env generic_type_map args

let registerMultiReturnMem (env : env) name t loc =
  let _, ret = t in
  match unlink ret with
  | {tx= TEComposed ("tuple", elems); _} ->
      let names = CCList.mapi (fun i t -> (pathString name ^ "_ret_" ^ string_of_int i, t)) elems in
      CCList.fold_left (fun env (name, t) -> Env.addReturnVar env name t loc) env names
  | _ ->
      env

let isRoot (args : Args.args) path =
  let s_path = Pla.print (Syntax.print_path path) in
  CCList.mem s_path args.roots

let customInitializer (env : env) tags name = if Ptags.has tags "init" then Env.addCustomInitFunction env name else env

let reportReturnTypeMismatch is_placeholder loc (specified_ret : type_ option) (inferred_ret : type_) =
  match (specified_ret, inferred_ret) with
  | None, {tx= Typed.TENoReturn; _} ->
      unifyRaise loc (C.noreturn loc) inferred_ret
  | None, _ ->
      ()
  | Some t, {tx= Typed.TENoReturn; _} ->
      (* If the function is a placeholder it will not have body, then the inferred type will be unbound.
       In this case we need to unify the specified and the inferred. *)
      if is_placeholder then unifyRaise loc t inferred_ret
      else
        let t = Pla.print (print_type_ t) in
        Error.raiseError ("This function is expected to have type '" ^ t ^ "' but nothing was returned.") loc
  | Some t1, t2 ->
      unifyRaise loc t1 t2

let rec function_def (iargs : Args.args) (env : env) (def : Syntax.function_def) : env * (function_def * stmt) =
  let specified_ret = getReturnType env def.t in
  let inferred_ret = C.noreturn def.loc in
  let args = convertArguments env def.args in
  let env, path, t = Env.enterFunction env def.name args inferred_ret def.loc in
  let env, body = stmt env inferred_ret def.body in
  let env = Env.exitFunction env in
  let next = addGeneratedFunctions def.tags def.name def.next in
  let env, next = function_def_opt iargs env next in
  let env = registerMultiReturnMem env path t def.loc in
  let env = customInitializer env def.tags path in
  let is_root = isRoot iargs path in
  let is_placeholder = Ptags.has def.tags "placeholder" in
  let () = reportReturnTypeMismatch is_placeholder def.loc specified_ret inferred_ret in
  (env, ({name= path; args; t; loc= def.loc; tags= def.tags; next; is_root}, stmt_block body))

and function_def_opt (iargs : Args.args) (env : env) def_opt =
  match def_opt with
  | None ->
      (env, None)
  | Some def ->
      let env = Env.addAliasToContext env def.name def.loc in
      let env, def_body = function_def iargs env def in
      (env, Some def_body)

let applyMutableTag (args : Typed.arg list) (tags : Typed.tag list) =
  match Ptags.getArguments tags "mutable" with
  | None ->
      args
  | Some [] ->
      args
  | Some vars ->
      CCList.map
        (fun (arg : arg) ->
          match CCList.find_opt (fun (n, _, _) -> String.compare n arg.name = 0) vars with
          | Some (_, {g= TagBool mut; _}, _) ->
              setTypeConstness arg.t (not mut) ; arg
          | _ ->
              arg )
        args

let ext_function (iargs : Args.args) (env : env) (def : Syntax.ext_def) : env * function_def =
  let ret = getOptType env def.loc def.t in
  let args = convertArguments env def.args in
  let args = applyMutableTag args def.tags in
  let env, path, t = Env.enterFunction env def.name args ret def.loc in
  let env = Env.exitFunction env in
  let next = addGeneratedFunctions def.tags def.name None in
  let env, next = function_def_opt iargs env next in
  (env, {name= path; args; t; loc= def.loc; tags= def.tags; next; is_root= false})

let getContextArgument (env : env) (path : path) loc : arg option =
  match Env.getCurrentContext env with
  | Some (_, {descr= Record members; _}) ->
      if Map.is_empty members then None
      else
        let ctx_t =
          let m = Env.getCurrentModule env in
          match Map.find path.id m.functions with
          | Some f -> (
            match Env.lookVarInScopes f.locals context_name with
            | Some var ->
                var.t
            | None ->
                failwith "context var not declared" )
          | None ->
              failwith "function not found"
        in
        let () = Env.Map.fold (fun _ (var : var) () -> unifyConstness ctx_t var.t) () members in
        Some {name= context_name; t= ctx_t; loc}
  | _ ->
      None

let insertContextArgument (env : env) (def : function_def) : function_def =
  (* Check if the function already has a _ctx argument *)
  let already_has_ctx = match def.args with {name; _} :: _ when String.equal name context_name -> true | _ -> false in
  if already_has_ctx then def
  else
    match getContextArgument env def.name def.loc with
    | None ->
        def
    | Some arg ->
        let rec loop (next : (function_def * stmt) option) : (function_def * stmt) option =
          match next with
          | Some (def, body) ->
              let next = loop def.next in
              Some ({def with args= arg :: def.args; next}, body)
          | None ->
              None
        in
        let next = loop def.next in
        {def with args= arg :: def.args; next}

let top_dexp (env : env) (d : Syntax.dexp) =
  match d with
  | {d= SDId (name, dims); loc} ->
      let t =
        match dims with Some size -> C.array ~loc ~size:(C.size ~loc size) (C.unbound loc) | None -> C.unbound loc
      in
      (*let env = Env.addVar env unify name t kind loc in*)
      (env, {d= DId (name, dims); t; loc})
  | _ ->
      failwith "invalid constant"

let convert_generic_param (env : env) (param : Syntax.generic_param) : Typed.generic_param =
  match param with
  | Syntax.GParamFunction (name, type_opt) ->
      (* Validate function parameter name *)
      if String.length name = 0 then Error.raiseError "Generic function parameter name cannot be empty" Loc.default ;
      (* For function templates, we'll determine the actual type during instantiation *)
      (* The type_opt can be used as a constraint later *)
      let converted_type =
        Option.map
          (fun t ->
            let t' = type_in_m env t in
            (* Validate that the constraint type is actually a function type *)
            ( match (unlink t').tx with
            | TEFunction (_, _) ->
                ()
            | _ ->
                Error.raiseError
                  (Printf.sprintf "Generic function parameter '%s' type constraint must be a function type, got %s" name
                     (Pla.print (Typed.print_type_ t')) )
                  Loc.default ) ;
            t' (* Store the constraint type, but actual function type determined at instantiation *) )
          type_opt
      in
      Typed.GParamFunction (name, converted_type)
  | Syntax.GParamType name ->
      (* Validate type parameter name *)
      if String.length name = 0 then Error.raiseError "Template type parameter name cannot be empty" Loc.default ;
      Typed.GParamType name
  | Syntax.GParamConstant (name, type_expr) ->
      (* Validate constant parameter name *)
      if String.length name = 0 then Error.raiseError "Generic constant parameter name cannot be empty" Loc.default ;
      let converted_type = type_in_m env type_expr in
      (* Validate that the type is a valid constant type (allow unbound for inference) *)
      ( match (unlink converted_type).tx with
      | TEId {id= "int" | "real" | "bool" | "string"; _} ->
          ()
      | TEUnbound _ ->
          () (* Allow unbound types - will be inferred from call site *)
      | _ ->
          Error.raiseError
            (Printf.sprintf "Generic constant parameter '%s' must have type int, real, bool, or string, got %s" name
               (Pla.print (Typed.print_type_ converted_type)) )
            Loc.default ) ;
      Typed.GParamConstant (name, converted_type)

let create_generic_function (env : env) (def : Syntax.function_def) : Typed.generic_function =
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
        | Syntax.GParamConstant (name, _) ->
            name )
      def.generic_params
  in
  let unique_names = CCList.sort_uniq ~cmp:String.compare param_names in
  if CCList.length unique_names <> CCList.length param_names then
    Error.raiseError (Printf.sprintf "Generic function '%s' has duplicate generic parameter names" def.name) def.loc ;
  (* Note: Type parameter names like 'a can have the same base name as function arguments
     like 'a' since they are in separate namespaces (type vs value). *)
  let generic_params = CCList.map (convert_generic_param env) def.generic_params in
  (* Extract just the type parameter names for context *)
  let type_param_names =
    CCList.filter_map (function Syntax.GParamType name -> Some name | _ -> None) def.generic_params
  in
  (* Create a shared mapping from type parameter names to unbound types *)
  (* This ensures all occurrences of the same parameter use the same unbound type *)
  let generic_type_map = createGenericTypeMapping type_param_names def.loc in
  let args = convertArgumentsWithGenericMapping env generic_type_map def.args in
  (* Process return type with the same generic type mapping *)
  let inferred_ret =
    match def.t with
    | Some ret_type ->
        type_in_m_with_generic_mapping env generic_type_map ret_type
    | None ->
        C.noreturn def.loc
  in
  (* Create function type from regular arguments only (exclude template params) *)
  let arg_types = CCList.map (fun (arg : Typed.arg) -> arg.t) args in
  (* Convert param_order from Syntax to Typed *)
  let param_order =
    CCList.map (function Syntax.PKGeneric i -> Typed.PKGeneric i | Syntax.PKArg i -> Typed.PKArg i) def.param_order
  in
  (* Capture the type index at definition time - this ensures specialized types appear near the generic's position *)
  let type_index = Env.getGlobalTick () in
  { name= def.name
  ; generic_params
  ; args
  ; param_order
  ; t= (arg_types, inferred_ret)
  ; body= def.body
  ; next= def.next
  ; loc= def.loc
  ; tags= def.tags
  ; type_index }

let has_generic_params (def : Syntax.function_def) : bool = not (CCList.is_empty def.generic_params)

let rec top_stmt (iargs : Args.args) (env : env) (s : Syntax.top_stmt) : env * top_stmt list =
  match s with
  | {top= STopError; _} ->
      failwith "Parser error"
  | {top= STopFunction def; loc} when has_generic_params def ->
      (* Store generic function and emit placeholder to mark where specializations go *)
      let generic_func = create_generic_function env def in
      let env = Env.addGeneric env generic_func in
      (env, [{top= TopGenericPlaceholder def.name; loc}])
  | {top= STopFunction def; _} ->
      let env = Env.createContextForFunction env def.name def.loc in
      let env, (def, body) = function_def iargs env def in
      let def = insertContextArgument env def in
      let env = Env.exitContext env in
      (* Generic function instantiation is now handled in toprog.ml when processing EGenCall *)
      (env, [{top= TopFunction (def, body); loc= def.loc}])
  | {top= STopExternal (def, link_name); _} ->
      let env = Env.createContextForExternal env in
      let env, def = ext_function iargs env def in
      let env = Env.exitContext env in
      (env, [{top= TopExternal (def, link_name); loc= def.loc}])
  | {top= STopType {name; members}; loc} ->
      let members = CCList.map (fun (name, t, tags, loc) -> (name, type_in_m env t, tags, loc)) members in
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
  | {top= STopConstant (({d= SDId (name, dim); _} as d), e); loc} ->
      let env, d = top_dexp env d in
      let env, e = exp ~context:constant_context env e in
      unifyRaise e.loc d.t e.t ;
      let m = Env.getCurrentModule env in
      let path = Env.getPath m name loc in
      let env = Env.addConstant env unify name d.t loc in
      (env, [{top= TopConstant (path, dim, d.t, e, None); loc}])
  | {top= STopConstant _; _} ->
      failwith ""

and top_stmt_list (iargs : Args.args) (env : env) (s : Syntax.top_stmt list) : env * top_stmt list =
  let env, rev_s =
    CCList.fold_left
      (fun (env, acc) s ->
        let env, stmt_list = top_stmt iargs env s in
        (env, stmt_list @ acc) )
      (env, []) s
  in
  (env, rev_s)

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

let typecheck_single (iargs : Args.args) (env : env) (h : Parse.parsed_file) : env * top_stmt list =
  let set = createExistingTypeSet (createTypes env) in
  let env = Env.enterModule env h.name in
  let env, stmt = top_stmt_list iargs env h.stmts in
  let env = Env.exitModule env in
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
  (env, CCList.rev module_stmts)

(* Type checking with elaboration - for backwards compatibility.
   This is equivalent to calling typecheck followed by Elaboration.elaborate. *)
let typecheck_and_elaborate (iargs : Args.args) (parsed : Parse.parsed_file list) : env * top_stmt list =
  let env, module_stmts = typecheck iargs parsed in
  let elaborated = Elaboration.elaborate iargs env module_stmts in
  let types = createTypes env in
  (env, types @ elaborated)

(* Initialize the Elaboration module's refs to use our functions.
   This is done at module load time. *)
let () =
  Elaboration.unify_ref := unify ;
  Elaboration.unifyRaise_ref := unifyRaise ;
  Elaboration.function_def_opt_ref := function_def_opt ;
  Elaboration.stmt_with_type_substitution_ref := stmt_with_type_substitution ;
  Elaboration.insertContextArgument_ref := insertContextArgument ;
  Elaboration.markExpMutable_ref := markExpMutable ;
  Elaboration.propagateVariability_ref := propagateVariability
