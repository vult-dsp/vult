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
let normal_context = { Env.in_constant = false; in_generic_arg = false }

let constant_context = { Env.in_constant = true; in_generic_arg = false }

let generic_arg_context = { Env.in_constant = false; in_generic_arg = true }

(* Check if a syntax statement contains mem declarations *)
let rec syntax_has_mem (stmt : Syntax.stmt) : bool =
  match stmt.s with
  | SStmtMem _ -> true
  | SStmtBlock stmts -> CCList.exists syntax_has_mem stmts
  | SStmtIf (_, then_stmt, else_opt) ->
    syntax_has_mem then_stmt || Option.fold ~none:false ~some:syntax_has_mem else_opt
  | SStmtWhile (_, body) -> syntax_has_mem body
  | _ -> false


let pickLoc (t1 : type_) (t2 : type_) : unit =
  if t1.loc == Loc.default then
    t1.loc <- t2.loc
  else if t2.loc == Loc.default then
    t2.loc <- t1.loc


let linkType ~from ~into =
  into.tx <- TELink from;
  pickLoc from into;
  true


let rec unlink (t : type_) =
  match t.tx with
  | TELink t -> unlink t
  | _ -> t


let path_string (p : Syntax.path) : string =
  match p with
  | { id; n = None; _ } -> id
  | { id; n = Some n; _ } -> n ^ "_" ^ id


(* Tries to unity the given type with all the elements of the set, if they can be unified we increase the counter *)
let rec pushTypeToSet (set : (type_ * int) list) (elem : type_) =
  match set, elem with
  | [], _ -> [ elem, 1 ]
  | (({ tx = TEComposed (n1, e1); _ } as h), count) :: t, { tx = TEComposed (n2, e2); _ }
    when n2 = n1 && CCList.length e1 = CCList.length e2 ->
    if unify h elem then
      (h, count + 1) :: t
    else
      (h, count) :: pushTypeToSet t elem
  (* Don't try to unify composed types with different arguments *)
  | (({ tx = TEComposed (n1, _); _ } as h), count) :: t, { tx = TEComposed (n2, _); _ } when n2 = n1 ->
    (h, count) :: pushTypeToSet t elem
  | (h, count) :: t, _ ->
    if unify h elem then
      (h, count + 1) :: t
    else
      (h, count) :: pushTypeToSet t elem


and constrainOption loc l1 l2 =
  let set = CCList.fold_left pushTypeToSet (CCList.map (fun e -> e, 1) l1) l2 in
  let final_set =
    CCList.filter_map
      (fun (e, n) ->
        if n > 1 then
          Some e
        else
          None)
      set
  in
  match final_set with
  | [] ->
    let t1 = Pla.map_sep Pla.commaspace Typed.print_type_ l1 in
    let t2 = Pla.map_sep Pla.commaspace Typed.print_type_ l2 in
    let msg = Pla.print {%pla|None of the following types: <#t1#>, matches with any of the following types <#t2#>. |} in
    Error.raiseError msg loc
  | [ t ] -> t
  | l -> { tx = TEOption l; loc = Loc.default; const = C.const () }


and pickOption original l tt =
  let rec loop l =
    match l with
    | [] -> false
    | h :: t ->
      if unify h tt then
        linkType ~from:tt ~into:original
      else
        loop t
  in
  loop l


and unifyConstnessValue (t1 : constness) (t2 : constness) =
  if t1 == t2 then
    ()
  else
    match t1.c, t2.c with
    | TECLink tl, _ -> unifyConstnessValue tl t2
    | _, TECLink tl -> unifyConstnessValue t1 tl
    | TEConst _, _ -> t1.c <- TECLink t2
    | _, TEConst _ -> t2.c <- TECLink t1
    | TEMut _, TEMut _ -> ()


and unifyConstness (t1 : type_) (t2 : type_) =
  unifyConstnessValue t1.const t2.const;
  match t1.tx, t2.tx with
  | TELink tlink, _ -> unifyConstness tlink t2
  | _, TELink tlink -> unifyConstness t1 tlink
  | _ -> ()


and unify ?(bind = false) (t1 : type_) (t2 : type_) =
  if t1 == t2 then
    true
  else (
    (* transfer memory use to determine constness *)
    if bind then
      unifyConstnessValue t1.const t2.const;
    match t1.tx, t2.tx with
    | TEId t1, TEId t2 -> Pparser.Syntax.compare_path t1 t2 = 0
    | TESize t1, TESize t2 -> t1 = t2
    | TEFunction (arg1, ret1), TEFunction (arg2, ret2) -> CCList.for_all2 unify arg1 arg2 && unify ret1 ret2
    | TEFunction _, _ -> false
    | _, TEFunction _ -> false
    (* special case for arrays without dimensions *)
    | TEComposed ("array", [ e1; _ ]), TEComposed ("array", [ e2 ])
     |TEComposed ("array", [ e1 ]), TEComposed ("array", [ e2; _ ]) -> unify e1 e2
    | TEComposed (n1, e1), TEComposed (n2, e2) when n1 = n2 && CCList.length e1 = CCList.length e2 ->
      CCList.for_all2 unify e1 e2
    (* follow the links *)
    | TELink tlink, _ -> unify tlink t2
    | _, TELink tlink -> unify t1 tlink
    | TENoReturn, _ -> linkType ~from:t2 ~into:t1
    | _, TENoReturn -> linkType ~from:t1 ~into:t2
    (* replace any unbound *)
    | TEUnbound None, TEUnbound _ -> linkType ~from:t1 ~into:t2
    | TEUnbound _, TEUnbound None -> linkType ~from:t2 ~into:t1
    | TEUnbound _, _ -> linkType ~from:t2 ~into:t1
    | _, TEUnbound _ -> linkType ~from:t1 ~into:t2
    (* types with alternatives *)
    | TEOption l1, TEOption l2 ->
      let t3 = constrainOption t2.loc l1 l2 in
      let _ = linkType ~from:t3 ~into:t2 in
      linkType ~from:t3 ~into:t1
    | TEOption l, _ -> pickOption t1 l t2
    | _, TEOption l -> pickOption t2 l t1
    | TEId _, _ -> false
    | TESize _, _ -> false
    | TEComposed _, _ -> false)


let unifyRaise ?(bind = false) (loc : Loc.t) (t1 : type_) (t2 : type_) : unit =
  (* TODO: improve unify error reporting for tuples *)
  let raise = true in
  if not (unify ~bind t1 t2) then
    let msg =
      let t1 = print_type_ t1 in
      let t2 = print_type_ t2 in
      Pla.print {%pla|This expression has type '<#t2#>' but '<#t1#>' was expected|}
    in
    if raise then
      Error.raiseError msg loc
    else (
      print_endline (Loc.to_string loc);
      print_endline msg)


(* Convert a type with a mapping from generic parameter names to their unbound types *)
let rec type_in_m_with_generic_mapping (env : env) (generic_type_map : (string * type_) list) (t : Syntax.type_) =
  match t with
  | { t = STUnbound; loc } -> { tx = TEUnbound None; loc; const = C.const () }
  | { t = STGenericType id; loc } -> (
    (* Explicit generic type parameter - look up in the mapping *)
    match CCList.assoc_opt ~eq:String.equal id generic_type_map with
    | Some unbound_type -> unbound_type
    | None -> Error.raiseError (Printf.sprintf "Generic type parameter '%s' is not declared in the function" id) loc)
  | { t = STId path; loc } -> (
    match path with
    | { id; n = None; _ } -> (
      (* Check if this is a generic type parameter *)
      match CCList.assoc_opt ~eq:String.equal id generic_type_map with
      | Some unbound_type -> unbound_type
      | None ->
        (* Regular type lookup *)
        let found = Env.lookType env path loc in
        { tx = TEId found.path; loc; const = C.const () })
    | _ ->
      (* Regular type lookup *)
      let found = Env.lookType env path loc in
      { tx = TEId found.path; loc; const = C.const () })
  | { t = STSize n; loc } ->
    let () =
      if n = 0 then
        let msg = "Empty arrays are not supported" in
        Error.raiseError msg loc
    in
    { tx = TESize n; loc; const = C.const () }
  | { t = STComposed (name, l); loc } ->
    let l = CCList.map (type_in_m_with_generic_mapping env generic_type_map) l in
    { tx = TEComposed (name, l); loc; const = C.const () }


(* Helper to create a mapping from generic parameter names to unbound types *)
let createGenericTypeMapping (generic_params : string list) (loc : Loc.t) : (string * type_) list =
  CCList.map (fun name -> name, { tx = TEUnbound None; loc; const = C.const () }) generic_params


(* Legacy wrapper for backwards compatibility - creates fresh mapping each time *)
let type_in_m_with_generics (env : env) (generic_params : string list) (t : Syntax.type_) =
  let generic_type_map = createGenericTypeMapping generic_params t.loc in
  type_in_m_with_generic_mapping env generic_type_map t


let type_in_m (env : env) (t : Syntax.type_) = type_in_m_with_generics env [] t

let rec checkArrayDimensions (t : type_) =
  match t.tx with
  | TEComposed ("array", [ _ ]) ->
    Error.raiseError
      "Array type declaration missing size. Use 'array(type, size)' format (e.g., 'array(real, 10)')"
      t.loc
  | TEComposed ("array", [ _; _ ]) -> ()
  | TELink t -> checkArrayDimensions t
  | _ -> ()


let type_in_c (env : env) (t : Syntax.type_) = type_in_m (Env.exitContext env) t

let type_in_f (env : env) (t : Syntax.type_) = type_in_c (Env.exitFunction env) t

let applyFunction loc (args_t_in : type_ list) (ret : type_) (args_in : exp list) =
  let rec loop (args_t : type_ list) args =
    match args_t, args with
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
    | [], [] -> ret
    | h :: args_t, (ht : exp) :: args ->
      unifyRaise ht.loc h ht.t;
      loop args_t args
  in
  loop args_t_in args_in


let rec markExpMutable env exp loc =
  match exp.e with
  | EId name -> (
    match Env.lookVar env name loc with
    | var -> Typed.setTypeMut var.t
    | exception Error.Errors _ -> ())
  | EMember (e, _) -> markExpMutable env e loc
  | EIndex { e; _ } -> markExpMutable env e loc
  | _ -> ()


let propagateVariability env loc (args : Typed.arg list option) (exp_args : exp list) =
  match args with
  | None -> ()
  | Some args ->
    (* Only propagate if the lists have the same length to avoid iter2 failures.
       Length mismatches can happen with external functions or context arguments. *)
    if CCList.length args = CCList.length exp_args then
      CCList.iter2
        (fun (arg : arg) (exp : exp) ->
          if isTypeConst arg.t = false then
            markExpMutable env exp loc)
        args
        exp_args


(* Convert type to mangled name for specialized function names *)
(* Used by toprog.ml for generating specialized function names *)
let rec type_to_mangled_name (t : Typed.type_) : string =
  match (unlink t).tx with
  | TEId { id; n = None; _ } -> id
  | TEId { id; n = Some module_name; _ } -> module_name ^ "_" ^ id
  | TEFunction (arg_types, ret_type) ->
    let args_str = CCList.map type_to_mangled_name arg_types |> String.concat "_" in
    let ret_str = type_to_mangled_name ret_type in
    "fn_" ^ args_str ^ "_to_" ^ ret_str
  | TEComposed (name, type_args) ->
    let args_str = CCList.map type_to_mangled_name type_args |> String.concat "_" in
    if args_str = "" then
      name
    else
      name ^ "_of_" ^ args_str
  | TEUnbound (Some id) -> "unbound" ^ string_of_int id
  | TEUnbound None -> "unbound"
  | TEOption type_list -> (
    (* For option types, try to find a concrete type that has been constrained *)
    (* This is a simplified approach - in practice, we should use the actual constrained types *)
    match type_list with
    | [ single_type ] -> type_to_mangled_name single_type (* Use the single concrete type *)
    | multiple_types -> (
      (* Try to find a non-option type in the list *)
      let non_option_types =
        CCList.filter
          (fun t ->
            match (unlink t).tx with
            | TEOption _ -> false
            | _ -> true)
          multiple_types
      in
      match non_option_types with
      | concrete_type :: _ -> type_to_mangled_name concrete_type
      | [] -> "opt_" ^ (CCList.map type_to_mangled_name type_list |> String.concat "_")))
  | TENoReturn -> "noreturn"
  | TESize i -> "size_" ^ string_of_int i
  | TELink _ -> "link" (* Should not happen after unlink *)


let rec addContextArg (env : env) instance (f : Env.f) args loc =
  if Env.isFunctionActive f then (
    let cpath = Env.getContext env in
    let fpath = Env.getFunctionContext f in
    (* get the context type of the current function *)
    let ctx_t =
      let f = Env.getCurrentFunction env in
      match Env.lookVarInScopes f.locals context_name with
      | Some var -> var.t
      | None -> failwith "context var not declared"
    in
    (* get the context type of the function we are calling *)
    let fctx_t =
      match Env.lookVarInScopes f.locals context_name with
      | Some var -> var.t
      | None -> failwith "context var not declared"
    in
    let is_ctx_mutable = isTypeConst fctx_t = false in
    match Syntax.compare_path cpath fpath, instance with
    | 0, None ->
      let e = { e = EId context_name; t = fctx_t; loc } in
      let () =
        if is_ctx_mutable then
          markExpMutable env e loc
      in
      env, e :: args
    | 0, Some _ ->
      let msg =
        Pla.print {%pla|This function belongs to the same instance and it must not be called on a different instance.|}
      in
      Error.raiseError msg loc
    (* no instance name provided *)
    | _, None ->
      let number =
        Printf.sprintf
          "%.2x%.2x"
          (0xFF land Hashtbl.hash (path_string fpath))
          (0xFF land Hashtbl.hash (path_string cpath))
      in
      let rec generateName () =
        let n = Env.getFunctionTick env in
        let name = "inst_" ^ string_of_int n ^ number in
        if checkMemExists env name || Env.checkConstantExists env name then
          generateName ()
        else
          name
      in
      let name = generateName () in
      let env = Env.addVar env unify name fctx_t Inst loc in
      let e = { e = EMember ({ e = EId context_name; t = ctx_t; loc }, name); loc; t = fctx_t } in
      let () =
        if is_ctx_mutable then
          markExpMutable env e loc
      in
      env, e :: args
    (* intance without subscripts *)
    | _, Some (name, None) ->
      let env = Env.addVar env unify name fctx_t Inst loc in
      let e = { e = EMember ({ e = EId context_name; t = ctx_t; loc }, name); loc; t = fctx_t } in
      let () =
        if is_ctx_mutable then
          markExpMutable env e loc
      in
      env, e :: args
    (* array of instances *)
    | _, Some (name, Some index) ->
      let env, index = exp ~context:normal_context env index in
      unifyRaise index.loc (C.int ~loc:Loc.default) index.t;
      let t = C.array ~loc fctx_t in
      let env = Env.addVar env unify name t Inst loc in
      let e = { e = EMember ({ e = EId context_name; t = ctx_t; loc }, name); loc; t = fctx_t } in
      let e = { e = EIndex { e; index }; loc; t = fctx_t } in
      let () =
        if is_ctx_mutable then
          markExpMutable env e loc
      in
      env, e :: args)
  else
    env, args


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
      env, { e = ECall { instance = None; path = f.path; args }; t; loc }
    | None -> (
      (* Check if this might be a companion function of a generic *)
      match Env.lookupGenericByCompanion env path with
      | Some parent_generic ->
        (* This is a companion call - create EGenCompanionCall for later processing *)
        let env, processed_args = exp_list env args in
        (* For now, we use noreturn as the return type - it will be resolved during instantiation *)
        let t = C.noreturn loc in
        let parent_path : path = { id = parent_generic.name; n = path.Syntax.n; loc = path.Syntax.loc } in
        (* Extract just the instance name from the Syntax instance type *)
        let instance_name = Option.map fst instance in
        ( env
        , { e =
              EGenCompanionCall
                { instance = instance_name
                ; companion_name = path.Syntax.id
                ; parent_generic_path = parent_path
                ; args = processed_args
                }
          ; t
          ; loc
          } )
      | None ->
        (* Function not found - raise the standard error *)
        let _ = Env.lookFunctionCall env path loc in
        (* lookFunctionCall will raise, so this is unreachable *)
        failwith "Unreachable"))


and generic_call (env : env) (instance : (string * Syntax.exp option) option) (generic_path : Syntax.path)
    (generic_func : Typed.generic_function) (args : Syntax.exp list) (_ : Loc.t) (eloc : Loc.t) : env * exp =
  (* Count only explicit generic parameters (exclude implicit type parameters) *)
  let explicit_generic_param_count =
    CCList.count
      (function
        | Typed.GParamType _ -> false (* Implicit type parameters - inferred from function args *)
        | _ -> true (* Explicit parameters - require explicit arguments *))
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
         generic_func.name
         total_expected
         explicit_generic_param_count
         function_param_count
         total_provided)
      eloc;
  if total_provided > total_expected then
    Error.raiseError
      (Printf.sprintf
         "Generic function '%s' expects %d arguments (%d explicit generic parameters + %d function parameters) but got \
          %d"
         generic_func.name
         total_expected
         explicit_generic_param_count
         function_param_count
         total_provided)
      eloc;
  (* Split arguments using param_order to handle interleaved params *)
  let args_array = Array.of_list args in
  let rec split_args (pos : int) (gen_acc : Syntax.exp list) (arg_acc : Syntax.exp list) (order : Typed.param_kind list)
      : Syntax.exp list * Syntax.exp list =
    match order with
    | [] -> List.rev gen_acc, List.rev arg_acc
    | Typed.PKGeneric _ :: rest -> split_args (pos + 1) (args_array.(pos) :: gen_acc) arg_acc rest
    | Typed.PKArg _ :: rest -> split_args (pos + 1) gen_acc (args_array.(pos) :: arg_acc) rest
  in
  let explicit_generic_args, function_args = split_args 0 [] [] generic_func.param_order in
  (* Process explicit template arguments with template argument context (allows function references) *)
  let env, processed_explicit_generic_args = exp_list ~context:generic_arg_context env explicit_generic_args in
  (* Process regular function arguments with normal context *)
  let env, processed_function_args = exp_list ~context:normal_context env function_args in
  (* Create fresh copies of the generic function's argument types for unification *)
  (* This allows the types to be constrained by unification without polluting the original *)
  let generic_func_arg_types, generic_func_ret_type = generic_func.t in
  let all_orig_types = generic_func_arg_types @ [ generic_func_ret_type ] in
  let all_fresh_types = Typed.copy_types_preserving_sharing all_orig_types in
  let fresh_arg_types, fresh_ret_type =
    match CCList.rev all_fresh_types with
    | last :: rest -> CCList.rev rest, last
    | [] -> failwith "copy_types_preserving_sharing returned empty list"
  in
  (* Unify argument types to constrain the fresh types *)
  CCList.iter2
    (fun fresh_t (arg : Typed.exp) ->
      let _ = unify fresh_t arg.t in
      ())
    fresh_arg_types
    processed_function_args;
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
  , { e =
        EGenCall
          { instance = Option.map fst instance (* Extract just the name *)
          ; generic_path (* Use full path for correct module-qualified lookup *)
          ; args = processed_function_args
          ; explicit_args = processed_explicit_generic_args
          }
    ; t
    ; loc = eloc
    } )


and exp ?(context = normal_context) ?(in_constant_context = false) (env : env) (e : Syntax.exp) : env * exp =
  (* Convert legacy in_constant_context parameter to new context *)
  let context =
    if in_constant_context then
      { context with in_constant = true }
    else
      context
  in
  match e with
  | { e = SEBool value; loc } ->
    let t = C.bool ~loc in
    env, { e = EBool value; t; loc }
  | { e = SEInt value; loc } ->
    let t = C.int ~loc in
    env, { e = EInt (int_of_string value); t; loc }
  | { e = SEReal value; loc } ->
    let t = C.real ~loc in
    env, { e = EReal (float_of_string value); t; loc }
  | { e = SEFixed value; loc } ->
    let t = C.fix16 ~loc in
    let value = String.sub value 0 (String.length value - 1) in
    env, { e = EFixed (float_of_string value); t; loc }
  | { e = SEString value; loc } ->
    let t = C.string ~loc in
    env, { e = EString value; t; loc }
  | { e = SEGroup e; _ } -> exp ~context env e
  | { e = SEId name; loc } when not (String.equal (String.capitalize_ascii name) name) -> (
    let name_path : path = { id = name; n = None; loc } in
    match Env.lookupExpressionSymbol env name_path context with
    | ExprVariable var ->
      let t = var.t in
      let e =
        match var.kind with
        | Val -> { e = EId name; t; loc }
        | Const ->
          let m = Env.getCurrentModule env in
          { e = EConst { id = name; n = Some m.name; loc }; t; loc }
        | Mem _ | Inst ->
          let ctx = Env.getContext env in
          let ctx_t = C.path_t loc ctx in
          { e = EMember ({ e = EId context_name; t = ctx_t; loc }, name); t; loc }
      in
      env, e
    | ExprEnum (type_path, tloc, index) ->
      let t = C.path_t tloc type_path in
      env, { e = EInt index; t; loc }
    | ExprFunction f ->
      if context.in_generic_arg then
        (* In template argument context, allow function references *)
        let args_t, ret = f.t in
        let t = { tx = TEFunction (args_t, ret); const = C.const (); loc } in
        env, { e = EId name; t; loc }
        (* Return function reference *)
      else
        (* In regular context, functions must be called with parentheses *)
        Error.raiseError ("Function '" ^ name ^ "' must be called with parentheses (e.g., '" ^ name ^ "()')") loc
    | ExprType _ ->
      (* Types in expression context are not directly supported - treat as error *)
      Error.raiseError
        ("Type '" ^ name ^ "' cannot be used as a value. Use it in variable declarations or type annotations")
        loc
    | ExprNotFound ->
      Error.raiseError ("Undefined symbol '" ^ name ^ "'. Check spelling or ensure it's declared before use") loc)
  | { e = SEIndex { e; index }; loc } ->
    let env, e = exp ~context env e in
    let env, index = exp ~context env index in
    let t = C.unbound Loc.default in
    (* Allow indexing on arrays and lists *)
    unifyRaise e.loc (C.indexable t) e.t;
    unifyRaise index.loc (C.int ~loc:Loc.default) index.t;
    (* if the type is a builtin (a value) do not unify the constness *)
    let () =
      if (not context.in_constant) && not (Env.isBuiltinType t) then
        unifyConstness t e.t
    in
    env, { e = EIndex { e; index }; t; loc }
  | { e = SEArray []; loc } ->
    Error.raiseError
      "Empty array literal '[]' is not supported. Specify array elements or use array type declaration"
      loc
  | { e = SEArray (h :: t); loc } ->
    let env, h = exp ~context env h in
    let env, t_rev, size =
      CCList.fold_left
        (fun (env, acc, size) e ->
          let env, e = exp ~context env e in
          unifyRaise e.loc h.t e.t;
          env, e :: acc, size + 1)
        (env, [], 1)
        t
    in
    let t = C.array ~size:(C.size ~loc size) h.t in
    env, { e = EArray (h :: CCList.rev t_rev); t; loc }
  | { e = SETuple l; loc } ->
    let env, l = exp_list ~context env l in
    let t = C.tuple ~loc (CCList.map (fun (e : exp) -> e.t) l) in
    env, { e = ETuple l; t; loc }
  | { e = SEIf { cond; then_; else_ }; loc } ->
    let env, cond = exp ~context env cond in
    let env, then_ = exp ~context env then_ in
    let env, else_ = exp ~context env else_ in
    let t = then_.t in
    unifyRaise cond.loc (C.bool ~loc) cond.t;
    unifyRaise else_.loc then_.t else_.t;
    env, { e = EIf { cond; then_; else_ }; t; loc }
  (* we need to add a special case for int() in order to support conversion of enumerations *)
  | { e = SECall { instance = None; path = { id = "int"; n = None; _ } as path; args = [ arg ] as args }; loc }
    when not context.in_constant -> (
    let env, arg = exp ~context env arg in
    match arg with
    | { e = EInt n; loc; _ } -> env, { e = EInt n; t = Typed.C.int ~loc; loc }
    | { e = EId _; loc; t = { tx = TEId tpath; _ } } -> (
      match Env.lookType env tpath loc with
      | { descr = Enum _; _ } -> env, { e = ECall { instance = None; path; args = [ arg ] }; t = Typed.C.int ~loc; loc }
      | _ -> call env None path args loc e.loc)
    | _ -> call env None path args loc e.loc)
  | { e =
        SENamed
          ( { e = SEIndex { e = { e = SEId instance; _ }; index }; _ }
          , { e = SECall { instance = None; path; args }; loc } )
    ; _
    }
    when not in_constant_context -> call env (Some (instance, Some index)) path args loc e.loc
  | { e = SENamed ({ e = SEId instance; _ }, { e = SECall { instance = None; path; args }; loc }); _ }
    when not in_constant_context -> call env (Some (instance, None)) path args loc e.loc
  | { e = SENamed (_e1, _e2); _ } when in_constant_context -> failwith "top_exp: Inference SENamed"
  | { e = SENamed (e1, ({ e = SECall _; _ } as e2)); _ } ->
    let e1 = Pla.print (Syntax.Print.exp e1) in
    let e2 = Pla.print (Syntax.Print.exp e2) in
    failwith ("Inference SENamed: " ^ e1 ^ " : " ^ e2)
  | { e = SENamed (_, _); loc } ->
    (* Handle case where second part is not a function call *)
    Error.raiseError "Invalid instance call syntax. After ':' you must have a function call (e.g., 'name:foo()')" loc
  | { e = SECall { instance; path; args }; loc } when in_constant_context ->
    (* Check if the function has memory declarations *)
    let f = Env.lookFunctionCall env path loc in
    let function_has_mem = Env.isFunctionActive f in
    if function_has_mem then
      Error.raiseError "Functions with memory variables cannot be called in constant expressions" loc
    else
      call env instance path args loc e.loc
  | { e = SECall { instance; path; args }; loc } -> call env instance path args loc e.loc
  | { e = SEOp (op, e1, e2); loc } ->
    let env, e1 = exp ~context env e1 in
    let env, e2 = exp ~context env e2 in
    let f =
      if context.in_constant then
        Env.lookOperatorInModule env op
      else
        Env.lookOperator env op
    in
    let args_t, ret = f.t in
    let t = applyFunction e.loc args_t ret [ e1; e2 ] in
    env, { e = EOp (op, e1, e2); t; loc }
  | { e = SEUnOp (op, e); loc } ->
    let env, e = exp ~context env e in
    let f =
      if context.in_constant then
        Env.lookOperatorInModule env ("u" ^ op)
      else
        Env.lookOperator env ("u" ^ op)
    in
    let args_t, ret = f.t in
    let t = applyFunction e.loc args_t ret [ e ] in
    env, { e = EUnOp (op, e); t; loc }
  | { e = SEMember (e1, m); loc } -> (
    (* First, try to interpret this as an enum reference if e1 is an SEId or SEEnum *)
    match e1 with
    | { e = SEId module_name; _ } when String.equal (String.capitalize_ascii module_name) module_name -> (
      (* First check if this is a module name - try module-qualified access *)
      let const_path = Syntax.{ id = m; n = Some module_name; loc } in
      let results = Env.lookupPath env const_path in
      match results with
      | _ :: _ -> (
        (* Found something in module - check what it is *)
        match Env.findVar results with
        | Some var when var.kind = Const ->
          let t = var.t in
          env, { e = EConst const_path; t; loc }
        | Some var ->
          Error.raiseError
            ("Found '"
            ^ module_name
            ^ "."
            ^ m
            ^ "' but it's not a constant (it's a "
            ^ (match var.kind with
              | Val -> "variable"
              | Mem _ -> "memory"
              | Inst -> "instance"
              | Const -> "constant")
            ^ ")")
            loc
        | None -> (
          (* Check for function or enum *)
          match Env.findFunction results with
          | Some _ ->
            Error.raiseError
              ("'"
              ^ module_name
              ^ "."
              ^ m
              ^ "' is a function, not a constant. Use function call syntax: "
              ^ module_name
              ^ "."
              ^ m
              ^ "(args)")
              loc
          | None -> (
            match Env.findEnum results with
            | Some (type_path, tloc, index) ->
              let t = C.path_t tloc type_path in
              env, { e = EInt index; t; loc }
            | None ->
              Error.raiseError ("Found '" ^ module_name ^ "." ^ m ^ "' but it's not a constant, function, or enum") loc)
          ))
      | [] ->
        (* Module not found - check if it's an actual module name or just not found *)
        Error.raiseError
          ("Module '" ^ module_name ^ "' not found. Check that the module is included or spelled correctly")
          loc)
    | _ -> (
      (* For non-SEId expressions, use normal member access *)
      let env, e1 = exp ~context env e1 in
      match (unlink e1.t).tx with
      | TEId path -> (
        match Env.lookType env path loc with
        | { path; descr = Record members; _ } -> (
          match Map.find m members with
          | None -> Error.raiseError ("The field '" ^ m ^ "' is not part of the type '" ^ pathString path ^ "'") loc
          | Some { t; _ } ->
            let t = refreshConstness t in
            (* if the type is a builtin (a value) do not unify the constness *)
            let () =
              if (not in_constant_context) && not (Env.isBuiltinType t) then
                unifyConstness t e1.t
            in
            env, { e = EMember (e1, m); t; loc })
        | _ ->
          let t = Pla.print (Typed.print_type_ e1.t) in
          let e = Pla.print (Typed.print_exp e1) in
          Error.raiseError ("The expression '" ^ e ^ "' of type '" ^ t ^ "' does not have a member '" ^ m ^ "'.") loc)
      | _ ->
        let t = Pla.print (Typed.print_type_ e1.t) in
        let e = Pla.print (Typed.print_exp e1) in
        Error.raiseError ("The expression '" ^ e ^ "' of type '" ^ t ^ "' does not have a member '" ^ m ^ "'.") loc))
  | { e = SEId id; loc } -> (
    (* This case handles uppercase identifiers (enum constructors) *)
    let id_path : path = { id; n = None; loc } in
    match Env.lookupExpressionSymbol env id_path context with
    | ExprEnum (type_path, tloc, index) ->
      let t = C.path_t tloc type_path in
      env, { e = EInt index; t; loc }
    | ExprNotFound ->
      Error.raiseError ("Undefined symbol '" ^ id ^ "'. Check spelling or ensure it's declared before use") loc
    | _ ->
      Error.raiseError
        ("Symbol '" ^ id ^ "' is not an enumeration value. Use enumeration constructors like 'MyEnum.Value'")
        loc)
  | { e = SERecord { path; elems }; loc } -> (
    let t = Env.lookType env path loc in
    match t with
    | { descr = Record members; _ } ->
      let env, elems_rev =
        CCList.fold_left
          (fun (env, acc) (id, v) ->
            let env, v = exp ~context env v in
            let id, id_loc =
              match id with
              | Syntax.{ id; n = None; loc } -> id, loc
              | { loc; _ } ->
                Error.raiseError ("The name '" ^ path_string id ^ "' is not a valid member of a data type.") loc
            in
            match Env.Map.find id members with
            | None ->
              Error.raiseError ("The name '" ^ id ^ "' does not belong to type '" ^ path_string path ^ "'.") id_loc
            | Some var ->
              unifyRaise v.loc var.t v.t;
              env, (id, v) :: acc)
          (env, [])
          elems
      in
      let elems = CCList.sort (fun (id1, _) (id2, _) -> String.compare id1 id2) elems_rev in
      env, { e = ERecord { path = t.path; elems }; t = Typed.C.path_t loc t.path; loc }
    | _ -> Error.raiseError ("The path '" ^ path_string path ^ "' is not a type.") loc)
  | { e = SETypeIntrinsic (intrinsic_name, type_param); loc } ->
    (* Type intrinsics - convert to typed representation *)
    (* The type is unbound here and will be resolved during generic instantiation *)
    let intrinsic =
      match intrinsic_name with
      | "typedefault" -> Typed.TypeDefault
      | "typemax" -> Typed.TypeMax
      | "typemin" -> Typed.TypeMin
      | _ -> Error.raiseError ("Unknown type intrinsic: " ^ intrinsic_name) loc
    in
    (* Type is unbound - will be resolved during generic specialization *)
    let t = C.unbound loc in
    env, { e = ETypeIntrinsic { intrinsic; type_param }; t; loc }


and exp_list ?(context = normal_context) ?(in_constant_context = false) (env : env) (l : Syntax.exp list) :
    env * exp list =
  (* Convert legacy in_constant_context parameter to new context *)
  let context =
    if in_constant_context then
      { context with in_constant = true }
    else
      context
  in
  let env, rev_l =
    CCList.fold_left
      (fun (env, acc) e ->
        let env, e = exp ~context env e in
        env, e :: acc)
      (env, [])
      l
  in
  env, CCList.rev rev_l


and lexp ?(const = false) (env : env) (e : Syntax.lexp) : env * lexp =
  match e with
  | { l = SLWild; loc } ->
    let t = C.noreturn loc in
    env, { l = LWild; t; loc }
  | { l = SLId name; loc } ->
    let var = Env.lookVar env name loc in
    let t = var.t in
    if not const then
      setTypeMut t;
    let e =
      match var.kind with
      | Val -> { l = LId name; t; loc }
      | Mem _ | Inst ->
        let ctx = Env.getContext env in
        let ctx_t = C.path_t loc ctx in
        { l = LMember ({ l = LId context_name; t = ctx_t; loc }, name); t; loc }
      | Const ->
        Error.raiseError ("Cannot assign to constant '" ^ name ^ "'. Constants are read-only after declaration") loc
    in
    env, e
  | { l = SLGroup e; _ } -> lexp ~const env e
  | { l = SLTuple elems; loc } ->
    let env, elems =
      CCList.fold_left
        (fun (env, acc) e ->
          let env, e = lexp ~const env e in
          env, e :: acc)
        (env, [])
        (CCList.rev elems)
    in
    let t_elems = CCList.map (fun (e : lexp) -> e.t) elems in
    let t = C.tuple ~loc t_elems in
    env, { l = LTuple elems; t; loc }
  | { l = SLIndex { e; index }; loc } ->
    let env, e = lexp ~const env e in
    let env, index = exp env index in
    let t = C.unbound loc in
    unifyRaise index.loc (C.int ~loc) index.t;
    unifyRaise e.loc (C.array ~fixed:false ~loc t) e.t;
    env, { l = LIndex { e; index }; t; loc }
  | { l = SLMember (e, m); loc } -> (
    let env, e = lexp ~const env e in
    match (unlink e.t).tx with
    | TEId path -> (
      match Env.lookType env path loc with
      | { path; descr = Record members; _ } -> (
        match Map.find m members with
        | None -> Error.raiseError ("The field '" ^ m ^ "' is not part of the type '" ^ pathString path ^ "'") loc
        | Some { t; _ } ->
          let t = refreshConstness t in
          (* if the type is a builtin (a value) do not unify the constness *)
          let t =
            if not (Env.isBuiltinType t) then
              { t with const = e.t.const }
            else
              t
          in
          env, { l = LMember (e, m); t; loc })
      | _ ->
        let t = Pla.print (Typed.print_type_ e.t) in
        let e = Pla.print (Typed.print_lexp e) in
        Error.raiseError ("The expression '" ^ e ^ "' of type '" ^ t ^ "' does not have a member '" ^ m ^ "'.") loc)
    | _ ->
      let t = Pla.print (Typed.print_type_ e.t) in
      let e = Pla.print (Typed.print_lexp e) in
      Error.raiseError ("The expression '" ^ e ^ "' of type '" ^ t ^ "' does not have a member '" ^ m ^ "'.") loc)


and dexp (env : env) (e : Syntax.dexp) (kind : var_kind) : env * dexp =
  match e with
  | { d = SDWild; loc } ->
    let t = C.noreturn loc in
    env, { d = DWild; t; loc }
  | { d = SDTuple l; loc } ->
    let env, l =
      CCList.fold_left
        (fun (env, acc) e ->
          let env, e = dexp env e kind in
          env, e :: acc)
        (env, [])
        (CCList.rev l)
    in
    let t = C.tuple ~loc (CCList.map (fun (e : dexp) -> e.t) l) in
    env, { d = DTuple l; t; loc }
  | { d = SDGroup e; _ } -> dexp env e kind
  | { d = SDTyped (e, t); _ } ->
    let env, e = dexp env e kind in
    let t = type_in_f env t in
    checkArrayDimensions t;
    unifyRaise ~bind:true e.loc t e.t;
    env, e
  | { d = SDId (name, dims); loc } ->
    let t =
      match dims with
      | Some size -> C.array ~loc ~size:(C.size ~loc size) (C.unbound loc)
      | None -> C.unbound loc
    in
    let env = Env.addVar env unify name t kind loc in
    env, { d = DId (name, dims); t; loc }


let rec dexp_to_lexp (d : Syntax.dexp) : Syntax.lexp =
  let loc = d.loc in
  match d.d with
  | SDTuple l ->
    let l = CCList.map dexp_to_lexp l in
    { l = SLTuple l; loc }
  | SDWild -> { l = SLWild; loc }
  | SDId (name, _) -> { l = SLId name; loc }
  | SDGroup e -> dexp_to_lexp e
  | SDTyped (e, _) -> dexp_to_lexp e


let stmt_block (stmts : stmt list) =
  match stmts with
  | [ s ] -> s
  | _ -> { s = StmtBlock stmts; loc = Loc.default }


let makeIterWhile (env : env) name id_loc value body loc =
  let tick = Env.getFunctionTick env in
  let itname = name ^ "__" ^ string_of_int tick in
  let open Syntax in
  let int_type = { t = STId { id = "int"; n = None; loc = id_loc }; loc } in
  let dlhs = { d = SDTyped ({ d = SDId (itname, None); loc = id_loc }, int_type); loc = id_loc } in
  let lhs = { l = SLId itname; loc = id_loc } in
  let rhs = { e = SEId itname; loc = id_loc } in
  let decl = { s = SStmtVal (dlhs, Some { e = SEInt "0"; loc }); loc } in
  let incr = { s = SStmtBind (lhs, { e = SEOp ("+", rhs, { e = SEInt "1"; loc }); loc }); loc } in
  let new_body = Syntax.ReaplaceId.inStmt [ name, itname ] { s = SStmtBlock [ body; incr ]; loc } in
  let cond = { e = SEOp ("<", rhs, value); loc } in
  let while_s = { s = SStmtWhile (cond, new_body); loc } in
  { s = SStmtBlock [ decl; while_s ]; loc }


let makeIfOfMatch env e cases =
  let rec makeComparison (e : Syntax.exp) (p : Syntax.pattern) =
    let makeEq e1 e2 = Syntax.{ e = SEOp ("==", e1, e2); loc = e1.loc } in
    let makeAnd e1 e2 = Syntax.{ e = SEOp ("&&", e1, e2); loc = e1.loc } in
    match e, p with
    | _, { p = SPWild; loc } -> Syntax.{ e = SEBool true; loc }
    | { e = SEGroup e; _ }, _ -> makeComparison e p
    | e, { p = SPGroup p; _ } -> makeComparison e p
    | { e = SETuple elems; _ }, { p = SPTuple patterns; loc } ->
      if CCList.length elems = CCList.length patterns then
        let conds = CCList.map2 (fun e p -> makeComparison e p) elems patterns in
        CCList.fold_right makeAnd conds Syntax.{ e = SEBool true; loc }
      else
        let msg =
          "The pattern cannot be matched with the input expression because it has different number of elements."
        in
        let loc = Loc.mergeList Loc.default @@ CCList.map (fun (p : Syntax.pattern) -> p.loc) patterns in
        Error.raiseError msg loc
    | { e = SETuple _; _ }, { loc; _ } ->
      let msg =
        "The pattern cannot be matched with the input expression because it has different number of elements."
      in
      Error.raiseError msg loc
    | _, { p = SPTuple patterns; _ } ->
      let loc = Loc.mergeList Loc.default @@ CCList.map (fun (p : Syntax.pattern) -> p.loc) patterns in
      let msg =
        "The pattern cannot be matched with the input expression because it has different number of elements."
      in
      Error.raiseError msg loc
    | _, { p = SPBool b; loc } -> makeEq e Syntax.{ e = SEBool b; loc }
    | _, { p = SPInt i; loc } -> makeEq e Syntax.{ e = SEInt i; loc }
    | _, { p = SPReal f; loc } -> makeEq e Syntax.{ e = SEReal f; loc }
    | _, { p = SPFixed f; loc } -> makeEq e Syntax.{ e = SEFixed f; loc }
    | _, { p = SPString s; loc } -> makeEq e Syntax.{ e = SEString s; loc }
    | _, { p = SPId id; loc } -> (
      (* Handle enum constructor and constant patterns *)
      let id_path : path = { id; n = None; loc } in
      match Env.lookupExpressionSymbol env id_path normal_context with
      | ExprEnum (_, _, _) ->
        (* Enum constructor: compare with the enum value itself *)
        makeEq e Syntax.{ e = SEId id; loc }
      | ExprVariable var when var.kind = Const ->
        (* Constant: create a constant reference for comparison *)
        makeEq e Syntax.{ e = SEId id; loc }
      | _ -> Error.raiseError ("Pattern '" ^ id ^ "' is not a valid enum constructor or constant") loc)
    | _, { p = SPMember ({ p = SPId module_name; _ }, variant_name); loc } -> (
      (* Handle qualified enum constructor patterns like Button.Push *)
      let id_path : path = { id = variant_name; n = Some module_name; loc } in
      match Env.lookupExpressionSymbol env id_path normal_context with
      | ExprEnum (_, _, _) -> makeEq e Syntax.{ e = SEMember (Syntax.{ e = SEId module_name; loc }, variant_name); loc }
      | _ -> Error.raiseError ("Pattern '" ^ module_name ^ "." ^ variant_name ^ "' is not a valid enum constructor") loc
      )
    | _, { p = SPMember _; loc } ->
      Error.raiseError "Invalid qualified pattern. Only Module.Variant patterns are supported" loc
  in
  let if_stmt =
    CCList.fold_right
      (fun (p, case) else_ ->
        let cond = makeComparison e p in
        Some Syntax.{ s = SStmtIf (cond, case, else_); loc = cond.loc })
      cases
      None
  in
  match if_stmt with
  | None -> failwith "makeIfOfMatch"
  | Some stmt -> stmt


(** Resolves a type intrinsic to a concrete expression during generic instantiation.
    This is called when processing specialized function bodies. *)
let resolve_type_intrinsic_inline (intrinsic : Typed.type_intrinsic) (concrete_type : Typed.type_) (loc : Loc.t) :
    Typed.exp =
  let t = concrete_type in
  let unlinked = unlink concrete_type in
  match intrinsic, unlinked.tx with
  (* typedefault - all types supported *)
  | TypeDefault, TEId { id = "int"; _ } -> { e = EInt 0; t; loc }
  | TypeDefault, TEId { id = "int16"; _ } -> { e = EInt 0; t; loc }
  | TypeDefault, TEId { id = "real"; _ } -> { e = EReal 0.0; t; loc }
  | TypeDefault, TEId { id = "fix16"; _ } -> { e = EFixed 0.0; t; loc }
  | TypeDefault, TEId { id = "bool"; _ } -> { e = EBool false; t; loc }
  | TypeDefault, TEId { id = "string"; _ } -> { e = EString ""; t; loc }
  (* typemax - numeric types only *)
  | TypeMax, TEId { id = "int"; _ } -> { e = EInt 2147483647; t; loc }
  | TypeMax, TEId { id = "int16"; _ } -> { e = EInt 32767; t; loc }
  | TypeMax, TEId { id = "real"; _ } -> { e = EReal 3.40282347e+38; t; loc }
  | TypeMax, TEId { id = "fix16"; _ } -> { e = EFixed 32767.99998; t; loc }
  | TypeMax, TEId { id = "bool"; _ } -> { e = EBool true; t; loc }
  (* typemin - numeric types only *)
  | TypeMin, TEId { id = "int"; _ } -> { e = EInt (-2147483648); t; loc }
  | TypeMin, TEId { id = "int16"; _ } -> { e = EInt (-32768); t; loc }
  | TypeMin, TEId { id = "real"; _ } -> { e = EReal (-3.40282347e+38); t; loc }
  | TypeMin, TEId { id = "fix16"; _ } -> { e = EFixed (-32768.0); t; loc }
  | TypeMin, TEId { id = "bool"; _ } -> { e = EBool false; t; loc }
  (* Compile-time error for unsupported types *)
  | TypeMax, _ ->
    let type_str = Pla.print (Typed.print_type_ concrete_type) in
    Error.raiseError (Printf.sprintf "typemax() is not supported for type '%s'" type_str) loc
  | TypeMin, _ ->
    let type_str = Pla.print (Typed.print_type_ concrete_type) in
    Error.raiseError (Printf.sprintf "typemin() is not supported for type '%s'" type_str) loc
  | TypeDefault, _ ->
    let type_str = Pla.print (Typed.print_type_ concrete_type) in
    Error.raiseError (Printf.sprintf "typedefault() is not supported for type '%s'" type_str) loc


(** Resolves type intrinsics in an expression tree using the type substitution map.
    This is called after processing an expression to replace ETypeIntrinsic nodes
    with concrete values. *)
let rec resolve_type_intrinsics_in_exp (type_substitution_map : (string * type_) list) (e : Typed.exp) : Typed.exp =
  match e.e with
  | ETypeIntrinsic { intrinsic; type_param } -> (
    (* Look up the type parameter in the substitution map *)
    match CCList.assoc_opt ~eq:String.equal type_param type_substitution_map with
    | Some concrete_type -> resolve_type_intrinsic_inline intrinsic concrete_type e.loc
    | None -> Error.raiseError (Printf.sprintf "Type parameter '%s' not found in generic bindings" type_param) e.loc)
  | ECall { instance; path; args } ->
    let args = CCList.map (resolve_type_intrinsics_in_exp type_substitution_map) args in
    { e with e = ECall { instance; path; args } }
  | EOp (op, e1, e2) ->
    let e1 = resolve_type_intrinsics_in_exp type_substitution_map e1 in
    let e2 = resolve_type_intrinsics_in_exp type_substitution_map e2 in
    { e with e = EOp (op, e1, e2) }
  | EUnOp (op, e1) ->
    let e1 = resolve_type_intrinsics_in_exp type_substitution_map e1 in
    { e with e = EUnOp (op, e1) }
  | EIf { cond; then_; else_ } ->
    let cond = resolve_type_intrinsics_in_exp type_substitution_map cond in
    let then_ = resolve_type_intrinsics_in_exp type_substitution_map then_ in
    let else_ = resolve_type_intrinsics_in_exp type_substitution_map else_ in
    { e with e = EIf { cond; then_; else_ } }
  | EIndex { e = arr; index } ->
    let arr = resolve_type_intrinsics_in_exp type_substitution_map arr in
    let index = resolve_type_intrinsics_in_exp type_substitution_map index in
    { e with e = EIndex { e = arr; index } }
  | EArray elems ->
    let elems = CCList.map (resolve_type_intrinsics_in_exp type_substitution_map) elems in
    { e with e = EArray elems }
  | ETuple elems ->
    let elems = CCList.map (resolve_type_intrinsics_in_exp type_substitution_map) elems in
    { e with e = ETuple elems }
  | EMember (e1, m) ->
    let e1 = resolve_type_intrinsics_in_exp type_substitution_map e1 in
    { e with e = EMember (e1, m) }
  | ERecord { path; elems } ->
    let elems = CCList.map (fun (n, v) -> n, resolve_type_intrinsics_in_exp type_substitution_map v) elems in
    { e with e = ERecord { path; elems } }
  | EGenCall { instance; generic_path; args; explicit_args } ->
    let args = CCList.map (resolve_type_intrinsics_in_exp type_substitution_map) args in
    let explicit_args = CCList.map (resolve_type_intrinsics_in_exp type_substitution_map) explicit_args in
    { e with e = EGenCall { instance; generic_path; args; explicit_args } }
  | EGenCompanionCall { instance; companion_name; parent_generic_path; args } ->
    let args = CCList.map (resolve_type_intrinsics_in_exp type_substitution_map) args in
    { e with e = EGenCompanionCall { instance; companion_name; parent_generic_path; args } }
  | EUnit | EBool _ | EInt _ | EReal _ | EFixed _ | EString _ | EId _ | EConst _ -> e


(** Substitutes constant parameter references with their literal values in expressions.
    This is used for specialized generic functions where constant params are inlined. *)
let rec substitute_constants_in_exp (constant_map : (string * Typed.exp) list) (e : Typed.exp) : Typed.exp =
  match e.e with
  | EId name -> (
    (* Check if this identifier is a constant parameter that should be substituted *)
    match CCList.assoc_opt ~eq:String.equal name constant_map with
    | Some const_exp -> { const_exp with loc = e.loc; t = e.t }
    | None -> e)
  | ECall { instance; path; args } ->
    let args = CCList.map (substitute_constants_in_exp constant_map) args in
    { e with e = ECall { instance; path; args } }
  | EOp (op, e1, e2) ->
    let e1 = substitute_constants_in_exp constant_map e1 in
    let e2 = substitute_constants_in_exp constant_map e2 in
    { e with e = EOp (op, e1, e2) }
  | EUnOp (op, e1) ->
    let e1 = substitute_constants_in_exp constant_map e1 in
    { e with e = EUnOp (op, e1) }
  | EIf { cond; then_; else_ } ->
    let cond = substitute_constants_in_exp constant_map cond in
    let then_ = substitute_constants_in_exp constant_map then_ in
    let else_ = substitute_constants_in_exp constant_map else_ in
    { e with e = EIf { cond; then_; else_ } }
  | EIndex { e = arr; index } ->
    let arr = substitute_constants_in_exp constant_map arr in
    let index = substitute_constants_in_exp constant_map index in
    { e with e = EIndex { e = arr; index } }
  | EArray elems ->
    let elems = CCList.map (substitute_constants_in_exp constant_map) elems in
    { e with e = EArray elems }
  | ETuple elems ->
    let elems = CCList.map (substitute_constants_in_exp constant_map) elems in
    { e with e = ETuple elems }
  | EMember (e1, m) ->
    let e1 = substitute_constants_in_exp constant_map e1 in
    { e with e = EMember (e1, m) }
  | ERecord { path; elems } ->
    let elems = CCList.map (fun (n, v) -> n, substitute_constants_in_exp constant_map v) elems in
    { e with e = ERecord { path; elems } }
  | EGenCall { instance; generic_path; args; explicit_args } ->
    let args = CCList.map (substitute_constants_in_exp constant_map) args in
    let explicit_args = CCList.map (substitute_constants_in_exp constant_map) explicit_args in
    { e with e = EGenCall { instance; generic_path; args; explicit_args } }
  | EGenCompanionCall { instance; companion_name; parent_generic_path; args } ->
    let args = CCList.map (substitute_constants_in_exp constant_map) args in
    { e with e = EGenCompanionCall { instance; companion_name; parent_generic_path; args } }
  | EUnit | EBool _ | EInt _ | EReal _ | EFixed _ | EString _ | EConst _ | ETypeIntrinsic _ -> e


(** Substitutes constant parameter references in a left-hand side expression.
    This handles cases like array indices that contain constant parameters. *)
let rec substitute_constants_in_lexp (constant_map : (string * Typed.exp) list) (l : Typed.lexp) : Typed.lexp =
  match l.l with
  | LWild -> l
  | LId _ -> l
  | LMember (e, member_name) ->
    let e = substitute_constants_in_lexp constant_map e in
    { l with l = LMember (e, member_name) }
  | LIndex { e; index } ->
    let e = substitute_constants_in_lexp constant_map e in
    let index = substitute_constants_in_exp constant_map index in
    { l with l = LIndex { e; index } }
  | LTuple lexps ->
    let lexps = CCList.map (substitute_constants_in_lexp constant_map) lexps in
    { l with l = LTuple lexps }


(** Substitutes constant parameter references in a statement tree. *)
let rec substitute_constants_in_stmt (constant_map : (string * Typed.exp) list) (s : Typed.stmt) : Typed.stmt =
  match s.s with
  | StmtVal _ -> s
  | StmtMem (_, _) -> s
  | StmtBind (lhs, rhs) ->
    let lhs = substitute_constants_in_lexp constant_map lhs in
    let rhs = substitute_constants_in_exp constant_map rhs in
    { s with s = StmtBind (lhs, rhs) }
  | StmtReturn e ->
    let e = substitute_constants_in_exp constant_map e in
    { s with s = StmtReturn e }
  | StmtIf (cond, then_, else_opt) ->
    let cond = substitute_constants_in_exp constant_map cond in
    let then_ = substitute_constants_in_stmt constant_map then_ in
    let else_opt = Option.map (substitute_constants_in_stmt constant_map) else_opt in
    { s with s = StmtIf (cond, then_, else_opt) }
  | StmtWhile (cond, body) ->
    let cond = substitute_constants_in_exp constant_map cond in
    let body = substitute_constants_in_stmt constant_map body in
    { s with s = StmtWhile (cond, body) }
  | StmtBlock stmts ->
    let stmts = CCList.map (substitute_constants_in_stmt constant_map) stmts in
    { s with s = StmtBlock stmts }


(* Type substitution version of stmt for processing specialized function bodies *)
let rec stmt_with_type_substitution (env : env) (type_substitution_map : (string * type_) list) (return : type_)
    (s : Syntax.stmt) : env * stmt list =
  (* Use a modified dexp function that substitutes concrete types for generic parameters *)
  let rec dexp_with_substitution env dexp kind =
    match dexp with
    | { Syntax.d = Syntax.SDTuple l; loc } ->
      let env, l =
        CCList.fold_left_map
          (fun env e ->
            let env, e = dexp_with_substitution env e kind in
            env, e)
          env
          (CCList.rev l)
      in
      let t = C.tuple ~loc (CCList.map (fun (e : dexp) -> e.t) l) in
      env, { d = DTuple l; t; loc }
    | { Syntax.d = Syntax.SDGroup e; _ } -> dexp_with_substitution env e kind
    | { Syntax.d = Syntax.SDTyped (e, t); _ } ->
      let env, e = dexp_with_substitution env e kind in
      let t = type_in_m_with_substitution env type_substitution_map t in
      checkArrayDimensions t;
      unifyRaise ~bind:true e.loc t e.t;
      env, e
    | { Syntax.d = Syntax.SDId (name, dims); loc } ->
      let t =
        match dims with
        | Some size -> C.array ~loc ~size:(C.size ~loc size) (C.unbound loc)
        | None -> C.unbound loc
      in
      let env = Env.addVar env unify name t kind loc in
      env, { d = DId (name, dims); t; loc }
    | { Syntax.d = Syntax.SDWild; loc } ->
      let t = C.noreturn loc in
      env, { d = DWild; t; loc }
  in
  (* Use the same stmt function but with our type-substituting dexp *)
  let env, stmts = stmt_generic env dexp_with_substitution return s in
  (* Resolve any type intrinsics in the resulting statements *)
  let stmts = CCList.map (resolve_type_intrinsics_in_stmt type_substitution_map) stmts in
  env, stmts


(** Resolves type intrinsics in a statement tree. *)
and resolve_type_intrinsics_in_stmt (type_substitution_map : (string * type_) list) (s : Typed.stmt) : Typed.stmt =
  match s.s with
  | StmtVal _ -> s (* Declaration doesn't have expressions *)
  | StmtMem (_, _) -> s (* Mem declaration doesn't have expressions *)
  | StmtBind (lhs, rhs) ->
    let rhs = resolve_type_intrinsics_in_exp type_substitution_map rhs in
    { s with s = StmtBind (lhs, rhs) }
  | StmtReturn e ->
    let e = resolve_type_intrinsics_in_exp type_substitution_map e in
    { s with s = StmtReturn e }
  | StmtIf (cond, then_, else_opt) ->
    let cond = resolve_type_intrinsics_in_exp type_substitution_map cond in
    let then_ = resolve_type_intrinsics_in_stmt type_substitution_map then_ in
    let else_opt = Option.map (resolve_type_intrinsics_in_stmt type_substitution_map) else_opt in
    { s with s = StmtIf (cond, then_, else_opt) }
  | StmtWhile (cond, body) ->
    let cond = resolve_type_intrinsics_in_exp type_substitution_map cond in
    let body = resolve_type_intrinsics_in_stmt type_substitution_map body in
    { s with s = StmtWhile (cond, body) }
  | StmtBlock stmts ->
    let stmts = CCList.map (resolve_type_intrinsics_in_stmt type_substitution_map) stmts in
    { s with s = StmtBlock stmts }


and type_in_m_with_substitution (env : env) (type_substitution_map : (string * type_) list) (t : Syntax.type_) =
  match t with
  | { t = STUnbound; loc } -> { tx = TEUnbound None; loc; const = C.const () }
  | { t = STGenericType id; loc } -> (
    (* Generic type parameter - substitute with concrete type *)
    try
      let concrete_type = CCList.assoc ~eq:String.equal id type_substitution_map in
      concrete_type
    with
    | Not_found -> Error.raiseError (Printf.sprintf "Generic type parameter '%s' has no concrete type binding" id) loc)
  | { t = STId path; loc } -> (
    match path with
    | { id; n = None; _ } -> (
      (* Check if this is a generic type parameter that should be substituted *)
      try
        let concrete_type = CCList.assoc ~eq:String.equal id type_substitution_map in
        concrete_type
      with
      | Not_found ->
        (* Regular type lookup *)
        let found = Env.lookType env path loc in
        { tx = TEId found.path; loc; const = C.const () })
    | _ ->
      (* Module qualified path - regular lookup *)
      let found = Env.lookType env path loc in
      { tx = TEId found.path; loc; const = C.const () })
  | { t = STSize n; loc } ->
    let () =
      if n = 0 then
        let msg = "Empty arrays are not supported" in
        Error.raiseError msg loc
    in
    { tx = TESize n; loc; const = C.const () }
  | { t = STComposed (name, l); loc } ->
    let l = CCList.map (type_in_m_with_substitution env type_substitution_map) l in
    { tx = TEComposed (name, l); loc; const = C.const () }


(* Generic-aware version of stmt for processing generic function bodies *)
and stmt_with_generics (env : env) (generic_params : string list) (return : type_) (s : Syntax.stmt) : env * stmt list =
  (* Use a modified dexp function that uses generic-aware type resolution *)
  let rec dexp_generic env dexp kind =
    match dexp with
    | { Syntax.d = Syntax.SDTuple l; loc } ->
      let env, l =
        CCList.fold_left_map
          (fun env e ->
            let env, e = dexp_generic env e kind in
            env, e)
          env
          (CCList.rev l)
      in
      let t = C.tuple ~loc (CCList.map (fun (e : dexp) -> e.t) l) in
      env, { d = DTuple l; t; loc }
    | { Syntax.d = Syntax.SDGroup e; _ } -> dexp_generic env e kind
    | { Syntax.d = Syntax.SDTyped (e, t); _ } ->
      let env, e = dexp_generic env e kind in
      let t = type_in_m_with_generics env generic_params t in
      checkArrayDimensions t;
      unifyRaise ~bind:true e.loc t e.t;
      env, e
    | { Syntax.d = Syntax.SDId (name, dims); loc } ->
      let t =
        match dims with
        | Some size -> C.array ~loc ~size:(C.size ~loc size) (C.unbound loc)
        | None -> C.unbound loc
      in
      let env = Env.addVar env unify name t kind loc in
      env, { d = DId (name, dims); t; loc }
    | { Syntax.d = Syntax.SDWild; loc } ->
      let t = C.noreturn loc in
      env, { d = DWild; t; loc }
  in
  (* Use the same stmt function but with our generic-aware dexp *)
  stmt_generic env dexp_generic return s


and stmt_generic (env : env) (dexp_func : env -> Syntax.dexp -> var_kind -> env * dexp) (return : type_)
    (s : Syntax.stmt) : env * stmt list =
  match s with
  | { s = SStmtError; _ } -> env, []
  | { s = SStmtBlock stmts; loc } ->
    let env = Env.pushScope env in
    let env, stmts = stmt_list_generic env dexp_func return stmts in
    let env = Env.popScope env in
    env, [ { s = StmtBlock stmts; loc } ]
  | { s = SStmtVal (lhs, None); loc } ->
    let env, lhs = dexp_func env lhs Val in
    env, [ { s = StmtVal lhs; loc } ]
  | { s = SStmtVal (lhs, Some rhs); loc } ->
    let env, dlhs = dexp_func env lhs Val in
    let env, lhs = lexp env (dexp_to_lexp lhs) in
    let env, rhs = exp env rhs in
    unifyRaise ~bind:true lhs.loc dlhs.t lhs.t;
    unifyRaise ~bind:true rhs.loc dlhs.t rhs.t;
    env, [ { s = StmtVal dlhs; loc }; { s = StmtBind (lhs, rhs); loc } ]
  | { s = SStmtMem (lhs, None, tags); loc } ->
    let env, lhs = dexp_func env lhs (Mem tags) in
    env, [ { s = StmtMem (lhs, tags); loc } ]
  | { s = SStmtMem (lhs, Some rhs, tags); loc } ->
    let env, dlhs = dexp_func env lhs (Mem tags) in
    let env, lhs = lexp env (dexp_to_lexp lhs) in
    let env, rhs = exp env rhs in
    unifyRaise ~bind:true rhs.loc lhs.t rhs.t;
    env, [ { s = StmtMem (dlhs, tags); loc }; { s = StmtBind (lhs, rhs); loc } ]
  | _ ->
    (* For other statements, use the regular dexp instead of the generic one *)
    let rec normal_stmt (env : env) (return : type_) (s : Syntax.stmt) : env * stmt list =
      match s with
      | { s = SStmtError; _ } -> env, []
      | { s = SStmtBlock stmts; loc } ->
        let env = Env.pushScope env in
        let env, stmts =
          CCList.fold_left_map (fun env s -> normal_stmt env return s) env stmts
          |> fun (env, nested_lists) -> env, CCList.flatten nested_lists
        in
        let env = Env.popScope env in
        env, [ { s = StmtBlock stmts; loc } ]
      | { s = SStmtReturn e; loc } ->
        let env, e = exp env e in
        unifyRaise e.loc return e.t;
        env, [ { s = StmtReturn e; loc } ]
      | { s = SStmtBind (lhs, rhs); loc } ->
        let env, lhs = lexp env lhs in
        let env, rhs = exp env rhs in
        unifyRaise ~bind:true rhs.loc lhs.t rhs.t;
        env, [ { s = StmtBind (lhs, rhs); loc } ]
      | { s = SStmtIf (cond, then_, else_); loc } ->
        let env, cond = exp env cond in
        let env, then_stmts = normal_stmt env return then_ in
        let env, else_stmts =
          match else_ with
          | None -> env, []
          | Some else_stmt ->
            let env, else_stmts = normal_stmt env return else_stmt in
            env, else_stmts
        in
        let then_stmt = stmt_block then_stmts in
        let else_stmt_opt =
          match else_stmts with
          | [] -> None
          | _ -> Some (stmt_block else_stmts)
        in
        env, [ { s = StmtIf (cond, then_stmt, else_stmt_opt); loc } ]
      | { s = SStmtWhile (cond, body); loc } ->
        let env, cond = exp env cond in
        let env, body_stmts = normal_stmt env return body in
        let body = stmt_block body_stmts in
        env, [ { s = StmtWhile (cond, body); loc } ]
      | _ -> failwith "Unhandled statement type in generic processing"
    in
    normal_stmt env return s


and stmt_list_generic (env : env) (dexp_func : env -> Syntax.dexp -> var_kind -> env * dexp) (return : type_)
    (stmts : Syntax.stmt list) : env * stmt list =
  CCList.fold_left_map (fun env s -> stmt_generic env dexp_func return s) env stmts
  |> fun (env, nested_lists) -> env, CCList.flatten nested_lists


let rec stmt (env : env) (return : type_) (s : Syntax.stmt) : env * stmt list =
  match s with
  | { s = SStmtError; _ } -> env, []
  | { s = SStmtBlock stmts; loc } ->
    let env = Env.pushScope env in
    let env, stmts = stmt_list env return stmts in
    let env = Env.popScope env in
    env, [ { s = StmtBlock stmts; loc } ]
  | { s = SStmtVal (lhs, None); loc } ->
    let env, lhs = dexp env lhs Val in
    env, [ { s = StmtVal lhs; loc } ]
  | { s = SStmtVal (lhs, Some rhs); loc } ->
    let env, dlhs = dexp env lhs Val in
    let env, lhs = lexp ~const:true env (dexp_to_lexp lhs) in
    let env, rhs = exp env rhs in
    unifyRaise ~bind:true lhs.loc dlhs.t lhs.t;
    unifyRaise ~bind:true rhs.loc dlhs.t rhs.t;
    env, [ { s = StmtVal dlhs; loc }; { s = StmtBind (lhs, rhs); loc } ]
  | { s = SStmtMem (lhs, None, tags); loc } ->
    let env, lhs = dexp env lhs (Mem tags) in
    env, [ { s = StmtMem (lhs, tags); loc } ]
  | { s = SStmtMem (lhs, Some rhs, tags); loc } ->
    let env, dlhs = dexp env lhs (Mem tags) in
    let env, lhs = lexp env (dexp_to_lexp lhs) in
    let env, rhs = exp env rhs in
    unifyRaise ~bind:true rhs.loc lhs.t rhs.t;
    env, [ { s = StmtMem (dlhs, tags); loc }; { s = StmtBind (lhs, rhs); loc } ]
  | { s = SStmtBind (lhs, rhs); loc } ->
    let env, lhs = lexp env lhs in
    let env, rhs = exp env rhs in
    unifyRaise ~bind:true rhs.loc lhs.t rhs.t;
    env, [ { s = StmtBind (lhs, rhs); loc } ]
  | { s = SStmtReturn e; loc } ->
    let env, e = exp env e in
    unifyRaise e.loc return e.t;
    env, [ { s = StmtReturn e; loc } ]
  | { s = SStmtIf (cond, then_, else_); loc } ->
    let env, cond = exp env cond in
    unifyRaise cond.loc (C.bool ~loc) cond.t;
    let env, then_ = stmt env return then_ in
    let env, else_ = stmt_opt env return else_ in
    env, [ { s = StmtIf (cond, stmt_block then_, else_); loc } ]
  | { s = SStmtWhile (cond, s); loc } ->
    let env, cond = exp env cond in
    unifyRaise cond.loc (C.bool ~loc) cond.t;
    let env, s = stmt env return s in
    env, [ { s = StmtWhile (cond, stmt_block s); loc } ]
  | { s = SStmtIter { id = name, id_loc; value; body }; loc } ->
    let while_s = makeIterWhile env name id_loc value body loc in
    stmt env return while_s
  | { s = SStmtMatch { e; cases }; _ } ->
    let if_stmt = makeIfOfMatch env e cases in
    stmt env return if_stmt


and stmt_opt env return s =
  match s with
  | None -> env, None
  | Some s ->
    let env, s = stmt env return s in
    env, Some (stmt_block s)


and stmt_list env return l =
  let env, l_rev =
    CCList.fold_left
      (fun (env, acc) s ->
        let env, s = stmt env return s in
        env, s :: acc)
      (env, [])
      l
  in
  env, CCList.flatten (CCList.rev l_rev)


let addGeneratedFunctions tags name next =
  if Ptags.has tags "wave" then
    let code = Pla.print {%pla|fun <#name#s>_samples() : int @[placeholder] {}|} in
    let def = Parse.parseFunctionDecl code in
    Some { def with next }
  else if Ptags.has tags "wavetable" then
    let samples = Pla.print {%pla|fun <#name#s>_samples() : int @[placeholder] {}|} in
    let code1 = Pla.print {%pla|fun <#name#s>_raw_c0(i:int) : real @[placeholder] {}|} in
    let code2 = Pla.print {%pla|fun <#name#s>_raw_c1(i:int) : real @[placeholder] {}|} in
    let samples = Parse.parseFunctionDecl samples in
    let def1 = Parse.parseFunctionDecl code1 in
    let def2 = Parse.parseFunctionDecl code2 in
    Some { def1 with next = Some { def2 with next = Some { samples with next } } }
  else
    next


let getOptType env loc (t : Syntax.type_ option) =
  match t with
  | None -> C.unbound loc
  | Some t -> type_in_c env t


let getReturnType env (t : Syntax.type_ option) =
  match t with
  | None -> None
  | Some t -> Some (type_in_c env t)


let convertArguments env (args : Syntax.arg list) : arg list =
  CCList.map (fun (name, t, loc) -> { name; t = getOptType env loc t; loc }) args


(* Convert arguments using a pre-created generic type mapping *)
let convertArgumentsWithGenericMapping env (generic_type_map : (string * type_) list) (args : Syntax.arg list) :
    arg list =
  let getOptTypeWithMapping env loc (t : Syntax.type_ option) =
    match t with
    | None -> C.unbound loc
    | Some t -> type_in_m_with_generic_mapping env generic_type_map t
  in
  CCList.map (fun (name, t, loc) -> { name; t = getOptTypeWithMapping env loc t; loc }) args


(* Legacy wrapper that creates a fresh mapping - kept for backwards compatibility *)
let convertArgumentsWithGenerics env (generic_params : string list) (args : Syntax.arg list) : arg list =
  let loc =
    match args with
    | (_, _, loc) :: _ -> loc
    | [] -> Loc.default
  in
  let generic_type_map = createGenericTypeMapping generic_params loc in
  convertArgumentsWithGenericMapping env generic_type_map args


let registerMultiReturnMem (env : env) name t loc =
  let _, ret = t in
  match unlink ret with
  | { tx = TEComposed ("tuple", elems); _ } ->
    let names = CCList.mapi (fun i t -> path_string name ^ "_ret_" ^ string_of_int i, t) elems in
    CCList.fold_left (fun env (name, t) -> Env.addReturnVar env name t loc) env names
  | _ -> env


let isRoot (args : Args.args) path =
  let s_path = Pla.print (Syntax.print_path path) in
  CCList.mem s_path args.roots


let customInitializer (env : env) tags name =
  if Ptags.has tags "init" then
    Env.addCustomInitFunction env name
  else
    env


let reportReturnTypeMismatch is_placeholder loc (specified_ret : type_ option) (inferred_ret : type_) =
  match specified_ret, inferred_ret with
  | None, { tx = Typed.TENoReturn; _ } -> unifyRaise loc (C.noreturn loc) inferred_ret
  | None, _ -> ()
  | Some t, { tx = Typed.TENoReturn; _ } ->
    (* If the function is a placeholder it will not have body, then the inferred type will be unbound.
       In this case we need to unify the specified and the inferred. *)
    if is_placeholder then
      unifyRaise loc t inferred_ret
    else
      let t = Pla.print (print_type_ t) in
      Error.raiseError ("This function is expected to have type '" ^ t ^ "' but nothing was returned.") loc
  | Some t1, t2 -> unifyRaise loc t1 t2


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
  env, ({ name = path; args; t; loc = def.loc; tags = def.tags; next; is_root }, stmt_block body)


and function_def_opt (iargs : Args.args) (env : env) def_opt =
  match def_opt with
  | None -> env, None
  | Some def ->
    let env = Env.addAliasToContext env def.name def.loc in
    let env, def_body = function_def iargs env def in
    env, Some def_body


let applyMutableTag (args : Typed.arg list) (tags : Typed.tag list) =
  match Ptags.getArguments tags "mutable" with
  | None -> args
  | Some [] -> args
  | Some vars ->
    CCList.map
      (fun (arg : arg) ->
        match CCList.find_opt (fun (n, _, _) -> String.compare n arg.name = 0) vars with
        | Some (_, { g = TagBool mut; _ }, _) ->
          setTypeConstness arg.t (not mut);
          arg
        | _ -> arg)
      args


let ext_function (iargs : Args.args) (env : env) (def : Syntax.ext_def) : env * function_def =
  let ret = getOptType env def.loc def.t in
  let args = convertArguments env def.args in
  let args = applyMutableTag args def.tags in
  let env, path, t = Env.enterFunction env def.name args ret def.loc in
  let env = Env.exitFunction env in
  let next = addGeneratedFunctions def.tags def.name None in
  let env, next = function_def_opt iargs env next in
  env, { name = path; args; t; loc = def.loc; tags = def.tags; next; is_root = false }


let getContextArgument (env : env) (path : path) loc : arg option =
  match Env.getCurrentContext env with
  | Some (_, { descr = Record members; _ }) ->
    if Map.is_empty members then
      None
    else
      let ctx_t =
        let m = Env.getCurrentModule env in
        match Map.find path.id m.functions with
        | Some f -> (
          match Env.lookVarInScopes f.locals context_name with
          | Some var -> var.t
          | None -> failwith "context var not declared")
        | None -> failwith "function not found"
      in
      let () = Env.Map.fold (fun _ (var : var) () -> unifyConstness ctx_t var.t) () members in
      Some { name = context_name; t = ctx_t; loc }
  | _ -> None


let insertContextArgument (env : env) (def : function_def) : function_def =
  (* Check if the function already has a _ctx argument *)
  let already_has_ctx =
    match def.args with
    | { name; _ } :: _ when String.equal name context_name -> true
    | _ -> false
  in
  if already_has_ctx then
    def
  else
    match getContextArgument env def.name def.loc with
    | None -> def
    | Some arg ->
      let rec loop (next : (function_def * stmt) option) : (function_def * stmt) option =
        match next with
        | Some (def, body) ->
          let next = loop def.next in
          Some ({ def with args = arg :: def.args; next }, body)
        | None -> None
      in
      let next = loop def.next in
      { def with args = arg :: def.args; next }


let top_dexp (env : env) (d : Syntax.dexp) =
  match d with
  | { d = SDId (name, dims); loc } ->
    let t =
      match dims with
      | Some size -> C.array ~loc ~size:(C.size ~loc size) (C.unbound loc)
      | None -> C.unbound loc
    in
    (*let env = Env.addVar env unify name t kind loc in*)
    env, { d = DId (name, dims); t; loc }
  | _ -> failwith "invalid constant"


let convert_generic_param (env : env) (param : Syntax.generic_param) : Typed.generic_param =
  match param with
  | Syntax.GParamFunction (name, type_opt) ->
    (* Validate function parameter name *)
    if String.length name = 0 then
      Error.raiseError "Generic function parameter name cannot be empty" Loc.default;
    (* For function templates, we'll determine the actual type during instantiation *)
    (* The type_opt can be used as a constraint later *)
    let converted_type =
      Option.map
        (fun t ->
          let t' = type_in_m env t in
          (* Validate that the constraint type is actually a function type *)
          (match (unlink t').tx with
          | TEFunction (_, _) -> ()
          | _ ->
            Error.raiseError
              (Printf.sprintf
                 "Generic function parameter '%s' type constraint must be a function type, got %s"
                 name
                 (Pla.print (Typed.print_type_ t')))
              Loc.default);
          t' (* Store the constraint type, but actual function type determined at instantiation *))
        type_opt
    in
    Typed.GParamFunction (name, converted_type)
  | Syntax.GParamType name ->
    (* Validate type parameter name *)
    if String.length name = 0 then
      Error.raiseError "Template type parameter name cannot be empty" Loc.default;
    Typed.GParamType name
  | Syntax.GParamConstant (name, type_expr) ->
    (* Validate constant parameter name *)
    if String.length name = 0 then
      Error.raiseError "Generic constant parameter name cannot be empty" Loc.default;
    let converted_type = type_in_m env type_expr in
    (* Validate that the type is a valid constant type (allow unbound for inference) *)
    (match (unlink converted_type).tx with
    | TEId { id = "int" | "real" | "bool" | "string"; _ } -> ()
    | TEUnbound _ -> () (* Allow unbound types - will be inferred from call site *)
    | _ ->
      Error.raiseError
        (Printf.sprintf
           "Generic constant parameter '%s' must have type int, real, bool, or string, got %s"
           name
           (Pla.print (Typed.print_type_ converted_type)))
        Loc.default);
    Typed.GParamConstant (name, converted_type)


let create_generic_function (env : env) (def : Syntax.function_def) : Typed.generic_function =
  (* Validate generic function has generic parameters *)
  if CCList.length def.generic_params = 0 then
    Error.raiseError (Printf.sprintf "Function '%s' marked as template but has no generic parameters" def.name) def.loc;
  (* Check for duplicate generic parameter names *)
  let param_names =
    CCList.map
      (function
        | Syntax.GParamFunction (name, _) -> name
        | Syntax.GParamType name -> name
        | Syntax.GParamConstant (name, _) -> name)
      def.generic_params
  in
  let unique_names = CCList.sort_uniq ~cmp:String.compare param_names in
  if CCList.length unique_names <> CCList.length param_names then
    Error.raiseError (Printf.sprintf "Generic function '%s' has duplicate generic parameter names" def.name) def.loc;
  (* Note: Type parameter names like 'a can have the same base name as function arguments
     like 'a' since they are in separate namespaces (type vs value). *)
  let generic_params = CCList.map (convert_generic_param env) def.generic_params in
  (* Extract just the type parameter names for context *)
  let type_param_names =
    CCList.filter_map
      (function
        | Syntax.GParamType name -> Some name
        | _ -> None)
      def.generic_params
  in
  (* Create a shared mapping from type parameter names to unbound types *)
  (* This ensures all occurrences of the same parameter use the same unbound type *)
  let generic_type_map = createGenericTypeMapping type_param_names def.loc in
  let args = convertArgumentsWithGenericMapping env generic_type_map def.args in
  (* Process return type with the same generic type mapping *)
  let inferred_ret =
    match def.t with
    | Some ret_type -> type_in_m_with_generic_mapping env generic_type_map ret_type
    | None -> C.noreturn def.loc
  in
  (* Create function type from regular arguments only (exclude template params) *)
  let arg_types = CCList.map (fun (arg : Typed.arg) -> arg.t) args in
  (* Convert param_order from Syntax to Typed *)
  let param_order =
    CCList.map
      (function
        | Syntax.PKGeneric i -> Typed.PKGeneric i
        | Syntax.PKArg i -> Typed.PKArg i)
      def.param_order
  in
  (* Capture the type index at definition time - this ensures specialized types appear near the generic's position *)
  let type_index = Env.getGlobalTick () in
  { name = def.name
  ; generic_params
  ; args
  ; param_order
  ; t = arg_types, inferred_ret
  ; body = def.body
  ; next = def.next
  ; loc = def.loc
  ; tags = def.tags
  ; type_index
  }


let has_generic_params (def : Syntax.function_def) : bool = not (CCList.is_empty def.generic_params)

let rec top_stmt (iargs : Args.args) (env : env) (s : Syntax.top_stmt) : env * top_stmt list =
  match s with
  | { top = STopError; _ } -> failwith "Parser error"
  | { top = STopFunction def; loc } when has_generic_params def ->
    (* Store generic function and emit placeholder to mark where specializations go *)
    let generic_func = create_generic_function env def in
    let env = Env.addGeneric env generic_func in
    env, [ { top = TopGenericPlaceholder def.name; loc } ]
  | { top = STopFunction def; _ } ->
    let env = Env.createContextForFunction env def.name def.loc in
    let env, (def, body) = function_def iargs env def in
    let def = insertContextArgument env def in
    let env = Env.exitContext env in
    (* Generic function instantiation is now handled in toprog.ml when processing EGenCall *)
    env, [ { top = TopFunction (def, body); loc = def.loc } ]
  | { top = STopExternal (def, link_name); _ } ->
    let env = Env.createContextForExternal env in
    let env, def = ext_function iargs env def in
    let env = Env.exitContext env in
    env, [ { top = TopExternal (def, link_name); loc = def.loc } ]
  | { top = STopType { name; members }; loc } ->
    let members = CCList.map (fun (name, t, tags, loc) -> name, type_in_m env t, tags, loc) members in
    let members = CCList.sort (fun (n1, _, _, _) (n2, _, _, _) -> compare n1 n2) members in
    let env = Env.addType env name members loc in
    let m = Env.getCurrentModule env in
    let path = Env.getPath m name loc in
    env, [ { top = TopType { path; members }; loc } ]
  | { top = STopEnum { name; members }; loc } ->
    let env = Env.addEnum env name members loc in
    let m = Env.getCurrentModule env in
    let path = Env.getPath m name loc in
    env, [ { top = TopEnum { path; members }; loc } ]
  | { top = STopConstant (({ d = SDId (name, dim); _ } as d), e); loc } ->
    let env, d = top_dexp env d in
    let env, e = exp ~context:constant_context env e in
    unifyRaise e.loc d.t e.t;
    let m = Env.getCurrentModule env in
    let path = Env.getPath m name loc in
    let env = Env.addConstant env unify name d.t loc in
    env, [ { top = TopConstant (path, dim, d.t, e, None); loc } ]
  | { top = STopConstant _; _ } -> failwith ""


and top_stmt_list (iargs : Args.args) (env : env) (s : Syntax.top_stmt list) : env * top_stmt list =
  let env, rev_s =
    CCList.fold_left
      (fun (env, acc) s ->
        let env, stmt_list = top_stmt iargs env s in
        env, stmt_list @ acc)
      (env, [])
      s
  in
  env, rev_s


let getTypesFromModule m =
  Map.fold
    (fun _ t s ->
      match t.descr with
      | Record members when Map.is_empty members -> s
      | Record _ -> t :: s
      | Alias _ -> t :: s
      | Simple | Enum _ -> s)
    []
    m.Env.types


let createTypes (env : env) =
  let types =
    Map.fold
      (fun _ m s ->
        let types = getTypesFromModule m in
        types @ s)
      []
      env.modules
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
        { top = TopType { path = t.path; members }; loc = t.loc }
      | Alias (path, alias_of) -> { top = TopAlias { path; alias_of }; loc = t.loc }
      | Enum _ | Simple -> failwith "There should not be other than records here")
    types


module Set = Set.Make (struct
  type t = path

  let compare = Syntax.compare_path
end)

let rec createExistingTypeSet stmts : Set.t =
  match stmts with
  | [] -> Set.empty
  | { top = TopType { path; _ }; _ } :: t -> Set.add path (createExistingTypeSet t)
  | _ :: t -> createExistingTypeSet t


let removeExistingTypes set types =
  let f s =
    match s with
    | { top = TopType { path; _ }; _ } when Set.mem path set -> false
    | _ -> true
  in
  CCList.filter f types


(* ========== Generic Function Instantiation Post-Processing ========== *)

(* Helper to create a simple path *)
let make_path (name : string) (loc : Loc.t) : Syntax.path = { id = name; n = None; loc }

(* Check if a typed expression is a compile-time constant literal *)
let is_constant_literal (e : Typed.exp) : bool =
  match e.e with
  | EInt _ | EReal _ | EBool _ | EString _ | EFixed _ -> true
  | _ -> false


(* Convert a constant literal expression to a string for signature encoding *)
let constant_to_signature_string (e : Typed.exp) : string =
  match e.e with
  | EInt n ->
    (* Handle negative numbers *)
    if n < 0 then
      "n" ^ string_of_int (abs n)
    else
      string_of_int n
  | EReal f ->
    (* Convert float to string, replacing dots and negative signs for valid identifier *)
    let s = Printf.sprintf "%.6g" f in
    let s = Str.global_replace (Str.regexp_string ".") "_" s in
    let s = Str.global_replace (Str.regexp_string "-") "n" s in
    s
  | EBool b ->
    if b then
      "true"
    else
      "false"
  | EString s ->
    (* Use hash of string for short signature *)
    Printf.sprintf "s%x" (Hashtbl.hash s land 0xFFFF)
  | EFixed f ->
    let s = Printf.sprintf "%.6g" f in
    let s = Str.global_replace (Str.regexp_string ".") "_" s in
    let s = Str.global_replace (Str.regexp_string "-") "n" s in
    "fx" ^ s
  | _ -> "var" (* Should not happen if is_constant_literal was checked *)


(* State for tracking instantiated generic functions during post-processing *)
type instantiation_state =
  { mutable instantiated : (string, Typed.function_def * Typed.stmt) Hashtbl.t
        (* signature string -> (specialized_def, body) *)
  ; mutable pending_functions : (string * string * Typed.function_def * Typed.stmt) list
        (* (module_name, generic_name, specialized_def, body) - functions to be added at specific placeholders *)
  ; mutable functions_needing_context : (string, Typed.type_) Hashtbl.t
        (* function path string -> context type. Tracks functions that have been updated to need _ctx *)
  ; mutable processed_companions : (string, (Typed.function_def * Typed.stmt) option) Hashtbl.t
        (* generic function name -> processed companion chain. Caches companions to avoid re-processing *)
  ; mutable pending_generic_calls : Typed.exp list
        (* EGenCall expressions found during prescan, to be processed on-demand when companion calls need them *)
  }

let create_instantiation_state () : instantiation_state =
  { instantiated = Hashtbl.create 16
  ; pending_functions = []
  ; functions_needing_context = Hashtbl.create 16
  ; processed_companions = Hashtbl.create 16
  ; pending_generic_calls = []
  }


(* Build a signature string for deduplication based on resolved types *)
let build_signature_string (generic_name : string) (arg_types : Typed.type_ list) : string =
  let type_strings = CCList.map type_to_mangled_name arg_types in
  generic_name ^ "_" ^ String.concat "_" type_strings


(* Build a signature string that includes constant values for fully specialized functions *)
let build_specialized_signature_string (generic_name : string) (arg_types : Typed.type_ list)
    (explicit_args : Typed.exp list) : string =
  let type_strings = CCList.map type_to_mangled_name arg_types in
  let const_strings = CCList.map constant_to_signature_string explicit_args in
  generic_name ^ "_" ^ String.concat "_" type_strings ^ "_" ^ String.concat "_" const_strings


(* Build a signature string for non-specialized version (when any constant param is a variable) *)
let build_nonspec_signature_string (generic_name : string) (_arg_types : Typed.type_ list) : string =
  (* Use original name for non-specialized version *)
  generic_name


(* Create a specialized function from a generic function with resolved types.
   This runs type inference on the body with the type bindings.
   explicit_args contains the values for constant generic params.

   Value-based specialization:
   - If ALL explicit_args are compile-time constant literals -> create specialized version
     with constants inlined in the body (no constant params as function args)
   - If ANY explicit_arg is a variable -> create non-specialized version with all
     constant params as function arguments *)
let instantiate_generic_function (iargs : Args.args) (env : env) (state : instantiation_state)
    (generic_func : Typed.generic_function) (call_arg_types : Typed.type_ list) (explicit_args : Typed.exp list)
    (loc : Loc.t) : Typed.function_def * Typed.stmt =
  (* Check if all explicit args are compile-time constant literals *)
  let all_constants = CCList.for_all is_constant_literal explicit_args in
  (* Build specialized name based on whether we can fully specialize *)
  let specialized_name =
    if all_constants && CCList.length explicit_args > 0 then
      (* All constants - include constant values in signature for unique specialization *)
      build_specialized_signature_string generic_func.name call_arg_types explicit_args
    else if CCList.length explicit_args > 0 then
      (* Any variable - use non-specialized signature (single version for all variable calls) *)
      build_nonspec_signature_string generic_func.name call_arg_types
    else
      (* No constant params - just use type-based signature *)
      build_signature_string generic_func.name call_arg_types
  in
  (* Extract type parameter names *)
  let type_param_names =
    CCList.filter_map
      (function
        | Typed.GParamType name -> Some name
        | _ -> None)
      generic_func.generic_params
  in
  (* Extract constant parameter info - name and type from explicit_args *)
  let constant_params =
    CCList.filter_map
      (function
        | Typed.GParamConstant (name, param_type) -> Some (name, param_type)
        | _ -> None)
      generic_func.generic_params
  in
  (* Create type bindings: map generic type params to concrete types *)
  let generic_func_arg_types, generic_func_ret_type = generic_func.t in
  (* Create a substitution map from generic type params to concrete types *)
  (* We do this by unifying the generic arg types with the call arg types *)
  let fresh_types = Typed.copy_types_preserving_sharing (generic_func_arg_types @ [ generic_func_ret_type ]) in
  let fresh_arg_types, fresh_ret_type =
    match CCList.rev fresh_types with
    | last :: rest -> CCList.rev rest, last
    | [] -> failwith "Empty type list"
  in
  (* Unify fresh types with call types to bind them to concrete types *)
  CCList.iter2
    (fun fresh_t call_t ->
      let _ = unify fresh_t call_t in
      ())
    fresh_arg_types
    call_arg_types;
  (* Build type substitution map: map type param names to concrete types from call args.
     For each type param, we find the corresponding concrete type.
     Since type params appear in the function args in order they're declared,
     we match type param i with call_arg_type at the position where that param is used. *)
  let type_substitution_map =
    CCList.mapi
      (fun i name ->
        (* For simple cases (one type param used for first arg), use call_arg_types[i] *)
        let concrete_type =
          if i < CCList.length call_arg_types then
            CCList.nth call_arg_types i
          else if CCList.length call_arg_types > 0 then
            CCList.hd call_arg_types
          else
            (* Fallback - create int type if no args *)
            { tx = TEId { id = "int"; n = None; loc }; loc; const = C.const () }
        in
        name, concrete_type)
      type_param_names
  in
  (* Build constant substitution map for specialized case *)
  let constant_substitution_map =
    if all_constants && CCList.length explicit_args > 0 then
      (* Build constant substitution map: param_name -> constant expression *)
      CCList.mapi
        (fun i (name, _param_type) ->
          if i < CCList.length explicit_args then
            name, CCList.nth explicit_args i
          else
            failwith "Mismatch between constant_params and explicit_args")
        constant_params
    else
      []
  in
  (* Build ALL args (including constant params) for body processing.
     This ensures constant params are in the environment during type inference. *)
  let constant_args : Typed.arg list =
    CCList.mapi
      (fun i (name, param_type) ->
        let actual_type =
          if i < CCList.length explicit_args then
            (CCList.nth explicit_args i).t
          else
            param_type
        in
        { name; t = actual_type; loc })
      constant_params
  in
  let regular_args_array = Array.of_list generic_func.args in
  let constant_args_array = Array.of_list constant_args in
  let all_args_for_body =
    CCList.map
      (fun pk ->
        match pk with
        | Typed.PKArg i ->
          let arg = regular_args_array.(i) in
          if i < CCList.length fresh_arg_types then
            { arg with t = CCList.nth fresh_arg_types i }
          else
            arg
        | Typed.PKGeneric i ->
          if i < Array.length constant_args_array then
            constant_args_array.(i)
          else
            failwith "Invalid generic param index in param_order")
      generic_func.param_order
  in
  (* Build the final specialized args based on specialization mode *)
  let specialized_args =
    if all_constants && CCList.length explicit_args > 0 then
      (* Path A: All constants - exclude constant params from function signature *)
      CCList.filter_map
        (fun pk ->
          match pk with
          | Typed.PKArg i ->
            let arg = regular_args_array.(i) in
            let arg_with_type =
              if i < CCList.length fresh_arg_types then
                { arg with t = CCList.nth fresh_arg_types i }
              else
                arg
            in
            Some arg_with_type
          | Typed.PKGeneric _ ->
            (* Skip constant params - they will be inlined *)
            None)
        generic_func.param_order
    else
      (* Path B: Any variable (or no constant params) - include all params as args *)
      all_args_for_body
  in
  (* Create context for the specialized function using the type_index from the generic definition *)
  (* This ensures the specialized type appears near where the original generic was defined *)
  let env = Env.createContextForFunctionWithIndex env specialized_name loc generic_func.type_index in
  (* Enter function context with ALL args so constant params are in environment during body processing *)
  let inferred_ret = C.noreturn loc in
  let env, path, _t = Env.enterFunction env specialized_name all_args_for_body inferred_ret loc in
  (* Process body with type substitution for generic parameters *)
  let env, body = stmt_with_type_substitution env type_substitution_map fresh_ret_type generic_func.body in
  (* If we have constant substitutions, apply them to the body *)
  let body =
    if CCList.length constant_substitution_map > 0 then
      CCList.map (substitute_constants_in_stmt constant_substitution_map) body
    else
      body
  in
  let env = Env.exitFunction env in
  (* Process companion 'and' functions if present - only process once per generic function *)
  let env, next =
    match generic_func.next with
    | None -> env, None
    | Some _ -> (
      (* Check if we've already processed companions for this generic function *)
      let generic_key = generic_func.name in
      match Hashtbl.find_opt state.processed_companions generic_key with
      | Some cached_next ->
        (* Already processed - reuse the cached companions *)
        env, cached_next
      | None ->
        (* First time processing this generic's companions *)
        let env, next = function_def_opt iargs env generic_func.next in
        Hashtbl.add state.processed_companions generic_key next;
        env, next)
  in
  (* Create the specialized function definition with the appropriate args *)
  let specialized_def : Typed.function_def =
    { name = path
    ; args = specialized_args
    ; t = CCList.map (fun (a : Typed.arg) -> a.t) specialized_args, fresh_ret_type
    ; loc = generic_func.loc
    ; tags = generic_func.tags
    ; is_root = false
    ; next
    }
  in
  (* Add context argument if the function has mem/instances *)
  let specialized_def = insertContextArgument env specialized_def in
  let env = Env.exitContext env in
  let _ = env in
  let _ = iargs in
  (* Combine the body statements into a single block *)
  let combined_body =
    match body with
    | [ single ] -> single
    | stmts -> { s = StmtBlock stmts; loc = generic_func.loc }
  in
  specialized_def, combined_body


(* Pre-scan a statement to find all EGenCall nodes and trigger their instantiation.
   This ensures that companion function calls (EGenCompanionCall) can find their parent's
   instantiation even if the companion is called BEFORE the primary generic function.
   This is the pattern used in VultModules-private where set_* companions are called
   inside an if block before the main function call.

   The instantiation is cached in state.instantiated, so when the actual processing
   happens later, the cached result is reused. This function does NOT create instances
   or increment tick counters - it only triggers the creation of specialized function
   definitions. *)
let rec prescan_generic_calls_in_stmt (iargs : Args.args) (env : env) (state : instantiation_state) (stmt : Typed.stmt)
    : unit =
  match stmt.s with
  | StmtVal _ -> () (* Variable declarations don't have expressions *)
  | StmtReturn e -> prescan_generic_calls_in_exp iargs env state e
  | StmtBind (_, e) -> prescan_generic_calls_in_exp iargs env state e
  | StmtIf (cond, then_s, else_opt) ->
    prescan_generic_calls_in_exp iargs env state cond;
    prescan_generic_calls_in_stmt iargs env state then_s;
    Option.iter (prescan_generic_calls_in_stmt iargs env state) else_opt
  | StmtWhile (cond, body) ->
    prescan_generic_calls_in_exp iargs env state cond;
    prescan_generic_calls_in_stmt iargs env state body
  | StmtBlock stmts -> CCList.iter (prescan_generic_calls_in_stmt iargs env state) stmts
  | StmtMem _ -> ()


and prescan_generic_calls_in_exp (iargs : Args.args) (env : env) (state : instantiation_state) (e : Typed.exp) : unit =
  match e.e with
  | EGenCall _ ->
    (* During prescan, collect this EGenCall expression so companion call handling
       can trigger its instantiation on-demand if needed. We don't process it here
       to avoid creating instances and incrementing tick counters prematurely. *)
    state.pending_generic_calls <- e :: state.pending_generic_calls
  | ECall { args; _ } -> CCList.iter (prescan_generic_calls_in_exp iargs env state) args
  | EIf { cond; then_; else_ } ->
    prescan_generic_calls_in_exp iargs env state cond;
    prescan_generic_calls_in_exp iargs env state then_;
    prescan_generic_calls_in_exp iargs env state else_
  | EOp (_, lhs, rhs) ->
    prescan_generic_calls_in_exp iargs env state lhs;
    prescan_generic_calls_in_exp iargs env state rhs
  | EUnOp (_, arg) -> prescan_generic_calls_in_exp iargs env state arg
  | EIndex { e = inner; index } ->
    prescan_generic_calls_in_exp iargs env state inner;
    prescan_generic_calls_in_exp iargs env state index
  | EArray elems | ETuple elems -> CCList.iter (prescan_generic_calls_in_exp iargs env state) elems
  | EMember (inner, _) -> prescan_generic_calls_in_exp iargs env state inner
  | ERecord { elems; _ } -> CCList.iter (fun (_, v) -> prescan_generic_calls_in_exp iargs env state v) elems
  | EGenCompanionCall { args; _ } ->
    (* Don't trigger companion calls during prescan - they depend on parent being instantiated.
       But do scan their arguments for nested generic calls. *)
    CCList.iter (prescan_generic_calls_in_exp iargs env state) args
  | EId _ | EUnit | EBool _ | EInt _ | EReal _ | EFixed _ | EString _ | EConst _ | ETypeIntrinsic _ -> ()


(* Process an expression, replacing EGenCall with ECall to specialized functions.
   Mutually recursive with process_stmt_instantiation to handle nested generic calls. *)
let rec process_exp_instantiation (iargs : Args.args) (env : env) (state : instantiation_state) (e : Typed.exp) :
    Typed.exp =
  let loc = e.loc in
  match e.e with
  | EGenCall { instance; generic_path; args; explicit_args } -> (
    (* Look up the generic function using the stored path *)
    let generic_name = Pla.print (Syntax.print_path generic_path) in
    match Env.lookupGeneric env generic_path with
    | None -> Error.raiseError (Printf.sprintf "Generic function '%s' not found" generic_name) loc
    | Some generic_func ->
      (* Get resolved types from the arguments *)
      let resolved_arg_types = CCList.map (fun (a : Typed.exp) -> unlink a.t) args in
      (* Check if all explicit args are compile-time constant literals *)
      let all_constants = CCList.for_all is_constant_literal explicit_args in
      (* Build signature for deduplication based on specialization mode *)
      let signature =
        if all_constants && CCList.length explicit_args > 0 then
          build_specialized_signature_string generic_name resolved_arg_types explicit_args
        else if CCList.length explicit_args > 0 then
          build_nonspec_signature_string generic_name resolved_arg_types
        else
          build_signature_string generic_name resolved_arg_types
      in
      (* Check if already instantiated, get the function definition and path *)
      let specialized_def =
        match Hashtbl.find_opt state.instantiated signature with
        | Some (def, _) -> def
        | None ->
          (* Determine which module to create the specialization in *)
          let generic_module = generic_path.n in
          (* If the generic is from a different module, enter that module's context *)
          let env_for_instantiation =
            match generic_module with
            | Some module_name ->
              (* Enter the generic's module context for instantiation *)
              Env.enterModule env module_name
            | None -> env
          in
          (* Create new instantiation in the correct module context *)
          let def, body =
            instantiate_generic_function
              iargs
              env_for_instantiation
              state
              generic_func
              resolved_arg_types
              explicit_args
              loc
          in
          (* Pre-scan the body to find all EGenCall nodes and trigger their instantiation.
             This ensures companion calls can find their parent's instantiation. *)
          let () = prescan_generic_calls_in_stmt iargs env_for_instantiation state body in
          (* Recursively process the body to replace any nested EGenCall nodes *)
          let processed_body = process_stmt_instantiation iargs env_for_instantiation state body in
          (* Exit the module if we entered one *)
          let _ =
            match generic_module with
            | Some _ -> Env.exitModule env_for_instantiation
            | None -> env_for_instantiation
          in
          (* Get the module name for tracking - use from the definition's path *)
          let target_module =
            match def.name.n with
            | Some m -> m
            | None -> (
              (* Fallback to current module if path doesn't have module info *)
              match generic_module with
              | Some m -> m
              | None ->
                (* Use current env's module *)
                let m = Env.getCurrentModule env in
                m.name)
          in
          Hashtbl.add state.instantiated signature (def, processed_body);
          state.pending_functions <- (target_module, generic_func.name, def, processed_body) :: state.pending_functions;
          def
      in
      (* Process both regular and explicit arguments recursively *)
      let processed_regular_args = CCList.map (process_exp_instantiation iargs env state) args in
      let processed_explicit_args = CCList.map (process_exp_instantiation iargs env state) explicit_args in
      (* Build the final args list based on specialization mode *)
      let processed_args =
        if all_constants && CCList.length explicit_args > 0 then
          (* Specialized version: only pass regular args, constants are inlined *)
          let regular_args_array = Array.of_list processed_regular_args in
          CCList.filter_map
            (fun pk ->
              match pk with
              | Typed.PKArg i ->
                if i < Array.length regular_args_array then
                  Some regular_args_array.(i)
                else
                  failwith "Invalid arg index in param_order"
              | Typed.PKGeneric _ ->
                (* Skip constant args - they are inlined in the function body *)
                None)
            generic_func.param_order
        else
          (* Non-specialized version: pass all args including constants *)
          let regular_args_array = Array.of_list processed_regular_args in
          let explicit_args_array = Array.of_list processed_explicit_args in
          CCList.map
            (fun pk ->
              match pk with
              | Typed.PKArg i ->
                if i < Array.length regular_args_array then
                  regular_args_array.(i)
                else
                  failwith "Invalid arg index in param_order"
              | Typed.PKGeneric i ->
                if i < Array.length explicit_args_array then
                  explicit_args_array.(i)
                else
                  failwith "Invalid generic param index in param_order")
            generic_func.param_order
      in
      (* Propagate variability from specialized function's args to the call expressions.
         This ensures that if the specialized function mutates an array parameter,
         the caller's array variable is marked as mutable (non-const).
         Skip _ctx if present since it's not part of the original call arguments. *)
      let specialized_non_ctx_args =
        match specialized_def.args with
        | { name; _ } :: rest when String.equal name context_name -> rest
        | args -> args
      in
      let () = propagateVariability env loc (Some specialized_non_ctx_args) processed_args in
      (* Check if specialized function needs context (has _ctx as first argument) *)
      let final_args =
        match specialized_def.args with
        | { name; t = ctx_t; _ } :: _ when String.equal name context_name ->
          (* Function needs context - create instance and add context argument *)
          let current_f = Env.getCurrentFunction env in
          let current_ctx_t =
            match Env.lookVarInScopes current_f.locals context_name with
            | Some var -> var.t
            | None -> failwith "context var not declared in caller"
          in
          (* Get or generate instance name *)
          let inst_name =
            match instance with
            | Some user_inst_name ->
              (* User provided explicit instance name - use it directly *)
              let () =
                if not (checkMemExists env user_inst_name || Env.checkConstantExists env user_inst_name) then
                  (* New user-provided instance - create it *)
                  let _ = Env.addVar env unify user_inst_name ctx_t Inst loc in
                  ()
              in
              user_inst_name
            | None ->
              (* Generate unique instance name *)
              let number =
                Printf.sprintf
                  "%.2x%.2x"
                  (0xFF land Hashtbl.hash (path_string specialized_def.name))
                  (0xFF land Hashtbl.hash (path_string (Env.getContext env)))
              in
              let rec generateName () =
                let n = Env.getFunctionTick env in
                let name = "inst_" ^ string_of_int n ^ number in
                if checkMemExists env name || Env.checkConstantExists env name then
                  generateName ()
                else
                  name
              in
              let name = generateName () in
              (* Add instance to caller's context *)
              let _ = Env.addVar env unify name ctx_t Inst loc in
              name
          in
          (* Create context argument expression *)
          let ctx_e = { e = EId context_name; t = current_ctx_t; loc } in
          let inst_e = { e = EMember (ctx_e, inst_name); t = ctx_t; loc } in
          inst_e :: processed_args
        | _ ->
          (* No context needed *)
          processed_args
      in
      (* Replace with regular ECall using the specialized function's path *)
      { e = ECall { instance = None; path = specialized_def.name; args = final_args }; t = e.t; loc })
  | ECall { instance; path; args } ->
    let processed_args = CCList.map (process_exp_instantiation iargs env state) args in
    (* Propagate variability from the called function's args to the call expressions.
       This handles the case where a function's args were marked mutable during Phase 2
       (e.g., because it calls a specialized generic function that mutates its args),
       and we need to propagate that to callers of this function. *)
    let () =
      match Env.tryLookFunctionCall env path with
      | Some f ->
        (* Skip _ctx if present since it's not part of the original call arguments *)
        let func_non_ctx_args =
          match f.args with
          | Some ({ name; _ } :: rest) when String.equal name context_name -> Some rest
          | args -> args
        in
        propagateVariability env loc func_non_ctx_args processed_args
      | None -> ()
    in
    (* Check if the called function has been updated to need context *)
    let func_path_str = path_string path in
    let final_args =
      match Hashtbl.find_opt state.functions_needing_context func_path_str with
      | Some ctx_t ->
        (* Function needs context - create instance and add context argument *)
        let current_f = Env.getCurrentFunction env in
        let current_ctx_t =
          match Env.lookVarInScopes current_f.locals context_name with
          | Some var -> var.t
          | None ->
            (* Caller doesn't have context yet - this can happen for top-level calls.
               In this case, we need to ensure the caller also gets context. *)
            failwith
              (Printf.sprintf
                 "Function '%s' calls '%s' which needs context, but caller has no context"
                 (path_string current_f.path)
                 func_path_str)
        in
        (* Generate unique instance name *)
        let number =
          Printf.sprintf
            "%.2x%.2x"
            (0xFF land Hashtbl.hash func_path_str)
            (0xFF land Hashtbl.hash (path_string (Env.getContext env)))
        in
        let rec generateName () =
          let n = Env.getFunctionTick env in
          let inst_name = "inst_" ^ string_of_int n ^ number in
          if checkMemExists env inst_name || Env.checkConstantExists env inst_name then
            generateName ()
          else
            inst_name
        in
        let inst_name = generateName () in
        (* Add instance to caller's context *)
        let _ = Env.addVar env unify inst_name ctx_t Inst loc in
        (* Create context argument expression *)
        let ctx_e = { e = EId context_name; t = current_ctx_t; loc } in
        let inst_e = { e = EMember (ctx_e, inst_name); t = ctx_t; loc } in
        inst_e :: processed_args
      | None ->
        (* Function doesn't need context *)
        processed_args
    in
    { e with e = ECall { instance; path; args = final_args } }
  | EOp (op, e1, e2) ->
    let e1 = process_exp_instantiation iargs env state e1 in
    let e2 = process_exp_instantiation iargs env state e2 in
    { e with e = EOp (op, e1, e2) }
  | EUnOp (op, e1) ->
    let e1 = process_exp_instantiation iargs env state e1 in
    { e with e = EUnOp (op, e1) }
  | EIf { cond; then_; else_ } ->
    let cond = process_exp_instantiation iargs env state cond in
    let then_ = process_exp_instantiation iargs env state then_ in
    let else_ = process_exp_instantiation iargs env state else_ in
    { e with e = EIf { cond; then_; else_ } }
  | EIndex { e = arr; index } ->
    let arr = process_exp_instantiation iargs env state arr in
    let index = process_exp_instantiation iargs env state index in
    { e with e = EIndex { e = arr; index } }
  | EArray elems ->
    let elems = CCList.map (process_exp_instantiation iargs env state) elems in
    { e with e = EArray elems }
  | ETuple elems ->
    let elems = CCList.map (process_exp_instantiation iargs env state) elems in
    { e with e = ETuple elems }
  | EMember (e1, m) ->
    let e1 = process_exp_instantiation iargs env state e1 in
    { e with e = EMember (e1, m) }
  | ERecord { path; elems } ->
    let elems = CCList.map (fun (n, v) -> n, process_exp_instantiation iargs env state v) elems in
    { e with e = ERecord { path; elems } }
  | EGenCompanionCall { instance; companion_name; parent_generic_path; args } -> (
    (* Look up the parent generic function *)
    match Env.lookupGeneric env parent_generic_path with
    | None ->
      Error.raiseError
        (Printf.sprintf
           "Parent generic function '%s' not found for companion '%s'"
           (path_string parent_generic_path)
           companion_name)
        loc
    | Some _parent_generic -> (
      (* Find an instantiated version of the parent generic in our state *)
      (* We look through pending_functions to find one matching the parent generic name *)
      let parent_name = parent_generic_path.id in
      let matching_instantiation =
        CCList.find_opt
          (fun (_module, gen_name, (def : Typed.function_def), _body) ->
            String.equal gen_name parent_name
            &&
            (* Also check that the specialized function has the companion in its next chain *)
            let rec has_companion (next : (Typed.function_def * Typed.stmt) option) =
              match next with
              | None -> false
              | Some (companion_def, _) ->
                if String.equal companion_def.name.id companion_name then
                  true
                else
                  has_companion companion_def.next
            in
            has_companion def.next)
          state.pending_functions
      in
      (* If no instantiation found, try to find and process a pending parent call *)
      let matching_instantiation =
        match matching_instantiation with
        | Some _ -> matching_instantiation
        | None -> (
          (* Look for a pending parent call that we can instantiate on-demand *)
          let matching_pending =
            CCList.find_opt
              (fun (pending_e : Typed.exp) ->
                match pending_e.e with
                | EGenCall { generic_path; _ } -> String.equal generic_path.id parent_name
                | _ -> false)
              state.pending_generic_calls
          in
          match matching_pending with
          | Some parent_call ->
            (* Process the parent call to instantiate it *)
            let _ = process_exp_instantiation iargs env state parent_call in
            (* Now look again in pending_functions *)
            CCList.find_opt
              (fun (_module, gen_name, (def : Typed.function_def), _body) ->
                String.equal gen_name parent_name
                &&
                let rec has_companion (next : (Typed.function_def * Typed.stmt) option) =
                  match next with
                  | None -> false
                  | Some (companion_def, _) ->
                    if String.equal companion_def.name.id companion_name then
                      true
                    else
                      has_companion companion_def.next
                in
                has_companion def.next)
              state.pending_functions
          | None -> None)
      in
      match matching_instantiation with
      | None ->
        (* No matching instantiation found - the parent generic hasn't been called yet.
           This is an error - companion calls must come after the main generic call. *)
        Error.raiseError
          (Printf.sprintf
             "Companion function '%s' called before parent generic '%s' was instantiated. Make sure to call the parent \
              function first."
             companion_name
             parent_name)
          loc
      | Some (_, _, specialized_def, _) -> (
        (* Find the companion function in the specialized function's next chain *)
        let rec find_companion (next : (function_def * stmt) option) : (function_def * stmt) option =
          match next with
          | None -> None
          | Some ((companion_def, _companion_body) as companion) ->
            if String.equal companion_def.name.id companion_name then
              Some companion
            else
              find_companion companion_def.next
        in
        match find_companion specialized_def.next with
        | None ->
          Error.raiseError
            (Printf.sprintf "Companion function '%s' not found in instantiated generic" companion_name)
            loc
        | Some (companion_def, _) ->
          (* Process arguments recursively *)
          let processed_args = CCList.map (process_exp_instantiation iargs env state) args in
          (* Unify companion function parameter types with call argument types.
             This enables type inference for companion function parameters that don't have
             explicit annotations and whose types can't be inferred from the body alone. *)
          let companion_non_ctx_args =
            match companion_def.args with
            | { name; _ } :: rest when String.equal name context_name -> rest
            | args -> args
          in
          let () =
            if CCList.length companion_non_ctx_args = CCList.length processed_args then
              CCList.iter2
                (fun (def_arg : Typed.arg) (call_arg : Typed.exp) ->
                  let _ = unify def_arg.t call_arg.t in
                  ())
                companion_non_ctx_args
                processed_args
          in
          (* Handle context argument for companion function if needed *)
          let final_args =
            match companion_def.args with
            | { name; t = ctx_t; _ } :: _ when String.equal name context_name ->
              (* Companion needs context - reuse the instance from the main function call *)
              let current_f = Env.getCurrentFunction env in
              let current_ctx_t =
                match Env.lookVarInScopes current_f.locals context_name with
                | Some var -> var.t
                | None -> failwith "context var not declared in caller for companion call"
              in
              (* Get instance name - use user-provided or search for auto-generated *)
              let inst_name =
                match instance with
                | Some user_inst_name ->
                  (* User provided explicit instance name - use it directly *)
                  (* If instance doesn't exist yet, create it (companion may be called before primary) *)
                  let () =
                    if not (checkMemExists env user_inst_name || Env.checkConstantExists env user_inst_name) then
                      let _ = Env.addVar env unify user_inst_name ctx_t Inst loc in
                      ()
                  in
                  user_inst_name
                | None ->
                  (* Find the instance that was created for the parent generic call *)
                  (* We need to use the same instance name pattern as the parent *)
                  let number =
                    Printf.sprintf
                      "%.2x%.2x"
                      (0xFF land Hashtbl.hash (path_string specialized_def.name))
                      (0xFF land Hashtbl.hash (path_string (Env.getContext env)))
                  in
                  let rec findInstance n =
                    if n < 0 then
                      failwith "Could not find instance for companion call"
                    else
                      let name = "inst_" ^ string_of_int n ^ number in
                      if checkMemExists env name then
                        name
                      else
                        findInstance (n - 1)
                  in
                  findInstance (Env.getFunctionTick env)
              in
              let ctx_e = { e = EId context_name; t = current_ctx_t; loc } in
              let inst_e = { e = EMember (ctx_e, inst_name); t = ctx_t; loc } in
              inst_e :: processed_args
            | _ ->
              (* No context needed *)
              processed_args
          in
          (* Convert to ECall using the companion's path *)
          { e = ECall { instance = None; path = companion_def.name; args = final_args }; t = e.t; loc })))
  | ETypeIntrinsic { intrinsic; type_param } ->
    (* Type intrinsics should have been resolved during stmt_with_type_substitution.
       If we see one here, it's an error - the intrinsic was used outside a generic function context. *)
    let intrinsic_name =
      match intrinsic with
      | TypeDefault -> "typedefault"
      | TypeMax -> "typemax"
      | TypeMin -> "typemin"
    in
    Error.raiseError
      (Printf.sprintf
         "Type intrinsic '%s('%s)' was not resolved - it must be used inside a generic function"
         intrinsic_name
         type_param)
      e.loc
  | EUnit | EBool _ | EInt _ | EReal _ | EFixed _ | EString _ | EId _ | EConst _ -> e


(* Process a statement, recursively processing expressions.
   Mutually recursive with process_exp_instantiation to handle nested generic calls. *)
and process_stmt_instantiation (iargs : Args.args) (env : env) (state : instantiation_state) (s : Typed.stmt) :
    Typed.stmt =
  match s.s with
  | StmtVal d -> { s with s = StmtVal d }
  | StmtMem (d, tags) ->
    (* StmtMem has dexp and tags, no init expression *)
    { s with s = StmtMem (d, tags) }
  | StmtBind (lhs, rhs) ->
    let rhs = process_exp_instantiation iargs env state rhs in
    { s with s = StmtBind (lhs, rhs) }
  | StmtReturn e ->
    let e = process_exp_instantiation iargs env state e in
    { s with s = StmtReturn e }
  | StmtIf (cond, then_, else_opt) ->
    let cond = process_exp_instantiation iargs env state cond in
    let then_ = process_stmt_instantiation iargs env state then_ in
    let else_opt = Option.map (process_stmt_instantiation iargs env state) else_opt in
    { s with s = StmtIf (cond, then_, else_opt) }
  | StmtWhile (cond, body) ->
    let cond = process_exp_instantiation iargs env state cond in
    let body = process_stmt_instantiation iargs env state body in
    { s with s = StmtWhile (cond, body) }
  | StmtBlock stmts ->
    let stmts = CCList.map (process_stmt_instantiation iargs env state) stmts in
    { s with s = StmtBlock stmts }


(* Process a function definition, including the 'next' chain for 'and' functions *)
let rec process_function_def (iargs : Args.args) (env : env) (state : instantiation_state) (def : Typed.function_def)
    (body : Typed.stmt) : Typed.function_def * Typed.stmt =
  (* Re-enter the function context so addContextArg can add instance variables *)
  let env = Env.reenterFunction env def.name in
  (* Pre-scan the body to find all EGenCall nodes and trigger their instantiation.
     This ensures that companion function calls (EGenCompanionCall) can find their parent's
     instantiation even if the companion is called BEFORE the primary generic function. *)
  let () = prescan_generic_calls_in_stmt iargs env state body in
  let body = process_stmt_instantiation iargs env state body in
  (* Check if function already had _ctx before processing *)
  let had_ctx_before =
    match def.args with
    | { name; _ } :: _ when String.equal name context_name -> true
    | _ -> false
  in
  (* After processing the body, instances may have been added to the context.
     Re-apply insertContextArgument to add _ctx if needed. *)
  let def = insertContextArgument env def in
  (* Track if this function now needs context (didn't have it before, has it now) *)
  let () =
    if not had_ctx_before then
      match def.args with
      | { name; t = ctx_t; _ } :: _ when String.equal name context_name ->
        (* Function was updated to need context - track it *)
        let func_path_str = path_string def.name in
        Hashtbl.replace state.functions_needing_context func_path_str ctx_t
      | _ -> ()
  in
  let next =
    match def.next with
    | None -> None
    | Some (next_def, next_body) ->
      let next_def, next_body = process_function_def iargs env state next_def next_body in
      Some (next_def, next_body)
  in
  { def with next }, body


(* Process a top statement *)
let process_top_stmt_instantiation (iargs : Args.args) (env : env) (state : instantiation_state) (t : Typed.top_stmt) :
    Typed.top_stmt =
  match t.top with
  | TopFunction (def, body) ->
    let def, body = process_function_def iargs env state def body in
    { t with top = TopFunction (def, body) }
  | TopGenericPlaceholder _ -> t (* Pass through - will be replaced in second pass *)
  | TopExternal _ | TopType _ | TopEnum _ | TopConstant _ | TopAlias _ -> t


(* Transform EGenCall to ECall in a module's statements.
   Uses two passes to handle functions that gain context arguments:
   Pass 1: Process all functions, collecting which ones need context
   Pass 2: Update ECall nodes to add context arguments where needed *)
let transform_module_generics (iargs : Args.args) (env : env) (state : instantiation_state)
    (stmts : Typed.top_stmt list) : Typed.top_stmt list =
  (* Pass 1: Process all functions - this populates functions_needing_context *)
  let stmts = CCList.map (process_top_stmt_instantiation iargs env state) stmts in
  (* Pass 2: Update ECall nodes for functions that now need context *)
  CCList.map (process_top_stmt_instantiation iargs env state) stmts


(* Replace placeholders with their specialized functions *)
let replace_placeholders_in_module (state : instantiation_state) (module_name : string) (stmts : Typed.top_stmt list) :
    Typed.top_stmt list =
  CCList.flat_map
    (fun (stmt : Typed.top_stmt) ->
      match stmt.top with
      | TopGenericPlaceholder generic_name ->
        (* Find all specializations for this generic in this module *)
        let for_this_generic, remaining =
          CCList.partition
            (fun (m, gname, _, _) -> String.equal m module_name && String.equal gname generic_name)
            state.pending_functions
        in
        state.pending_functions <- remaining;
        CCList.map
          (fun (_, _, def, body) -> { top = TopFunction (def, body); loc = def.loc })
          (CCList.rev for_this_generic)
      | _ -> [ stmt ])
    stmts


(* ========== End Generic Function Instantiation ========== *)

let infer_single (iargs : Args.args) (env : env) (h : Parse.parsed_file) : env * top_stmt list =
  let set = createExistingTypeSet (createTypes env) in
  let env = Env.enterModule env h.name in
  let env, stmt = top_stmt_list iargs env h.stmts in
  let env = Env.exitModule env in
  let types = removeExistingTypes set (createTypes env) in
  env, stmt @ types


let infer (iargs : Args.args) (parsed : Parse.parsed_file list) : env * top_stmt list =
  (* Phase 1: Process all modules to build the environment and create EGenCall nodes *)
  let env, module_stmts =
    CCList.fold_left
      (fun (env, acc) (h : Parse.parsed_file) ->
        let env = Env.enterModule env h.name in
        let env, stmt = top_stmt_list iargs env h.stmts in
        let env = Env.exitModule env in
        (* Keep track of (module_name, stmts) pairs for phase 2 *)
        env, (h.name, stmt) :: acc)
      (Env.empty (), [])
      parsed
  in
  let module_stmts = CCList.rev module_stmts in
  (* Phase 2: Instantiate generics - two passes *)
  let instantiation_state = create_instantiation_state () in
  (* Pass 1: Transform all EGenCall to ECall across all modules, collecting specialized functions *)
  let transformed_stmts =
    CCList.map
      (fun (module_name, stmts) ->
        let env = Env.enterModule env module_name in
        let stmts = transform_module_generics iargs env instantiation_state stmts in
        let _ = Env.exitModule env in
        module_name, stmts)
      module_stmts
  in
  (* Pass 2: Replace placeholders with specialized functions, preserving original order *)
  let final_stmts =
    CCList.fold_left
      (fun acc (module_name, stmts) ->
        let stmts = replace_placeholders_in_module instantiation_state module_name stmts in
        stmts @ acc)
      []
      transformed_stmts
  in
  let types = createTypes env in
  env, types @ CCList.rev final_stmts
