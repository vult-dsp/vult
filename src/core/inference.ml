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


(* Convert typed expression to syntax expression for substitution *)
let typed_exp_to_syntax_exp (typed_exp : Typed.exp) : Syntax.exp =
  let loc = typed_exp.loc in
  match typed_exp.e with
  | EInt i -> { e = SEInt (string_of_int i); loc }
  | EReal r -> { e = SEReal (string_of_float r); loc }
  | EBool b -> { e = SEBool b; loc }
  | EString s -> { e = SEString s; loc }
  | EConst path -> { e = SEId path.id; loc } (* Constant references become identifiers *)
  | _ -> failwith ("Unsupported typed expression for syntax conversion: " ^ Pla.print (print_exp typed_exp))


(* Substitute generic parameters in syntax expressions *)
let rec substitute_exp (string_substitutions : (string * string) list)
    (const_substitutions : (string * Syntax.exp) list) (exp : Syntax.exp) : Syntax.exp =
  match exp.e with
  | SEId id -> (
    (* Check if this identifier should be substituted with a string (function name) *)
    try
      let replacement = CCList.assoc ~eq:String.equal id string_substitutions in
      { exp with e = SEId replacement }
    with
    | Not_found -> (
      (* Check if this identifier should be substituted with a constant expression *)
      try
        let const_exp = CCList.assoc ~eq:String.equal id const_substitutions in
        const_exp (* Replace the identifier with the constant expression *)
      with
      | Not_found -> exp))
  | SECall { instance; path; args } ->
    let substituted_args = CCList.map (substitute_exp string_substitutions const_substitutions) args in
    (* Also substitute the function name in the path if it matches *)
    let substituted_path =
      match path with
      | { id = func_id; n = None; loc } -> (
        try
          let replacement = CCList.assoc ~eq:String.equal func_id string_substitutions in
          { Pparser.Syntax.id = replacement; n = None; loc }
        with
        | Not_found -> path)
      | _ -> path (* Don't substitute module-qualified paths *)
    in
    { exp with e = SECall { instance; path = substituted_path; args = substituted_args } }
  | SEOp (op, e1, e2) ->
    let e1' = substitute_exp string_substitutions const_substitutions e1 in
    let e2' = substitute_exp string_substitutions const_substitutions e2 in
    { exp with e = SEOp (op, e1', e2') }
  | SEUnOp (op, e) ->
    let e' = substitute_exp string_substitutions const_substitutions e in
    { exp with e = SEUnOp (op, e') }
  | SEIf { cond; then_; else_ } ->
    let cond' = substitute_exp string_substitutions const_substitutions cond in
    let then_' = substitute_exp string_substitutions const_substitutions then_ in
    let else_' = substitute_exp string_substitutions const_substitutions else_ in
    { exp with e = SEIf { cond = cond'; then_ = then_'; else_ = else_' } }
  | SETuple exps ->
    let exps' = CCList.map (substitute_exp string_substitutions const_substitutions) exps in
    { exp with e = SETuple exps' }
  | SEArray exps ->
    let exps' = CCList.map (substitute_exp string_substitutions const_substitutions) exps in
    { exp with e = SEArray exps' }
  | SEIndex { e; index } ->
    let e' = substitute_exp string_substitutions const_substitutions e in
    let index' = substitute_exp string_substitutions const_substitutions index in
    { exp with e = SEIndex { e = e'; index = index' } }
  | SEGroup e ->
    let e' = substitute_exp string_substitutions const_substitutions e in
    { exp with e = SEGroup e' }
  | SERecord { path; elems } ->
    let elems' =
      CCList.map (fun (name, exp) -> name, substitute_exp string_substitutions const_substitutions exp) elems
    in
    { exp with e = SERecord { path; elems = elems' } }
  | SEMember (e, member) ->
    let e' = substitute_exp string_substitutions const_substitutions e in
    { exp with e = SEMember (e', member) }
  | _ -> exp (* Literals and other expressions that don't contain identifiers *)


(* Substitute generic parameters in syntax statements *)
let rec substitute_stmt (string_substitutions : (string * string) list)
    (const_substitutions : (string * Syntax.exp) list) (stmt : Syntax.stmt) : Syntax.stmt =
  match stmt.s with
  | SStmtReturn exp ->
    let exp' = substitute_exp string_substitutions const_substitutions exp in
    { stmt with s = SStmtReturn exp' }
  | SStmtBind (lexp, exp) ->
    let exp' = substitute_exp string_substitutions const_substitutions exp in
    { stmt with s = SStmtBind (lexp, exp') }
  | SStmtIf (cond, then_stmt, else_stmt_opt) ->
    let cond' = substitute_exp string_substitutions const_substitutions cond in
    let then_stmt' = substitute_stmt string_substitutions const_substitutions then_stmt in
    let else_stmt_opt' = Option.map (substitute_stmt string_substitutions const_substitutions) else_stmt_opt in
    { stmt with s = SStmtIf (cond', then_stmt', else_stmt_opt') }
  | SStmtWhile (cond, body) ->
    let cond' = substitute_exp string_substitutions const_substitutions cond in
    let body' = substitute_stmt string_substitutions const_substitutions body in
    { stmt with s = SStmtWhile (cond', body') }
  | SStmtBlock stmts ->
    let stmts' = CCList.map (substitute_stmt string_substitutions const_substitutions) stmts in
    { stmt with s = SStmtBlock stmts' }
  | _ -> stmt (* Other statements that don't contain expressions *)


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
    CCList.iter2
      (fun (arg : arg) (exp : exp) ->
        if isTypeConst arg.t = false then
          markExpMutable env exp loc)
      args
      exp_args


(* Template instantiation helper functions *)
let generate_specialized_name (generic_name : string) (bindings : (string * Typed.generic_binding) list) : string =
  let binding_suffix =
    bindings
    |> CCList.filter_map (fun (_, binding) ->
           match binding with
           | Typed.BindFunction (func_name, _) -> Some func_name
           | Typed.BindType _ -> Some "T" (* Simple type suffix for now *)
           | Typed.BindConstant (_, _) -> Some "C" (* Simple constant suffix for now *)
           | Typed.BindNonSpecializable -> None (* Skip non-specializable bindings *))
    |> String.concat "_"
  in
  generic_name ^ "__" ^ binding_suffix ^ "_"


(* Bind generic parameters to processed expressions (after inference) *)
let bind_generic_arguments_from_exps (_ : env) (generic_params : Typed.generic_param list) (args : Typed.exp list) :
    (string * Typed.generic_binding) list =
  (* Validate argument count *)
  let param_count = CCList.length generic_params in
  let arg_count = CCList.length args in
  if arg_count < param_count then
    Error.raiseError
      (Printf.sprintf "Template requires %d parameters but only %d were provided" param_count arg_count)
      Loc.default;
  let rec loop params args acc =
    match params, args with
    | [], _ -> List.rev acc
    | _, [] -> List.rev acc (* This case should not happen due to validation above *)
    | Typed.GParamFunction (name, expected_type) :: params_rest, { e = EId func_name; t; loc } :: args_rest ->
      (* Validate function parameter type if expected type is specified *)
      (match expected_type with
      | Some expected_t ->
        if not (unify expected_t t) then
          Error.raiseError
            (Printf.sprintf
               "Generic function parameter '%s' expected type %s but got %s"
               name
               (Pla.print (Typed.print_type_ expected_t))
               (Pla.print (Typed.print_type_ t)))
            loc
      | None -> ());
      let binding = Typed.BindFunction (func_name, t) in
      loop params_rest args_rest ((name, binding) :: acc)
    | Typed.GParamFunction (name, _) :: _, exp :: _ ->
      Error.raiseError
        (Printf.sprintf
           "Generic function parameter '%s' must be a function identifier, but got %s"
           name
           (Pla.print (Typed.print_exp exp)))
        exp.loc
    | Typed.GParamType name :: params_rest, { t; _ } :: args_rest ->
      (* Type parameters - bind to the inferred type *)
      let binding = Typed.BindType t in
      loop params_rest args_rest ((name, binding) :: acc)
    | Typed.GParamConstant (name, expected_type) :: params_rest, exp :: args_rest -> (
      (* Validate constant parameter type *)
      if not (unify expected_type exp.t) then
        Error.raiseError
          (Printf.sprintf
             "Generic constant parameter '%s' expected type %s but got %s"
             name
             (Pla.print (Typed.print_type_ expected_type))
             (Pla.print (Typed.print_type_ exp.t)))
          exp.loc;
      (* Check if expression is actually constant *)
      match exp.e with
      | EInt _ | EReal _ | EBool _ | EString _ | EConst _ ->
        (* Constant expression - can specialize *)
        let binding = Typed.BindConstant (exp, exp.t) in
        loop params_rest args_rest ((name, binding) :: acc)
      | _ ->
        (* Non-constant expression - mark as non-specializable *)
        (* We'll return a special binding to indicate this generic cannot be specialized *)
        let binding = Typed.BindNonSpecializable in
        loop params_rest args_rest ((name, binding) :: acc))
  in
  loop generic_params args []


let bind_generic_arguments (_ : env) (generic_params : Typed.generic_param list) (args : Syntax.exp list) :
    (string * Typed.generic_binding) list =
  (* For now, implement a simple binding for function parameters *)
  let rec loop params args acc =
    match params, args with
    | [], _ -> List.rev acc
    | _, [] -> List.rev acc (* Not enough arguments - will be caught later *)
    | Typed.GParamFunction (name, _) :: params_rest, { Syntax.e = Syntax.SEId func_name; _ } :: args_rest ->
      (* Bind function parameter to function name *)
      let binding = Typed.BindFunction (func_name, C.noreturn Loc.default) in
      loop params_rest args_rest ((name, binding) :: acc)
    | Typed.GParamType name :: params_rest, _ :: args_rest ->
      (* Type parameters need type inference - skip for now *)
      let binding = Typed.BindType (C.noreturn Loc.default) in
      loop params_rest args_rest ((name, binding) :: acc)
    | Typed.GParamConstant (name, _) :: params_rest, _ :: args_rest ->
      (* Constant parameters - skip for now *)
      let binding =
        Typed.BindConstant ({ e = EUnit; t = C.noreturn Loc.default; loc = Loc.default }, C.noreturn Loc.default)
      in
      loop params_rest args_rest ((name, binding) :: acc)
    | _ :: params_rest, _ :: args_rest ->
      (* Skip unhandled cases for now *)
      loop params_rest args_rest acc
  in
  loop generic_params args []


(* Convert constant expressions to constant_value *)
let extract_constant_value (exp : Typed.exp) : Typed.constant_value =
  match exp.e with
  | EInt i -> IntConstant i
  | EReal r -> RealConstant r
  | EBool b -> BoolConstant b
  | EString s -> StringConstant s
  | EConst _ ->
    (* For constant references, we need to evaluate them *)
    Error.raiseError
      (Printf.sprintf
         "Generic constant parameter must be a literal value, not a constant reference '%s'"
         (Pla.print (Typed.print_exp exp)))
      exp.loc
  | _ ->
    Error.raiseError
      (Printf.sprintf
         "Generic constant parameter must be a literal value (int, real, bool, or string), got %s"
         (Pla.print (Typed.print_exp exp)))
      exp.loc


(* Convert generic bindings to generic_param_binding list *)
let convert_generic_bindings (bindings : (string * Typed.generic_binding) list) : Typed.generic_param_binding list =
  CCList.filter_map
    (fun (name, binding) ->
      match binding with
      | BindFunction (func_name, function_type) -> (
        (* Extract function signature from TEFunction type *)
        match (unlink function_type).tx with
        | TEFunction (arg_types, return_type) ->
          let binding_value = Typed.FunctionBinding { func_name; arg_types; return_type } in
          Some { param_name = name; binding = binding_value }
        | _ ->
          Error.raiseError
            (Printf.sprintf
               "Generic function parameter '%s' has invalid function type: %s"
               name
               (Pla.print (Typed.print_type_ function_type)))
            Loc.default)
      | BindType t ->
        (* Validate that the type is well-formed *)
        (match (unlink t).tx with
        | TEUnbound _ ->
          Error.raiseError (Printf.sprintf "Template type parameter '%s' cannot be unbound" name) Loc.default
        | _ -> ());
        let binding_value = Typed.TypeBinding t in
        Some { param_name = name; binding = binding_value }
      | BindConstant (exp, t) ->
        let value = extract_constant_value exp in
        let binding_value = Typed.ConstantBinding { value; value_type = t } in
        Some { param_name = name; binding = binding_value }
      | BindNonSpecializable ->
        (* Skip non-specializable parameters - they should not be included in the signature *)
        None)
    bindings


(* Extract function argument signatures from processed function arguments *)
let extract_function_arg_signatures (_generic_func : Typed.generic_function) (processed_function_args : Typed.exp list)
    : Typed.function_arg_signature list =
  (* Extract actual types from processed function arguments *)
  CCList.mapi
    (fun i (exp_arg : Typed.exp) ->
      let param_name = "arg" ^ string_of_int i in
      let arg_type = (exp_arg.t : Typed.type_) in
      ({ param_name; arg_type } : Typed.function_arg_signature))
    processed_function_args


(* Build complete instantiation signature *)
let build_instantiation_signature (generic_func : Typed.generic_function)
    (bindings : (string * Typed.generic_binding) list) (processed_function_args : Typed.exp list)
    (return_type : Typed.type_) : Typed.instantiation_signature =
  { generic_name = generic_func.name
  ; generic_params = convert_generic_bindings bindings
  ; function_args = extract_function_arg_signatures generic_func processed_function_args
  ; return_type
  }


(* Convert type to mangled name for specialized function names *)
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
  | TEUnbound _ -> "unbound"
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


(* Generate specialized function name from signature *)
let signature_to_specialized_name (signature : Typed.instantiation_signature) : string =
  let base_name = signature.generic_name in
  (* Create a simple, clean name using just the essential information *)
  let generic_part =
    CCList.map
      (fun param ->
        match param.binding with
        | FunctionBinding f ->
          (* Just use the function name - much cleaner *)
          f.func_name
        | TypeBinding t -> type_to_mangled_name t
        | ConstantBinding c -> (
          match c.value with
          | IntConstant i -> string_of_int i
          | RealConstant r ->
            Printf.sprintf "%.0f" r
            |> String.map (function
                 | '.' -> 'p'
                 | c -> c)
          | BoolConstant b -> string_of_bool b
          | StringConstant s ->
            String.map
              (function
                | ' ' -> '_'
                | c -> c)
              s))
      signature.generic_params
    |> String.concat "_"
  in
  (* Use only the essential argument types - and try to get concrete types *)
  let args_part =
    CCList.map
      (fun arg ->
        let simplified_type =
          match (unlink arg.arg_type).tx with
          | TEOption [ single_type ] -> single_type (* Use the concrete type from option *)
          | _ -> arg.arg_type
        in
        type_to_mangled_name simplified_type)
      signature.function_args
    |> String.concat "_"
  in
  (* Create a much cleaner name *)
  if generic_part = "" then
    base_name ^ "_" ^ args_part
  else
    base_name ^ "_" ^ generic_part ^ "_" ^ args_part


(* Add context argument for specialized generic functions that have state *)
let addContextArgForSpecialized (env : env) (specialized_name : string) args loc =
  let m = Env.getCurrentModule env in
  (* The specialized function's context type name *)
  let specialized_type_name = specialized_name ^ "_type" in
  let fpath : Pparser.Syntax.path = { id = specialized_type_name; n = Some m.name; loc } in
  let fctx_t = C.path_t loc fpath in
  (* Get the current function's context type *)
  let cpath = Env.getContext env in
  let ctx_t =
    let f = Env.getCurrentFunction env in
    match Env.lookVarInScopes f.locals context_name with
    | Some var -> var.t
    | None -> failwith "context var not declared in addContextArgForSpecialized"
  in
  (* Generate unique instance name *)
  let number =
    Printf.sprintf "%.2x%.2x" (0xFF land Hashtbl.hash (path_string fpath)) (0xFF land Hashtbl.hash (path_string cpath))
  in
  let rec generateName () =
    let n = Env.getFunctionTick env in
    let name = "inst_" ^ string_of_int n ^ number in
    if checkMemExists env name || Env.checkConstantExists env name then
      generateName ()
    else
      name
  in
  let inst_name = generateName () in
  let env = Env.addVar env unify inst_name fctx_t Inst loc in
  let e = { e = EMember ({ e = EId context_name; t = ctx_t; loc }, inst_name); loc; t = fctx_t } in
  (* Mark the context expression as mutable since specialized functions with state modify it *)
  let () = markExpMutable env e loc in
  env, e :: args


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
  let path_string = Env.pathString path in
  match Env.lookupGeneric env path_string with
  | Some generic_func ->
    (* This is a generic call - handle instantiation *)
    (* NOTE: Don't process args with exp_list yet - generic_call will handle them *)
    generic_call env generic_func args loc eloc
  | None ->
    (* Regular function call *)
    let env, args = exp_list env args in
    let f = Env.lookFunctionCall env path loc in
    let args_t, ret = f.t in
    let t = applyFunction eloc args_t ret args in
    let () = propagateVariability env loc f.args args in
    let env, args = addContextArg env instance f args loc in
    env, { e = ECall { instance = None; path = f.path; args }; t; loc }


(* Create a non-specialized version of a generic function where generic parameters become regular parameters *)
and create_non_specialized_function (env : env) (generic_func : Typed.generic_function) (eloc : Loc.t) : env =
  let non_specialized_name = generic_func.name in
  (* Check if we already generated this non-specialized version *)
  let generic_path : Typed.path = { id = non_specialized_name; n = None; loc = eloc } in
  match Env.findFunction (Env.lookupPath env generic_path) with
  | Some _ -> env (* Already exists *)
  | None ->
    (* Need to generate the non-specialized function *)
    (* Convert generic parameters to regular function arguments *)
    let generic_param_args =
      CCList.map
        (fun param ->
          match param with
          | Typed.GParamFunction (name, Some t) -> { Typed.name; t; loc = eloc }
          | Typed.GParamFunction (name, None) -> { Typed.name; t = C.unbound eloc; loc = eloc }
          | Typed.GParamType name -> { Typed.name; t = C.unbound eloc; loc = eloc }
          | Typed.GParamConstant (name, t) -> { Typed.name; t; loc = eloc })
        generic_func.generic_params
    in
    (* Combine generic parameters with regular arguments *)
    let all_args = generic_param_args @ generic_func.args in
    (* Create the function definition *)
    let func_def =
      { Typed.name = generic_path
      ; args = all_args
      ; t = CCList.map (fun (arg : Typed.arg) -> arg.t) all_args, snd generic_func.t
      ; loc = eloc
      ; tags = generic_func.tags
      ; next = None
      ; is_root = false
      }
    in
    (* Add to pending injections with empty substitutions (no substitution needed) *)
    (* Non-specialized functions don't have concrete type bindings *)
    let env = Env.addPendingInjection env (func_def, generic_func.body, ([], []), []) in
    env


(* Create a specialized function with fresh types by re-processing the original syntax *)
and create_specialized_function (_env : env) (generic_func : Typed.generic_function)
    (bindings : (string * Typed.generic_binding) list) (_processed_function_args : Typed.exp list)
    (_inferred_ret : Typed.type_) (specialized_name : string) (eloc : Loc.t) :
    Typed.function_def * Pparser.Syntax.stmt * ((string * string) list * (string * Syntax.exp) list) =
  (* Get the original generic function's types and create fresh copies preserving sharing *)
  (* CRITICAL: We must copy arg types AND return type together so that if the same type variable *)
  (* appears in both (e.g., 't in array('t, 3) and return type 't), they map to the SAME fresh unbound *)
  let original_arg_types, original_ret_type = generic_func.t in
  let all_original_types = original_arg_types @ [ original_ret_type ] in
  let all_fresh_types = Typed.copy_types_preserving_sharing all_original_types in
  (* Split back into arg types and return type *)
  let fresh_arg_types, fresh_ret_type =
    match CCList.rev all_fresh_types with
    | last :: rest -> CCList.rev rest, last
    | [] -> failwith "copy_types_preserving_sharing returned empty list"
  in
  let specialized_type = fresh_arg_types, fresh_ret_type in
  (* Create fresh argument list with specialized types *)
  let fresh_args =
    CCList.map2
      (fun (original_arg : Typed.arg) (specialized_type : Typed.type_) -> { original_arg with t = specialized_type })
      (generic_func.args : Typed.arg list)
      fresh_arg_types
  in
  (* Create substitution maps for different types of bindings *)
  let function_substitutions =
    CCList.filter_map
      (fun (param_name, binding) ->
        match binding with
        | Typed.BindFunction (func_name, _) -> Some (param_name, func_name)
        | _ -> None)
      bindings
  in
  let constant_substitutions =
    CCList.filter_map
      (fun (param_name, binding) ->
        match binding with
        | Typed.BindConstant (const_exp, _) ->
          (* Convert typed expression to syntax expression for substitution *)
          let const_syntax = typed_exp_to_syntax_exp const_exp in
          Some (param_name, const_syntax)
        | _ -> None)
      bindings
  in
  (* Create specialized function definition *)
  let specialized_path = { Pparser.Syntax.id = specialized_name; n = None; loc = eloc } in
  let specialized_def =
    { name = specialized_path
    ; args = fresh_args
    ; t = specialized_type
    ; loc = eloc
    ; tags = generic_func.tags
    ; next = None
    ; is_root = false
    }
  in
  (* Return the specialized definition, original syntax body, and substitutions for deferred processing *)
  specialized_def, generic_func.body, (function_substitutions, constant_substitutions)


(* Bind mixed generic parameters - explicit args for explicit params, inferred types for implicit type params *)
(* Now takes generic function's arg types to extract type bindings via unification *)
and bind_mixed_generic_arguments (env : env) (generic_params : Typed.generic_param list)
    (explicit_generic_args : Typed.exp list) (function_args : Typed.exp list)
    (generic_func_arg_types : Typed.type_ list) : (string * Typed.generic_binding) list =
  let rec bind_params params explicit_args acc =
    match params, explicit_args with
    | [], _ -> acc (* No more parameters to bind *)
    | Typed.GParamFunction (name, _) :: rest_params, { e = EConst path; _ } :: rest_args ->
      (* Function parameter bound to function name *)
      (* Look up the actual function type *)
      let func_type =
        try
          let func = Env.lookFunctionCall env path path.loc in
          let args_t, ret_t = func.t in
          { tx = TEFunction (args_t, ret_t); const = C.const (); loc = Loc.default }
        with
        | _ -> { tx = TEFunction ([], C.unit ~loc:Loc.default); const = C.const (); loc = Loc.default }
      in
      let binding = Typed.BindFunction (path.id, func_type) in
      bind_params rest_params rest_args ((name, binding) :: acc)
    | Typed.GParamFunction (name, _) :: rest_params, { e = EId func_name; _ } :: rest_args ->
      (* Function parameter bound to identifier *)
      (* Look up the actual function type *)
      let func_path = { Pparser.Syntax.id = func_name; n = None; loc = Loc.default } in
      let func_type =
        try
          let func = Env.lookFunctionCall env func_path Loc.default in
          let args_t, ret_t = func.t in
          { tx = TEFunction (args_t, ret_t); const = C.const (); loc = Loc.default }
        with
        | _ -> { tx = TEFunction ([], C.unit ~loc:Loc.default); const = C.const (); loc = Loc.default }
      in
      let binding = Typed.BindFunction (func_name, func_type) in
      bind_params rest_params rest_args ((name, binding) :: acc)
    | Typed.GParamConstant (name, expected_type) :: rest_params, exp :: rest_args ->
      (* Constant parameter - validate and bind *)
      if not (unify expected_type exp.t) then
        Error.raiseError
          (Printf.sprintf
             "Generic constant parameter '%s' expected type %s but got %s"
             name
             (Pla.print (Typed.print_type_ expected_type))
             (Pla.print (Typed.print_type_ exp.t)))
          exp.loc;
      (* Check if this is a literal value that can be specialized *)
      let binding =
        match exp.e with
        | EInt _ | EReal _ | EBool _ | EString _ ->
          (* Literal value - can be specialized *)
          Typed.BindConstant (exp, exp.t)
        | _ ->
          (* Non-literal value - cannot be specialized *)
          Typed.BindNonSpecializable
      in
      bind_params rest_params rest_args ((name, binding) :: acc)
    | Typed.GParamType _ :: _, _ ->
      (* All remaining params are implicit type parameters *)
      (* Use unification-based binding extraction for correct handling of composed types *)
      (* First, collect all implicit type parameters *)
      let rec collect_type_params params =
        match params with
        | Typed.GParamType name :: rest -> name :: collect_type_params rest
        | _ :: rest -> collect_type_params rest
        | [] -> []
      in
      let type_param_names = collect_type_params params in
      (* Find unbounds in the generic function's arg types (in order of first appearance) *)
      let original_unbounds = Typed.find_unbounds_in_types generic_func_arg_types in
      (* Copy the generic function's arg types with unbound mapping *)
      let fresh_arg_types, unbound_mapping = Typed.copy_types_with_unbound_mapping generic_func_arg_types in
      (* Unify fresh arg types with actual function arg types *)
      let () =
        CCList.iter2
          (fun fresh_t (arg : Typed.exp) ->
            let _ = unify fresh_t arg.t in
            ())
          fresh_arg_types
          function_args
      in
      (* For each type param name, find the corresponding concrete type *)
      let bindings =
        CCList.mapi
          (fun i name ->
            let binding =
              (* Find the original unbound at position i *)
              match CCList.nth_opt original_unbounds i with
              | Some orig_unbound -> (
                (* Find the fresh unbound that corresponds to this original *)
                match CCList.find_opt (fun (orig, _) -> orig == orig_unbound) unbound_mapping with
                | Some (_, fresh_unbound) ->
                  (* Get what the fresh unbound is now linked to after unification *)
                  Typed.BindType (unlink fresh_unbound)
                | None -> (
                  (* Fallback: use the original approach if no mapping found *)
                  match CCList.nth_opt function_args i with
                  | Some arg -> Typed.BindType arg.t
                  | None -> Typed.BindType (C.unbound Loc.default)))
              | None -> (
                (* No unbound at this position - use original approach *)
                match CCList.nth_opt function_args i with
                | Some arg -> Typed.BindType arg.t
                | None -> Typed.BindType (C.unbound Loc.default))
            in
            name, binding)
          type_param_names
      in
      List.rev_append bindings acc
    | param :: rest_params, [] -> (
      (* No more explicit arguments for explicit params *)
      match param with
      | Typed.GParamType _ ->
        (* This case should be handled above, but include for safety *)
        bind_params rest_params [] acc
      | _ ->
        (* Missing explicit argument for explicit parameter *)
        bind_params rest_params [] acc)
    | _ :: rest_params, _ :: rest_args ->
      (* Skip unsupported parameter types for now *)
      bind_params rest_params rest_args acc
  in
  List.rev (bind_params generic_params explicit_generic_args [])


and generic_call (env : env) (generic_func : Typed.generic_function) (args : Syntax.exp list) (_ : Loc.t) (eloc : Loc.t)
    : env * exp =
  (* Count only explicit generic parameters (exclude implicit type parameters) *)
  let explicit_generic_param_count =
    CCList.count
      (function
        | Typed.GParamType _ -> false (* Implicit type parameters - inferred from function args *)
        | _ -> true (* Explicit parameters - require explicit arguments *))
      generic_func.generic_params
  in
  let function_param_count = CCList.length generic_func.args in
  let total_expected = explicit_generic_param_count + function_param_count in
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
  (* Split arguments into explicit generic parameters and regular function arguments *)
  let explicit_generic_args = CCList.take explicit_generic_param_count args in
  let function_args = CCList.drop explicit_generic_param_count args in
  (* Process explicit template arguments with template argument context (allows function references) *)
  let env, processed_explicit_generic_args = exp_list ~context:generic_arg_context env explicit_generic_args in
  (* Process regular function arguments with normal context *)
  let env, processed_function_args = exp_list ~context:normal_context env function_args in
  (* Bind generic parameters to their values - handle both explicit and implicit parameters *)
  (* Pass generic function's arg types for unification-based type binding extraction *)
  let generic_func_arg_types, _ = generic_func.t in
  let bindings =
    bind_mixed_generic_arguments
      env
      generic_func.generic_params
      processed_explicit_generic_args
      processed_function_args
      generic_func_arg_types
  in
  (* Check if this call can be specialized *)
  let can_specialize =
    not
      (CCList.exists
         (fun (_, binding) ->
           match binding with
           | Typed.BindNonSpecializable -> true
           | _ -> false)
         bindings)
  in
  (* Infer the return type based on generic parameter bindings *)
  let inferred_ret =
    let original_ret = generic_func.t |> snd in
    match (unlink original_ret).tx with
    | TEUnbound _ -> (
      (* If the original return type is unbound, try to infer it from the function parameters *)
      match generic_func.generic_params with
      | [ GParamFunction (param_name, _) ] -> (
        (* For single function parameter templates, use the function's return type *)
        match CCList.find_opt (fun (name, _) -> name = param_name) bindings with
        | Some (_, BindFunction (_, function_type)) -> (
          (* Extract return type from function type *)
          match (unlink function_type).tx with
          | TEFunction (_, ret_type) -> ret_type
          | _ -> original_ret)
        | _ -> original_ret)
      | _ -> original_ret)
    | _ -> original_ret (* Use the explicit return type if specified *)
  in
  if not can_specialize then
    (* Cannot specialize - generate non-specialized version *)
    (* For now, we'll generate a simple non-specialized version that takes all parameters *)
    let all_args = processed_explicit_generic_args @ processed_function_args in
    (* Create a call to a non-specialized version of the generic function *)
    let non_specialized_name = generic_func.name in
    let non_specialized_path = { Pparser.Syntax.id = non_specialized_name; n = None; loc = eloc } in
    (* Generate the non-specialized function if it doesn't exist *)
    let env = create_non_specialized_function env generic_func eloc in
    (* Create a call to the non-specialized function *)
    let args_t = CCList.map (fun (e : exp) -> e.t) all_args in
    let t = applyFunction eloc args_t inferred_ret all_args in
    env, { e = ECall { instance = None; path = non_specialized_path; args = all_args }; t; loc = eloc }
  else
    (* Can specialize - proceed with normal specialization *)
    (* Build complete instantiation signature *)
    let signature = build_instantiation_signature generic_func bindings processed_function_args inferred_ret in
    (* Generate specialized function name from signature *)
    let specialized_name = signature_to_specialized_name signature in
    (* Check if we already have this instantiation using signature-based lookup *)
    match Env.findInstantiation env signature with
    | Some instantiation ->
      (* Already instantiated - just call it *)
      let specialized_path = instantiation.specialized_def.name in
      let args_t, ret = instantiation.specialized_def.t in
      (* Check if the generic function has mem statements - if so, add context arg *)
      let has_state = syntax_has_mem generic_func.body in
      let env, call_args =
        if has_state then
          addContextArgForSpecialized env instantiation.specialized_name processed_function_args eloc
        else
          env, processed_function_args
      in
      let m = Env.getCurrentModule env in
      let call_args_t =
        if has_state then
          let specialized_type_name = instantiation.specialized_name ^ "_type" in
          let ctx_path : Pparser.Syntax.path = { id = specialized_type_name; n = Some m.name; loc = eloc } in
          C.path_t eloc ctx_path :: args_t
        else
          args_t
      in
      let t = applyFunction eloc call_args_t ret call_args in
      env, { e = ECall { instance = None; path = specialized_path; args = call_args }; t; loc = eloc }
    | None ->
      (* Create new instantiation with complete signature *)
      (* Create specialized function definition with fresh types *)
      (* Generate specialized function by re-processing the original syntax with fresh types *)
      let specialized_def, syntax_body, (function_substitutions, constant_substitutions) =
        create_specialized_function env generic_func bindings processed_function_args inferred_ret specialized_name eloc
      in
      let instantiation : Typed.generic_instantiation = { signature; specialized_name; bindings; specialized_def } in
      (* Register the instantiation *)
      let env = Env.addInstantiation env instantiation in
      (* Add the specialized function to pending injections *)
      (* Extract type bindings from the generic parameter bindings *)
      let type_bindings =
        CCList.filter_map
          (function
            | name, Typed.BindType t -> Some (name, t)
            | _ -> None)
          bindings
      in
      let env =
        Env.addPendingInjection
          env
          (specialized_def, syntax_body, (function_substitutions, constant_substitutions), type_bindings)
      in
      (* Check if the generic function has mem statements - if so, add context arg *)
      let has_state = syntax_has_mem generic_func.body in
      let env, call_args =
        if has_state then
          addContextArgForSpecialized env specialized_name processed_function_args eloc
        else
          env, processed_function_args
      in
      (* Return a call to the specialized function *)
      (* Use fresh argument types and unify them with processed function arguments *)
      let specialized_args_t, _ = specialized_def.t in
      (* First unify the fresh types with the actual processed argument types *)
      CCList.iter2
        (fun fresh_t (processed_arg : Typed.exp) -> unifyRaise processed_arg.loc fresh_t processed_arg.t)
        specialized_args_t
        processed_function_args;
      (* Build the call args type - include context type if function has state *)
      let m = Env.getCurrentModule env in
      let call_args_t =
        if has_state then
          let specialized_type_name = specialized_name ^ "_type" in
          let ctx_path : Pparser.Syntax.path = { id = specialized_type_name; n = Some m.name; loc = eloc } in
          C.path_t eloc ctx_path :: specialized_args_t
        else
          specialized_args_t
      in
      (* CRITICAL: Use the fresh return type from specialized_def.t, NOT inferred_ret *)
      (* inferred_ret is the ORIGINAL generic function's return type, and using it here *)
      (* would cause later unifications to pollute the original generic function's types *)
      let _, fresh_ret = specialized_def.t in
      let t = applyFunction eloc call_args_t fresh_ret call_args in
      env, { e = ECall { instance = None; path = specialized_def.name; args = call_args }; t; loc = eloc }


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
  stmt_generic env dexp_with_substitution return s


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
  match getContextArgument env def.name def.loc with
  | None -> def
  | Some arg ->
    let rec loop next =
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
    (* Validate that the type is a valid constant type *)
    (match (unlink converted_type).tx with
    | TEId { id = "int" | "real" | "bool" | "string"; _ } -> ()
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
  (* Check for name conflicts between generic parameters and function arguments *)
  let arg_names =
    CCList.map
      (fun arg ->
        let name, _, _ = arg in
        name)
      def.args
  in
  let conflicting_names = CCList.filter (fun name -> CCList.mem name arg_names) param_names in
  if conflicting_names <> [] then
    Error.raiseError
      (Printf.sprintf
         "Generic function '%s' has generic parameters with same names as function arguments: %s"
         def.name
         (String.concat ", " conflicting_names))
      def.loc;
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
  { name = def.name
  ; generic_params
  ; args
  ; t = arg_types, inferred_ret
  ; body = def.body
  ; loc = def.loc
  ; tags = def.tags
  }


(* Substitute generic parameters in statements *)
let substitute_generic_params (stmt : Syntax.stmt) (substitution_map : (string * string) list) : Syntax.stmt =
  let rec substitute_stmt (stmt : Syntax.stmt) : Syntax.stmt =
    match stmt.s with
    | SStmtError -> stmt
    | SStmtVal (dexp, exp_opt) ->
      let exp_opt = Option.map substitute_exp exp_opt in
      { stmt with s = SStmtVal (dexp, exp_opt) }
    | SStmtMem (dexp, exp_opt, tags) ->
      let exp_opt = Option.map substitute_exp exp_opt in
      { stmt with s = SStmtMem (dexp, exp_opt, tags) }
    | SStmtBind (lexp, exp) ->
      let exp = substitute_exp exp in
      { stmt with s = SStmtBind (lexp, exp) }
    | SStmtReturn exp ->
      let exp = substitute_exp exp in
      { stmt with s = SStmtReturn exp }
    | SStmtBlock stmts ->
      let stmts = CCList.map substitute_stmt stmts in
      { stmt with s = SStmtBlock stmts }
    | SStmtIf (cond, then_stmt, else_opt) ->
      let cond = substitute_exp cond in
      let then_stmt = substitute_stmt then_stmt in
      let else_opt = Option.map substitute_stmt else_opt in
      { stmt with s = SStmtIf (cond, then_stmt, else_opt) }
    | SStmtWhile (cond, body) ->
      let cond = substitute_exp cond in
      let body = substitute_stmt body in
      { stmt with s = SStmtWhile (cond, body) }
    | SStmtIter { id; value; body } ->
      let value = substitute_exp value in
      let body = substitute_stmt body in
      { stmt with s = SStmtIter { id; value; body } }
    | SStmtMatch { e; cases } ->
      let e = substitute_exp e in
      let cases = CCList.map (fun (pattern, stmt) -> pattern, substitute_stmt stmt) cases in
      { stmt with s = SStmtMatch { e; cases } }
  and substitute_exp (exp : Syntax.exp) : Syntax.exp =
    match exp.e with
    | SEBool _ | SEInt _ | SEReal _ | SEFixed _ | SEString _ -> exp
    | SEId id -> (
      (* Check if this identifier should be substituted *)
      match CCList.assoc_opt ~eq:String.equal id substitution_map with
      | Some replacement -> { exp with e = SEId replacement }
      | None -> exp)
    | SEIndex { e; index } ->
      let e = substitute_exp e in
      let index = substitute_exp index in
      { exp with e = SEIndex { e; index } }
    | SEArray exps ->
      let exps = CCList.map substitute_exp exps in
      { exp with e = SEArray exps }
    | SECall { instance; path; args } ->
      let args = CCList.map substitute_exp args in
      { exp with e = SECall { instance; path; args } }
    | SEUnOp (op, exp1) ->
      let exp1 = substitute_exp exp1 in
      { exp with e = SEUnOp (op, exp1) }
    | SEOp (op, exp1, exp2) ->
      let exp1 = substitute_exp exp1 in
      let exp2 = substitute_exp exp2 in
      { exp with e = SEOp (op, exp1, exp2) }
    | SEIf { cond; then_; else_ } ->
      let cond = substitute_exp cond in
      let then_ = substitute_exp then_ in
      let else_ = substitute_exp else_ in
      { exp with e = SEIf { cond; then_; else_ } }
    | SETuple exps ->
      let exps = CCList.map substitute_exp exps in
      { exp with e = SETuple exps }
    | SEMember (exp1, member) ->
      let exp1 = substitute_exp exp1 in
      { exp with e = SEMember (exp1, member) }
    | SEGroup exp1 ->
      let exp1 = substitute_exp exp1 in
      { exp with e = SEGroup exp1 }
    | SERecord { path; elems } ->
      let elems = CCList.map (fun (path, exp) -> path, substitute_exp exp) elems in
      { exp with e = SERecord { path; elems } }
    | SENamed (name, exp1) ->
      let name = substitute_exp name in
      let exp1 = substitute_exp exp1 in
      { exp with e = SENamed (name, exp1) }
  in
  substitute_stmt stmt


(* Helper function to convert Typed.arg to Syntax.arg *)
let convert_typed_arg_to_syntax (arg : Typed.arg) : Syntax.arg = arg.name, None, arg.loc
(* No type annotation - will be inferred *)

(* Creates a specialized function definition by substituting generic parameters *)
let create_specialized_function (generic_func : Typed.generic_function)
    (bindings : (string * Typed.generic_binding) list) (specialized_name : string) : Syntax.function_def =
  (* Create substitution map from bindings *)
  let substitution_map =
    CCList.fold_left
      (fun acc (param_name, binding) ->
        match binding with
        | Typed.BindFunction (func_name, _) -> (param_name, func_name) :: acc
        | Typed.BindType _ -> acc (* Type substitution not implemented yet *)
        | Typed.BindConstant _ -> acc (* Constant substitution not implemented yet *)
        | Typed.BindNonSpecializable -> acc (* Non-specializable parameters are skipped *))
      []
      bindings
  in
  (* Substitute generic parameters in the function body *)
  let substituted_body = substitute_generic_params generic_func.body substitution_map in
  (* Convert args from Typed.arg to Syntax.arg *)
  let syntax_args = CCList.map convert_typed_arg_to_syntax generic_func.args in
  (* Create new function definition with substituted body *)
  { name = specialized_name
  ; generic_params = [] (* Specialized functions have no generic parameters *)
  ; args = syntax_args
  ; t = None (* Will be inferred *)
  ; next = None
  ; tags = generic_func.tags
  ; loc = generic_func.loc
  ; body = substituted_body
  }


let has_generic_params (def : Syntax.function_def) : bool = not (CCList.is_empty def.generic_params)

let rec top_stmt (iargs : Args.args) (env : env) (s : Syntax.top_stmt) : env * top_stmt list =
  match s with
  | { top = STopError; _ } -> failwith "Parser error"
  | { top = STopFunction def; _ } when has_generic_params def ->
    (* Store generic function, don't process yet *)
    let generic_func = create_generic_function env def in
    let env = Env.addGeneric env generic_func in
    env, [] (* No output yet, templates processed on demand *)
  | { top = STopFunction def; _ } ->
    let env = Env.createContextForFunction env def.name def.loc in
    let env, (def, body) = function_def iargs env def in
    let def = insertContextArgument env def in
    let env = Env.exitContext env in
    (* Process pending injections iteratively until no more are created *)
    (* This handles nested generic calls where processing one creates more *)
    let rec process_pending_injections depth acc =
      if depth > 100 then
        failwith "Too many nested generic instantiations - possible infinite recursion"
      else
        let pending_functions = Env.getPendingInjectionsAndClear env in
        if CCList.is_empty pending_functions then
          acc
        else
          let new_stmts =
            CCList.map
              (fun ( (func_def : Typed.function_def)
                   , syntax_body
                   , (string_substitutions, const_substitutions)
                   , type_bindings )
                 ->
                (* Process the substituted syntax body through type inference *)
                let substituted_body = substitute_stmt string_substitutions const_substitutions syntax_body in
                (* Create proper function context for body processing *)
                let func_name = func_def.name.id in
                let env_with_context = Env.createContextForFunction env func_name func_def.loc in
                let env_in_function, _, _ =
                  Env.enterFunction env_with_context func_name func_def.args (snd func_def.t) func_def.loc
                in
                let _, return_type = func_def.t in
                (* Use the type bindings directly - they already contain the correct mappings *)
                let type_substitution_map = type_bindings in
                let _, typed_body_list =
                  stmt_with_type_substitution env_in_function type_substitution_map return_type substituted_body
                in
                let typed_body =
                  match typed_body_list with
                  | [ single_stmt ] -> single_stmt
                  | multiple_stmts -> { s = StmtBlock multiple_stmts; loc = func_def.loc }
                in
                (* Add _ctx argument if the function has mem variables *)
                let func_def = insertContextArgument env_in_function func_def in
                { top = TopFunction (func_def, typed_body); loc = func_def.loc })
              pending_functions
          in
          (* Process any newly created pending injections *)
          process_pending_injections (depth + 1) (acc @ new_stmts)
    in
    let injected_stmts = process_pending_injections 0 [] in
    (* Return injected functions followed by the main function *)
    env, injected_stmts @ [ { top = TopFunction (def, body); loc = def.loc } ]
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


let infer_single (iargs : Args.args) (env : env) (h : Parse.parsed_file) : env * top_stmt list =
  let set = createExistingTypeSet (createTypes env) in
  let env = Env.enterModule env h.name in
  let env, stmt = top_stmt_list iargs env h.stmts in
  let env = Env.exitModule env in
  let types = removeExistingTypes set (createTypes env) in
  env, stmt @ types


let infer (iargs : Args.args) (parsed : Parse.parsed_file list) : env * top_stmt list =
  let env, stmts =
    CCList.fold_left
      (fun (env, acc) (h : Parse.parsed_file) ->
        let env = Env.enterModule env h.name in
        let env, stmt = top_stmt_list iargs env h.stmts in
        let env = Env.exitModule env in
        env, stmt @ acc)
      (Env.empty (), [])
      parsed
  in
  let types = createTypes env in
  env, types @ CCList.rev stmts
