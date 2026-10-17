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

(** Elaboration phase: generic function instantiation and type intrinsic resolution.
    This module transforms EGenCall nodes to ECall nodes by creating specialized
    versions of generic functions with concrete types. *)

open Util
open Pparser
open Env
open Typed

let context_name = Env.context_name

(* ========== Helper Functions ========== *)

let pathString = Typed.pathString

let resolve (env : env) (t : type_) : type_ = Subst.walk env.subst t

let checkMemExists (env : env) name =
  let f = Env.getCurrentFunction env in
  (* Check context record where Inst/Mem variables are stored by addVar *)
  match Env.contextOf f with
  | Some context -> (
    match Env.Map.find name (Env.contextMembers env context) with Some {kind= Mem _ | Inst; _} -> true | _ -> false )
  | None ->
      false

(* Get the type of a mem/inst variable in the current function's context *)
let memVarType (env : env) name : Typed.type_ option =
  let f = Env.getCurrentFunction env in
  match Env.contextOf f with
  | Some context -> (
    match Env.Map.find name (Env.contextMembers env context) with
    | Some {kind= Mem _ | Inst; t; _} ->
        Some t
    | _ ->
        None )
  | None ->
      None

(* Get the name of the context type of a function (its first argument when it takes a context) *)
let ctxTypeNameOf (env : env) (def : Typed.function_def) : string option =
  match def.args with
  | {name; t; _} :: _ when String.equal name context_name -> (
    match (resolve env t).tx with TEId p -> Some (pathString p) | _ -> None )
  | _ ->
      None

(* Search the current function for the most recent auto-generated instance of the given
   specialized function. Returns the tick and the instance name. *)
let findAnonymousInstance (env : env) (spec_def : Typed.function_def) : (int * string) option =
  let number =
    Printf.sprintf "%.2x%.2x"
      (0xFF land Hashtbl.hash (pathString spec_def.name))
      (0xFF land Hashtbl.hash (pathString (Env.getContext env)))
  in
  let rec loop n =
    if n < 0 then None
    else
      let name = "inst_" ^ string_of_int n ^ number in
      if checkMemExists env name then Some (n, name) else loop (n - 1)
  in
  loop (Env.currentFunctionTick env)

(* Key identifying a call site within a function: used to reuse the same generated instance
   name when the same call expression is processed more than once *)
let callSiteKey (env : env) (loc : Loc.t) : string =
  let f = Env.getCurrentFunction env in
  Printf.sprintf "%s|%s:%d-%d" (pathString f.path) (Loc.file loc) loc.Loc.start_pos.Lexing.pos_cnum
    loc.Loc.end_pos.Lexing.pos_cnum

let typeToMangledName = Typed.typeToMangledName

(* ========== Constant Literal Handling ========== *)

(* Check if a typed expression is a compile-time constant literal *)
let is_constant_literal (e : Typed.exp) : bool =
  match e.e with EInt _ | EReal _ | EBool _ | EString _ | EFixed _ -> true | _ -> false

(* Encode a float exactly as an identifier-safe string. Prefers the short representation
   when it round-trips; falls back to the full precision one. *)
let float_to_signature_string (f : float) : string =
  let short = Printf.sprintf "%.6g" f in
  let s = if float_of_string short = f then short else Printf.sprintf "%.17g" f in
  let s = Str.global_replace (Str.regexp_string ".") "_" s in
  let s = Str.global_replace (Str.regexp_string "-") "n" s in
  let s = Str.global_replace (Str.regexp_string "+") "p" s in
  s

(* Convert a constant literal expression to a string for signature encoding.
   The encoding must be exact: two different constants must never produce the same string. *)
let constant_to_signature_string (e : Typed.exp) : string =
  match e.e with
  | EInt n ->
      if n < 0 then "n" ^ string_of_int (abs n) else string_of_int n
  | EReal f ->
      float_to_signature_string f
  | EBool b ->
      if b then "true" else "false"
  | EString s ->
      Printf.sprintf "s%s" (Digest.to_hex (Digest.string s))
  | EFixed f ->
      "fx" ^ float_to_signature_string f
  | _ ->
      "var"

(* ========== Structural Specialization Keys ========== *)

(* Length-prefixed atom used by the structural keys: makes the concatenations injective *)
let signature_atom (s : string) : string = Printf.sprintf "%d:%s" (String.length s) s

(* A canonical, injective encoding of a type used in the specialization deduplication keys.
   Unlike [typeToMangledName], which flattens the type into an identifier where distinct
   types can collide (e.g. the paths A.b_c and A_b.c both mangle to "A_b_c"), every
   component here is length-prefixed and the type-tree boundaries are preserved, so two
   structurally different types never produce the same key. The equivalences of
   [typeToMangledName] (following links, collapsing single options) are mirrored so types
   that are semantically the same still share one specialization. The type must be resolved. *)
let rec type_to_structural_key (t : Typed.type_) : string =
  match t.tx with
  | TEId {id; n= None; _} ->
      "i(" ^ signature_atom id ^ ")"
  | TEId {id; n= Some module_name; _} ->
      "i(" ^ signature_atom module_name ^ signature_atom id ^ ")"
  | TEFunction (arg_types, ret_type) ->
      let args_str = CCList.map type_to_structural_key arg_types |> String.concat "" in
      "f(" ^ args_str ^ ">" ^ type_to_structural_key ret_type ^ ")"
  | TEComposed (name, type_args) ->
      let args_str = CCList.map type_to_structural_key type_args |> String.concat "" in
      "c(" ^ signature_atom name ^ args_str ^ ")"
  | TEUnbound id ->
      "u(" ^ string_of_int id ^ ")"
  | TEOption (_, type_list) -> (
    match type_list with
    | [single_type] ->
        type_to_structural_key single_type
    | multiple_types -> (
        let non_option_types =
          CCList.filter (fun t -> match t.tx with TEOption _ -> false | _ -> true) multiple_types
        in
        match non_option_types with
        | concrete_type :: _ ->
            type_to_structural_key concrete_type
        | [] ->
            "o(" ^ (CCList.map type_to_structural_key type_list |> String.concat "") ^ ")" ) )
  | TENoReturn _ ->
      "n"
  | TESize i ->
      "s(" ^ string_of_int i ^ ")"

(* Structural deduplication key of a specialization: the module that owns the generic, the
   generic's name, a specialized/non-specialized discriminator, the structural argument
   types, and the explicit argument types (with their constant values when the call is
   fully specialized). This key decides whether two calls share a specialization; the
   mangled names below are used only for the emitted identifier and may collide. *)
let build_structural_signature (owner_module : string) (generic_name : string) (arg_types : Typed.type_ list)
    (explicit_args : Typed.exp list) (specialized : bool) : string =
  let kind = if specialized then "spec" else "dyn" in
  let type_keys = CCList.map type_to_structural_key arg_types in
  let explicit_keys =
    CCList.map
      (fun (e : Typed.exp) ->
        let value = if specialized then "=" ^ signature_atom (constant_to_signature_string e) else "" in
        type_to_structural_key e.t ^ value )
      explicit_args
  in
  String.concat "|" ((signature_atom owner_module :: signature_atom generic_name :: kind :: type_keys) @ explicit_keys)

(* ========== Signature Building ========== *)

(* Build a readable name for a specialization based on the resolved types. These names are
   used only as emitted identifiers: they are not injective, so they must not be used to
   decide whether two specializations are the same (see [build_structural_signature]). *)
let build_signature_string (generic_name : string) (arg_types : Typed.type_ list) : string =
  String.concat "_" (generic_name :: CCList.map typeToMangledName arg_types)

(* Build a signature string that includes constant types and values for fully specialized functions *)
let build_specialized_signature_string (generic_name : string) (arg_types : Typed.type_ list)
    (explicit_args : Typed.exp list) : string =
  let type_strings = CCList.map typeToMangledName arg_types in
  let const_strings =
    CCList.map (fun (e : Typed.exp) -> typeToMangledName e.t ^ "_" ^ constant_to_signature_string e) explicit_args
  in
  String.concat "_" ((generic_name :: type_strings) @ const_strings)

(* Build a signature string for the non-specialized version (when any constant param is a variable).
   It includes the argument types and the types of the explicit arguments so that different type
   instantiations do not collapse into a single function. *)
let build_nonspec_signature_string (generic_name : string) (arg_types : Typed.type_ list)
    (explicit_args : Typed.exp list) : string =
  let type_strings = CCList.map typeToMangledName arg_types in
  let const_strings = CCList.map (fun (e : Typed.exp) -> typeToMangledName e.t) explicit_args in
  String.concat "_" ((generic_name :: type_strings) @ ("dyn" :: const_strings))

(* ========== Companion Renaming ========== *)

(* The part of the specialized name that was appended to the generic name *)
let companion_suffix (generic_name : string) (specialized_name : string) : string =
  let gl = String.length generic_name in
  if String.length specialized_name > gl && String.equal (String.sub specialized_name 0 gl) generic_name then
    String.sub specialized_name gl (String.length specialized_name - gl)
  else "_" ^ specialized_name

(* Rename a chain of companion functions so each specialization gets its own set *)
let rec rename_companion_chain (suffix : string) (def : Syntax.function_def) : Syntax.function_def =
  {def with name= def.name ^ suffix; next= Option.map (rename_companion_chain suffix) def.next}

(* ========== Instantiation State ========== *)

module StrMap = Util.Maps.Map

(* State for tracking instantiated generic functions during post-processing. It is threaded
   through the elaboration together with the environment. *)
type instantiation_state =
  { instantiated: (Typed.function_def * Typed.stmt) StrMap.t (* Structural signature -> specialization *)
  ; pending_functions: (string * string * Typed.function_def * Typed.stmt) list
        (* Specializations to emit: module, generic name, definition and body. The newest first. *)
  ; functions_needing_context: Typed.type_ StrMap.t
  ; in_progress: bool StrMap.t
        (* Signatures currently being instantiated (for recursive generics). The flag records
           whether a recursive call was emitted before knowing the function needs a context. *)
  ; call_instance_names: string StrMap.t
        (* Instance names generated per call site, so re-processing a call reuses the same instance *)
  ; reuse_env_specializations: bool
        (* When elaborating code against an already-elaborated program (e.g. an expression given
           to -eval), specializations that already exist in the environment are reused *)
  ; name_signatures: string StrMap.t
        (* Emitted specialization name -> structural signature that owns it. The mangled names
           are not injective, so when a structurally different specialization wants a name that
           is already taken, its name is extended with a digest of its structural signature. *)
  ; pending_generic_calls: Typed.exp list
        (* Generic calls found in the function being processed, to resolve companion calls that
           come before the call to their parent *)
  }

let create_instantiation_state (reuse_env_specializations : bool) : instantiation_state =
  { instantiated= StrMap.empty
  ; pending_functions= []
  ; functions_needing_context= StrMap.empty
  ; in_progress= StrMap.empty
  ; call_instance_names= StrMap.empty
  ; reuse_env_specializations
  ; name_signatures= StrMap.empty
  ; pending_generic_calls= [] }

(* Sentinel stored in [name_signatures] for names that are taken but do not correspond to a
   specialization created by this elaboration (e.g. a structurally different function that
   already exists in the environment). It can never equal a structural signature because
   those always start with a length-prefixed atom. *)
let foreign_name_sentinel = "!foreign"

(* Reserve an emitted name for the given structural signature. Two different signatures can
   mangle to the same readable name; the later one gets the name extended with a digest of
   its signature (see [Util.Names.disambiguate]). *)
let claim_specialization_name (state : instantiation_state) (signature : string) (base_name : string) :
    instantiation_state * string =
  let taken candidate = StrMap.find_opt candidate state.name_signatures in
  let name = Names.disambiguate ~taken signature base_name in
  ({state with name_signatures= StrMap.add name signature state.name_signatures}, name)

let resolveTypeIntrinsicsInStmt = Typed.resolveTypeIntrinsicsInStmt

let substituteConstantsInStmt = Typed.substituteConstantsInStmt

(* ========== Generic Parameter Pairing ========== *)

(* Pairs each constant generic parameter with the value the call gives it *)
let constant_param_pairs (params : Typed.call_param list) : ((string * Typed.type_) * Typed.exp) list =
  CCList.filter_map
    (function
      | Typed.CallGeneric {param= Typed.GParamConstant (name, param_type, _); value} ->
          Some ((name, param_type), value)
      | Typed.CallGeneric {param= Typed.GParamType _ | Typed.GParamFunction _; _} | Typed.CallArg _ ->
          None )
    params

(* The argument types (excluding any context argument) that the specialization created for a
   call will have: the regular argument types, interleaved with the explicit constant
   parameters when the call is not fully specialized. Used to verify that a function found in
   the environment really is the wanted specialization and not a mangled-name collision. The
   parameters must be resolved. *)
let expected_specialization_arg_types (params : Typed.call_param list) (all_constants : bool) : Typed.type_ list =
  CCList.filter_map
    (function
      | Typed.CallArg {value; _} ->
          Some value.t
      | Typed.CallGeneric {param= Typed.GParamConstant _; value} ->
          if all_constants then None else Some value.t
      | Typed.CallGeneric {param= Typed.GParamType _ | Typed.GParamFunction _; _} ->
          None )
    params

(* How a parameter of a generic function appears in one of its specializations *)
type specialized_param =
  | RegularParam of Typed.arg (* A regular argument, with its fresh type *)
  | ConstantParam of Typed.arg (* A constant parameter, an argument when it is not inlined *)
  | ResolvedParam
(* A type or function parameter, resolved by the specialization *)

(* ========== Typechecker Dependencies ========== *)

(** Functions from the typechecker that the elaborator needs. Elaboration and typechecking
    are mutually recursive across modules (the typechecker elaborates and the elaborator
    typechecks specialized bodies): the functor breaks the cycle. Typechecking.ml
    instantiates it. *)
module type TYPECHECKER = sig
  val unify : env -> type_ -> type_ -> env option

  val unifyRaise : ?bind:bool -> env -> Loc.t -> type_ -> type_ -> env

  val function_def_opt : Args.args -> env -> Syntax.function_def option -> env * (function_def * stmt) option

  val stmt_with_type_substitution : env -> (string * type_) list -> type_ -> Syntax.stmt -> env * stmt list

  val insertContextArgument : env -> function_def -> env * function_def

  val propagateVariability : env -> Loc.t -> arg list option -> exp list -> env
end

module Make (T : TYPECHECKER) = struct
  open T

  (* ========== Generic Function Instantiation ========== *)

  (* Create a specialized function from a generic function with resolved types.
     The body is fully processed (nested generic calls are instantiated) while the
     environment is still inside the specialized function, so instances created by
     nested calls are stored in this function's context. The returned environment is at the
     location where the specialization was created. *)
  let rec instantiate_generic_function (iargs : Args.args) (env : env) (state : instantiation_state)
      (generic_func : Typed.generic_function) (signature : string) (specialized_name : string)
      (params : Typed.call_param list) (loc : Loc.t) : env * instantiation_state * (Typed.function_def * Typed.stmt) =
    let explicit_args = Typed.callExplicitArgs params in
    let all_constants = CCList.length explicit_args > 0 && CCList.for_all is_constant_literal explicit_args in
    let constant_pairs = constant_param_pairs params in
    (* Create fresh copies of the generic types, keeping the mapping from the original unbound
       variables so the type parameters can be resolved after unification *)
    let specialize_param (env, mapping) (param : Typed.call_param) =
      match param with
      | Typed.CallArg {arg; value} ->
          let (env, mapping), fresh_t = Env.copyType (env, mapping) arg.t in
          let env = match unify env fresh_t value.t with Some env -> env | None -> env in
          ((env, mapping), RegularParam {arg with t= fresh_t})
      | Typed.CallGeneric {param= Typed.GParamConstant (name, param_type, param_loc); value} ->
          (* Defensive check: the typechecker already validated the argument against the
             declared type. A fresh copy is unified so the generic definition itself is never
             constrained. *)
          let (env, _), fresh_param_type = Env.copyType (env, Subst.emptyCopyMapping) param_type in
          let env = unifyRaise env value.loc fresh_param_type value.t in
          ((env, mapping), ConstantParam ({name; t= value.t; loc= param_loc} : Typed.arg))
      | Typed.CallGeneric {param= Typed.GParamType _ | Typed.GParamFunction _; _} ->
          ((env, mapping), ResolvedParam)
    in
    (* Each parameter as an argument of the specialization, when it becomes one *)
    let (env, mapping), specialized_params =
      CCList.fold_left_map specialize_param (env, Subst.emptyCopyMapping) params
    in
    let (env, mapping), fresh_ret_type = Env.copyType (env, mapping) (snd generic_func.t) in
    (* Resolve each type parameter to its concrete type using the unified fresh types *)
    let type_substitution_map =
      CCList.map
        (fun (name, orig_t) ->
          match Subst.copyOf env.subst mapping orig_t with
          | Some fresh_t ->
              (name, resolve env fresh_t)
          | None ->
              (name, resolve env orig_t) )
        generic_func.type_param_map
    in
    let constant_substitution_map =
      if all_constants then CCList.map (fun ((name, _), value) -> (name, value)) constant_pairs else []
    in
    (* All arguments in declaration order: regular arguments with their fresh types and
       constant parameters turned into regular arguments *)
    let all_args_for_body =
      CCList.filter_map
        (function RegularParam arg | ConstantParam arg -> Some arg | ResolvedParam -> None)
        specialized_params
    in
    (* When all constant parameters are literals they are inlined in the body and dropped
       from the argument list *)
    let specialized_args =
      if all_constants then
        CCList.filter_map
          (function RegularParam arg -> Some arg | ConstantParam _ | ResolvedParam -> None)
          specialized_params
      else all_args_for_body
    in
    let env = Env.createContextForFunctionWithIndex env specialized_name loc generic_func.type_index in
    let env, inferred_ret = Env.fresh env (C.noreturn loc) in
    let env, path, _t = Env.enterFunction env specialized_name all_args_for_body inferred_ret loc in
    (* Pre-register a provisional definition so recursive calls resolve to this specialization
       instead of instantiating it again *)
    let make_def next : Typed.function_def =
      { name= path
      ; args= specialized_args
      ; t= (CCList.map (fun (a : Typed.arg) -> a.t) specialized_args, fresh_ret_type)
      ; loc= generic_func.loc
      ; tags= generic_func.tags
      ; is_root= false
      ; next }
    in
    let state =
      { state with
        instantiated= StrMap.add signature (make_def None, {s= StmtBlock []; loc}) state.instantiated
      ; in_progress= StrMap.add signature false state.in_progress }
    in
    let env, body = stmt_with_type_substitution env type_substitution_map fresh_ret_type generic_func.body in
    let resolved_substitution_map =
      CCList.map (fun (name, t) -> (name, Subst.zonk env.subst t)) type_substitution_map
    in
    let body = CCList.map (resolveTypeIntrinsicsInStmt resolved_substitution_map) body in
    let body =
      if CCList.length constant_substitution_map > 0 then
        CCList.map (substituteConstantsInStmt constant_substitution_map) body
      else body
    in
    let combined_body = match body with [single] -> single | stmts -> {s= StmtBlock stmts; loc= generic_func.loc} in
    (* Process nested generic calls while still inside the specialized function so instances
       created by them are stored in this function's context *)
    let state = prescan_generic_calls_in_stmt state combined_body in
    let env, state, processed_body = process_stmt_instantiation iargs env state combined_body in
    let recursed_without_context = StrMap.get_or signature state.in_progress ~default:false in
    let state = {state with in_progress= StrMap.remove signature state.in_progress} in
    let env = Env.reportUnusedArguments env in
    let env = Env.exitFunction env in
    (* Companion functions are instantiated once per specialization with unique names so each
       one is bound to the context of its own specialization *)
    let env, next =
      match generic_func.next with
      | None ->
          (env, None)
      | Some syntax_next ->
          let suffix = companion_suffix generic_func.name specialized_name in
          let renamed = rename_companion_chain suffix syntax_next in
          function_def_opt iargs env (Some renamed)
    in
    let env, state, next = process_companion_bodies iargs env state next in
    let specialized_def = make_def next in
    let env, specialized_def = insertContextArgument env specialized_def in
    let env = Env.exitContext env in
    let () =
      if recursed_without_context then
        match specialized_def.args with
        | {name; _} :: _ when String.equal name context_name ->
            Error.raiseError
              (Printf.sprintf
                 "The generic function '%s' is recursive and requires state (mem or instances). Recursive generic \
                  functions with state are not supported."
                 generic_func.name )
              generic_func.loc
        | _ ->
            ()
    in
    (env, state, (specialized_def, processed_body))

  (* Process the bodies of companion functions so generic calls inside them are instantiated.
     Instances created by them are stored in the context shared with the parent specialization. *)
  and process_companion_bodies (iargs : Args.args) (env : env) (state : instantiation_state)
      (next : (Typed.function_def * Typed.stmt) option) :
      env * instantiation_state * (Typed.function_def * Typed.stmt) option =
    match next with
    | None ->
        (env, state, None)
    | Some (cdef, cbody) ->
        let cenv = Env.reenterFunction env cdef.name in
        let state = prescan_generic_calls_in_stmt state cbody in
        let cenv, state, cbody = process_stmt_instantiation iargs cenv state cbody in
        let env = Env.restoreLocation env cenv in
        let env, state, cnext = process_companion_bodies iargs env state cdef.next in
        (env, state, Some ({cdef with next= cnext}, cbody))

  (* ========== Prescan for Generic Calls ========== *)

  and prescan_generic_calls_in_stmt (state : instantiation_state) (stmt : Typed.stmt) : instantiation_state =
    match stmt.s with
    | StmtVal _ | StmtMem _ ->
        state
    | StmtReturn e ->
        prescan_generic_calls_in_exp state e
    | StmtBind (lhs, e) ->
        let state = prescan_generic_calls_in_lexp state lhs in
        prescan_generic_calls_in_exp state e
    | StmtIf (cond, then_s, else_opt) ->
        let state = prescan_generic_calls_in_exp state cond in
        let state = prescan_generic_calls_in_stmt state then_s in
        Option.fold ~none:state ~some:(prescan_generic_calls_in_stmt state) else_opt
    | StmtWhile (cond, body) ->
        let state = prescan_generic_calls_in_exp state cond in
        prescan_generic_calls_in_stmt state body
    | StmtBlock stmts ->
        CCList.fold_left prescan_generic_calls_in_stmt state stmts

  and prescan_generic_calls_in_lexp (state : instantiation_state) (l : Typed.lexp) : instantiation_state =
    match l.l with
    | LWild | LId _ ->
        state
    | LMember (le, _) ->
        prescan_generic_calls_in_lexp state le
    | LIndex {e= le; index} ->
        let state = prescan_generic_calls_in_lexp state le in
        prescan_generic_calls_in_exp state index
    | LTuple elems ->
        CCList.fold_left prescan_generic_calls_in_lexp state elems

  and prescan_generic_calls_in_exp (state : instantiation_state) (e : Typed.exp) : instantiation_state =
    let prescan_list state l = CCList.fold_left prescan_generic_calls_in_exp state l in
    match e.e with
    | EGenCall {params; _} ->
        let state = {state with pending_generic_calls= e :: state.pending_generic_calls} in
        let state = prescan_list state (Typed.callArgs params) in
        prescan_list state (Typed.callExplicitArgs params)
    | ECall {args; _} | EGenCompanionCall {args; _} ->
        prescan_list state args
    | EIf {cond; then_; else_} ->
        prescan_list state [cond; then_; else_]
    | EOp (_, lhs, rhs) ->
        prescan_list state [lhs; rhs]
    | EUnOp (_, arg) | EMember (arg, _) ->
        prescan_generic_calls_in_exp state arg
    | EIndex {e= inner; index} ->
        prescan_list state [inner; index]
    | EArray elems | ETuple elems ->
        prescan_list state elems
    | ERecord {elems; _} ->
        prescan_list state (CCList.map snd elems)
    | EId _ | EUnit | EBool _ | EInt _ | EReal _ | EFixed _ | EString _ | EConst _ | ETypeIntrinsic _ | EEnum _ ->
        state

  (* ========== Expression and Statement Processing ========== *)

  (* Names an instance of [callee] stored in the current function. A call site that was already
     processed reuses the name it got before. *)
  and anonymous_instance_name (env : env) (state : instantiation_state) (callee : string) (ctx_t : type_) (loc : Loc.t)
      : env * instantiation_state * string =
    let site = callSiteKey env loc in
    match StrMap.find_opt site state.call_instance_names with
    | Some name ->
        (Env.addVar env name ctx_t Env.LocalInst loc, state, name)
    | None ->
        let number =
          Printf.sprintf "%.2x%.2x"
            (0xFF land Hashtbl.hash callee)
            (0xFF land Hashtbl.hash (pathString (Env.getContext env)))
        in
        let rec generateName env =
          let env, n = Env.nextFunctionTick env in
          let name = "inst_" ^ string_of_int n ^ number in
          if checkMemExists env name || Env.checkConstantExists env name then generateName env else (env, name)
        in
        let env, name = generateName env in
        let env = Env.addVar env name ctx_t Env.LocalInst loc in
        let state = {state with call_instance_names= StrMap.add site name state.call_instance_names} in
        (env, state, name)

  and process_exp_list (iargs : Args.args) (env : env) (state : instantiation_state) (l : Typed.exp list) :
      env * instantiation_state * Typed.exp list =
    let process (env, state) e =
      let env, state, e = process_exp_instantiation iargs env state e in
      ((env, state), e)
    in
    let (env, state), l = CCList.fold_left_map process (env, state) l in
    (env, state, l)

  (* The specialization of the generic function for a call: an existing one or a new one. When
     the specialization is being instantiated (a recursive call), its signature is returned. *)
  and find_or_instantiate (iargs : Args.args) (env : env) (state : instantiation_state)
      (generic_func : Typed.generic_function) (generic_path : path) (params : Typed.call_param list) (loc : Loc.t) :
      env * instantiation_state * Typed.function_def * string option =
    let resolved_params = CCList.map (Subst.zonkCallParam env.subst) params in
    let args = Typed.callArgs resolved_params in
    let explicit_args = Typed.callExplicitArgs resolved_params in
    let resolved_arg_types = CCList.map (fun (a : Typed.exp) -> a.t) args in
    let has_explicit = CCList.length explicit_args > 0 in
    let all_constants = has_explicit && CCList.for_all is_constant_literal explicit_args in
    let base_name =
      if all_constants then build_specialized_signature_string generic_func.name resolved_arg_types explicit_args
      else if has_explicit then build_nonspec_signature_string generic_func.name resolved_arg_types explicit_args
      else build_signature_string generic_func.name resolved_arg_types
    in
    let generic_module = generic_path.n in
    (* The deduplication key includes the module that owns the generic so a qualified and
       an unqualified call to the same generic map to the same specialization, and
       same-named generics in different modules do not collide *)
    let owner_module = match generic_module with Some m -> m | None -> (Env.getCurrentModule env).name in
    let signature =
      build_structural_signature owner_module generic_func.name resolved_arg_types explicit_args all_constants
    in
    match StrMap.find_opt signature state.instantiated with
    | Some (def, _) ->
        let in_progress = if StrMap.mem signature state.in_progress then Some signature else None in
        (env, state, def, in_progress)
    | None -> (
        let env_for_instantiation =
          match generic_module with Some module_name -> Env.enterModule env module_name | None -> env
        in
        let env_for_instantiation, existing =
          if state.reuse_env_specializations then
            Env.tryLookFunctionCall env_for_instantiation {id= base_name; n= None; loc}
          else (env_for_instantiation, None)
        in
        (* The lookup can create fresh variables, so the environment continues from it *)
        let env = Env.restoreLocation env env_for_instantiation in
        (* A function found in the environment must be verified structurally: mangled
           names are not injective, so it could be a different specialization that
           happens to share the name. In that case the name is marked as taken and a
           fresh specialization (with a disambiguated name) is created instead. *)
        let state, existing =
          match existing with
          | Some f ->
              let expected = expected_specialization_arg_types resolved_params all_constants in
              let found =
                match f.args with
                | Some a ->
                    CCList.map (fun (a : Typed.arg) -> Subst.zonk env.subst a.t) a
                | None ->
                    []
              in
              let same_key a b = String.equal (type_to_structural_key a) (type_to_structural_key b) in
              if CCList.length expected = CCList.length found && CCList.for_all2 same_key expected found then
                (state, Some f)
              else if StrMap.mem base_name state.name_signatures then (state, None)
              else
                let name_signatures = StrMap.add base_name foreign_name_sentinel state.name_signatures in
                ({state with name_signatures}, None)
          | None ->
              (state, None)
        in
        match existing with
        | Some f ->
            (* The specialization was created by a previous elaboration of the program:
               reuse it instead of instantiating it again *)
            let base_args = match f.args with Some a -> a | None -> [] in
            let def_args =
              match Env.activeContextArgument env f with
              | Some {arg_t; _} ->
                  ({name= context_name; t= arg_t; loc} : Typed.arg) :: base_args
              | None ->
                  base_args
            in
            let def : Typed.function_def =
              { name= f.path
              ; args= def_args
              ; t= (CCList.map (fun (a : Typed.arg) -> a.t) def_args, snd f.t)
              ; loc= generic_func.loc
              ; tags= generic_func.tags
              ; is_root= false
              ; next= None }
            in
            let state =
              {state with instantiated= StrMap.add signature (def, {s= StmtBlock []; loc}) state.instantiated}
            in
            (env, state, def, None)
        | None ->
            let state, specialized_name = claim_specialization_name state signature base_name in
            let instantiated_env, state, (def, processed_body) =
              instantiate_generic_function iargs env_for_instantiation state generic_func signature specialized_name
                params loc
            in
            let env = Env.restoreLocation env instantiated_env in
            let target_module =
              match def.name.n with
              | Some m ->
                  m
              | None -> (
                match generic_module with
                | Some m ->
                    m
                | None ->
                    let m = Env.getCurrentModule env in
                    m.name )
            in
            let state =
              { state with
                instantiated= StrMap.add signature (def, processed_body) state.instantiated
              ; pending_functions= (target_module, generic_func.name, def, processed_body) :: state.pending_functions }
            in
            (env, state, def, None) )

  and process_exp_instantiation (iargs : Args.args) (env : env) (state : instantiation_state) (e : Typed.exp) :
      env * instantiation_state * Typed.exp =
    let loc = e.loc in
    match e.e with
    | EGenCall {instance; generic_path; params} -> (
        let generic_name = Pla.print (Syntax.print_path generic_path) in
        match Env.lookupGeneric env generic_path with
        | None ->
            Error.raiseError (Printf.sprintf "Generic function '%s' not found" generic_name) loc
        | Some generic_func ->
            let explicit_args = Typed.callExplicitArgs params in
            let all_constants = CCList.length explicit_args > 0 && CCList.for_all is_constant_literal explicit_args in
            let env, state, specialized_def, in_progress =
              find_or_instantiate iargs env state generic_func generic_path params loc
            in
            (* The regular arguments are processed before the explicit generic ones: processing
               can instantiate nested generics and create instances, whose names follow that order *)
            let process (env, state) value =
              let env, state, value = process_exp_instantiation iargs env state value in
              ((env, state), value)
            in
            let (env, state), processed_params =
              CCList.fold_left_map
                (fun env_state param ->
                  match param with
                  | Typed.CallArg {arg; value} ->
                      let env_state, value = process env_state value in
                      (env_state, Typed.CallArg {arg; value})
                  | Typed.CallGeneric _ ->
                      (env_state, param) )
                (env, state) params
            in
            let (env, state), processed_params =
              CCList.fold_left_map
                (fun env_state param ->
                  match param with
                  | Typed.CallGeneric {param; value} ->
                      let env_state, value = process env_state value in
                      (env_state, Typed.CallGeneric {param; value})
                  | Typed.CallArg _ ->
                      (env_state, param) )
                (env, state) processed_params
            in
            (* The regular arguments and, unless they are inlined in the specialized body, the
               constant arguments, in declaration order *)
            let processed_args =
              CCList.filter_map
                (function
                  | Typed.CallArg {value; _} ->
                      Some value
                  | Typed.CallGeneric {param= Typed.GParamConstant _; value} ->
                      if all_constants then None else Some value
                  | Typed.CallGeneric {param= Typed.GParamType _ | Typed.GParamFunction _; _} ->
                      None )
                processed_params
            in
            let specialized_non_ctx_args =
              match specialized_def.args with
              | {name; _} :: rest when String.equal name context_name ->
                  rest
              | args ->
                  args
            in
            let env = propagateVariability env loc (Some specialized_non_ctx_args) processed_args in
            let env, state, final_args =
              match in_progress with
              | Some signature -> (
                  (* Recursive call to a specialization that is being instantiated right now.
                     When it is a direct self-call and the function has state, the context is
                     passed through unchanged (same instance). *)
                  let current_f = Env.getCurrentFunction env in
                  match Env.activeContextArgument env current_f with
                  | Some {arg_t; _} when Syntax.compare_path current_f.path specialized_def.name = 0 ->
                      (env, state, {e= EId context_name; t= arg_t; loc} :: processed_args)
                  | Some _ | None ->
                      let state = {state with in_progress= StrMap.add signature true state.in_progress} in
                      (env, state, processed_args) )
              | None -> (
                match specialized_def.args with
                | {name; t= ctx_t; _} :: _ when String.equal name context_name ->
                    let current_ctx_t = (Env.currentContextArgument env loc).arg_t in
                    let env, state, inst_name =
                      match instance with
                      | Some user_inst_name ->
                          let () =
                            if Env.checkConstantExists env user_inst_name then
                              Error.raiseError
                                (Printf.sprintf "Cannot use '%s' as an instance name: a constant with this name exists"
                                   user_inst_name )
                                loc
                          in
                          (* addVar unifies the context types when the instance already exists,
                             reporting an error if the same instance is used with an incompatible
                             specialization *)
                          (Env.addVar env user_inst_name ctx_t Env.LocalInst loc, state, user_inst_name)
                      | None ->
                          anonymous_instance_name env state (pathString specialized_def.name) ctx_t loc
                    in
                    let ctx_e = {e= EId context_name; t= current_ctx_t; loc} in
                    let inst_e = {e= EMember (ctx_e, inst_name); t= ctx_t; loc} in
                    (env, state, inst_e :: processed_args)
                | _ ->
                    (env, state, processed_args) )
            in
            (env, state, {e= ECall {instance= None; path= specialized_def.name; args= final_args}; t= e.t; loc}) )
    | ECall {instance; path; args} ->
        let env, state, processed_args = process_exp_list iargs env state args in
        let env =
          match Env.tryLookFunctionCall env path with
          | env, Some f ->
              let func_non_ctx_args =
                match f.args with
                | Some ({name; _} :: rest) when String.equal name context_name ->
                    Some rest
                | args ->
                    args
              in
              propagateVariability env loc func_non_ctx_args processed_args
          | env, None ->
              env
        in
        let func_path_str = pathString path in
        let env, state, final_args =
          match StrMap.find_opt func_path_str state.functions_needing_context with
          | Some ctx_t ->
              let current_ctx_t = (Env.currentContextArgument env loc).arg_t in
              let env, state, inst_name = anonymous_instance_name env state func_path_str ctx_t loc in
              let ctx_e = {e= EId context_name; t= current_ctx_t; loc} in
              let inst_e = {e= EMember (ctx_e, inst_name); t= ctx_t; loc} in
              (env, state, inst_e :: processed_args)
          | None ->
              (env, state, processed_args)
        in
        (env, state, {e with e= ECall {instance; path; args= final_args}})
    | EOp (op, e1, e2) ->
        let env, state, e1 = process_exp_instantiation iargs env state e1 in
        let env, state, e2 = process_exp_instantiation iargs env state e2 in
        (env, state, {e with e= EOp (op, e1, e2)})
    | EUnOp (op, e1) ->
        let env, state, e1 = process_exp_instantiation iargs env state e1 in
        (env, state, {e with e= EUnOp (op, e1)})
    | EIf {cond; then_; else_} ->
        let env, state, cond = process_exp_instantiation iargs env state cond in
        let env, state, then_ = process_exp_instantiation iargs env state then_ in
        let env, state, else_ = process_exp_instantiation iargs env state else_ in
        (env, state, {e with e= EIf {cond; then_; else_}})
    | EIndex {e= arr; index} ->
        let env, state, arr = process_exp_instantiation iargs env state arr in
        let env, state, index = process_exp_instantiation iargs env state index in
        (env, state, {e with e= EIndex {e= arr; index}})
    | EArray elems ->
        let env, state, elems = process_exp_list iargs env state elems in
        (env, state, {e with e= EArray elems})
    | ETuple elems ->
        let env, state, elems = process_exp_list iargs env state elems in
        (env, state, {e with e= ETuple elems})
    | EMember (e1, m) ->
        let env, state, e1 = process_exp_instantiation iargs env state e1 in
        (env, state, {e with e= EMember (e1, m)})
    | ERecord {elems} ->
        let names, values = CCList.split elems in
        let env, state, values = process_exp_list iargs env state values in
        (env, state, {e with e= ERecord {elems= CCList.combine names values}})
    | EGenCompanionCall {instance; companion_name; parent_generic_path; args} ->
        process_companion_call iargs env state e instance companion_name parent_generic_path args
    | ETypeIntrinsic {intrinsic; type_param} ->
        let intrinsic_name =
          match intrinsic with TypeDefault -> "typedefault" | TypeMax -> "typemax" | TypeMin -> "typemin"
        in
        Error.raiseError
          (Printf.sprintf "Type intrinsic '%s('%s)' was not resolved - it must be used inside a generic function"
             intrinsic_name type_param )
          e.loc
    | EUnit | EBool _ | EInt _ | EReal _ | EFixed _ | EString _ | EId _ | EConst _ | EEnum _ ->
        (env, state, e)

  and process_companion_call (iargs : Args.args) (env : env) (state : instantiation_state) (e : Typed.exp)
      (instance : string option) (companion_name : string) (parent_generic_path : path) (args : Typed.exp list) :
      env * instantiation_state * Typed.exp =
    let loc = e.loc in
    match Env.lookupGeneric env parent_generic_path with
    | None ->
        Error.raiseError
          (Printf.sprintf "Parent generic function '%s' not found for companion '%s'" (pathString parent_generic_path)
             companion_name )
          loc
    | Some _parent_generic -> (
        let parent_name = parent_generic_path.id in
        (* The module that owns the parent generic. An unqualified companion call can only
           refer to a generic of the current module (the lookup rules only search there), so
           candidates from same-named generics in other modules must not be considered. *)
        let parent_module = match parent_generic_path.n with Some m -> m | None -> (Env.getCurrentModule env).name in
        (* A specialization holds this companion under the name companion_name ^ suffix *)
        let find_companion_in (def : Typed.function_def) : (Typed.function_def * Typed.stmt) option =
          let suffix = companion_suffix parent_name def.name.id in
          let target = companion_name ^ suffix in
          let rec loop (next : (Typed.function_def * Typed.stmt) option) =
            match next with
            | None ->
                None
            | Some ((cdef, _) as companion) ->
                if String.equal cdef.name.id target then Some companion else loop cdef.next
          in
          loop def.next
        in
        let collect_candidates (state : instantiation_state) =
          CCList.filter_map
            (fun (candidate_module, gen_name, (def : Typed.function_def), _body) ->
              if String.equal candidate_module parent_module && String.equal gen_name parent_name then
                match find_companion_in def with Some companion -> Some (def, companion) | None -> None
              else None )
            state.pending_functions
        in
        let candidates = collect_candidates state in
        let env, state, candidates =
          if CCList.is_empty candidates then
            (* The companion may be called before the parent: instantiate the parent call
               found during the prescan of this function. The pending call must refer to the
               same generic: both paths are resolved to their owning module before comparing. *)
            match
              CCList.find_opt
                (fun (pending_e : Typed.exp) ->
                  match pending_e.e with
                  | EGenCall {generic_path; _} ->
                      let call_module =
                        match generic_path.n with Some m -> m | None -> (Env.getCurrentModule env).name
                      in
                      String.equal generic_path.id parent_name && String.equal call_module parent_module
                  | _ ->
                      false )
                state.pending_generic_calls
            with
            | Some parent_call ->
                let env, state, _ = process_exp_instantiation iargs env state parent_call in
                (env, state, collect_candidates state)
            | None ->
                (env, state, [])
          else (env, state, candidates)
        in
        match candidates with
        | [] ->
            Error.raiseError
              (Printf.sprintf
                 "Companion function '%s' called before parent generic '%s' was instantiated. Make sure to call the \
                  parent function first."
                 companion_name parent_name )
              loc
        | ((_, (first_companion_def, _)) as first_candidate) :: _ ->
            let env, state, processed_args = process_exp_list iargs env state args in
            let companion_has_ctx =
              match first_companion_def.args with
              | {name; _} :: _ when String.equal name context_name ->
                  true
              | _ ->
                  false
            in
            let (companion_def : Typed.function_def), inst_name_opt =
              if not companion_has_ctx then
                let _, (cdef, _) = first_candidate in
                (cdef, None)
              else
                match instance with
                | Some user_inst_name ->
                    let () =
                      if Env.checkConstantExists env user_inst_name then
                        Error.raiseError
                          (Printf.sprintf "Cannot use '%s' as an instance name: a constant with this name exists"
                             user_inst_name )
                          loc
                    in
                    let _, (cdef, _) =
                      match memVarType env user_inst_name with
                      | Some t -> (
                          let inst_ctx = match (resolve env t).tx with TEId p -> Some (pathString p) | _ -> None in
                          match
                            CCList.find_opt
                              (fun ((spec : Typed.function_def), _) ->
                                match (ctxTypeNameOf env spec, inst_ctx) with
                                | Some a, Some b ->
                                    String.equal a b
                                | _ ->
                                    false )
                              candidates
                          with
                          | Some candidate ->
                              candidate
                          | None ->
                              Error.raiseError
                                (Printf.sprintf "'%s' is not an instance of the generic function '%s'" user_inst_name
                                   parent_name )
                                loc )
                      | None -> (
                        match candidates with
                        | [candidate] ->
                            candidate
                        | _ ->
                            Error.raiseError
                              (Printf.sprintf
                                 "Cannot determine which version of the generic function '%s' the instance '%s' \
                                  belongs to. Call '%s' on this instance first."
                                 parent_name user_inst_name parent_name )
                              loc )
                    in
                    (cdef, Some user_inst_name)
                | None -> (
                    (* Anonymous companion call: use the most recent auto-generated instance
                       of any specialization of the parent *)
                    let hits =
                      CCList.filter_map
                        (fun ((spec, _) as candidate) ->
                          match findAnonymousInstance env spec with
                          | Some (n, name) ->
                              Some (n, name, candidate)
                          | None ->
                              None )
                        candidates
                    in
                    match CCList.sort (fun (n1, _, _) (n2, _, _) -> compare n2 n1) hits with
                    | (_, name, (_, (cdef, _))) :: _ ->
                        (cdef, Some name)
                    | [] ->
                        Error.raiseError
                          (Printf.sprintf
                             "No instance of the generic function '%s' was found in this function. Call '%s' first or \
                              qualify the companion call with an instance name (e.g. myinst:%s(...))."
                             parent_name parent_name companion_name )
                          loc )
            in
            let companion_non_ctx_args =
              match companion_def.args with
              | {name; _} :: rest when String.equal name context_name ->
                  rest
              | args ->
                  args
            in
            let () =
              if CCList.length companion_non_ctx_args <> CCList.length processed_args then
                Error.raiseError
                  (Printf.sprintf "Companion function '%s' expects %d arguments but got %d" companion_name
                     (CCList.length companion_non_ctx_args) (CCList.length processed_args) )
                  loc
            in
            let env =
              CCList.fold_left2
                (fun env (def_arg : Typed.arg) (call_arg : Typed.exp) ->
                  unifyRaise env call_arg.loc def_arg.t call_arg.t )
                env companion_non_ctx_args processed_args
            in
            (* The typechecker gives companion calls a provisional return type: resolve it
               against the selected companion's actual return type so callers whose types
               depend on this call (e.g. unannotated wrappers) are fully inferred *)
            let env = unifyRaise env loc e.t (snd companion_def.t) in
            let env, final_args =
              match (companion_def.args, inst_name_opt) with
              | {name; t= ctx_t; _} :: _, Some inst_name when String.equal name context_name ->
                  let current_ctx_t = (Env.currentContextArgument env loc).arg_t in
                  let env = Env.addVar env inst_name ctx_t Env.LocalInst loc in
                  let ctx_e = {e= EId context_name; t= current_ctx_t; loc} in
                  let inst_e = {e= EMember (ctx_e, inst_name); t= ctx_t; loc} in
                  (env, inst_e :: processed_args)
              | _ ->
                  (env, processed_args)
            in
            (env, state, {e= ECall {instance= None; path= companion_def.name; args= final_args}; t= e.t; loc}) )

  and process_lexp_instantiation (iargs : Args.args) (env : env) (state : instantiation_state) (l : Typed.lexp) :
      env * instantiation_state * Typed.lexp =
    match l.l with
    | LWild | LId _ ->
        (env, state, l)
    | LMember (le, m) ->
        let env, state, le = process_lexp_instantiation iargs env state le in
        (env, state, {l with l= LMember (le, m)})
    | LIndex {e= le; index} ->
        let env, state, le = process_lexp_instantiation iargs env state le in
        let env, state, index = process_exp_instantiation iargs env state index in
        (env, state, {l with l= LIndex {e= le; index}})
    | LTuple elems ->
        let process (env, state) le =
          let env, state, le = process_lexp_instantiation iargs env state le in
          ((env, state), le)
        in
        let (env, state), elems = CCList.fold_left_map process (env, state) elems in
        (env, state, {l with l= LTuple elems})

  and process_stmt_instantiation (iargs : Args.args) (env : env) (state : instantiation_state) (s : Typed.stmt) :
      env * instantiation_state * Typed.stmt =
    match s.s with
    | StmtVal _ | StmtMem _ ->
        (env, state, s)
    | StmtBind (lhs, rhs) ->
        let env, state, lhs = process_lexp_instantiation iargs env state lhs in
        let env, state, rhs = process_exp_instantiation iargs env state rhs in
        (env, state, {s with s= StmtBind (lhs, rhs)})
    | StmtReturn e ->
        let env, state, e = process_exp_instantiation iargs env state e in
        (env, state, {s with s= StmtReturn e})
    | StmtIf (cond, then_, else_opt) ->
        let env, state, cond = process_exp_instantiation iargs env state cond in
        let env, state, then_ = process_stmt_instantiation iargs env state then_ in
        let env, state, else_opt =
          match else_opt with
          | None ->
              (env, state, None)
          | Some else_ ->
              let env, state, else_ = process_stmt_instantiation iargs env state else_ in
              (env, state, Some else_)
        in
        (env, state, {s with s= StmtIf (cond, then_, else_opt)})
    | StmtWhile (cond, body) ->
        let env, state, cond = process_exp_instantiation iargs env state cond in
        let env, state, body = process_stmt_instantiation iargs env state body in
        (env, state, {s with s= StmtWhile (cond, body)})
    | StmtBlock stmts ->
        let process (env, state) s =
          let env, state, s = process_stmt_instantiation iargs env state s in
          ((env, state), s)
        in
        let (env, state), stmts = CCList.fold_left_map process (env, state) stmts in
        (env, state, {s with s= StmtBlock stmts})

  (* ========== Function Definition Processing ========== *)

  let rec process_function_def (iargs : Args.args) (env : env) (state : instantiation_state) (def : Typed.function_def)
      (body : Typed.stmt) : env * instantiation_state * (Typed.function_def * Typed.stmt) =
    let fenv = Env.reenterFunction env def.name in
    (* Pending generic calls are per-function: they are only used to resolve companion calls
       that happen before the parent call within the same function *)
    let state = prescan_generic_calls_in_stmt {state with pending_generic_calls= []} body in
    let fenv, state, body = process_stmt_instantiation iargs fenv state body in
    let had_ctx_before =
      match def.args with {name; _} :: _ when String.equal name context_name -> true | _ -> false
    in
    let fenv, def = insertContextArgument fenv def in
    let state =
      match def.args with
      | {name; t= ctx_t; _} :: _ when (not had_ctx_before) && String.equal name context_name ->
          let func_path_str = pathString def.name in
          {state with functions_needing_context= StrMap.add func_path_str ctx_t state.functions_needing_context}
      | _ ->
          state
    in
    let fenv, state, next =
      match def.next with
      | None ->
          (fenv, state, None)
      | Some (next_def, next_body) ->
          let fenv, state, next = process_function_def iargs fenv state next_def next_body in
          (fenv, state, Some next)
    in
    (Env.restoreLocation env fenv, state, ({def with next}, body))

  (* ========== Top-Level Processing ========== *)

  let process_top_stmt_instantiation (iargs : Args.args) (env : env) (state : instantiation_state) (t : Typed.top_stmt)
      : env * instantiation_state * Typed.top_stmt =
    match t.top with
    | TopFunction (def, body) ->
        let env, state, (def, body) = process_function_def iargs env state def body in
        (env, state, {t with top= TopFunction (def, body)})
    | TopGenericPlaceholder _ | TopExternal _ | TopType _ | TopEnum _ | TopConstant _ | TopAlias _ ->
        (env, state, t)

  let transform_module_generics (iargs : Args.args) (env : env) (state : instantiation_state)
      (stmts : Typed.top_stmt list) : env * instantiation_state * Typed.top_stmt list =
    (* The statements are in source order, so callees are processed before their callers:
       functions that gain a context argument during elaboration are registered before any
       call to them is processed, allowing the context requirement to propagate through
       call chains of any depth in a single pass. *)
    let process (env, state) t =
      let env, state, t = process_top_stmt_instantiation iargs env state t in
      ((env, state), t)
    in
    let (env, state), stmts = CCList.fold_left_map process (env, state) stmts in
    (env, state, stmts)

  (* Replaces the placeholders of the module by the specializations of their generic functions.
     Returns the specializations that are left. *)
  let replace_placeholders_in_module (pending : (string * string * Typed.function_def * Typed.stmt) list)
      (module_name : string) (stmts : Typed.top_stmt list) :
      (string * string * Typed.function_def * Typed.stmt) list * Typed.top_stmt list =
    let replace pending (stmt : Typed.top_stmt) =
      match stmt.top with
      | TopGenericPlaceholder generic_name ->
          let for_this_generic, remaining =
            CCList.partition
              (fun (m, gname, _, _) -> String.equal m module_name && String.equal gname generic_name)
              pending
          in
          let specializations =
            CCList.map
              (fun (_, _, (def : Typed.function_def), body) -> {top= TopFunction (def, body); loc= def.loc})
              (CCList.rev for_this_generic)
          in
          (remaining, specializations)
      | _ ->
          (pending, [stmt])
    in
    let pending, stmts = CCList.fold_left_map replace pending stmts in
    (pending, CCList.flatten stmts)

  (* ========== Main Entry Point ========== *)

  (** Elaborate a typed AST by instantiating generic functions.
      This transforms EGenCall nodes to ECall nodes by creating specialized
      versions of generic functions with concrete types.

      @param reuse_existing
        Reuse specializations already present in the environment. Used when elaborating code
        against an already-elaborated program (e.g. an expression given to -eval).
      @param iargs Compiler arguments
      @param env The type environment from typechecking
      @param module_stmts List of (module_name, statements) pairs from typechecking
      @return The environment with the inference state after elaboration and the elaborated
        top-level statements *)
  let elaborate ?(reuse_existing = false) (iargs : Args.args) (env : env)
      (module_stmts : (string * Typed.top_stmt list) list) : env * Typed.top_stmt list =
    let state = create_instantiation_state reuse_existing in
    (* Pass 1: Transform all EGenCall to ECall across all modules *)
    let process (env, state) (module_name, stmts) =
      let env = Env.enterModule env module_name in
      let env, state, stmts = transform_module_generics iargs env state stmts in
      let env = Env.exitModule env in
      ((env, state), (module_name, stmts))
    in
    let (env, state), transformed_stmts = CCList.fold_left_map process (env, state) module_stmts in
    (* Pass 2: Replace placeholders with specialized functions *)
    let pending, final_stmts =
      CCList.fold_left_map
        (fun pending (module_name, stmts) -> replace_placeholders_in_module pending module_name stmts)
        state.pending_functions transformed_stmts
    in
    let final_stmts = CCList.flatten final_stmts in
    (* Specializations whose generic placeholder is not part of these statements (e.g. when
       elaborating an expression against an existing program) are emitted before everything else *)
    let leftover =
      CCList.map
        (fun (_, _, (def : Typed.function_def), body) -> {top= TopFunction (def, body); loc= def.loc})
        (CCList.rev pending)
    in
    (env, leftover @ final_stmts)
end
