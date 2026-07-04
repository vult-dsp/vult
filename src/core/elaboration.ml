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

let context_name = "_ctx"

(* ========== Helper Functions ========== *)

let pathString = Typed.pathString

let unlink = Typed.unlink

let checkMemExists (env : env) name =
  let f = Env.getCurrentFunction env in
  (* Check context record where Inst/Mem variables are stored by addVar *)
  match f.context with
  | Some (_, {descr= Record members; _}) -> (
    match Env.Map.find name members with Some {kind= Mem _ | Inst; _} -> true | _ -> false )
  | _ ->
      false

(* Get the type of a mem/inst variable in the current function's context *)
let memVarType (env : env) name : Typed.type_ option =
  let f = Env.getCurrentFunction env in
  match f.context with
  | Some (_, {descr= Record members; _}) -> (
    match Env.Map.find name members with Some {kind= Mem _ | Inst; t; _} -> Some t | _ -> None )
  | _ ->
      None

(* Get the name of the context type of a function (its first argument when it takes a context) *)
let ctxTypeNameOf (def : Typed.function_def) : string option =
  match def.args with
  | {name; t; _} :: _ when String.equal name context_name -> (
    match (unlink t).tx with TEId p -> Some (pathString p) | _ -> None )
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
  let f = Env.getCurrentFunction env in
  let rec loop n =
    if n < 0 then None
    else
      let name = "inst_" ^ string_of_int n ^ number in
      if checkMemExists env name then Some (n, name) else loop (n - 1)
  in
  loop f.tick

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

(* ========== Signature Building ========== *)

(* Build a signature string for deduplication based on resolved types *)
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

(* State for tracking instantiated generic functions during post-processing *)
type instantiation_state =
  { instantiated: (string, Typed.function_def * Typed.stmt) Hashtbl.t
  ; mutable pending_functions: (string * string * Typed.function_def * Typed.stmt) list
  ; functions_needing_context: (string, Typed.type_) Hashtbl.t
  ; in_progress: (string, bool ref) Hashtbl.t
        (* Signatures currently being instantiated (for recursive generics). The flag records
           whether a recursive call was emitted before knowing the function needs a context. *)
  ; call_instance_names: (string, string) Hashtbl.t
        (* Instance names generated per call site, so re-processing a call reuses the same instance *)
  ; reuse_env_specializations: bool
        (* When elaborating code against an already-elaborated program (e.g. an expression given
           to -eval), specializations that already exist in the environment are reused *)
  ; mutable pending_generic_calls: Typed.exp list }

let create_instantiation_state (reuse_env_specializations : bool) : instantiation_state =
  { instantiated= Hashtbl.create 16
  ; pending_functions= []
  ; functions_needing_context= Hashtbl.create 16
  ; in_progress= Hashtbl.create 4
  ; call_instance_names= Hashtbl.create 16
  ; reuse_env_specializations
  ; pending_generic_calls= [] }

let resolveTypeIntrinsicsInStmt = Typed.resolveTypeIntrinsicsInStmt

let substituteConstantsInStmt = Typed.substituteConstantsInStmt

(* ========== Forward Declarations for Mutual Recursion ========== *)

(* These need to be defined as refs to handle the mutual recursion between
   Typechecking and Elaboration modules. They are set by Typechecking.ml at load time. *)
let unify_ref : (type_ -> type_ -> bool) ref = ref (fun _ _ -> failwith "unify_ref not initialized")

let unifyRaise_ref : (?bind:bool -> Loc.t -> type_ -> type_ -> unit) ref =
  ref (fun ?bind:_ _ _ _ -> failwith "unifyRaise_ref not initialized")

let function_def_opt_ref : (Args.args -> env -> Syntax.function_def option -> env * (function_def * stmt) option) ref =
  ref (fun _ _ _ -> failwith "function_def_opt_ref not initialized")

let stmt_with_type_substitution_ref : (env -> (string * type_) list -> type_ -> Syntax.stmt -> env * stmt list) ref =
  ref (fun _ _ _ _ -> failwith "stmt_with_type_substitution_ref not initialized")

let insertContextArgument_ref : (env -> function_def -> function_def) ref =
  ref (fun _ _ -> failwith "insertContextArgument_ref not initialized")

let markExpMutable_ref : (env -> exp -> Loc.t -> unit) ref =
  ref (fun _ _ _ -> failwith "markExpMutable_ref not initialized")

let propagateVariability_ref : (env -> Loc.t -> arg list option -> exp list -> unit) ref =
  ref (fun _ _ _ _ -> failwith "propagateVariability_ref not initialized")

let unify t1 t2 = !unify_ref t1 t2

let unifyRaise ?bind loc t1 t2 = !unifyRaise_ref ?bind loc t1 t2

let function_def_opt iargs env def_opt = !function_def_opt_ref iargs env def_opt

let stmt_with_type_substitution env type_substitution_map ret_type body =
  !stmt_with_type_substitution_ref env type_substitution_map ret_type body

let insertContextArgument env def = !insertContextArgument_ref env def

let markExpMutable env exp loc = !markExpMutable_ref env exp loc

let propagateVariability env loc args exp_args = !propagateVariability_ref env loc args exp_args

(* ========== Generic Parameter Pairing ========== *)

(* Pair each GParamConstant with its explicit argument. The explicit arguments come in the
   order the explicit generic parameters (constants and functions) appear in param_order,
   so the pairing must count all explicit parameters, not only the constants. *)
let constant_param_pairs (generic_func : Typed.generic_function) (explicit_args : Typed.exp list) :
    ((string * Typed.type_) * Typed.exp) list =
  let params = Array.of_list generic_func.generic_params in
  let explicit = Array.of_list explicit_args in
  let _, rev_pairs =
    CCList.fold_left
      (fun (expl_idx, acc) pk ->
        match pk with
        | Typed.PKArg _ ->
            (expl_idx, acc)
        | Typed.PKGeneric i -> (
            if i >= Array.length params then failwith "Invalid generic param index in param_order" ;
            match params.(i) with
            | Typed.GParamConstant (name, param_type) ->
                if expl_idx < Array.length explicit then (expl_idx + 1, ((name, param_type), explicit.(expl_idx)) :: acc)
                else failwith "Mismatch between constant params and explicit arguments"
            | Typed.GParamType _ | Typed.GParamFunction _ ->
                (expl_idx + 1, acc) ) )
      (0, []) generic_func.param_order
  in
  CCList.rev rev_pairs

(* ========== Generic Function Instantiation ========== *)

(* Create a specialized function from a generic function with resolved types.
   The body is fully processed (nested generic calls are instantiated) while the
   environment is still inside the specialized function, so instances created by
   nested calls are stored in this function's context. *)
let rec instantiate_generic_function (iargs : Args.args) (env : env) (state : instantiation_state)
    (generic_func : Typed.generic_function) (signature : string) (specialized_name : string)
    (call_arg_types : Typed.type_ list) (explicit_args : Typed.exp list) (loc : Loc.t) : Typed.function_def * Typed.stmt
    =
  let all_constants = CCList.length explicit_args > 0 && CCList.for_all is_constant_literal explicit_args in
  let constant_pairs = constant_param_pairs generic_func explicit_args in
  (* Create fresh copies of the generic types, keeping the mapping from the original unbound
     cells so the type parameters can be resolved after unification *)
  let generic_func_arg_types, generic_func_ret_type = generic_func.t in
  let fresh_types, unbound_mapping =
    Typed.copy_types_with_unbound_mapping (generic_func_arg_types @ [generic_func_ret_type])
  in
  let fresh_arg_types, fresh_ret_type =
    match CCList.rev fresh_types with last :: rest -> (CCList.rev rest, last) | [] -> failwith "Empty type list"
  in
  CCList.iter2
    (fun fresh_t call_t ->
      let _ = unify fresh_t call_t in
      () )
    fresh_arg_types call_arg_types ;
  (* Resolve each type parameter to its concrete type using the unified fresh types *)
  let type_substitution_map =
    CCList.map
      (fun (name, orig_t) ->
        let orig_t = unlink orig_t in
        let resolved =
          match orig_t.tx with
          | TEUnbound _ -> (
            match CCList.find_opt (fun (o, _) -> o == orig_t) unbound_mapping with
            | Some (_, fresh_t) ->
                unlink fresh_t
            | None ->
                orig_t )
          | _ ->
              orig_t
        in
        (name, resolved) )
      generic_func.type_param_map
  in
  let constant_substitution_map =
    if all_constants then CCList.map (fun ((name, _), value) -> (name, value)) constant_pairs else []
  in
  let regular_args_array = Array.of_list generic_func.args in
  let fresh_args_array = Array.of_list fresh_arg_types in
  let generic_params_array = Array.of_list generic_func.generic_params in
  let explicit_args_array = Array.of_list explicit_args in
  (* All arguments in declaration order: regular arguments with their fresh types and
     constant parameters turned into regular arguments *)
  let all_args_for_body =
    let _, rev_args =
      CCList.fold_left
        (fun (expl_idx, acc) pk ->
          match pk with
          | Typed.PKArg i ->
              let arg = regular_args_array.(i) in
              let arg = if i < Array.length fresh_args_array then {arg with t= fresh_args_array.(i)} else arg in
              (expl_idx, arg :: acc)
          | Typed.PKGeneric i -> (
              if i >= Array.length generic_params_array then failwith "Invalid generic param index in param_order" ;
              match generic_params_array.(i) with
              | Typed.GParamConstant (name, param_type) ->
                  let t =
                    if expl_idx < Array.length explicit_args_array then explicit_args_array.(expl_idx).t else param_type
                  in
                  (expl_idx + 1, ({name; t; loc} : Typed.arg) :: acc)
              | Typed.GParamType _ | Typed.GParamFunction _ ->
                  (expl_idx + 1, acc) ) )
        (0, []) generic_func.param_order
    in
    CCList.rev rev_args
  in
  (* When all constant parameters are literals they are inlined in the body and dropped
     from the argument list *)
  let specialized_args =
    if all_constants then
      CCList.filter_map
        (fun pk ->
          match pk with
          | Typed.PKArg i ->
              let arg = regular_args_array.(i) in
              Some (if i < Array.length fresh_args_array then {arg with t= fresh_args_array.(i)} else arg)
          | Typed.PKGeneric _ ->
              None )
        generic_func.param_order
    else all_args_for_body
  in
  let env = Env.createContextForFunctionWithIndex env specialized_name loc generic_func.type_index in
  let inferred_ret = C.noreturn loc in
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
  Hashtbl.replace state.instantiated signature (make_def None, {s= StmtBlock []; loc}) ;
  let recursed_without_context = ref false in
  Hashtbl.replace state.in_progress signature recursed_without_context ;
  let env, body = stmt_with_type_substitution env type_substitution_map fresh_ret_type generic_func.body in
  let body = CCList.map (resolveTypeIntrinsicsInStmt type_substitution_map) body in
  let body =
    if CCList.length constant_substitution_map > 0 then
      CCList.map (substituteConstantsInStmt constant_substitution_map) body
    else body
  in
  let combined_body = match body with [single] -> single | stmts -> {s= StmtBlock stmts; loc= generic_func.loc} in
  (* Process nested generic calls while still inside the specialized function so instances
     created by them are stored in this function's context *)
  let () = prescan_generic_calls_in_stmt iargs env state combined_body in
  let processed_body = process_stmt_instantiation iargs env state combined_body in
  Hashtbl.remove state.in_progress signature ;
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
  let next = process_companion_bodies iargs env state next in
  let specialized_def = make_def next in
  let specialized_def = insertContextArgument env specialized_def in
  let env = Env.exitContext env in
  let _ = env in
  let () =
    if !recursed_without_context then
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
  (specialized_def, processed_body)

(* Process the bodies of companion functions so generic calls inside them are instantiated.
   Instances created by them are stored in the context shared with the parent specialization. *)
and process_companion_bodies (iargs : Args.args) (env : env) (state : instantiation_state)
    (next : (Typed.function_def * Typed.stmt) option) : (Typed.function_def * Typed.stmt) option =
  match next with
  | None ->
      None
  | Some (cdef, cbody) ->
      let cenv = Env.reenterFunction env cdef.name in
      let () = prescan_generic_calls_in_stmt iargs cenv state cbody in
      let cbody = process_stmt_instantiation iargs cenv state cbody in
      let cnext = process_companion_bodies iargs env state cdef.next in
      Some ({cdef with next= cnext}, cbody)

(* ========== Prescan for Generic Calls ========== *)

and prescan_generic_calls_in_stmt (iargs : Args.args) (env : env) (state : instantiation_state) (stmt : Typed.stmt) :
    unit =
  match stmt.s with
  | StmtVal _ ->
      ()
  | StmtReturn e ->
      prescan_generic_calls_in_exp iargs env state e
  | StmtBind (lhs, e) ->
      prescan_generic_calls_in_lexp iargs env state lhs ;
      prescan_generic_calls_in_exp iargs env state e
  | StmtIf (cond, then_s, else_opt) ->
      prescan_generic_calls_in_exp iargs env state cond ;
      prescan_generic_calls_in_stmt iargs env state then_s ;
      Option.iter (prescan_generic_calls_in_stmt iargs env state) else_opt
  | StmtWhile (cond, body) ->
      prescan_generic_calls_in_exp iargs env state cond ;
      prescan_generic_calls_in_stmt iargs env state body
  | StmtBlock stmts ->
      CCList.iter (prescan_generic_calls_in_stmt iargs env state) stmts
  | StmtMem _ ->
      ()

and prescan_generic_calls_in_lexp (iargs : Args.args) (env : env) (state : instantiation_state) (l : Typed.lexp) : unit
    =
  match l.l with
  | LWild | LId _ ->
      ()
  | LMember (le, _) ->
      prescan_generic_calls_in_lexp iargs env state le
  | LIndex {e= le; index} ->
      prescan_generic_calls_in_lexp iargs env state le ;
      prescan_generic_calls_in_exp iargs env state index
  | LTuple elems ->
      CCList.iter (prescan_generic_calls_in_lexp iargs env state) elems

and prescan_generic_calls_in_exp (iargs : Args.args) (env : env) (state : instantiation_state) (e : Typed.exp) : unit =
  match e.e with
  | EGenCall {args; explicit_args; _} ->
      state.pending_generic_calls <- e :: state.pending_generic_calls ;
      CCList.iter (prescan_generic_calls_in_exp iargs env state) args ;
      CCList.iter (prescan_generic_calls_in_exp iargs env state) explicit_args
  | ECall {args; _} ->
      CCList.iter (prescan_generic_calls_in_exp iargs env state) args
  | EIf {cond; then_; else_} ->
      prescan_generic_calls_in_exp iargs env state cond ;
      prescan_generic_calls_in_exp iargs env state then_ ;
      prescan_generic_calls_in_exp iargs env state else_
  | EOp (_, lhs, rhs) ->
      prescan_generic_calls_in_exp iargs env state lhs ;
      prescan_generic_calls_in_exp iargs env state rhs
  | EUnOp (_, arg) ->
      prescan_generic_calls_in_exp iargs env state arg
  | EIndex {e= inner; index} ->
      prescan_generic_calls_in_exp iargs env state inner ;
      prescan_generic_calls_in_exp iargs env state index
  | EArray elems | ETuple elems ->
      CCList.iter (prescan_generic_calls_in_exp iargs env state) elems
  | EMember (inner, _) ->
      prescan_generic_calls_in_exp iargs env state inner
  | ERecord {elems; _} ->
      CCList.iter (fun (_, v) -> prescan_generic_calls_in_exp iargs env state v) elems
  | EGenCompanionCall {args; _} ->
      CCList.iter (prescan_generic_calls_in_exp iargs env state) args
  | EId _ | EUnit | EBool _ | EInt _ | EReal _ | EFixed _ | EString _ | EConst _ | ETypeIntrinsic _ ->
      ()

(* ========== Expression and Statement Processing ========== *)

and process_exp_instantiation (iargs : Args.args) (env : env) (state : instantiation_state) (e : Typed.exp) : Typed.exp
    =
  let loc = e.loc in
  match e.e with
  | EGenCall {instance; generic_path; args; explicit_args} -> (
      let generic_name = Pla.print (Syntax.print_path generic_path) in
      match Env.lookupGeneric env generic_path with
      | None ->
          Error.raiseError (Printf.sprintf "Generic function '%s' not found" generic_name) loc
      | Some generic_func ->
          let resolved_arg_types = CCList.map (fun (a : Typed.exp) -> unlink a.t) args in
          let has_explicit = CCList.length explicit_args > 0 in
          let all_constants = has_explicit && CCList.for_all is_constant_literal explicit_args in
          let specialized_name =
            if all_constants then build_specialized_signature_string generic_func.name resolved_arg_types explicit_args
            else if has_explicit then build_nonspec_signature_string generic_func.name resolved_arg_types explicit_args
            else build_signature_string generic_func.name resolved_arg_types
          in
          let generic_module = generic_path.n in
          (* The deduplication key includes the module that owns the generic so a qualified and
             an unqualified call to the same generic map to the same specialization, and
             same-named generics in different modules do not collide *)
          let owner_module = match generic_module with Some m -> m | None -> (Env.getCurrentModule env).name in
          let signature = owner_module ^ "." ^ specialized_name in
          let specialized_def, in_progress =
            match Hashtbl.find_opt state.instantiated signature with
            | Some (def, _) ->
                (def, Hashtbl.find_opt state.in_progress signature)
            | None -> (
                let env_for_instantiation =
                  match generic_module with Some module_name -> Env.enterModule env module_name | None -> env
                in
                let existing =
                  if state.reuse_env_specializations then
                    Env.tryLookFunctionCall env_for_instantiation {id= specialized_name; n= None; loc}
                  else None
                in
                match existing with
                | Some f ->
                    (* The specialization was created by a previous elaboration of the program:
                       reuse it instead of instantiating it again *)
                    let base_args = match f.args with Some a -> a | None -> [] in
                    let def_args =
                      if Env.isFunctionActive f then
                        match Env.lookVarInScopes f.locals context_name with
                        | Some var ->
                            ({name= context_name; t= var.t; loc} : Typed.arg) :: base_args
                        | None ->
                            base_args
                      else base_args
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
                    Hashtbl.replace state.instantiated signature (def, {s= StmtBlock []; loc}) ;
                    (def, None)
                | None ->
                    let def, processed_body =
                      instantiate_generic_function iargs env_for_instantiation state generic_func signature
                        specialized_name resolved_arg_types explicit_args loc
                    in
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
                    Hashtbl.replace state.instantiated signature (def, processed_body) ;
                    state.pending_functions <-
                      (target_module, generic_func.name, def, processed_body) :: state.pending_functions ;
                    (def, None) )
          in
          let processed_regular_args = CCList.map (process_exp_instantiation iargs env state) args in
          let processed_explicit_args = CCList.map (process_exp_instantiation iargs env state) explicit_args in
          let processed_args =
            if all_constants then
              (* Constants are inlined in the specialized body: pass only the regular arguments *)
              let regular_args_array = Array.of_list processed_regular_args in
              CCList.filter_map
                (fun pk ->
                  match pk with
                  | Typed.PKArg i ->
                      if i < Array.length regular_args_array then Some regular_args_array.(i)
                      else failwith "Invalid arg index in param_order"
                  | Typed.PKGeneric _ ->
                      None )
                generic_func.param_order
            else
              (* Interleave regular arguments and explicit generic arguments in declaration order *)
              let regular_args_array = Array.of_list processed_regular_args in
              let generic_params_array = Array.of_list generic_func.generic_params in
              let explicit_args_array = Array.of_list processed_explicit_args in
              let _, rev_args =
                CCList.fold_left
                  (fun (expl_idx, acc) pk ->
                    match pk with
                    | Typed.PKArg i ->
                        if i < Array.length regular_args_array then (expl_idx, regular_args_array.(i) :: acc)
                        else failwith "Invalid arg index in param_order"
                    | Typed.PKGeneric i -> (
                        if i >= Array.length generic_params_array then
                          failwith "Invalid generic param index in param_order" ;
                        match generic_params_array.(i) with
                        | Typed.GParamConstant _ ->
                            if expl_idx < Array.length explicit_args_array then
                              (expl_idx + 1, explicit_args_array.(expl_idx) :: acc)
                            else failwith "Invalid constant param index in param_order"
                        | Typed.GParamType _ | Typed.GParamFunction _ ->
                            (expl_idx + 1, acc) ) )
                  (0, []) generic_func.param_order
              in
              CCList.rev rev_args
          in
          let specialized_non_ctx_args =
            match specialized_def.args with
            | {name; _} :: rest when String.equal name context_name ->
                rest
            | args ->
                args
          in
          let () = propagateVariability env loc (Some specialized_non_ctx_args) processed_args in
          let final_args =
            match in_progress with
            | Some flag ->
                (* Recursive call to a specialization that is being instantiated right now.
                   When it is a direct self-call and the function has state, the context is
                   passed through unchanged (same instance). *)
                let current_f = Env.getCurrentFunction env in
                if Syntax.compare_path current_f.path specialized_def.name = 0 && Env.isFunctionActive current_f then
                  match Env.lookVarInScopes current_f.locals context_name with
                  | Some var ->
                      {e= EId context_name; t= var.t; loc} :: processed_args
                  | None ->
                      failwith "context var not declared in recursive generic function"
                else (
                  flag := true ;
                  processed_args )
            | None -> (
              match specialized_def.args with
              | {name; t= ctx_t; _} :: _ when String.equal name context_name ->
                  let current_f = Env.getCurrentFunction env in
                  let current_ctx_t =
                    match Env.lookVarInScopes current_f.locals context_name with
                    | Some var ->
                        var.t
                    | None ->
                        failwith "context var not declared in caller"
                  in
                  let inst_name =
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
                        let _ = Env.addVar env unify user_inst_name ctx_t Inst loc in
                        user_inst_name
                    | None -> (
                        let site = callSiteKey env loc in
                        match Hashtbl.find_opt state.call_instance_names site with
                        | Some name ->
                            let _ = Env.addVar env unify name ctx_t Inst loc in
                            name
                        | None ->
                            let number =
                              Printf.sprintf "%.2x%.2x"
                                (0xFF land Hashtbl.hash (pathString specialized_def.name))
                                (0xFF land Hashtbl.hash (pathString (Env.getContext env)))
                            in
                            let rec generateName () =
                              let n = Env.getFunctionTick env in
                              let name = "inst_" ^ string_of_int n ^ number in
                              if checkMemExists env name || Env.checkConstantExists env name then generateName ()
                              else name
                            in
                            let name = generateName () in
                            let _ = Env.addVar env unify name ctx_t Inst loc in
                            Hashtbl.add state.call_instance_names site name ;
                            name )
                  in
                  let ctx_e = {e= EId context_name; t= current_ctx_t; loc} in
                  let inst_e = {e= EMember (ctx_e, inst_name); t= ctx_t; loc} in
                  inst_e :: processed_args
              | _ ->
                  processed_args )
          in
          {e= ECall {instance= None; path= specialized_def.name; args= final_args}; t= e.t; loc} )
  | ECall {instance; path; args} ->
      let processed_args = CCList.map (process_exp_instantiation iargs env state) args in
      let () =
        match Env.tryLookFunctionCall env path with
        | Some f ->
            let func_non_ctx_args =
              match f.args with
              | Some ({name; _} :: rest) when String.equal name context_name ->
                  Some rest
              | args ->
                  args
            in
            propagateVariability env loc func_non_ctx_args processed_args
        | None ->
            ()
      in
      let func_path_str = pathString path in
      let final_args =
        match Hashtbl.find_opt state.functions_needing_context func_path_str with
        | Some ctx_t ->
            let current_f = Env.getCurrentFunction env in
            let current_ctx_t =
              match Env.lookVarInScopes current_f.locals context_name with
              | Some var ->
                  var.t
              | None ->
                  failwith
                    (Printf.sprintf "Function '%s' calls '%s' which needs context, but caller has no context"
                       (pathString current_f.path) func_path_str )
            in
            let inst_name =
              let site = callSiteKey env loc in
              match Hashtbl.find_opt state.call_instance_names site with
              | Some name ->
                  let _ = Env.addVar env unify name ctx_t Inst loc in
                  name
              | None ->
                  let number =
                    Printf.sprintf "%.2x%.2x"
                      (0xFF land Hashtbl.hash func_path_str)
                      (0xFF land Hashtbl.hash (pathString (Env.getContext env)))
                  in
                  let rec generateName () =
                    let n = Env.getFunctionTick env in
                    let name = "inst_" ^ string_of_int n ^ number in
                    if checkMemExists env name || Env.checkConstantExists env name then generateName () else name
                  in
                  let name = generateName () in
                  let _ = Env.addVar env unify name ctx_t Inst loc in
                  Hashtbl.add state.call_instance_names site name ;
                  name
            in
            let ctx_e = {e= EId context_name; t= current_ctx_t; loc} in
            let inst_e = {e= EMember (ctx_e, inst_name); t= ctx_t; loc} in
            inst_e :: processed_args
        | None ->
            processed_args
      in
      {e with e= ECall {instance; path; args= final_args}}
  | EOp (op, e1, e2) ->
      let e1 = process_exp_instantiation iargs env state e1 in
      let e2 = process_exp_instantiation iargs env state e2 in
      {e with e= EOp (op, e1, e2)}
  | EUnOp (op, e1) ->
      let e1 = process_exp_instantiation iargs env state e1 in
      {e with e= EUnOp (op, e1)}
  | EIf {cond; then_; else_} ->
      let cond = process_exp_instantiation iargs env state cond in
      let then_ = process_exp_instantiation iargs env state then_ in
      let else_ = process_exp_instantiation iargs env state else_ in
      {e with e= EIf {cond; then_; else_}}
  | EIndex {e= arr; index} ->
      let arr = process_exp_instantiation iargs env state arr in
      let index = process_exp_instantiation iargs env state index in
      {e with e= EIndex {e= arr; index}}
  | EArray elems ->
      let elems = CCList.map (process_exp_instantiation iargs env state) elems in
      {e with e= EArray elems}
  | ETuple elems ->
      let elems = CCList.map (process_exp_instantiation iargs env state) elems in
      {e with e= ETuple elems}
  | EMember (e1, m) ->
      let e1 = process_exp_instantiation iargs env state e1 in
      {e with e= EMember (e1, m)}
  | ERecord {path; elems} ->
      let elems = CCList.map (fun (n, v) -> (n, process_exp_instantiation iargs env state v)) elems in
      {e with e= ERecord {path; elems}}
  | EGenCompanionCall {instance; companion_name; parent_generic_path; args} -> (
    match Env.lookupGeneric env parent_generic_path with
    | None ->
        Error.raiseError
          (Printf.sprintf "Parent generic function '%s' not found for companion '%s'" (pathString parent_generic_path)
             companion_name )
          loc
    | Some _parent_generic -> (
        let parent_name = parent_generic_path.id in
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
        let collect_candidates () =
          CCList.filter_map
            (fun (_module, gen_name, (def : Typed.function_def), _body) ->
              if String.equal gen_name parent_name then
                match find_companion_in def with Some companion -> Some (def, companion) | None -> None
              else None )
            state.pending_functions
        in
        let candidates = collect_candidates () in
        let candidates =
          if CCList.is_empty candidates then
            (* The companion may be called before the parent: instantiate the parent call
               found during the prescan of this function *)
            match
              CCList.find_opt
                (fun (pending_e : Typed.exp) ->
                  match pending_e.e with
                  | EGenCall {generic_path; _} ->
                      String.equal generic_path.id parent_name
                  | _ ->
                      false )
                state.pending_generic_calls
            with
            | Some parent_call ->
                let _ = process_exp_instantiation iargs env state parent_call in
                collect_candidates ()
            | None ->
                []
          else candidates
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
            let processed_args = CCList.map (process_exp_instantiation iargs env state) args in
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
                          let inst_ctx = match (unlink t).tx with TEId p -> Some (pathString p) | _ -> None in
                          match
                            CCList.find_opt
                              (fun ((spec : Typed.function_def), _) ->
                                match (ctxTypeNameOf spec, inst_ctx) with
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
                  loc ;
              CCList.iter2
                (fun (def_arg : Typed.arg) (call_arg : Typed.exp) -> unifyRaise call_arg.loc def_arg.t call_arg.t)
                companion_non_ctx_args processed_args
            in
            let final_args =
              match (companion_def.args, inst_name_opt) with
              | {name; t= ctx_t; _} :: _, Some inst_name when String.equal name context_name ->
                  let current_f = Env.getCurrentFunction env in
                  let current_ctx_t =
                    match Env.lookVarInScopes current_f.locals context_name with
                    | Some var ->
                        var.t
                    | None ->
                        failwith "context var not declared in caller for companion call"
                  in
                  let _ = Env.addVar env unify inst_name ctx_t Inst loc in
                  let ctx_e = {e= EId context_name; t= current_ctx_t; loc} in
                  let inst_e = {e= EMember (ctx_e, inst_name); t= ctx_t; loc} in
                  inst_e :: processed_args
              | _ ->
                  processed_args
            in
            {e= ECall {instance= None; path= companion_def.name; args= final_args}; t= e.t; loc} ) )
  | ETypeIntrinsic {intrinsic; type_param} ->
      let intrinsic_name =
        match intrinsic with TypeDefault -> "typedefault" | TypeMax -> "typemax" | TypeMin -> "typemin"
      in
      Error.raiseError
        (Printf.sprintf "Type intrinsic '%s('%s)' was not resolved - it must be used inside a generic function"
           intrinsic_name type_param )
        e.loc
  | EUnit | EBool _ | EInt _ | EReal _ | EFixed _ | EString _ | EId _ | EConst _ ->
      e

and process_lexp_instantiation (iargs : Args.args) (env : env) (state : instantiation_state) (l : Typed.lexp) :
    Typed.lexp =
  match l.l with
  | LWild | LId _ ->
      l
  | LMember (le, m) ->
      let le = process_lexp_instantiation iargs env state le in
      {l with l= LMember (le, m)}
  | LIndex {e= le; index} ->
      let le = process_lexp_instantiation iargs env state le in
      let index = process_exp_instantiation iargs env state index in
      {l with l= LIndex {e= le; index}}
  | LTuple elems ->
      let elems = CCList.map (process_lexp_instantiation iargs env state) elems in
      {l with l= LTuple elems}

and process_stmt_instantiation (iargs : Args.args) (env : env) (state : instantiation_state) (s : Typed.stmt) :
    Typed.stmt =
  match s.s with
  | StmtVal d ->
      {s with s= StmtVal d}
  | StmtMem (d, tags) ->
      {s with s= StmtMem (d, tags)}
  | StmtBind (lhs, rhs) ->
      let lhs = process_lexp_instantiation iargs env state lhs in
      let rhs = process_exp_instantiation iargs env state rhs in
      {s with s= StmtBind (lhs, rhs)}
  | StmtReturn e ->
      let e = process_exp_instantiation iargs env state e in
      {s with s= StmtReturn e}
  | StmtIf (cond, then_, else_opt) ->
      let cond = process_exp_instantiation iargs env state cond in
      let then_ = process_stmt_instantiation iargs env state then_ in
      let else_opt = Option.map (process_stmt_instantiation iargs env state) else_opt in
      {s with s= StmtIf (cond, then_, else_opt)}
  | StmtWhile (cond, body) ->
      let cond = process_exp_instantiation iargs env state cond in
      let body = process_stmt_instantiation iargs env state body in
      {s with s= StmtWhile (cond, body)}
  | StmtBlock stmts ->
      let stmts = CCList.map (process_stmt_instantiation iargs env state) stmts in
      {s with s= StmtBlock stmts}

(* ========== Function Definition Processing ========== *)

let rec process_function_def (iargs : Args.args) (env : env) (state : instantiation_state) (def : Typed.function_def)
    (body : Typed.stmt) : Typed.function_def * Typed.stmt =
  let env = Env.reenterFunction env def.name in
  (* Pending generic calls are per-function: they are only used to resolve companion calls
     that happen before the parent call within the same function *)
  let () = state.pending_generic_calls <- [] in
  let () = prescan_generic_calls_in_stmt iargs env state body in
  let body = process_stmt_instantiation iargs env state body in
  let had_ctx_before = match def.args with {name; _} :: _ when String.equal name context_name -> true | _ -> false in
  let def = insertContextArgument env def in
  let () =
    if not had_ctx_before then
      match def.args with
      | {name; t= ctx_t; _} :: _ when String.equal name context_name ->
          let func_path_str = pathString def.name in
          Hashtbl.replace state.functions_needing_context func_path_str ctx_t
      | _ ->
          ()
  in
  let next =
    match def.next with
    | None ->
        None
    | Some (next_def, next_body) ->
        let next_def, next_body = process_function_def iargs env state next_def next_body in
        Some (next_def, next_body)
  in
  ({def with next}, body)

(* ========== Top-Level Processing ========== *)

let process_top_stmt_instantiation (iargs : Args.args) (env : env) (state : instantiation_state) (t : Typed.top_stmt) :
    Typed.top_stmt =
  match t.top with
  | TopFunction (def, body) ->
      let def, body = process_function_def iargs env state def body in
      {t with top= TopFunction (def, body)}
  | TopGenericPlaceholder _ ->
      t
  | TopExternal _ | TopType _ | TopEnum _ | TopConstant _ | TopAlias _ ->
      t

let transform_module_generics (iargs : Args.args) (env : env) (state : instantiation_state) (stmts : Typed.top_stmt list)
    : Typed.top_stmt list =
  (* The statements arrive in reverse source order. Process them in source order so callees
     are processed before their callers: functions that gain a context argument during
     elaboration are registered before any call to them is processed, allowing the context
     requirement to propagate through call chains of any depth in a single pass. *)
  let processed = CCList.map (process_top_stmt_instantiation iargs env state) (CCList.rev stmts) in
  CCList.rev processed

let replace_placeholders_in_module (state : instantiation_state) (module_name : string) (stmts : Typed.top_stmt list) :
    Typed.top_stmt list =
  CCList.flat_map
    (fun (stmt : Typed.top_stmt) ->
      match stmt.top with
      | TopGenericPlaceholder generic_name ->
          let for_this_generic, remaining =
            CCList.partition
              (fun (m, gname, _, _) -> String.equal m module_name && String.equal gname generic_name)
              state.pending_functions
          in
          state.pending_functions <- remaining ;
          CCList.map
            (fun (_, _, def, body) -> {top= TopFunction (def, body); loc= def.loc})
            (CCList.rev for_this_generic)
      | _ ->
          [stmt] )
    stmts

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
    @return The elaborated top-level statements *)
let elaborate ?(reuse_existing = false) (iargs : Args.args) (env : env)
    (module_stmts : (string * Typed.top_stmt list) list) : Typed.top_stmt list =
  let instantiation_state = create_instantiation_state reuse_existing in
  (* Pass 1: Transform all EGenCall to ECall across all modules *)
  let transformed_stmts =
    CCList.map
      (fun (module_name, stmts) ->
        let env = Env.enterModule env module_name in
        let stmts = transform_module_generics iargs env instantiation_state stmts in
        let _ = Env.exitModule env in
        (module_name, stmts) )
      module_stmts
  in
  (* Pass 2: Replace placeholders with specialized functions *)
  let final_stmts =
    CCList.fold_left
      (fun acc (module_name, stmts) ->
        let stmts = replace_placeholders_in_module instantiation_state module_name stmts in
        stmts @ acc )
      [] transformed_stmts
  in
  (* Specializations whose generic placeholder is not part of these statements (e.g. when
     elaborating an expression against an existing program) are emitted before everything else *)
  let leftover =
    CCList.map
      (fun (_, _, def, body) -> {top= TopFunction (def, body); loc= def.loc})
      (CCList.rev instantiation_state.pending_functions)
  in
  leftover @ CCList.rev final_stmts
