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

let path_string (p : Syntax.path) : string = match p with {id; n= None; _} -> id | {id; n= Some n; _} -> n ^ "_" ^ id

let rec unlink (t : type_) = match t.tx with TELink t -> unlink t | _ -> t

let checkMemExists (env : env) name =
  let f = Env.getCurrentFunction env in
  (* Check context record where Inst/Mem variables are stored by addVar *)
  match f.context with
  | Some (_, {descr= Record members; _}) -> (
    match Env.Map.find name members with Some {kind= Mem _ | Inst; _} -> true | _ -> false )
  | _ ->
      false

(* ========== Type Mangling ========== *)

(* Convert type to mangled name for specialized function names *)
let rec type_to_mangled_name (t : Typed.type_) : string =
  match (unlink t).tx with
  | TEId {id; n= None; _} ->
      id
  | TEId {id; n= Some module_name; _} ->
      module_name ^ "_" ^ id
  | TEFunction (arg_types, ret_type) ->
      let args_str = CCList.map type_to_mangled_name arg_types |> String.concat "_" in
      let ret_str = type_to_mangled_name ret_type in
      "fn_" ^ args_str ^ "_to_" ^ ret_str
  | TEComposed (name, type_args) ->
      let args_str = CCList.map type_to_mangled_name type_args |> String.concat "_" in
      if args_str = "" then name else name ^ "_of_" ^ args_str
  | TEUnbound (Some id) ->
      "unbound" ^ string_of_int id
  | TEUnbound None ->
      "unbound"
  | TEOption type_list -> (
    match type_list with
    | [single_type] ->
        type_to_mangled_name single_type
    | multiple_types -> (
        let non_option_types =
          CCList.filter (fun t -> match (unlink t).tx with TEOption _ -> false | _ -> true) multiple_types
        in
        match non_option_types with
        | concrete_type :: _ ->
            type_to_mangled_name concrete_type
        | [] ->
            "opt_" ^ (CCList.map type_to_mangled_name type_list |> String.concat "_") ) )
  | TENoReturn ->
      "noreturn"
  | TESize i ->
      "size_" ^ string_of_int i
  | TELink _ ->
      "link"

(* ========== Constant Literal Handling ========== *)

(* Check if a typed expression is a compile-time constant literal *)
let is_constant_literal (e : Typed.exp) : bool =
  match e.e with EInt _ | EReal _ | EBool _ | EString _ | EFixed _ -> true | _ -> false

(* Convert a constant literal expression to a string for signature encoding *)
let constant_to_signature_string (e : Typed.exp) : string =
  match e.e with
  | EInt n ->
      if n < 0 then "n" ^ string_of_int (abs n) else string_of_int n
  | EReal f ->
      let s = Printf.sprintf "%.6g" f in
      let s = Str.global_replace (Str.regexp_string ".") "_" s in
      let s = Str.global_replace (Str.regexp_string "-") "n" s in
      s
  | EBool b ->
      if b then "true" else "false"
  | EString s ->
      Printf.sprintf "s%x" (Hashtbl.hash s land 0xFFFF)
  | EFixed f ->
      let s = Printf.sprintf "%.6g" f in
      let s = Str.global_replace (Str.regexp_string ".") "_" s in
      let s = Str.global_replace (Str.regexp_string "-") "n" s in
      "fx" ^ s
  | _ ->
      "var"

(* ========== Signature Building ========== *)

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
let build_nonspec_signature_string (generic_name : string) (_arg_types : Typed.type_ list) : string = generic_name

(* ========== Instantiation State ========== *)

(* State for tracking instantiated generic functions during post-processing *)
type instantiation_state =
  { mutable instantiated: (string, Typed.function_def * Typed.stmt) Hashtbl.t
  ; mutable pending_functions: (string * string * Typed.function_def * Typed.stmt) list
  ; mutable functions_needing_context: (string, Typed.type_) Hashtbl.t
  ; mutable processed_companions: (string, (Typed.function_def * Typed.stmt) option) Hashtbl.t
  ; mutable pending_generic_calls: Typed.exp list }

let create_instantiation_state () : instantiation_state =
  { instantiated= Hashtbl.create 16
  ; pending_functions= []
  ; functions_needing_context= Hashtbl.create 16
  ; processed_companions= Hashtbl.create 16
  ; pending_generic_calls= [] }

(* ========== Type Intrinsic Resolution ========== *)

(** Resolves a type intrinsic to a concrete expression during generic instantiation. *)
let resolve_type_intrinsic_inline (intrinsic : Typed.type_intrinsic) (concrete_type : Typed.type_) (loc : Loc.t) :
    Typed.exp =
  let t = concrete_type in
  let unlinked = unlink concrete_type in
  match (intrinsic, unlinked.tx) with
  | TypeDefault, TEId {id= "int"; _} ->
      {e= EInt 0; t; loc}
  | TypeDefault, TEId {id= "int16"; _} ->
      {e= EInt 0; t; loc}
  | TypeDefault, TEId {id= "real"; _} ->
      {e= EReal 0.0; t; loc}
  | TypeDefault, TEId {id= "fix16"; _} ->
      {e= EFixed 0.0; t; loc}
  | TypeDefault, TEId {id= "bool"; _} ->
      {e= EBool false; t; loc}
  | TypeDefault, TEId {id= "string"; _} ->
      {e= EString ""; t; loc}
  | TypeMax, TEId {id= "int"; _} ->
      {e= EInt 2147483647; t; loc}
  | TypeMax, TEId {id= "int16"; _} ->
      {e= EInt 32767; t; loc}
  | TypeMax, TEId {id= "real"; _} ->
      {e= EReal 3.40282347e+38; t; loc}
  | TypeMax, TEId {id= "fix16"; _} ->
      {e= EFixed 32767.99998; t; loc}
  | TypeMax, TEId {id= "bool"; _} ->
      {e= EBool true; t; loc}
  | TypeMin, TEId {id= "int"; _} ->
      {e= EInt (-2147483648); t; loc}
  | TypeMin, TEId {id= "int16"; _} ->
      {e= EInt (-32768); t; loc}
  | TypeMin, TEId {id= "real"; _} ->
      {e= EReal (-3.40282347e+38); t; loc}
  | TypeMin, TEId {id= "fix16"; _} ->
      {e= EFixed (-32768.0); t; loc}
  | TypeMin, TEId {id= "bool"; _} ->
      {e= EBool false; t; loc}
  | TypeMax, _ ->
      let type_str = Pla.print (Typed.print_type_ concrete_type) in
      Error.raiseError (Printf.sprintf "typemax() is not supported for type '%s'" type_str) loc
  | TypeMin, _ ->
      let type_str = Pla.print (Typed.print_type_ concrete_type) in
      Error.raiseError (Printf.sprintf "typemin() is not supported for type '%s'" type_str) loc
  | TypeDefault, _ ->
      let type_str = Pla.print (Typed.print_type_ concrete_type) in
      Error.raiseError (Printf.sprintf "typedefault() is not supported for type '%s'" type_str) loc

(** Resolves type intrinsics in an expression tree using the type substitution map. *)
let rec resolve_type_intrinsics_in_exp (type_substitution_map : (string * type_) list) (e : Typed.exp) : Typed.exp =
  match e.e with
  | ETypeIntrinsic {intrinsic; type_param} -> (
    match CCList.assoc_opt ~eq:String.equal type_param type_substitution_map with
    | Some concrete_type ->
        resolve_type_intrinsic_inline intrinsic concrete_type e.loc
    | None ->
        Error.raiseError (Printf.sprintf "Type parameter '%s' not found in generic bindings" type_param) e.loc )
  | ECall {instance; path; args} ->
      let args = CCList.map (resolve_type_intrinsics_in_exp type_substitution_map) args in
      {e with e= ECall {instance; path; args}}
  | EOp (op, e1, e2) ->
      let e1 = resolve_type_intrinsics_in_exp type_substitution_map e1 in
      let e2 = resolve_type_intrinsics_in_exp type_substitution_map e2 in
      {e with e= EOp (op, e1, e2)}
  | EUnOp (op, e1) ->
      let e1 = resolve_type_intrinsics_in_exp type_substitution_map e1 in
      {e with e= EUnOp (op, e1)}
  | EIf {cond; then_; else_} ->
      let cond = resolve_type_intrinsics_in_exp type_substitution_map cond in
      let then_ = resolve_type_intrinsics_in_exp type_substitution_map then_ in
      let else_ = resolve_type_intrinsics_in_exp type_substitution_map else_ in
      {e with e= EIf {cond; then_; else_}}
  | EIndex {e= arr; index} ->
      let arr = resolve_type_intrinsics_in_exp type_substitution_map arr in
      let index = resolve_type_intrinsics_in_exp type_substitution_map index in
      {e with e= EIndex {e= arr; index}}
  | EArray elems ->
      let elems = CCList.map (resolve_type_intrinsics_in_exp type_substitution_map) elems in
      {e with e= EArray elems}
  | ETuple elems ->
      let elems = CCList.map (resolve_type_intrinsics_in_exp type_substitution_map) elems in
      {e with e= ETuple elems}
  | EMember (e1, m) ->
      let e1 = resolve_type_intrinsics_in_exp type_substitution_map e1 in
      {e with e= EMember (e1, m)}
  | ERecord {path; elems} ->
      let elems = CCList.map (fun (n, v) -> (n, resolve_type_intrinsics_in_exp type_substitution_map v)) elems in
      {e with e= ERecord {path; elems}}
  | EGenCall {instance; generic_path; args; explicit_args} ->
      let args = CCList.map (resolve_type_intrinsics_in_exp type_substitution_map) args in
      let explicit_args = CCList.map (resolve_type_intrinsics_in_exp type_substitution_map) explicit_args in
      {e with e= EGenCall {instance; generic_path; args; explicit_args}}
  | EGenCompanionCall {instance; companion_name; parent_generic_path; args} ->
      let args = CCList.map (resolve_type_intrinsics_in_exp type_substitution_map) args in
      {e with e= EGenCompanionCall {instance; companion_name; parent_generic_path; args}}
  | EUnit | EBool _ | EInt _ | EReal _ | EFixed _ | EString _ | EId _ | EConst _ ->
      e

(** Resolves type intrinsics in a statement tree. *)
let rec resolve_type_intrinsics_in_stmt (type_substitution_map : (string * type_) list) (s : Typed.stmt) : Typed.stmt =
  match s.s with
  | StmtVal _ ->
      s
  | StmtMem (_, _) ->
      s
  | StmtBind (lhs, rhs) ->
      let rhs = resolve_type_intrinsics_in_exp type_substitution_map rhs in
      {s with s= StmtBind (lhs, rhs)}
  | StmtReturn e ->
      let e = resolve_type_intrinsics_in_exp type_substitution_map e in
      {s with s= StmtReturn e}
  | StmtIf (cond, then_, else_opt) ->
      let cond = resolve_type_intrinsics_in_exp type_substitution_map cond in
      let then_ = resolve_type_intrinsics_in_stmt type_substitution_map then_ in
      let else_opt = Option.map (resolve_type_intrinsics_in_stmt type_substitution_map) else_opt in
      {s with s= StmtIf (cond, then_, else_opt)}
  | StmtWhile (cond, body) ->
      let cond = resolve_type_intrinsics_in_exp type_substitution_map cond in
      let body = resolve_type_intrinsics_in_stmt type_substitution_map body in
      {s with s= StmtWhile (cond, body)}
  | StmtBlock stmts ->
      let stmts = CCList.map (resolve_type_intrinsics_in_stmt type_substitution_map) stmts in
      {s with s= StmtBlock stmts}

(* ========== Constant Substitution ========== *)

(** Substitutes constant parameter references with their literal values in expressions. *)
let rec substitute_constants_in_exp (constant_map : (string * Typed.exp) list) (e : Typed.exp) : Typed.exp =
  match e.e with
  | EId name -> (
    match CCList.assoc_opt ~eq:String.equal name constant_map with
    | Some const_exp ->
        {const_exp with loc= e.loc; t= e.t}
    | None ->
        e )
  | ECall {instance; path; args} ->
      let args = CCList.map (substitute_constants_in_exp constant_map) args in
      {e with e= ECall {instance; path; args}}
  | EOp (op, e1, e2) ->
      let e1 = substitute_constants_in_exp constant_map e1 in
      let e2 = substitute_constants_in_exp constant_map e2 in
      {e with e= EOp (op, e1, e2)}
  | EUnOp (op, e1) ->
      let e1 = substitute_constants_in_exp constant_map e1 in
      {e with e= EUnOp (op, e1)}
  | EIf {cond; then_; else_} ->
      let cond = substitute_constants_in_exp constant_map cond in
      let then_ = substitute_constants_in_exp constant_map then_ in
      let else_ = substitute_constants_in_exp constant_map else_ in
      {e with e= EIf {cond; then_; else_}}
  | EIndex {e= arr; index} ->
      let arr = substitute_constants_in_exp constant_map arr in
      let index = substitute_constants_in_exp constant_map index in
      {e with e= EIndex {e= arr; index}}
  | EArray elems ->
      let elems = CCList.map (substitute_constants_in_exp constant_map) elems in
      {e with e= EArray elems}
  | ETuple elems ->
      let elems = CCList.map (substitute_constants_in_exp constant_map) elems in
      {e with e= ETuple elems}
  | EMember (e1, m) ->
      let e1 = substitute_constants_in_exp constant_map e1 in
      {e with e= EMember (e1, m)}
  | ERecord {path; elems} ->
      let elems = CCList.map (fun (n, v) -> (n, substitute_constants_in_exp constant_map v)) elems in
      {e with e= ERecord {path; elems}}
  | EGenCall {instance; generic_path; args; explicit_args} ->
      let args = CCList.map (substitute_constants_in_exp constant_map) args in
      let explicit_args = CCList.map (substitute_constants_in_exp constant_map) explicit_args in
      {e with e= EGenCall {instance; generic_path; args; explicit_args}}
  | EGenCompanionCall {instance; companion_name; parent_generic_path; args} ->
      let args = CCList.map (substitute_constants_in_exp constant_map) args in
      {e with e= EGenCompanionCall {instance; companion_name; parent_generic_path; args}}
  | EUnit | EBool _ | EInt _ | EReal _ | EFixed _ | EString _ | EConst _ | ETypeIntrinsic _ ->
      e

(** Substitutes constant parameter references in a left-hand side expression. *)
let rec substitute_constants_in_lexp (constant_map : (string * Typed.exp) list) (l : Typed.lexp) : Typed.lexp =
  match l.l with
  | LWild ->
      l
  | LId _ ->
      l
  | LMember (e, member_name) ->
      let e = substitute_constants_in_lexp constant_map e in
      {l with l= LMember (e, member_name)}
  | LIndex {e; index} ->
      let e = substitute_constants_in_lexp constant_map e in
      let index = substitute_constants_in_exp constant_map index in
      {l with l= LIndex {e; index}}
  | LTuple lexps ->
      let lexps = CCList.map (substitute_constants_in_lexp constant_map) lexps in
      {l with l= LTuple lexps}

(** Substitutes constant parameter references in a statement tree. *)
let rec substitute_constants_in_stmt (constant_map : (string * Typed.exp) list) (s : Typed.stmt) : Typed.stmt =
  match s.s with
  | StmtVal _ ->
      s
  | StmtMem (_, _) ->
      s
  | StmtBind (lhs, rhs) ->
      let lhs = substitute_constants_in_lexp constant_map lhs in
      let rhs = substitute_constants_in_exp constant_map rhs in
      {s with s= StmtBind (lhs, rhs)}
  | StmtReturn e ->
      let e = substitute_constants_in_exp constant_map e in
      {s with s= StmtReturn e}
  | StmtIf (cond, then_, else_opt) ->
      let cond = substitute_constants_in_exp constant_map cond in
      let then_ = substitute_constants_in_stmt constant_map then_ in
      let else_opt = Option.map (substitute_constants_in_stmt constant_map) else_opt in
      {s with s= StmtIf (cond, then_, else_opt)}
  | StmtWhile (cond, body) ->
      let cond = substitute_constants_in_exp constant_map cond in
      let body = substitute_constants_in_stmt constant_map body in
      {s with s= StmtWhile (cond, body)}
  | StmtBlock stmts ->
      let stmts = CCList.map (substitute_constants_in_stmt constant_map) stmts in
      {s with s= StmtBlock stmts}

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

(* ========== Generic Function Instantiation ========== *)

(* Create a specialized function from a generic function with resolved types. *)
let instantiate_generic_function (iargs : Args.args) (env : env) (state : instantiation_state)
    (generic_func : Typed.generic_function) (call_arg_types : Typed.type_ list) (explicit_args : Typed.exp list)
    (loc : Loc.t) : Typed.function_def * Typed.stmt =
  let all_constants = CCList.for_all is_constant_literal explicit_args in
  let specialized_name =
    if all_constants && CCList.length explicit_args > 0 then
      build_specialized_signature_string generic_func.name call_arg_types explicit_args
    else if CCList.length explicit_args > 0 then build_nonspec_signature_string generic_func.name call_arg_types
    else build_signature_string generic_func.name call_arg_types
  in
  let type_param_names =
    CCList.filter_map (function Typed.GParamType name -> Some name | _ -> None) generic_func.generic_params
  in
  let constant_params =
    CCList.filter_map
      (function Typed.GParamConstant (name, param_type) -> Some (name, param_type) | _ -> None)
      generic_func.generic_params
  in
  let generic_func_arg_types, generic_func_ret_type = generic_func.t in
  let fresh_types = Typed.copy_types_preserving_sharing (generic_func_arg_types @ [generic_func_ret_type]) in
  let fresh_arg_types, fresh_ret_type =
    match CCList.rev fresh_types with last :: rest -> (CCList.rev rest, last) | [] -> failwith "Empty type list"
  in
  CCList.iter2
    (fun fresh_t call_t ->
      let _ = unify fresh_t call_t in
      () )
    fresh_arg_types call_arg_types ;
  let type_substitution_map =
    CCList.mapi
      (fun i name ->
        let concrete_type =
          if i < CCList.length call_arg_types then CCList.nth call_arg_types i
          else if CCList.length call_arg_types > 0 then CCList.hd call_arg_types
          else {tx= TEId {id= "int"; n= None; loc}; loc; const= C.const ()}
        in
        (name, concrete_type) )
      type_param_names
  in
  let constant_substitution_map =
    if all_constants && CCList.length explicit_args > 0 then
      CCList.mapi
        (fun i (name, _param_type) ->
          if i < CCList.length explicit_args then (name, CCList.nth explicit_args i)
          else failwith "Mismatch between constant_params and explicit_args" )
        constant_params
    else []
  in
  let constant_args : Typed.arg list =
    CCList.mapi
      (fun i (name, param_type) ->
        let actual_type = if i < CCList.length explicit_args then (CCList.nth explicit_args i).t else param_type in
        {name; t= actual_type; loc} )
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
            if i < CCList.length fresh_arg_types then {arg with t= CCList.nth fresh_arg_types i} else arg
        | Typed.PKGeneric i ->
            if i < Array.length constant_args_array then constant_args_array.(i)
            else failwith "Invalid generic param index in param_order" )
      generic_func.param_order
  in
  let specialized_args =
    if all_constants && CCList.length explicit_args > 0 then
      CCList.filter_map
        (fun pk ->
          match pk with
          | Typed.PKArg i ->
              let arg = regular_args_array.(i) in
              let arg_with_type =
                if i < CCList.length fresh_arg_types then {arg with t= CCList.nth fresh_arg_types i} else arg
              in
              Some arg_with_type
          | Typed.PKGeneric _ ->
              None )
        generic_func.param_order
    else all_args_for_body
  in
  let env = Env.createContextForFunctionWithIndex env specialized_name loc generic_func.type_index in
  let inferred_ret = C.noreturn loc in
  let env, path, _t = Env.enterFunction env specialized_name all_args_for_body inferred_ret loc in
  let env, body = stmt_with_type_substitution env type_substitution_map fresh_ret_type generic_func.body in
  let body = CCList.map (resolve_type_intrinsics_in_stmt type_substitution_map) body in
  let body =
    if CCList.length constant_substitution_map > 0 then
      CCList.map (substitute_constants_in_stmt constant_substitution_map) body
    else body
  in
  let env = Env.exitFunction env in
  let env, next =
    match generic_func.next with
    | None ->
        (env, None)
    | Some _ -> (
        let generic_key = generic_func.name in
        match Hashtbl.find_opt state.processed_companions generic_key with
        | Some cached_next ->
            (env, cached_next)
        | None ->
            let env, next = function_def_opt iargs env generic_func.next in
            Hashtbl.add state.processed_companions generic_key next ;
            (env, next) )
  in
  let specialized_def : Typed.function_def =
    { name= path
    ; args= specialized_args
    ; t= (CCList.map (fun (a : Typed.arg) -> a.t) specialized_args, fresh_ret_type)
    ; loc= generic_func.loc
    ; tags= generic_func.tags
    ; is_root= false
    ; next }
  in
  let specialized_def = insertContextArgument env specialized_def in
  let env = Env.exitContext env in
  let _ = env in
  let _ = iargs in
  let combined_body = match body with [single] -> single | stmts -> {s= StmtBlock stmts; loc= generic_func.loc} in
  (specialized_def, combined_body)

(* ========== Prescan for Generic Calls ========== *)

let rec prescan_generic_calls_in_stmt (iargs : Args.args) (env : env) (state : instantiation_state) (stmt : Typed.stmt)
    : unit =
  match stmt.s with
  | StmtVal _ ->
      ()
  | StmtReturn e ->
      prescan_generic_calls_in_exp iargs env state e
  | StmtBind (_, e) ->
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

and prescan_generic_calls_in_exp (iargs : Args.args) (env : env) (state : instantiation_state) (e : Typed.exp) : unit =
  match e.e with
  | EGenCall _ ->
      state.pending_generic_calls <- e :: state.pending_generic_calls
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

let rec process_exp_instantiation (iargs : Args.args) (env : env) (state : instantiation_state) (e : Typed.exp) :
    Typed.exp =
  let loc = e.loc in
  match e.e with
  | EGenCall {instance; generic_path; args; explicit_args} -> (
      let generic_name = Pla.print (Syntax.print_path generic_path) in
      match Env.lookupGeneric env generic_path with
      | None ->
          Error.raiseError (Printf.sprintf "Generic function '%s' not found" generic_name) loc
      | Some generic_func ->
          let resolved_arg_types = CCList.map (fun (a : Typed.exp) -> unlink a.t) args in
          let all_constants = CCList.for_all is_constant_literal explicit_args in
          let signature =
            if all_constants && CCList.length explicit_args > 0 then
              build_specialized_signature_string generic_name resolved_arg_types explicit_args
            else if CCList.length explicit_args > 0 then build_nonspec_signature_string generic_name resolved_arg_types
            else build_signature_string generic_name resolved_arg_types
          in
          let specialized_def =
            match Hashtbl.find_opt state.instantiated signature with
            | Some (def, _) ->
                def
            | None ->
                let generic_module = generic_path.n in
                let env_for_instantiation =
                  match generic_module with Some module_name -> Env.enterModule env module_name | None -> env
                in
                let def, body =
                  instantiate_generic_function iargs env_for_instantiation state generic_func resolved_arg_types
                    explicit_args loc
                in
                let () = prescan_generic_calls_in_stmt iargs env_for_instantiation state body in
                let processed_body = process_stmt_instantiation iargs env_for_instantiation state body in
                let _ =
                  match generic_module with
                  | Some _ ->
                      Env.exitModule env_for_instantiation
                  | None ->
                      env_for_instantiation
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
                Hashtbl.add state.instantiated signature (def, processed_body) ;
                state.pending_functions <-
                  (target_module, generic_func.name, def, processed_body) :: state.pending_functions ;
                def
          in
          let processed_regular_args = CCList.map (process_exp_instantiation iargs env state) args in
          let processed_explicit_args = CCList.map (process_exp_instantiation iargs env state) explicit_args in
          let processed_args =
            if all_constants && CCList.length explicit_args > 0 then
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
              let regular_args_array = Array.of_list processed_regular_args in
              let explicit_args_array = Array.of_list processed_explicit_args in
              CCList.map
                (fun pk ->
                  match pk with
                  | Typed.PKArg i ->
                      if i < Array.length regular_args_array then regular_args_array.(i)
                      else failwith "Invalid arg index in param_order"
                  | Typed.PKGeneric i ->
                      if i < Array.length explicit_args_array then explicit_args_array.(i)
                      else failwith "Invalid generic param index in param_order" )
                generic_func.param_order
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
                        if not (checkMemExists env user_inst_name || Env.checkConstantExists env user_inst_name) then
                          let _ = Env.addVar env unify user_inst_name ctx_t Inst loc in
                          ()
                      in
                      user_inst_name
                  | None ->
                      let number =
                        Printf.sprintf "%.2x%.2x"
                          (0xFF land Hashtbl.hash (path_string specialized_def.name))
                          (0xFF land Hashtbl.hash (path_string (Env.getContext env)))
                      in
                      let rec generateName () =
                        let n = Env.getFunctionTick env in
                        let name = "inst_" ^ string_of_int n ^ number in
                        if checkMemExists env name || Env.checkConstantExists env name then generateName () else name
                      in
                      let name = generateName () in
                      let _ = Env.addVar env unify name ctx_t Inst loc in
                      name
                in
                let ctx_e = {e= EId context_name; t= current_ctx_t; loc} in
                let inst_e = {e= EMember (ctx_e, inst_name); t= ctx_t; loc} in
                inst_e :: processed_args
            | _ ->
                processed_args
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
      let func_path_str = path_string path in
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
                       (path_string current_f.path) func_path_str )
            in
            let number =
              Printf.sprintf "%.2x%.2x"
                (0xFF land Hashtbl.hash func_path_str)
                (0xFF land Hashtbl.hash (path_string (Env.getContext env)))
            in
            let rec generateName () =
              let n = Env.getFunctionTick env in
              let inst_name = "inst_" ^ string_of_int n ^ number in
              if checkMemExists env inst_name || Env.checkConstantExists env inst_name then generateName ()
              else inst_name
            in
            let inst_name = generateName () in
            let _ = Env.addVar env unify inst_name ctx_t Inst loc in
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
          (Printf.sprintf "Parent generic function '%s' not found for companion '%s'" (path_string parent_generic_path)
             companion_name )
          loc
    | Some _parent_generic -> (
        let parent_name = parent_generic_path.id in
        let matching_instantiation =
          CCList.find_opt
            (fun (_module, gen_name, (def : Typed.function_def), _body) ->
              String.equal gen_name parent_name
              &&
              let rec has_companion (next : (Typed.function_def * Typed.stmt) option) =
                match next with
                | None ->
                    false
                | Some (companion_def, _) ->
                    if String.equal companion_def.name.id companion_name then true else has_companion companion_def.next
              in
              has_companion def.next )
            state.pending_functions
        in
        let matching_instantiation =
          match matching_instantiation with
          | Some _ ->
              matching_instantiation
          | None -> (
              let matching_pending =
                CCList.find_opt
                  (fun (pending_e : Typed.exp) ->
                    match pending_e.e with
                    | EGenCall {generic_path; _} ->
                        String.equal generic_path.id parent_name
                    | _ ->
                        false )
                  state.pending_generic_calls
              in
              match matching_pending with
              | Some parent_call ->
                  let _ = process_exp_instantiation iargs env state parent_call in
                  CCList.find_opt
                    (fun (_module, gen_name, (def : Typed.function_def), _body) ->
                      String.equal gen_name parent_name
                      &&
                      let rec has_companion (next : (Typed.function_def * Typed.stmt) option) =
                        match next with
                        | None ->
                            false
                        | Some (companion_def, _) ->
                            if String.equal companion_def.name.id companion_name then true
                            else has_companion companion_def.next
                      in
                      has_companion def.next )
                    state.pending_functions
              | None ->
                  None )
        in
        match matching_instantiation with
        | None ->
            Error.raiseError
              (Printf.sprintf
                 "Companion function '%s' called before parent generic '%s' was instantiated. Make sure to call the \
                  parent function first."
                 companion_name parent_name )
              loc
        | Some (_, _, specialized_def, _) -> (
            let rec find_companion (next : (function_def * stmt) option) : (function_def * stmt) option =
              match next with
              | None ->
                  None
              | Some ((companion_def, _companion_body) as companion) ->
                  if String.equal companion_def.name.id companion_name then Some companion
                  else find_companion companion_def.next
            in
            match find_companion specialized_def.next with
            | None ->
                Error.raiseError
                  (Printf.sprintf "Companion function '%s' not found in instantiated generic" companion_name)
                  loc
            | Some (companion_def, _) ->
                let processed_args = CCList.map (process_exp_instantiation iargs env state) args in
                let companion_non_ctx_args =
                  match companion_def.args with
                  | {name; _} :: rest when String.equal name context_name ->
                      rest
                  | args ->
                      args
                in
                let () =
                  if CCList.length companion_non_ctx_args = CCList.length processed_args then
                    CCList.iter2
                      (fun (def_arg : Typed.arg) (call_arg : Typed.exp) ->
                        let _ = unify def_arg.t call_arg.t in
                        () )
                      companion_non_ctx_args processed_args
                in
                let final_args =
                  match companion_def.args with
                  | {name; t= ctx_t; _} :: _ when String.equal name context_name ->
                      let current_f = Env.getCurrentFunction env in
                      let current_ctx_t =
                        match Env.lookVarInScopes current_f.locals context_name with
                        | Some var ->
                            var.t
                        | None ->
                            failwith "context var not declared in caller for companion call"
                      in
                      let inst_name =
                        match instance with
                        | Some user_inst_name ->
                            let () =
                              if not (checkMemExists env user_inst_name || Env.checkConstantExists env user_inst_name)
                              then
                                let _ = Env.addVar env unify user_inst_name ctx_t Inst loc in
                                ()
                            in
                            user_inst_name
                        | None ->
                            let number =
                              Printf.sprintf "%.2x%.2x"
                                (0xFF land Hashtbl.hash (path_string specialized_def.name))
                                (0xFF land Hashtbl.hash (path_string (Env.getContext env)))
                            in
                            let rec findInstance n =
                              if n < 0 then failwith "Could not find instance for companion call"
                              else
                                let name = "inst_" ^ string_of_int n ^ number in
                                if checkMemExists env name then name else findInstance (n - 1)
                            in
                            findInstance (Env.getFunctionTick env)
                      in
                      let ctx_e = {e= EId context_name; t= current_ctx_t; loc} in
                      let inst_e = {e= EMember (ctx_e, inst_name); t= ctx_t; loc} in
                      inst_e :: processed_args
                  | _ ->
                      processed_args
                in
                {e= ECall {instance= None; path= companion_def.name; args= final_args}; t= e.t; loc} ) ) )
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

and process_stmt_instantiation (iargs : Args.args) (env : env) (state : instantiation_state) (s : Typed.stmt) :
    Typed.stmt =
  match s.s with
  | StmtVal d ->
      {s with s= StmtVal d}
  | StmtMem (d, tags) ->
      {s with s= StmtMem (d, tags)}
  | StmtBind (lhs, rhs) ->
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
  let () = prescan_generic_calls_in_stmt iargs env state body in
  let body = process_stmt_instantiation iargs env state body in
  let had_ctx_before = match def.args with {name; _} :: _ when String.equal name context_name -> true | _ -> false in
  let def = insertContextArgument env def in
  let () =
    if not had_ctx_before then
      match def.args with
      | {name; t= ctx_t; _} :: _ when String.equal name context_name ->
          let func_path_str = path_string def.name in
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
  let stmts = CCList.map (process_top_stmt_instantiation iargs env state) stmts in
  CCList.map (process_top_stmt_instantiation iargs env state) stmts

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

    @param iargs Compiler arguments
    @param env The type environment from typechecking
    @param module_stmts List of (module_name, statements) pairs from typechecking
    @return The elaborated top-level statements *)
let elaborate (iargs : Args.args) (env : env) (module_stmts : (string * Typed.top_stmt list) list) : Typed.top_stmt list
    =
  let instantiation_state = create_instantiation_state () in
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
  CCList.rev final_stmts
