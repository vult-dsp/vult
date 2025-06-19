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
open Maps

let global_tick = ref 0

let getGlobalTick () =
  let n = !global_tick in
  incr global_tick;
  n


let pathString p = Pla.print (Pparser.Syntax.print_path p)

module Map = struct
  type 'a t = 'a Map.t ref

  let empty () = ref Map.empty

  let update (report : 'a -> 'a -> 'a) (key : string) (value : 'a) (t : 'a t) : unit =
    t :=
      Map.update
        key
        (fun a ->
          match a with
          | None -> Some value
          | Some b ->
            let c = report b value in
            Some c)
        !t


  let of_list elems : 'a t =
    let m = CCList.fold_left (fun m (key, value) -> Map.add key value m) Map.empty elems in
    ref m


  let to_list (t : 'a t) = Map.to_list !t

  let find key t = Map.find_opt key !t

  let is_empty (t : 'a t) : bool = Map.is_empty !t

  let fold (f : string -> 'a -> 'b -> 'b) (s : 'b) (t : 'a t) : 'b = Map.fold f !t s
end

module type TSig = sig
  type t

  val convert : Typed.type_ -> t

  val convert_function_type : Typed.type_ list * Typed.type_ -> t list * t
end

type var_kind =
  | Mem of Pparser.Ptags.tags
  | Inst
  | Val
  | Const

type path = Typed.path

type var =
  { name : string
  ; t : Typed.type_
  ; kind : var_kind
  ; tags : Pparser.Ptags.tags
  ; loc : Loc.t
  }

type type_descr =
  | Simple
  | Record of var Map.t
  | Enum of (string * int * Loc.t) Map.t
  | Alias of path * path

type t =
  { path : path
  ; descr : type_descr
  ; index : int
  ; loc : Loc.t
  ; generated : bool
  }

type context = (path * t) option

type function_type =
  { args : Typed.arg list
  ; ret : Typed.type_
  ; context : context
  }

type f =
  { path : path
  ; t : Typed.type_ list * Typed.type_
  ; args : Typed.arg list option
  ; context : context
  ; mutable locals : var Map.t list
  ; mutable tick : int
  }

type m =
  { name : string
  ; functions : f Map.t
  ; types : t Map.t
  ; mutable init : (path * path) list
  ; enums : t Map.t
  ; mutable constants : var Map.t
  }

(* Generic lookup result variant *)
type lookup_result =
  | LookupVar of var
  | LookupFunction of f
  | LookupType of t
  | LookupEnum of (path * Loc.t * int) (* path, location, index *)
  | LookupConstant of var
  | LookupBuiltinFunction of f
  | LookupNotFound

(* Location within the environment hierarchy *)
type location =
  | Top
  | InModule of string
  | InContext of string * context
  | InFunction of string * f

(* Unified environment type that tracks current location internally *)
type env =
  { modules : m Map.t
  ; builtin_functions : (unit -> Typed.fun_type) Map.t
  ; builtin_types : t Map.t
  ; location : location
  }

(* Legacy type aliases for backward compatibility during transition *)
type in_top = env

type in_module = env

type in_context = env

type in_func = env

let builtin_functions =
  Typed.
    [ "size", C.array_size
    ; "abs", C.num_num
    ; "exp", C.freal_freal
    ; "log10", C.freal_freal
    ; "log", C.freal_freal
    ; "sin", C.freal_freal
    ; "cos", C.freal_freal
    ; "floor", C.freal_freal
    ; "tanh", C.freal_freal
    ; "pow", C.real_real_real
    ; "cosh", C.freal_freal
    ; "sinh", C.freal_freal
    ; "tan", C.freal_freal
    ; "sqrt", C.freal_freal
    ; "clip", C.clip
    ; "int", C.valid_int
    ; "real", C.valid_real
    ; "fix16", C.valid_fix16
    ; "int16", C.valid_int16
    ; "string", C.valid_string
    ; "bool", C.valid_bool
    ; "u-", C.num_num
    ; "+", C.numstr_numstr_numstr
    ; "-", C.num_num_num
    ; "*", C.num_num_num
    ; "/", C.num_num_num
    ; "%", C.num_num_num
    ; ">", C.a_a_bool
    ; "<", C.a_a_bool
    ; "==", C.a_a_bool
    ; "<>", C.a_a_bool
    ; ">=", C.a_a_bool
    ; "<=", C.a_a_bool
    ; "|", C.int_int_int
    ; "&", C.int_int_int
    ; "^", C.int_int_int
    ; ">>", C.int_int_int
    ; "<<", C.int_int_int
    ; "not", C.bool_bool
    ; "||", C.bool_bool_bool
    ; "&&", C.bool_bool_bool
    ; "eps", C.unit_real
    ; "pi", C.unit_real
    ; "random", C.unit_real
    ; "irandom", C.unit_int
    ; "samplerate", C.unit_real
    ; "wrap_array", C.wrap_array
    ; "length", C.str_length
    ]
  |> Map.of_list


let builtin_types =
  [ "int"; "int16"; "real"; "fix16"; "bool"; "string"; "unit" ]
  |> CCList.map (fun n ->
         ( n
         , { path = Pparser.Syntax.{ id = n; n = None; loc = Loc.default }
           ; descr = Simple
           ; index = 0
           ; loc = Loc.default
           ; generated = false
           } ))
  |> Map.of_list


let rec isBuiltinType (t : Typed.type_) : bool =
  match t.tx with
  | TEId { id; n = None; _ } -> Map.find id builtin_types <> None
  | TELink t -> isBuiltinType t
  | _ -> false


let makeFunctionForBuiltin name t : f =
  { path = { id = name; n = None; loc = Loc.default }; t; context = None; locals = []; tick = 0; args = None }


(* Helper functions to extract current state *)
let getCurrentModule (env : env) : m =
  match env.location with
  | InModule name | InContext (name, _) | InFunction (name, _) -> (
    match Map.find name env.modules with
    | Some m -> m
    | None -> failwith ("Module " ^ name ^ " not found"))
  | Top -> failwith "Not currently in a module"


let getCurrentContext (env : env) : context =
  match env.location with
  | InContext (_, context) -> context
  | InFunction (_, f) -> f.context
  | _ -> None


let getCurrentFunction (env : env) : f =
  match env.location with
  | InFunction (_, f) -> f
  | _ -> failwith "Not currently in a function"


(* Private helper functions for internal use only *)
let rec lookVarInScopes (scopes : var Map.t list) name : var option =
  match scopes with
  | [] -> None
  | h :: t -> (
    match Map.find name h with
    | Some found -> Some found
    | None -> lookVarInScopes t name)


let lookVarInContext (context : context) name : var option =
  match context with
  | Some (_, { descr = Record members; _ }) -> Map.find name members
  | _ -> None


let registerArguments (args : Typed.arg list) =
  let locals = Map.empty () in
  let report loc (found : var) =
    Error.raiseError
      ("A variable with the name '" ^ found.name ^ "' has already been declared at " ^ Loc.to_string_readable found.loc)
      loc
  in
  let rev_args =
    CCList.fold_left
      (fun acc ({ name; t; loc } : Typed.arg) ->
        let () = Map.update (report loc) name { name; t; kind = Val; tags = []; loc } locals in
        t :: acc)
      []
      args
  in
  locals, CCList.rev rev_args


let registerContextLocal loc locals (context : context) =
  let report loc (found : var) = Error.raiseError ("The name '" ^ found.name ^ "' is reserved.") loc in
  match context with
  | Some (p, _) ->
    let name = "_ctx" in
    let t = Typed.C.path_t loc p in
    Map.update (report loc) name { name; t; kind = Val; tags = []; loc } locals;
    locals
  | None -> locals


let lookVar (env : env) (name : string) (loc : Loc.t) : var =
  let f = getCurrentFunction env in
  let m = getCurrentModule env in
  match lookVarInContext f.context name with
  | Some found -> found
  | None -> (
    match lookVarInScopes f.locals name with
    | Some found -> found
    | None -> (
      match Map.find name m.constants with
      | Some var -> var
      | None -> Error.raiseError ("The variable '" ^ name ^ "' could not be found") loc))


let reportModuleNotFound n loc = Error.raiseError ("The module named '" ^ n ^ "' could not be found") loc

let lookEnum (env : env) (path : path) (loc : Loc.t) =
  let error () = Error.raiseError ("An enumeration with the name '" ^ pathString path ^ "' could not be found") loc in
  let findEnumInModule enums id =
    match Map.find id enums with
    | Some ({ descr = Enum members; _ } as t) -> (
      match Map.find id members with
      | Some (_, index, _) -> t.path, t.loc, index
      | None -> error ())
    | _ -> error ()
  in
  match path with
  | { id; n = None; _ } ->
    let m = getCurrentModule env in
    findEnumInModule m.enums id
  | { id; n = Some n; loc } -> (
    match Map.find n env.modules with
    | Some m -> findEnumInModule m.enums id
    | None -> reportModuleNotFound n loc)


let getType (env : env) (path : path) : t option =
  match path with
  | { id; n = Some n; loc } -> (
    match Map.find n env.modules with
    | None -> reportModuleNotFound n loc
    | Some m -> Map.find id m.types)
  | _ -> None


let addConstant (env : env) _unify (name : string) (t : Typed.type_) loc : env =
  let m = getCurrentModule env in
  let report (found : var) =
    Error.raiseError
      ("A constant with the name '" ^ found.name ^ "' has already been declared at " ^ Loc.to_string_readable found.loc)
      loc
  in
  Map.update report name { name; t; kind = Const; tags = []; loc } m.constants;
  env


let addVar (env : env) unify (name : string) (t : Typed.type_) (kind : var_kind) loc : env =
  let f = getCurrentFunction env in
  let context = getCurrentContext env in
  let report_mem (found : var) (value : var) =
    if unify found.t t then
      let tags = Pparser.Ptags.mergeTags found.tags value.tags in
      { found with tags }
    else
      let old_type = Pla.print (Typed.print_type_ found.t) in
      let new_type = Pla.print (Typed.print_type_ t) in
      Error.raiseError
        ("This declaration tries to change the type of "
        ^ found.name
        ^ ". The previous type is '"
        ^ old_type
        ^ "' and the new is '"
        ^ new_type
        ^ "'")
        value.loc
  in
  let checkDuplicatedMem context name =
    match context with
    | Some (_, { descr = Record members; _ }) -> (
      match Map.find name members with
      | None -> ()
      | Some found ->
        Error.raiseError
          ("A mem variable with the name '"
          ^ found.name
          ^ "' has already been declared at "
          ^ Loc.to_string_readable found.loc)
          loc)
    | _ -> ()
  in
  let checkDuplicatedVal locals name =
    CCList.iter
      (fun (scope : var Map.t) ->
        match Map.find name scope with
        | None -> ()
        | Some found ->
          Error.raiseError
            ("A variable with the name '"
            ^ found.name
            ^ "' has already been declared at "
            ^ Loc.to_string_readable found.loc)
            loc)
      locals
  in
  match kind, context with
  | Inst, Some (_, { descr = Record members; _ }) ->
    let () = checkDuplicatedVal f.locals name in
    Map.update report_mem name { name; t; kind; tags = []; loc } members;
    env
  | Mem tags, Some (_, { descr = Record members; _ }) ->
    let () = checkDuplicatedVal f.locals name in
    Map.update report_mem name { name; t; kind; tags; loc } members;
    env
  | (Mem _ | Inst), None -> failwith "Internal error: cannot add mem to functions with no context"
  | Val, context -> (
    let report (found : var) =
      Error.raiseError
        ("A variable with the name '"
        ^ found.name
        ^ "' has already been declared at "
        ^ Loc.to_string_readable found.loc)
        loc
    in
    let () = checkDuplicatedMem context name in
    match f.locals with
    | [] -> failwith "no local scope"
    | h :: _ ->
      Map.update report name { name; t; kind; tags = []; loc } h;
      env)
  | Const, _ -> failwith "Do not use to add constants"
  | _, Some _ -> failwith "Not a record"


let checkMemExists (env : env) name =
  let f = getCurrentFunction env in
  match f.context with
  | Some (_, { descr = Record members; _ }) -> (
    match Map.find name members with
    | None -> false
    | Some _ -> true)
  | _ -> false


let addReturnVar (env : env) (name : string) (t : Typed.type_) loc : env =
  let report_mem found _ = found in
  let () = Typed.setTypeMut t in
  match getCurrentContext env with
  | Some (_, { descr = Record members; _ }) ->
    Map.update report_mem name { name; t; kind = Mem []; tags = []; loc } members;
    env
  | None -> failwith "Internal error: cannot add mem to functions with no context"
  | Some _ -> failwith "Not a record"


let pushScope (env : env) : env =
  let f = getCurrentFunction env in
  f.locals <- Map.empty () :: f.locals;
  env


let popScope (env : env) : env =
  let f = getCurrentFunction env in
  match f.locals with
  | [] -> failwith "invalid scope"
  | _ :: t ->
    f.locals <- t;
    env


let getPath m name loc : path = { id = name; n = Some m.name; loc }

let createContextForFunction (env : env) name loc : env =
  let m = getCurrentModule env in
  let report name (found : t) =
    Error.raiseError
      ("A function with the name '" ^ name ^ "' already exists at " ^ Loc.to_string_readable found.loc)
      loc
  in
  let type_name = name ^ "_type" in
  let path = getPath m type_name loc in
  let index = getGlobalTick () in
  let t = { descr = Record (Map.empty ()); path; index; loc; generated = true } in
  let _ = Map.update (report name) type_name t m.types in
  { env with location = InContext (m.name, Some (path, t)) }


let addAliasToContext (env : env) name loc : env =
  match getCurrentContext env with
  | Some (ctx, { descr = Record members; _ }) when not (Map.is_empty members) ->
    let m = getCurrentModule env in
    let report found =
      Error.raiseError ("A context with the same name already exists at " ^ Loc.to_string_readable found.loc) loc
    in
    let type_name = name ^ "_type" in
    let path = getPath m type_name loc in
    let index = getGlobalTick () in
    let t = { descr = Alias (path, ctx); path; index; loc; generated = true } in
    let _ = Map.update report type_name t m.types in
    env
  | _ -> env


let addRecordMember members =
  let report loc (found : var) =
    Error.raiseError
      ("A member with the name '" ^ found.name ^ "' has already been declared at " ^ Loc.to_string_readable found.loc)
      loc
  in
  let members =
    CCList.fold_left
      (fun m (name, t, tags, loc) ->
        Map.update (report loc) name { name; t; kind = Val; tags; loc } m;
        m)
      (Map.empty ())
      members
  in
  Record members


let addType (env : env) type_name members loc : env =
  let m = getCurrentModule env in
  let report (found : t) =
    Error.raiseError
      ("A type with the name '" ^ found.path.id ^ "' has already been declared at " ^ Loc.to_string_readable found.loc)
      loc
  in
  let index = getGlobalTick () in
  let path = getPath m type_name loc in
  let descr = addRecordMember members in
  let t = { path; descr; loc; index; generated = false } in
  let _ = Map.update report type_name t m.types in
  env


let addEnumMember members =
  let report loc (name, _, floc) =
    Error.raiseError
      ("A member with the name '" ^ name ^ "' has already been declared at " ^ Loc.to_string_readable floc)
      loc
  in
  let members, _ =
    CCList.fold_left
      (fun (m, i) (name, loc) ->
        Map.update (report loc) name (name, i, loc) m;
        m, i + 1)
      (Map.empty (), 0)
      members
  in
  Enum members


let addEnumToModule (env : env) members t =
  let m = getCurrentModule env in
  let report loc name (found : t) =
    Error.raiseError
      ("A enum value with the name '" ^ name ^ "' has already been declared at " ^ Loc.to_string_readable found.loc)
      loc
  in
  let () = CCList.iter (fun (name, loc) -> Map.update (report loc name) name t m.enums) members in
  env


let addEnum (env : env) type_name members loc : env =
  let m = getCurrentModule env in
  let report (found : t) =
    Error.raiseError
      ("A enum with the name '" ^ found.path.id ^ "' has already been declared at " ^ Loc.to_string_readable found.loc)
      loc
  in
  let index = getGlobalTick () in
  let path = getPath m type_name loc in
  let descr = addEnumMember members in
  let t = { path; descr; loc; index; generated = false } in
  let _ = Map.update report type_name t m.types in
  let env = addEnumToModule env members t in
  env


let createContextForExternal (env : env) : env =
  let m = getCurrentModule env in
  { env with location = InContext (m.name, None) }


let exitContext (env : env) : env =
  let m = getCurrentModule env in
  { env with location = InModule m.name }


let getFunctionTick (env : env) : int =
  let f = getCurrentFunction env in
  let n = f.tick + 1 in
  f.tick <- n;
  n


let getContext (env : env) : path =
  match getCurrentContext env with
  | Some (p, _) -> p
  | None -> failwith "trying to get the context of a function without one"


let getFunctionContext (f : f) : path =
  match f.context with
  | Some (p, _) -> p
  | None -> failwith "trying to get the context of a function without one"


let enterFunction (env : env) (name : string) (args : Typed.arg list) (ret : Typed.type_) loc :
    env * path * (Typed.type_ list * Typed.type_) =
  let m = getCurrentModule env in
  let context = getCurrentContext env in
  let report (found : f) =
    Error.raiseError ("A function with the name '" ^ found.path.id ^ "' has already been declared.") loc
  in
  let path = getPath m name loc in
  let locals, args_t = registerArguments args in
  let locals = registerContextLocal loc locals context in
  let t = args_t, ret in
  let f : f = { path; t; context; locals = [ locals ]; tick = 0; args = Some args } in
  let _ = Map.update report name f m.functions in
  { env with location = InFunction (m.name, f) }, path, t


let isFunctionActive (f : f) =
  match f.context with
  | Some (_, { descr = Record members; _ }) -> not (Map.is_empty members)
  | _ -> false


let exitFunction (env : env) : env =
  let m = getCurrentModule env in
  let f = getCurrentFunction env in
  { env with location = InContext (m.name, f.context) }


let addCustomInitFunction (env : env) name =
  match getCurrentContext env with
  | Some (p, _) ->
    let m = getCurrentModule env in
    m.init <- (p, name) :: m.init;
    env
  | _ -> env


let enterModule (env : env) (name : string) : env =
  match Map.find name env.modules with
  | Some _ -> { env with location = InModule name }
  | None ->
    let report _ = failwith ("duplicate module: " ^ name) in
    let m : m =
      { name
      ; functions = Map.empty ()
      ; types = Map.empty ()
      ; enums = Map.empty ()
      ; init = []
      ; constants = Map.empty ()
      }
    in
    let () = Map.update report name m env.modules in
    { env with location = InModule name }


let exitModule (env : env) : env = { env with location = Top }

(* Generic lookup function for paths - returns list of all possible meanings *)
let lookupPath (env : env) (path : path) : lookup_result list =
  let lookupInModule (m : m) (id : string) : lookup_result list =
    let results = [] in
    (* Find all meanings in module *)
    let results =
      match Map.find id m.functions with
      | Some f -> LookupFunction f :: results
      | None -> results
    in
    let results =
      match Map.find id m.types with
      | Some t -> LookupType t :: results
      | None -> results
    in
    let results =
      match Map.find id m.constants with
      | Some var -> LookupConstant var :: results
      | None -> results
    in
    let results =
      match Map.find id m.enums with
      | Some ({ descr = Enum members; _ } as t) -> (
        match Map.find id members with
        | Some (_, index, loc) -> LookupEnum (t.path, loc, index) :: results
        | None -> results)
      | _ -> results
    in
    results
  in
  match path with
  | { id; n = Some module_name; _ } -> (
    (* Module-qualified path: Module.name *)
    match Map.find module_name env.modules with
    | Some m -> lookupInModule m id
    | None -> [])
  | { id; n = None; _ } ->
    (* Local path: name - collect all possible meanings *)
    let results = [] in
    (* Check local scope first (variables have priority) *)
    let results =
      match env.location with
      | InFunction (_, f) -> (
        match lookVarInContext f.context id with
        | Some var -> LookupVar var :: results
        | None -> (
          match lookVarInScopes f.locals id with
          | Some var -> LookupVar var :: results
          | None -> results))
      | _ -> results
    in
    (* Add module-level symbols *)
    let results =
      match env.location with
      | InFunction (_, _) | InModule _ | InContext (_, _) ->
        let m = getCurrentModule env in
        lookupInModule m id @ results
      | Top -> results
    in
    (* Add builtin functions *)
    let results =
      match Map.find id env.builtin_functions with
      | Some f -> LookupBuiltinFunction (makeFunctionForBuiltin id (f ())) :: results
      | None -> results
    in
    (* Add builtin types *)
    let results =
      match Map.find id env.builtin_types with
      | Some t -> LookupType t :: results
      | None -> results
    in
    results


(* Helper functions to find specific lookup result types from a list *)
let findType (results : lookup_result list) : t option =
  let rec find = function
    | [] -> None
    | LookupType t :: _ -> Some t
    | _ :: rest -> find rest
  in
  find results


let findFunction (results : lookup_result list) : f option =
  let rec findLocal = function
    | [] -> None
    | LookupFunction f :: _ -> Some f
    | _ :: rest -> findLocal rest
  in
  let rec findBuiltin = function
    | [] -> None
    | LookupBuiltinFunction f :: _ -> Some f
    | _ :: rest -> findBuiltin rest
  in
  match findLocal results with
  | Some f -> Some f
  | None -> findBuiltin results


let findVar (results : lookup_result list) : var option =
  let rec find = function
    | [] -> None
    | LookupVar var :: _ -> Some var
    | LookupConstant var :: _ -> Some var
    | _ :: rest -> find rest
  in
  find results


let findEnum (results : lookup_result list) : (path * Loc.t * int) option =
  let rec find = function
    | [] -> None
    | LookupEnum (path, loc, index) :: _ -> Some (path, loc, index)
    | _ :: rest -> find rest
  in
  find results


(* Function lookup using the new generic lookup system *)
let lookFunctionCall (env : env) (path : path) (loc : Loc.t) : f =
  match findFunction (lookupPath env path) with
  | Some f -> f
  | None -> Error.raiseError ("A function with the name '" ^ pathString path ^ "' could not be found") loc


(* Operator lookup using the new generic lookup system *)
let lookOperator (env : env) (op : string) : f =
  let op_path : path = { id = op; n = None; loc = Loc.default } in
  match findFunction (lookupPath env op_path) with
  | Some f -> f
  | None -> failwith ("operator not found " ^ op)


(* Since operators are only builtins, this behaves the same as lookOperator *)
let lookOperatorInModule (env : env) (op : string) : f = lookOperator env op

(* Unified expression lookup for handling ambiguous symbols *)
type expression_symbol =
  | ExprVariable of var
  | ExprFunction of f
  | ExprType of t
  | ExprEnum of (path * Loc.t * int)
  | ExprNotFound

let lookupExpressionSymbol (env : env) (path : path) (in_constant_context : bool) : expression_symbol =
  let results = lookupPath env path in
  if in_constant_context then
    (* In constant context: constants first, then enums, then types *)
    match findVar results with
    | Some var when var.kind = Const -> ExprVariable var
    | _ -> (
      match findEnum results with
      | Some enum_data -> ExprEnum enum_data
      | None -> (
        match findType results with
        | Some t -> ExprType t
        | None -> ExprNotFound))
  else
    (* In regular context: variables first, then functions, then enums *)
    match findVar results with
    | Some var -> ExprVariable var
    | None -> (
      match findFunction results with
      | Some f -> ExprFunction f
      | None -> (
        match findEnum results with
        | Some enum_data -> ExprEnum enum_data
        | None -> ExprNotFound))


(* Unified type lookup function using the new lookup system *)
let lookType (env : env) (path : path) (loc : Loc.t) : t =
  match findType (lookupPath env path) with
  | Some t -> t
  | None -> Error.raiseError ("A type with the name '" ^ pathString path ^ "' could not be found") loc


let empty () = { modules = Map.empty (); builtin_functions; builtin_types; location = Top }
