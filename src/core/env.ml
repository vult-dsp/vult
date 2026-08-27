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
  incr global_tick ; n

let pathString p = Pla.print (Pparser.Syntax.print_path p)

module Map = struct
  type 'a t = 'a Map.t ref

  let empty () = ref Map.empty

  let update (report : 'a -> 'a -> 'a) (key : string) (value : 'a) (t : 'a t) : unit =
    t :=
      Map.update key
        (fun a ->
          match a with
          | None ->
              Some value
          | Some b ->
              let c = report b value in
              Some c )
        !t

  let of_list elems : 'a t =
    let m = CCList.fold_left (fun m (key, value) -> Map.add key value m) Map.empty elems in
    ref m

  let to_list (t : 'a t) = Map.to_list !t

  let find key t = Map.find_opt key !t

  let is_empty (t : 'a t) : bool = Map.is_empty !t

  let fold (f : string -> 'a -> 'b -> 'b) (s : 'b) (t : 'a t) : 'b = Map.fold f !t s
end

(* Signature-based map for template instantiations *)
module SignatureMap = struct
  module SignatureOrder = struct
    type t = Typed.instantiation_signature

    let compare = Typed.compare_instantiation_signature
  end

  module BaseMap = CCMap.Make (SignatureOrder)

  type 'a t = 'a BaseMap.t ref

  let empty () = ref BaseMap.empty

  let update (report : 'a -> 'a -> 'a) (key : Typed.instantiation_signature) (value : 'a) (t : 'a t) : unit =
    t :=
      BaseMap.update key
        (fun a ->
          match a with
          | None ->
              Some value
          | Some b ->
              let c = report b value in
              Some c )
        !t

  let find (key : Typed.instantiation_signature) (t : 'a t) : 'a option = BaseMap.find_opt key !t

  let is_empty (t : 'a t) : bool = BaseMap.is_empty !t
end

module type TSig = sig
  type t

  val convert : Typed.type_ -> t

  val convert_function_type : Typed.type_ list * Typed.type_ -> t list * t
end

type var_kind = Mem of Pparser.Ptags.tags | Inst | Val | Const

type path = Typed.path

type var = {name: string; t: Typed.type_; kind: var_kind; tags: Pparser.Ptags.tags; loc: Loc.t}

type type_descr = Simple | Record of var Map.t | Enum of (string * int * Loc.t) Map.t | Alias of path * path

type t = {path: path; descr: type_descr; index: int; loc: Loc.t; generated: bool}

type context = (path * t) option

type function_type = {args: Typed.arg list; ret: Typed.type_; context: context}

type f =
  { path: path
  ; t: Typed.type_ list * Typed.type_
  ; args: Typed.arg list option
  ; context: context
  ; mutable locals: var Map.t list
  ; mutable tick: int }

type m =
  { name: string
  ; functions: f Map.t
  ; generics: Typed.generic_function Map.t
  ; instantiated: Typed.generic_instantiation SignatureMap.t
  ; types: t Map.t
  ; mutable init: (path * path) list
  ; enums: t Map.t
  ; mutable constants: var Map.t }

(* Generic lookup result variant *)
type lookup_result =
  | LookupVar of var
  | LookupFunction of f
  | LookupGeneric of Typed.generic_function
  | LookupInstantiation of Typed.generic_instantiation
  | LookupType of t
  | LookupEnum of (path * Loc.t * int) (* path, location, index *)
  | LookupConstant of var
  | LookupBuiltinFunction of f
  | LookupNotFound

(* Location within the environment hierarchy *)
type location = Top | InModule of string | InContext of string * context | InFunction of string * f

(* Unified environment type that tracks current location internally *)
type env =
  {modules: m Map.t; builtin_functions: (unit -> Typed.fun_type) Map.t; builtin_types: t Map.t; location: location}

(* Legacy type aliases for backward compatibility during transition *)
type in_top = env

type in_module = env

type in_context = env

type in_func = env

type extension = VCVPrototype

let vcv_prototype_builtins : (string * (unit -> Typed.fun_type)) list =
  Typed.
    [ ("sampletime", C.unit_real)
    ; ("display", C.string_unit)
    ; ("stringAppend", C.string_string_string)
    ; ("getKnob", C.int_real)
    ; ("getSwitch", C.int_bool)
    ; ("setLight", C.int_real_real_real_unit)
    ; ("setSwitchLight", C.int_real_real_real_unit) ]

let builtins_for_extension (ext : extension) : (string * (unit -> Typed.fun_type)) list =
  match ext with VCVPrototype -> vcv_prototype_builtins

let builtin_functions =
  Typed.
    [ ("size", C.array_size)
    ; ("abs", C.num_num)
    ; ("exp", C.freal_freal)
    ; ("log10", C.freal_freal)
    ; ("log", C.freal_freal)
    ; ("sin", C.freal_freal)
    ; ("cos", C.freal_freal)
    ; ("floor", C.freal_freal)
    ; ("ceil", C.freal_freal)
    ; ("tanh", C.freal_freal)
    ; ("asin", C.freal_freal)
    ; ("acos", C.freal_freal)
    ; ("atan", C.freal_freal)
    ; ("atan2", C.real_real_real)
    ; ("min", C.num_num_num)
    ; ("max", C.num_num_num)
    ; ("pow", C.real_real_real)
    ; ("cosh", C.freal_freal)
    ; ("sinh", C.freal_freal)
    ; ("tan", C.freal_freal)
    ; ("sqrt", C.freal_freal)
    ; ("clip", C.clip)
    ; ("int", C.valid_int)
    ; ("real", C.valid_real)
    ; ("fix16", C.valid_fix16)
    ; ("int16", C.valid_int16)
    ; ("string", C.valid_string)
    ; ("bool", C.valid_bool)
    ; ("u-", C.num_num)
    ; ("+", C.numstr_numstr_numstr)
    ; ("-", C.num_num_num)
    ; ("*", C.num_num_num)
    ; ("/", C.num_num_num)
    ; ("%", C.num_num_num)
    ; (">", C.a_a_bool)
    ; ("<", C.a_a_bool)
    ; ("==", C.a_a_bool)
    ; ("<>", C.a_a_bool)
    ; (">=", C.a_a_bool)
    ; ("<=", C.a_a_bool)
    ; ("|", C.int_int_int)
    ; ("&", C.int_int_int)
    ; ("^", C.int_int_int)
    ; (">>", C.int_int_int)
    ; ("<<", C.int_int_int)
    ; ("not", C.bool_bool)
    ; ("||", C.bool_bool_bool)
    ; ("&&", C.bool_bool_bool)
    ; ("eps", C.unit_real)
    ; ("pi", C.unit_real)
    ; ("random", C.unit_real)
    ; ("irandom", C.unit_int)
    ; ("samplerate", C.unit_real)
    ; ("wrap_array", C.wrap_array)
    ; ("length", C.str_length)
    ; ("list_size", C.list_size)
    ; ("list_append", C.list_append)
    ; ("list_insert", C.list_insert)
    ; ("list_remove", C.list_remove)
    ; ("list_clear", C.list_clear)
    ; ("list_reserve", C.list_reserve)
    ; ("list_capacity", C.list_capacity)
    ; ("list_get", C.list_get)
    ; ("list_set", C.list_set) ]
  |> Map.of_list

let builtin_types =
  ["int"; "int16"; "real"; "fix16"; "bool"; "string"; "unit"]
  |> CCList.map (fun n ->
      ( n
      , { path= Pparser.Syntax.{id= n; n= None; loc= Loc.default}
        ; descr= Simple
        ; index= 0
        ; loc= Loc.default
        ; generated= false } ) )
  |> Map.of_list

let rec isBuiltinType (t : Typed.type_) : bool =
  match t.tx with
  | TEId {id; n= None; _} ->
      Map.find id builtin_types <> None
  | TELink t ->
      isBuiltinType t
  | _ ->
      false

let makeFunctionForBuiltin name t : f =
  {path= {id= name; n= None; loc= Loc.default}; t; context= None; locals= []; tick= 0; args= None}

(* Helper functions to extract current state *)
let getCurrentModule (env : env) : m =
  match env.location with
  | InModule name | InContext (name, _) | InFunction (name, _) -> (
    match Map.find name env.modules with
    | Some m ->
        m
    | None ->
        failwith ("Internal error in getCurrentModule: module '" ^ name ^ "' not found in env.modules") )
  | Top ->
      failwith "Internal error in getCurrentModule: called at Top level (not inside any module)"

let getCurrentContext (env : env) : context =
  match env.location with InContext (_, context) -> context | InFunction (_, f) -> f.context | _ -> None

let getCurrentFunction (env : env) : f =
  match env.location with
  | InFunction (_, f) ->
      f
  | InModule name ->
      failwith ("Internal error in getCurrentFunction: currently in module '" ^ name ^ "', not a function")
  | InContext (name, _) ->
      failwith ("Internal error in getCurrentFunction: currently in context '" ^ name ^ "', not a function")
  | Top ->
      failwith "Internal error in getCurrentFunction: currently at Top level, not in a function"

(* Private helper functions for internal use only *)
let rec lookVarInScopes (scopes : var Map.t list) name : var option =
  match scopes with
  | [] ->
      None
  | h :: t -> (
    match Map.find name h with Some found -> Some found | None -> lookVarInScopes t name )

let lookVarInContext (context : context) name : var option =
  match context with Some (_, {descr= Record members; _}) -> Map.find name members | _ -> None

let registerArguments (args : Typed.arg list) =
  let locals = Map.empty () in
  let report loc (found : var) =
    Error.raiseError
      ("A variable with the name '" ^ found.name ^ "' has already been declared at " ^ Loc.to_string_readable found.loc)
      loc
  in
  let rev_args =
    CCList.fold_left
      (fun acc ({name; t; loc} : Typed.arg) ->
        let () = Map.update (report loc) name {name; t; kind= Val; tags= []; loc} locals in
        t :: acc )
      [] args
  in
  (locals, CCList.rev rev_args)

let registerContextLocal loc locals (context : context) =
  let report loc (found : var) = Error.raiseError ("The name '" ^ found.name ^ "' is reserved.") loc in
  match context with
  | Some (p, _) ->
      let name = "_ctx" in
      let t = Typed.C.path_t loc p in
      Map.update (report loc) name {name; t; kind= Val; tags= []; loc} locals ;
      locals
  | None ->
      locals

let lookVar (env : env) (name : string) (loc : Loc.t) : var =
  let f = getCurrentFunction env in
  let m = getCurrentModule env in
  match lookVarInContext f.context name with
  | Some found ->
      found
  | None -> (
    match lookVarInScopes f.locals name with
    | Some found ->
        found
    | None -> (
      match Map.find name m.constants with
      | Some var ->
          var
      | None ->
          Error.raiseError ("The variable '" ^ name ^ "' could not be found") loc ) )

let reportModuleNotFound n loc = Error.raiseError ("The module named '" ^ n ^ "' could not be found") loc

let lookEnum (env : env) (path : path) (loc : Loc.t) =
  let error () = Error.raiseError ("An enumeration with the name '" ^ pathString path ^ "' could not be found") loc in
  let findEnumInModule enums id =
    match Map.find id enums with
    | Some ({descr= Enum members; _} as t) -> (
      match Map.find id members with Some (_, index, _) -> (t.path, t.loc, index) | None -> error () )
    | _ ->
        error ()
  in
  match path with
  | {id; n= None; _} ->
      let m = getCurrentModule env in
      findEnumInModule m.enums id
  | {id; n= Some n; loc} -> (
    match Map.find n env.modules with Some m -> findEnumInModule m.enums id | None -> reportModuleNotFound n loc )

let getType (env : env) (path : path) : t option =
  match path with
  | {id; n= Some n; loc} -> (
    match Map.find n env.modules with None -> reportModuleNotFound n loc | Some m -> Map.find id m.types )
  | _ ->
      None

let addConstant (env : env) _unify (name : string) (t : Typed.type_) loc : env =
  let m = getCurrentModule env in
  let report (found : var) =
    Error.raiseError
      ("A constant with the name '" ^ found.name ^ "' has already been declared at " ^ Loc.to_string_readable found.loc)
      loc
  in
  Map.update report name {name; t; kind= Const; tags= []; loc} m.constants ;
  env

(* Helper: Check if a mem variable with the same name already exists in the context *)
let checkDuplicatedMem (context : context) (name : string) (loc : Loc.t) : unit =
  match context with
  | Some (_, {descr= Record members; _}) -> (
    match Map.find name members with
    | None ->
        ()
    | Some found ->
        Error.raiseError
          ( "A mem variable with the name '" ^ found.name ^ "' has already been declared at "
          ^ Loc.to_string_readable found.loc )
          loc )
  | _ ->
      ()

(* Helper: Check if a val variable with the same name already exists in any scope *)
let checkDuplicatedVal (locals : var Map.t list) (name : string) (loc : Loc.t) : unit =
  CCList.iter
    (fun (scope : var Map.t) ->
      match Map.find name scope with
      | None ->
          ()
      | Some found ->
          Error.raiseError
            ( "A variable with the name '" ^ found.name ^ "' has already been declared at "
            ^ Loc.to_string_readable found.loc )
            loc )
    locals

(* Helper: Check if a constant with the same name exists in the module *)
let checkDuplicatedConstant (env : env) (name : string) (loc : Loc.t) : unit =
  let m = getCurrentModule env in
  match Map.find name m.constants with
  | None ->
      ()
  | Some found ->
      Error.raiseError
        ( "A constant with the name '" ^ found.name ^ "' has already been declared at "
        ^ Loc.to_string_readable found.loc ^ ". Local variables cannot shadow constants." )
        loc

(* Helper: Check if any argument names conflict with mem variables in the context or constants *)
let checkArgumentsAgainstContext (env : env) (context : context) (args : Typed.arg list) : unit =
  let m = getCurrentModule env in
  (* Check against mem variables in context *)
  let () =
    match context with
    | Some (_, {descr= Record members; _}) ->
        CCList.iter
          (fun ({name; loc; _} : Typed.arg) ->
            match Map.find name members with
            | None ->
                ()
            | Some found ->
                Error.raiseError
                  ( "Function parameter '" ^ name ^ "' shadows a mem variable declared at "
                  ^ Loc.to_string_readable found.loc
                  ^ ". Rename the parameter or the mem variable to avoid this conflict." )
                  loc )
          args
    | _ ->
        ()
  in
  (* Check against module constants *)
  CCList.iter
    (fun ({name; loc; _} : Typed.arg) ->
      match Map.find name m.constants with
      | None ->
          ()
      | Some found ->
          Error.raiseError
            ( "Function parameter '" ^ name ^ "' shadows a constant declared at " ^ Loc.to_string_readable found.loc
            ^ ". Rename the parameter or the constant to avoid this conflict." )
            loc )
    args

(* Helper: Create a reporter for mem variable updates that handles type unification *)
let makeMemReporter (unify : Typed.type_ -> Typed.type_ -> bool) (t : Typed.type_) (found : var) (value : var) : var =
  if unify found.t t then
    let tags = Pparser.Ptags.mergeTags found.tags value.tags in
    {found with tags}
  else
    let old_type = Pla.print (Typed.print_type_ found.t) in
    let new_type = Pla.print (Typed.print_type_ t) in
    Error.raiseError
      ( "This declaration tries to change the type of " ^ found.name ^ ". The previous type is '" ^ old_type
      ^ "' and the new is '" ^ new_type ^ "'" )
      value.loc

(* Helper: Add a mem or inst variable to the context record *)
let addMemOrInst (f : f) (members : var Map.t) (unify : Typed.type_ -> Typed.type_ -> bool) (name : string)
    (t : Typed.type_) (kind : var_kind) (tags : Pparser.Ptags.tag list) (loc : Loc.t) (env : env) : env =
  let () = checkDuplicatedVal f.locals name loc in
  let report_mem = makeMemReporter unify t in
  Map.update report_mem name {name; t; kind; tags; loc} members ;
  env

(* Helper: Add a val variable to the current local scope *)
let addValVar (f : f) (context : context) (name : string) (t : Typed.type_) (loc : Loc.t) (env : env) : env =
  let report (found : var) =
    Error.raiseError
      ("A variable with the name '" ^ found.name ^ "' has already been declared at " ^ Loc.to_string_readable found.loc)
      loc
  in
  let () = checkDuplicatedMem context name loc in
  let () = checkDuplicatedConstant env name loc in
  match f.locals with
  | [] ->
      failwith ("Internal error in addVar: no local scope when adding variable '" ^ name ^ "'")
  | h :: _ ->
      Map.update report name {name; t; kind= Val; tags= []; loc} h ;
      env

let addVar (env : env) unify (name : string) (t : Typed.type_) (kind : var_kind) loc : env =
  let f = getCurrentFunction env in
  let context = getCurrentContext env in
  match (kind, context) with
  | Inst, Some (_, {descr= Record members; _}) ->
      addMemOrInst f members unify name t kind [] loc env
  | Mem tags, Some (_, {descr= Record members; _}) ->
      addMemOrInst f members unify name t kind tags loc env
  | (Mem _ | Inst), None ->
      failwith ("Internal error in addVar: cannot add mem/inst variable '" ^ name ^ "' to function with no context")
  | Val, context ->
      addValVar f context name t loc env
  | Const, _ ->
      failwith ("Internal error in addVar: use addConstant instead for constant '" ^ name ^ "'")
  | _, Some _ ->
      failwith ("Internal error in addVar: context exists but is not a Record for variable '" ^ name ^ "'")

let checkMemExists (env : env) name =
  let f = getCurrentFunction env in
  match f.context with
  | Some (_, {descr= Record members; _}) -> (
    match Map.find name members with None -> false | Some _ -> true )
  | _ ->
      false

let checkConstantExists (env : env) (name : string) : bool =
  let m = getCurrentModule env in
  match Map.find name m.constants with None -> false | Some _ -> true

let addReturnVar (env : env) (name : string) (t : Typed.type_) loc : env =
  let () = Typed.setTypeMut t in
  match getCurrentContext env with
  | Some (_, {descr= Record members; _}) -> (
    match Map.find name members with
    | Some found ->
        Error.raiseError
          ( "Return variable '" ^ name ^ "' conflicts with a mem variable declared at "
          ^ Loc.to_string_readable found.loc ^ ". Consider using different names for your mem variables." )
          loc
    | None ->
        let report_mem found _ = found in
        Map.update report_mem name {name; t; kind= Mem []; tags= []; loc} members ;
        env )
  | None ->
      failwith ("Internal error in addReturnVar: cannot add return variable '" ^ name ^ "' to function with no context")
  | Some _ ->
      failwith ("Internal error in addReturnVar: context exists but is not a Record for variable '" ^ name ^ "'")

let pushScope (env : env) : env =
  let f = getCurrentFunction env in
  f.locals <- Map.empty () :: f.locals ;
  env

let popScope (env : env) : env =
  let f = getCurrentFunction env in
  match f.locals with
  | [] ->
      failwith ("Internal error in popScope: no scope to pop in function '" ^ f.path.id ^ "'")
  | _ :: t ->
      f.locals <- t ;
      env

let getPath m name loc : path = {id= name; n= Some m.name; loc}

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
  let t = {descr= Record (Map.empty ()); path; index; loc; generated= true} in
  let _ = Map.update (report name) type_name t m.types in
  {env with location= InContext (m.name, Some (path, t))}

let createContextForFunctionWithIndex (env : env) name loc (type_index : int) : env =
  let m = getCurrentModule env in
  let report name (found : t) =
    Error.raiseError
      ("A function with the name '" ^ name ^ "' already exists at " ^ Loc.to_string_readable found.loc)
      loc
  in
  let type_name = name ^ "_type" in
  let path = getPath m type_name loc in
  let t = {descr= Record (Map.empty ()); path; index= type_index; loc; generated= true} in
  let _ = Map.update (report name) type_name t m.types in
  {env with location= InContext (m.name, Some (path, t))}

let addAliasToContext (env : env) name loc : env =
  match getCurrentContext env with
  | Some (ctx, {descr= Record members; _}) when not (Map.is_empty members) -> (
      let m = getCurrentModule env in
      let type_name = name ^ "_type" in
      (* Check if alias already exists - skip if it does (can happen with multiple instantiations of generics) *)
      match Map.find type_name m.types with
      | Some _ ->
          env (* Alias already exists, skip *)
      | None ->
          let path = getPath m type_name loc in
          let index = getGlobalTick () in
          let t = {descr= Alias (path, ctx); path; index; loc; generated= true} in
          let report found =
            Error.raiseError ("A context with the same name already exists at " ^ Loc.to_string_readable found.loc) loc
          in
          let _ = Map.update report type_name t m.types in
          env )
  | _ ->
      env

let addRecordMember members =
  let report loc (found : var) =
    Error.raiseError
      ("A member with the name '" ^ found.name ^ "' has already been declared at " ^ Loc.to_string_readable found.loc)
      loc
  in
  let members =
    CCList.fold_left
      (fun m (name, t, tags, loc) ->
        Map.update (report loc) name {name; t; kind= Val; tags; loc} m ;
        m )
      (Map.empty ()) members
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
  let t = {path; descr; loc; index; generated= false} in
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
        Map.update (report loc) name (name, i, loc) m ;
        (m, i + 1) )
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
  let t = {path; descr; loc; index; generated= false} in
  let _ = Map.update report type_name t m.types in
  let env = addEnumToModule env members t in
  env

let createContextForExternal (env : env) : env =
  let m = getCurrentModule env in
  {env with location= InContext (m.name, None)}

let exitContext (env : env) : env =
  let m = getCurrentModule env in
  {env with location= InModule m.name}

(* Returns the current tick of the function without advancing it *)
let currentFunctionTick (env : env) : int = (getCurrentFunction env).tick

(* Advances the tick of the current function and returns the new value *)
let nextFunctionTick (env : env) : int =
  let f = getCurrentFunction env in
  let n = f.tick + 1 in
  f.tick <- n ;
  n

let getContext (env : env) : path =
  match getCurrentContext env with
  | Some (p, _) ->
      p
  | None ->
      failwith "Internal error in getContext: trying to get context but current function has no context"

let getFunctionContext (f : f) : path =
  match f.context with
  | Some (p, _) ->
      p
  | None ->
      failwith ("Internal error in getFunctionContext: function '" ^ f.path.id ^ "' has no context")

let enterFunction (env : env) (name : string) (args : Typed.arg list) (ret : Typed.type_) loc :
    env * path * (Typed.type_ list * Typed.type_) =
  let m = getCurrentModule env in
  let context = getCurrentContext env in
  let () = checkArgumentsAgainstContext env context args in
  let report (found : f) =
    Error.raiseError ("A function with the name '" ^ found.path.id ^ "' has already been declared.") loc
  in
  let path = getPath m name loc in
  let locals, args_t = registerArguments args in
  let locals = registerContextLocal loc locals context in
  let t = (args_t, ret) in
  let f : f = {path; t; context; locals= [locals]; tick= 0; args= Some args} in
  let _ = Map.update report name f m.functions in
  ({env with location= InFunction (m.name, f)}, path, t)

let isFunctionActive (f : f) =
  match f.context with Some (_, {descr= Record members; _}) -> not (Map.is_empty members) | _ -> false

let exitFunction (env : env) : env =
  let m = getCurrentModule env in
  let f = getCurrentFunction env in
  {env with location= InContext (m.name, f.context)}

(** Re-enter a function context for post-processing. Looks up the function by path
    and sets the environment to be inside that function. *)
let reenterFunction (env : env) (func_path : path) : env =
  let module_name = match func_path.n with Some n -> n | None -> (getCurrentModule env).name in
  match Map.find module_name env.modules with
  | None ->
      failwith ("reenterFunction: module '" ^ module_name ^ "' not found")
  | Some m -> (
    match Map.find func_path.id m.functions with
    | Some f ->
        {env with location= InFunction (module_name, f)}
    | None ->
        failwith ("reenterFunction: function '" ^ pathString func_path ^ "' not found") )

let addCustomInitFunction (env : env) name =
  match getCurrentContext env with
  | Some (p, _) ->
      let m = getCurrentModule env in
      m.init <- (p, name) :: m.init ;
      env
  | _ ->
      env

let enterModule (env : env) (name : string) : env =
  match Map.find name env.modules with
  | Some _ ->
      {env with location= InModule name}
  | None ->
      let report _ = failwith ("Internal error in enterModule: duplicate module '" ^ name ^ "'") in
      let m : m =
        { name
        ; functions= Map.empty ()
        ; generics= Map.empty ()
        ; instantiated= SignatureMap.empty ()
        ; types= Map.empty ()
        ; enums= Map.empty ()
        ; init= []
        ; constants= Map.empty () }
      in
      let () = Map.update report name m env.modules in
      {env with location= InModule name}

let exitModule (env : env) : env = {env with location= Top}

(* Generic lookup function for paths - returns list of all possible meanings *)
(* Helper: Prepend to results if option is Some *)
let consOpt (make : 'a -> lookup_result) (opt : 'a option) (results : lookup_result list) : lookup_result list =
  match opt with Some x -> make x :: results | None -> results

let lookupPath (env : env) (path : path) : lookup_result list =
  let lookupInModule (m : m) (id : string) : lookup_result list =
    (* Find enum member if applicable *)
    let enumResult =
      match Map.find id m.enums with
      | Some ({descr= Enum members; _} as t) -> (
        match Map.find id members with Some (_, index, loc) -> Some (LookupEnum (t.path, loc, index)) | None -> None )
      | _ ->
          None
    in
    []
    |> consOpt (fun f -> LookupFunction f) (Map.find id m.functions)
    |> consOpt (fun t -> LookupType t) (Map.find id m.types)
    |> consOpt (fun var -> LookupConstant var) (Map.find id m.constants)
    |> consOpt Fun.id enumResult
    |> consOpt (fun g -> LookupGeneric g) (Map.find id m.generics)
  in
  let lookupLocalVar (f : f) (id : string) : var option =
    match lookVarInContext f.context id with Some var -> Some var | None -> lookVarInScopes f.locals id
  in
  match path with
  | {id; n= Some module_name; _} -> (
    (* Module-qualified path: Module.name *)
    match Map.find module_name env.modules with
    | Some m ->
        lookupInModule m id
    | None ->
        [] )
  | {id; n= None; _} ->
      (* Local path: name - collect all possible meanings *)
      let results = [] in
      (* Check local scope first (variables have priority) *)
      let results =
        match env.location with
        | InFunction (_, f) ->
            consOpt (fun var -> LookupVar var) (lookupLocalVar f id) results
        | _ ->
            results
      in
      (* Add module-level symbols *)
      let results =
        match env.location with
        | InFunction (_, _) | InModule _ | InContext (_, _) ->
            lookupInModule (getCurrentModule env) id @ results
        | Top ->
            results
      in
      (* Add builtin functions and types *)
      results
      |> consOpt (fun f -> LookupBuiltinFunction (makeFunctionForBuiltin id (f ()))) (Map.find id env.builtin_functions)
      |> consOpt (fun t -> LookupType t) (Map.find id env.builtin_types)

(* Helper functions to find specific lookup result types from a list *)
let findType (results : lookup_result list) : t option =
  let rec find = function [] -> None | LookupType t :: _ -> Some t | _ :: rest -> find rest in
  find results

let findFunction (results : lookup_result list) : f option =
  let rec findLocal = function [] -> None | LookupFunction f :: _ -> Some f | _ :: rest -> findLocal rest in
  let rec findBuiltin = function
    | [] ->
        None
    | LookupBuiltinFunction f :: _ ->
        Some f
    | _ :: rest ->
        findBuiltin rest
  in
  match findLocal results with Some f -> Some f | None -> findBuiltin results

let findVar (results : lookup_result list) : var option =
  let rec find = function
    | [] ->
        None
    | LookupVar var :: _ ->
        Some var
    | LookupConstant var :: _ ->
        Some var
    | _ :: rest ->
        find rest
  in
  find results

let findEnum (results : lookup_result list) : (path * Loc.t * int) option =
  let rec find = function
    | [] ->
        None
    | LookupEnum (path, loc, index) :: _ ->
        Some (path, loc, index)
    | _ :: rest ->
        find rest
  in
  find results

let findGeneric (results : lookup_result list) : Typed.generic_function option =
  let rec find = function [] -> None | LookupGeneric generic :: _ -> Some generic | _ :: rest -> find rest in
  find results

(* Function lookup using the new generic lookup system *)
let lookFunctionCall (env : env) (path : path) (loc : Loc.t) : f =
  match findFunction (lookupPath env path) with
  | Some f ->
      f
  | None ->
      Error.raiseError ("A function with the name '" ^ pathString path ^ "' could not be found") loc

(** Try to find a function without raising an error *)
let tryLookFunctionCall (env : env) (path : path) : f option = findFunction (lookupPath env path)

(* Operator lookup using the new generic lookup system *)
let lookOperator (env : env) (op : string) : f =
  let op_path : path = {id= op; n= None; loc= Loc.default} in
  match findFunction (lookupPath env op_path) with
  | Some f ->
      f
  | None ->
      failwith ("Internal error in lookOperator: builtin operator '" ^ op ^ "' not found")

(* Since operators are only builtins, this behaves the same as lookOperator *)
let lookOperatorInModule (env : env) (op : string) : f = lookOperator env op

(* Generic management functions *)
let addGeneric (env : env) (generic : Typed.generic_function) : env =
  match env.location with
  | InModule name -> (
      let module_opt = Map.find name env.modules in
      match module_opt with
      | Some m ->
          let () =
            Map.update
              (fun _ _ -> failwith ("Internal error in addGeneric: duplicate generic '" ^ generic.name ^ "'"))
              generic.name generic m.generics
          in
          env
      | None ->
          failwith ("Internal error in addGeneric: module '" ^ name ^ "' not found in env.modules") )
  | InContext (name, _) ->
      failwith ("Internal error in addGeneric: cannot add generic '" ^ generic.name ^ "' from context '" ^ name ^ "'")
  | InFunction (name, _) ->
      failwith
        ( "Internal error in addGeneric: cannot add generic '" ^ generic.name ^ "' from function in module '" ^ name
        ^ "'" )
  | Top ->
      failwith ("Internal error in addGeneric: cannot add generic '" ^ generic.name ^ "' at Top level")

(* Generic lookup using the new generic lookup system *)
let lookupGeneric (env : env) (path : path) : Typed.generic_function option = findGeneric (lookupPath env path)

(** Check if a name matches a companion function in the given generic's next chain *)
let rec hasCompanionNamed (name : string) (next : Pparser.Syntax.function_def option) : bool =
  match next with
  | None ->
      false
  | Some def ->
      if String.equal def.name name then true else hasCompanionNamed name def.next

(** Look up a generic function by companion name.
    Returns the parent generic function if the given name is a companion of any generic. *)
let lookupGenericByCompanion (env : env) (path : path) : Typed.generic_function option =
  (* Get the module to search in *)
  let module_name =
    match path.n with
    | Some n ->
        n
    | None -> (
      match env.location with
      | InModule name ->
          name
      | InContext (name, _) ->
          name
      | InFunction (name, _) ->
          name
      | Top ->
          "" )
  in
  match Map.find module_name env.modules with
  | None ->
      None
  | Some m ->
      (* Search through all generic functions in this module *)
      let companion_name = path.id in
      Map.fold
        (fun _gen_name generic acc ->
          match acc with
          | Some _ ->
              acc
          | None ->
              if hasCompanionNamed companion_name generic.Typed.next then Some generic else acc )
        None m.generics

let addInstantiation (env : env) (instantiation : Typed.generic_instantiation) : env =
  let module_name =
    match env.location with
    | InModule name ->
        name
    | InContext (name, _) ->
        name
    | InFunction (name, _) ->
        name
    | Top ->
        failwith
          ( "Internal error in addInstantiation: cannot add instantiation '" ^ instantiation.specialized_name
          ^ "' at Top level" )
  in
  let module_opt = Map.find module_name env.modules in
  match module_opt with
  | Some m ->
      let () =
        SignatureMap.update
          (fun _ _ ->
            failwith
              ("Internal error in addInstantiation: duplicate instantiation '" ^ instantiation.specialized_name ^ "'") )
          instantiation.signature instantiation m.instantiated
      in
      env
  | None ->
      failwith ("Internal error in addInstantiation: module '" ^ module_name ^ "' not found")

let findInstantiation (env : env) (signature : Typed.instantiation_signature) : Typed.generic_instantiation option =
  let module_name =
    match env.location with
    | InModule name ->
        Some name
    | InContext (name, _) ->
        Some name
    | InFunction (name, _) ->
        Some name
    | Top ->
        None
  in
  match module_name with
  | Some module_name -> (
      let module_opt = Map.find module_name env.modules in
      match module_opt with Some m -> SignatureMap.find signature m.instantiated | None -> None )
  | None ->
      None

(* Unified expression lookup for handling ambiguous symbols *)
(* Expression evaluation context *)
type expr_context = {in_constant: bool; in_generic_arg: bool}

type expression_symbol =
  | ExprVariable of var
  | ExprFunction of f
  | ExprType of t
  | ExprEnum of (path * Loc.t * int)
  | ExprNotFound

let lookupExpressionSymbol (env : env) (path : path) (context : expr_context) : expression_symbol =
  let results = lookupPath env path in
  if context.in_constant then
    (* In constant context: constants first, then enums, then types *)
    match findVar results with
    | Some var when var.kind = Const ->
        ExprVariable var
    | _ -> (
      match findEnum results with
      | Some enum_data ->
          ExprEnum enum_data
      | None -> (
        match findType results with Some t -> ExprType t | None -> ExprNotFound ) )
  else if context.in_generic_arg then
    (* In generic argument context: variables first, then functions (allowed as references), then enums *)
    match findVar results with
    | Some var ->
        ExprVariable var
    | None -> (
      match findFunction results with
      | Some f ->
          ExprFunction f (* Allow function references in generic context *)
      | None -> (
        match findEnum results with Some enum_data -> ExprEnum enum_data | None -> ExprNotFound ) )
  else
    (* In regular context: variables first, then functions (require call), then enums *)
    match findVar results with
    | Some var ->
        ExprVariable var
    | None -> (
      match findFunction results with
      | Some f ->
          ExprFunction f
      | None -> (
        match findEnum results with Some enum_data -> ExprEnum enum_data | None -> ExprNotFound ) )

(* Unified type lookup function using the new lookup system *)
let lookType (env : env) (path : path) (loc : Loc.t) : t =
  match findType (lookupPath env path) with
  | Some t ->
      t
  | None ->
      Error.raiseError ("A type with the name '" ^ pathString path ^ "' could not be found") loc

let builtin_functions_with_extensions (extensions : extension list) : (unit -> Typed.fun_type) Map.t =
  CCList.fold_left
    (fun m ext ->
      CCList.iter (fun (name, t) -> m := Maps.Map.add name t !m) (builtins_for_extension ext) ;
      m )
    builtin_functions extensions

let empty ?(extensions : extension list = []) () : env =
  {modules= Map.empty (); builtin_functions= builtin_functions_with_extensions extensions; builtin_types; location= Top}
