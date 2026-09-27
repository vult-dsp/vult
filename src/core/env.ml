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

  let add (key : string) (value : 'a) (t : 'a t) : unit = t := Map.add key value !t

  let is_empty (t : 'a t) : bool = Map.is_empty !t

  let fold (f : string -> 'a -> 'b -> 'b) (s : 'b) (t : 'a t) : 'b = Map.fold f !t s
end

type var_kind = Mem of Pparser.Ptags.tags | Inst | Val | Const

(* The kinds of variables declared inside a function. Constants are added with addConstant. *)
type local_kind = LocalVal | LocalMem of Pparser.Ptags.tags | LocalInst

type path = Typed.path

type var = {name: string; t: Typed.type_; kind: var_kind; tags: Pparser.Ptags.tags; loc: Loc.t}

type type_descr = Simple | Record of var Map.t | Enum of (string * int * Loc.t) Map.t | Alias of path * path

type t = {path: path; descr: type_descr; index: int; loc: Loc.t; generated: bool}

(* The record that holds the memory of a function and of its companions *)
type context = {path: path; t: t; members: var Map.t}

(* The context of a function together with the type of its '_ctx' argument. The companions
   share the context but each function has its own argument type. *)
type function_context = NoContext | Context of {context: context; arg_t: Typed.type_}

(* The context argument of a function: the path of its context and the type of '_ctx' *)
type context_argument = {context_path: path; arg_t: Typed.type_}

(* The local variables of a function: the innermost scope and the scopes that enclose it *)
type scopes = {current: var Map.t; enclosing: var Map.t list}

type f =
  { path: path
  ; t: Typed.type_ list * Typed.type_
  ; args: Typed.arg list option
  ; context: function_context
  ; mutable scopes: scopes
  ; mutable tick: int }

type m =
  { name: string
  ; functions: f Map.t
  ; generics: Typed.generic_function Map.t
  ; types: t Map.t
  ; mutable init: (path * path) list
  ; enums: t Map.t
  ; mutable constants: var Map.t }

(* Generic lookup result variant *)
type lookup_result =
  | LookupVar of var
  | LookupFunction of f
  | LookupGeneric of Typed.generic_function
  | LookupType of t
  | LookupEnum of (path * Loc.t * int) (* path, location, index *)
  | LookupConstant of var
  | LookupBuiltinFunction of f
  | LookupNotFound

(* Location within the environment hierarchy *)
type location = Top | InModule of m | InContext of m * context option | InFunction of m * f

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

(* The type of each builtin. Adding a constructor to [Builtin.t] fails here
   until it is given a signature. *)
let builtin_type (b : Builtin.t) =
  Typed.(
    match b with
    | Size ->
        C.array_size
    | Abs ->
        C.num_num
    | Exp ->
        C.freal_freal
    | Log10 ->
        C.freal_freal
    | Log ->
        C.freal_freal
    | Sin ->
        C.freal_freal
    | Cos ->
        C.freal_freal
    | Floor ->
        C.freal_freal
    | Ceil ->
        C.freal_freal
    | Tanh ->
        C.freal_freal
    | Asin ->
        C.freal_freal
    | Acos ->
        C.freal_freal
    | Atan ->
        C.freal_freal
    | Atan2 ->
        C.real_real_real
    | Min ->
        C.num_num_num
    | Max ->
        C.num_num_num
    | Pow ->
        C.real_real_real
    | Cosh ->
        C.freal_freal
    | Sinh ->
        C.freal_freal
    | Tan ->
        C.freal_freal
    | Sqrt ->
        C.freal_freal
    | Clip ->
        C.clip
    | Int ->
        C.valid_int
    | Real ->
        C.valid_real
    | Fix16 ->
        C.valid_fix16
    | Int16 ->
        C.valid_int16
    | String ->
        C.valid_string
    | Bool ->
        C.valid_bool
    | Not ->
        C.bool_bool
    | Eps ->
        C.unit_real
    | Pi ->
        C.unit_real
    | Random ->
        C.unit_real
    | Irandom ->
        C.unit_int
    | Samplerate ->
        C.unit_real
    | WrapArray ->
        C.wrap_array
    | Length ->
        C.str_length
    | ListAppend ->
        C.list_append
    | ListInsert ->
        C.list_insert
    | ListRemove ->
        C.list_remove
    | ListClear ->
        C.list_clear
    | ListReserve ->
        C.list_reserve
    | ListCapacity ->
        C.list_capacity
    | ListGet ->
        C.list_get
    | ListSet ->
        C.list_set )

(* Operators are spelled by their symbol and never reach [Builtin.t]: the
   parser turns them into EOp and EUnOp nodes. *)
let operator_functions =
  Typed.
    [ ("u-", C.num_num)
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
    ; ("||", C.bool_bool_bool)
    ; ("&&", C.bool_bool_bool) ]

let builtin_functions =
  Map.of_list (operator_functions @ CCList.map (fun b -> (Builtin.name b, builtin_type b)) Builtin.all)

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
  { path= {id= name; n= None; loc= Loc.default}
  ; t
  ; context= NoContext
  ; scopes= {current= Map.empty (); enclosing= []}
  ; tick= 0
  ; args= None }

(* Helper functions to extract current state *)
let getCurrentModule (env : env) : m =
  match env.location with
  | InModule m | InContext (m, _) | InFunction (m, _) ->
      m
  | Top ->
      failwith "Internal error in getCurrentModule: called at Top level (not inside any module)"

let contextOf (f : f) : context option = match f.context with Context {context; _} -> Some context | NoContext -> None

let getCurrentContext (env : env) : context option =
  match env.location with InContext (_, context) -> context | InFunction (_, f) -> contextOf f | _ -> None

let getCurrentFunction (env : env) : f =
  match env.location with
  | InFunction (_, f) ->
      f
  | InModule m ->
      failwith ("Internal error in getCurrentFunction: currently in module '" ^ m.name ^ "', not a function")
  | InContext (m, _) ->
      failwith ("Internal error in getCurrentFunction: currently in context '" ^ m.name ^ "', not a function")
  | Top ->
      failwith "Internal error in getCurrentFunction: currently at Top level, not in a function"

(* Private helper functions for internal use only *)
(* The scopes from the innermost to the outermost *)
let allScopes (scopes : scopes) : var Map.t list = scopes.current :: scopes.enclosing

let lookVarInScopes (scopes : scopes) name : var option =
  CCList.find_map (fun scope -> Map.find name scope) (allScopes scopes)

let lookVarInContext (context : context option) name : var option =
  match context with Some context -> Map.find name context.members | None -> None

(* A name that starts with an uppercase letter is read as an enumeration value, so a binding
   spelled that way could never be read back. Rejecting it at the declaration points at the name
   itself instead of at its first use. *)
let context_name = "_ctx"

(* The context is passed to the functions as the argument '_ctx' *)
let checkReservedName (name : string) (loc : Loc.t) : unit =
  if String.equal name context_name then Error.raiseError ("The name '" ^ name ^ "' is reserved.") loc

let checkBindingName (kind : string) (name : string) (loc : Loc.t) : unit =
  checkReservedName name loc ;
  if Pparser.Syntax.startsUppercase name then
    Error.raiseError
      ( "The " ^ kind ^ " '" ^ name
      ^ "' starts with an uppercase letter, which is reserved for enumeration values. Rename it to '"
      ^ String.uncapitalize_ascii name ^ "'." )
      loc

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
        let () = checkBindingName "argument" name loc in
        let () = Map.update (report loc) name {name; t; kind= Val; tags= []; loc} locals in
        t :: acc )
      [] args
  in
  (locals, CCList.rev rev_args)

let lookVar (env : env) (name : string) (loc : Loc.t) : var =
  let f = getCurrentFunction env in
  let m = getCurrentModule env in
  match lookVarInContext (contextOf f) name with
  | Some found ->
      found
  | None -> (
    match lookVarInScopes f.scopes name with
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
  let () = checkBindingName "constant" name loc in
  let m = getCurrentModule env in
  let report (found : var) =
    Error.raiseError
      ("A constant with the name '" ^ found.name ^ "' has already been declared at " ^ Loc.to_string_readable found.loc)
      loc
  in
  Map.update report name {name; t; kind= Const; tags= []; loc} m.constants ;
  env

(* Helper: Check if a mem variable with the same name already exists in the context *)
let checkDuplicatedMem (context : context option) (name : string) (loc : Loc.t) : unit =
  match context with
  | Some context -> (
    match Map.find name context.members with
    | None ->
        ()
    | Some found ->
        Error.raiseError
          ( "A mem variable with the name '" ^ found.name ^ "' has already been declared at "
          ^ Loc.to_string_readable found.loc )
          loc )
  | None ->
      ()

(* Helper: Check if a val variable with the same name already exists in any scope *)
let checkDuplicatedVal (scopes : scopes) (name : string) (loc : Loc.t) : unit =
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
    (allScopes scopes)

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
let checkArgumentsAgainstContext (env : env) (context : context option) (args : Typed.arg list) : unit =
  let m = getCurrentModule env in
  (* Check against mem variables in context *)
  let () =
    match context with
    | Some context ->
        CCList.iter
          (fun ({name; loc; _} : Typed.arg) ->
            match Map.find name context.members with
            | None ->
                ()
            | Some found ->
                Error.raiseError
                  ( "Function parameter '" ^ name ^ "' shadows a mem variable declared at "
                  ^ Loc.to_string_readable found.loc
                  ^ ". Rename the parameter or the mem variable to avoid this conflict." )
                  loc )
          args
    | None ->
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
  let () =
    match kind with Mem _ -> checkBindingName "variable" name loc | Inst | Val | Const -> checkReservedName name loc
  in
  let () = checkDuplicatedVal f.scopes name loc in
  let report_mem = makeMemReporter unify t in
  Map.update report_mem name {name; t; kind; tags; loc} members ;
  env

(* Helper: Add a val variable to the current local scope *)
let addValVar (f : f) (context : context option) (name : string) (t : Typed.type_) (loc : Loc.t) (env : env) : env =
  let report (found : var) =
    Error.raiseError
      ("A variable with the name '" ^ found.name ^ "' has already been declared at " ^ Loc.to_string_readable found.loc)
      loc
  in
  let () = checkBindingName "variable" name loc in
  let () = checkDuplicatedMem context name loc in
  let () = checkDuplicatedConstant env name loc in
  Map.update report name {name; t; kind= Val; tags= []; loc} f.scopes.current ;
  env

let addVar (env : env) unify (name : string) (t : Typed.type_) (kind : local_kind) loc : env =
  let f = getCurrentFunction env in
  let context = getCurrentContext env in
  match (kind, context) with
  | LocalInst, Some context ->
      addMemOrInst f context.members unify name t Inst [] loc env
  | LocalMem tags, Some context ->
      addMemOrInst f context.members unify name t (Mem tags) tags loc env
  | (LocalMem _ | LocalInst), None ->
      Error.raiseError ("The variable '" ^ name ^ "' cannot be stored in memory. Only functions have memory.") loc
  | LocalVal, context ->
      addValVar f context name t loc env

let checkMemExists (env : env) name =
  let f = getCurrentFunction env in
  match contextOf f with
  | Some context -> (
    match Map.find name context.members with None -> false | Some _ -> true )
  | None ->
      false

let checkConstantExists (env : env) (name : string) : bool =
  let m = getCurrentModule env in
  match Map.find name m.constants with None -> false | Some _ -> true

let addReturnVar (env : env) (name : string) (t : Typed.type_) loc : env =
  let () = Typed.setTypeMut t in
  match getCurrentContext env with
  | Some context -> (
    match Map.find name context.members with
    | Some found ->
        Error.raiseError
          ( "Return variable '" ^ name ^ "' conflicts with a mem variable declared at "
          ^ Loc.to_string_readable found.loc ^ ". Consider using different names for your mem variables." )
          loc
    | None ->
        let report_mem found _ = found in
        Map.update report_mem name {name; t; kind= Mem []; tags= []; loc} context.members ;
        env )
  | None ->
      Error.raiseError "Only functions can return multiple values." loc

(* Runs [body] with a new innermost scope for the local variables of the current function *)
let withScope (env : env) (body : env -> env * 'a) : env * 'a =
  let f = getCurrentFunction env in
  let outer = f.scopes in
  f.scopes <- {current= Map.empty (); enclosing= outer.current :: outer.enclosing} ;
  let env, result = body env in
  f.scopes <- outer ;
  (env, result)

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
  let members = Map.empty () in
  let t = {descr= Record members; path; index; loc; generated= true} in
  let _ = Map.update (report name) type_name t m.types in
  {env with location= InContext (m, Some {path; t; members})}

let createContextForFunctionWithIndex (env : env) name loc (type_index : int) : env =
  let m = getCurrentModule env in
  let report name (found : t) =
    Error.raiseError
      ("A function with the name '" ^ name ^ "' already exists at " ^ Loc.to_string_readable found.loc)
      loc
  in
  let type_name = name ^ "_type" in
  let path = getPath m type_name loc in
  let members = Map.empty () in
  let t = {descr= Record members; path; index= type_index; loc; generated= true} in
  let _ = Map.update (report name) type_name t m.types in
  {env with location= InContext (m, Some {path; t; members})}

let addAliasToContext (env : env) name loc : env =
  match getCurrentContext env with
  | Some context when not (Map.is_empty context.members) -> (
      let m = getCurrentModule env in
      let type_name = name ^ "_type" in
      (* Check if alias already exists - skip if it does (can happen with multiple instantiations of generics) *)
      match Map.find type_name m.types with
      | Some _ ->
          env (* Alias already exists, skip *)
      | None ->
          let path = getPath m type_name loc in
          let index = getGlobalTick () in
          let t = {descr= Alias (path, context.path); path; index; loc; generated= true} in
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
  {env with location= InContext (m, None)}

let exitContext (env : env) : env =
  let m = getCurrentModule env in
  {env with location= InModule m}

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
  | Some context ->
      context.path
  | None ->
      failwith "Internal error in getContext: trying to get context but current function has no context"

let contextArgument (f : f) : context_argument option =
  match f.context with Context {context; arg_t} -> Some {context_path= context.path; arg_t} | NoContext -> None

(* The context argument of a function that has memory, which the callers have to pass *)
let activeContextArgument (f : f) : context_argument option =
  match f.context with
  | Context {context; arg_t} when not (Map.is_empty context.members) ->
      Some {context_path= context.path; arg_t}
  | Context _ | NoContext ->
      None

(* The context argument of the current function, used to pass memory to the functions it calls *)
let currentContextArgument (env : env) (loc : Loc.t) : context_argument =
  match contextArgument (getCurrentFunction env) with
  | Some argument ->
      argument
  | None ->
      Error.raiseError "Functions with memory variables can only be called from other functions." loc

let enterFunction (env : env) (name : string) (args : Typed.arg list) (ret : Typed.type_) loc :
    env * path * (Typed.type_ list * Typed.type_) =
  let m = getCurrentModule env in
  let context = getCurrentContext env in
  let () = checkArgumentsAgainstContext env context args in
  let report (found : f) =
    Error.raiseError ("A function with the name '" ^ found.path.id ^ "' has already been declared.") loc
  in
  let () =
    match Map.find name m.generics with
    | Some (found : Typed.generic_function) ->
        Error.raiseError
          ("A function with the name '" ^ name ^ "' has already been declared at " ^ Loc.to_string_readable found.loc)
          loc
    | None ->
        ()
  in
  let path = getPath m name loc in
  let locals, args_t = registerArguments args in
  let context =
    match context with Some context -> Context {context; arg_t= Typed.C.path_t loc context.path} | None -> NoContext
  in
  let t = (args_t, ret) in
  let f : f = {path; t; context; scopes= {current= locals; enclosing= []}; tick= 0; args= Some args} in
  let _ = Map.update report name f m.functions in
  ({env with location= InFunction (m, f)}, path, t)

let isFunctionActive (f : f) = Option.is_some (activeContextArgument f)

let exitFunction (env : env) : env =
  let m = getCurrentModule env in
  let f = getCurrentFunction env in
  {env with location= InContext (m, contextOf f)}

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
        {env with location= InFunction (m, f)}
    | None ->
        failwith ("reenterFunction: function '" ^ pathString func_path ^ "' not found") )

let addCustomInitFunction (env : env) name =
  match getCurrentContext env with
  | Some context ->
      let m = getCurrentModule env in
      m.init <- (context.path, name) :: m.init ;
      env
  | None ->
      env

let enterModule (env : env) (name : string) : env =
  match Map.find name env.modules with
  | Some m ->
      {env with location= InModule m}
  | None ->
      let m : m =
        { name
        ; functions= Map.empty ()
        ; generics= Map.empty ()
        ; types= Map.empty ()
        ; enums= Map.empty ()
        ; init= []
        ; constants= Map.empty () }
      in
      let () = Map.add name m env.modules in
      {env with location= InModule m}

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
    match lookVarInContext (contextOf f) id with Some var -> Some var | None -> lookVarInScopes f.scopes id
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
  | InModule m ->
      let report found_loc =
        Error.raiseError
          ( "A function with the name '" ^ generic.name ^ "' has already been declared at "
          ^ Loc.to_string_readable found_loc )
          generic.loc
      in
      let () = match Map.find generic.name m.functions with Some (found : f) -> report found.path.loc | None -> () in
      let () =
        Map.update (fun (found : Typed.generic_function) _ -> report found.loc) generic.name generic m.generics
      in
      env
  | InContext (m, _) ->
      failwith ("Internal error in addGeneric: cannot add generic '" ^ generic.name ^ "' from context '" ^ m.name ^ "'")
  | InFunction (m, _) ->
      failwith
        ( "Internal error in addGeneric: cannot add generic '" ^ generic.name ^ "' from function in module '" ^ m.name
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
  let module_opt =
    match (path.n, env.location) with
    | Some n, _ ->
        Map.find n env.modules
    | None, (InModule m | InContext (m, _) | InFunction (m, _)) ->
        Some m
    | None, Top ->
        None
  in
  match module_opt with
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
