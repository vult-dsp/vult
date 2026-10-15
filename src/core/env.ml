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

(* The environment of the typechecker: the modules with their functions, types and constants, the
   location being typechecked and the substitution of the type inference. The environment is a
   value: every function that declares something returns a new environment. *)

open Util

let pathString p = Pla.print (Pparser.Syntax.print_path p)

(* Immutable maps indexed by name *)
module Map = struct
  type 'a t = 'a Maps.Map.t

  let empty : 'a t = Maps.Map.empty

  (* Adds the value. When the key exists, [report] receives the existing value and the new one
     and returns the value to keep. *)
  let update (report : 'a -> 'a -> 'a) (key : string) (value : 'a) (t : 'a t) : 'a t =
    Maps.Map.update key (function None -> Some value | Some found -> Some (report found value)) t

  let of_list elems : 'a t = CCList.fold_left (fun m (key, value) -> Maps.Map.add key value m) Maps.Map.empty elems

  let to_list (t : 'a t) = Maps.Map.to_list t

  let find key (t : 'a t) = Maps.Map.find_opt key t

  let add (key : string) (value : 'a) (t : 'a t) : 'a t = Maps.Map.add key value t

  let is_empty (t : 'a t) : bool = Maps.Map.is_empty t

  let fold (f : string -> 'a -> 'b -> 'b) (s : 'b) (t : 'a t) : 'b = Maps.Map.fold f t s

  let map (f : 'a -> 'b) (t : 'a t) : 'b t = Maps.Map.map f t
end

type var_kind = Mem of Pparser.Ptags.tags | Inst | Val | Const

(* The kinds of variables declared inside a function. Constants are added with addConstant. *)
type local_kind = LocalVal | LocalMem of Pparser.Ptags.tags | LocalInst

type path = Typed.path

type var = {name: string; t: Typed.type_; kind: var_kind; tags: Pparser.Ptags.tags; loc: Loc.t}

type type_descr = Simple | Record of var Map.t | Enum of (string * int * Loc.t) Map.t | Alias of path * path

type t = {path: path; descr: type_descr; index: int; loc: Loc.t; generated: bool}

(* The record that holds the memory of a function and of its companions. The members are the
   ones of the record type with this path, stored in the module. *)
type context = {path: path}

(* The context of a function together with the type of its '_ctx' argument. The companions
   share the context but each function has its own argument type. *)
type function_context = NoContext | Context of {context: context; arg_t: Typed.type_}

(* The context argument of a function: the path of its context and the type of '_ctx' *)
type context_argument = {context_path: path; arg_t: Typed.type_}

(* How a local variable was declared. A loop counter keeps the name written in the 'iter'. *)
type declaration = Argument | Variable | LoopCounter of string

(* A local variable of a function and the number of times its value has been read *)
type local = {var: var; declaration: declaration; reads: int}

(* The local variables of a function: the innermost scope and the scopes that enclose it *)
type scopes = {current: local Map.t; enclosing: local Map.t list}

type f =
  { path: path
  ; t: Typed.type_ list * Typed.type_
  ; args: Typed.arg list option
  ; context: function_context
  ; scopes: scopes
  ; tick: int (* Counter used to name the instances created in the function *) }

type m =
  { name: string
  ; functions: f Map.t
  ; generics: Typed.generic_function Map.t
  ; types: t Map.t
  ; init: (path * path) list
  ; enums: t list Map.t (* The enumerations that have each value name *)
  ; constants: var Map.t }

(* A value of an enumeration: the enumeration, where the value is declared and its index *)
type enum_value = {enum_path: path; value_loc: Loc.t; index: int}

(* The values with the same name in the enumerations of a module. There is always at least one. *)
type enum_values = enum_value * enum_value list

(* Generic lookup result variant *)
type lookup_result =
  | LookupVar of var
  | LookupFunction of f
  | LookupGeneric of Typed.generic_function
  | LookupType of t
  | LookupEnum of enum_values
  | LookupConstant of var
  | LookupBuiltinFunction of f
  | LookupNotFound

(* The context of the current location with a copy of its members *)
type current_context = {context: context; members: var Map.t}

(* Location within the environment hierarchy, given by the name of the module. The current function
   and the members of the current context are copies of the ones stored in the module, kept to access
   them quickly: the copies and the originals are always updated together, except for the scopes of
   the function. The module keeps the scope of the arguments and only the location has the scopes of
   the blocks being typechecked. *)
type location =
  | Top
  | InModule of string
  | InContext of string * current_context option
  | InFunction of string * f * current_context option

(* Unified environment type that tracks current location internally. It also carries the
   substitution of the type inference. *)
type env =
  { modules: m Map.t
  ; builtin_functions: (unit -> Typed.fun_type) Map.t
  ; builtin_types: t Map.t
  ; location: location
  ; type_index: int (* Gives each declared type its position in the generated code *)
  ; subst: Subst.t
  ; loop_counters: string Map.t (* The renamed counters of the 'iter' loops and the names written by the user *)
  ; warnings: Error.warning list }

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

let isBuiltinType (env : env) (t : Typed.type_) : bool =
  match (Subst.walk env.subst t).tx with TEId {id; n= None; _} -> Map.find id builtin_types <> None | _ -> false

let makeFunctionForBuiltin name t : f =
  { path= {id= name; n= None; loc= Loc.default}
  ; t
  ; context= NoContext
  ; scopes= {current= Map.empty; enclosing= []}
  ; tick= 0
  ; args= None }

(* The index of the next declared type *)
let nextTypeIndex (env : env) : env * int = ({env with type_index= env.type_index + 1}, env.type_index)

(* ========== Current location ========== *)

let currentModuleName (env : env) : string =
  match env.location with
  | InModule name | InContext (name, _) | InFunction (name, _, _) ->
      name
  | Top ->
      failwith "Internal error in getCurrentModule: called at Top level (not inside any module)"

let getCurrentModule (env : env) : m =
  let name = currentModuleName env in
  match Maps.Map.find name env.modules with
  | m ->
      m
  | exception Not_found ->
      failwith ("Internal error in getCurrentModule: module '" ^ name ^ "' not found")

let setModule (env : env) (m : m) : env = {env with modules= Map.add m.name m env.modules}

let updateCurrentModule (env : env) (update : m -> m) : env = setModule env (update (getCurrentModule env))

let contextOf (f : f) : context option = match f.context with Context {context; _} -> Some context | NoContext -> None

let getCurrentFunction (env : env) : f =
  match env.location with
  | InFunction (_, f, _) ->
      f
  | InModule m ->
      failwith ("Internal error in getCurrentFunction: currently in module '" ^ m ^ "', not a function")
  | InContext (m, _) ->
      failwith ("Internal error in getCurrentFunction: currently in context '" ^ m ^ "', not a function")
  | Top ->
      failwith "Internal error in getCurrentFunction: currently at Top level, not in a function"

(* Changes the scopes of the current function, which are only stored in the location *)
let setCurrentScopes (env : env) (scopes : scopes) : env =
  match env.location with
  | InFunction (module_name, f, current) ->
      {env with location= InFunction (module_name, {f with scopes}, current)}
  | Top | InModule _ | InContext _ ->
      failwith "Internal error in setCurrentScopes: not in a function"

(* Changes the tick of the current function in the location and in the module *)
let setCurrentTick (env : env) (tick : int) : env =
  let f = getCurrentFunction env in
  let env =
    updateCurrentModule env (fun m ->
        match Map.find f.path.id m.functions with
        | Some stored ->
            {m with functions= Map.add f.path.id {stored with tick} m.functions}
        | None ->
            failwith "Internal error in setCurrentTick: the function is not in the module" )
  in
  match env.location with
  | InFunction (module_name, f, current) ->
      {env with location= InFunction (module_name, {f with tick}, current)}
  | Top | InModule _ | InContext _ ->
      failwith "Internal error in setCurrentTick: not in a function"

let currentContext (env : env) : current_context option =
  match env.location with InContext (_, current) | InFunction (_, _, current) -> current | InModule _ | Top -> None

let getCurrentContext (env : env) : context option = Option.map (fun current -> current.context) (currentContext env)

let sameContext (c1 : context) (c2 : context) : bool =
  String.equal c1.path.id c2.path.id && Option.equal String.equal c1.path.n c2.path.n

(* The members stored in the record type of the context *)
let storedContextMembers (env : env) (context : context) : var Map.t =
  let module_members =
    match
      Option.bind
        (Map.find (Option.value context.path.n ~default:"") env.modules)
        (fun m -> Map.find context.path.id m.types)
    with
    | Some {descr= Record members; _} ->
        Some members
    | Some {descr= Simple | Enum _ | Alias _; _} | None ->
        None
  in
  match module_members with
  | Some members ->
      members
  | None ->
      failwith ("Internal error in contextMembers: the context '" ^ pathString context.path ^ "' has no record")

let contextMembers (env : env) (context : context) : var Map.t =
  match currentContext env with
  | Some current when sameContext current.context context ->
      current.members
  | Some _ | None ->
      storedContextMembers env context

(* The current context of the given function, reading its members *)
let currentContextOf (env : env) (context : context option) : current_context option =
  Option.map (fun context -> {context; members= storedContextMembers env context}) context

(* Updates the copy of the members in the location when the context is the current one *)
let setCurrentContextMembers (env : env) (context : context) (members : var Map.t) : env =
  let update current =
    match current with
    | Some current when sameContext current.context context ->
        Some {current with members}
    | _ ->
        current
  in
  match env.location with
  | InContext (module_name, current) ->
      {env with location= InContext (module_name, update current)}
  | InFunction (module_name, f, current) ->
      {env with location= InFunction (module_name, f, update current)}
  | InModule _ | Top ->
      env

let setContextMembers (env : env) (context : context) (members : var Map.t) : env =
  let env = setCurrentContextMembers env context members in
  let module_name = Option.value context.path.n ~default:"" in
  match Map.find module_name env.modules with
  | Some m -> (
    match Map.find context.path.id m.types with
    | Some t ->
        setModule env {m with types= Map.add context.path.id {t with descr= Record members} m.types}
    | None ->
        failwith ("Internal error in setContextMembers: the context '" ^ pathString context.path ^ "' has no record") )
  | None ->
      failwith ("Internal error in setContextMembers: module '" ^ module_name ^ "' not found")

let currentContextMembers (env : env) : var Map.t option = Option.map (contextMembers env) (getCurrentContext env)

(* Private helper functions for internal use only *)
(* The scopes from the innermost to the outermost *)
let allScopes (scopes : scopes) : local Map.t list = scopes.current :: scopes.enclosing

let lookVarInScopes (scopes : scopes) name : var option =
  CCList.find_map (fun scope -> Option.map (fun (local : local) -> local.var) (Map.find name scope)) (allScopes scopes)

let lookVarInContext (env : env) (context : context option) name : var option =
  match context with Some context -> Map.find name (contextMembers env context) | None -> None

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

let registerArguments (args : Typed.arg list) : local Map.t * Typed.type_ list =
  let report loc ({var= found; _} : local) _ =
    Error.raiseError
      ("A variable with the name '" ^ found.name ^ "' has already been declared at " ^ Loc.to_string_readable found.loc)
      loc
  in
  let locals =
    CCList.fold_left
      (fun locals ({name; t; loc} : Typed.arg) ->
        let () = checkBindingName "argument" name loc in
        let var = {name; t; kind= Val; tags= []; loc} in
        Map.update (report loc) name {var; declaration= Argument; reads= 0} locals )
      Map.empty args
  in
  (locals, CCList.map (fun (arg : Typed.arg) -> arg.t) args)

let lookVar (env : env) (name : string) (loc : Loc.t) : var =
  let f = getCurrentFunction env in
  let m = getCurrentModule env in
  match lookVarInContext env (contextOf f) name with
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

let getType (env : env) (path : path) : t option =
  match path with
  | {id; n= Some n; loc} -> (
    match Map.find n env.modules with None -> reportModuleNotFound n loc | Some m -> Map.find id m.types )
  | _ ->
      None

let addConstant (env : env) (name : string) (t : Typed.type_) loc : env =
  let () = checkBindingName "constant" name loc in
  let report (found : var) _ =
    Error.raiseError
      ("A constant with the name '" ^ found.name ^ "' has already been declared at " ^ Loc.to_string_readable found.loc)
      loc
  in
  updateCurrentModule env (fun m ->
      {m with constants= Map.update report name {name; t; kind= Const; tags= []; loc} m.constants} )

(* Helper: Check if a mem variable with the same name already exists in the context *)
let checkDuplicatedMem (env : env) (context : context option) (name : string) (loc : Loc.t) : unit =
  match lookVarInContext env context name with
  | None ->
      ()
  | Some found ->
      Error.raiseError
        ( "A mem variable with the name '" ^ found.name ^ "' has already been declared at "
        ^ Loc.to_string_readable found.loc )
        loc

(* Helper: Check if a val variable with the same name already exists in any scope *)
let checkDuplicatedVal (scopes : scopes) (name : string) (loc : Loc.t) : unit =
  CCList.iter
    (fun (scope : local Map.t) ->
      match Map.find name scope with
      | None ->
          ()
      | Some {var= found; _} ->
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
    CCList.iter
      (fun ({name; loc; _} : Typed.arg) ->
        match lookVarInContext env context name with
        | None ->
            ()
        | Some found ->
            Error.raiseError
              ( "Function parameter '" ^ name ^ "' shadows a mem variable declared at "
              ^ Loc.to_string_readable found.loc ^ ". Rename the parameter or the mem variable to avoid this conflict."
              )
              loc )
      args
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

(* Declaring again a mem variable is allowed when the type is the same: the tags are merged *)
let redeclareMem (env : env) (found : var) (value : var) : env * var =
  match Subst.unify env.subst found.t value.t with
  | Some subst ->
      let tags = Pparser.Ptags.mergeTags found.tags value.tags in
      ({env with subst}, {found with tags})
  | None ->
      let old_type = Pla.print (Subst.printType env.subst found.t) in
      let new_type = Pla.print (Subst.printType env.subst value.t) in
      Error.raiseError
        ( "This declaration tries to change the type of " ^ found.name ^ ". The previous type is '" ^ old_type
        ^ "' and the new is '" ^ new_type ^ "'" )
        value.loc

(* Helper: Add a mem or inst variable to the context record *)
let addMemOrInst (f : f) (context : context) (name : string) (t : Typed.type_) (kind : var_kind)
    (tags : Pparser.Ptags.tag list) (loc : Loc.t) (env : env) : env =
  let () =
    match kind with Mem _ -> checkBindingName "variable" name loc | Inst | Val | Const -> checkReservedName name loc
  in
  let () = checkDuplicatedVal f.scopes name loc in
  let members = contextMembers env context in
  let value = {name; t; kind; tags; loc} in
  match Map.find name members with
  | None ->
      setContextMembers env context (Map.add name value members)
  | Some found ->
      let env, var = redeclareMem env found value in
      setContextMembers env context (Map.add name var members)

(* Helper: Add a val variable to the current local scope *)
let addValVar (f : f) (context : context option) (name : string) (t : Typed.type_) (loc : Loc.t) (env : env) : env =
  let report ({var= found; _} : local) _ =
    Error.raiseError
      ("A variable with the name '" ^ found.name ^ "' has already been declared at " ^ Loc.to_string_readable found.loc)
      loc
  in
  let () = checkBindingName "variable" name loc in
  let () = checkDuplicatedMem env context name loc in
  let () = checkDuplicatedConstant env name loc in
  let declaration =
    match Map.find name env.loop_counters with Some written -> LoopCounter written | None -> Variable
  in
  let local = {var= {name; t; kind= Val; tags= []; loc}; declaration; reads= 0} in
  let current = Map.update report name local f.scopes.current in
  setCurrentScopes env {f.scopes with current}

let addVar (env : env) (name : string) (t : Typed.type_) (kind : local_kind) loc : env =
  let f = getCurrentFunction env in
  let context = getCurrentContext env in
  match (kind, context) with
  | LocalInst, Some context ->
      addMemOrInst f context name t Inst [] loc env
  | LocalMem tags, Some context ->
      addMemOrInst f context name t (Mem tags) tags loc env
  | (LocalMem _ | LocalInst), None ->
      Error.raiseError ("The variable '" ^ name ^ "' cannot be stored in memory. Only functions have memory.") loc
  | LocalVal, context ->
      addValVar f context name t loc env

let checkMemExists (env : env) name =
  match lookVarInContext env (contextOf (getCurrentFunction env)) name with None -> false | Some _ -> true

let checkConstantExists (env : env) (name : string) : bool =
  let m = getCurrentModule env in
  match Map.find name m.constants with None -> false | Some _ -> true

let addReturnVar (env : env) (name : string) (t : Typed.type_) loc : env =
  let env = {env with subst= Subst.setMut env.subst t} in
  match getCurrentContext env with
  | Some context -> (
      let members = contextMembers env context in
      match Map.find name members with
      | Some found ->
          Error.raiseError
            ( "Return variable '" ^ name ^ "' conflicts with a mem variable declared at "
            ^ Loc.to_string_readable found.loc ^ ". Consider using different names for your mem variables." )
            loc
      | None ->
          setContextMembers env context (Map.add name {name; t; kind= Mem []; tags= []; loc} members) )
  | None ->
      Error.raiseError "Only functions can return multiple values." loc

(* Registers the renamed counter of an 'iter' loop, declared later as a regular variable *)
let addLoopCounter (env : env) ~(counter : string) ~(written : string) : env =
  {env with loop_counters= Map.add counter written env.loop_counters}

(* Changes the innermost local variable with the given name *)
let updateLocal (env : env) (name : string) (update : local -> local) : env =
  let f = getCurrentFunction env in
  let rec updateIn scopes =
    match scopes with
    | [] ->
        None
    | scope :: rest -> (
      match Map.find name scope with
      | Some local ->
          Some (Map.add name (update local) scope :: rest)
      | None ->
          Option.map (fun rest -> scope :: rest) (updateIn rest) )
  in
  match updateIn (allScopes f.scopes) with
  | Some (current :: enclosing) ->
      setCurrentScopes env {current; enclosing}
  | Some [] | None ->
      env

(* Counts a read of a local variable. Names that are not local variables are ignored. *)
let countRead (env : env) (name : string) : env =
  updateLocal env name (fun local -> {local with reads= local.reads + 1})

(* Writing into an element or member of an argument changes the value of the caller, so it is
   counted as a read of the argument *)
let countPartialWrite (env : env) (name : string) : env =
  updateLocal env name (fun local ->
      match local.declaration with Argument -> {local with reads= local.reads + 1} | Variable | LoopCounter _ -> local )

(* The condition and the increment generated for an 'iter' loop read its counter twice *)
let loop_counter_generated_reads = 2

let isUnused (local : local) : bool =
  match local.declaration with
  | Argument | Variable ->
      local.reads = 0
  | LoopCounter _ ->
      local.reads <= loop_counter_generated_reads

let unusedWarning (local : local) : Error.warning option =
  let report what name =
    if isUnused local && not (String.starts_with ~prefix:"_" name) then
      let msg = Printf.sprintf "The %s '%s' is never used. If this is intended, rename it to '_%s'." what name name in
      Some {Error.loc= local.var.loc; msg}
    else None
  in
  match local.declaration with
  | Argument ->
      report "argument" local.var.name
  | Variable ->
      report "variable" local.var.name
  | LoopCounter written ->
      report "variable" written

let addUnusedWarnings (env : env) (scope : local Map.t) : env =
  let warnings = Map.fold (fun _ local acc -> Option.to_list (unusedWarning local) @ acc) [] scope in
  {env with warnings= warnings @ env.warnings}

(* Runs [body] with a new innermost scope for the local variables of the current function. The
   scope is removed afterwards, which keeps the reads counted in the enclosing scopes. *)
let withScope (env : env) (body : env -> env * 'a) : env * 'a =
  let f = getCurrentFunction env in
  let outer = f.scopes in
  let env = setCurrentScopes env {current= Map.empty; enclosing= outer.current :: outer.enclosing} in
  let env, result = body env in
  let inner = (getCurrentFunction env).scopes in
  match inner.enclosing with
  | current :: enclosing ->
      let env = addUnusedWarnings env inner.current in
      (setCurrentScopes env {current; enclosing}, result)
  | [] ->
      failwith "Internal error in withScope: the scope was removed by its body"

(* Reports the arguments of the current function that have not been read *)
let reportUnusedArguments (env : env) : env =
  let f = getCurrentFunction env in
  CCList.fold_left addUnusedWarnings env (allScopes f.scopes)

(* The warnings found so far, in source order. The body of a generic function is checked once for
   each specialization, so the repeated warnings are removed. *)
let getWarnings (env : env) : Error.warning list =
  let same (w1 : Error.warning) (w2 : Error.warning) =
    String.equal (Loc.file w1.loc) (Loc.file w2.loc) && Loc.isSameLoc w1.loc w2.loc && String.equal w1.msg w2.msg
  in
  let compare (w1 : Error.warning) (w2 : Error.warning) =
    match String.compare (Loc.file w1.loc) (Loc.file w2.loc) with 0 -> Loc.compareLoc w1.loc w2.loc | c -> c
  in
  env.warnings |> CCList.uniq ~eq:same |> CCList.stable_sort compare

let getPath (m : m) name loc : path = {id= name; n= Some m.name; loc}

(* Adds the record type that holds the memory of the function and enters its context *)
let createContextWithIndex (env : env) name loc (index : int) : env =
  let m = getCurrentModule env in
  let report (found : t) _ =
    Error.raiseError
      ("A function with the name '" ^ name ^ "' already exists at " ^ Loc.to_string_readable found.loc)
      loc
  in
  let type_name = name ^ "_type" in
  let path = getPath m type_name loc in
  let t = {descr= Record Map.empty; path; index; loc; generated= true} in
  let env = setModule env {m with types= Map.update report type_name t m.types} in
  {env with location= InContext (m.name, Some {context= {path}; members= Map.empty})}

let createContextForFunction (env : env) name loc : env =
  let env, index = nextTypeIndex env in
  createContextWithIndex env name loc index

let createContextForFunctionWithIndex (env : env) name loc (type_index : int) : env =
  createContextWithIndex env name loc type_index

let addAliasToContext (env : env) name loc : env =
  match getCurrentContext env with
  | Some context when not (Map.is_empty (contextMembers env context)) -> (
      let m = getCurrentModule env in
      let type_name = name ^ "_type" in
      (* Check if alias already exists - skip if it does (can happen with multiple instantiations of generics) *)
      match Map.find type_name m.types with
      | Some _ ->
          env (* Alias already exists, skip *)
      | None ->
          let path = getPath m type_name loc in
          let env, index = nextTypeIndex env in
          let t = {descr= Alias (path, context.path); path; index; loc; generated= true} in
          updateCurrentModule env (fun m -> {m with types= Map.add type_name t m.types}) )
  | _ ->
      env

let addRecordMember members =
  let report loc (found : var) _ =
    Error.raiseError
      ("A member with the name '" ^ found.name ^ "' has already been declared at " ^ Loc.to_string_readable found.loc)
      loc
  in
  let members =
    CCList.fold_left
      (fun m (name, t, tags, loc) -> Map.update (report loc) name {name; t; kind= Val; tags; loc} m)
      Map.empty members
  in
  Record members

let addType (env : env) type_name members loc : env =
  let m = getCurrentModule env in
  let report (found : t) _ =
    Error.raiseError
      ("A type with the name '" ^ found.path.id ^ "' has already been declared at " ^ Loc.to_string_readable found.loc)
      loc
  in
  let env, index = nextTypeIndex env in
  let path = getPath m type_name loc in
  let descr = addRecordMember members in
  let t = {path; descr; loc; index; generated= false} in
  setModule env {m with types= Map.update report type_name t m.types}

let addEnumMember members =
  let report loc (name, _, floc) _ =
    Error.raiseError
      ("A member with the name '" ^ name ^ "' has already been declared at " ^ Loc.to_string_readable floc)
      loc
  in
  let members, _ =
    CCList.fold_left
      (fun (m, i) (name, loc) -> (Map.update (report loc) name (name, i, loc) m, i + 1))
      (Map.empty, 0) members
  in
  Enum members

(* Several enumerations can have values with the same name: which one is used is inferred *)
let addEnumToModule (m : m) members t : m =
  let add enums (name, _) =
    let previous = Option.value ~default:[] (Map.find name enums) in
    Map.add name (previous @ [t]) enums
  in
  {m with enums= CCList.fold_left add m.enums members}

(* The values named [id] in the enumerations of the module *)
let enumValuesIn (m : m) (id : string) : enum_values option =
  let value (t : t) =
    match t.descr with
    | Enum members ->
        Option.map (fun (_, index, value_loc) -> {enum_path= t.path; value_loc; index}) (Map.find id members)
    | Simple | Record _ | Alias _ ->
        None
  in
  match CCList.filter_map value (Option.value ~default:[] (Map.find id m.enums)) with
  | first :: others ->
      Some (first, others)
  | [] ->
      None

let addEnum (env : env) type_name members loc : env =
  let m = getCurrentModule env in
  let report (found : t) _ =
    Error.raiseError
      ("A enum with the name '" ^ found.path.id ^ "' has already been declared at " ^ Loc.to_string_readable found.loc)
      loc
  in
  let env, index = nextTypeIndex env in
  let path = getPath m type_name loc in
  let descr = addEnumMember members in
  let t = {path; descr; loc; index; generated= false} in
  let m = {m with types= Map.update report type_name t m.types} in
  setModule env (addEnumToModule m members t)

let createContextForExternal (env : env) : env = {env with location= InContext (currentModuleName env, None)}

let exitContext (env : env) : env = {env with location= InModule (currentModuleName env)}

(* Returns the current tick of the function without advancing it *)
let currentFunctionTick (env : env) : int = (getCurrentFunction env).tick

(* Advances the tick of the current function and returns the new value *)
let nextFunctionTick (env : env) : env * int =
  let f = getCurrentFunction env in
  let n = f.tick + 1 in
  (setCurrentTick env n, n)

let getContext (env : env) : path =
  match getCurrentContext env with
  | Some context ->
      context.path
  | None ->
      failwith "Internal error in getContext: trying to get context but current function has no context"

let contextArgument (f : f) : context_argument option =
  match f.context with Context {context; arg_t} -> Some {context_path= context.path; arg_t} | NoContext -> None

(* The context argument of a function that has memory, which the callers have to pass *)
let activeContextArgument (env : env) (f : f) : context_argument option =
  match f.context with
  | Context {context; arg_t} when not (Map.is_empty (contextMembers env context)) ->
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
  let report (found : f) _ =
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
  let env = setModule env {m with functions= Map.update report name f m.functions} in
  ({env with location= InFunction (m.name, f, currentContext env)}, path, t)

let isFunctionActive (env : env) (f : f) = Option.is_some (activeContextArgument env f)

let exitFunction (env : env) : env = {env with location= InContext (currentModuleName env, currentContext env)}

(** Re-enter a function context for post-processing. Looks up the function by path
    and sets the environment to be inside that function. *)
let reenterFunction (env : env) (func_path : path) : env =
  let module_name = match func_path.n with Some n -> n | None -> currentModuleName env in
  match Map.find module_name env.modules with
  | None ->
      failwith ("reenterFunction: module '" ^ module_name ^ "' not found")
  | Some m -> (
    match Map.find func_path.id m.functions with
    | Some f ->
        {env with location= InFunction (module_name, f, currentContextOf env (contextOf f))}
    | None ->
        failwith ("reenterFunction: function '" ^ pathString func_path ^ "' not found") )

let addCustomInitFunction (env : env) name =
  match getCurrentContext env with
  | Some context ->
      updateCurrentModule env (fun m -> {m with init= (context.path, name) :: m.init})
  | None ->
      env

let enterModule (env : env) (name : string) : env =
  match Map.find name env.modules with
  | Some _ ->
      {env with location= InModule name}
  | None ->
      let m : m =
        { name
        ; functions= Map.empty
        ; generics= Map.empty
        ; types= Map.empty
        ; enums= Map.empty
        ; init= []
        ; constants= Map.empty }
      in
      {(setModule env m) with location= InModule name}

let exitModule (env : env) : env = {env with location= Top}

(* Goes back to the location of [outer] keeping the declarations and the inference state of
   [inner]. Used after processing code in another function or module. *)
let restoreLocation (outer : env) (inner : env) : env =
  match outer.location with
  | InFunction (_, f, _) ->
      setCurrentScopes (reenterFunction inner f.path) f.scopes
  | InContext (module_name, current) ->
      let current = currentContextOf inner (Option.map (fun current -> current.context) current) in
      {inner with location= InContext (module_name, current)}
  | Top | InModule _ ->
      {inner with location= outer.location}

(* Generic lookup function for paths - returns list of all possible meanings *)
(* Helper: Prepend to results if option is Some *)
let consOpt (make : 'a -> lookup_result) (opt : 'a option) (results : lookup_result list) : lookup_result list =
  match opt with Some x -> make x :: results | None -> results

let lookupPath (env : env) (path : path) : lookup_result list =
  let lookupInModule (m : m) (id : string) : lookup_result list =
    (* Find enum member if applicable *)
    let enumResult = Option.map (fun values -> LookupEnum values) (enumValuesIn m id) in
    []
    |> consOpt (fun f -> LookupFunction f) (Map.find id m.functions)
    |> consOpt (fun t -> LookupType t) (Map.find id m.types)
    |> consOpt (fun var -> LookupConstant var) (Map.find id m.constants)
    |> consOpt Fun.id enumResult
    |> consOpt (fun g -> LookupGeneric g) (Map.find id m.generics)
  in
  let lookupLocalVar (f : f) (id : string) : var option =
    match lookVarInContext env (contextOf f) id with Some var -> Some var | None -> lookVarInScopes f.scopes id
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
        | InFunction _ ->
            consOpt (fun var -> LookupVar var) (lookupLocalVar (getCurrentFunction env) id) results
        | _ ->
            results
      in
      (* Add module-level symbols *)
      let results =
        match env.location with
        | InFunction _ | InModule _ | InContext _ ->
            lookupInModule (getCurrentModule env) id @ results
        | Top ->
            results
      in
      (* Add builtin functions and types *)
      results
      |> consOpt (fun f -> LookupBuiltinFunction (makeFunctionForBuiltin id (f ()))) (Map.find id env.builtin_functions)
      |> consOpt (fun t -> LookupType t) (Map.find id env.builtin_types)

(* Helper functions to find specific lookup result types from a list *)
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

let findEnum (results : lookup_result list) : enum_values option =
  let rec find = function [] -> None | LookupEnum values :: _ -> Some values | _ :: rest -> find rest in
  find results

(* The following functions find one kind of meaning of a path, with the same priorities as the
   list returned by [lookupPath], without looking for the other kinds *)

(* The module where the path is searched *)
let moduleOfPath (env : env) (path : path) : m option =
  match (path.n, env.location) with
  | Some n, _ ->
      Map.find n env.modules
  | None, Top ->
      None
  | None, (InModule _ | InContext _ | InFunction _) ->
      Some (getCurrentModule env)

(* The constants of the module have priority over the local variables *)
let findVarOf (env : env) (path : path) : var option =
  let constant = Option.bind (moduleOfPath env path) (fun m -> Map.find path.id m.constants) in
  match (constant, path.n, env.location) with
  | Some _, _, _ ->
      constant
  | None, None, InFunction (_, f, _) -> (
    match lookVarInContext env (contextOf f) path.id with
    | Some var ->
        Some var
    | None ->
        lookVarInScopes f.scopes path.id )
  | None, _, _ ->
      None

(* The functions of the module have priority over the builtin functions *)
let findFunctionOf (env : env) (path : path) : f option =
  match Option.bind (moduleOfPath env path) (fun m -> Map.find path.id m.functions) with
  | Some f ->
      Some f
  | None -> (
    match path.n with
    | Some _ ->
        None
    | None ->
        Option.map (fun t -> makeFunctionForBuiltin path.id (t ())) (Map.find path.id env.builtin_functions) )

(* The builtin types have priority over the types of the module *)
let findTypeOf (env : env) (path : path) : t option =
  let builtin = match path.n with Some _ -> None | None -> Map.find path.id env.builtin_types in
  match builtin with
  | Some _ ->
      builtin
  | None ->
      Option.bind (moduleOfPath env path) (fun m -> Map.find path.id m.types)

let findEnumOf (env : env) (path : path) : enum_values option =
  Option.bind (moduleOfPath env path) (fun m -> enumValuesIn m path.id)

let findGenericOf (env : env) (path : path) : Typed.generic_function option =
  Option.bind (moduleOfPath env path) (fun m -> Map.find path.id m.generics)

(* Function lookup using the new generic lookup system *)
let lookFunctionCall (env : env) (path : path) (loc : Loc.t) : f =
  match findFunctionOf env path with
  | Some f ->
      f
  | None ->
      Error.raiseError ("A function with the name '" ^ pathString path ^ "' could not be found") loc

(** Try to find a function without raising an error *)
let tryLookFunctionCall (env : env) (path : path) : f option = findFunctionOf env path

(* Operator lookup using the new generic lookup system *)
let lookOperator (env : env) (op : string) : f =
  let op_path : path = {id= op; n= None; loc= Loc.default} in
  match findFunctionOf env op_path with
  | Some f ->
      f
  | None ->
      failwith ("Internal error in lookOperator: builtin operator '" ^ op ^ "' not found")

(* Since operators are only builtins, this behaves the same as lookOperator *)
let lookOperatorInModule (env : env) (op : string) : f = lookOperator env op

(* Generic management functions *)
let addGeneric (env : env) (generic : Typed.generic_function) : env =
  match env.location with
  | InModule _ ->
      let m = getCurrentModule env in
      let report found_loc =
        Error.raiseError
          ( "A function with the name '" ^ generic.name ^ "' has already been declared at "
          ^ Loc.to_string_readable found_loc )
          generic.loc
      in
      let () = match Map.find generic.name m.functions with Some (found : f) -> report found.path.loc | None -> () in
      let generics =
        Map.update (fun (found : Typed.generic_function) _ -> report found.loc) generic.name generic m.generics
      in
      setModule env {m with generics}
  | InContext (m, _) ->
      failwith ("Internal error in addGeneric: cannot add generic '" ^ generic.name ^ "' from context '" ^ m ^ "'")
  | InFunction (m, _, _) ->
      failwith
        ("Internal error in addGeneric: cannot add generic '" ^ generic.name ^ "' from function in module '" ^ m ^ "'")
  | Top ->
      failwith ("Internal error in addGeneric: cannot add generic '" ^ generic.name ^ "' at Top level")

(* Generic lookup using the new generic lookup system *)
let lookupGeneric (env : env) (path : path) : Typed.generic_function option = findGenericOf env path

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
    | None, (InModule _ | InContext _ | InFunction _) ->
        Some (getCurrentModule env)
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
  | ExprEnum of enum_values
  | ExprNotFound

let lookupExpressionSymbol (env : env) (path : path) (context : expr_context) : expression_symbol =
  let orElse (next : unit -> expression_symbol) (found : expression_symbol) =
    match found with ExprNotFound -> next () | ExprVariable _ | ExprFunction _ | ExprType _ | ExprEnum _ -> found
  in
  let variable () = match findVarOf env path with Some var -> ExprVariable var | None -> ExprNotFound in
  let constant () =
    match findVarOf env path with Some var when var.kind = Const -> ExprVariable var | Some _ | None -> ExprNotFound
  in
  let function_ () = match findFunctionOf env path with Some f -> ExprFunction f | None -> ExprNotFound in
  let enum () = match findEnumOf env path with Some enum_data -> ExprEnum enum_data | None -> ExprNotFound in
  let type_ () = match findTypeOf env path with Some t -> ExprType t | None -> ExprNotFound in
  if context.in_constant then
    (* In constant context: constants first, then enums, then types *)
    constant () |> orElse enum |> orElse type_
  else
    (* Otherwise: variables first, then functions (references in generic arguments, calls in regular
       contexts), then enums *)
    variable () |> orElse function_ |> orElse enum

(* The record types declared in any module whose members are accepted by [accepts], to guess the type
   of a value whose members are used before its type is known *)
let recordCandidates (env : env) (accepts : var Map.t -> bool) : Subst.candidate list =
  let candidate (t : t) : Subst.candidate option =
    match t.descr with
    | Record members when (not t.generated) && accepts members ->
        let members =
          Map.fold
            (fun name (var : var) acc ->
              ({member= name; member_t= var.t; value_type= isBuiltinType env var.t} : Subst.record_member) :: acc )
            [] members
        in
        Some {candidate_path= t.path; members}
    | Record _ | Simple | Enum _ | Alias _ ->
        None
  in
  Map.fold
    (fun _ (m : m) acc -> Map.fold (fun _ t acc -> Option.to_list (candidate t) @ acc) acc m.types)
    [] env.modules
  |> CCList.rev

let isEnum (env : env) (path : path) : bool =
  match findTypeOf env path with
  | Some {descr= Enum _; _} ->
      true
  | Some {descr= Simple | Record _ | Alias _; _} | None ->
      false

(* Unified type lookup function using the new lookup system *)
let lookType (env : env) (path : path) (loc : Loc.t) : t =
  match findTypeOf env path with
  | Some t ->
      t
  | None ->
      Error.raiseError ("A type with the name '" ^ pathString path ^ "' could not be found") loc

let builtin_functions_with_extensions (extensions : extension list) : (unit -> Typed.fun_type) Map.t =
  CCList.fold_left
    (fun m ext -> CCList.fold_left (fun m (name, t) -> Map.add name t m) m (builtins_for_extension ext))
    builtin_functions extensions

let empty ?(extensions : extension list = []) () : env =
  { modules= Map.empty
  ; builtin_functions= builtin_functions_with_extensions extensions
  ; builtin_types
  ; location= Top
  ; type_index= 0
  ; subst= Subst.empty ()
  ; loop_counters= Map.empty
  ; warnings= [] }

(* Replaces the variables in the types stored in the environment by their bindings *)
let zonk (env : env) : env =
  let zonk = Subst.zonk env.subst in
  let zonkVar (var : var) = {var with t= zonk var.t} in
  let zonkContext (context : function_context) =
    match context with NoContext -> NoContext | Context {context; arg_t} -> Context {context; arg_t= zonk arg_t}
  in
  let zonkLocal (local : local) = {local with var= zonkVar local.var} in
  let zonkScopes (scopes : scopes) =
    {current= Map.map zonkLocal scopes.current; enclosing= CCList.map (Map.map zonkLocal) scopes.enclosing}
  in
  let zonkFunction (f : f) =
    { f with
      t= Subst.zonkFunType env.subst f.t
    ; args= Option.map (CCList.map (Subst.zonkArg env.subst)) f.args
    ; context= zonkContext f.context
    ; scopes= zonkScopes f.scopes }
  in
  let zonkGeneric (g : Typed.generic_function) =
    { g with
      args= CCList.map (Subst.zonkArg env.subst) g.args
    ; t= Subst.zonkFunType env.subst g.t
    ; type_param_map= CCList.map (fun (name, t) -> (name, zonk t)) g.type_param_map }
  in
  let zonkType (t : t) =
    match t.descr with Record members -> {t with descr= Record (Map.map zonkVar members)} | _ -> t
  in
  let zonkModule (m : m) =
    { m with
      functions= Map.map zonkFunction m.functions
    ; generics= Map.map zonkGeneric m.generics
    ; constants= Map.map zonkVar m.constants
    ; types= Map.map zonkType m.types }
  in
  {env with modules= Map.map zonkModule env.modules}
