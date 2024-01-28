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
    t
    := Map.update
         key
         (fun a ->
           match a with
           | None -> Some value
           | Some b ->
             let c = report b value in
             Some c)
         !t


  let of_list elems : 'a t =
    let m = List.fold_left (fun m (key, value) -> Map.add key value m) Map.empty elems in
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

type f =
  { path : path
  ; t : Typed.type_ list * Typed.type_
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

type in_top =
  { modules : m Map.t
  ; builtin_functions : (unit -> Typed.fun_type) Map.t
  ; builtin_types : t Map.t
  }

type in_module =
  { top : in_top
  ; m : m
  }

type in_context =
  { top : in_top
  ; m : m
  ; context : context
  }

type in_func =
  { top : in_top
  ; m : m
  ; f : f
  }

let builtin_functions =
  Typed.
    [ "size", C.array_size
    ; "abs", C.num_num
    ; "exp", C.freal_freal
    ; "log10", C.freal_freal
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
  [ "int"; "real"; "fix16"; "bool"; "string"; "unit" ]
  |> List.map (fun n ->
    ( n
    , { path = Pparser.Syntax.{ id = n; n = None; loc = Loc.default }
      ; descr = Simple
      ; index = 0
      ; loc = Loc.default
      ; generated = false
      } ))
  |> Map.of_list


let makeFunctionForBuiltin name t : f =
  { path = { id = name; n = None; loc = Loc.default }; t; context = None; locals = []; tick = 0 }


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


let lookVar (env : in_func) (name : string) (loc : Loc.t) : var =
  match lookVarInContext env.f.context name with
  | Some found -> found
  | None -> (
    match lookVarInScopes env.f.locals name with
    | Some found -> found
    | None -> (
      match Map.find name env.m.constants with
      | Some var -> var
      | None -> Error.raiseError ("The variable '" ^ name ^ "' could not be found") loc))


let lookConstant (env : in_module) (name : string) (loc : Loc.t) : var =
  match Map.find name env.m.constants with
  | None -> Error.raiseError ("A constant with the name '" ^ name ^ "' could not be found") loc
  | Some var -> var


let reportModuleNotFound n loc = Error.raiseError ("The module named '" ^ n ^ "' could not be found") loc

let lookEnum (env : in_func) (path : path) (loc : Loc.t) =
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
  | { id; n = None; _ } -> findEnumInModule env.m.enums id
  | { id; n = Some n; loc } -> (
    match Map.find n env.top.modules with
    | Some m -> findEnumInModule m.enums id
    | None -> reportModuleNotFound n loc)


(* TODO: this function is exactly the same as lookEnum. Refactor to use the same code. *)
let lookEnumInModule (env : in_module) (path : path) (loc : Loc.t) =
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
  | { id; n = None; _ } -> findEnumInModule env.m.enums id
  | { id; n = Some n; loc } -> (
    match Map.find n env.top.modules with
    | Some m -> findEnumInModule m.enums id
    | None -> reportModuleNotFound n loc)


let lookFunctionCall (env : in_func) (path : path) (loc : Loc.t) : f =
  let reportNotFound result =
    match result with
    | Some found -> found
    | None -> Error.raiseError ("A function with the name '" ^ pathString path ^ "' could not be found") loc
  in
  match path with
  | { id; n = Some n; _ } -> (
    match Map.find n env.top.modules with
    | None -> reportModuleNotFound n loc
    | Some m -> reportNotFound (Map.find id m.functions))
  | { id; _ } -> (
    match Map.find id env.m.functions with
    | Some found -> found
    | None -> (
      match Map.find id env.top.builtin_functions with
      | Some f -> makeFunctionForBuiltin id (f ())
      | None -> reportNotFound None))


let lookOperator (env : in_func) (op : string) : f =
  match Map.find op env.top.builtin_functions with
  | Some found -> makeFunctionForBuiltin op (found ())
  | None -> failwith ("operator not found " ^ op)


let lookOperatorInModule (env : in_module) (op : string) : f =
  match Map.find op env.top.builtin_functions with
  | Some found -> makeFunctionForBuiltin op (found ())
  | None -> failwith ("operator not found " ^ op)


let getType (env : in_top) (path : path) : t option =
  match path with
  | { id; n = Some n; loc } -> (
    match Map.find n env.modules with
    | None -> reportModuleNotFound n loc
    | Some m -> Map.find id m.types)
  | _ -> None


let lookType (env : in_func) (path : path) (loc : Loc.t) : t =
  let reportNotFound result =
    match result with
    | Some found -> found
    | None -> Error.raiseError ("A type with the name '" ^ pathString path ^ "' could not be found") loc
  in
  match path with
  | { id; n = Some n; loc } -> (
    match Map.find n env.top.modules with
    | None -> reportModuleNotFound n loc
    | Some m -> reportNotFound (Map.find id m.types))
  | { id; _ } -> (
    match Map.find id env.m.types with
    | Some _ as found -> reportNotFound found
    | None -> reportNotFound (Map.find id env.top.builtin_types))


let lookTypeInModule (env : in_module) (path : path) (loc : Loc.t) : t =
  let reportNotFound result =
    match result with
    | Some found -> found
    | None -> Error.raiseError ("A type with the name '" ^ pathString path ^ "' could not be found") loc
  in
  match path with
  | { id; n = Some n; _ } -> (
    match Map.find n env.top.modules with
    | None -> reportModuleNotFound n loc
    | Some m -> reportNotFound (Map.find id m.types))
  | { id; _ } -> (
    match Map.find id env.m.types with
    | Some _ as found -> reportNotFound found
    | None -> reportNotFound (Map.find id env.top.builtin_types))


let addConstant (env : in_module) _unify (name : string) (t : Typed.type_) loc : in_module =
  let report (found : var) =
    Error.raiseError
      ("A constant with the name '" ^ found.name ^ "' has already been declared at " ^ Loc.to_string_readable found.loc)
      loc
  in
  Map.update report name { name; t; kind = Const; tags = []; loc } env.m.constants;
  env


let addVar (env : in_func) unify (name : string) (t : Typed.type_) (kind : var_kind) loc : in_func =
  let report_mem (found : var) (value : var) =
    if unify found.t t then (
      let tags = Pparser.Ptags.mergeTags found.tags value.tags in
      { found with tags })
    else
      Error.raiseError ("This declaration tries to change the type of " ^ found.name) value.loc
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
    List.iter
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
  match kind, env.f.context with
  | Inst, Some (_, { descr = Record members; _ }) ->
    let () = checkDuplicatedVal env.f.locals name in
    Map.update report_mem name { name; t; kind; tags = []; loc } members;
    env
  | Mem tags, Some (_, { descr = Record members; _ }) ->
    let () = checkDuplicatedVal env.f.locals name in
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
    match env.f.locals with
    | [] -> failwith "no local scope"
    | h :: _ ->
      Map.update report name { name; t; kind; tags = []; loc } h;
      env)
  | Const, _ -> failwith "Do not use to add constants"
  | _, Some _ -> failwith "Not a record"


let addReturnVar (env : in_context) (name : string) (t : Typed.type_) loc : in_context =
  let report_mem found _ = found in
  match env.context with
  | Some (_, { descr = Record members; _ }) ->
    Map.update report_mem name { name; t; kind = Mem []; tags = []; loc } members;
    env
  | None -> failwith "Internal error: cannot add mem to functions with no context"
  | Some _ -> failwith "Not a record"


let pushScope (env : in_func) : in_func =
  env.f.locals <- Map.empty () :: env.f.locals;
  env


let popScope (env : in_func) : in_func =
  match env.f.locals with
  | [] -> failwith "invalid scope"
  | _ :: t ->
    env.f.locals <- t;
    env


let registerArguments (args : (string * Typed.type_ * Loc.t) list) =
  let locals = Map.empty () in
  let report loc (found : var) =
    Error.raiseError
      ("A variable with the name '" ^ found.name ^ "' has already been declared at " ^ Loc.to_string_readable found.loc)
      loc
  in
  let rev_args =
    List.fold_left
      (fun acc (name, t, loc) ->
        let () = Map.update (report loc) name { name; t; kind = Val; tags = []; loc } locals in
        t :: acc)
      []
      args
  in
  locals, List.rev rev_args


let getPath m name loc : path = { id = name; n = Some m.name; loc }

let createContextForFunction (env : in_module) name loc : in_context =
  let report name (found : t) =
    Error.raiseError
      ("A function with the name '" ^ name ^ "' already exists at " ^ Loc.to_string_readable found.loc)
      loc
  in
  let type_name = name ^ "_type" in
  let path = getPath env.m type_name loc in
  let index = getGlobalTick () in
  let t = { descr = Record (Map.empty ()); path; index; loc; generated = true } in
  let _ = Map.update (report name) type_name t env.m.types in
  { top = env.top; m = env.m; context = Some (path, t) }


let addAliasToContext (env : in_context) name loc : in_context =
  match env.context with
  | Some (ctx, { descr = Record members; _ }) when not (Map.is_empty members) ->
    let report found =
      Error.raiseError ("A context with the same name already exists at " ^ Loc.to_string_readable found.loc) loc
    in
    let type_name = name ^ "_type" in
    let path = getPath env.m type_name loc in
    let index = getGlobalTick () in
    let t = { descr = Alias (path, ctx); path; index; loc; generated = true } in
    let _ = Map.update report type_name t env.m.types in
    env
  | _ -> env


let addRecordMember members =
  let report loc (found : var) =
    Error.raiseError
      ("A member with the name '" ^ found.name ^ "' has already been declared at " ^ Loc.to_string_readable found.loc)
      loc
  in
  let members =
    List.fold_left
      (fun m (name, t, tags, loc) ->
        Map.update (report loc) name { name; t; kind = Val; tags; loc } m;
        m)
      (Map.empty ())
      members
  in
  Record members


let addType (env : in_module) type_name members loc : in_module =
  let report (found : t) =
    Error.raiseError
      ("A type with the name '" ^ found.path.id ^ "' has already been declared at " ^ Loc.to_string_readable found.loc)
      loc
  in
  let index = getGlobalTick () in
  let path = getPath env.m type_name loc in
  let descr = addRecordMember members in
  let t = { path; descr; loc; index; generated = false } in
  let _ = Map.update report type_name t env.m.types in
  env


let addEnumMember members =
  let report loc (name, _, floc) =
    Error.raiseError
      ("A member with the name '" ^ name ^ "' has already been declared at " ^ Loc.to_string_readable floc)
      loc
  in
  let members, _ =
    List.fold_left
      (fun (m, i) (name, loc) ->
        Map.update (report loc) name (name, i, loc) m;
        m, i + 1)
      (Map.empty (), 0)
      members
  in
  Enum members


let addEnumToModule (env : in_module) members t =
  let report loc name (found : t) =
    Error.raiseError
      ("A enum value with the name '" ^ name ^ "' has already been declared at " ^ Loc.to_string_readable found.loc)
      loc
  in
  let () = List.iter (fun (name, loc) -> Map.update (report loc name) name t env.m.enums) members in
  env


let addEnum (env : in_module) type_name members loc : in_module =
  let report (found : t) =
    Error.raiseError
      ("A enum with the name '" ^ found.path.id ^ "' has already been declared at " ^ Loc.to_string_readable found.loc)
      loc
  in
  let index = getGlobalTick () in
  let path = getPath env.m type_name loc in
  let descr = addEnumMember members in
  let t = { path; descr; loc; index; generated = false } in
  let _ = Map.update report type_name t env.m.types in
  let env = addEnumToModule env members t in
  env


let createContextForExternal (env : in_module) : in_context = { top = env.top; m = env.m; context = None }
let exitContext (env : in_context) : in_module = { top = env.top; m = env.m }

let getFunctionTick (env : in_func) : int =
  let n = env.f.tick + 1 in
  env.f.tick <- n;
  n


let getContext (env : in_func) : path =
  match env.f.context with
  | Some (p, _) -> p
  | None -> failwith "trying to get the context of a function without one"


let getFunctionContext (f : f) : path =
  match f.context with
  | Some (p, _) -> p
  | None -> failwith "trying to get the context of a function without one"


let enterFunction
  (env : in_context)
  (name : string)
  (args : (string * Typed.type_ * Loc.t) list)
  (ret : Typed.type_)
  loc
  : in_func * path * 'a
  =
  let report (found : f) =
    Error.raiseError ("A function with the name '" ^ found.path.id ^ "' has already been declared.") loc
  in
  let path = getPath env.m name loc in
  let locals, args_t = registerArguments args in
  let t = args_t, ret in
  let f : f = { path; t; context = env.context; locals = [ locals ]; tick = 0 } in
  let _ = Map.update report name f env.m.functions in
  { top = env.top; m = env.m; f }, path, t


let isFunctionActive (f : f) =
  match f.context with
  | Some (_, { descr = Record members; _ }) -> not (Map.is_empty members)
  | _ -> false


let exitFunction (env : in_func) : in_context = { top = env.top; m = env.m; context = env.f.context }

let addCustomInitFunction (env : in_context) name =
  match env.context with
  | Some (p, _) ->
    env.m.init <- (p, name) :: env.m.init;
    env
  | _ -> env


let enterModule (env : in_top) (name : string) : in_module =
  match Map.find name env.modules with
  | Some m -> { top = env; m }
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
    { top = env; m }


let exitModule (env : in_module) : in_top = env.top
let empty () = { modules = Map.empty (); builtin_functions; builtin_types }
