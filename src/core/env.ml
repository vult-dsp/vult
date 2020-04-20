(*
   The MIT License (MIT)

   Copyright (c) 2020 Leonardo Laguna Ruiz

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

module StringMap = CCMap.Make (String)

module Map = struct
  type 'a t = 'a StringMap.t ref

  let empty () = ref StringMap.empty

  let update (report : 'a -> unit) (key : string) (value : 'a) (t : 'a t) : unit =
    t :=
      StringMap.update
        key
        (fun a ->
          match a with
          | None -> Some value
          | Some b ->
              report b ;
              Some b)
        !t


  let of_list elems : 'a t =
    let m = List.fold_left (fun m (key, value) -> StringMap.add key value m) StringMap.empty elems in
    ref m


  let find key t = StringMap.find_opt key !t
end

module type TSig = sig
  type t

  val convert : Typed.ext_type -> t

  val convert_function_type : Typed.ext_type list * Typed.ext_type -> t list * t
end

type var_kind =
  | Mem
  | Val

(*

  "int", Scope.Type, TX.type_type, true
  ; "real", Scope.Type, TX.type_type, true
  ; "bool", Scope.Type, TX.type_type, true
  ; "unit", Scope.Type, TX.type_type, true
  ; "string", Scope.Type, TX.type_type, true
  ; "abstract", Scope.Type, TX.type_type, true
  ; "fix16", Scope.Type, TX.type_type, true

*)

let builtin_functions =
  Typed.
    [ "set", TX.array_set
    ; "get", TX.array_get
    ; "size", TX.array_size
    ; "makeArray", TX.array_make
    ; "abs", TX.freal_freal
    ; "exp", TX.freal_freal
    ; "log10", TX.freal_freal
    ; "sin", TX.freal_freal
    ; "cos", TX.freal_freal
    ; "floor", TX.freal_freal
    ; "tanh", TX.freal_freal
    ; "pow", TX.real_real_real
    ; "cosh", TX.freal_freal
    ; "sinh", TX.freal_freal
    ; "tan", TX.freal_freal
    ; "sqrt", TX.freal_freal
    ; "clip", TX.clip
    ; "int", TX.num_int
    ; "real", TX.num_real
    ; "fix16", TX.num_fix16
    ; "u-", TX.num_num
    ; "+", TX.num_num_num
    ; "-", TX.num_num_num
    ; "*", TX.num_num_num
    ; "/", TX.num_num_num
    ; "%", TX.num_num_num
    ; ">", TX.num_num_bool
    ; "<", TX.num_num_bool
    ; "==", TX.a_a_bool
    ; "<>", TX.a_a_bool
    ; ">=", TX.num_num_bool
    ; "<=", TX.num_num_bool
    ; "|", TX.int_int_int
    ; "&", TX.int_int_int
    ; ">>", TX.int_int_int
    ; "<<", TX.int_int_int
    ; "not", TX.bool_bool
    ; "||", TX.bool_bool_bool
    ; "&&", TX.bool_bool_bool
    ; "eps", TX.unit_real
    ; "pi", TX.unit_real
    ; "random", TX.unit_real
    ; "irandom", TX.unit_int
    ; "samplerate", TX.unit_real
    ; "wrap_array", TX.wrap_array
    ]
  |> Map.of_list


type path = Syntax.path

module Env (T : TSig) = struct
  type var =
    { name : string
    ; t : T.t
    ; loc : Loc.t
    }

  type t =
    { path : path
    ; members : var Map.t
    }

  type context = (path * t) option

  type f =
    { path : path
    ; t : T.t list * T.t
    ; context : context
    ; mutable locals : var Map.t list
    ; mutable active : bool
    }

  type m =
    { name : string
    ; functions : f Map.t
    ; types : t Map.t
    }

  type in_top =
    { modules : m Map.t
    ; builtin_functions : (unit -> Typed.ext_fun_type) Map.t
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

  let makeFunctionForBuiltin name t : f =
    { path = { id = name; n = None; loc = Loc.default }; t; context = None; locals = []; active = false }


  let rec lookVarInScopes (scopes : var Map.t list) name : var option =
    match scopes with
    | [] -> None
    | h :: t ->
        ( match Map.find name h with
        | Some found -> Some found
        | None -> lookVarInScopes t name )


  let lookVarInContext (context : context) name : var option =
    match context with
    | Some (_, { members }) -> Map.find name members
    | None -> None


  let lookVar (env : in_func) (name : string) : var =
    match lookVarInContext env.f.context name with
    | Some found -> found
    | None ->
        ( match lookVarInScopes env.f.locals name with
        | Some found -> found
        | None -> failwith "var not found" )


  let lookFunctionCall (env : in_func) (path : path) : f =
    let reportNotFound result =
      match result with
      | Some found -> found
      | None -> failwith "function not found"
    in
    match path with
    | { id; n = Some n } ->
        begin
          match Map.find n env.top.modules with
          | None -> failwith "module not found"
          | Some m -> reportNotFound (Map.find id m.functions)
        end
    | { id } ->
        ( match Map.find id env.m.functions with
        | Some found -> found
        | None ->
            ( match Map.find id env.top.builtin_functions with
            | Some f -> makeFunctionForBuiltin id (T.convert_function_type (f ()))
            | None -> reportNotFound None ) )


  let lookOperator (env : in_func) (op : string) : f =
    match Map.find op env.top.builtin_functions with
    | Some found -> makeFunctionForBuiltin op (T.convert_function_type (found ()))
    | None -> failwith ("operator not found " ^ op)


  let rec lookType (env : in_func) (path : path) : t =
    let reportNotFound result =
      match result with
      | Some found -> found
      | None -> failwith "type not found"
    in
    match path with
    | { id; n = Some n } ->
        begin
          match Map.find n env.top.modules with
          | None -> failwith "module not found"
          | Some m -> reportNotFound (Map.find id m.types)
        end
    | { id } ->
        ( match Map.find id env.m.types with
        | Some _ as found -> reportNotFound found
        | None -> reportNotFound (Map.find id env.top.builtin_types) )


  let addVar (env : in_func) (name : string) (t : T.t) (kind : var_kind) loc : in_func =
    let report_mem _ = () in
    match kind, env.f.context with
    | Mem, Some (_, context) ->
        let () = env.f.active <- true in
        Map.update report_mem name { name; t; loc } context.members ;
        env
    | Mem, None -> failwith "Internal error: cannot add mem to functions with no context"
    | Val, _ ->
        let report _found = failwith "duplicated declaration" in
        ( match env.f.locals with
        | [] -> failwith "no local scope"
        | h :: _ ->
            Map.update report name { name; t; loc } h ;
            env )


  let pushScope (env : in_func) : in_func =
    env.f.locals <- Map.empty () :: env.f.locals ;
    env


  let popScope (env : in_func) : in_func =
    match env.f.locals with
    | [] -> failwith "invalid scope"
    | _ :: t ->
        env.f.locals <- t ;
        env


  let registerArguments (args : (string * T.t * Loc.t) list) =
    let locals = Map.empty () in
    let report _ = failwith "duplicate argument" in
    let rev_args =
      List.fold_left
        (fun acc (name, t, loc) ->
          let () = Map.update report name { name; t; loc } locals in
          t :: acc)
        []
        args
    in
    locals, List.rev rev_args


  let getPath m name loc : path = { id = name; n = Some m.name; loc }

  let createContextForFunction (env : in_module) name loc : in_context =
    let report _ = failwith "function exitst" in
    let type_name = name ^ "_type" in
    let path = getPath env.m type_name loc in
    let t = { members = Map.empty (); path } in
    let _ = Map.update report type_name t env.m.types in
    { top = env.top; m = env.m; context = Some (path, t) }


  let createContextForExternal (env : in_module) name loc : in_context = { top = env.top; m = env.m; context = None }

  let exitContext (env : in_context) : in_module = { top = env.top; m = env.m }

  let enterFunction (env : in_context) (name : string) (args : (string * T.t * Loc.t) list) (ret : T.t) loc :
      in_func * 'a =
    let report _ = failwith "function exitst" in
    let path = getPath env.m name loc in
    let locals, args_t = registerArguments args in
    let t = args_t, ret in
    let f : f = { path; t; context = env.context; locals = [ locals ]; active = false } in
    let _ = Map.update report name f env.m.functions in
    { top = env.top; m = env.m; f }, t


  let exitFunction (env : in_func) : in_context = { top = env.top; m = env.m; context = env.f.context }

  let addType (env : in_module) (name : string) members loc : in_module =
    let report _ = failwith "type exitst" in
    let path = getPath env.m name loc in
    let t = { members; path } in
    let _ = Map.update report name t env.m.types in
    { top = env.top; m = env.m }


  let enterModule (env : in_top) (name : string) : in_module =
    match Map.find name env.modules with
    | Some m -> { top = env; m }
    | None ->
        let report _ = failwith ("duplicate module: " ^ name) in
        let m : m = { name; functions = Map.empty (); types = Map.empty () } in
        let () = Map.update report name m env.modules in
        { top = env; m }


  let exitModule (env : in_module) : in_top = env.top

  let empty () = { modules = Map.empty (); builtin_functions; builtin_types = Map.empty () }
end

module EnvX = Env (struct
  type t = Typed.ext_type

  let convert t = t

  let convert_function_type (args, ret) = args, ret
end)
