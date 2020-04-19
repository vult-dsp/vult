module StringMap = CCMap.Make (String)

module Map = struct
  type 'a t = 'a StringMap.t ref

  let empty () = ref StringMap.empty

  let update report key value t =
    t :=
      StringMap.update
        key
        (fun found ->
          match found with
          | None -> Some value
          | Some found ->
              let () = report found in
              None)
        !t


  let of_list elems : 'a t =
    let m = List.fold_left (fun m (key, value) -> StringMap.add key value m) StringMap.empty elems in
    ref m


  let find key t = StringMap.find_opt key !t
end

module type TSig = sig
  type t

  val convert : Typed.ext_type -> t
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


module Env (T : TSig) = struct
  type var_n =
    { name : string
    ; t : T.t
    ; kind : var_kind
    }

  type f =
    { path : Syntax.path
    ; t : T.t
    ; mems : var_n Map.t
    ; mutable locals : var_n Map.t list
    ; mutable active : bool
    }

  type t =
    { path : Syntax.path
    ; members : (string * T.t * Loc.t) list
    }

  type m =
    { name : string
    ; functions : f Map.t
    ; types : t Map.t
    }

  type in_top =
    { modules : m Map.t
    ; builtin_functions : (unit -> Typed.ext_type) Map.t
    ; builtin_types : t Map.t
    }

  type in_func = in_top * m * f

  type in_module = in_top * m

  let makeFunction name t : f =
    { path = { id = name; n = None; loc = Loc.default }; t; mems = Map.empty (); locals = []; active = false }


  let lookVar (env : in_func) (name : string) : var_n =
    let _, _, f = env in
    match Map.find name f.mems with
    | Some found -> found
    | None ->
        let rec loop (scopes : var_n Map.t list) =
          match scopes with
          | [] -> failwith "Not found"
          | h :: t ->
              ( match Map.find name h with
              | Some found -> found
              | None -> loop t )
        in
        loop f.locals


  let lookFunctionCall (env : in_func) (path : Syntax.path) : f =
    let reportNotFound result =
      match result with
      | Some found -> found
      | None -> failwith "function not found"
    in
    let t, m, _ = env in
    match path with
    | { id; n = Some n } ->
        begin
          match Map.find n t.modules with
          | None -> failwith "module not found"
          | Some m -> reportNotFound (Map.find id m.functions)
        end
    | { id } ->
        ( match Map.find id m.functions with
        | Some found -> found
        | None ->
            ( match Map.find id t.builtin_functions with
            | Some f -> makeFunction id (T.convert (f ()))
            | None -> reportNotFound None ) )


  let lookOperator (env : in_func) (op : string) : f =
    let t, _, _ = env in
    match Map.find op t.builtin_functions with
    | Some found -> makeFunction op (T.convert (found ()))
    | None -> failwith ("operator not found " ^ op)


  let rec lookType (env : in_func) (path : Syntax.path) : t =
    let reportNotFound result =
      match result with
      | Some found -> found
      | None -> failwith "type not found"
    in
    let t, m, _ = env in
    match path with
    | { id; n = Some n } ->
        begin
          match Map.find n t.modules with
          | None -> failwith "module not found"
          | Some m -> reportNotFound (Map.find id m.types)
        end
    | { id } ->
        ( match Map.find id m.types with
        | Some _ as found -> reportNotFound found
        | None -> reportNotFound (Map.find id t.builtin_types) )


  let addVar (env : in_func) (name : string) (t : T.t) (kind : var_kind) : in_func =
    let _, _, f = env in
    let report _found = failwith "duplicated declaration" in
    match kind with
    | Mem ->
        let () = f.active <- true in
        Map.update report name { name; t; kind } f.mems ;
        env
    | Val ->
        ( match f.locals with
        | [] -> failwith "no local scope"
        | h :: _ ->
            Map.update report name { name; t; kind } h ;
            env )


  let pushScope (env : in_func) : in_func =
    let _, _, f = env in
    f.locals <- Map.empty () :: f.locals ;
    env


  let popScope (env : in_func) : in_func =
    let _, _, f = env in
    match f.locals with
    | [] -> failwith "invalid scope"
    | _ :: t ->
        f.locals <- t ;
        env


  let enterFunction (env : in_module) (name : string) (t : T.t) loc : in_func =
    let top, m = env in
    let report _ = failwith "function exitst" in
    let path : Syntax.path = { id = name; n = Some m.name; loc } in
    let f : f = { path; t; mems = Map.empty (); locals = [ Map.empty () ]; active = false } in
    let _ = Map.update report name f m.functions in
    top, m, f


  let exitFunction (env : in_func) : in_module =
    let t, m, _ = env in
    t, m


  let addType (env : in_module) (name : string) members loc : in_module =
    let top, m = env in
    let report _ = failwith "type exitst" in
    let path : Syntax.path = { id = name; n = Some m.name; loc } in
    let t = { members; path } in
    let _ = Map.update report name t m.types in
    top, m


  let enterModule (env : in_top) (name : string) : in_module =
    match Map.find name env.modules with
    | Some m -> env, m
    | None ->
        let report _ = failwith "duplicate module" in
        let m : m = { name; functions = Map.empty (); types = Map.empty () } in
        let () = Map.update report name m env.modules in
        env, m


  let exitModule (env : in_module) : in_top =
    let top, _ = env in
    top


  let empty () = { modules = Map.empty (); builtin_functions; builtin_types = Map.empty () }
end

module EnvX = Env (struct
  type t = Typed.ext_type

  let convert t = t
end)
