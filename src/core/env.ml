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
open Util
open Maps

let global_tick = ref 0

let getGlobalTick () =
  let n = !global_tick in
  incr global_tick ;
  n


module Map = struct
  type 'a t = 'a Map.t ref

  let empty () = ref Map.empty

  let update (report : 'a -> unit) (key : string) (value : 'a) (t : 'a t) : unit =
    t :=
      Map.update
        key
        (fun a ->
          match a with
          | None -> Some value
          | Some b ->
              report b ;
              Some b)
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
  | Mem
  | Inst
  | Val

type path = Typed.path

type var =
  { name : string
  ; t : Typed.type_
  ; kind : var_kind
  ; loc : Loc.t
  }

type type_descr =
  | Simple
  | Record of var Map.t
  | Enum   of (string * int * Loc.t) Map.t

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
  ; enums : t Map.t
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
    [ "set", C.array_set
    ; "get", C.array_get
    ; "size", C.array_size
    ; "makeArray", C.array_make
    ; "abs", C.freal_freal
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
    ; "int", C.num_int
    ; "real", C.num_real
    ; "fix16", C.num_fix16
    ; "u-", C.num_num
    ; "+", C.num_num_num
    ; "-", C.num_num_num
    ; "*", C.num_num_num
    ; "/", C.num_num_num
    ; "%", C.num_num_num
    ; ">", C.num_num_bool
    ; "<", C.num_num_bool
    ; "==", C.a_a_bool
    ; "<>", C.a_a_bool
    ; ">=", C.num_num_bool
    ; "<=", C.num_num_bool
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
    ]
  |> Map.of_list


let builtin_types =
  [ "int"; "real"; "fix16"; "bool"; "string"; "unit" ]
  |> List.map (fun n ->
         ( n
         , { path = Parser.Syntax.{ id = n; n = None; loc = Loc.default }
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
  | h :: t ->
      ( match Map.find name h with
      | Some found -> Some found
      | None -> lookVarInScopes t name )


let lookVarInContext (context : context) name : var option =
  match context with
  | Some (_, { descr = Record members; _ }) -> Map.find name members
  | _ -> None


let lookVar (env : in_func) (name : string) : var =
  match lookVarInContext env.f.context name with
  | Some found -> found
  | None ->
      ( match lookVarInScopes env.f.locals name with
      | Some found -> found
      | None -> failwith "var not found" )


let lookEnum (env : in_func) (path : path) =
  let findEnumInModule enums id =
    match Map.find id enums with
    | Some ({ descr = Enum members; _ } as t) ->
        begin
          match Map.find id members with
          | Some (_, index, _) -> t.path, t.loc, index
          | None -> failwith ("Enum not found " ^ id)
        end
    | _ -> failwith ("Enum not found " ^ id)
  in
  match path with
  | { id; n = None; _ } -> findEnumInModule env.m.enums id
  | { id; n = Some n; _ } ->
      begin
        match Map.find n env.top.modules with
        | Some m -> findEnumInModule m.enums id
        | None -> failwith "Module"
      end


let lookFunctionCall (env : in_func) (path : path) : f =
  let reportNotFound result =
    match result with
    | Some found -> found
    | None -> failwith "function not found"
  in
  match path with
  | { id; n = Some n; _ } ->
      begin
        match Map.find n env.top.modules with
        | None -> failwith ("module not found " ^ n)
        | Some m -> reportNotFound (Map.find id m.functions)
      end
  | { id; _ } ->
      ( match Map.find id env.m.functions with
      | Some found -> found
      | None ->
          ( match Map.find id env.top.builtin_functions with
          | Some f -> makeFunctionForBuiltin id (f ())
          | None -> reportNotFound None ) )


let lookOperator (env : in_func) (op : string) : f =
  match Map.find op env.top.builtin_functions with
  | Some found -> makeFunctionForBuiltin op (found ())
  | None -> failwith ("operator not found " ^ op)


let getType (env : in_top) (path : path) : t option =
  match path with
  | { id; n = Some n; _ } ->
      begin
        match Map.find n env.modules with
        | None -> failwith ("module not found " ^ n)
        | Some m -> Map.find id m.types
      end
  | _ -> None


let lookType (env : in_func) (path : path) : t =
  let reportNotFound result =
    match result with
    | Some found -> found
    | None ->
        let path = Pla.print (Parser.Syntax.print_path path) in
        failwith ("lookType: type not found " ^ path)
  in
  match path with
  | { id; n = Some n; _ } ->
      begin
        match Map.find n env.top.modules with
        | None -> failwith ("module not found " ^ n)
        | Some m -> reportNotFound (Map.find id m.types)
      end
  | { id; _ } ->
      ( match Map.find id env.m.types with
      | Some _ as found -> reportNotFound found
      | None -> reportNotFound (Map.find id env.top.builtin_types) )


let lookTypeInModule (env : in_module) (path : path) : t =
  let reportNotFound result =
    match result with
    | Some found -> found
    | None ->
        let path = Pla.print (Parser.Syntax.print_path path) in
        failwith ("lookTypeInModule: type not found " ^ path)
  in
  match path with
  | { id; n = Some n; _ } ->
      begin
        match Map.find n env.top.modules with
        | None -> failwith ("module not found " ^ n)
        | Some m -> reportNotFound (Map.find id m.types)
      end
  | { id; _ } ->
      ( match Map.find id env.m.types with
      | Some _ as found -> reportNotFound found
      | None -> reportNotFound (Map.find id env.top.builtin_types) )


let addVar (env : in_func) unify (name : string) (t : Typed.type_) (kind : var_kind) loc : in_func =
  let report_mem (found : var) =
    if unify found.t t then
      ()
    else
      failwith ("type changed: " ^ found.name)
  in
  match kind, env.f.context with
  | (Mem | Inst), Some (_, { descr = Record members; _ }) ->
      Map.update report_mem name { name; t; kind; loc } members ;
      env
  | (Mem | Inst), None -> failwith "Internal error: cannot add mem to functions with no context"
  | Val, _ ->
      let report _found = failwith "duplicated declaration" in
      ( match env.f.locals with
      | [] -> failwith "no local scope"
      | h :: _ ->
          Map.update report name { name; t; kind; loc } h ;
          env )
  | _, Some _ -> failwith "Not a record"


let addReturnVar (env : in_context) (name : string) (t : Typed.type_) loc : in_context =
  let report_mem _ = () in
  match env.context with
  | Some (_, { descr = Record members; _ }) ->
      Map.update report_mem name { name; t; kind = Mem; loc } members ;
      env
  | None -> failwith "Internal error: cannot add mem to functions with no context"
  | Some _ -> failwith "Not a record"


let pushScope (env : in_func) : in_func =
  env.f.locals <- Map.empty () :: env.f.locals ;
  env


let popScope (env : in_func) : in_func =
  match env.f.locals with
  | [] -> failwith "invalid scope"
  | _ :: t ->
      env.f.locals <- t ;
      env


let registerArguments (args : (string * Typed.type_ * Loc.t) list) =
  let locals = Map.empty () in
  let report _ = failwith "duplicate argument" in
  let rev_args =
    List.fold_left
      (fun acc (name, t, loc) ->
        let () = Map.update report name { name; t; kind = Val; loc } locals in
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
  let index = getGlobalTick () in
  let t = { descr = Record (Map.empty ()); path; index; loc; generated = true } in
  let _ = Map.update report type_name t env.m.types in
  { top = env.top; m = env.m; context = Some (path, t) }


let addRecordMember members =
  let report _ = failwith "duplicated member of type" in
  let members =
    List.fold_left
      (fun m (name, t, loc) ->
        Map.update report name { name; t; kind = Val; loc } m ;
        m)
      (Map.empty ())
      members
  in
  Record members


let addType (env : in_module) type_name members loc : in_module =
  let report _ = failwith "type exitst" in
  let index = getGlobalTick () in
  let path = getPath env.m type_name loc in
  let descr = addRecordMember members in
  let t = { path; descr; loc; index; generated = false } in
  let _ = Map.update report type_name t env.m.types in
  env


let addEnumMember members =
  let report _ = failwith "duplicated enum type" in
  let members, _ =
    List.fold_left
      (fun (m, i) (name, loc) ->
        Map.update report name (name, i, loc) m ;
        m, i + 1)
      (Map.empty (), 0)
      members
  in
  Enum members


let addEnumToModule (env : in_module) members t =
  let report _ = failwith "the enum exitst" in
  let () = List.iter (fun (name, _) -> Map.update report name t env.m.enums) members in
  env


let addEnum (env : in_module) type_name members loc : in_module =
  let report _ = failwith "type exitst" in
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
  env.f.tick <- n ;
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
    loc : in_func * path * 'a =
  let report _ = failwith "function exitst" in
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

let enterModule (env : in_top) (name : string) : in_module =
  match Map.find name env.modules with
  | Some m -> { top = env; m }
  | None ->
      let report _ = failwith ("duplicate module: " ^ name) in
      let m : m = { name; functions = Map.empty (); types = Map.empty (); enums = Map.empty () } in
      let () = Map.update report name m env.modules in
      { top = env; m }


let exitModule (env : in_module) : in_top = env.top

let empty () = { modules = Map.empty (); builtin_functions; builtin_types }
