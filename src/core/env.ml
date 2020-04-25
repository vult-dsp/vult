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


  let to_list (t : 'a t) = StringMap.to_list !t

  let find key t = StringMap.find_opt key !t

  let is_empty (t : 'a t) : bool = StringMap.is_empty !t

  let fold (f : string -> 'a -> 'b -> 'b) (s : 'b) (t : 'a t) : 'b = StringMap.fold f !t s
end

module type TSig = sig
  type t

  val convert : Typed.type_ -> t

  val convert_function_type : Typed.type_ list * Typed.type_ -> t list * t
end

type var_kind =
  | Mem
  | Val
  | Inst

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


type path = Typed.path

type var =
  { name : string
  ; t : Typed.type_
  ; kind : var_kind
  ; loc : Loc.t
  }

type t =
  { path : path
  ; members : var Map.t
  ; index : int
  ; loc : Loc.t
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
  }

type in_top =
  { modules : m Map.t
  ; builtin_functions : (unit -> Typed.fun_type) Map.t
  ; builtin_types : t Map.t
  }

type in_module =
  { top : in_top
  ; m : m
  ; mutable tick : int
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
        | None -> failwith ("module not found " ^ n)
        | Some m -> reportNotFound (Map.find id m.functions)
      end
  | { id } ->
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


let rec getType (env : in_top) (path : path) : t option =
  match path with
  | { id; n = Some n } ->
      begin
        match Map.find n env.modules with
        | None -> failwith ("module not found " ^ n)
        | Some m -> Map.find id m.types
      end
  | { id } -> None


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
        | None -> failwith ("module not found " ^ n)
        | Some m -> reportNotFound (Map.find id m.types)
      end
  | { id } ->
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
  | (Mem | Inst), Some (_, context) ->
      Map.update report_mem name { name; t; kind; loc } context.members ;
      env
  | (Mem | Inst), None -> failwith "Internal error: cannot add mem to functions with no context"
  | Val, _ ->
      let report _found = failwith "duplicated declaration" in
      ( match env.f.locals with
      | [] -> failwith "no local scope"
      | h :: _ ->
          Map.update report name { name; t; kind; loc } h ;
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

let getModuleTick (env : in_module) : int =
  let n = env.tick + 1 in
  env.tick <- n ;
  n


let createContextForFunction (env : in_module) name loc : in_context =
  let report _ = failwith "function exitst" in
  let type_name = name ^ "_type" in
  let path = getPath env.m type_name loc in
  let index = getModuleTick env in
  let t = { members = Map.empty (); path; index; loc } in
  let _ = Map.update report type_name t env.m.types in
  { top = env.top; m = env.m; context = Some (path, t) }


let createContextForExternal (env : in_module) name loc : in_context = { top = env.top; m = env.m; context = None }

let exitContext (env : in_context) : in_module = { top = env.top; m = env.m; tick = 0 }

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
  | Some (_, t) -> not (Map.is_empty t.members)
  | None -> false


let exitFunction (env : in_func) : in_context = { top = env.top; m = env.m; context = env.f.context }

let addType (env : in_module) (name : string) members loc : in_module =
  let report _ = failwith "type exitst" in
  let path = getPath env.m name loc in
  let index = getModuleTick env in
  let t = { members; path; index; loc } in
  let _ = Map.update report name t env.m.types in
  { env with top = env.top; m = env.m }


let enterModule (env : in_top) (name : string) : in_module =
  match Map.find name env.modules with
  | Some m -> { top = env; m; tick = 0 }
  | None ->
      let report _ = failwith ("duplicate module: " ^ name) in
      let m : m = { name; functions = Map.empty (); types = Map.empty () } in
      let () = Map.update report name m env.modules in
      { top = env; m; tick = 0 }


let exitModule (env : in_module) : in_top = env.top

let empty () = { modules = Map.empty (); builtin_functions; builtin_types = Map.empty () }
