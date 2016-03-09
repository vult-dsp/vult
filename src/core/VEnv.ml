(*
The MIT License (MIT)

Copyright (c) 2014 Leonardo Laguna Ruiz, Carl Jönsson

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

open TypesVult
open CCOpt

let idStr = PrintTypes.identifierStr

let pathStr path = path |> pathId |> PrintTypes.identifierStr

let builtin_table =
   [
      ["int"]  , `Type, VType.Constants.type_type, true;
      ["real"] , `Type, VType.Constants.type_type, true;
      ["bool"] , `Type, VType.Constants.type_type, true;
      ["unit"] , `Type, VType.Constants.type_type, true;

      ["set"] , `Function, VType.Constants.array_set (), false;
      ["get"] , `Function, VType.Constants.array_get (), false;
      ["size"] , `Function, VType.Constants.array_size (), false;
      ["makeArray"], `Function, VType.Constants.array_make (), false;

      ["abs"]  , `Function, VType.Constants.real_real (), false;
      ["exp"]  , `Function, VType.Constants.real_real (), false;
      ["sin"]  , `Function, VType.Constants.real_real (), false;
      ["cos"]  , `Function, VType.Constants.real_real (), false;
      ["floor"], `Function, VType.Constants.real_real (), false;
      ["tanh"] , `Function, VType.Constants.real_real (), false;
      ["tan"]  , `Function, VType.Constants.real_real (), false;
      ["sqrt"] , `Function, VType.Constants.real_real (), false;
      ["clip"] , `Function, VType.Constants.a_a_a_a (), false;

      ["int"]  , `Function, VType.Constants.num_int (), false;
      ["real"] , `Function, VType.Constants.num_real (), false;

      ["|-|"] , `Operator, VType.Constants.num_num (), false;
      ["+"]  , `Operator, VType.Constants.a_a_a (), false;
      ["-"]  , `Operator, VType.Constants.a_a_a (), false;
      ["*"]  , `Operator, VType.Constants.a_a_a (), false;
      ["/"]  , `Operator, VType.Constants.a_a_a (), false;
      ["%"]  , `Operator, VType.Constants.a_a_a (), false;

      [">"]   , `Operator, VType.Constants.a_a_bool (), false;
      ["<"]   , `Operator, VType.Constants.a_a_bool (), false;
      ["=="]  , `Operator, VType.Constants.a_a_bool (), false;
      ["<>"]  , `Operator, VType.Constants.a_a_bool (), false;
      [">="]  , `Operator, VType.Constants.a_a_bool (), false;
      ["<="]  , `Operator, VType.Constants.a_a_bool (), false;

      ["not"]  , `Function, VType.Constants.bool_bool (), false;
      ["||"]  , `Operator, VType.Constants.bool_bool_bool (), false;
      ["&&"]  , `Operator, VType.Constants.bool_bool_bool (), false;
   ]

let builtin_functions = List.map (fun (a,_,_,_)->a) builtin_table |> IdSet.of_list

(** Maps which function belongs to which context *)
module Context = struct

   type t =
      {
         forward   : id IdMap.t;
         backward  : IdSet.t IdMap.t;
         init_fun  : id IdMap.t;
         count     : int;
         current   : id;
      }

   let empty =
      {
         forward   = IdMap.empty;
         backward  = IdMap.empty;
         init_fun  = IdMap.empty;
         count     = 0;
         current   = [];
      }

   let addTo (context:t) (func:id) (is_init:bool) : t =
      let current_in_ctx =
         try IdMap.find context.current context.backward with
         | Not_found -> IdSet.empty
      in
      {
         context with
         forward  = IdMap.add func context.current context.forward;
         backward = IdMap.add context.current (IdSet.add func current_in_ctx) context.backward;
         init_fun = if is_init then IdMap.add context.current func context.init_fun else context.init_fun;
      }

   let makeNew (context:t) (func:id) (is_init:bool) : t =
      if IdMap.mem func context.forward then
         context
      else
         let context_name = ["_ctx_type_"^(string_of_int context.count)] in
         {
            count    = context.count+1;
            current  = context_name;
            forward  = IdMap.add func context_name context.forward;
            backward = IdMap.add context_name (IdSet.of_list [func]) context.backward;
            init_fun = if is_init then IdMap.add context_name func context.init_fun else context.init_fun;
         }

   let getAllWithContext (context:t) (func:id) : id list =
      try
         let ctx = IdMap.find func context.forward in
         let all = IdMap.find ctx context.backward in
         IdSet.fold (fun a s -> a :: s) all []
      with
      | Not_found -> []

   let getContext (context:t) (func:id) : id =
      IdMap.find func context.forward

   let getInitFunction (context:t) (name:id) : id option =
      let ctx = IdMap.find name context.forward in
      match IdMap.find ctx context.init_fun with
      | init_fun -> Some(init_fun)
      | exception Not_found -> None

end

type symbol_kind =
   | MemSymbol
   | VarSymbol
   | InstanceSymbol
   | FunctionSymbol
   | ModuleSymbol
   | OperatorSymbol
   | TypeSymbol
   | UndefSymbol
   [@@deriving show]

(** Used to track the location while traversing and also to lookup function, mem, and module *)
module Scope = struct

   type t =
      {
         name      : id;             (** Name of the current scope *)
         kind      : symbol_kind;    (** Type of the current scope *)
         parent    : t option;       (** Pointer to it's parent *)
         typ       : VType.t;        (** Type of the symbol *)

         operators : t IdMap.t;      (** Operators *)
         modules   : t IdMap.t;      (** Modules or namespaces *)
         types     : t IdMap.t;      (** Types *)
         func      : t IdMap.t;      (** Functions *)
         mem_inst  : t IdMap.t;      (** Mem and instances *)
         locals    : t IdMap.t list; (** Variables and subscopes *)

         ctx       : Context.t;

         single    : bool;           (** true if every function call does not create a new instance *)

         active    : bool;           (** true if the fuction contains a mem or an instance *)

         ext_fn    : string option;  (** contains the replacement name if it's an external function *)

      }

   type t_simple =
      {
         sname      : id;
         skind      : symbol_kind;
         styp       : VType.t;

         smodules   : (id * t_simple) list;
         soperators : (id * t_simple) list;
         stypes     : (id * t_simple) list;
         sfunc      : (id * t_simple) list;
         smem_inst  : (id * t_simple) list;
         slocals    : (id * t_simple) list list;

         ssingle    : bool;
         sactive    : bool;
         sext_fn    : string option;

      }
      [@@deriving show]

   let rec simple (t:t) : t_simple =
      let simple_pair (k,v) = (k,simple v) in
      let f = List.map simple_pair in
      {  sname = t.name;
         skind =t.kind;
         styp = t.typ;
         soperators = IdMap.to_list t.operators |> f;
         smodules = IdMap.to_list t.modules |> f;
         stypes = IdMap.to_list t.types |> f;
         sfunc = IdMap.to_list t.func |> f;
         smem_inst = IdMap.to_list t.mem_inst |> f;
         slocals = List.map IdMap.to_list t.locals |> List.map f;
         ssingle = t.single;
         sactive = t.active;
         sext_fn = t.ext_fn;
      }

   let show t = simple t |> show_t_simple

   let show_full t =
      let rec up t =
         match t.parent with
         | Some(p) -> up p
         | _ -> t
      in show (up t)

   let empty : t =
      {
         name      = [];
         kind      = UndefSymbol;
         parent    = None;
         operators = IdMap.empty;
         modules   = IdMap.empty;
         types     = IdMap.empty;
         func      = IdMap.empty;
         mem_inst  = IdMap.empty;
         locals    = [];
         typ       = ref (VType.TId([""],None));
         ctx       = Context.empty;
         single    = true;
         active    = false;
         ext_fn    = None;

      }

   let getFunction (t:t) : t IdMap.t =
      t.func

   let findFunction (t:t) (name:id) : t option =
      try Some(IdMap.find name t.func) with
      | _ ->
         try Some(IdMap.find name t.modules) with
         | _ -> None

   let primExitFunction (parent:t) (t:t) : t =
      { parent with func = IdMap.add t.name t parent.func }

   let getModule (t:t) : t IdMap.t =
      t.modules

   let findModule (t:t) (name:id) : t option =
      try Some(IdMap.find name t.modules) with
      | _ -> None

   let primExitModule (parent:t) (t:t) : t =
      { parent with modules = IdMap.add t.name t parent.modules }

   let getMemInst (t:t) : t IdMap.t =
      t.mem_inst

   let findMemInst (t:t) (name:id) : t option =
      try Some(IdMap.find name t.mem_inst) with
      | _ -> None

   let primExitMemInst (parent:t) (t:t) : t =
      { parent with mem_inst = IdMap.add t.name t parent.mem_inst }

   let getTypes (t:t) : t IdMap.t =
      t.types

   let findType (t:t) (name:id) : t option =
      try Some(IdMap.find name t.types) with
      | _ ->
         try Some(IdMap.find name t.modules) with
         | _ -> None

   let primExitTypes (parent:t) (t:t) : t =
      { parent with types = IdMap.add t.name t parent.types }

   let getOperators (t:t) : t IdMap.t =
      t.operators

   let findOperator (t:t) (name:id) : t option =
      try Some(IdMap.find name t.operators) with
      | _ -> None

   let primExitOperators (parent:t) (t:t) : t =
      { parent with operators = IdMap.add t.name t parent.operators }

   let addMem (t:t) (name:id) (typ:VType.t) : t =
      let new_symbol = { empty with name = name; kind = MemSymbol; typ = typ } in
      { t with mem_inst = IdMap.add name new_symbol t.mem_inst; active = true }

   let addInstance (t:t) (name:id) (typ:VType.t) : t =
      let new_symbol = { empty with name = name; kind = InstanceSymbol; typ = typ } in
      { t with mem_inst = IdMap.add name new_symbol t.mem_inst; active = true }

   let addVar (t:t) (name:id) (typ:VType.t) : t =
      let new_symbol = { empty with name = name; kind = VarSymbol; typ = typ  } in
      let first,rest =
         match t.locals with
         | [] -> IdMap.empty,[]
         | h::t -> h,t
      in
      { t with locals = (IdMap.add name new_symbol first) :: rest }

   let enterAny kind (get:t -> t IdMap.t) (t:t) (name:id) : t =
      match IdMap.find name (get t) with
      | sub ->
         { sub with parent = Some(t) }
      | exception Not_found ->
         { empty with parent = Some(t); name = name; kind = kind }

   let exitAny (exit:t -> t -> t) (t:t) : t =
      match t.parent with
      | None -> failwith "Scope.exit: Cannot exit more scopes"
      | Some(parent) ->
         exit parent t

   let current (t:t) : path =
      let rec parentName parent =
         match parent with
         | None -> []
         | Some(parent_t) ->
            parent_t.name :: parentName parent_t.parent
      in
      t.name :: parentName t.parent
      |> List.rev |> List.flatten |> fun a -> Path(a)

   let newContext (t:t) (name:id) (is_init:bool) : t =
      match t.parent with
      | None -> t
      | Some(parent) ->
         { t with parent = Some({ parent with ctx = Context.makeNew parent.ctx name is_init }) }

   let addToContext (t:t) (name:id) (is_init:bool) : t =
      match t.parent with
      | None -> t
      | Some(parent) ->
         { t with parent = Some({ parent with ctx = Context.addTo parent.ctx name is_init }) }

   let getContext (t:t) : path =
      match t.parent with
      | None -> raise (Invalid_argument "Scope.getContext")
      | Some(parent) ->
         let Path(parent_path) = current parent in
         let ctx = Context.getContext parent.ctx t.name in
         Path(parent_path@ctx)

   let getInitFunction (t:t) (name:id) : id option =
      match t.parent with
      | None -> raise (Invalid_argument "Scope.getContext")
      | Some(parent) ->
         Context.getInitFunction parent.ctx name

   let enter kind (attr:attr) (t:t) (name:id) : t =
      match kind with
      | `Function ->
         let t' = enterAny FunctionSymbol getFunction t name in
         let t' = { t' with ext_fn = t'.ext_fn <+> attr.ext_fn } in
         if attr.fun_and then
            addToContext t' name attr.init
         else
            newContext t' name attr.init
      | `Module   -> enterAny ModuleSymbol getModule t name
      | `Operator -> enterAny OperatorSymbol getOperators t name
      | `Type     -> enterAny TypeSymbol getTypes t name
      | `Block    -> { t with locals = IdMap.empty :: t.locals }
      | _ -> raise (Invalid_argument "Scope.enter")

   let exit kind (t:t) : t =
      match kind with
      | `Function -> exitAny primExitFunction t
      | `Module   -> exitAny primExitModule t
      | `Operator -> exitAny primExitOperators t
      | `Type     -> exitAny primExitTypes t
      | `Block    -> { t with locals = List.tl t.locals }
      | _ -> raise (Invalid_argument "Scope.exit")

   let setCurrentType (t:t) (typ:VType.t) (single:bool) : t =
      { t with typ = typ; single = single }

   let getParent (t:t) : t option =
      match t.parent with
      | None -> None
      | Some(parent) ->
         match t.kind with
         | ModuleSymbol ->
            Some(primExitModule parent t)
         | FunctionSymbol ->
            Some(primExitFunction parent t)
         | MemSymbol | InstanceSymbol ->
            Some(primExitMemInst parent t)
         | VarSymbol -> Some(parent)
         | OperatorSymbol ->
            Some(primExitOperators parent t)
         | TypeSymbol ->
            Some(primExitTypes parent t)
         | _ -> failwith "The type is undefined"

   let getPath (t:t) : id =
      let rec parentPath (parent:t option) : id =
         match parent with
         | None -> []
         | Some(p) -> p.name @ parentPath p.parent
      in
      t.name @ (parentPath t.parent)
      |> List.rev

   let getPathAndType (t:t) : path * VType.t * t =
      let typ = if t.single then t.typ else VType.newinst t.typ in
      Path(getPath t), typ, t

   let rec findAny (find_up:bool) (find:t -> id -> t option) (t:t) (name:id) : t option =
      match name with
      | [] -> Some(t)
      | h::rest ->
         match find t [h] with
         | Some(found) ->
            findAny find_up find found rest
         | None ->
            if find_up then
               match getParent t with
               | Some(parent) ->
                  findAny find_up find parent name
               | None -> None
            else None

   let rec lookupVal (t:t) (name:id) : t option =
      match name with
      | [] -> Some(t)
      | h::rest ->
         let rec inner_loop l =
            match l with
            | []   -> None
            | local::locals ->
               match IdMap.find [h] local with
               | found ->
                  lookupVal found rest
               | exception Not_found ->
                  inner_loop locals
         in inner_loop t.locals

   let getAllWithSameContext (t:t) : t list =
      let contexts =
         match t.parent with
         | None -> []
         | Some(parent) -> Context.getAllWithContext parent.ctx t.name
      in
      match t.parent with
      | Some(parent) ->
         List.fold_left (fun s a -> try (IdMap.find a parent.func) :: s with | _ -> s) [t] contexts
      | _ -> [t]

   let getFunctionMemInst (t:t) : (path * VType.t * t) list * (path * VType.t * t) list =
      let tables = getAllWithSameContext t in
      let mem_inst =
         List.map (fun t -> t.mem_inst) tables
         |> List.map IdMap.to_list
         |> List.flatten
         |> List.map snd
      in
      let mem, inst = List.partition (fun a -> a.kind = MemSymbol) mem_inst in
      List.map getPathAndType mem, List.map getPathAndType inst

   let lookupMemInAllContext (t:t) (name:id) : t option =
      let tables = getAllWithSameContext t in
      let rec loop ctx =
         match ctx with
         | [] -> None
         | h::tt ->
            match findAny false findMemInst h name with
            | Some(_) as a -> a
            | None -> loop tt
      in loop tables

   let lookupVariable (t:t) (name:id) : t option =
      match lookupMemInAllContext t name with
      | Some(_) as a -> a
      | None -> lookupVal t name

   let lookup kind (t:t) (name:id) : t option =
      match kind with
      | `Function -> findAny true findFunction t name
      | `Module   -> findAny true findModule t name
      | `Operator -> findAny true findOperator t name
      | `Type     -> findAny true findType t name
      | `Variable -> lookupVariable t name

   let isMemOrInstance (t:t) (name:id) : bool =
      match lookupMemInAllContext t name with
      | Some(_) -> true
      | None    -> false

   let isActive (t:t) : bool =
      let tables = getAllWithSameContext t in
      List.exists (fun a -> a.active) tables

end

module Env = struct

   type 'a t =
      {
         data    : 'a;
         scope   : Scope.t;
         tick    : int;

      }

   (** Prints all the information of the current environment *)
   let show (state:'a t) : string=
      Scope.show state.scope

   let show_full (state:'a t) : string =
      Scope.show_full state.scope

   (** Gets a new tick (integer value) and updates the state *)
   let tick (state:'a t) : int * 'a t =
      state.tick,{ state with tick = state.tick+1 }

   (** Adds a mem variable to the current context *)
   let addMem (state:'a t) (name:id) (typ:VType.t) : 'a t  =
      {
         state with
         scope   = Scope.addMem state.scope name typ;
      }

   (** Adds a variable to the current block *)
   let addVar (state:'a t) (name:id) (typ:VType.t) : 'a t  =
      {
         state with
         scope   = Scope.addVar state.scope name typ;
      }

   (** Adds an instance to the current block *)
   let addInstance (state:'a t) (name:id) (typ:VType.t) : 'a t  =
      {
         state with
         scope   = Scope.addInstance state.scope name typ;
      }

   (** Returns the full path of a function. Raises an error if it cannot be found *)
   let lookup kind (state:'a t) (name:id) : path * VType.t * Scope.t =
      match Scope.lookup kind state.scope name with
      | None ->
         failwith (Printf.sprintf "Cannot find symbol '%s'" (idStr name))
      | Some(t) -> Scope.getPathAndType t

      (** Returns the mem and instances for a function *)
   let getMemAndInstances (state:'a t) (name:id) : IdTypeSet.t * IdTypeSet.t =
      match Scope.lookup `Function state.scope name with
      | None -> IdTypeSet.empty, IdTypeSet.empty
      | Some(t) ->
         let mem, inst = Scope.getFunctionMemInst t in
         let f s = List.map (fun (k,v,_) -> pathLast k, v) s |> IdTypeSet.of_list in
         f mem, f inst

   (** Returns the generated context name for the given function *)
   let getContext (state:'a t) (name:id) : path =
      match Scope.lookup `Function state.scope name with
      | None -> failwith "Function not found"
      | Some(t) -> Scope.getContext t

   (** Returns the initialization function if it has beed defines with the attribute *)
   let getInitFunction (state:'a t) (name:id) : id option =
      match Scope.lookup `Function state.scope name with
      | None -> failwith "Function not found"
      | Some(t) -> Scope.getInitFunction t name

   (** Returns true if the function is active *)
   let isActive (state:'a t) (name:id) : bool =
      match Scope.lookup `Function state.scope name with
      | None -> false
      | Some(t) ->
         Scope.isActive t

   (** Returns true if the id is a mem or an instance *)
   let isLocalInstanceOrMem (state:'a t) (name:id) : bool =
      Scope.isMemOrInstance state.scope name

   (** Generates a new name for an instance based on the tick *)
   let generateInstanceName (state:'a t) (name_opt:id option) : 'a t * id =
      match name_opt with
      | Some(name) -> state,name
      | None ->
         let tick, state' = tick state in
         let id = ["$fun_"^(string_of_int tick)] in
         state', id

   (** Returns the current location *)
   let currentScope (state:'a t) : path =
      Scope.current state.scope

   let setCurrentType (state:'a t) (typ:VType.t) (single:bool) : 'a t =
      {
         state with
         scope = Scope.setCurrentType state.scope typ single;
      }

   let enter kind ?(attr=emptyAttr) (state:'a t) (func:id) : 'a t =
      {
         state with
         scope = Scope.enter kind attr state.scope func;
         tick  = if (kind = `Function && attr.fun_and = false) then 0 else state.tick;
      }

   (** Closes the current context *)
   let exit kind (state:'a t) : 'a t  =
      {
         state with
         scope = Scope.exit kind state.scope;
      }

   let addBuiltinFunction (state:'a t) (name,kind,typ,single) : 'a t =
      let state' = enter kind state name in
      let state' = setCurrentType state' typ single in
      let state' = exit kind state' in
      state'

   (** Adds the builtin functions to the given context *)
   let initialize (s:'a t) : 'a t =
      List.fold_left (fun s a -> addBuiltinFunction s a) s builtin_table

   (** Creates an empty module context *)
   let empty data : 'a t =
      {
         data    = data;
         tick    = 0;
         scope   = Scope.empty;
      }
      |> initialize

   let get (state:'a t) : 'a =
      state.data

   let set (state:'a t) (data:'a) : 'a t =
      { state with data = data }

   let pathFromCurrent (state:'a t) (path:path) =
      let Path(current) = currentScope state in
      let Path(id) = path in
      let rec loop p1 p2 =
         match p1,p2 with
         | h1::t1, h2::t2 when h1 = h2 -> loop t1 t2
         | _,_ -> p2
      in loop current id

end
