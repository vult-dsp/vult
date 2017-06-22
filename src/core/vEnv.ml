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

let idStr = PrintTypes.identifierStr

let pathStr path = path |> pathId |> PrintTypes.identifierStr


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


(** Used to track the location while traversing and also to lookup function, mem, and module *)
module Scope = struct

   type kind =
      | Function
      | Module
      | Operator
      | Type
   [@@deriving show]

   type var =
      {
         name      : id;
         typ       : VType.t;
         loc       : Loc.t;
         is_inst   : bool;
      }

   type t =
      {
         name      : id;                   (** Name of the current scope *)
         kind      : kind;                 (** Type of the current scope *)
         parent    : t option ref;         (** Pointer to it's parent *)
         typ       : VType.t ref;          (** Type of the symbol *)

         (* Sub elements *)
         operators : t IdMap.t ref;        (** Operators *)
         modules   : t IdMap.t ref;        (** Modules or namespaces *)
         types     : t IdMap.t ref;        (** Types *)
         func      : t IdMap.t ref;        (** Functions *)
         mem_inst  : var IdMap.t ref;      (** Mem and instances *)
         locals    : var IdMap.t list ref; (** Variables and subscopes *)

         ctx       : Context.t ref;        (** keeps track of which functions belong to the same context *)

         single    : bool ref;             (** true if every function call does not create a new instance of the type *)

         active    : bool ref;             (** true if the fuction contains a mem or an instance *)

         ext_fn    : string option ref;    (** contains the replacement name if it's an external function *)

         loc       : Loc.t ref;

         tick      : int ref;
         in_if     : bool ref; (** set to true by the mapper when inside an if-expression *)

      }

   let kindStr kind : string =
      match kind with
      | Function -> "function"
      | Module   -> "module"
      | Operator -> "operator"
      | Type     -> "type"

   let create (kind:kind) tick : t =
      {
         name      = [];
         kind      = kind;
         parent    = ref None;
         operators = ref IdMap.empty;
         modules   = ref IdMap.empty;
         types     = ref IdMap.empty;
         func      = ref IdMap.empty;
         mem_inst  = ref IdMap.empty;
         locals    = ref [];
         typ       = ref VType.Const.empty;
         ctx       = ref Context.empty;
         single    = ref true;
         active    = ref false;
         ext_fn    = ref None;
         loc       = ref Loc.default;
         tick      = tick;
         in_if     = ref false;

      }

   let tick (t:t) : int * t =
      let n = !(t.tick) in
      incr t.tick;
      n,t

   let findFunction (t:t) (name:id) : t option =
      try Some(IdMap.find name !(t.func)) with
      | _ ->
         try Some(IdMap.find name !(t.modules)) with
         | _ -> None

   let findModule (t:t) (name:id) : t option =
      try Some(IdMap.find name !(t.modules)) with
      | _ -> None

   let findMemInst (t:t) (name:id) : var option =
      try Some(IdMap.find name !(t.mem_inst)) with
      | _ -> None

   let findType (t:t) (name:id) : t option =
      try Some(IdMap.find name !(t.types)) with
      | _ ->
         try Some(IdMap.find name !(t.modules)) with
         | _ -> None

   let findOperator (t:t) (name:id) : t option =
      try Some(IdMap.find name !(t.operators)) with
      | _ -> None

   let getTable (t:t) (kind:kind) : t IdMap.t =
      match kind with
      | Function -> !(t.func)
      | Module   -> !(t.modules)
      | Operator -> !(t.operators)
      | Type     -> !(t.types)

   let setOptLoc (opt_loc:Loc.t option) (t:t) : t =
      match opt_loc with
      | Some(loc) ->
         t.loc := loc;
         t
      | None -> t

   let setOptType (opt_typ:VType.t option) (t:t) : t =
      match opt_typ with
      | Some(typ) ->
         t.typ := typ;
         t
      | None -> t

   let findOrCreate (t:t) (typ:VType.t option) (loc:Loc.t option) (kind:kind) (name:id) =
      match IdMap.find name (getTable t kind) with
      | found -> found
      | exception Not_found ->
         { (create kind t.tick) with name = name } |> setOptLoc loc |> setOptType typ

   let enterBlock (t:t) : t =
      t.locals := IdMap.empty :: !(t.locals);
      t

   let enterKind (t:t) ?(typ:VType.t option) ?(loc:Loc.t option) (kind:kind) (name:id) =
      let sub = findOrCreate t typ loc kind name in
      sub.parent := Some(t);
      sub

   let exit (t:t) : t =
      match !(t.parent) with
      | None -> failwith "Scope.exit: cannot exit the top scope"
      | Some(parent) ->
         match t.kind with
         | Function ->
            parent.func := IdMap.add t.name t !(parent.func);
            parent
         | Module   ->
            parent.modules := IdMap.add t.name t !(parent.modules);
            parent
         | Operator ->
            parent.operators := IdMap.add t.name t !(parent.operators);
            parent
         | Type     ->
            parent.types := IdMap.add t.name t !(parent.types);
            parent

   let newContext (parent:t) (name:id) (is_init:bool) : t =
      parent.ctx := Context.makeNew !(parent.ctx) name is_init;
      parent

   let addToContext (parent:t) (name:id) (is_init:bool) : t =
      parent.ctx := Context.addTo !(parent.ctx) name is_init;
      parent

   let current (t:t) : path =
      let rec parentName parent =
         match parent with
         | None -> []
         | Some(parent_t) ->
            parent_t.name :: parentName !(parent_t.parent)
      in
      t.name :: parentName !(t.parent)
      |> List.rev |> List.flatten |> fun a -> Path(a)

   let enter (kind:kind) (t:t) (name:id) (attr:attr) : t =
      enterKind t ~loc:attr.loc kind name

   let exitBlock (t:t) : t =
      t.locals := List.tl !(t.locals);
      t

   let setCurrentType (t:t) (typ:VType.t) (single:bool) : t =
      t.typ := typ;
      t.single := single;
      t

   let addBuiltin (t:t) (kind:kind) (name:id) (typ:VType.t) (single:bool) =
      let t' = enter kind t name emptyAttr in
      let t' = setCurrentType t' typ single in
      exit t'

   let getPath (t:t) : id =
      let rec parentPath (parent:t option) : id =
         match parent with
         | None -> []
         | Some(p) -> p.name @ parentPath !(p.parent)
      in
      t.name @ (parentPath !(t.parent))
      |> List.rev

   let getPathAndType (t:t) : path * VType.t * t =
      let typ = if !(t.single) then !(t.typ) else VType.newinst !(t.typ) in
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
               match !(t.parent) with
               | Some(_) ->
                  findAny find_up find (exit t) name
               | None -> None
            else None

   let rec lookupVal (t:t) (name:id) : var option =
      let rec inner_loop l =
         match l with
         | []   -> None
         | local::locals ->
            match IdMap.find name local with
            | found -> Some(found)
            | exception Not_found ->
               inner_loop locals
      in inner_loop !(t.locals)

   (** Returns all context (functions) within the same context as the current *)
   let getAllWithSameContext (t:t) : t list =
      let contexts =
         match !(t.parent) with
         | None -> []
         | Some(parent) -> Context.getAllWithContext !(parent.ctx) t.name
      in
      match !(t.parent) with
      | Some(parent) ->
         List.fold_left (fun s a -> try (IdMap.find a !(parent.func)) :: s with | _ -> s) [t] contexts
      | _ -> [t]

   (** Search for any mem or instance with the given name.
       Note: it looks in all functions with the same context.
   *)
   let lookupMemInAllContext (t:t) (name:id) : var option =
      let tables = getAllWithSameContext t in
      let rec loop ctx =
         match ctx with
         | [] -> None
         | h::tt ->
            match findMemInst h name with
            | Some(_) as a -> a
            | None -> loop tt
      in loop tables

   let lookupVariable (t:t) (name:id) : var option =
      match lookupMemInAllContext t name with
      | Some(_) as a -> a
      | None -> lookupVal t name

   let lookupScope (kind:kind) (t:t) (name:id) : t option =
      match kind with
      | Function -> findAny true findFunction t name
      | Module   -> findAny true findModule t name
      | Operator -> findAny true findOperator t name
      | Type     -> findAny true findType t name

   let lookup kind (t:t) (name:id) : (path * VType.t * t) option =
      match lookupScope kind t name with
      | Some(lt) -> Some(getPathAndType lt)
      | None -> None

   let lookupRaise kind (t:t) (name:id) (loc:Loc.t) : path * VType.t * t =
      match lookup kind t name with
      | Some(a) -> a
      | None ->
         Printf.printf "Unknown %s '%s'" (kindStr kind) (idStr name);
         Error.raiseError (Printf.sprintf "Unknown %s '%s'" (kindStr kind) (idStr name)) loc

   let lookupVariableRaise (t:t) (name:id) (loc:Loc.t) : var =
      match lookupVariable  t name with
      | Some(a) -> a
      | None ->
         Printf.printf "Unknown symbol '%s'" (idStr name);
         Error.raiseError (Printf.sprintf "Unknown symbol '%s'" (idStr name)) loc


   let isMemOrInstance (t:t) (name:id) : bool =
      match lookupMemInAllContext t name with
      | Some(_) -> true
      | None    -> false

   (** Adds a new instance to the given scope *)
   let addMem (t:t) (name:id) (typ:VType.t) (loc:Loc.t) : t =
      match lookupVal t name with
      | None ->
         let new_symbol = { name = name; typ = typ; loc = loc; is_inst = false } in
         t.mem_inst := IdMap.add name new_symbol !(t.mem_inst);
         t.active := true;
         t
      | Some(decl) ->
         let msg =
            Printf.sprintf
               "Redefinition of variable '%s'. Previously defined at %s" (idStr name) (Loc.to_string_readable decl.loc)
         in
         Error.raiseError msg loc

   (** Adds a new mem variable to the given scope *)
   let addInstance (t:t) (name:id) (typ:VType.t) (loc:Loc.t) : t =
      let new_symbol = { name = name; typ = typ; loc = loc; is_inst = true } in
      t.mem_inst := IdMap.add name new_symbol !(t.mem_inst);
      t.active := true;
      t

   (** Adds a variable to the given context *)
   let addVar (t:t) (name:id) (typ:VType.t) (loc:Loc.t) : t =
      (* Check the if the variable exitst *)
      match lookupVariable t name with
      | None ->
         (* The variable does not exits, add it *)
         let new_symbol = { name = name; typ = typ; loc = loc; is_inst = false } in
         let first,rest =
            match !(t.locals) with
            | [] -> IdMap.empty,[]
            | h::t -> h,t
         in
         t.locals := (IdMap.add name new_symbol first) :: rest;
         t
      | Some(decl) ->
         (* If it exitst check the locations *)
         if Loc.isSameLoc loc decl.loc then
            t
         else
            let msg =
               Printf.sprintf
                  "Redefinition of variable '%s'. Previously defined at %s"
                  (idStr name) (Loc.to_string_readable decl.loc)
            in
            Error.raiseError msg loc

   let addFunction (t:t) (name:id) (attr:attr) : t =
      let add_it () =
         let new_symbol =
            let sub = { (create Function t.tick) with name = name} in
            sub.loc := attr.loc;
            sub.ext_fn := attr.ext_fn;
            sub
         in
         let init = Attributes.has attr.exp ["init"] in
         let t' =
            if attr.fun_and then
               addToContext t name init
            else
               newContext t name init
         in
         t'.func := IdMap.add name new_symbol !(t.func);
         t'
      in

      match lookup Function t name with
      | None -> add_it ()

      | Some(path,_,decl) ->
         let current_path = pathAppend (current t) name in
         if path <> current_path then
            add_it ()
            (* If it exitst check the locations *)
         else if Loc.isSameLoc attr.loc !(decl.loc) && current_path = path then
            t
         else
            let msg =
               Printf.sprintf
                  "Redefinition of function '%s'. Previously defined at %s" (idStr name) (Loc.to_string_readable !(decl.loc))
            in
            Error.raiseError msg attr.loc


   (** Returns all mem and instances of the given scope, assuming is a function *)
   let getFunctionMemInst (t:t) : var list =
      getAllWithSameContext t
      |> List.map (fun a -> !(a.mem_inst))
      |> List.map IdMap.to_list
      |> List.flatten
      |> List.map snd

   (** Lookup the function and returns a set containing all the
       instances and mem of all functions in the context *)
   let getFunctionMemInstSet (t:t) (name:id) : IdTypeSet.t =
      match lookup Function t name with
      | None -> IdTypeSet.empty
      | Some(_,_,s) ->
         getFunctionMemInst s
         |> List.map (fun (a:var) -> a.name, a.typ)
         |> IdTypeSet.of_list

   (** Lookup the function and optionally returns the name of the initilization function *)
   let getInitFunction (t:t) (name:id) : id option =
      match lookup Function t name with
      | None -> None
      | Some(_,_,s) ->
         match !(s.parent) with
         | None -> raise (Invalid_argument "Scope.getInitFunction")
         | Some(parent) ->
            Context.getInitFunction !(parent.ctx) name


   (** Lookup the function and returns the path to the function context *)
   let getContext (t:t) (name:id) : path =
      match lookup Function t name with
      | None -> raise (Invalid_argument "Scope.getContext")
      | Some(_,_,s) ->
         match !(s.parent) with
         | None -> raise (Invalid_argument "Scope.getContext")
         | Some(parent) ->
            let Path(parent_path) = current parent in
            let ctx = Context.getContext !(parent.ctx) s.name in
            Path(parent_path@ctx)

   (** Returns true/false if the given scope is active *)
   let isActive (t:t) : bool =
      let tables = getAllWithSameContext t in
      List.exists (fun a -> !(a.active)) tables

   (** Lookup the function and returns true/false if the function is active *)
   let isActiveFunction (t:t) (name:id) : bool =
      match lookup Function t name with
      | Some(_,_,s) ->
         isActive s
      | None -> false


   let pathFromCurrent (t:t) (path:path) =
      let Path(current) = current t in
      let Path(id) = path in
      let rec loop p1 p2 =
         match p1,p2 with
         | h1::t1, h2::t2 when h1 = h2 -> loop t1 t2
         | _,_ -> p2
      in loop current id

   let enterIf (t:t) : t =
      t.in_if := true;
      t

   let exitIf (t:t) : t =
      t.in_if := false;
      t

   let insideIf (t:t) : bool =
      !(t.in_if)

end


let builtin_table =
   [
      ["int"]  , Scope.Type, VType.Const.type_type, true;
      ["real"] , Scope.Type, VType.Const.type_type, true;
      ["bool"] , Scope.Type, VType.Const.type_type, true;
      ["unit"] , Scope.Type, VType.Const.type_type, true;
      ["string"], Scope.Type, VType.Const.type_type, true;

      ["wrap_array"] , Scope.Function, VType.Const.wrap_array (), true;

      ["set"] ,      Scope.Function, VType.Const.array_set (), false;
      ["get"] ,      Scope.Function, VType.Const.array_get (), false;
      ["size"] ,     Scope.Function, VType.Const.array_size (), false;
      ["makeArray"], Scope.Function, VType.Const.array_make (), false;

      ["abs"]  , Scope.Function, VType.Const.real_real (), false;
      ["exp"]  , Scope.Function, VType.Const.real_real (), false;
      ["sin"]  , Scope.Function, VType.Const.real_real (), false;
      ["cos"]  , Scope.Function, VType.Const.real_real (), false;
      ["floor"], Scope.Function, VType.Const.real_real (), false;
      ["tanh"] , Scope.Function, VType.Const.real_real (), false;
      ["tan"]  , Scope.Function, VType.Const.real_real (), false;
      ["sqrt"] , Scope.Function, VType.Const.real_real (), false;
      ["clip"] , Scope.Function, VType.Const.a_a_a_a (), false;

      ["int"]  , Scope.Function, VType.Const.num_int (), false;
      ["real"] , Scope.Function, VType.Const.num_real (), false;

      ["|-|"] , Scope.Operator, VType.Const.num_num (), false;
      ["+"]  , Scope.Operator, VType.Const.num_num_num (), false;
      ["-"]  , Scope.Operator, VType.Const.num_num_num (), false;
      ["*"]  , Scope.Operator, VType.Const.num_num_num (), false;
      ["/"]  , Scope.Operator, VType.Const.num_num_num (), false;
      ["%"]  , Scope.Operator, VType.Const.num_num_num (), false;

      [">"]   , Scope.Operator, VType.Const.num_num_bool (), false;
      ["<"]   , Scope.Operator, VType.Const.num_num_bool (), false;
      ["=="]  , Scope.Operator, VType.Const.a_a_bool (), false;
      ["<>"]  , Scope.Operator, VType.Const.a_a_bool (), false;
      [">="]  , Scope.Operator, VType.Const.num_num_bool (), false;
      ["<="]  , Scope.Operator, VType.Const.num_num_bool (), false;

      ["not"] , Scope.Function, VType.Const.bool_bool (), false;
      ["||"]  , Scope.Operator, VType.Const.bool_bool_bool (), false;
      ["&&"]  , Scope.Operator, VType.Const.bool_bool_bool (), false;

      ["eps"] , Scope.Function, VType.Const.real_type, false;

      ["random"], Scope.Function, VType.Const.real_type, false;
      ["irandom"], Scope.Function, VType.Const.int_type, false;

      ["log"], Scope.Function, VType.Const.a_a (), false;

   ]

let builtin_functions = List.map (fun (a,_,_,_)-> a ) builtin_table |> IdSet.of_list

module Env = struct

   type 'a t =
      {
         data    : 'a;
         scope   : Scope.t;
      }

   (** Gets a new tick (integer value) and updates the state *)
   let tick (state:'a t) : int * 'a t =
      let tick, scope = Scope.tick state.scope in
      tick, { state with scope = scope }

   let enterIf (state:'a t) : 'a t =
      { state with scope = Scope.enterIf state.scope }

   let exitIf (state:'a t) : 'a t =
      { state with scope = Scope.exitIf state.scope }

   let insideIf (state:'a t) : bool =
      Scope.insideIf state.scope

   (** Adds a mem variable to the current context *)
   let addMem (state:'a t) (name:id) (typ:VType.t) (attr:attr) : 'a t  =
      {
         state with
         scope   = Scope.addMem state.scope name typ attr.loc;
      }

   (** Adds a variable to the current block *)
   let addVar (state:'a t) (name:id) (typ:VType.t) (attr:attr) : 'a t  =
      {
         state with
         scope   = Scope.addVar state.scope name typ attr.loc;
      }

   (** Adds an instance to the current block *)
   let addInstance (state:'a t) (name:id) (typ:VType.t) (attr:attr) : 'a t  =
      {
         state with
         scope   = Scope.addInstance state.scope name typ attr.loc;
      }

   let addFunction (state:'a t) (name:id) (attr:attr) : 'a t  =
      {
         state with
         scope   = Scope.addFunction state.scope name attr;
      }

   (** Returns the full path of a function. *)
   let lookup kind (state:'a t) (name:id) : (path * VType.t * Scope.t) option =
      Scope.lookup kind state.scope name

   (** Returns the full path of a function. Raises an error if it cannot be found *)
   let lookupRaise kind (state:'a t) (name:id) (loc:Loc.t) : path * VType.t * Scope.t =
      Scope.lookupRaise kind state.scope name loc

   (** Looks for a variable in the given scope *)
   let lookupVariable (state:'a t) (name:id) : Scope.var option =
      Scope.lookupVariable state.scope name

   (** Looks for a variable in the given scope. Raises an error if it cannot be found *)
   let lookupVariableRaise (state:'a t) (name:id) (loc:Loc.t) : Scope.var =
      Scope.lookupVariableRaise state.scope name loc

   (** Returns the mem and instances for a function *)
   let getMemAndInstances (state:'a t) (name:id) : IdTypeSet.t =
      Scope.getFunctionMemInstSet state.scope name

   (** Returns the generated context name for the given function *)
   let getContext (state:'a t) (name:id) : path =
      Scope.getContext state.scope name

   (** Returns the initialization function if it has beed defines with the attribute *)
   let getInitFunction (state:'a t) (name:id) : id option =
      Scope.getInitFunction state.scope name

   (** Returns true if the function is active *)
   let isActive (state:'a t) (name:id) : bool =
      Scope.isActiveFunction state.scope name

   (** Returns true if the id is a mem or an instance *)
   let isLocalInstanceOrMem (state:'a t) (name:id) : bool =
      Scope.isMemOrInstance state.scope name

   (** Returns the current location *)
   let currentScope (state:'a t) : path =
      Scope.current state.scope

   let setCurrentType (state:'a t) (typ:VType.t) (single:bool) : 'a t =
      {
         state with
         scope = Scope.setCurrentType state.scope typ single;
      }

   let enter kind (state:'a t) (func:id) attr : 'a t =
      {
         state with
         scope = Scope.enter kind state.scope func attr;
      }

   (** Closes the current context *)
   let exit (state:'a t) : 'a t  =
      {
         state with
         scope = Scope.exit state.scope;
      }

   let enterBlock (state:'a t) : 'a t =
      {
         state with
         scope = Scope.enterBlock state.scope;
      }

   let exitBlock (state:'a t) : 'a t  =
      {
         state with
         scope = Scope.exitBlock state.scope;
      }

   let addBuiltin (state:'a t) (name,kind,typ,single) : 'a t =
      {
         state with
         scope = Scope.addBuiltin state.scope kind name typ single;
      }

   (** Adds the builtin functions to the given context *)
   let initialize (s:'a t) : 'a t =
      List.fold_left (fun s a -> addBuiltin s a) s builtin_table

   (** Creates an empty module context *)
   let empty data : 'a t =
      {
         data    = data;
         scope   = Scope.create Scope.Module (ref 0);
      }
      |> initialize

   let get (state:'a t) : 'a =
      state.data

   let set (state:'a t) (data:'a) : 'a t =
      { state with data = data }

   let pathFromCurrent (state:'a t) (path:path) =
      Scope.pathFromCurrent state.scope path

   let derive (state:'a t) (data:'b) : 'b t =
      { state with data = data }

end
