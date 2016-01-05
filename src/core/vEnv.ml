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

let builtin_table =
   [
      ["abs"]  , `Function, Constants.num_num (), false;
      ["exp"]  , `Function, Constants.num_num (), false;
      ["sin"]  , `Function, Constants.num_num (), false;
      ["cos"]  , `Function, Constants.num_num (), false;
      ["floor"], `Function, Constants.num_num (), false;
      ["clip"] , `Function, Constants.num_num_num_num (), false;
      ["not"]  , `Function, Constants.bool_bool (), false;
      ["tanh"] , `Function, Constants.num_num (), false;
      ["tan"]  , `Function, Constants.num_num (), false;
      ["sqrt"] , `Function, Constants.num_num (), false;

      ["int"]  , `Function, Constants.num_int (), false;
      ["real"] , `Function, Constants.num_real (), false;

      ["|-|"] , `Operator, Constants.num_num (), false;
      ["+"]  , `Operator, Constants.num_num_num (), false;
      ["-"]  , `Operator, Constants.num_num_num (), false;
      ["*"]  , `Operator, Constants.num_num_num (), false;
      ["/"]  , `Operator, Constants.num_num_num (), false;
      ["%"]  , `Operator, Constants.num_num_num (), false;

      [">"]   , `Operator, Constants.num_num_bool (), false;
      ["<"]   , `Operator, Constants.num_num_bool (), false;
      ["=="]  , `Operator, Constants.num_num_bool (), false;
      ["<>"]  , `Operator, Constants.num_num_bool (), false;
      [">="]  , `Operator, Constants.num_num_bool (), false;
      ["<="]  , `Operator, Constants.num_num_bool (), false;

      ["||"]  , `Operator, Constants.bool_bool_bool (), false;
      ["&&"]  , `Operator, Constants.bool_bool_bool (), false;
   ]

let builtin_functions = List.map (fun (a,_,_,_)->a) builtin_table |> IdSet.of_list

(** Makes a copy of a type (a new instance) *)
let newinstance (t:vtype) : vtype =
   let rec copy table t =
      try (List.find (fun (key,_) -> key == t) table |> snd),table with | Not_found ->
         match !t with
         | TUnbound(s,level,loc) ->
            let o = ref (TUnbound(s,level,loc)) in
            o, ((t,o) :: table)
         | TId(id,loc) ->
            let o = ref (TId(id,loc)) in
            o, (t,o) :: table
         | TComposed(id,elems,loc) ->
            let elems', table' = copyList table elems in
            let o = ref (TComposed(id,elems',loc)) in
            o, (t,o) :: table'
         | TArrow(t1,t2,loc) ->
            let t1', table' = copy table t1 in
            let t2', table' = copy table' t2 in
            let o = ref (TArrow(t1',t2',loc)) in
            o, (t,o) :: table'
         | TLink(link) ->
            let link', table' = copy table link in
            let o = ref (TLink(link')) in
            o, (t,o) :: table'
         | TExpAlt(elems) ->
            let elems', table' = copyList table elems in
            let o = ref (TExpAlt(elems')) in
            o, (t,o) :: table'
   and copyList table l =
      let l',table' =
         List.fold_left
            (fun (ol,table) t -> let o, table' = copy table t in o :: ol,table')
            ([],table) l
      in
      List.rev l', table'
   in copy [] t |> fst

(** Maps which function belongs to which context *)
module Context = struct

   type t =
      {
         forward   : id IdMap.t;
         backward  : id list IdMap.t;
         count     : int;
         current   : id;
      }

   let empty =
      {
         forward   = IdMap.empty;
         backward  = IdMap.empty;
         count     = 0;
         current   = [];
      }

   let addTo (context:t) (func:id) : t =
      let current_in_ctx =
         try IdMap.find context.current context.backward with
         | Not_found -> []
      in
      {
         context with
         forward  = IdMap.add func context.current context.forward;
         backward = IdMap.add context.current (func::current_in_ctx) context.backward;
      }

   let makeNew (context:t) (func:id) : t =
      let context_name = ["ctx_"^(string_of_int context.count)] in
      {
         count    = context.count+1;
         current  = context_name;
         forward  = IdMap.add func context_name context.forward;
         backward = IdMap.add context_name [func] context.backward;
      }

end

type symbol_kind =
   | MemSymbol
   | VarSymbol
   | InstanceSymbol
   | FunctionSymbol
   | ModuleSymbol

let kindStr = function
   | MemSymbol      -> "mem"
   | VarSymbol      -> "var"
   | InstanceSymbol -> "instance"
   | FunctionSymbol -> "function"
   | ModuleSymbol   -> "module"

(** Used to track the location while traversing and also to lookup function, mem, and module *)
module Scope = struct

   type t =
      {
         name      : id;             (** Name of the current scope *)
         kind      : symbol_kind;    (** Type of the current scope *)
         parent    : t option;       (** Pointer to it's parent *)
         typ       : vtype;          (** Type of the symbol *)

         operators : t IdMap.t;      (** Operators *)
         modules   : t IdMap.t;      (** Modules or namespaces *)
         types     : t IdMap.t;      (** Types *)
         func      : t IdMap.t;      (** Functions *)
         mem_inst  : t IdMap.t;      (** Mem and instances *)
         locals    : t IdMap.t list; (** Variables and subscopes *)

         ctx       : Context.t;

         single    : bool;           (** true if every function call does not create a new instance *)

      }

   let empty : t =
      {
         name      = [];
         kind      = ModuleSymbol;
         parent    = None;
         operators = IdMap.empty;
         modules   = IdMap.empty;
         types     = IdMap.empty;
         func      = IdMap.empty;
         mem_inst  = IdMap.empty;
         locals    = [];
         typ       = ref (TId([""],None));
         ctx       = Context.empty;
         single    = true;

      }

   let getFunction (t:t) : t IdMap.t =
      t.func

   let primExitFunction (parent:t) (t:t) : t =
      { parent with func = IdMap.add t.name t parent.func }

   let getModule (t:t) : t IdMap.t =
      t.modules

   let primExitModule (parent:t) (t:t) : t =
      { parent with modules = IdMap.add t.name t parent.modules }

   let getMemInst (t:t) : t IdMap.t =
      t.mem_inst

   let primExitMemInst (parent:t) (t:t) : t =
      { parent with mem_inst = IdMap.add t.name t parent.mem_inst }

   let getTypes (t:t) : t IdMap.t =
      t.types

   let primExitTypes (parent:t) (t:t) : t =
      { parent with types = IdMap.add t.name t parent.types }

   let getOperators (t:t) : t IdMap.t =
      t.operators

   let primExitOperators (parent:t) (t:t) : t =
      { parent with operators = IdMap.add t.name t parent.operators }

   (*let rec dump (t:t) (level:int) : unit =
      Printf.printf "%s'%s' = %s\n" (String.make level ' ') (idStr t.name) (kindStr t.kind);
      IdMap.iter (fun _ sub -> dump sub (level+3)) t.keep*)

   let addMem (t:t) (name:id) (typ:vtype) : t =
      let new_symbol = { empty with name = name; kind = MemSymbol; typ = typ } in
      { t with mem_inst = IdMap.add name new_symbol t.mem_inst }

   let addInstance (t:t) (name:id) (typ:vtype) : t =
      let new_symbol = { empty with name = name; kind = InstanceSymbol; typ = typ } in
      { t with mem_inst = IdMap.add name new_symbol t.mem_inst }

   let addVar (t:t) (name:id) (typ:vtype) : t =
      let new_symbol = { empty with name = name; kind = VarSymbol; typ = typ  } in
      let first,rest =
         match t.locals with
         | [] -> IdMap.empty,[]
         | h::t -> h,t
      in
      { t with locals = (IdMap.add name new_symbol first) :: rest }

   let enterAny (get:t -> t IdMap.t) (t:t) (name:id) : t =
      match IdMap.find name (get t) with
      | sub ->
         { sub with parent = Some(t) }
      | exception Not_found ->
         { empty with parent = Some(t); name = name; }

   let exitAny (exit:t -> t -> t) (t:t) : t =
      match t.parent with
      | None -> failwith "Scope.exit: Cannot exit more scopes"
      | Some(parent) ->
         exit parent t

   let newContext (t:t) (name:id) : t =
      { t with ctx = Context.makeNew t.ctx name }

   let addToContext (t:t) (name:id) : t =
      { t with ctx = Context.makeNew t.ctx name }

   let enter kind ?(sharectx=false) (t:t) (name:id) : t =
      match kind with
      | `Function ->
         let t' = enterAny getFunction t name in
         if sharectx then
            addToContext t' name
         else
            newContext t' name
      | `Module   -> enterAny getModule t name
      | `Operator -> enterAny getOperators t name
      | `Block    -> { t with locals = IdMap.empty :: t.locals }
      | _ -> raise (Invalid_argument "Scope.enter")

   let exit kind (t:t) : t =
      match kind with
      | `Function -> exitAny primExitFunction t
      | `Module   -> exitAny primExitModule t
      | `Operator -> exitAny primExitOperators t
      | `Block    -> { t with locals = List.tl t.locals }
      | _ -> raise (Invalid_argument "Scope.exit")

   let setCurrentType (t:t) (typ:vtype) (single:bool) : t =
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

   let current (t:t) : path =
      let rec parentName parent =
         match parent with
         | None -> []
         | Some(parent_t) ->
            parent_t.name :: parentName parent_t.parent
      in
      t.name :: parentName t.parent
      |> List.rev |> List.flatten |> fun a -> Path(a)

   let getPath (t:t) : id =
      let rec parentPath (parent:t option) : id =
         match parent with
         | None -> []
         | Some(p) -> p.name @ parentPath p.parent
      in
      t.name @ (parentPath t.parent)
      |> List.rev

   let rec findAny (find_up:bool) (get:t -> t IdMap.t) (t:t) (name:id) : (path * vtype) option =
      match name with
      | [] ->
         let typ = if t.single then t.typ else newinstance t.typ in
         Some(Path(getPath t), typ)
      | h::rest ->
         match IdMap.find [h] (get t) with
         | found ->
            findAny find_up get found rest
         | exception Not_found ->
            if find_up then
               match getParent t with
               | Some(parent) ->
                  findAny find_up get parent name
               | None -> None
            else None

   let rec lookupVal (t:t) (name:id) : (path * vtype) option =
      match name with
      | [] -> Some(Path(getPath t), t.typ)
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

   let lookupVariable (t:t) (name:id) : (path * vtype) option =
      match findAny false getMemInst t name with
      | Some(_) as a -> a
      | None -> lookupVal t name

   let lookup kind (t:t) (name:id) : (path * vtype) option =
      match kind with
      | `Function -> findAny true getFunction t name
      | `Module   -> findAny true getModule t name
      | `Operator -> findAny true getOperators t name
      | `Type     -> findAny true getTypes t name
      | `Variable -> lookupVariable t name

   let isMemOrInstance (t:t) (name:id) : bool =
      match findAny false getMemInst t name with
      | Some(_) -> true
      | None    -> false

end

(** Maps which function belongs to which context *)
module FunctionContex = struct

   type t =
      {
         forward   : id PathMap.t;
         backward  : path list IdMap.t;
         mem       : IdTypeSet.t IdMap.t;
         instance  : IdTypeSet.t IdMap.t;
         count     : int;
         current   : id;
      }

   let empty =
      {
         forward   = PathMap.empty;
         backward  = IdMap.empty;
         mem       = IdMap.empty;
         instance  = IdMap.empty;
         count     = 0;
         current   = [];
      }

   (** Returns the context type of a given function *)
   let findContext (context:t) (func:path) : id =
      try PathMap.find func context.forward with
      | Not_found -> failwith (Printf.sprintf "Cannot find context for function '%s'" (pathStr func))

   (** Returns the instances for the given context *)
   let getInstancesForContext (context:t) (name:id) : IdTypeSet.t =
      try IdMap.find name context.instance with
      | Not_found -> IdTypeSet.empty

   (** Returns the mem for the given context *)
   let getMemForContext (context:t) (name:id) : IdTypeSet.t =
      try IdMap.find name context.mem with
      | Not_found ->
         IdTypeSet.empty

   let addTo (context:t) (func:path) : t =
      let current_in_ctx =
         try IdMap.find context.current context.backward with
         | Not_found -> []
      in
      {
         context with
         forward  = PathMap.add func context.current context.forward;
         backward = IdMap.add context.current (func::current_in_ctx) context.backward;
      }

   let makeNew (context:t) (func:path) : t =
      let context_name = ["ctx_"^(string_of_int context.count)] in
      {
         context with
         count    = context.count+1;
         current  = context_name;
         forward  = PathMap.add func context_name context.forward;
         backward = IdMap.add context_name [func] context.backward;
      }

   let addMem (context:t) (func:path) (name:id) (typ:vtype) : t =
      let context_for_func = findContext context func in
      let mem_for_context  =
         try IdMap.find context_for_func context.mem with
         | Not_found -> IdTypeSet.empty
      in
      {
         context with
         mem = IdMap.add context_for_func (IdTypeSet.add (name,typ) mem_for_context) context.mem;
      }

   let addInstance (context:t) (func:path) (name:id) (kind:path) : t =
      let context_for_func = findContext context func in
      let instance_for_context  =
         try IdMap.find context_for_func context.instance with
         | Not_found -> IdTypeSet.empty
      in
      let instance_type = ref (TId(pathId kind,None)) in
      {
         context with
         instance = IdMap.add context_for_func (IdTypeSet.add (name,instance_type) instance_for_context) context.instance;
      }

   let isBuiltinPath (name:path) : bool =
      IdSet.mem (pathId name) builtin_functions

   let isActive (context:t) (name:path) : bool =
      if isBuiltinPath name then
         false
      else
         let fun_context = findContext context name in
         let is_active   = not (
            IdTypeSet.is_empty (getInstancesForContext context fun_context) &&
               IdTypeSet.is_empty (getMemForContext context fun_context))
         in
         is_active

   let dump (context:t) : unit =
      Printf.printf "Current = '%s'\n" (idStr context.current);
      print_endline "Forward";
      PathMap.iter
         (fun key value -> Printf.printf "   '%s' -> '%s'\n" (pathStr key) (idStr value))
         context.forward;
      print_endline "Backward";
      IdMap.iter
         (fun key value ->
            Printf.printf "   '%s' <- " (idStr key);
            List.iter (fun v -> Printf.printf "'%s' " (pathStr v)) value;
            print_newline ())
         context.backward;
      print_endline "Mem";
      IdMap.iter
         (fun key value ->
            Printf.printf "   '%s' = " (idStr key);
            IdTypeSet.iter (fun (v,tp) -> Printf.printf "'%s:%s' " (idStr v) (PrintTypes.typeStr tp)) value;
            print_newline ())
         context.mem;
      print_endline "Instance";
      IdMap.iter
         (fun key value -> Printf.printf "   '%s' = " (idStr key);
            IdTypeSet.iter (fun (name,kind) -> Printf.printf "'%s:%s' " (idStr name) (PrintTypes.typeStr kind)) value;
            print_newline ())
         context.instance;
      print_endline "Active";
      PathMap.iter
         (fun key _ -> Printf.printf "   '%s' active = %s\n" (pathStr key) (if isActive context key then "true" else "false") )
         context.forward


end

module Env = struct

   type 'a t =
      {
         data    : 'a;
         context : FunctionContex.t;
         scope   : Scope.t;
         tick    : int;

      }

   (** Prints all the information of the current environment *)
   let dump (state:'a t) =
      FunctionContex.dump state.context;
      print_endline "Scope"
      (*Scope.dump state.scope 3*)

   (** Gets a new tick (integer value) and updates the state *)
   let tick (state:'a t) : int * 'a t =
      state.tick,{ state with tick = state.tick+1 }

   (** Creates a new context to store functions and mem definitions *)
   let makeNewContext (state:'a t) : 'a t  =
      {
         state with
         context = FunctionContex.makeNew state.context (Scope.current state.scope)
      }

   (** Adds a function definition to the current context *)
   let includeFunctionInContext (state:'a t) : 'a t  =
      {
         state with
         context = FunctionContex.addTo state.context (Scope.current state.scope)
      }

   (** Creates a new context if the fun_and attribute is true, otherwise adds to the current *)
   let prepareContext (fun_and:bool) (state:'a t) : 'a t =
      if fun_and then
         includeFunctionInContext state
      else
         makeNewContext state

   (** Adds a mem variable to the current context *)
   let addMem (state:'a t) (name:id) (typ:vtype) : 'a t  =
      {
         state with
         context = FunctionContex.addMem state.context (Scope.current state.scope) name typ;
         scope   = Scope.addMem state.scope name typ;
      }

   (** Adds a variable to the current block *)
   let addVar (state:'a t) (name:id) (typ:vtype) : 'a t  =
      {
         state with
         scope   = Scope.addVar state.scope name typ;
      }

   (** Returns the full path of a function. Raises an error if it cannot be found *)
   let lookup kind (state:'a t) (name:id) : path * vtype =
      match Scope.lookup kind state.scope name with
      | None -> failwith (Printf.sprintf "Cannot find symbol '%s'" (idStr name))
      | Some(path) -> path


      (** Returns the mem and instances for a function *)
   let getMemAndInstances (state:'a t) (name:id) : IdTypeSet.t * IdTypeSet.t =
      let path,_   = lookup `Function state name in
      let ctx      = FunctionContex.findContext state.context path in
      let mem      = FunctionContex.getMemForContext state.context ctx in
      let instance = FunctionContex.getInstancesForContext state.context ctx in
      mem, instance

   (** Returns the generated context name for the given function *)
   let getContext (state:'a t) (name:id) : id =
      let path,_ = lookup `Function state name in
      FunctionContex.findContext state.context path

   (** Returns true if the function is active *)
   let isActive (state:'a t) (name:id) : bool =
      let path,_ = lookup `Function state name in
      FunctionContex.isActive state.context path

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

   (** Adds a new instance to the context if the function is active *)
   let addInstanceToContext (state:'a t) (name_opt:id option) (kind:id) : 'a t * id option  =
      let kind_path_opt = Scope.lookup `Function state.scope kind in
      let kind_path,_ =
         match kind_path_opt with
         | Some(path) -> path
         | None -> failwith "Cannot find function"
      in
      let kind_type =
         match kind_path with
         | Path(p) -> ref (TId(p,None))
      in
      if isActive state kind then
         let state', name' = generateInstanceName state name_opt in
         {
            state' with
            context = FunctionContex.addInstance state'.context (Scope.current state.scope) name' kind_path;
            scope   = Scope.addInstance state.scope name' kind_type;
         }, Some(name')
      else
         state, None

   (** Returns the current location *)
   let currentScope (state:'a t) : path =
      Scope.current state.scope

   let setCurrentType (state:'a t) (typ:vtype) (single:bool) : 'a t =
      {
         state with
         scope = Scope.setCurrentType state.scope typ single;
      }

   let enter kind (state:'a t) (func:id) : 'a t =
      {
         state with
         scope = Scope.enter kind state.scope func;
         tick  = 0;
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
   let empty (init:id) data : 'a t =
      {
         data    = data;
         context = FunctionContex.empty;
         tick    = 0;
         scope   = Scope.empty;
      }
      |> initialize
      |> fun s -> enter `Module s init

   let get (state:'a t) : 'a =
      state.data

   let set (state:'a t) (data:'a) : 'a t =
      { state with data = data }

end
