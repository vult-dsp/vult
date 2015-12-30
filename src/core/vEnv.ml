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

let builtin_functions =
   [
      ["abs"];
      ["exp"];
      ["sin"];
      ["floor"];
      ["clip"];
      ["not"];
      ["tanh"];
   ] |> IdSet.of_list

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
         name    : id;             (** Name of the current scope *)
         kind    : symbol_kind;    (** Type of the current scope *)
         parent  : t option;       (** Pointer to it's parent *)
         keep    : t IdMap.t;      (** Persistent symbols (keeps mem and functions) *)
         locals  : t IdMap.t list; (** Temporary symbols (variables in subscopes) *)
         typ     : type_ref;       (** Type of the symbol *)
      }

   let empty : t =
      {
         name    = [];
         kind    = ModuleSymbol;
         parent  = None;
         keep    = IdMap.empty;
         locals  = [];
         typ     = ref (TId([""],None));
      }

   let rec dump (t:t) (level:int) : unit =
      Printf.printf "%s'%s' = %s\n" (String.make level ' ') (idStr t.name) (kindStr t.kind);
      IdMap.iter (fun _ sub -> dump sub (level+3)) t.keep

   let addMem (t:t) (name:id) (typ:type_ref) : t =
      let new_symbol = { empty with name = name; kind = MemSymbol; typ = typ } in
      { t with keep = IdMap.add name new_symbol t.keep }

   let addInstance (t:t) (name:id) : t =
      let new_symbol = { empty with name = name; kind = InstanceSymbol } in
      { t with keep = IdMap.add name new_symbol t.keep }

   let addVar (t:t) (name:id) (typ:type_ref) : t =
      let new_symbol = { empty with name = name; kind = VarSymbol; typ = typ  } in
      let first,rest =
         match t.locals with
         | [] -> IdMap.empty,[]
         | h::t -> h,t
      in
      { t with locals = (IdMap.add name new_symbol first) :: rest }

   let enter (t:t) (name:id) (kind:symbol_kind) : t =
      match IdMap.find name t.keep with
      | sub ->
         { sub with parent = Some(t) }
      | exception Not_found ->
         { empty with parent = Some(t); name = name; kind = kind }

   let exit (t:t) : t =
      match t.parent with
      | None -> failwith "Scope.exit: Cannot exit more scopes"
      | Some(parent) ->
         { parent with keep = IdMap.add t.name t parent.keep }

   let enterBlock (t:t) : t =
      { t with locals = IdMap.empty :: t.locals }

   let exitBlock (t:t) : t =
      { t with locals = List.tl t.locals }

   let enterFunction (t:t) (name:id) : t =
      enter t name FunctionSymbol

   let enterModule (t:t) (name:id) : t =
      enter t name ModuleSymbol

   let setCurrentType (t:t) (typ:type_ref) : t =
      { t with typ = typ }

   let getParent (t:t) : t option =
      match t.parent with
      | None -> None
      | Some(parent) ->
         Some({ parent with keep = IdMap.add t.name t parent.keep })

   let current (t:t) : path =
      let rec parentName parent =
         match parent with
         | None -> []
         | Some(parent_t) ->
            parent_t.name :: parentName parent_t.parent
      in
      t.name :: parentName t.parent
      |> List.rev |> List.flatten |> fun a -> Path(a)

   let rec find (t:t) (name:id) : t option =
      match name with
      | [] -> Some(t)
      | h::rest ->
         match IdMap.find [h] t.keep with
         | found ->
            find found rest
         | exception Not_found ->
            let rec findLocals l =
               match l with
               | []   -> None
               | local::locals ->
                  match IdMap.find [h] local with
                  | found ->
                     find found rest
                  | exception Not_found ->
                     findLocals locals
            in findLocals t.locals

   let rec lookupAny (t:t) (name:id) : t option =
      match find t name with
      | Some(value) -> Some(value)
      | None ->
         match getParent t with
         | Some(parent) ->
            lookupAny parent name
         | None -> None

   let getPath (t:t) : id =
      let rec parentPath (parent:t option) : id =
         match parent with
         | None -> []
         | Some(p) -> p.name @ parentPath p.parent
      in
      t.name @ (parentPath t.parent)
      |> List.rev

   let lookup (t:t) (name:id) : (path * type_ref) option =
      match lookupAny t name with
      | Some(t) -> Some(Path(getPath t), t.typ)
      | _ -> None

   let isMemOrInstance (t:t) (name:id) : bool =
      match find t name with
      | Some({ kind = MemSymbol }) -> true
      | Some({ kind = InstanceSymbol }) -> true
      | _  -> false

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

   let addMem (context:t) (func:path) (name:id) (typ:type_ref) : t =
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
            IdTypeSet.iter (fun (v,tp) -> Printf.printf "'%s:%s' " (idStr v) (PrintTypes.typeStr !tp)) value;
            print_newline ())
         context.mem;
      print_endline "Instance";
      IdMap.iter
         (fun key value -> Printf.printf "   '%s' = " (idStr key);
            IdTypeSet.iter (fun (name,kind) -> Printf.printf "'%s:%s' " (idStr name) (PrintTypes.typeStr !kind)) value;
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
      print_endline "Scope";
      Scope.dump state.scope 3

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

   (** Adds a mem variable to the current context *)
   let addMem (state:'a t) (name:id) (typ:type_ref) : 'a t  =
      {
         state with
         context = FunctionContex.addMem state.context (Scope.current state.scope) name typ;
         scope   = Scope.addMem state.scope name typ;
      }

   (** Adds a variable to the current block *)
   let addVar (state:'a t) (name:id) (typ:type_ref) : 'a t  =
      {
         state with
         scope   = Scope.addVar state.scope name typ;
      }

   (** Returns the full path of a function. Raises an error if it cannot be found *)
   let lookup (state:'a t) (name:id) : path * type_ref =
      match Scope.lookup state.scope name with
      | None -> failwith (Printf.sprintf "Cannot find symbol '%s'" (idStr name))
      | Some(path) -> path

      (** Returns the mem and instances for a function *)
   let getMemAndInstances (state:'a t) (name:id) : IdTypeSet.t * IdTypeSet.t =
      let path,_   = lookup state name in
      let ctx      = FunctionContex.findContext state.context path in
      let mem      = FunctionContex.getMemForContext state.context ctx in
      let instance = FunctionContex.getInstancesForContext state.context ctx in
      mem, instance

   (** Returns the generated context name for the given function *)
   let getContext (state:'a t) (name:id) : id =
      let path,_ = lookup state name in
      FunctionContex.findContext state.context path

   (** Returns true if the function is active *)
   let isActive (state:'a t) (name:id) : bool =
      let path,_ = lookup state name in
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
      let kind_path_opt = Scope.lookup state.scope kind in
      let kind_path,_ =
         match kind_path_opt with
         | Some(path) -> path
         | None -> failwith "Cannot find function"
      in
      if isActive state kind then
         let state', name' = generateInstanceName state name_opt in
         {
            state' with
            context = FunctionContex.addInstance state'.context (Scope.current state.scope) name' kind_path;
            scope = Scope.addInstance state.scope name';
         }, Some(name')
      else
         state, None

   (** Returns the current location *)
   let currentScope (state:'a t) : path =
      Scope.current state.scope

   let setCurrentType (state:'a t) (typ:type_ref) : 'a t =
      {
         state with
         scope = Scope.setCurrentType state.scope typ;
      }

   (** Enters to the context of the given function *)
   let enterFunction (state:'a t) (func:id) : 'a t  =
      {
         state with
         scope = Scope.enterFunction state.scope func;
         tick  = 0;
      }

   (** Closes the current context *)
   let exit (state:'a t) : 'a t  =
      {
         state with
         scope = Scope.exit state.scope;
      }

   (** Enters to a block of statements *)
   let enterBlock (state:'a t) : 'a t  =
      {
         state with
         scope = Scope.enterBlock state.scope ;
      }

   (** Exits to a block of statements *)
   let exitBlock (state:'a t) : 'a t  =
      {
         state with
         scope = Scope.exitBlock state.scope ;
      }

   (** Adds the builtin functions to the given context *)
   let addBuiltin (s:Scope.t) : Scope.t =
      IdSet.fold (fun a s -> Scope.enterFunction s a |> Scope.exit ) builtin_functions s

   (** Creates an empty module context *)
   let empty (init:id) data : 'a t =
      {
         data    = data;
         context = FunctionContex.empty;
         tick    = 0;
         scope   = Scope.enterModule (Scope.empty |> addBuiltin) init;
      }

   let get (state:'a t) : 'a =
      state.data

   let set (state:'a t) (data:'a) : 'a t =
      { state with data = data }

end
