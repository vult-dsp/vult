open TypesVult

let builtin_functions =
   [
      ["abs"];
      ["exp"];
      ["sin"];
      ["floor"];
      ["clip"];
      ["not"];
   ] |> IdSet.of_list

type bind_type =
   | MemBind
   | ModuleBind
   | FunctionBind

(** Used to track the location while traversing and also to lookup function, mem, and module *)
module Scope = struct

   type t =
      {
         name   : id;
         parent : t option;
         sub    : t IdMap.t;
         kind   : bind_type;
      }

   let empty : t =
      {
         name    = [];
         parent  = None;
         sub     = IdMap.empty;
         kind    = ModuleBind;
      }

   let enter (t:t) (name:id) (kind:bind_type) : t =
      match IdMap.find name t.sub with
      | sub ->
         { sub with parent = Some(t) }
      | exception Not_found ->
         { empty with parent = Some(t); name = name; kind = kind }

   let enterFunction (t:t) (name:id) : t =
      enter t name FunctionBind

   let exit (t:t) : t =
      match t.parent with
      | None -> failwith "Scope.exit: Cannot exit more scopes"
      | Some(parent) ->
         { parent with sub = IdMap.add t.name t parent.sub }

   let getParent (t:t) : t option =
      match t.parent with
      | None -> None
      | Some(parent) ->
         Some({ parent with sub = IdMap.add t.name t parent.sub })

   let current (t:t) : id =
      let rec parentName parent =
         match parent with
         | None -> []
         | Some(parent_t) ->
            parent_t.name :: parentName parent_t.parent
      in
      t.name :: parentName t.parent
      |> List.rev |> List.flatten

   let rec find (t:t) (name:id) : t option =
      match name with
      | [] -> Some(t)
      | h::rest ->
         match IdMap.find [h] t.sub with
         | found ->
            find found rest
         | exception Not_found ->
            None

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

   let lookupFunction (t:t) (name:id) =
      match lookupAny t name with
      | Some(t) -> Some(getPath t)
      | _ -> None


end

(** Maps which function belongs to which context *)
module FunctionContex = struct

   type t =
      {
         forward   : id IdMap.t;
         backward  : id list IdMap.t;
         mem       : IdSet.t IdMap.t;
         instance  : IdSet.t IdMap.t;
         count     : int;
         current   : id;
         activefun : bool IdMap.t;
      }

   let empty =
      {
         forward   = IdMap.empty;
         backward  = IdMap.empty;
         mem       = IdMap.empty;
         instance  = IdMap.empty;
         count     = 0;
         current   = [];
         activefun = IdMap.empty;
      }

   (** Returns the context type of a given function *)
   let findContext (context:t) (func:id) : id =
      try IdMap.find func context.forward with
      | Not_found -> failwith (Printf.sprintf "Cannot find context for function '%s'" (PrintTypes.identifierStr func))

   (** Returns the instances for the given context *)
   let getInstancesForContext (context:t) (name:id) : IdSet.t =
      try IdMap.find name context.instance with
      | Not_found -> IdSet.empty

   (** Returns the mem for the given context *)
   let getMemForContext (context:t) (name:id) : IdSet.t =
      try IdMap.find name context.mem with
      | Not_found -> IdSet.empty

   let addTo (context:t) (func:id) : t =
      let current_in_ctx =
         try IdMap.find context.current context.backward with
         | Not_found -> []
      in
      Printf.printf
         "function '%s' belongs to context '%s'\n"
         (PrintTypes.identifierStr func)
         (PrintTypes.identifierStr context.current);
      {
         context with
         forward  = IdMap.add func context.current context.forward;
         backward = IdMap.add context.current (func::current_in_ctx) context.backward;
      }

   let makeNew (context:t) (func:id) : t =
      let context_name = ["ctx_"^(string_of_int context.count)] in
      Printf.printf
         "function '%s' belongs to context '%s'\n"
         (PrintTypes.identifierStr func)
         (PrintTypes.identifierStr context_name);
      {
         context with
         count    = context.count+1;
         current  = context_name;
         forward  = IdMap.add func context.current context.forward;
         backward = IdMap.add context.current [func] context.backward;
      }

   let addMem (context:t) (func:id) (name:id) : t =
      let context_for_func = findContext context func in
      let mem_for_context  =
         try IdMap.find context_for_func context.mem with
         | Not_found -> IdSet.empty
      in
      Printf.printf
         " mem '%s' added to '%s'\n"
         (PrintTypes.identifierStr name)
         (PrintTypes.identifierStr func);
      {
         context with
         mem = IdMap.add func (IdSet.add name mem_for_context) context.mem;
      }

   let addInstance (context:t) (func:id) (name:id) (kind:id) : t =
      let context_for_func = findContext context func in
      let instance_for_context  =
         try IdMap.find context_for_func context.instance with
         | Not_found -> IdSet.empty
      in
      Printf.printf
         " instance '%s' of kind '%s' added to '%s'\n"
         (PrintTypes.identifierStr name)
         (PrintTypes.identifierStr kind)
         (PrintTypes.identifierStr func);
      {
         context with
         instance = IdMap.add func (IdSet.add name instance_for_context) context.instance;
      }

   let isActive (context:t) (name:id) : t * bool =
      try context,IdMap.find name context.activefun with
      | Not_found ->
         if IdSet.mem name builtin_functions then
            context, false
         else
            let fun_context = findContext context name in
            let is_active   = not @@
               IdSet.is_empty (getInstancesForContext context fun_context) &&
                  IdSet.is_empty (getMemForContext context fun_context)
            in
            { context with activefun = IdMap.add name is_active context.activefun }, is_active


end

module Env = struct

   type 'a t =
      {
         data    : 'a;
         context : FunctionContex.t;
         scope   : Scope.t;
         tick    : int;
      }

   let tick (state:'a t) : int * 'a t =
      state.tick,{ state with tick = state.tick+1 }

   let includeFunctionInContext (state:'a t) : 'a t  =
      {
         state with
         context = FunctionContex.addTo state.context (Scope.current state.scope)
      }

   let makeNewContext (state:'a t) : 'a t  =
      {
         state with
         context = FunctionContex.makeNew state.context (Scope.current state.scope)
      }

   let addMemToContext (state:'a t) (name:id) : 'a t  =
      {
         state with
         context = FunctionContex.addMem state.context (Scope.current state.scope) name
      }

   let addInstanceToContext (state:'a t) (name:id) (kind:id) : 'a t  =
      let full_name =
         match Scope.lookupFunction state.scope kind with
         | None -> failwith (Printf.sprintf "Cannot find function '%s'" (PrintTypes.identifierStr kind))
         | Some(full_name) -> full_name
      in
      let context', is_active = FunctionContex.isActive state.context full_name in
      if is_active then
         {
            state with
            context = FunctionContex.addInstance context' (Scope.current state.scope) name kind
         }
      else
         { state with context = context' }

   let enterFunction (state:'a t) (func:id) : 'a t  =
      {
         state with
         scope = Scope.enterFunction state.scope func;
         tick  = 0;
      }

   let exit (state:'a t) : 'a t  =
      {
         state with
         scope = Scope.exit state.scope;
      }

   let addBuiltin (s:Scope.t) : Scope.t =
      IdSet.fold (fun a s -> Scope.enterFunction s a |> Scope.exit ) builtin_functions s

   let empty (init:id) data : 'a t =
      {
         data    = data;
         context = FunctionContex.empty;
         tick    = 0;
         scope   = Scope.enter (Scope.empty |> addBuiltin) init ModuleBind;
      }

end
