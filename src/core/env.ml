open TypesVult

module Scope = struct

   type t = id list

   let empty = []

   let enter (t:t) (name:id) : t =
      name::t

   let exit (t:t) : t =
      match t with
      | [] -> failwith "Scope.exit: cannot exit more scopes"
      | _::e -> e

   let get (t:t) : id =
      List.rev t |> List.flatten

end

(** Maps which function belongs to which context *)
module FunctionContex = struct

   type t =
      {
         forward  : id IdMap.t;
         backward : id list IdMap.t;
         mem      : IdSet.t IdMap.t;
         count    : int;
         current  : id;
      }

   let empty =
      {
         forward  = IdMap.empty;
         backward = IdMap.empty;
         mem      = IdMap.empty;
         count    = 0;
         current  = [];
      }

   let findContext (context:t) (func:id) : id =
      try IdMap.find func context.forward with
      | Not_found -> failwith (Printf.sprintf "The function '%s' is unknown" (PrintTypes.identifierStr func))

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
         " '%s' added to '%s'\n"
         (PrintTypes.identifierStr name)
         (PrintTypes.identifierStr func);
      {
         context with
         mem = IdMap.add func (IdSet.add name mem_for_context) context.mem;
      }
end

module Env = struct

   type 'a t =
      {
         data    : 'a;
         context : FunctionContex.t;
         scope   : Scope.t;
         tick    : int;
      }

   let empty data =
      {
         data    = data;
         context = FunctionContex.empty;
         tick    = 0;
         scope   = Scope.empty;
      }

   let addToContext (state:'a t) (name:id) =
      {
         state with
         context = FunctionContex.addTo state.context name
      }

   let makeNewContext (state:'a t) (name:id) =
      {
         state with
         context = FunctionContex.makeNew state.context name
      }

   let addMemToContext (state:'a t) (name:id) =
      {
         state with
         context = FunctionContex.addMem state.context (Scope.get state.scope) name
      }

   let enter (state:'a t) (func:id) =
      {
         state with
         scope = Scope.enter state.scope func;
      }

   let exit (state:'a t) =
      {
         state with
         scope = Scope.exit state.scope;
      }

end
