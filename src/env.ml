open TypesVult

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

   let addTo (context:t) (name:id) : t =
      let current_in_ctx =
         try IdMap.find context.current context.backward with
         | Not_found -> []
      in
      Printf.printf
         "function '%s' belongs to context '%s'\n"
         (PrintTypes.identifierStr name)
         (PrintTypes.identifierStr context.current);
      {
         context with
         forward  = IdMap.add name context.current context.forward;
         backward = IdMap.add context.current (name::current_in_ctx) context.backward;
      }

   let makeNew (context:t) (name:id) : t =
      let context_name = ["ctx_"^(string_of_int context.count)] in
      Printf.printf
         "function '%s' belongs to context '%s'\n"
         (PrintTypes.identifierStr name)
         (PrintTypes.identifierStr context_name);
      {
         context with
         count    = context.count+1;
         current  = context_name;
         forward  = IdMap.add name context.current context.forward;
         backward = IdMap.add context.current [name] context.backward;
      }

   let addMem (context:t) (name:id) : t =
      let mem_for_context =
         try IdMap.find context.current context.mem with
         | Not_found -> IdSet.empty
      in
      Printf.printf
         " '%s' added to '%s'\n"
         (PrintTypes.identifierStr name)
         (PrintTypes.identifierStr context.current);
      {
         context with
         mem = IdMap.add context.current (IdSet.add name mem_for_context) context.mem;
      }
end

module Env = struct

   type t =
      {
         context : FunctionContex.t;
         tick    : int;
      }

   let empty =
      {
         context = FunctionContex.empty;
         tick = 0;
      }

   let addToContext (state:t) (name:id) =
      {
         state with
         context = FunctionContex.addTo state.context name
      }

   let makeNewContext (state:t) (name:id) =
      {
         state with
         context = FunctionContex.makeNew state.context name
      }

   let addMemToContext (state:t) (name:id) =
      {
         state with
         context = FunctionContex.addMem state.context name
      }

end
