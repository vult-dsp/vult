open TypesVult

(** Maps which function belongs to which context *)
module FunctionContex = struct

   type t =
      {
         forward  : id IdMap.t;
         backward : id list IdMap.t;
         count    : int;
         current  : id;
      }

   let empty =
      {
         forward  = IdMap.empty;
         backward = IdMap.empty;
         count    = 0;
         current  = [];
      }

   let addToCurrent (context:t) (name:id) =
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

   let addToNew (context:t) (name:id) =
      let context_name = ["ctx_"^(string_of_int context.count)] in
      Printf.printf
         "function '%s' belongs to context '%s'\n"
         (PrintTypes.identifierStr name)
         (PrintTypes.identifierStr context_name);
      {
         count    = context.count+1;
         current  = context_name;
         forward  = IdMap.add name context.current context.forward;
         backward = IdMap.add context.current [name] context.backward;
      }
end

type env =
   {
      context : FunctionContex.t;
      tick    : int;
   }

let empty =
   {
      context = FunctionContex.empty;
      tick = 0;
   }

