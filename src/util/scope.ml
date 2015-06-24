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

(** Provides a simple way of handling scopes *)

module type ScopeSig = sig
   type t
   type v
   type kind
   val compare :    t -> t -> int
   val string_t:    t -> string
   val string_v:    v -> string
end

module Scope (KeyType:ScopeSig) = struct
   module TypeMap = Map.Make(KeyType)
   type t =
      {
         parent       : t option;
         subscopes    : t TypeMap.t;
         current_name : TypeMap.key option;
         values       : KeyType.v TypeMap.t;
         kind         : KeyType.kind option;
      }

   let empty =
      {
         parent       = None;
         subscopes    = TypeMap.empty;
         current_name = None;
         values       = TypeMap.empty;
         kind         = None;
      }

   let enterAny (scope:t) (name:TypeMap.key) (kind:KeyType.kind option) : t =
      if TypeMap.mem name scope.subscopes then
         { (TypeMap.find name scope.subscopes) with parent = Some(scope) }
      else
         { empty with
           parent       = Some(scope);
           current_name = Some(name);
           kind         = kind;
         }

   let enter (scope:t) (name:TypeMap.key) (kind:KeyType.kind option) : t =
      enterAny scope name kind

   let exit (scope:t) : t =
      match scope.parent,scope.current_name with
      | None,_ -> failwith "Scope.exit: This is a top scope"
      | Some(parent),Some(name) ->
         { parent with
           subscopes = TypeMap.add name scope parent.subscopes }
      | Some(parent),None -> failwith "Scope.exit: All scopes should have name"

   let lookup (scope:t) (name:TypeMap.key) : 'a option =
      let rec lookup_loop = function
         | None    -> None
         | Some(s) ->
            if TypeMap.mem name s.values then
               Some(TypeMap.find name s.values)
            else
               lookup_loop s.parent
      in lookup_loop (Some(scope))

   let bind (scope:t) (name:TypeMap.key) (value:'a) : t =
      { scope with values = TypeMap.add name value scope.values }

   let rec enterPath (scope:t) (path:TypeMap.key list) : t =
      match path with
      | []   -> scope
      | h::t -> enter scope h None

   let getCurrentPath (scope:t) : KeyType.t list =
      let rec getCurrentPath_loop s =
         match s.current_name,s.parent with
         | None,_ -> []
         | Some(name), Some(parent) -> name :: getCurrentPath_loop parent
         | Some(name), None -> [name]
      in getCurrentPath_loop scope |> List.rev

   let getName o =
      match o with
      | Some(a) -> a
      | _ -> failwith "getName: cannot get the name"

   let rebind (scope:t) (name:TypeMap.key) (value:'a) : t =
      let rec rebind_loop s acc =
         if TypeMap.mem name s.values then
            Some(bind s name value,acc)
         else
            match s.parent with
            | None    -> None
            | Some(_) -> rebind_loop (exit s) ((getName s.current_name)::acc)
      in
      match rebind_loop scope [] with
      | None -> failwith "rebind:Cannot rebind a not bound item"
      | Some(final_scope,poped_scopes) -> enterPath final_scope poped_scopes

   let rec getTop (scope: t) : t =
      match scope.parent with
      | None    -> scope
      | Some(s) -> getTop s

   let nameStr = function
      | None   -> "-"
      | Some(name) -> KeyType.string_t name

   let valuesStr (scope: t) : string =
      TypeMap.fold (fun (key:TypeMap.key) (value:KeyType.v) acc ->
            let key_s   = KeyType.string_t key in
            let value_s = KeyType.string_v value in
            (Printf.sprintf "- %s = %s\n" key_s value_s)^acc) scope.values ""

   let rec scopeStr (scope: t) : string =
      let name   = nameStr scope.current_name in
      let values = valuesStr scope in
      let subs   =
         TypeMap.fold (fun key value acc ->
               (scopeStr value)^acc
            ) scope.subscopes ""
         |> Str.global_replace (Str.regexp_string "\n") "\n\t"
      in
      Printf.sprintf "\nname:%s\n%s\nsubs:%s\n" name values subs

   let rec printFullScope (scope: t) : unit =
      let top = getTop scope in
      print_endline (scopeStr top)

   let rec printScope (scope: t) : unit =
      print_endline (scopeStr scope)

end