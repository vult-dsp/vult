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

module Scope (KeyType:Map.OrderedType) = struct
  module TypeMap = Map.Make(KeyType)
  type 'a t =
    {
      parent       : ('a t) option;
      subscopes    : ('a t) TypeMap.t;
      current_name : TypeMap.key option;
      values       : 'a TypeMap.t;
    }

  let empty =
    {
      parent       = None;
      subscopes    = TypeMap.empty;
      current_name = None;
      values       = TypeMap.empty;
    }

  let enter (scope:'a t) (name:TypeMap.key) : 'a t =
    if TypeMap.mem name scope.subscopes then
      TypeMap.find name scope.subscopes
    else
      { empty with
        parent       = Some(scope);
        current_name = Some(name);
      }

  let exit (scope:'a t) : 'a t =
    match scope.parent,scope.current_name with
    | None,_ -> failwith "Scope.exit: This is a top scope"
    | Some(parent),Some(name) ->
      { parent with
        subscopes = TypeMap.add name scope parent.subscopes }
    | _ -> failwith "Scope.exit: Malformed scope"

  let lookup (scope:'a t) (name:TypeMap.key) : 'a option =
    let rec lookup_loop = function
      | None    -> None
      | Some(s) ->
        if TypeMap.mem name s.values then
          Some(TypeMap.find name s.values)
        else
          lookup_loop s.parent
    in lookup_loop (Some(scope))

  let bind (scope:'a t) (name:TypeMap.key) (value:'a) : 'a t =
    { scope with values = TypeMap.add name value scope.values }

end