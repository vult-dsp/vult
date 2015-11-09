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

open Either

module type S = sig
   type t
   type key
   type binding
   type bindings

   val empty : t

   (** create adds a new key to the map. If the key is already in the map,
       		it is removed from its current set. *)
   val create : key -> t -> t

   (** find finds the representative of a certain key, and an optimized structure . *)
   val find : key -> t -> key option * t

   (** bindings returns the bindings of the set represented by key, if any. *)
   val bindings : key -> t -> bindings option

   (** findBinding returns the representative, the bindings of the set and an optimized structure. *)
   val findBindings : key -> t -> key option * bindings option * t

   (** add_binding adds a binding to the set represented by key. *)
   val addBinding : key -> binding -> t -> t

   (** union unifies two keys. *)
   val union : key -> key -> t -> t
end

module Make (O : Map.OrderedType)(D : Set.OrderedType) = struct
   module InfoSet = Set.Make(D)
   module MapS    = CCMap.Make(O)

   type key = O.t
   type binding = D.t
   type bindings = InfoSet.t
   type info = (key, bindings) either
   type t = info MapS.t

   let empty : t = MapS.empty

   let create : key -> t -> t =
      fun k map -> MapS.add k (Left k) map

   let find : key -> t -> key option * t =
      fun key map ->
         let updater : key -> info option -> info option =
            fun parent _ -> Some (Left parent)
         in
         let update_all : key list -> key -> t =
            fun children parent ->
               List.fold_left 
                  (fun m c -> MapS.update c (updater parent) m)
                  map children
         in
         let rec go : key -> key list -> key option * t =
            fun key children -> match MapS.get key map with
               | Some (Left n) ->
                  if key == n 
                  then (Some n, update_all children n) 
                  else go n (key::children)
               | Some (Right _) -> (Some key, update_all children key)
               | None -> (None,map)
         in go key []

   let findBindings : key -> t -> key option * bindings option * t = 
      fun key map ->
         let updater : key -> info option -> info option =
            fun parent _ -> Some (Left parent)
         in
         let update_all : key list -> key -> t =
            fun children parent ->
               List.fold_left 
                  (fun m c -> MapS.update c (updater parent) m)
                  map children
         in
         let rec go : key -> key list -> key option * bindings option * t =
            fun key children -> match MapS.get key map with
               | Some (Left n) ->
                  if key == n 
                  then (Some n, None, update_all children n) 
                  else go n (key::children)
               | Some (Right b) -> (Some key, Some b, update_all children key)
               | None -> (Some key, None, map)
         in go key []

   let rec bindings : key -> t -> bindings option =
      fun k map -> match MapS.get k map with
         | Some (Left n) -> if k == n then None else bindings n map
         | Some (Right b) -> Some b
         | None -> None

   let rec addBinding : key -> binding -> t -> t =
      fun key binding map -> match findBindings key map with
         | (None, _, m) -> m
         | (Some parent, None,  m) ->
            MapS.update parent (fun _ -> Some (Right (InfoSet.singleton binding))) m
         | (Some parent, Some oldbindings, m) ->
            MapS.update parent (fun _ -> Some (Right (InfoSet.add binding oldbindings))) m

   let maybeUnion : key -> bindings option -> bindings option -> info =
      fun key mb1 mb2 -> match (mb1, mb2) with
         | (Some b1, Some b2) -> Right (InfoSet.union b1 b2)
         | (None, Some b2) -> Right b2
         | (Some b1, None) -> Right b1
         | _ -> Left key

   let union : key -> key -> t -> t =
      fun k1 k2 map -> match findBindings k1 map with
         | (None, _, m) -> m
         | (Some k1head, mb1, map2) -> begin match findBindings k2 map2 with
               | (None, _, m2) -> m2
               | (Some k2head, mb2, map3) -> 
                  if k1head != k2head
                  then let map4 = MapS.update k1head (fun _ -> Some (Left k2head)) map3
                     in MapS.update k2head (fun _ -> Some (maybeUnion k2head mb1 mb2)) map4
                  else map3
            end
end
