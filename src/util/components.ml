(*
   The MIT License (MIT)

   Copyright (c) 2016 Leonardo Laguna Ruiz

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

module type GESig = sig
  type key
  type data

  val get : data -> key
end

(* Graph module *)
module G (T : GESig) = struct
  (* main type of a graph *)
  type g =
    { forward : (T.key, T.data list) Hashtbl.t
    ; (* forward representation of the graph *)
      backward : (T.key, T.data list) Hashtbl.t
    ; (* reversed repesentation of directed graph *)
      vertex : (T.key, T.data) Hashtbl.t (* set with all the vertex in the graph *)
    }

  (* returns an empty graph *)
  let empty () : g = { forward = Hashtbl.create 8; backward = Hashtbl.create 8; vertex = Hashtbl.create 8 }

  (* adds a graph edge *)
  let addEdge (g : g) (from_v : T.data) (to_v : T.data) : unit =
    let from_key = T.get from_v in
    let to_key = T.get to_v in
    let () =
      (* inserts edge from_v to to_v *)
      match Hashtbl.find g.forward from_key with
      | deps -> Hashtbl.replace g.forward from_key (to_v :: deps)
      | exception Not_found -> Hashtbl.add g.forward from_key [ to_v ]
    in
    let () =
      (* inserts edge to_v to from_v *)
      match Hashtbl.find g.backward to_key with
      | deps -> Hashtbl.replace g.backward to_key (from_v :: deps)
      | exception Not_found -> Hashtbl.add g.backward to_key [ from_v ]
    in
    (* adds both vertex to the set *)
    let () = Hashtbl.replace g.vertex from_key from_v in
    let () = Hashtbl.replace g.vertex to_key to_v in
    ()


  let addVertex (g : g) (v : T.data) : unit = Hashtbl.replace g.vertex (T.get v) v

  (* gets the all vertices pointed by a given vertex *)
  let getDependencies (g : g) (v : T.data) : T.data list =
    match Hashtbl.find g.forward (T.get v) with
    | deps -> deps
    | exception Not_found -> []


  (* gets the all vertices that point to a given vertex *)
  let getRevDependencies (g : g) (v : T.data) : T.data list =
    match Hashtbl.find g.backward (T.get v) with
    | deps -> deps
    | exception Not_found -> []


  (* returns a list with all vertices of the graph *)
  let getVertices (g : g) : T.data list = Hashtbl.fold (fun _ v acc -> v :: acc) g.vertex []

  (* makes a graph given a list of the vertices and it's dependencies *)
  let make (e : (T.data * T.data list) list) : g =
    let g = empty () in
    let () =
      List.iter
        (fun (v, deps) ->
          addVertex g v;
          List.iter (addEdge g v) deps)
        e
    in
    g
end

(* imperative stack implementation *)
module S = struct
  (* the stack is represented as a list ref *)
  type 'a t = 'a list ref

  (* creates a new empty stack *)
  let empty () : 'a t = ref []

  (* returns true if the stack is empty *)
  let isEmpty (s : 'a t) : bool = !s = []

  (* inserts an element to the stack *)
  let push (s : 'a t) (e : 'a) : unit = s := e :: !s

  (* returns the first element of the stack *)
  let pop (s : 'a t) : 'a =
    match !s with
    | [] -> failwith "Stack is empty"
    | h :: t ->
      s := t;
      h


  (* returns a list representation of the stack *)
  let toList (s : 'a t) : 'a list = !s
end

(* trivial implementation of an imperative set *)
module V (T : GESig) = struct
  (* the set is represented with a hastable *)
  type t = (T.key, unit) Hashtbl.t

  (* returns an empty set *)
  let empty () : t = Hashtbl.create 8

  (* adds an element to the set*)
  let add (t : t) (v : T.data) : unit = Hashtbl.replace t (T.get v) ()

  (* returns true if the set contains the give element *)
  let contains (t : t) (v : T.data) : bool = Hashtbl.mem t (T.get v)
end

module Make (T : GESig) = struct
  module G = G (T)
  module V = V (T)

  (* pass one of the Kosaraju's algorithm *)
  let rec pass1 g stack visited v =
    if not (V.contains visited v) then (
      let () = V.add visited v in
      let children = G.getDependencies g v in
      let () = List.iter (pass1 g stack visited) children in
      S.push stack v)


  let rec pass2_part g visited comp v =
    if not (V.contains visited v) then (
      let deps = G.getRevDependencies g v in
      let () = V.add visited v in
      let () = S.push comp v in
      List.iter (pass2_part g visited comp) deps)


  let rec pass2 g comps stack visited =
    if not (S.isEmpty stack) then (
      let v = S.pop stack in
      if V.contains visited v then
        pass2 g comps stack visited
      else (
        let comp = S.empty () in
        let () = S.push comps comp in
        pass2_part g visited comp v;
        pass2 g comps stack visited))


  (* calculates the strong components of a graph *)
  let calculate (graph : (T.data * T.data list) list) : T.data list list =
    let g = G.make graph in
    let stack = S.empty () in
    (* creates a set of visited vertex *)
    let visited = V.empty () in
    (* performs the first pass *)
    let () = List.iter (fun v -> pass1 g stack visited v) (G.getVertices g) in
    (* creates a stack to hold the components *)
    let comps = S.empty () in
    (* creates a new set of visited vertex *)
    let visited = V.empty () in
    (* performs the second pass *)
    pass2 g comps stack visited;
    (* returns the components as a list of lists *)
    comps |> S.toList |> List.map S.toList
end
