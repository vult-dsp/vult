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

(** Functions to create graphs and analye them *)

open Graph
open CCHashtbl

module G = Imperative.Digraph.Abstract(struct type t = TypesVult.identifier end)
open G

module C = Components.Make(G)

(** Creates a graph given a list of vertex and a hash table defining the edges *)
let createGraph (vertex:'a list) (edges:('a,'a list) Hashtbl.t) : G.t =
   let g = create () in
   let vertexTable = Hashtbl.create 50 in
   List.iter (fun a -> Hashtbl.add vertexTable  a (G.V.create(a))) vertex;
   List.iter (fun a -> G.add_vertex g (Hashtbl.find vertexTable a);
             ) vertex;
   List.iter (fun a ->
         let dependencies =
            if Hashtbl.mem edges a then
               Hashtbl.find edges a
            else []
         in
         List.iter (fun b ->
               let g1 = Hashtbl.find vertexTable a in
               let g2 = Hashtbl.find vertexTable b in
               G.add_edge g g1 g2) dependencies
      ) vertex;
   g

(** Returns the cycles in the graph in order *)
let calculateComponents g =
   let comps = C.scc_array g in
   comps |> Array.map (fun a-> List.map V.label a ) |> Array.to_list
