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
  val compare : t -> t -> int
  val string_t : t -> bytes
  val string_v : v -> bytes
end

module Scope : functor (KeyType : ScopeSig) -> sig
      module TypeMap :
        sig
          type key = KeyType.t
        end

      type t

      (** Returns an empty scope *)
      val empty : t

      (** Enters to a subscope, if it does not exists it creates it *)
      val enter : t -> KeyType.t -> t

      (** Leaves the current scope and returns the parent *)
      val exit : t -> t

      (** Search for a value in the current scope.
       If it does not exists continues searching up. *)
      val lookup : t -> KeyType.t -> KeyType.v option

      (** Binds the value to the given key in the current scope *)
      val bind : t -> KeyType.t -> KeyType.v -> t

      (** Lookup for a name in the current or parent scopes ad changes the value *)
      val rebind : t -> KeyType.t -> KeyType.v -> t

      (** returns the path to the current scope *)
      val getCurrentPath : t -> KeyType.t list

      (** Prints the top scope *)
      val printFullScope : t -> unit

      (** Prints the current scope *)
      val printScope : t -> unit
    end