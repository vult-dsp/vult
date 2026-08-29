(*
   The MIT License (MIT)

   Copyright (c) 2014-2024 Leonardo Laguna Ruiz

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

open Prog

(* An access path: an identifier followed by member selections. Indices are
   excluded because evaluating one can have observable effects. *)
module Path : sig
  type t

  val of_exp : exp -> t option

  val of_lexp : lexp -> t option

  (* Rebuilds the expression that reads the location written by an lexp. *)
  val exp_of_lexp : lexp -> exp option

  (* Whether two paths may denote overlapping storage. *)
  val may_alias : t -> t -> bool

  val equal : t -> t -> bool
end

(* Folds over a statement and every statement nested in it, parents first. The
   function sees only the statement itself, never its children. *)
val fold_stmt : ('a -> stmt -> 'a) -> 'a -> stmt -> 'a

module GetVariables : sig
  val in_exp : exp -> Util.Maps.Set.t

  val in_lexp : lexp -> Util.Maps.Set.t

  val in_stmts : stmt list -> Util.Maps.Set.t
end
