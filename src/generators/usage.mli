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

(** Detects which runtime functions a generated program uses so the backends
    can emit only the parts of their runtime that are needed. *)

val calledFunctions : Core.Prog.prog -> Util.Maps.Set.t
(** All the function names called anywhere in the program (after the language
    replacements have been applied). *)

val existsExp : (Core.Prog.exp -> bool) -> Core.Prog.prog -> bool
(** [existsExp pred prog] is true when any expression of the program (at any
    nesting level) satisfies [pred]. *)

val existsType : (Core.Prog.type_ -> bool) -> Core.Prog.prog -> bool
(** [existsType pred prog] is true when any type used by the program
    (including sub-types like array elements) satisfies [pred]. *)

(** Dependencies of one generated file on the other files of a program
    generated with [-split-files]. [interface] lists the files whose header
    must be included from this file's header (their types appear in type
    definitions, function signatures or constants); [body] lists the files
    only referenced from function bodies, which can be included from the
    implementation file. *)
type file_deps = {interface: string list; body: string list}

val fileDependencies : (string * Core.Prog.prog) list -> (string * file_deps) list
(** Computes the dependencies between the files of a split program from the
    final program (source-level imports are not reliable: elaboration can
    place code in a module that refers to types of its callers). *)

(** Optional features of the C++ runtime (vultin). *)
type features =
  { fix16_math: bool  (** Fixed-point transcendental functions: [fix_sin], [fix_exp], etc. *)
  ; random: bool  (** Random number generators: [float_random], [fix_random], [int_random] *)
  ; strings: bool  (** [std::string] values and the [*_to_string] conversions *)
  ; serialization: bool  (** [CustomBuffer] and the [push_*]/[deserialize_*] functions *)
  ; tuples: bool  (** [std::tuple] values *)
  ; lists: bool  (** [std::vector] values coming from the list type *) }

val detect : Core.Prog.prog -> features
(** Scans the final program (after replacements have been applied) and reports
    which C++ runtime features the generated code uses. *)

val runtimeRequirements : string -> features -> Pla.t
(** [runtimeRequirements file features] is a preprocessor block with one
    [#error] for every feature the code of [file] needs, guarded by the
    matching [VULT_NO_*] macro. It is placed at the top of the generated
    header so that trimming the runtime with [-DVULT_NO_*] fails with one
    readable line instead of a wall of errors from the standard library. *)
