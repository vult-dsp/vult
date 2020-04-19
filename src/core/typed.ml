(*
   The MIT License (MIT)

   Copyright (c) 2020 Leonardo Laguna Ruiz

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

type path = Syntax.path [@@deriving show, eq, ord]

type type_d =
  | TId       of path
  | TSize     of int
  | TComposed of path * type_ list
[@@deriving show, eq, ord]

and type_ =
  { t : type_d
  ; loc : Loc.t [@compare fun _ _ -> 0]
  }
[@@deriving show, eq, ord]

type ext_type_d =
  | TEUnbound  of string
  | TEId       of path
  | TESize     of int
  | TELink     of ext_type
  | TEOption   of ext_type list
  | TEComposed of string * ext_type list
[@@deriving show, eq, ord]

and ext_type =
  { mutable tx : ext_type_d
  ; mutable loc : Loc.t
  }
[@@deriving show, eq, ord]

type ext_fun_type = ext_type list * ext_type

module type TSig = sig
  type t [@@deriving show, eq, ord]
end

module Prog (T : TSig) = struct
  type tag = Syntax.tag [@@deriving show, eq, ord]

  type exp_d =
    | EUnit
    | EBool   of bool
    | EInt    of int
    | EReal   of float
    | EString of string
    | EId     of string
    | EIndex  of
        { e : exp
        ; index : exp
        }
    | EArray  of exp list
    | ECall   of
        { instance : string option
        ; path : path
        ; args : exp list
        }
    | EUnOp   of string * exp
    | EOp     of string * exp * exp
    | EIf     of
        { cond : exp
        ; then_ : exp
        ; else_ : exp
        }
    | ETuple  of exp list
    | EMember of exp * string
  [@@deriving show, eq, ord]

  and exp =
    { e : exp_d
    ; loc : Loc.t
    ; t : T.t
    }
  [@@deriving show, eq, ord]

  and lexp_d =
    | LWild
    | LId     of string
    | LMember of lexp * string
    | LIndex  of
        { e : lexp
        ; index : exp
        }
    | LTuple  of lexp list
  [@@deriving show, eq, ord]

  and lexp =
    { l : lexp_d
    ; loc : Loc.t
    ; t : T.t
    }
  [@@deriving show, eq, ord]

  type dexp_d =
    | DWild
    | DId    of string * int option
    | DTuple of dexp list
  [@@deriving show, eq, ord]

  and dexp =
    { d : dexp_d
    ; loc : Loc.t
    ; t : T.t
    }
  [@@deriving show, eq, ord]

  type arg = string * T.t * Loc.t [@@deriving show, eq, ord]

  and stmt_d =
    | StmtVal    of dexp * exp option
    | StmtMem    of dexp * exp option * tag list
    | StmtBind   of lexp * exp
    | StmtReturn of exp
    | StmtBlock  of stmt list
    | StmtIf     of exp * stmt * stmt option
    | StmtWhile  of exp * stmt
  [@@deriving show, eq, ord]

  and stmt =
    { s : stmt_d
    ; loc : Loc.t
    }
  [@@deriving show, eq, ord]

  and function_def =
    { name : string
    ; args : arg list
    ; t : T.t list * T.t
    ; next : (function_def * stmt) option
    ; loc : Loc.t
    ; tags : tag list
    }
  [@@deriving show, eq, ord]

  type top_stmt_d =
    | TopExternal  of function_def * string
    | TopFunction  of function_def * stmt
    | TopTypeAlias of string * T.t
    | TopType      of
        { name : string
        ; members : (string * T.t * Loc.t) list
        }
  [@@deriving show, eq, ord]

  and top_stmt =
    { top : top_stmt_d
    ; loc : Loc.t
    }
  [@@deriving show, eq, ord]

  type program = top_stmt list [@@deriving show, eq, ord]
end

module TX = struct
  let makeId loc id = { tx = TEId { id; n = None; loc }; loc }

  let unbound loc = { tx = TEUnbound ""; loc }

  let unit ~loc = makeId loc "unit"

  let int ~loc = makeId loc "int"

  let bool ~loc = makeId loc "bool"

  let string ~loc = makeId loc "string"

  let real ~loc = makeId loc "real"

  let fix16 ~loc = makeId loc "fix16"

  let num loc = { tx = TEOption [ real loc; int loc; fix16 loc ]; loc }

  let size ?(loc = Loc.default) n = { tx = TESize n; loc }

  let array ?(loc = Loc.default) ?(size = unbound loc) t = { tx = TEComposed ("array", [ t; unbound loc ]); loc }

  let tuple ?(loc = Loc.default) l = { tx = TEComposed ("tuple", l); loc }

  let freal_type () =
    let loc = Loc.default in
    { tx = TEOption [ real loc; fix16 loc ]; loc }


  let array_set () : ext_fun_type =
    let loc = Loc.default in
    let a = unbound loc in
    let a_array = array a in
    [ a_array; int loc; a ], unit loc


  let array_get () : ext_fun_type =
    let loc = Loc.default in
    let a = unbound loc in
    let a_array = array a in
    [ a_array; int loc ], a


  let array_size () : ext_fun_type =
    let loc = Loc.default in
    let a = unbound loc in
    let a_array = array a in
    [ a_array ], int loc


  let array_make () : ext_fun_type =
    let loc = Loc.default in
    let a = unbound loc in
    let a_array = array a in
    [ int loc; a ], a_array


  let wrap_array () : ext_fun_type =
    let loc = Loc.default in
    let a = unbound loc in
    let array_type = array a in
    [ array_type ], array_type


  let freal_freal () : ext_fun_type =
    let t = freal_type () in
    [ t ], t


  let real_real_real () : ext_fun_type =
    let loc = Loc.default in
    let t = real loc in
    [ t; t ], t


  let clip () : ext_fun_type =
    let loc = Loc.default in
    let t = unbound loc in
    [ t; t; t ], t


  let num_int () : ext_fun_type =
    let loc = Loc.default in
    [ num loc ], int loc


  let num_real () : ext_fun_type =
    let loc = Loc.default in
    [ num loc ], real loc


  let num_fix16 () : ext_fun_type =
    let loc = Loc.default in
    [ num loc ], fix16 loc


  let num_num () : ext_fun_type =
    let loc = Loc.default in
    let t = num loc in
    [ t ], t


  let num_num_num () : ext_fun_type =
    let loc = Loc.default in
    let t = num loc in
    [ t; t ], t


  let int_int_int () : ext_fun_type =
    let loc = Loc.default in
    let t = int loc in
    [ t; t ], t


  let num_num_bool () : ext_fun_type =
    let loc = Loc.default in
    let t = num loc in
    [ t; t ], bool loc


  let a_a_bool () : ext_fun_type =
    let loc = Loc.default in
    let t = unbound loc in
    [ t; t ], bool loc


  let bool_bool () : ext_fun_type =
    let loc = Loc.default in
    let t = bool loc in
    [ t ], t


  let bool_bool_bool () : ext_fun_type =
    let loc = Loc.default in
    let t = bool loc in
    [ t; t ], t


  let unit_int () : ext_fun_type =
    let loc = Loc.default in
    [ unit loc ], int loc


  let unit_real () : ext_fun_type =
    let loc = Loc.default in
    [ unit loc ], real loc
end

module PX = Prog (struct
  type t = ext_type [@@deriving show, eq, ord]
end)
