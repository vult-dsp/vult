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
open Util

type path =
  { id : string
  ; n : string option
  ; loc : Loc.t
  }

type type_d =
  | STId       of path
  | STSize     of int
  | STComposed of string * type_ list

and type_ =
  { t : type_d
  ; loc : Loc.t
  }

type exp_d =
  | SEUnit
  | SEBool   of bool
  | SEInt    of int
  | SEReal   of float
  | SEString of string
  | SEId     of string
  | SEEnum   of path
  | SEIndex  of
      { e : exp
      ; index : exp
      }
  | SEArray  of exp list
  | SECall   of
      { instance : string option
      ; path : path
      ; args : exp list
      }
  | SEUnOp   of string * exp
  | SEOp     of string * exp * exp
  | SEIf     of
      { cond : exp
      ; then_ : exp
      ; else_ : exp
      }
  | SETuple  of exp list
  | SEMember of exp * string
  | SEGroup  of exp

and exp =
  { e : exp_d
  ; loc : Loc.t
  }

and lexp_d =
  | SLWild
  | SLId     of string
  | SLMember of lexp * string
  | SLIndex  of
      { e : lexp
      ; index : exp
      }
  | SLGroup  of lexp
  | SLTuple  of lexp list

and lexp =
  { l : lexp_d
  ; loc : Loc.t
  }

type dexp_d =
  | SDWild
  | SDId    of string * int option
  | SDTuple of dexp list
  | SDGroup of dexp
  | SDTyped of dexp * type_

and dexp =
  { d : dexp_d
  ; loc : Loc.t
  }

type arg = string * type_ option * Loc.t

and stmt_d =
  | SStmtError
  | SStmtVal    of dexp * exp option
  | SStmtMem    of dexp * exp option * Ptags.tag list
  | SStmtBind   of lexp * exp
  | SStmtReturn of exp
  | SStmtBlock  of stmt list
  | SStmtIf     of exp * stmt * stmt option
  | SStmtWhile  of exp * stmt
  | SStmtIter   of
      { id : string * Loc.t
      ; value : exp
      ; body : stmt
      }

and stmt =
  { s : stmt_d
  ; loc : Loc.t
  }

and function_def =
  { name : string
  ; args : arg list
  ; t : type_ option
  ; next : (function_def * stmt) option
  ; loc : Loc.t
  ; tags : Ptags.tag list
  }

type top_stmt_d =
  | STopError
  | STopExternal of function_def * string option
  | STopFunction of function_def * stmt
  | STopType     of
      { name : string
      ; members : (string * type_ * Loc.t) list
      }
  | STopEnum     of
      { name : string
      ; members : (string * Loc.t) list
      }

and top_stmt =
  { top : top_stmt_d
  ; loc : Loc.t
  }

type stmts = top_stmt list

let compare_path (p1 : path) (p2 : path) =
  match p1, p2 with
  | { id = id1; n = Some n1; _ }, { id = id2; n = Some n2; _ } ->
      let ret = String.compare id1 id2 in
      if ret = 0 then String.compare n1 n2 else ret
  | { id = id1; n = None; _ }, { id = id2; n = None; _ } -> String.compare id1 id2
  | _ -> compare p1 p2


let print_path (p : path) =
  match p with
  | { id; n = None; _ } -> Pla.string id
  | { id; n = Some m; _ } -> [%pla {|<#m#s>.<#id#s>|}]
