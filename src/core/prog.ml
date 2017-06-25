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

(** Main description of a Vult program *)

type tag =
   | TId     of Id.t * Loc.t
   | TFun    of Id.t * (Id.t * tag) list * Loc.t
   | TInt    of string * Loc.t
   | TReal   of string * Loc.t
   | TString of string * Loc.t

type attr =
   {
      loc     : Loc.t;
      fun_and : bool;
      active  : bool;
      bound   : bool;
      const   : bool;
      ext_fn  : string option;
      typ     : Typ.t option;
      exp     : tag list;
      evaluated : bool;
   }

let pp_attr = fun fmt _ -> Format.pp_print_string fmt "attr"
let equal_attr _ _ = true
let compare_attr _ _ = 0

type arg_type =
   | InputArg
   | OutputArg
   | ContextArg
[@@deriving show,eq,ord]

type typed_id =
   | SimpleId of Id.t * arg_type * attr
   | TypedId  of Id.t * Typ.t * arg_type * attr
[@@deriving show,eq,ord]

type lhs_exp =
   | LWild  of attr
   | LId    of Id.t * Typ.t option * attr
   | LTuple of lhs_exp list * attr
   | LTyped of lhs_exp * Typ.t * attr
   | LGroup of lhs_exp * attr
[@@deriving show,eq,ord]

(** Parser syntax tree *)
type exp =
   | PUnit
      of attr
   | PBool
      of bool
         *  attr
   | PInt
      of int
         *  attr
   | PReal
      of float
         *  attr
   | PString
      of string
         *  attr

   | PId
      of Id.t    (* name *)
         *  attr
   | PArray
      of exp array
         * attr
   | PUnOp
      of string      (* operator *)
         *  exp
         *  attr
   | POp
      of string      (* operator *)
         *  exp list
         *  attr
   | PCall
      of Id.t option    (* name/instance *)
         *  Id.t        (* type/function name *)
         *  exp list  (* arguments *)
         *  attr
   | PIf
      of exp    (* condition *)
         *  exp (* then *)
         *  exp (* else *)
         *  attr
   | PGroup
      of exp
         *  attr
   | PTuple
      of exp list
         *  attr
   | PSeq
      of Id.t option (* Scope name *)
         *  stmt
         *  attr
   | PEmpty
[@@deriving show,eq,ord]

and stmt =
    | StmtVal
   of lhs_exp        (* names/lhs *)
      *  exp option  (* rhs *)
      *  attr
  | StmtMem
    of lhs_exp        (* names/lhs *)
       *  exp option  (* rhs *)
       *  attr
  | StmtWhile
    of exp            (* condition*)
       *  stmt        (* statements *)
       *  attr
  | StmtReturn
    of exp
       *  attr
  | StmtIf
    of exp            (* condition *)
       *  stmt        (* then *)
       *  stmt option (* else *)
       *  attr
  | StmtFun
    of Id.t               (* name *)
       *  typed_id list   (* arguments *)
       *  stmt            (* body *)
       *  Typ.t option  (* return type *)
       *  attr
  | StmtExternal
    of Id.t              (* name *)
       *  typed_id list  (* arguments *)
       *  Typ.t        (* return type *)
       *  string option  (* linking name *)
       *  attr
  | StmtBind
    of lhs_exp        (* lhs *)
       *  exp         (* rhs *)
       *  attr
  | StmtBlock
    of Id.t option      (* scope name *)
       *  stmt list
       *  attr
  | StmtType
    of Typ.t          (* name *)
       *  val_decl list (* members *)
       *  attr
  | StmtAliasType
    of Typ.t          (* name *)
       *  Typ.t       (* alias type *)
       *  attr
  | StmtEmpty
[@@deriving show,eq,ord]

and val_decl =
    Id.t        (* name *)
    * Typ.t   (* type *)
    * attr
[@@deriving show,eq,ord]

type exp_list = exp list
[@@deriving show,eq,ord]

type scope_kind =
   | FuncScope
   | LocalScope

type parser_results =
   {
      presult : stmt list;
      file    : string;
   }

let makeAttr (loc:Loc.t) : attr =
   {
      loc     = loc;
      fun_and = false;
      active  = false;
      bound   = false;
      typ     = None;
      ext_fn  = None;
      exp     = [];
      const   = false;
      evaluated = false;
   }

let emptyAttr =
   {
      loc     = Loc.default;
      fun_and = false;
      active  = false;
      bound   = false;
      typ     = None;
      ext_fn  = None;
      exp     = [];
      const   = false;
      evaluated = false;
   }

let moduleName (file:string) : string =
   file
   |> Filename.basename
   |> Filename.chop_extension
   |> String.capitalize_ascii

