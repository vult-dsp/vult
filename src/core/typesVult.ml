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

type id = string list
[@@deriving show,eq,ord]

(** This path is used to differentiate simple identifiers from full paths *)
type path =
   | Path of id


type attr_exp =
   | AId of id * Loc.t
   | AFun of id * (id * attr_exp) list * Loc.t
   | AInt of string * Loc.t
   | AReal of string * Loc.t

type attr =
   {
      loc     : Loc.t;
      fun_and : bool;
      active  : bool;
      bound   : bool;
      const   : bool;
      ext_fn  : string option;
      typ     : VType.t option;
      exp     : attr_exp list;
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
   | SimpleId of id * arg_type * attr
   | TypedId  of id * VType.t * arg_type * attr
[@@deriving show,eq,ord]

type lhs_exp =
   | LWild  of attr
   | LId    of id * VType.t option * attr
   | LTuple of lhs_exp list * attr
   | LTyped of lhs_exp * VType.t * attr
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
   | PId
      of id    (* name *)
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
      of id option    (* name/instance *)
         *  id        (* type/function name *)
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
      of id option (* Scope name *)
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
    of id                 (* name *)
       *  typed_id list   (* arguments *)
       *  stmt            (* body *)
       *  VType.t option  (* return type *)
       *  attr
  | StmtExternal
    of id                (* name *)
       *  typed_id list  (* arguments *)
       *  VType.t        (* return type *)
       *  string         (* linking name *)
       *  attr
  | StmtBind
    of lhs_exp        (* lhs *)
       *  exp         (* rhs *)
       *  attr
  | StmtBlock
    of id option      (* scope name *)
       *  stmt list
       *  attr
  | StmtType
    of VType.t          (* name *)
       *  val_decl list (* members *)
       *  attr
  | StmtAliasType
    of VType.t          (* name *)
       *  VType.t       (* alias type *)
       *  attr
  | StmtEmpty
[@@deriving show,eq,ord]

and val_decl =
    id          (* name *)
    * VType.t   (* type *)
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

type interpreter_results =
   {
      iresult : (string,Error.t list) CCError.t;
      lines   : string array;
   }

(** Stores the options passed to the command line *)
type arguments =
   {
      mutable files    : string list;
      mutable dparse   : bool;
      mutable eval     : bool;
      mutable ccode    : bool;
      mutable jscode   : bool;
      mutable luacode  : bool;
      mutable output   : string;
      mutable real     : string;
      mutable template : string;
      mutable show_version : bool;
      mutable includes : string list;
   }

let default_arguments =
   {
      files  = [];
      dparse = false;
      ccode  = false;
      eval   = false;
      jscode = false;
      luacode = false;
      output = "";
      real   = "float";
      template = "default";
      show_version = false;
      includes = [];
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
   }

module IdMap = CCMap.Make(struct type t = id let compare = compare end)

module PathMap = CCMap.Make(struct type t = path let compare = compare end)

module PathSet = CCSet.Make(struct type t = path let compare = compare end)

module IdSet = CCSet.Make(struct type t = id let compare = compare end)

type id_type = id * VType.t
[@@deriving show,eq,ord]


module IdTypeSet = CCSet.Make(struct type t = id_type let compare = compare_id_type end)

module TypeSet = CCSet.Make(struct type t = VType.t let compare = VType.compare end)

module TypeMap = CCMap.Make(struct type t = VType.t let compare = VType.compare end)

let pathId (path:path) : id =
   let Path(id) = path in
   id

let pathAppend (path:path) (id:id) : path =
   let Path(p) = path in
   Path(p@id)

let pathLast (path:path) : id =
   let rec last = function
      | []  -> failwith "Invalid path"
      | [h] -> h
      | _::t -> last t
   in
   match path with
   | Path(p) -> [ last p ]

let moduleName (file:string) : string =
   file
   |> Filename.basename
   |> Filename.chop_extension
   |> String.capitalize

