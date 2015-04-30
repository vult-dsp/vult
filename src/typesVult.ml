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

open Lexing
(** Module containing the types *)

(** Location information *)
type location =
   {
      start_pos : position;
      end_pos   : position;
   }
let pp_location = fun fmt _ -> Format.pp_print_string fmt "loc"
let equal_location _ _ = true
let compare_location _ _ = 0

let default_loc = { start_pos = dummy_pos  ; end_pos = dummy_pos }

type lexed_lines =
   {
      current_line      : Buffer.t;
      mutable all_lines : string list;
   }

(** Tokens *)
type token_enum =
   | EOF
   | INT
   | REAL
   | ID
   | FUN
   | MEM
   | VAL
   | RET
   | IF
   | THEN
   | ELSE
   | LBRAC
   | RBRAC
   | LPAREN
   | RPAREN
   | LSEQ
   | RSEQ
   | COLON
   | SEMI
   | COMMA
   | EQUAL
   | OP
   | AT
   | DOT
   | WHILE
   | TYPE
   | TAB
   | LARR
   | RARR

type 'a token =
   {
      kind     : token_enum;
      value    : string;
      contents : 'a;
      loc      : location;
   }

type error =
   | PointedError of location * string
   | SimpleError  of string

type errors = error list

(** Type containing the stream of tokens *)
type 'a lexer_stream =
   {
      lexbuf             : Lexing.lexbuf;
      mutable has_errors : bool;
      mutable errors     : error list;
      mutable peeked     : 'a token;
      mutable prev       : 'a token;
      lines              : lexed_lines;
   }

type identifier = string list
   [@@deriving show,eq,ord]

(** This type is used to attach more information to the function calls *)
type call_attribute =
   | SimpleBinding (* Used by Passes.bindFunctionCalls to mark the function calls that have been bound *)
   | DummtAttr
   [@@deriving show,eq,ord]

type call_attributes = call_attribute list
   [@@deriving show,eq,ord]

type named_id =
   | SimpleId of identifier * location
   | NamedId  of identifier * exp * location
   [@@deriving show,eq,ord]

(** Parser syntax tree *)
and exp =
   | PUnit
      of location
   | PBool
      of bool
      *  location
   | PInt
      of int
      *  location
   | PReal
      of float
      *  location
   | PId
      of identifier  (* name *)
      *  exp option  (* type *)
      *  location
   | PTyped
      of exp   (* expression *)
      *  exp   (* type *)
      *  location
   | PUnOp
      of string      (* operator *)
      *  exp
      *  location
   | PBinOp
      of string      (* operator *)
      *  exp
      *  exp
      *  location
   | PCall
      of identifier option (* name/instance *)
      *  identifier        (* type/function name *)
      *  exp list          (* arguments *)
      *  location
      *  call_attributes
   | PIf
      of exp (* condition *)
      *  exp (* then *)
      *  exp (* else *)
      *  location
   | PGroup
      of exp
      *  location
   | PTuple
      of exp list
      *  location
   | PSeq
      of identifier option (* Scope name *)
      *  exp list
      *  location
   | PEmpty

   | StmtVal
      of exp        (* names/lhs *)
      *  exp option (* rhs *)
      *  location
   | StmtMem
      of exp        (* names/lhs *)
      *  exp option (* initial value *)
      *  exp option (* rhs *)
      *  location
   | StmtTab
      of identifier (* name *)
      *  exp list   (* data *)
      *  location
   | StmtWhile
      of exp         (* condition*)
      *  exp         (* statements *)
      *  location
   | StmtReturn
      of exp
      *  location
   | StmtIf
      of exp        (* condition *)
      *  exp        (* then *)
      *  exp option (* else *)
      *  location
   | StmtFun
      of identifier       (* name *)
      *  named_id list    (* arguments *)
      *  exp              (* body *)
      *  exp option       (* return type *)
      *  bool             (* is_active *)
      *  location
   | StmtBind
      of exp         (* lhs *)
      *  exp         (* rhs *)
      *  location
   | StmtBlock
      of identifier option (* scope name *)
      *  exp list
      *  location
   | StmtType
      of identifier           (* name *)
      *  named_id list        (* arguments *)
      *  val_decl list        (* members *)
      *  location
   | StmtAliasType
      of identifier           (* name *)
      *  named_id list        (* arguments *)
      *  exp                  (* alias type *)
      *  location
   | StmtEmpty
   [@@deriving show,eq,ord]

and val_decl =
   identifier  (* name *)
   * exp       (* type *)
   * location

type exp_list = exp list
   [@@deriving show,eq,ord]

type parser_results =
   {
      presult : (exp list,error list) CCError.t;
      file    : string;
      lines   : string array;
   }

type interpreter_results =
   {
      iresult : (string,error list) CCError.t;
      lines   : string array;
   }

(** Stores the options passed to the command line *)
type arguments =
   {
      mutable files  : string list;
      mutable dparse : bool;
      mutable rundyn : bool;
      mutable debug  : bool;
      mutable ccode  : bool;
      mutable run_check  : bool;
      mutable output : string;
      mutable real   : string;
   }
