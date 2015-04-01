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
   | NamedId  of identifier * parse_exp * location
   [@@deriving show,eq,ord]

(** Parser syntax tree *)
and parse_exp =
   | PUnit
      of location
   | PBool
      of bool
      *  location
   | PInt
      of string
      *  location
   | PReal
      of string
      *  location
   | PId
      of identifier        (* name *)
      *  parse_exp option  (* type *)
      *  location
   | PTyped
      of parse_exp   (* expression *)
      *  parse_exp   (* type *)
      *  location
   | PUnOp
      of string      (* operator *)
      *  parse_exp
      *  location
   | PBinOp
      of string      (* operator *)
      *  parse_exp
      *  parse_exp
      *  location
   | PCall
      of identifier option (* name/instance *)
      *  identifier        (* type/function name *)
      *  parse_exp list    (* arguments *)
      *  location
      *  call_attributes
   | PIf
      of parse_exp (* condition *)
      *  parse_exp (* then *)
      *  parse_exp (* else *)
      *  location
   | PGroup
      of parse_exp
      *  location
   | PTuple
      of parse_exp list
      *  location
   | PSeq
      of parse_exp list
      *  location
   | PEmpty

   | StmtVal
      of parse_exp        (* names/lhs *)
      *  parse_exp option (* rhs *)
      *  location
   | StmtMem
      of parse_exp        (* names/lhs *)
      *  parse_exp option (* initial value *)
      *  parse_exp option (* rhs *)
      *  location
   | StmtWhile
      of parse_exp (* condition*)
      *  parse_exp (* statements *)
      *  location
   | StmtReturn
      of parse_exp
      *  location
   | StmtIf
      of parse_exp        (* condition *)
      *  parse_exp        (* then *)
      *  parse_exp option (* else *)
      *  location
   | StmtFun
      of identifier       (* name *)
      *  named_id list    (* arguments *)
      *  parse_exp        (* body *)
      *  parse_exp option (* return type *)
      *  location
   | StmtBind
      of parse_exp (* lhs *)
      *  parse_exp (* rhs *)
      *  location
   | StmtBlock
      of parse_exp list
      *  location
   | StmtType
      of identifier           (* name *)
      *  named_id list        (* arguments *)
      *  val_decl list option (* members *)
      *  parse_exp option     (* alias type *)
      *  location
   | StmtEmpty
   [@@deriving show,eq,ord]

and val_decl =
   identifier  (* name *)
   * parse_exp (* type *)
   * location

type parse_exp_list = parse_exp list
   [@@deriving show,eq,ord]

type parser_results =
   {
      presult : (parse_exp list,error list) CCError.t;
      lines   : string array;
   }

type interpreter_results =
   {
      iresult : (string,error list) CCError.t;
      lines   : string array;
   }