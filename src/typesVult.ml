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
   | PUnit  of location
   | PBool  of bool       * location
   | PInt   of string     * location
   | PReal  of string     * location
   | PId    of identifier * parse_exp option * location
   | PTyped of parse_exp  * parse_exp        * location
   | PUnOp  of string     * parse_exp        * location
   | PBinOp of string     * parse_exp        * parse_exp * location
   | PCall  of identifier option  * identifier * parse_exp list   * location  * call_attributes
   | PIf    of parse_exp  * parse_exp        * parse_exp * location
   | PGroup of parse_exp  * location
   | PTuple of parse_exp list * location
   | PSeq   of parse_exp list * location
   | PEmpty
   | StmtVal      of parse_exp * parse_exp option * location
   | StmtMem      of parse_exp * parse_exp option * parse_exp option * location
   | StmtWhile    of parse_exp * parse_exp * location
   | StmtReturn   of parse_exp      * location
   | StmtIf       of parse_exp      * parse_exp * parse_exp option * location
   | StmtFun      of identifier     * named_id list  * parse_exp   * parse_exp option * location
   | StmtBind     of parse_exp      * parse_exp      * location
   | StmtBlock    of parse_exp list * location
   | StmtType     of identifier     * named_id list  * val_decl list option * parse_exp option * location
   | StmtEmpty
         [@@deriving show,eq,ord]

and val_decl =
   identifier * parse_exp * location

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