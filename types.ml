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
   | ELSE
   | LBRAC
   | RBRAC
   | LPAREN
   | RPAREN
   | COLON
   | SEMI
   | COMMA
   | EQUAL
   | OP

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
      lines              : lexed_lines;
   }

type named_id =
   | SimpleId of string
   | NamedId  of string * string

(** Parser syntax tree *)
type parse_exp =
   | PUnit
   | PInt   of string   * location
   | PReal  of string   * location
   | PId    of named_id * location
   | PUnOp  of string   * parse_exp
   | PBinOp of string   * parse_exp      * parse_exp
   | PCall  of named_id * parse_exp list * location
   | PGroup of parse_exp
   | PTuple of parse_exp list
   | PEmpty

type val_bind =
   | ValBind   of named_id * parse_exp option * parse_exp
   | ValNoBind of named_id * parse_exp option

type stmt =
   | StmtVal    of val_bind list
   | StmtMem    of val_bind list
   | StmtReturn of parse_exp
   | StmtIf     of parse_exp * stmt list     * (stmt list) option
   | StmtFun    of named_id  * val_bind list * stmt list
   | StmtBind   of parse_exp * parse_exp
   | StmtEmpty

type parser_results =
   {
      presult : (error list,stmt list) Either.either;
      lines   : string array;
   }

type interpreter_results =
   {
      iresult : (error list,unit) Either.either;
      lines   : string array;
   }