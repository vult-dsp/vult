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

(** Location information *)
type location =
  {
    start_pos : position;
    end_pos   : position;
  }

type named_id =
  | SimpleId of string
  | NamedId of string * string

(** Parser syntax tree *)
type parse_exp =
  | PId    of named_id * location
  | PInt   of string * location
  | PReal  of string * location
  | PBinOp of string * parse_exp * parse_exp
  | PUnOp  of string * parse_exp
  | PCall  of named_id * parse_exp list * location
  | PEmpty

(** Tokens *)
type token_enum =
  | EOF
  | INT
  | REAL
  | ID
  | FUN
  | MEM
  | VAL
  | LBRAC
  | RBRAC
  | LPAREN
  | RPAREN
  | COLON
  | SEMI
  | COMMA
  | EQUAL
  | OP

type token =
  {
    kind     : token_enum;
    value    : string;
    contents : parse_exp;
    loc      : location;
  }
