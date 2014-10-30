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
{

open Lexing

(* Location information *)
type location =
  {
    start_pos : position;
    end_pos   : position;
  }

let getLocation lexbuf =
  {
    start_pos = lexbuf.lex_start_p;
    end_pos   = lexbuf.lex_curr_p;
  }

let updateLocation lexbuf line chars =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <- { pos with
    pos_lnum = pos.pos_lnum + line;
    pos_bol = pos.pos_cnum - chars;
  }

(* Tokens *)
type token =
  | EOF
  | INT  of (int * location)
  | REAL of (string * location)
  | ID  of (string * location)
  | FUN  of location
  | MEM  of location
  | VAL  of location

(* Keywords *)
let keyword_table =
  let table = Hashtbl.create 50 in
  let keywords = [
      "fun",(fun loc -> FUN(loc));
      "mem",(fun loc -> MEM(loc));
      "val",(fun loc -> VAL(loc));
    ] in
  let _ = List.iter (fun (a,b) -> Hashtbl.add table a b) keywords in
  table

(* Auxiliary functions for processing the lexeme buffer *)

let getString lexbuf = (lexeme lexbuf, getLocation lexbuf)
let getInt    lexbuf = (int_of_string (lexeme lexbuf), getLocation lexbuf)
let getIdKeyword lexbuf =
  let s,loc = getString lexbuf in
  if Hashtbl.mem keyword_table s then
    (Hashtbl.find keyword_table s) loc
  else ID(s,loc)

}

let newline = ('\010' | '\013' | "\013\010")
let blank = [' ' '\009' '\012']
let lowercase = ['a'-'z']
let uppercase = ['A'-'Z']
let identchar = ['A'-'Z' 'a'-'z' '_']
let int = ['0'-'9'] ['0'-'9' '_']*
let float =
  ['0'-'9'] ['0'-'9' '_']*
  ('.' ['0'-'9' '_']* )?
  (['e' 'E'] ['+' '-']? ['0'-'9'] ['0'-'9' '_']*)?

rule token =
  parse
  | newline
    { let _ = updateLocation lexbuf 1 0 in (* Increases the line *)
      token lexbuf
    }
  | blank +     { token lexbuf }
  | int         { INT(getInt lexbuf) }
  | float       { REAL(getString lexbuf)}
  | identchar + { getIdKeyword lexbuf }
  | eof         { EOF }