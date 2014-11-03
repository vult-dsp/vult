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
  | ID   of (string * location)
  | FUN  of location
  | MEM  of location
  | VAL  of location
  | LBRAC  of location
  | RBRAC  of location
  | LPAREN of location
  | RPAREN of location
  | COLON  of location
  | SEMI   of location
  | COMMA  of location
  | EQUAL  of location
  | OPLOG0 of (string * location)
  | OPLOG1 of (string * location)
  | OPCOMP of (string * location)
  | OPBIT0 of (string * location)
  | OPBIT1 of (string * location)
  | OPARIT0 of (string * location)
  | OPARIT1 of (string * location)

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

(* Functions for testing the tokenizer *)
let tokenizeString tokenizer str =
  let lexbuf = Lexing.from_string str in
  let rec loop acc =
    match tokenizer lexbuf with
    | EOF -> List.rev acc
    | t -> loop (t::acc)
  in loop []

let tokenToString l =
  match l with
  | EOF -> "'eof' "
  | INT(i,_) -> "'"^(string_of_int i)^"' "
  | REAL(r,_)-> "'"^r^"' "
  | ID(s,_)  -> "'"^s^"' "
  | FUN(_)   -> "'$fun' "
  | MEM(_)   -> "'$mem' "
  | VAL(_)   -> "'$val' "
  | LBRAC(_) -> "'{' "
  | RBRAC(_) -> "'}' "
  | LPAREN(_)-> "'(' "
  | RPAREN(_)-> "')' "
  | COLON(_) -> "':' "
  | SEMI(_)  -> "';' "
  | COMMA(_) -> "',' "
  | EQUAL(_) -> "'=' "
  | OPLOG0(o,_) -> "'"^o^"' "
  | OPLOG1(o,_) -> "'"^o^"' "
  | OPCOMP(o,_) -> "'"^o^"' "
  | OPBIT0(o,_) -> "'"^o^"' "
  | OPBIT1(o,_) -> "'"^o^"' "
  | OPARIT0(o,_) -> "'"^o^"' "
  | OPARIT1(o,_) -> "'"^o^"' "

let rec printTokenList l =
  match l with
  | [] -> ()
  | h::t ->
    let _ = print_string (tokenToString h) in
    printTokenList t

}

let newline = ('\010' | '\013' | "\013\010")
let blank = [' ' '\009' '\012']
let lowercase = ['a'-'z']
let uppercase = ['A'-'Z']
let startid = ['A'-'Z' 'a'-'z' '_']
let idchar = ['A'-'Z' 'a'-'z' '_' '0'-'9']
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
  | '('         { LPAREN(getLocation lexbuf) }
  | ')'         { RPAREN(getLocation lexbuf) }
  | '{'         { LBRAC(getLocation lexbuf) }
  | '}'         { RBRAC(getLocation lexbuf) }
  | ':'         { COLON(getLocation lexbuf) }
  | ';'         { SEMI(getLocation lexbuf) }
  | ','         { COMMA(getLocation lexbuf) }
  | '='         { EQUAL(getLocation lexbuf) }
  | "||"        { OPLOG0(getString lexbuf) }
  | "&&"        { OPLOG1(getString lexbuf) }
  | "=="        { OPCOMP(getString lexbuf) }
  | "<="        { OPCOMP(getString lexbuf) }
  | ">="        { OPCOMP(getString lexbuf) }
  | [ '<' '>' ] { OPCOMP(getString lexbuf) }
  | '|'         { OPBIT1(getString lexbuf) }
  | '&'         { OPBIT0(getString lexbuf) }
  | [ '+' '-' ] { OPARIT1(getString lexbuf) }
  | [ '*' '/' '%' ] { OPARIT0(getString lexbuf) }
  | int         { INT(getInt lexbuf) }
  | float       { REAL(getString lexbuf)}
  | startid idchar *
                { getIdKeyword lexbuf }
  | eof         { EOF }