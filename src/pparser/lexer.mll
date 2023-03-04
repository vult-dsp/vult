(*
The MIT License (MIT)

Copyright (c) 2020 Leonardo Laguna Ruiz, Carl Jönsson

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
   (** Vult Lexer based on ocamllex *)
open Lexing
open Tokens
open Util

(** Updates the location of the lexbuf*)
let updateLocation lexbuf (line:int) (chars:int) : unit =
   let pos = lexbuf.lex_curr_p in
   lexbuf.lex_curr_p <- { pos with
                          pos_lnum = pos.pos_lnum + line;
                          pos_bol = pos.pos_cnum - chars;
                        }

(** Hash table contaning the keywords. The values are a function to create the keyword token *)
let keyword_table =
   let table = Hashtbl.create 50 in
   let keywords = [
      "fun",FUN;
      "mem",MEM;
      "val",VAL;
      "if",IF;
      "then",THEN;
      "else",ELSE;
      "return",RET;
      "while",WHILE;
      "iter", ITER;
      "type",TYPE;
      "enum",ENUM;
      "true",TRUE;
      "false",FALSE;
      "and",AND;
      "external",EXTERNAL;
   ] in
   let _ = List.iter (fun (a,b) -> Hashtbl.add table a b) keywords in
   table

(** Returs the token given the current token kind *)
let makeToken source kind lexbuf =
   { kind = kind; value = lexeme lexbuf; loc = Loc.getLocation source lexbuf; }

(** Returs the a keyword token if that's the case otherwise and id token *)
let makeIdToken source lexbuf =
   let s = lexeme lexbuf in
   let kind =
      if Hashtbl.mem keyword_table s then
         Hashtbl.find keyword_table s
      else ID
   in
   { kind = kind; value = s; loc = Loc.getLocation source lexbuf; }

(* Functions for testing the tokenizer *)
let tokenizeString tokenizer str =
   let lexbuf = Lexing.from_string str in
   let rec loop acc =
      match tokenizer lexbuf with
      | EOF -> List.rev acc
      | t -> loop (t::acc)
   in loop []

(** Returns a string representation of the kind *)
let kindToString kind =
   match kind with
   | EOF   -> "'eof'"
   | INT   -> "'int'"
   | REAL  -> "'real'"
   | FIXED  -> "'fixed'"
   | ID    -> "'id'"
   | STRING-> "'string'"
   | FUN   -> "'fun'"
   | MEM   -> "'mem'"
   | VAL   -> "'val'"
   | TYPE  -> "'type'"
   | ENUM  -> "'enum'"
   | RET   -> "'return'"
   | IF    -> "'if'"
   | THEN  -> "'then'"
   | ELSE  -> "'else'"
   | WHILE -> "'while'"
   | ITER  -> "'iter'"
   | LBRACE -> "'{'"
   | RBRACE -> "'}'"
   | LBRACK -> "'['"
   | RBRACK -> "']'"
   | LPAREN-> "'('"
   | RPAREN-> "')'"
   | COLON -> "':'"
   | SEMI  -> "';'"
   | COMMA -> "','"
   | EQUAL -> "'='"
   | AT    -> "'@'"
   | OP    -> "'operator'"
   | DOT   -> "'.'"
   | TRUE  -> "'true'"
   | FALSE -> "'false'"
   | AND   -> "'and'"
   | WILD  -> "'_'"
   | EXTERNAL -> "'external'"
   | LT -> "'<'"
   | GT -> "'>'"

(** Returns a string representation of the token *)
let tokenToString l =
   match l.kind with
   | INT   -> "'"^l.value^"'"
   | REAL  -> "'"^l.value^"'"
   | ID    -> "'"^l.value^"'"
   | OP    -> "'"^l.value^"'"
   | k     -> kindToString k

(** Prints the list of tokens*)
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
let startid = ['A'-'Z' 'a'-'z' '_' '$']
let idchar = ['A'-'Z' 'a'-'z' '_' '0'-'9' '$']
let int = ['0'-'9']+
let xint = "0x" ['0'-'9' 'A'-'F' 'a'-'f']+
let float =
  ['0'-'9']+
  ('.' ['0'-'9']* )?
  (['e' 'E'] ['+' '-']? ['0'-'9']+)?

let fixed = float 'x'

rule next_token source = parse
  | newline
    { let _ = updateLocation lexbuf 1 0 in (* Increases the line *)
      next_token source lexbuf
    }
  | blank +     { let _ = lexeme lexbuf in next_token source lexbuf }
  | '.'         { makeToken source DOT lexbuf }
  | '@'         { makeToken source AT lexbuf }
  | '_'         { makeToken source WILD lexbuf }
  | '('         { makeToken source LPAREN lexbuf }
  | ')'         { makeToken source RPAREN lexbuf }
  | '{'         { makeToken source LBRACE lexbuf }
  | '['         { makeToken source LBRACK lexbuf }
  | '}'         { makeToken source RBRACE lexbuf }
  | ']'         { makeToken source RBRACK lexbuf }
  | ':'         { makeToken source COLON lexbuf }
  | ';'         { makeToken source SEMI lexbuf }
  | ','         { makeToken source COMMA lexbuf }
  | '='         { makeToken source EQUAL lexbuf }
  | "||"        { makeToken source OP lexbuf }
  | "!"         { makeToken source OP lexbuf }
  | "&&"        { makeToken source OP lexbuf }
  | "=="        { makeToken source OP lexbuf }
  | "<>"        { makeToken source OP lexbuf }
  | "<="        { makeToken source OP lexbuf }
  | ">="        { makeToken source OP lexbuf }
  | ">>"        { makeToken source OP lexbuf }
  | "<<"        { makeToken source OP lexbuf }
  (*| [ '<' '>' ] { makeToken source OP lexbuf }*)
  | '<'         { makeToken source LT lexbuf}
  | '>'         { makeToken source GT lexbuf}
  | '|'         { makeToken source OP lexbuf }
  | '&'         { makeToken source OP lexbuf }
  | [ '+' '-' ] { makeToken source OP lexbuf }
  | [ '*' '/' '%' ] { makeToken source OP lexbuf }
  | int         { makeToken source INT lexbuf }
  | xint        { makeToken source INT lexbuf }
  | float       { makeToken source REAL lexbuf }
  | fixed       { makeToken source FIXED lexbuf }
  | startid idchar *
                { makeIdToken source lexbuf }
  |  '"'        {
                  let start_loc = Loc.getLocation source lexbuf in
                  let buffer    = Buffer.create 0 in
                  let ()        = string source buffer lexbuf in
                  let end_loc   = Loc.getLocation source lexbuf in
                  let str       = Buffer.contents buffer in
                  let loc       = Loc.merge start_loc end_loc in
                  { kind = STRING; value = str; loc = loc; }

                }
  | "//"        { line_comment source lexbuf}
  | "/*"        { comment source 0 lexbuf }
  | eof         { makeToken source EOF lexbuf }
  | _ as c      {
                  let loc = Loc.getLocation source lexbuf in
                  let message = Error.PointedError(loc, Printf.sprintf "Invalid character '%c' " c) in
                  raise (Error.Errors([message]))
                }

and line_comment source = parse
   newline
     {
      let _ = updateLocation lexbuf 1 0 in (* Increases the line *)
      next_token source lexbuf
     }
  | eof { makeToken source EOF lexbuf }
  | _   { line_comment source lexbuf }

and comment source level = parse
  newline
     {
      let _ = updateLocation lexbuf 1 0 in (* Increases the line *)
      comment source level lexbuf
     }
  | "/*"
    {
      comment source (level+1) lexbuf
    }
  | "*/"
    {
      if level = 0 then
        next_token source lexbuf
      else
        comment source (level-1) lexbuf
    }
  | _ { comment source level lexbuf }
  | eof { makeToken source EOF lexbuf }

and string source buffer = parse
  |  '"' { () }
  | '\\' newline ([' ' '\t'] * as space)
      {
        let _ = updateLocation lexbuf 1 (String.length space) in
        let s = lexeme lexbuf in
        let () = Buffer.add_string buffer s in
        string source buffer lexbuf
      }
  | newline
      {
        let _ = updateLocation lexbuf 1 0 in
        let s = lexeme lexbuf in
        let () = Buffer.add_string buffer s in
        string source buffer lexbuf
      }
  | eof
      { Error.raiseError "Unterminated string" (Loc.getLocation source lexbuf) }
  | _
      {
        let s = lexeme lexbuf in
        let () = Buffer.add_string buffer s in
        string source buffer lexbuf
      }
