{
(* Vult DSP Language - OCaml Lexer
 * For use with the Menhir parser
 *)

open Grammar (* Reference to the parser module that Menhir will generate *)
open Lexing

(* Helper function to update the current position for error reporting *)
let update_loc lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <- { pos with
    pos_lnum = pos.pos_lnum + 1;
    pos_bol = pos.pos_cnum;
  }

exception LexError of string

}

let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']
let id = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*
let int = digit+
let real = digit+ '.' digit* ('e' '-'? digit+)? | digit+ ('e' '-'? digit+)
let fixed = digit+ '.' digit* ('e' '-'? digit+)? 'x' | digit+ ('e' '-'? digit+) 'x'
let xint = '0' 'x' ['0'-'9' 'a'-'f' 'A'-'F' 'd' 'D']+
let whitespace = [' ' '\t']
let newline = '\r' | '\n' | "\r\n"

rule token = parse
  | whitespace+        { token lexbuf }
  | newline            { update_loc lexbuf; token lexbuf }
  | "/*"               { block_comment lexbuf }
  | "//"               { line_comment lexbuf }
  | "type"             { TYPE }
  | "val"              { VAL }
  | "fun"              { FUN }
  | "and"              { AND }
  | "external"         { EXTERNAL }
  | "mem"              { MEM }
  | "return"           { RETURN }
  | "if"               { IF }
  | "then"             { THEN }
  | "else"             { ELSE }
  | "while"            { WHILE }
  | "iter"             { ITER }
  | "match"            { MATCH }
  | "constant"         { CONSTANT }
  | "enum"             { ENUM }
  | "true"             { TRUE }
  | "false"            { FALSE }
  | "("                { LPAREN }
  | ")"                { RPAREN }
  | "{"                { LBRACE }
  | "}"                { RBRACE }
  | "["                { LBRACKET }
  | "]"                { RBRACKET }
  | "@["               { TAG }  (* Special token for tag start *)
  | "->"               { ARROW }
  | ":"                { COLON }
  | ";"                { SEMICOLON }
  | ","                { COMMA }
  | "."                { DOT }
  | "+"                { OP_LEVEL_1 "+" }
  | "-"                { MINUS }
  | "*"                { OP_LEVEL_0 "*"}
  | "/"                { OP_LEVEL_0 "/" }
  | "%"                { OP_LEVEL_0 "%" }
  | "=="               { OP_LEVEL_3 "==" }
  | "<>"               { OP_LEVEL_3 "<>" }
  | "<"                { OP_LEVEL_3 "<" }
  | ">"                { OP_LEVEL_3 ">" }
  | "<="               { OP_LEVEL_3 "<=" }
  | ">="               { OP_LEVEL_3 ">=" }
  | "<<"               { OP_LEVEL_2 "<<" }
  | ">>"               { OP_LEVEL_2 ">>" }
  | "="                { ASSIGN }
  | "&&"               { LAND }
  | "||"               { LOR }
  | "&"                { OP_LEVEL_0 "&" }
  | "|"                { OP_LEVEL_1 "|" }
  | "^"                { OP_LEVEL_1 "^" }
  | "_"                { WILDCARD }
  | id as s            { ID s }
  | int as s           { INT s }
  | real as s          { REAL s }
  | fixed as s         { FIXED s }
  | xint as s          { XINT s }
  | '"'                { read_string (Buffer.create 17) lexbuf }
  | eof                { EOF }
  | _ as c             { raise (LexError ("Unexpected character: " ^ String.make 1 c)) }

and read_string buf = parse
  | '"'                { STRING (Buffer.contents buf) }
  | '\\' '/'           { Buffer.add_char buf '/'; read_string buf lexbuf }
  | '\\' '\\'          { Buffer.add_char buf '\\'; read_string buf lexbuf }
  | '\\' 'b'           { Buffer.add_char buf '\b'; read_string buf lexbuf }
  | '\\' 'f'           { Buffer.add_char buf '\012'; read_string buf lexbuf }
  | '\\' 'n'           { Buffer.add_char buf '\n'; read_string buf lexbuf }
  | '\\' 'r'           { Buffer.add_char buf '\r'; read_string buf lexbuf }
  | '\\' 't'           { Buffer.add_char buf '\t'; read_string buf lexbuf }
  | '\\' _ as s        { Buffer.add_string buf s; read_string buf lexbuf }
  | [^ '"' '\\']+ as s { Buffer.add_string buf s; read_string buf lexbuf }
  | eof                { raise (LexError "String is not terminated") }

and block_comment = parse
  | "*/"               { token lexbuf }
  | newline            { update_loc lexbuf; block_comment lexbuf }
  | _                  { block_comment lexbuf }
  | eof                { raise (LexError "Comment is not terminated") }

and line_comment = parse
  | newline            { update_loc lexbuf; token lexbuf }
  | _                  { line_comment lexbuf }
  | eof                { EOF }
