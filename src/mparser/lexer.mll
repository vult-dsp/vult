{
(* Vult DSP Language - OCaml Lexer
 * For use with the Menhir parser
 *)

open Grammar (* Reference to the parser module that Menhir will generate *)
open Lexing

(* Tokenizer configuration for controlling emission of different token types *)
type tokenizer_config = {
  emit_comments : bool;
  emit_whitespace : bool;
}

(* Default configuration for backward compatibility *)
let default_config = { emit_comments = false; emit_whitespace = false }

(* Create config with only comments enabled (backward compatibility) *)
let comment_config = { emit_comments = true; emit_whitespace = false }

(* Create config with both comments and whitespace enabled *)
let full_config = { emit_comments = true; emit_whitespace = true }

(* Helper function to update the current position for error reporting *)
let update_loc lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <- { pos with
    pos_lnum = pos.pos_lnum + 1;
    pos_bol = pos.pos_cnum;
  }

let mk_loc lexbuf =
  Util.Loc.{ 
    start_pos = Lexing.lexeme_start_p lexbuf;
    end_pos = Lexing.lexeme_end_p lexbuf;
    source = File ""
  }

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

rule token config = parse
  | whitespace+ as s   {
      if config.emit_whitespace then (
        WHITESPACE s
      ) else (
        token config lexbuf
      )
    }
  | newline            {
      update_loc lexbuf;
      if config.emit_whitespace then (
        NEWLINE
      ) else (
        token config lexbuf
      )
    }
  | "/*"               {
      let start_pos = lexbuf.lex_start_p in
      block_comment config (Buffer.create 32) start_pos lexbuf
    }
  | "//"               {
      let start_pos = lexbuf.lex_start_p in
      line_comment config (Buffer.create 32) start_pos lexbuf
    }
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
  | '"'                {
      let start_pos = lexbuf.lex_start_p in
      read_string (Buffer.create 32) start_pos lexbuf
    }
  | eof                { EOF }
  | _ as c             { Util.Error.raiseError (Printf.sprintf "Invalid character '%c' " c) (mk_loc lexbuf) }

and read_string buf start_pos = parse
  | '"'                {
      lexbuf.lex_start_p <- start_pos;
      STRING (Buffer.contents buf)
    }
  | '\\' '/'           { Buffer.add_char buf '/'; read_string buf start_pos lexbuf }
  | '\\' '\\'          { Buffer.add_char buf '\\'; read_string buf start_pos lexbuf }
  | '\\' 'b'           { Buffer.add_char buf '\b'; read_string buf start_pos lexbuf }
  | '\\' 'f'           { Buffer.add_char buf '\012'; read_string buf start_pos lexbuf }
  | '\\' 'n'           { Buffer.add_char buf '\n'; read_string buf start_pos lexbuf }
  | '\\' 'r'           { Buffer.add_char buf '\r'; read_string buf start_pos lexbuf }
  | '\\' 't'           { Buffer.add_char buf '\t'; read_string buf start_pos lexbuf }
  | '\\' _ as s        { Buffer.add_string buf s; read_string buf start_pos lexbuf }
  | newline            { Util.Error.raiseError "Unterminated string literal (newline not allowed)" (mk_loc lexbuf) }
  | [^ '"' '\\' '\r' '\n']+ as s { Buffer.add_string buf s; read_string buf start_pos lexbuf }
  | eof                { Util.Error.raiseError "String is not terminated" (mk_loc lexbuf) }

and block_comment config buf start_pos = parse
  | "*/" as s          {
      if config.emit_comments then (
        Buffer.add_string buf s;
        lexbuf.lex_start_p <- start_pos;
        BLOCK_COMMENT (Buffer.contents buf))
      else token config lexbuf
    }
  | newline as s       { Buffer.add_string buf s; update_loc lexbuf; block_comment config buf start_pos lexbuf }
  | _ as s             { Buffer.add_char buf s; block_comment config buf start_pos lexbuf }
  | eof                { Util.Error.raiseError "Comment is not terminated" (mk_loc lexbuf) }

and line_comment config buf start_pos = parse
  | newline as s       {
    update_loc lexbuf;
    if config.emit_comments then (
      Buffer.add_string buf s;
      lexbuf.lex_start_p <- start_pos;
      LINE_COMMENT (Buffer.contents buf))
    else
      token config lexbuf
    }
  | _ as s             { Buffer.add_char buf s; line_comment config buf start_pos lexbuf }
  | eof                { EOF }
