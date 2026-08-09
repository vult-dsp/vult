(*
The MIT License (MIT)

Copyright (c) 2014-2024 Leonardo Laguna Ruiz, Carl Jönsson

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
      "match",MATCH;
      "constant",CONSTANT;
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

let makeQuotedIdToken source lexbuf =
   let s = lexeme lexbuf in
   let value = String.sub s 1 (String.length s - 1) in  (* Remove the leading ' *)
   { kind = QUOTED_ID; value = value; loc = Loc.getLocation source lexbuf; }

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
   | XINT  -> "'xint'"
   | REAL  -> "'real'"
   | FIXED  -> "'fixed'"
   | ID    -> "'id'"
   | QUOTED_ID -> "'quoted_id'"
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
   | MATCH -> "'MATCH'"
   | ARROW -> "'->'"
   | CONSTANT -> "'constant'"
   | TAG -> "'@['"
   | WHITESPACE -> "'whitespace'"
   | NEWLINE -> "'newline'"
   | BLOCK_COMMENT -> "'block_comment'"
   | LINE_COMMENT -> "'line_comment'"

(** Returns a string representation of the token *)
let tokenToString l =
   match l.kind with
   | INT   -> "'"^l.value^"'"
   | XINT  -> "'"^l.value^"'"
   | REAL  -> "'"^l.value^"'"
   | ID    -> "'"^l.value^"'"
   | QUOTED_ID -> "''"^l.value^"'"
   | OP    -> "'"^l.value^"'"
   | WHITESPACE -> "'ws'"
   | BLOCK_COMMENT -> "'/*...*/'"
   | LINE_COMMENT -> "'//...'"
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

(* Original entry point - backward compatible, skips comments and whitespace *)
rule next_token source = parse
  | newline
    { let _ = updateLocation lexbuf 1 0 in (* Increases the line *)
      next_token source lexbuf
    }
  | blank +     { let _ = lexeme lexbuf in next_token source lexbuf }
  | '.'         { makeToken source DOT lexbuf }
  | "@["        { makeToken source TAG lexbuf }
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
  | "->"        { makeToken source ARROW lexbuf }
  | "||"        { makeToken source OP lexbuf }
  | "!"         { makeToken source OP lexbuf }
  | "&&"        { makeToken source OP lexbuf }
  | "=="        { makeToken source OP lexbuf }
  | "<>"        { makeToken source OP lexbuf }
  | "<="        { makeToken source OP lexbuf }
  | ">="        { makeToken source OP lexbuf }
  | ">>"        { makeToken source OP lexbuf }
  | "<<"        { makeToken source OP lexbuf }
  | '|'         { makeToken source OP lexbuf }
  | '&'         { makeToken source OP lexbuf }
  | '^'         { makeToken source OP lexbuf }
  | [ '+' '-' ] { makeToken source OP lexbuf }
  | [ '*' '/' '%' ] { makeToken source OP lexbuf }
  | '<'         { makeToken source LT lexbuf }
  | '>'         { makeToken source GT lexbuf }
  | int         { makeToken source INT lexbuf }
  | xint        { makeToken source XINT lexbuf }
  | float       { makeToken source REAL lexbuf }
  | fixed       { makeToken source FIXED lexbuf }
  | '\'' startid idchar *
                { makeQuotedIdToken source lexbuf }
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

(* Line comment that skips content - used by next_token *)
and line_comment source = parse
   newline
     {
      let _ = updateLocation lexbuf 1 0 in (* Increases the line *)
      next_token source lexbuf
     }
  | eof { makeToken source EOF lexbuf }
  | _   { line_comment source lexbuf }

(* Block comment that skips content - used by next_token *)
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

(* Configurable entry point - can emit comments and whitespace *)
and next_token_config source config = parse
  | newline
    { let _ = updateLocation lexbuf 1 0 in (* Increases the line *)
      if config.emit_whitespace then
        makeToken source NEWLINE lexbuf
      else
        next_token_config source config lexbuf
    }
  | blank + as s
    { if config.emit_whitespace then
        { kind = WHITESPACE; value = s; loc = Loc.getLocation source lexbuf }
      else
        next_token_config source config lexbuf
    }
  | '.'         { makeToken source DOT lexbuf }
  | "@["        { makeToken source TAG lexbuf }
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
  | "->"        { makeToken source ARROW lexbuf }
  | "||"        { makeToken source OP lexbuf }
  | "!"         { makeToken source OP lexbuf }
  | "&&"        { makeToken source OP lexbuf }
  | "=="        { makeToken source OP lexbuf }
  | "<>"        { makeToken source OP lexbuf }
  | "<="        { makeToken source OP lexbuf }
  | ">="        { makeToken source OP lexbuf }
  | ">>"        { makeToken source OP lexbuf }
  | "<<"        { makeToken source OP lexbuf }
  | '|'         { makeToken source OP lexbuf }
  | '&'         { makeToken source OP lexbuf }
  | '^'         { makeToken source OP lexbuf }
  | [ '+' '-' ] { makeToken source OP lexbuf }
  | [ '*' '/' '%' ] { makeToken source OP lexbuf }
  | '<'         { makeToken source LT lexbuf }
  | '>'         { makeToken source GT lexbuf }
  | int         { makeToken source INT lexbuf }
  | xint        { makeToken source XINT lexbuf }
  | float       { makeToken source REAL lexbuf }
  | fixed       { makeToken source FIXED lexbuf }
  | '\'' startid idchar *
                { makeQuotedIdToken source lexbuf }
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
  | "//"        {
                  let start_loc = Loc.getLocation source lexbuf in
                  let buffer    = Buffer.create 32 in
                  line_comment_capture source config buffer start_loc lexbuf
                }
  | "/*"        {
                  let start_loc = Loc.getLocation source lexbuf in
                  let buffer    = Buffer.create 32 in
                  block_comment_capture source config buffer start_loc 0 lexbuf
                }
  | eof         { makeToken source EOF lexbuf }
  | _ as c      {
                  let loc = Loc.getLocation source lexbuf in
                  let message = Error.PointedError(loc, Printf.sprintf "Invalid character '%c' " c) in
                  raise (Error.Errors([message]))
                }

(* Line comment that captures content - used by next_token_config *)
and line_comment_capture source config buffer start_loc = parse
   newline
     {
      let _ = updateLocation lexbuf 1 0 in
      if config.emit_comments then
        (* The location ends right before the newline, the comment does not cover it *)
        let end_loc = { Loc.start_pos = lexbuf.lex_start_p; end_pos = lexbuf.lex_start_p; source } in
        let loc = Loc.merge start_loc end_loc in
        { kind = LINE_COMMENT; value = Buffer.contents buffer; loc = loc }
      else
        next_token_config source config lexbuf
     }
  | eof {
      if config.emit_comments then
        let end_loc = Loc.getLocation source lexbuf in
        let loc = Loc.merge start_loc end_loc in
        { kind = LINE_COMMENT; value = Buffer.contents buffer; loc = loc }
      else
        makeToken source EOF lexbuf
    }
  | _ as c
    {
      let () = Buffer.add_char buffer c in
      line_comment_capture source config buffer start_loc lexbuf
    }

(* Block comment that captures content - used by next_token_config *)
and block_comment_capture source config buffer start_loc level = parse
  newline as s
     {
      let _ = updateLocation lexbuf 1 0 in
      let () = Buffer.add_string buffer s in
      block_comment_capture source config buffer start_loc level lexbuf
     }
  | "/*"
    {
      let () = Buffer.add_string buffer "/*" in
      block_comment_capture source config buffer start_loc (level+1) lexbuf
    }
  | "*/"
    {
      if level = 0 then begin
        if config.emit_comments then
          let end_loc = Loc.getLocation source lexbuf in
          let loc = Loc.merge start_loc end_loc in
          { kind = BLOCK_COMMENT; value = Buffer.contents buffer; loc = loc }
        else
          next_token_config source config lexbuf
      end else begin
        let () = Buffer.add_string buffer "*/" in
        block_comment_capture source config buffer start_loc (level-1) lexbuf
      end
    }
  | _ as c
    {
      let () = Buffer.add_char buffer c in
      block_comment_capture source config buffer start_loc level lexbuf
    }
  (* Unterminated block comment: report what was read so far so editors keep highlighting it *)
  | eof {
      if config.emit_comments then
        let end_loc = Loc.getLocation source lexbuf in
        let loc = Loc.merge start_loc end_loc in
        { kind = BLOCK_COMMENT; value = Buffer.contents buffer; loc = loc }
      else
        makeToken source EOF lexbuf
    }

and string source buffer = parse
  |  '"' { () }
  (* Escape sequences *)
  | '\\' '/'  { Buffer.add_char buffer '/'; string source buffer lexbuf }
  | '\\' '\\' { Buffer.add_char buffer '\\'; string source buffer lexbuf }
  | '\\' 'b'  { Buffer.add_char buffer '\b'; string source buffer lexbuf }
  | '\\' 'f'  { Buffer.add_char buffer '\012'; string source buffer lexbuf }
  | '\\' 'n'  { Buffer.add_char buffer '\n'; string source buffer lexbuf }
  | '\\' 'r'  { Buffer.add_char buffer '\r'; string source buffer lexbuf }
  | '\\' 't'  { Buffer.add_char buffer '\t'; string source buffer lexbuf }
  | '\\' '"'  { Buffer.add_char buffer '"'; string source buffer lexbuf }
  (* Unknown escape sequences are kept as-is *)
  | '\\' (_ as c)
      {
        let () = Buffer.add_char buffer '\\' in
        let () = Buffer.add_char buffer c in
        string source buffer lexbuf
      }
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
  | [^ '"' '\\' '\r' '\n']+ as s
      {
        let () = Buffer.add_string buffer s in
        string source buffer lexbuf
      }
  | _
      {
        let s = lexeme lexbuf in
        let () = Buffer.add_string buffer s in
        string source buffer lexbuf
      }
