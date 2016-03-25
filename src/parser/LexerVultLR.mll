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

(** Vult Lexer based on ocamllex *)
open Lexing
open ParserVultLR

(** Updates the location of the lexbuf*)
let updateLocation lexbuf (line:int) (chars:int) : unit =
   let pos = lexbuf.lex_curr_p in
   lexbuf.lex_curr_p <- { pos with
                          pos_lnum = pos.pos_lnum + line;
                          pos_bol = pos.pos_cnum - chars;
                        }

(** Hash table contaning the keywords. The values are a function to create the keyword token *)
let keyword_table =
   let table = Hashtbl.create 16 in
   let keywords = [
      "fun",     FUN;
      "mem",     MEM;
      "val",     VAL;
      "if",      IF;
      "then",    THEN;
      "else",    ELSE;
      "return",  RETURN;
      "while",   WHILE;
      "type",    TYPE;
      "true",    TRUE;
      "false",   FALSE;
      "and",     AND;
      "external",EXTERNAL;
   ] in
   let _ = List.iter (fun (a,b) -> Hashtbl.add table a b) keywords in
   table


(**
  Appends the current lexeme to the line buffer and returns it.
  NOTE: Use this function every time you want to get the lexeme so it
  updates the lines.
 *)
let getLexeme lexbuf = lexeme lexbuf

(** Returs the a keyword token if that's the case otherwise and id token *)
let makeIdToken lexbuf =
   let s = getLexeme lexbuf in
   if Hashtbl.mem keyword_table s then
      Hashtbl.find keyword_table s
   else Ident(s)

(* Functions for testing the tokenizer *)
let tokenizeString tokenizer str =
   let lexbuf = Lexing.from_string str in
   let rec loop acc =
      match tokenizer lexbuf with
      | EOF -> List.rev acc
      | t -> loop (t::acc)
   in loop []
}

let newline = ('\010' | '\013' | "\013\010")
let blank = [' ' '\009' '\012']
let lowercase = ['a'-'z']
let uppercase = ['A'-'Z']
let startid = ['A'-'Z' 'a'-'z' '_']
let idchar = ['A'-'Z' 'a'-'z' '_' '0'-'9' '.']
let int = ['0'-'9']+
let float =
  ['0'-'9'] ['0'-'9' '_']*
  ('.' ['0'-'9' '_']* )?
  (['e' 'E'] ['+' '-']? ['0'-'9'] ['0'-'9' '_']*)?

rule next_token = parse
  | newline
    { let _ = updateLocation lexbuf 1 0 in (* Increases the line *)
      next_token lexbuf
    }
  | blank +     { let _ = getLexeme lexbuf in next_token lexbuf }
  | '.'         { DOT      }
  | "@["        { ATTR     }
  | '_'         { WILD     }
  | '('         { LPAR     }
  | ')'         { RPAR     }
  | '{'         { LBRACE   }
  | '['         { LBRACKET }
  | '}'         { RBRACE   }
  | ']'         { RBRACKET }
  | "{|"        { LSEQ     }
  | "|}"        { RSEQ     }
  | "[|"        { LARR     }
  | "|]"        { RARR     }
  | ':'         { COLON    }
  | ';'         { SEMI     }
  | ','         { COMMA    }
  | '='         { EQUAL    }
  | "->"        { ARROW    }
  | "||"        { OP_LOGIC (getLexeme lexbuf) }
  | "!"         { OP_LOGIC (getLexeme lexbuf) }
  | "&&"        { OP_LOGIC (getLexeme lexbuf) }
  | "=="        { OP_REL   (getLexeme lexbuf) }
  | "<>"        { OP_REL   (getLexeme lexbuf) }
  | "<="        { OP_REL   (getLexeme lexbuf) }
  | ">="        { OP_REL   (getLexeme lexbuf) }
  | [ '<' '>' ] { OP_REL   (getLexeme lexbuf) }
  | [ '+' '-' ] { OP_SUM   (getLexeme lexbuf) }
  | [ '*' '/' ] { OP_PROD  (getLexeme lexbuf) }
  | '%'         { OP_PROD  (getLexeme lexbuf) }
  | int         { Int  (getLexeme lexbuf) }
  | float       { Real (getLexeme lexbuf) }
  | ''' idchar +
                { Tick (getLexeme lexbuf) }
  | startid idchar *
                { makeIdToken lexbuf }
  |  '"'        {
                  let buffer    = Buffer.create 0 in
                  let ()        = string buffer lexbuf in
                  let str       = Buffer.contents buffer in
                  String(str)

                }
  | "//"        { line_comment lexbuf}
  | "/*"        { comment 0 lexbuf }
  | eof         { EOF }

and line_comment = parse
   newline
     {
      let _ = updateLocation lexbuf 1 0 in (* Increases the line *)
      next_token lexbuf
     }
  | eof { EOF }
  | _   { line_comment lexbuf }

and comment level = parse
  newline
     {
      let _ = updateLocation lexbuf 1 0 in (* Increases the line *)
      comment level lexbuf
     }
  | "/*"
    {
      comment (level+1) lexbuf
    }
  | "*/"
    {
      if level = 0 then
        next_token lexbuf
      else
        comment (level-1) lexbuf
    }
  | _ { comment level lexbuf }
  | eof { EOF }

and string buffer = parse
  |  '"' { () }
  | '\\' newline ([' ' '\t'] * as space)
      {
        let _ = updateLocation lexbuf 1 (String.length space) in
        let s = getLexeme lexbuf in
        let () = Buffer.add_string buffer s in
        string buffer lexbuf
      }
  | newline
      {
        let _ = updateLocation lexbuf 1 0 in
        let s = getLexeme lexbuf in
        let () = Buffer.add_string buffer s in
        string buffer lexbuf
      }
  | eof
      { Error.raiseError "Unterminated string" (Loc.getLocation lexbuf) }
  | _
      {
        let s = getLexeme lexbuf in
        let () = Buffer.add_string buffer s in
        string buffer lexbuf
      }
