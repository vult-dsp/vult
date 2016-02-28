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
open ParserTypes

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
      "type",TYPE;
      "true",TRUE;
      "false",FALSE;
      "and",AND;
      "external",EXTERNAL;
   ] in
   let _ = List.iter (fun (a,b) -> Hashtbl.add table a b) keywords in
   table


(** Stores the current line and starts a new one *)
let newLineInBuffer (lines:lexed_lines) : unit =
   let current = Buffer.contents lines.current_line in
   let _ = lines.all_lines <- current::lines.all_lines in
   Buffer.clear lines.current_line

(** Returns all the lines that have been tokenized *)
let getFileLines (lines:lexed_lines) =
   let current = Buffer.contents lines.current_line in
   Array.of_list (List.rev (current::lines.all_lines))

(**
  Appends the current lexeme to the line buffer and returns it.
  NOTE: Use this function every time you want to get the lexeme so it
  updates the lines.
 *)
let getLexeme (lines:lexed_lines) lexbuf =
   let s = lexeme lexbuf in
   let _ = Buffer.add_string lines.current_line s in
   s

(** Returs the token given the current token kind *)
let makeToken (lines:lexed_lines) kind lexbuf =
   { kind = kind; value = getLexeme lines lexbuf; loc = Loc.getLocation lexbuf; }

(** Returs the a keyword token if that's the case otherwise and id token *)
let makeIdToken lines lexbuf =
   let s = getLexeme lines lexbuf in
   let kind =
      if Hashtbl.mem keyword_table s then
         Hashtbl.find keyword_table s
      else ID
   in
   { kind = kind; value = s; loc = Loc.getLocation lexbuf; }

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
   | ID    -> "'id'"
   | STRING-> "'string'"
   | FUN   -> "'fun'"
   | MEM   -> "'mem'"
   | VAL   -> "'val'"
   | TYPE  -> "'type'"
   | RET   -> "'return'"
   | IF    -> "'if'"
   | THEN  -> "'then'"
   | ELSE  -> "'else'"
   | WHILE -> "'while'"
   | LSEQ -> "'{|'"
   | RSEQ -> "'|}'"
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
   | LARR  -> "'[|'"
   | RARR  -> "'|]'"
   | TRUE  -> "'true'"
   | FALSE -> "'false'"
   | AND   -> "'and'"
   | WILD  -> "'_'"
   | EXTERNAL -> "'external'"
   | TICK  -> "'''"
   | ARROW -> "'->'"
   | BANG  -> "'!'"

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
let startid = ['A'-'Z' 'a'-'z' '_']
let idchar = ['A'-'Z' 'a'-'z' '_' '0'-'9' '.']
let int = ['0'-'9']+
let float =
  ['0'-'9'] ['0'-'9' '_']*
  ('.' ['0'-'9' '_']* )?
  (['e' 'E'] ['+' '-']? ['0'-'9'] ['0'-'9' '_']*)?

rule next_token lines = parse
  | newline
    { let _ = updateLocation lexbuf 1 0 in (* Increases the line *)
      let _ = newLineInBuffer lines in
      next_token lines lexbuf
    }
  | blank +     { let _ = getLexeme lines lexbuf in next_token lines lexbuf }
  | '.'         { makeToken lines DOT lexbuf }
  | '@'         { makeToken lines AT lexbuf }
  | '_'         { makeToken lines WILD lexbuf }
  | '('         { makeToken lines LPAREN lexbuf }
  | ')'         { makeToken lines RPAREN lexbuf }
  | '{'         { makeToken lines LBRACE lexbuf }
  | '['         { makeToken lines LBRACK lexbuf }
  | '}'         { makeToken lines RBRACE lexbuf }
  | ']'         { makeToken lines RBRACK lexbuf }
  | "{|"        { makeToken lines LSEQ lexbuf }
  | "|}"        { makeToken lines RSEQ lexbuf }
  | "[|"        { makeToken lines LARR lexbuf }
  | "|]"        { makeToken lines RARR lexbuf }
  | ':'         { makeToken lines COLON lexbuf }
  | ';'         { makeToken lines SEMI lexbuf }
  | ','         { makeToken lines COMMA lexbuf }
  | '='         { makeToken lines EQUAL lexbuf }
  | '''         { makeToken lines TICK lexbuf }
  | "->"        { makeToken lines ARROW lexbuf }
  | "||"        { makeToken lines OP lexbuf }
  | "!"         { makeToken lines OP lexbuf }
  | "&&"        { makeToken lines OP lexbuf }
  | "=="        { makeToken lines OP lexbuf }
  | "<>"        { makeToken lines OP lexbuf }
  | "<="        { makeToken lines OP lexbuf }
  | ">="        { makeToken lines OP lexbuf }
  | [ '<' '>' ] { makeToken lines OP lexbuf }
  | '|'         { makeToken lines OP lexbuf }
  | '&'         { makeToken lines OP lexbuf }
  | [ '+' '-' ] { makeToken lines OP lexbuf }
  | [ '*' '/' '%' ] { makeToken lines OP lexbuf }
  | int         { makeToken lines INT lexbuf }
  | float       { makeToken lines REAL lexbuf }
  | startid idchar *
                { makeIdToken lines lexbuf }
  |  '"'        {
                  let start_loc = Loc.getLocation lexbuf in
                  let buffer    = Buffer.create 0 in
                  let ()        = string buffer lines lexbuf in
                  let end_loc   = Loc.getLocation lexbuf in
                  let str       = Buffer.contents buffer in
                  let loc       = Loc.merge start_loc end_loc in
                  { kind = STRING; value = str; loc = loc; }

                }
  | "//"        { line_comment lines lexbuf}
  | "/*"        { comment lines 0 lexbuf }
  | eof         { makeToken lines EOF lexbuf }

and line_comment lines = parse
   newline
     {
      let _ = updateLocation lexbuf 1 0 in (* Increases the line *)
      let _ = newLineInBuffer lines in
      next_token lines lexbuf
     }
  | eof { makeToken lines EOF lexbuf }
  | _   { line_comment lines lexbuf }

and comment lines level = parse
  newline
     {
      let _ = updateLocation lexbuf 1 0 in (* Increases the line *)
      let _ = newLineInBuffer lines in
      comment lines level lexbuf
     }
  | "/*"
    {
      comment lines (level+1) lexbuf
    }
  | "*/"
    {
      if level = 0 then
        next_token lines lexbuf
      else
        comment lines (level-1) lexbuf
    }
  | _ { comment lines level lexbuf }
  | eof { makeToken lines EOF lexbuf }

and string buffer lines = parse
  |  '"' { () }
  | '\\' newline ([' ' '\t'] * as space)
      {
        let _ = updateLocation lexbuf 1 (String.length space) in
        let _ = newLineInBuffer lines in
        let s = getLexeme lines lexbuf in
        let () = Buffer.add_string buffer s in
        string buffer lines lexbuf
      }
  | newline
      {
        let _ = updateLocation lexbuf 1 0 in
        let _ = newLineInBuffer lines in
        let s = getLexeme lines lexbuf in
        let () = Buffer.add_string buffer s in
        string buffer lines lexbuf
      }
  | eof
      { Error.raiseError "Unterminated string" (Loc.getLocation lexbuf) }
  | _
      {
        let s = getLexeme lines lexbuf in
        let () = Buffer.add_string buffer s in
        string buffer lines lexbuf
      }
