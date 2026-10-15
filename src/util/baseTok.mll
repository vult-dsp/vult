{
  type token =
    | Int of Int64.t
    | Float of float
    | Id of string
    | Hex of int
    | Other of string
    | EOF

}

let int = '-'? ['0'-'9'] ['0'-'9']*

let digit = ['0'-'9']
let frac = '.' digit*
let exp = ['e' 'E'] ['-' '+']? digit+
let float = ['-' '+']? digit+ frac? exp?

let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let id = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*

let hex = ['-' '+']? "0x" ['0'-'9' 'a'-'f' 'A'-'F']+


rule read =
  parse
  | white    { read lexbuf }
  | newline  { read lexbuf }
  (* Numbers that do not fit are compared as text *)
  | int      { let s = Lexing.lexeme lexbuf in match Int64.of_string_opt s with Some i -> Int i | None -> Other s }
  | float    { Float (float_of_string (Lexing.lexeme lexbuf)) }
  | id       { Id (Lexing.lexeme lexbuf) }
  | hex      { let s = Lexing.lexeme lexbuf in match int_of_string_opt s with Some i -> Hex i | None -> Other s }
  | _        { Other (Lexing.lexeme lexbuf) }
  | eof      { EOF }