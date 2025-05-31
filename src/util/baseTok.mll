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
  | int      { Int (Int64.of_string (Lexing.lexeme lexbuf)) }
  | float    { Float (float_of_string (Lexing.lexeme lexbuf)) }
  | id       { Id (Lexing.lexeme lexbuf) }
  | hex      { Hex (int_of_string (Lexing.lexeme lexbuf)) }
  | _        { Other (Lexing.lexeme lexbuf) }
  | eof      { EOF }