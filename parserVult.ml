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
(** Vult Parser *)
open LexerVult

open Types
open Lexing

(** Parsing exception *)
exception ParserError of string

let getFileLocation location =
  let col_start = location.start_pos.pos_cnum - location.start_pos.pos_bol in
  let col_end = location.end_pos.pos_cnum - location.start_pos.pos_bol in
  Printf.sprintf "Error in file: %s: line %i col:%i-%i\n"
    location.start_pos.pos_fname
    location.start_pos.pos_lnum
    col_start
    col_end

let getErrorPointer line location =
  let col_start = location.start_pos.pos_cnum - location.start_pos.pos_bol in
  let col_end = location.end_pos.pos_cnum - location.start_pos.pos_bol in
  Printf.sprintf "%s\n%s%s\n"
    line
    (String.make col_start ' ')
    (String.make (col_end - col_start) '^')

let getErrorForToken buffer message =
  let file = getFileLocation buffer.peeked.loc in
  let pointer = getErrorPointer (getLastLines ()) buffer.peeked.loc in
  file^pointer^message

let getNotExpectedTokenError token =
  let message = Printf.sprintf "Not expecting to find %s\n" (tokenToString token) in
  let file = getFileLocation token.loc in
  let pointer = getErrorPointer (getLastLines ()) token.loc in
  file^pointer^message

(** Skips one token *)
let skip buffer =
  buffer.peeked <- token buffer.lexbuf

(** Returns the current token in the buffer *)
let current buffer = buffer.peeked

(** Returns the kind of the current token *)
let peekKind buffer = (current buffer).kind

let rec moveToNextStatement buffer =
  match buffer.peeked.kind with
  | SEMI -> skip buffer
  | EOF -> ()
  | FUN | VAL
  | IF | RET -> ()
  | RBRAC -> skip buffer
  | _ ->
    let _ = skip buffer in
    moveToNextStatement buffer

(** Checks that the next token matches the given kind and skip it *)
let consume buffer kind =
  match buffer.peeked with
  | t when t.kind=kind -> buffer.peeked <- token buffer.lexbuf
  | got_token ->
    let expected = kindToString kind in
    let got = tokenToString got_token in
    let message = Printf.sprintf "Expecting a %s but got %s\n" expected got in
    raise (ParserError(getErrorForToken buffer message))

(** Checks that the next token matches *)
let expect buffer kind =
  match buffer.peeked with
  | t when t.kind=kind -> ()
  | got_token ->
    let expected = kindToString kind in
    let got = kindToString got_token.kind in
    let message = Printf.sprintf "Expecting a %s but got %s\n" expected got in
    raise (ParserError(getErrorForToken buffer message))

(** Creates a token stream given a string *)
let bufferFromString str =
  let lexbuf = Lexing.from_string str in
  { lexbuf = lexbuf; peeked = token lexbuf; error = false; error_msg = [] }

(** Creates a token stream given a channel *)
let bufferFromChannel chan file =
  let lexbuf = Lexing.from_channel chan in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = file };
  { lexbuf = lexbuf; peeked = token lexbuf ; error = false; error_msg = [] }

(** Returns the left binding powers of the token *)
let getLbp token =
  match token.kind,token.value with
  | COMMA,_ -> 20
  | OP,"||" -> 30
  | OP,"&&" -> 30
  | OP,"==" -> 40
  | OP,"!=" -> 40
  | OP,">"  -> 40
  | OP,"<"  -> 40
  | OP,">=" -> 40
  | OP,"<=" -> 40
  | OP,"+"  -> 50
  | OP,"-"  -> 50
  | OP,"*"  -> 60
  | OP,"/"  -> 60
  | OP,"%"  -> 60
  | _       -> 0

(** Get the contents (the expression) stored in the token *)
let getContents token =
  match token.kind,token.contents with
  | INT,PEmpty  -> PInt(token.value,token.loc)
  | ID,PEmpty   -> PId(SimpleId(token.value),token.loc)
  | REAL,PEmpty -> PReal(token.value,token.loc)
  | _    -> token.contents

(** Parses an expression using a Pratt parser *)
let rec expression rbp buffer =
  let current_token = current buffer in
  let _             = skip buffer in
  let left          = nud buffer current_token in
  let next_token    = current buffer in
  let rec loop token left repeat =
    if repeat then
      let _         = skip buffer in
      let new_left  = led buffer token left in
      let new_token = current buffer in
      loop new_token new_left (rbp < (getLbp new_token))
    else
      left
  in loop next_token left (rbp < (getLbp next_token))

(** Nud function for the Pratt parser *)
and nud buffer token =
  match token.kind,token.value with
  | OP,"-" -> (* Unary minus *)
    unaryOp buffer token
  | ID,_   -> (* Id or function call *)
    let id = namedIdToken buffer token in
    begin
      match peekKind buffer with
      | LPAREN ->
        functionCall buffer token id
      | _ -> { token with contents = PId(id,token.loc)}
    end
  | LPAREN,_ ->
    begin
      match peekKind buffer with
      | RPAREN ->
        let _ = skip buffer in
        { token with contents = PUnit }
      | _ ->
        let e = getContents (expression 0 buffer) in
        let _ = consume buffer RPAREN in
        { token with contents = PGroup(e) }
    end
  | INT,_ | REAL,_ -> token
  | _ ->
    let message = getNotExpectedTokenError token in
    raise (ParserError(message))

(** Led function for the Pratt parser *)
and led buffer token left =
  match token.kind,token.value with
  | OP,_ -> (* Binary operators *)
    binaryOp buffer token left
  | COMMA,_ ->
    pair buffer token left
  | _ -> token

(** <pair> :=  <expression>  ',' <expression> [ ',' <expression> ] *)
and pair buffer token left  =
  let right = expression (getLbp token) buffer in
  let getElems e =
    match e with
    | PTuple(elems) -> elems
    | _ -> [e]
  in
  let elems1 = getContents left |> getElems in
  let elems2 = getContents right |> getElems in
  { token with contents = PTuple(elems1@elems2) }

(** <functionCall> := <namedId> '(' <expressionList> ')' *)
and functionCall buffer token id =
  let _ = skip buffer in
  let args =
    match peekKind buffer with
    | RPAREN -> []
    | _ -> expressionList buffer
  in
  let _ = consume buffer RPAREN in
  { token with contents = PCall(id,args,token.loc) }

(** <unaryOp> := OP <expression> *)
and unaryOp buffer token =
  let right = expression 70 buffer in
  { token with contents = PUnOp("-",getContents right) }

(** <binaryOp> := <expression> OP <expression> *)
and binaryOp buffer token left =
  let right = expression (getLbp token) buffer in
  { token with contents = PBinOp(token.value,getContents left,getContents right) }

(** <expressionList> := <expression> [',' <expression> ] *)
and expressionList buffer =
  let rec loop acc =
    let e = getContents (expression 0 buffer) in
    match peekKind buffer with
    | COMMA ->
      let _ = skip buffer in
      loop (e::acc)
    | _ -> List.rev (e::acc)
  in loop []

(** namedId used when the first id token has been consumed *)
and namedIdToken buffer token =
  match peekKind buffer with
  | COLON ->
    let _   = skip buffer in
    let _   = expect buffer ID in
    let id1 = token.value in
    let id2 = (current buffer).value in
    let _   = skip buffer in
    NamedId(id1,id2)
  | _ -> SimpleId(token.value)

(** namedId := <ID> [ ':' <ID>]  *)
let namedId buffer =
  let _     = expect buffer ID in
  let token = current buffer in
  let _     = skip buffer in
  namedIdToken buffer token

(** <optStartValue> := '(' <expression> ')' *)
let optStartValue buffer =
  match peekKind buffer with
  | LPAREN ->
    let _ = consume buffer LPAREN in
    let e = getContents (expression 0 buffer) in
    let _ = consume buffer RPAREN in
    Some(e)
  | _ -> None

(** <valBind> := <namedId> [<optStartValue>] [ '=' <expression>] *)
let valBind buffer =
  let id    = namedId buffer in
  let opt_init = optStartValue buffer in
  match peekKind buffer with
  | EQUAL ->
    let _ = skip buffer in
    let e = getContents (expression 20 (* 20 to avoid COMMA *) buffer) in
    ValBind(id,opt_init,e)
  | _ ->
    ValNoBind(id,opt_init)

(** <valBindList> := <valBind> [ ',' <valBind>] *)
let valBindList buffer =
  let rec loop acc =
    let e = valBind buffer in
    match peekKind buffer with
    | COMMA ->
      let _ = skip buffer in
      loop (e::acc)
    | _ -> List.rev (e::acc)
  in let _ = expect buffer ID in
  loop []

(** <statement> := | 'val' <valBindList> ';' *)
let stmtVal buffer =
  let _ = consume buffer VAL in
  let vals = valBindList buffer in
  let _ = consume buffer SEMI in
  StmtVal(vals)

(** <statement> := | 'mem' <valBindList> ';' *)
let stmtMem buffer =
  let _ = consume buffer MEM in
  let vals = valBindList buffer in
  let _ = consume buffer SEMI in
  StmtMem(vals)

(** <statement> := | 'return' <expression> ';' *)
let stmtReturn buffer =
  let _ = consume buffer RET in
  let e = expression 0 buffer in
  let _ = consume buffer SEMI in
  StmtReturn(getContents e)

let stmtBind buffer =
  let e1 = expression 0 buffer |> getContents in
  match peekKind buffer with
  | EQUAL ->
    let _ = consume buffer EQUAL in
    let e2 = expression 0 buffer |> getContents in
    let _ = consume buffer SEMI in
    StmtBind(e1,e2)
  | SEMI ->
    let _ = consume buffer SEMI in
    StmtBind(PEmpty,e1)
  | kind ->
    let expected = kindToString EQUAL in
    let got = kindToString kind in
    let message = Printf.sprintf "Expecting a %s while trying to parse a binding (%s = ...) but got %s\n" expected (PrintTypes.expressionStr e1) got in
    raise (ParserError(getErrorForToken buffer message))


(** <statement> := 'if' '(' <expression> ')' <statementList> ['else' <statementList> ]*)
let rec stmtIf buffer =
  let _    = consume buffer IF in
  let _    = consume buffer LPAREN in
  let cond = getContents (expression 0 buffer) in
  let _    = consume buffer RPAREN in
  let tstm = stmtList buffer in
  match peekKind buffer with
  | ELSE ->
    let _ = consume buffer ELSE in
    let fstm = stmtList buffer in
    StmtIf(cond,tstm,Some(fstm))
  | _ -> StmtIf(cond,tstm,None)

(** <statement> := ... *)
and stmt buffer =
  try
    match peekKind buffer with
    | VAL -> stmtVal     buffer
    | MEM -> stmtMem     buffer
    | RET -> stmtReturn  buffer
    | IF  -> stmtIf      buffer
    | FUN -> stmtFunction buffer
    | _   -> stmtBind buffer
  with
  | ParserError(message) ->
    let _ = print_string message in
    let _ = moveToNextStatement buffer in
    let _ = buffer.error<-true in
    StmtEmpty

(** <statementList> := '{' <statement> [<statement>] '}' *)
and stmtList buffer =
  let rec loop acc =
    match peekKind buffer with
    | RBRAC ->
      let _ = skip buffer in
      List.rev acc
    | _ ->
      let s = stmt buffer in
      loop (s::acc)
  in
  match peekKind buffer with
  | LBRAC ->
    let _ = skip buffer in
    loop []
  | _ ->
    let s = stmt buffer in
    [s]

(** 'fun' <namedId> '(' <valBindList> ')' <stmtList> *)
and stmtFunction buffer =
  let _    = consume buffer FUN in
  let name = namedId buffer in
  let _    = consume buffer LPAREN in
  let args = valBindList buffer in
  let _    = consume buffer RPAREN in
  let body = stmtList buffer in
  StmtFun(name,args,body)

(** Parses an expression given a string *)
let parseExp s =
  let buffer = bufferFromString s in
  let result = expression 0 buffer in
  getContents result

(** Parses an statement given a string *)
let parseStmt s =
  let buffer = bufferFromString s in
  let result = stmt buffer in
  result

(** Parses a list of statements given a string *)
let parseStmtList s =
  let buffer = bufferFromString s in
  let result = stmtList buffer in
  result

let parseDumpExp s =
  let e = parseExp s in
  PrintTypes.expressionStr e

let parseDumpStmtList s =
  let e = parseStmtList s in
  PrintTypes.stmtListStr e

let parseFile filename =
  let chan = open_in filename in
  try
    let buffer = bufferFromChannel chan filename in
    let rec loop acc =
      match peekKind buffer with
      | EOF -> List.rev acc
      | _ -> loop ((stmtList buffer)::acc)
    in
    let result = loop [] |> List.flatten in
    let _ = close_in chan in
    if buffer.error then
      None
    else
      Some(result)
  with
  | ParserError(message) ->
    let _ = print_string message in
    let _ = close_in chan in
    failwith "Failed to parse the file"
  | _ ->
    let _ = close_in chan in
    failwith "Failed to parse the file"


