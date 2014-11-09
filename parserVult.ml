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

(** Type containing the stream of tokens *)
type lexer_stream =
  {
    lexbuf : Lexing.lexbuf;
    mutable peeked : token;
  }

(** Skips one token *)
let skip buffer =
  buffer.peeked <- token buffer.lexbuf

(** Returns the current token in the buffer *)
let current buffer = buffer.peeked

(** Checks that the next token matches the given kind and skip it *)
let consume buffer kind =
  match buffer.peeked with
  | t when t.kind=kind -> buffer.peeked <- token buffer.lexbuf
  | _ -> failwith ""

(** Checks that the next token matches *)
let expect buffer kind =
  match buffer.peeked with
  | t when t.kind=kind -> ()
  | _ -> failwith ""

(** Returns the kind of the current token *)
let peekKind buffer = (current buffer).kind

(** Creates a token stream given a string *)
let bufferFromString str =
  let lexbuf = Lexing.from_string str in
  { lexbuf = lexbuf; peeked = token lexbuf }

(** Returns the left binding powers of the token *)
let getLbp token =
  match token.kind,token.value with
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
    tuple buffer token
  | _ -> token

(** Led function for the Pratt parser *)
and led buffer token left =
  match token.kind,token.value with
  | OP,_ -> (* Binary operators *)
    binaryOp buffer token left
  | _ -> token

(** <tuple> := '(' <expression> [ ',' <expression> ] ')' *)
and tuple buffer token =
  let elems =
    match peekKind buffer with
    | RPAREN -> []
    | _ -> expressionList buffer
  in
  let _ = consume buffer RPAREN in
  let result =
    match elems with
    | []  -> PUnit
    | [h] -> h
    | _   -> PTuple(elems)
  in { token with contents = result }

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

(** <valInit> := <namedId> [ '=' <expression>] *)
let valInit buffer =
  let id    = namedId buffer in
  match peekKind buffer with
  | EQUAL ->
    let _ = skip buffer in
    let e = getContents (expression 0 buffer) in
    ValInit(id,e)
  | _ ->
    ValNoInit(id)

(** <valInitList> := <valInit> [ ',' <valInit>] *)
let valInitList buffer =
  let rec loop acc =
    let e = valInit buffer in
    match peekKind buffer with
    | COMMA ->
      let _ = skip buffer in
      loop (e::acc)
    | _ -> List.rev (e::acc)
  in let _ = expect buffer ID in
  loop []

(** <statement> := | 'val' <valInitList> ';' *)
let stmtVal buffer =
  let _ = consume buffer VAL in
  let vals = valInitList buffer in
  let _ = consume buffer SEMI in
  StmtVal(vals)

(** <statement> := | 'mem' <valInitList> ';' *)
let stmtMem buffer =
  let _ = consume buffer MEM in
  let vals = valInitList buffer in
  let _ = consume buffer SEMI in
  StmtMem(vals)

(** <statement> := | 'return' <expression> ';' *)
let stmtReturn buffer =
  let _ = consume buffer RET in
  let e = expression 0 buffer in
  let _ = consume buffer SEMI in
  StmtReturn(getContents e)

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
  match peekKind buffer with
  | VAL -> stmtVal     buffer
  | MEM -> stmtMem     buffer
  | RET -> stmtReturn  buffer
  | IF  -> stmtIf      buffer
  | FUN -> stmtFunction buffer
  | _ -> failwith "stmt"

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

(** 'fun' <namedId> '(' <valInitList> ')' <stmtList> *)
and stmtFunction buffer =
  let _    = consume buffer FUN in
  let name = namedId buffer in
  let _    = consume buffer LPAREN in
  let args = valInitList buffer in
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

let parsePrintExp s =
  let e = parseExp s in
  let print_buffer = PrintTypes.makePrintBuffer () in
  PrintTypes.expressionStr print_buffer e;
  let s = PrintTypes.contents print_buffer in
  print_string s;
  print_string "\n"

let parsePrintStmtList s =
  let e = parseStmtList s in
  let print_buffer = PrintTypes.makePrintBuffer () in
  PrintTypes.stmtListStr print_buffer e;
  let s = PrintTypes.contents print_buffer in
  print_string s;
  print_string "\n"