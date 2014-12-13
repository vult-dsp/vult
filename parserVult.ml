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
open Errors
open Types
open Lexing
open CCError

(** Parsing exception *)
exception ParserError of error

let getErrorForToken (buffer:'a lexer_stream) (message:string) : error =
   PointedError(buffer.peeked.loc,message)

let getNotExpectedTokenError (token:'a token) : error =
   let message = Printf.sprintf "Not expecting to find %s" (tokenToString token) in
   PointedError(token.loc,message)

let appendError (buffer:'a lexer_stream) (error:error) =
   buffer.errors <- error::buffer.errors

(** Skips one token *)
let skip (buffer:'a lexer_stream) : unit =
   buffer.peeked <- next_token buffer.lines buffer.lexbuf

(** Returns the current token in the buffer *)
let current (buffer:'a lexer_stream) : 'a token =
   buffer.peeked

(** Returns the kind of the current token *)
let peekKind (buffer:'a lexer_stream) : token_enum =
   (current buffer).kind

(** Consumes tokens until it finds the begining of a new statememt or the end of the current *)
let rec moveToNextStatement (buffer:'a lexer_stream) : unit =
   match buffer.peeked.kind with
   | SEMI -> skip buffer
   | EOF -> ()
   | FUN | VAL
   | IF  | RET -> ()
   | RBRAC -> skip buffer
   | _ ->
      let _ = skip buffer in
      moveToNextStatement buffer

(** Checks that the next token matches the given kind and skip it *)
let consume (buffer:'a lexer_stream) (kind:token_enum) : unit =
   match buffer.peeked with
   | t when t.kind=kind -> buffer.peeked <- next_token buffer.lines buffer.lexbuf
   | got_token ->
      let expected = kindToString kind in
      let got = tokenToString got_token in
      let message = Printf.sprintf "Expecting a %s but got %s" expected got in
      raise (ParserError(getErrorForToken buffer message))

(** Checks that the next token matches *)
let expect (buffer:'a lexer_stream) (kind:token_enum) : unit =
   match buffer.peeked with
   | t when t.kind=kind -> ()
   | got_token ->
      let expected = kindToString kind in
      let got = kindToString got_token.kind in
      let message = Printf.sprintf "Expecting a %s but got %s" expected got in
      raise (ParserError(getErrorForToken buffer message))

(** Returns an empty 'lexed_lines' type *)
let emptyLexedLines () =
   {
      current_line = Buffer.create 100;
      all_lines    = [];
   }

(** Creates a token stream given a string *)
let bufferFromString (str:string) : 'a lexer_stream =
   let lexbuf = Lexing.from_string str in
   let lines = emptyLexedLines () in
   { lexbuf = lexbuf; peeked = next_token lines lexbuf; has_errors = false; errors= []; lines = lines }

(** Creates a token stream given a channel *)
let bufferFromChannel (chan:in_channel) (file:string) : 'a lexer_stream =
   let lexbuf = Lexing.from_channel chan in
   let lines = emptyLexedLines () in
   lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = file };
   { lexbuf = lexbuf; peeked = next_token lines lexbuf ; has_errors = false; errors = []; lines = lines }

(** Returns the left binding powers of the token *)
let getLbp (token:'a token) : int =
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
let getContents (token:parse_exp token) : parse_exp =
   match token.kind,token.contents with
   | INT ,PEmpty -> PInt(token.value,token.loc)
   | ID  ,PEmpty -> PId(SimpleId(token.value,token.loc))
   | REAL,PEmpty -> PReal(token.value,token.loc)
   | _    -> token.contents

(** Parses an expression using a Pratt parser *)
let rec expression (rbp:int) (buffer:parse_exp lexer_stream) : parse_exp token =
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
and nud (buffer:parse_exp lexer_stream) (token:parse_exp token) : parse_exp token =
   match token.kind,token.value with
   | OP,"-" -> (* Unary minus *)
      unaryOp buffer token
   | ID,_   -> (* Id or function call *)
      let id = namedIdToken buffer token in
      begin
         match peekKind buffer with
         | LPAREN ->
            functionCall buffer token id
         | _ -> { token with contents = PId(id)}
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
   | IF,_ ->
      let cond = getContents (expression 0 buffer) in
      let _ = consume buffer THEN in
      let then_exp = getContents (expression 0 buffer) in
      let _ = consume buffer ELSE in
      let else_exp = getContents (expression 0 buffer) in
      { token with contents = PIf(cond,then_exp,else_exp) }
   | _ ->
      let message = getNotExpectedTokenError token in
      raise (ParserError(message))

(** Led function for the Pratt parser *)
and led (buffer:parse_exp lexer_stream) (token:parse_exp token) (left:parse_exp token) : parse_exp token =
   match token.kind,token.value with
   | OP,_ -> (* Binary operators *)
      binaryOp buffer token left
   | COMMA,_ ->
      pair buffer token left
   | _ -> token

(** <pair> :=  <expression>  ',' <expression> [ ',' <expression> ] *)
and pair (buffer:parse_exp lexer_stream) (token:parse_exp token) (left:parse_exp token) : parse_exp token =
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
and functionCall (buffer:parse_exp lexer_stream) (token:parse_exp token) (id:named_id) : parse_exp token =
   let _ = skip buffer in
   let args =
      match peekKind buffer with
      | RPAREN -> []
      | _ -> expressionList buffer
   in
   let _ = consume buffer RPAREN in
   { token with contents = PCall(id,args,token.loc) }

(** <unaryOp> := OP <expression> *)
and unaryOp (buffer:parse_exp lexer_stream) (token:parse_exp token) : parse_exp token =
   let right = expression 70 buffer in
   { token with contents = PUnOp(token.value,getContents right,token.loc) }

(** <binaryOp> := <expression> OP <expression> *)
and binaryOp (buffer:parse_exp lexer_stream) (token:parse_exp token) (left:parse_exp token) : parse_exp token =
   let right = expression (getLbp token) buffer in
   { token with contents = PBinOp(token.value,getContents left,getContents right,token.loc) }

(** <expressionList> := <expression> [',' <expression> ] *)
and expressionList (buffer:parse_exp lexer_stream) : parse_exp list =
   let rec loop acc =
      (* power of 20 avoids returning a tuple instead of a list*)
      let e = getContents (expression 20 buffer) in
      match peekKind buffer with
      | COMMA ->
         let _ = skip buffer in
         loop (e::acc)
      | _ -> List.rev (e::acc)
   in loop []

(** namedId used when the first id token has been consumed *)
and namedIdToken (buffer:parse_exp lexer_stream) (token:parse_exp token) : named_id =
   match peekKind buffer with
   | COLON ->
      let _   = skip buffer in
      let _   = expect buffer ID in
      let id1 = token.value in
      let current = current buffer in
      let id2 = current.value in
      let _   = skip buffer in
      NamedId(id1,id2,token.loc,current.loc)
   | _ -> SimpleId(token.value,token.loc)

(** namedId := <ID> [ ':' <ID>]  *)
let namedId (buffer:parse_exp lexer_stream) : named_id =
   let _     = expect buffer ID in
   let token = current buffer in
   let _     = skip buffer in
   namedIdToken buffer token

(** <optStartValue> := '(' <expression> ')' *)
let optStartValue (buffer:parse_exp lexer_stream) : parse_exp option =
   match peekKind buffer with
   | LPAREN ->
      let _ = consume buffer LPAREN in
      let e = getContents (expression 0 buffer) in
      let _ = consume buffer RPAREN in
      Some(e)
   | _ -> None

(** <valBind> := <namedId> [<optStartValue>] [ '=' <expression>] *)
let valBind (buffer:parse_exp lexer_stream) : val_bind =
   let id       = namedId buffer in
   let opt_init = optStartValue buffer in
   match peekKind buffer with
   | EQUAL ->
      let _ = skip buffer in
      let e = getContents (expression 20 (* 20 to avoid COMMA *) buffer) in
      ValBind(id,opt_init,e)
   | _ ->
      ValNoBind(id,opt_init)

(** <valBindList> := <valBind> [ ',' <valBind>] *)
let valBindList (buffer:parse_exp lexer_stream) : val_bind list =
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
let stmtVal (buffer:parse_exp lexer_stream) : parse_exp =
   let _ = consume buffer VAL in
   let vals = valBindList buffer in
   let _ = consume buffer SEMI in
   StmtVal(vals)

(** <statement> := | 'mem' <valBindList> ';' *)
let stmtMem (buffer:parse_exp lexer_stream) : parse_exp =
   let _ = consume buffer MEM in
   let vals = valBindList buffer in
   let _ = consume buffer SEMI in
   StmtMem(vals)

(** <statement> := | 'return' <expression> ';' *)
let stmtReturn (buffer:parse_exp lexer_stream) : parse_exp =
   let _ = consume buffer RET in
   let e = expression 0 buffer in
   let _ = consume buffer SEMI in
   StmtReturn(getContents e)

let stmtBind (buffer:parse_exp lexer_stream) : parse_exp =
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
      let message = Printf.sprintf "Expecting a %s while trying to parse a binding (%s = ...) but got %s" expected (PrintTypes.expressionStr e1) got in
      raise (ParserError(getErrorForToken buffer message))


(** <statement> := 'if' '(' <expression> ')' <statementList> ['else' <statementList> ]*)
let rec stmtIf (buffer:parse_exp lexer_stream) : parse_exp =
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
and stmt (buffer:parse_exp lexer_stream) : parse_exp =
   try
      match peekKind buffer with
      | VAL -> stmtVal     buffer
      | MEM -> stmtMem     buffer
      | RET -> stmtReturn  buffer
      | IF  -> stmtIf      buffer
      | FUN -> stmtFunction buffer
      | _   -> stmtBind buffer
   with
   | ParserError(error) ->
      let _ = appendError buffer error in
      let _ = moveToNextStatement buffer in
      let _ = buffer.has_errors<-true in
      StmtEmpty

(** <statementList> := LBRACK <statement> [<statement>] RBRACK *)
and stmtList (buffer:parse_exp lexer_stream) : parse_exp list =
   let rec loop acc =
      match peekKind buffer with
      | RBRAC ->
         let _ = skip buffer in
         List.rev acc
      | EOF ->
         let _ = expect buffer RBRAC in
         []
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
and stmtFunction (buffer:parse_exp lexer_stream) : parse_exp =
   let _    = consume buffer FUN in
   let name = namedId buffer in
   let _    = consume buffer LPAREN in
   let args =
      match peekKind buffer with
      | RPAREN -> []
      | _ -> valBindList buffer
   in
   let _    = consume buffer RPAREN in
   let body = stmtList buffer in
   StmtFun(name,args,body)

(** Parses an expression given a string *)
let parseExp (s:string) : parse_exp =
   let buffer = bufferFromString s in
   let result = expression 0 buffer in
   getContents result

(** Parses an statement given a string *)
let parseStmt (s:string) : parse_exp =
   let buffer = bufferFromString s in
   let result = stmt buffer in
   result

(** Parses a list of statements given a string *)
let parseStmtList (s:string) : parse_exp list =
   let buffer = bufferFromString s in
   let result = stmtList buffer in
   result

(** Parses the given expression and prints it *)
let parseDumpExp (s:string) : string =
   let e = parseExp s in
   PrintTypes.expressionStr e

(** Parses a list of statements and prints them *)
let parseDumpStmtList (s:string) : string =
   let e = parseStmtList s in
   PrintTypes.stmtListStr e

(** Parses a buffer containing a list of statements and returns the results *)
let parseBuffer (buffer) : parser_results =
   try
      let rec loop acc =
         match peekKind buffer with
         | EOF -> List.rev acc
         | _ -> loop ((stmtList buffer)::acc)
      in
      let result = loop [] |> List.flatten in
      let all_lines = getFileLines buffer.lines in
      if buffer.has_errors then
         { presult = `Error(List.rev buffer.errors); lines = all_lines }
      else
         { presult = `Ok(result); lines = all_lines }
   with
   | ParserError(error) ->
      let all_lines = getFileLines buffer.lines in
      {presult = `Error([error]); lines = all_lines }
   | _ ->
      let all_lines = getFileLines buffer.lines in
      {presult = `Error([SimpleError("Failed to parse the file")]); lines = all_lines }


(** Parses a file containing a list of statements and returns the results *)
let parseFile (filename:string) : parser_results =
   let chan = open_in filename in
   let buffer = bufferFromChannel chan filename in
   let result = parseBuffer buffer in
   let _ = close_in chan in
   result

(** Parses a string containing a list of statements and returns the results *)
let parseString (text:string) : parser_results =
   let buffer = bufferFromString text in
   let result = parseBuffer buffer in
   result
