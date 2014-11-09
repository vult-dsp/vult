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

type lexer_stream =
  {
    lexbuf : Lexing.lexbuf;
    mutable peeked : token;
  }

let skip buffer =
  buffer.peeked <- token buffer.lexbuf

let current buffer = buffer.peeked

let consume buffer kind =
  match buffer.peeked with
  | t when t.kind=kind -> buffer.peeked <- token buffer.lexbuf
  | _ -> failwith ""

let expect buffer kind =
  match buffer.peeked with
  | t when t.kind=kind -> ()
  | _ -> failwith ""

let peekKind buffer = (current buffer).kind

let bufferFromString str =
  let lexbuf = Lexing.from_string str in
  { lexbuf = lexbuf; peeked = token lexbuf }

let getLbp token =
  match token.kind,token.value with
  | OP,"+" -> 50
  | OP,"-" -> 50
  | OP,"*" -> 60
  | OP,"/" -> 60
  | _      -> 0

let getContents token =
  match token.kind,token.contents with
  | INT,PEmpty  -> PInt(token.value,token.loc)
  | ID,PEmpty   -> PId(SimpleId(token.value),token.loc)
  | REAL,PEmpty -> PReal(token.value,token.loc)
  | _    -> token.contents

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

and nud buffer token =
  match token.kind,token.value with
  | OP,"-" -> (* Unary minus *)
    unaryOp buffer token
  | ID,_   -> (* Id or function call *)
    let id = namedId buffer token in
    begin
      match peekKind buffer with
      | LPAREN ->
        functionCall buffer token id
      | _ -> { token with contents = PId(id,token.loc)}
    end
  | LPAREN,_ ->
    tuple buffer token
  | _ -> token

and led buffer token left =
  match token.kind,token.value with
  | OP,_ -> (* Binary operators *)
    binaryOp buffer token left
  | _ -> token

(** tuple  (a,b) *)
and tuple buffer token =
  let _ = skip buffer in
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

(** foo(a,b,..,c) *)
and functionCall buffer token id =
  let _ = skip buffer in
  let args =
    match peekKind buffer with
    | RPAREN -> []
    | _ -> expressionList buffer
  in
  let _ = consume buffer RPAREN in
  { token with contents = PCall(id,args,token.loc) }
(** a:b *)
and namedId buffer token =
  match peekKind buffer with
  | COLON ->
    let _ = skip buffer in
    let _ = expect buffer ID in
    let id1 = token.value in
    let id2 = (current buffer).value in
    let _ = skip buffer in
    NamedId(id1,id2)
  | _ -> SimpleId(token.value)

and expressionList buffer =
  let rec loop acc =
    let e = getContents (expression 0 buffer) in
    match peekKind buffer with
    | COMMA ->
      let _ = skip buffer in
      loop (e::acc)
    | _ -> List.rev (e::acc)
  in loop []

and unaryOp buffer token =
  let right = expression 70 buffer in
  { token with contents = PUnOp("-",getContents right) }

and binaryOp buffer token left =
  let right = expression (getLbp token) buffer in
  { token with contents = PBinOp(token.value,getContents left,getContents right) }


let printNamedId id =
  match id with
  | SimpleId(id1) -> print_string id1
  | NamedId(id1,id2) ->
    print_string id1;
    print_string ":";
    print_string id2

let rec printParseExp exp =
  match exp with
  | PId(s,_)   -> printNamedId s
  | PInt(s,_)  -> print_string s
  | PReal(s,_) -> print_string s
  | PBinOp(op,e1,e2) ->
    print_string "(";
    printParseExp e1;
    print_string op;
    printParseExp e2;
    print_string ")"
  | PUnOp(op,e) ->
    print_string "(";
    print_string op;
    printParseExp e;
    print_string ")"
  | PCall(id,args,_) ->
    printNamedId id;
    print_string "(";
    printParseExpList args;
    print_string ")"
  | PUnit -> print_string "()"
  | PTuple(elems) ->
    print_string "(";
    printParseExpList elems;
    print_string ")"
  | _ -> print_string "Empty"
and printParseExpList expl =
  match expl with
  | [] -> ()
  | [h] -> printParseExp h
  | h::t -> printParseExp h; print_string ","; printParseExpList t

let parsePrintExp s =
  let buffer = bufferFromString s in
  let result = expression 0 buffer in
  let e = getContents result in
  printParseExp e;
  print_string "\n"
