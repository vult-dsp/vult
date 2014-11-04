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
open LexerVult


type lexer_stream =
  {
    lexbuf : Lexing.lexbuf;
    mutable peeked : token;
  }

let consume buffer =
  buffer.peeked <- token buffer.lexbuf

let peek buffer = buffer.peeked

let accept buffer =
  let t = buffer.peeked in
  let _  = buffer.peeked <- token buffer.lexbuf in
  t
let bufferFromString str =
  let lexbuf = Lexing.from_string str in
  { lexbuf = lexbuf; peeked = token lexbuf }

type vexp =
  | Id of string * location
  | Int of int * location
  | Real of string * location
  | BinOp of string * vexp * vexp

let rec factor buffer =
  match accept buffer with
  | ID(id,loc) -> Id(id,loc)
  | INT(i,loc) -> Int(i,loc)
  | REAL(r,loc)-> Real(r,loc)
  | LPAREN(_) ->
    let e1 = sum buffer in
    begin
      match peek buffer with
      | RPAREN(_) ->
        let _ = consume buffer in
        e1
      | _ -> failwith ("Report error in line "^(getLineBuffer ()))
    end
  | _ -> failwith ("Report error in line "^(getLineBuffer ()))

and product buffer =
  let e1 = factor buffer in
  match peek buffer with
  | OPARIT0(o,_) ->
    let _ =  consume buffer in
    let e2 = factor buffer in
    BinOp(o,e1,e2)
  | _ -> e1

and sum buffer =
  let e1 = product buffer in
  match peek buffer with
  | OPARIT1(o,_) ->
    let _ =  consume buffer in
    let e2 = product buffer in
    BinOp(o,e1,e2)
  | _ -> e1
