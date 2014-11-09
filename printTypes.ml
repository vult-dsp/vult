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

(** Conver to string the types *)

open Types

(** Type used to hold a print buffer *)
type print_buffer =
  {
    buffer : Buffer.t;
    mutable indent : int;
    mutable space  : string;
  }

(** Creates a print buffer *)
let makePrintBuffer () =
  { buffer = Buffer.create 100; indent = 0; space = "" }

(** Inserts a new line to the print buffer *)
let newline buffer =
  Buffer.add_string buffer.buffer "\n";
  Buffer.add_string buffer.buffer buffer.space

(** Inserts a new line and indents all strings appended *)
let indent buffer =
  buffer.indent <- buffer.indent + 1;
  buffer.space <- String.make (buffer.indent * 3) ' ';
  newline buffer

(** Removes one indentation step *)
let outdent buffer =
  buffer.indent <- buffer.indent - 1;
  if buffer.indent < 0 then
    failwith "Cannot outdent more";
  buffer.space <- String.make (buffer.indent * 3) ' '

(** Inserts a string to the print buffer *)
let append buffer s =
  Buffer.add_string buffer.buffer s

(** Returns the contents of the print buffer *)
let contents buffer =
  Buffer.contents buffer.buffer

(** Adds to the print buffer a namedId *)
let namedIdStr buffer id =
  match id with
  | SimpleId(id1) -> append buffer id1
  | NamedId(id1,id2) ->
    append buffer id1;
    append buffer ":";
    append buffer id2

(** Function for printing list of elements *)
let rec printList buffer f sep l =
  match l with
  | []   -> ()
  | [h]  -> f buffer h
  | h::t ->
    f buffer h;
    append buffer sep;
    printList buffer f sep t

(** Adds to the print buffer an expression *)
let rec expressionStr buffer exp =
  match exp with
  | PId(s,_)   -> namedIdStr buffer s
  | PInt(s,_)  -> append buffer s
  | PReal(s,_) -> append buffer s
  | PBinOp(op,e1,e2) ->
    append buffer "(";
    expressionStr buffer e1;
    append buffer op;
    expressionStr buffer e2;
    append buffer ")"
  | PUnOp(op,e) ->
    append buffer "(";
    append buffer op;
    expressionStr buffer e;
    append buffer ")"
  | PCall(id,args,_) ->
    namedIdStr buffer id;
    append buffer "(";
    expressionListStr buffer args;
    append buffer ")"
  | PUnit -> append buffer "()"
  | PTuple(elems) ->
    append buffer "(";
    expressionListStr buffer elems;
    append buffer ")"
  | _ -> append buffer "Empty"

(** Adds to the print buffer an expression list *)
and expressionListStr buffer expl =
  printList buffer expressionStr "," expl


let rec valInitStr buffer v =
  match v with
  | ValNoInit(id) -> namedIdStr buffer id
  | ValInit(id,e) ->
    namedIdStr buffer id;
    append buffer "=";
    expressionStr buffer e
and valInitStrList buffer l =
  printList buffer valInitStr "," l

let rec stmtStr buffer stmt =
  match stmt with
  | StmtVal(elems) ->
    append buffer "val ";
    valInitStrList buffer elems
  | StmtMem(elems) ->
    append buffer "mem ";
    valInitStrList buffer elems
  | StmtReturn(e) ->
    append buffer "return ";
    expressionStr buffer e