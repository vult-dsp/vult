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
      mutable insert : bool;
   }

(** Creates a print buffer *)
let makePrintBuffer () =
   { buffer = Buffer.create 100; indent = 0; space = ""; insert = false }

(** Inserts a new line to the print buffer *)
let newline buffer =
   Buffer.add_string buffer.buffer "\n";
   buffer.insert <- true

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
   if buffer.insert then
      begin
         Buffer.add_string buffer.buffer buffer.space;
         buffer.insert <- false
      end;
   Buffer.add_string buffer.buffer s

(** Returns the contents of the print buffer *)
let contents buffer =
   Buffer.contents buffer.buffer

(** Adds to the print buffer a namedId *)
let namedIdBuff buffer id =
   match id with
   | SimpleId(id1,_) -> append buffer id1
   | NamedId(id1,id2,_,_) ->
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
let rec expressionBuff buffer exp =
   match exp with
   | PId(s)   -> namedIdBuff buffer s
   | PInt(s,_)  -> append buffer s
   | PReal(s,_) -> append buffer s
   | PBinOp(op,e1,e2,_) ->
      append buffer "(";
      expressionBuff buffer e1;
      append buffer op;
      expressionBuff buffer e2;
      append buffer ")"
   | PUnOp(op,e,_) ->
      append buffer "(";
      append buffer op;
      expressionBuff buffer e;
      append buffer ")"
   | PCall(id,args,_) ->
      namedIdBuff buffer id;
      append buffer "(";
      expressionListBuff buffer args;
      append buffer ")"
   | PUnit -> append buffer "()"
   | PTuple(elems) ->
      expressionListBuff buffer elems
   | PGroup(e1) ->
      append buffer "(";
      expressionBuff buffer e1;
      append buffer ")"
   | PIf(cond,then_exp,else_exp) ->
      append buffer "if ";
      expressionBuff buffer cond;
      append buffer " then ";
      expressionBuff buffer then_exp;
      append buffer " else ";
      expressionBuff buffer else_exp
   | PEmpty -> append buffer "Empty"

(** Adds to the print buffer an expression list *)
and expressionListBuff buffer expl =
   printList buffer expressionBuff "," expl


let rec valInitBuff buffer v =
   match v with
   | ValNoBind(id,None) -> namedIdBuff buffer id
   | ValNoBind(id,Some(init)) ->
      namedIdBuff buffer id;
      append buffer "(";
      expressionBuff buffer init;
      append buffer ")";
   | ValBind(id,None,e) ->
      namedIdBuff buffer id;
      append buffer "=";
      expressionBuff buffer e
   | ValBind(id,Some(init),e) ->
      namedIdBuff buffer id;
      append buffer "(";
      expressionBuff buffer init;
      append buffer ")";
      append buffer "=";
      expressionBuff buffer e
and valInitBuffList buffer l =
   printList buffer valInitBuff "," l

let rec stmtBuff buffer stmt =
   match stmt with
   | StmtVal(elems) ->
      append buffer "val ";
      valInitBuffList buffer elems;
      append buffer ";"
   | StmtMem(elems) ->
      append buffer "mem ";
      valInitBuffList buffer elems;
      append buffer ";"
   | StmtReturn(e) ->
      append buffer "return ";
      expressionBuff buffer e;
      append buffer ";"
   | StmtIf(cond,true_stmt,None) ->
      append buffer "if(";
      expressionBuff buffer cond;
      append buffer ") ";
      stmtListBuff buffer true_stmt
   | StmtIf(cond,true_stmt,Some(false_stmt)) ->
      append buffer "if(";
      expressionBuff buffer cond;
      append buffer ") ";
      stmtListBuff buffer true_stmt;
      newline buffer;
      append buffer "else ";
      stmtListBuff buffer false_stmt;
   | StmtFun(name,args,body) ->
      append buffer "fun ";
      namedIdBuff buffer name;
      append buffer "(";
      valInitBuffList buffer args;
      append buffer ") ";
      stmtListBuff buffer body
   | StmtBind(PEmpty,e) ->
      expressionBuff buffer e;
      append buffer ";"
   | StmtBind(e1,e2) ->
      expressionBuff buffer e1;
      append buffer "=";
      expressionBuff buffer e2;
      append buffer ";"
   | StmtEmpty -> ()


and stmtListBuff buffer l =
   match l with
   | [h] -> stmtBuff buffer h
   | _ ->
      let rec loop l =
         match l with
         | [] -> ()
         | h::t ->
            stmtBuff buffer h;
            newline buffer;
            loop t
      in
      append buffer "{";
      indent buffer;
      loop l;
      outdent buffer;
      append buffer "}"

let stmtListStr e =
   let print_buffer = makePrintBuffer () in
   stmtListBuff print_buffer e;
   contents print_buffer

let expressionStr e =
   let print_buffer = makePrintBuffer () in
   expressionBuff print_buffer e;
   contents print_buffer