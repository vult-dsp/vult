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

(** Printing of types *)

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

(** Function for printing list of elements *)
let rec printList buffer f sep l =
   match l with
   | []   -> ()
   | [h]  -> f buffer h
   | h::t ->
      f buffer h;
      append buffer sep;
      printList buffer f sep t

(** Adds to the print buffer a namedId *)
let namedIdBuff buffer id =
   match id with
   | SimpleId(id1,_) -> append buffer id1
   | NamedId(id1,id2,_,_) ->
      append buffer id1;
      append buffer ":";
      append buffer id2

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
   | PCall(id,args,_,_) ->
      namedIdBuff buffer id;
      append buffer "(";
      expressionListBuff buffer args;
      append buffer ")"
   | PUnit(_) -> append buffer "()"
   | PTuple(elems,_) ->
      expressionListBuff buffer elems
   | PGroup(e1,_) ->
      append buffer "(";
      expressionBuff buffer e1;
      append buffer ")"
   | PIf(cond,then_exp,else_exp,_) ->
      append buffer "if ";
      expressionBuff buffer cond;
      append buffer " then ";
      expressionBuff buffer then_exp;
      append buffer " else ";
      expressionBuff buffer else_exp
   | PSeq(stmts,_) ->
      pseqListBuff buffer stmts;
   | PEmpty -> append buffer "Empty"

   | StmtVal(e1,Some(e2),_) ->
      append buffer "val ";
      expressionBuff buffer e1;
      append buffer "=";
      expressionBuff buffer e2;
      append buffer ";"
   | StmtVal(e1,None,_) ->
      append buffer "val ";
      expressionBuff buffer e1;
      append buffer ";"
   | StmtMem(e1,e2,e3,_) ->
      append buffer "mem ";
      expressionBuff buffer e1;
      CCOpt.iter (fun a ->
         append buffer "@";
         expressionBuff buffer a) e2;
      CCOpt.iter (fun a ->
         append buffer "=";
         expressionBuff buffer a) e3;
      append buffer ";"
   | StmtReturn(e,_) ->
      append buffer "return ";
      expressionBuff buffer e;
      append buffer ";"
   | StmtIf(cond,true_stmt,None,_) ->
      append buffer "if(";
      expressionBuff buffer cond;
      append buffer ") ";
      stmtListBuff buffer true_stmt
   | StmtIf(cond,true_stmt,Some(false_stmt),_) ->
      append buffer "if(";
      expressionBuff buffer cond;
      append buffer ") ";
      stmtListBuff buffer true_stmt;
      newline buffer;
      append buffer "else ";
      stmtListBuff buffer false_stmt;
   | StmtFun(name,args,body,_) ->
      append buffer "fun ";
      namedIdBuff buffer name;
      append buffer "(";
      printList buffer namedIdBuff "," args;
      append buffer ") ";
      stmtListBuff buffer body
   | StmtBind(PUnit(_),e,_) ->
      expressionBuff buffer e;
      append buffer ";"
   | StmtBind(e1,e2,_) ->
      expressionBuff buffer e1;
      append buffer "=";
      expressionBuff buffer e2;
      append buffer ";"
   | StmtBlock(stmts,_) ->
      stmtListBuff buffer stmts;
   | StmtEmpty -> ()

(** Adds to the print buffer an expression list *)
and expressionListBuff buffer expl =
   printList buffer expressionBuff "," expl

(** Adds to the print buffer a statement in a block list *)
and stmtListBuff buffer expl =
      match expl with
      | [h] -> expressionBuff buffer h
      | _ ->
         let rec loop l =
            match l with
            | [] -> ()
            | h::t ->
               expressionBuff buffer h;
               newline buffer;
               loop t
         in
         append buffer "{";
         indent buffer;
         loop expl;
         outdent buffer;
         append buffer "}"

(** Adds to the print buffer a statement in a block list *)
and pseqListBuff buffer expl =
         let rec loop l =
            match l with
            | [] -> ()
            | h::t ->
               expressionBuff buffer h;
               newline buffer;
               loop t
         in
         append buffer "{|";
         indent buffer;
         loop expl;
         outdent buffer;
         append buffer "|}"

(** Converts to string a list of statememts *)
let stmtListStr e =
   let print_buffer = makePrintBuffer () in
   stmtListBuff print_buffer e;
   contents print_buffer

(** Converts to string an expression *)
let expressionStr e =
   let print_buffer = makePrintBuffer () in
   expressionBuff print_buffer e;
   contents print_buffer