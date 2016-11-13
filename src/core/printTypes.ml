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

open TypesVult
open PrintBuffer

(** Add an identifier to the print buffer *)
let identifierBuff buffer id =
   printList buffer append "." id

let commentedId buffer id =
   match id with
   | Some(ids) ->
      append buffer "/* ";
      identifierBuff buffer ids;
      append buffer " */";
      newline buffer
   | _ -> ()

let rec typeExpressionBuff buffer (tp:VType.t) =
   match !tp with
   | VType.TId(id,_) ->
      identifierBuff buffer id
   | VType.TInt(n,_) ->
      append buffer (string_of_int n)
   | VType.TComposed(id,args,_) ->
      identifierBuff buffer id;
      append buffer "(";
      typeExpressionListBuff buffer args;
      append buffer ")"
   | VType.TArrow(t1,t2,_) ->
      typeExpressionBuff buffer t1;
      append buffer " -> ";
      typeExpressionBuff buffer t2
   | VType.TUnbound(name,_,_) -> append buffer name;
   | VType.TLink(tp) -> typeExpressionBuff buffer tp
   | VType.TExpAlt(expl) ->
      append buffer "(";
      printList buffer typeExpressionBuff " | " expl;
      append buffer ")"
and typeExpressionListBuff buffer expl =
   printList buffer typeExpressionBuff "," expl

let rec lhsExpressionBuff buffer (lhs:lhs_exp) =
   match lhs with
   | LWild({typ=Some(tp)}) ->
      append buffer "(_:";
      typeExpressionBuff buffer tp;
      append buffer ")";
   | LWild(_) ->
      append buffer "_"
   | LId(id,None,_) ->
      identifierBuff buffer id
   | LId(id,Some(tp),_) ->
      append buffer "(";
      identifierBuff buffer id;
      append buffer ":";
      typeExpressionBuff buffer tp;
      append buffer ")"
   | LTuple(elems,_) ->
      append buffer "(";
      lhsExpressionListBuff buffer elems;
      append buffer ")"
   | LTyped(e,tp,_) ->
      append buffer "(";
      lhsExpressionBuff buffer e;
      append buffer ":";
      typeExpressionBuff buffer tp;
      append buffer ")"
   | LGroup(elems,_) ->
      append buffer "(";
      lhsExpressionBuff buffer elems;
      append buffer ")"
and lhsExpressionListBuff buffer expl =
   printList buffer lhsExpressionBuff "," expl

(** Adds to the print buffer a namedId *)
let rec typedArgBuff buffer id =
   match id with
   | SimpleId(id1,_,_) -> identifierBuff buffer id1
   | TypedId(["_"],id_type,_,_) ->
      typeExpressionBuff buffer id_type
   | TypedId(id1,id_type,_,_) ->
      identifierBuff buffer id1;
      append buffer ":";
      typeExpressionBuff buffer id_type

(** Adds to the print buffer an expression *)
and expressionBuff buffer (exp:exp) =
   match exp with
   | PId(s,_)   ->
      identifierBuff buffer s
   | PInt(s,_)  -> append buffer (string_of_int s)
   | PReal(s,_) -> append buffer (string_of_float s)
   | PBool(true,_)  -> append buffer "true"
   | PBool(false,_) -> append buffer "false"
   | PArray(elems,_) ->
      append buffer "[";
      printArray buffer expressionBuff ", " elems;
      append buffer "]"
   | POp(op,args,_) ->
      append buffer "(";
      printList buffer expressionBuff (" "^op^" ") args;
      append buffer ")"
   | PUnOp(op,e,_) ->
      append buffer "(";
      append buffer op;
      expressionBuff buffer e;
      append buffer ")"
   | PCall(id,fname,args,_) ->
      CCOpt.iter (fun a ->
            identifierBuff buffer a;
            append buffer ":") id;
      identifierBuff buffer fname;
      append buffer "(";
      expressionListBuff buffer args;
      append buffer ")"
   | PUnit(_) -> append buffer "()"
   | PTuple(elems,_) ->
      append buffer "(";
      expressionListBuff buffer elems;
      append buffer ")"
   | PGroup(e1,_) ->
      append buffer "(";
      expressionBuff buffer e1;
      append buffer ")"
   | PIf(cond,then_exp,else_exp,_) ->
      append buffer "(if ";
      expressionBuff buffer cond;
      append buffer " then ";
      expressionBuff buffer then_exp;
      append buffer " else ";
      expressionBuff buffer else_exp;
      append buffer ")"
   | PSeq(name,stmts,_) ->
      commentedId buffer name;
      pseqListBuff buffer stmts;
   | PEmpty -> append buffer "Empty"

and stmtBuff buffer (s:stmt) =
   match s with
   | StmtVal(e1,Some(e2),_) ->
      append buffer "val ";
      lhsExpressionBuff buffer e1;
      append buffer " = ";
      expressionBuff buffer e2;
      append buffer ";"
   | StmtVal(e1,None,_) ->
      append buffer "val ";
      lhsExpressionBuff buffer e1;
      append buffer ";"
   | StmtMem(e1,e3,_) ->
      append buffer "mem ";
      lhsExpressionBuff buffer e1;
      CCOpt.iter (fun a ->
            append buffer " = ";
            expressionBuff buffer a) e3;
      append buffer ";"
   | StmtReturn(e,_) ->
      append buffer "return ";
      expressionBuff buffer e;
      append buffer ";"
   | StmtIf(cond,true_stmt,None,_) ->
      append buffer "if(";
      expressionBuff buffer cond;
      append buffer ")";
      indent buffer;
      stmtBuff buffer true_stmt;
      outdent buffer
   | StmtIf(cond,true_stmt,Some(false_stmt),_) ->
      append buffer "if(";
      expressionBuff buffer cond;
      append buffer ")";
      indent buffer;
      stmtBuff buffer true_stmt;
      outdent buffer;
      newline buffer;
      append buffer "else";
      indent buffer;
      stmtBuff buffer false_stmt;
      outdent buffer
   | StmtFun(name,args,body,vtype,attr) ->
      append buffer (if attr.fun_and then "and " else "fun ");
      identifierBuff buffer name;
      append buffer "(";
      printList buffer typedArgBuff "," args;
      append buffer ") ";
      CCOpt.iter(fun a ->
            append buffer ": ";
            typeExpressionBuff buffer a;
            append buffer " ") vtype;
      stmtBuff buffer body;
      newline buffer
   | StmtExternal(name,args,vtype,link_name,_) ->
      append buffer "external ";
      identifierBuff buffer name;
      append buffer "(";
      printList buffer typedArgBuff "," args;
      append buffer ") : ";
      typeExpressionBuff buffer vtype;
      append buffer " \"";
      append buffer link_name;
      append buffer "\"";
      append buffer ";"
   | StmtBind(e1,e2,_) ->
      lhsExpressionBuff buffer e1;
      append buffer " = ";
      expressionBuff buffer e2;
      append buffer ";"
   | StmtBlock(name,stmts,_) ->
      commentedId buffer name;
      stmtListBuff buffer stmts;
   | StmtWhile(cond,stmts,_) ->
      append buffer "while(";
      expressionBuff buffer cond;
      append buffer ")";
      stmtBuff buffer stmts
   | StmtAliasType(type_name,alias,_) ->
      append buffer "type ";
      typeExpressionBuff buffer type_name;
      append buffer " : ";
      typeExpressionBuff buffer alias;
      append buffer ";"
   | StmtType(type_name,decl_list,_) ->
      append buffer "type ";
      typeExpressionBuff buffer type_name;
      append buffer " {";
      indent buffer;
      List.iter (valDecl buffer) decl_list;
      outdent buffer;
      append buffer "}";
      newline buffer
   | StmtEmpty -> ()

(** Adds to the print buffer an expression list *)
and expressionListBuff buffer expl =
   printList buffer expressionBuff "," expl

(** Adds to the print buffer a statement in a block list *)
and stmtListBuff buffer (expl:stmt list) =
   match expl with
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
      loop expl;
      outdent buffer;
      append buffer "}"

(** Adds to the print buffer a statement in a block list *)
and pseqListBuff buffer expl =
   let stmtList stmt =
      match stmt with
      | StmtBlock(_,stmts,_) -> stmts
      | _ -> [stmt]
   in
   let rec loop l =
      match l with
      | [] -> ()
      | h::t ->
         stmtBuff buffer h;
         newline buffer;
         loop t
   in
   append buffer "{|";
   indent buffer;
   loop (stmtList expl);
   outdent buffer;
   append buffer "|}"

(** Adds a val declaration part of a type definition *)
and valDecl buffer val_decl =
   let id,e,_ = val_decl in
   append buffer "val ";
   identifierBuff buffer id;
   append buffer " : ";
   typeExpressionBuff buffer e;
   append buffer ";";
   newline buffer

let identifierStr id =
   let print_buffer = makePrintBuffer () in
   identifierBuff print_buffer id;
   contents print_buffer

(** Converts to string a list of statememts *)
let stmtListStr e =
   let print_buffer = makePrintBuffer () in
   stmtListBuff print_buffer e;
   contents print_buffer

(** Converts to string a statememt *)
let stmtStr e =
   let print_buffer = makePrintBuffer () in
   stmtBuff print_buffer e;
   contents print_buffer

(** Converts to string an lhs expression *)
let lhsExpressionStr e =
   let print_buffer = makePrintBuffer () in
   lhsExpressionBuff print_buffer e;
   contents print_buffer

(** Converts to string an expression *)
let expressionStr e =
   let print_buffer = makePrintBuffer () in
   expressionBuff print_buffer e;
   contents print_buffer

(** Converts to string an type expression *)
let typeStr (e:VType.t) =
   let print_buffer = makePrintBuffer () in
   typeExpressionBuff print_buffer e;
   contents print_buffer
