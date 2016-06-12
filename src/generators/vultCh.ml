(*
The MIT License (MIT)

Copyright (c) 2014 Leonardo Laguna Ruiz

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

open TypesVult
open PrintBuffer
open CLike

type real_type =
   | Float
   | Fixed

module Templates = struct

   type t =
      | None
      | Header
      | Implementation

   let get template =
      match template with
      | "none"   -> None
      | "default" -> None
      | t -> failwith (Printf.sprintf "The template '%s' is not available for this generator" t)

   let none code = code

   let header (name:string) (code:string) : string =
      let file = String.uppercase name in
      Printf.sprintf
         "#ifndef %s_H
#define %s_H
#include <stdint.h>
#include <math.h>
#include \"vultin.h\"

%s

#endif // %s_H
"
         file file code file

   let implementation (file:string) (code:string) : string =
      Printf.sprintf
         "#include \"%s.h\"

%s
"
         file code

   let apply template file code =
      match template with
      | None -> none code
      | Header -> header file code
      | Implementation -> implementation file code

end


type parameters =
   {
      real : real_type;
      template : Templates.t;
      is_header : bool;
      output : string;
      repl : Replacements.t;
   }


(** Prints the C code *)
module PrintC = struct

   let isSimple (e:cexp) : bool =
      match e with
      | CEInt _
      | CEFloat _
      | CEBool _
      | CEString _
      | CECall _
      | CEVar _
      | CENewObj -> true
      | _ -> false

   let toFixed (n:float) : string =
      let value =
         if n < 0.0 then
            Int32.of_float ((-. n) *. (float_of_int 0x10000))
            |> Int32.lognot
            |> Int32.add Int32.one
         else
            Int32.of_float (n *. (float_of_int 0x10000))
      in Printf.sprintf "0x%lx /* %f */" value n
   (*let value =
      if n < 0.0 then
         int_of_float ((-. n) *. (float_of_int 0x10000))
         |> lnot
         |> (+) 1
         |> (land) 0xFFFFFFFF
      else
         int_of_float (n *. (float_of_int 0x10000))
     in Printf.sprintf "0x%x /* %f */" value n*)

   let rec printExp (params:parameters) buffer (e:cexp) : unit =
      match e with
      | CEFloat(n) when params.real = Fixed ->
         if n < 0.0 then append buffer "(";
         append buffer (toFixed n);
         if n < 0.0 then append buffer ")"
      | CEFloat(n)  ->
         if n < 0.0 then append buffer "(";
         append buffer ((string_of_float n)^"f");
         if n < 0.0 then append buffer ")";
      | CEInt(n)    ->
         if n < 0 then append buffer "(";
         append buffer (string_of_int n);
         if n < 0 then append buffer ")"
      | CEBool(v)   -> append buffer (if v then "1" else "0")
      | CEString(s) -> append buffer ("\"" ^ s ^ "\"")
      | CEArray(elems) ->
         append buffer "{";
         printList buffer (printExp params) "," elems;
         append buffer "}"
      | CECall(name,args) ->
         append buffer name;
         append buffer "(";
         printList buffer (printExp params) "," args;
         append buffer ")"
      | CEUnOp(op,e) ->
         append buffer "(";
         append buffer op;
         append buffer " ";
         printExp params buffer e;
         append buffer ")"
      | CEOp(op,elems) ->
         append buffer "(";
         printList buffer (printExp params) (" "^(op)^" ") elems;
         append buffer ")";
      | CEVar(name) ->
         append buffer name
      | CEIf(cond,then_,else_) ->
         append buffer "(";
         printExp params buffer cond;
         append buffer "?";
         printExp params buffer then_;
         append buffer ":";
         printExp params buffer else_;
         append buffer ")"
      | CENewObj -> append buffer "{}"
      | CETuple(elems) ->
         append buffer "{ ";
         printList buffer (printChField params) ", " elems;
         append buffer " }"

   and printChField params buffer (name,value) =
      append buffer ".";
      append buffer name;
      append buffer " = ";
      printExp params buffer value

   let isSpecial name =
      match name with
      | "process" -> true
      | "noteOn" -> true
      | "noteOff" -> true
      | "controlChange" -> true
      | "default" -> true
      | _ -> false

   let rec simplifyArray (typ:type_descr) : string * string list =
      match typ with
      | CTSimple(name) -> name, []
      | CTArray(sub,size) ->
         let name,sub_size = simplifyArray sub in
         name, sub_size @ [string_of_int size]

   let printTypeDescr buffer (typ:type_descr) =
      let kind, sizes = simplifyArray typ in
      match sizes with
      | [] ->
         append buffer kind
      | _ ->
         append buffer kind;
         append buffer "[";
         printList buffer append ", " sizes;
         append buffer "]"

   let printTypeAndName (is_decl:bool) buffer (typ:type_descr) (name:string) =
      let kind, sizes = simplifyArray typ in
      if is_decl then (append buffer kind; append buffer " ");
      match sizes with
      | [] ->
         append buffer name
      | _ ->
         append buffer name;
         if is_decl then begin
            append buffer "[";
            printList buffer append ", " sizes;
            append buffer "]"
         end

   let printLhsExpTuple buffer (var:string) (is_var:bool) (i:int) (e:clhsexp) : unit =
      match e with
      | CLId(CTSimple(typ),name) ->
         if is_var then (append buffer typ; append buffer " ");
         append buffer name;
         append buffer " = ";
         append buffer var;
         append buffer (".field_"^(string_of_int i));
         append buffer "; "
      | CLId(typ,name) ->
         printTypeAndName is_var buffer typ name;
         append buffer " = ";
         append buffer var;
         append buffer (".field_"^(string_of_int i));
         append buffer "; "
      | CLWild -> ()

      | _ -> failwith "printLhsExpTuple: All other cases should be already covered"

   let printArrayBinding params buffer (var:string) (i:int) (e:cexp) : unit =
      append buffer var;
      append buffer "[";
      append buffer (string_of_int i);
      append buffer "] = ";
      printExp params buffer e;
      append buffer "; "

   let printLhsExp buffer (is_var:bool) (e:clhsexp) : unit =
      match e with
      | CLId(CTSimple(typ),name) ->
         if is_var then (append buffer typ; append buffer " ");
         append buffer name;
      | CLId(typ,name) ->
         printTypeAndName is_var buffer typ name
      | CLWild -> ()

      | _ -> failwith "printLhsExp: All other cases should be already covered"

   let printFunArg buffer (ntype,name) =
      match ntype with
      | Var(typ) ->
         printTypeDescr buffer typ;
         append buffer " ";
         append buffer name
      | Ref(typ) ->
         printTypeDescr buffer typ;
         append buffer " &";
         append buffer name

   let rec printStmt (params:parameters) buffer (stmt:cstmt) : bool =
      match stmt with
      | CSVarDecl(CLWild,None) -> false
      | CSVarDecl(CLWild,Some(value)) ->
         printExp params buffer value;
         append buffer ";";
         true
      | CSVarDecl((CLId(_,_) as lhs),Some(value)) ->
         printLhsExp buffer true lhs;
         append buffer " = ";
         printExp params buffer value;
         append buffer ";";
         true
      | CSVarDecl((CLId(_,_) as lhs),None) ->
         printLhsExp buffer true lhs;
         append buffer ";";
         true
      | CSVarDecl(CLTuple(elems),Some(CEVar(name))) ->
         List.iteri (printLhsExpTuple buffer name true) elems;
         true
      | CSVarDecl(CLTuple(_),_) -> failwith "printStmt: invalid tuple assign"
      | CSBind(CLWild,value) ->
         printExp params buffer value;
         append buffer ";";
         true
      | CSBind(CLTuple(elems),CEVar(name)) ->
         List.iteri (printLhsExpTuple buffer name false) elems;
         true
      | CSBind(CLTuple(_),_) -> failwith "printStmt: invalid tuple assign"
      | CSBind(CLId(_,name),CEArray(elems)) ->
         List.iteri (printArrayBinding params buffer name) elems;
         true
      | CSBind(CLId(_,name),value) ->
         append buffer name;
         append buffer " = ";
         printExp params buffer value;
         append buffer ";";
         true
      | CSFunction(ntype,name,args,(CSBlock(_) as body)) ->
         printTypeDescr buffer ntype;
         append buffer " ";
         append buffer name;
         append buffer "(";
         printList buffer printFunArg ", " args;
         append buffer ")";
         if params.is_header then begin
            append buffer ";";
            newline buffer;
            true
         end
         else begin
            printStmt params buffer body |> ignore;
            newline buffer;
            true
         end
      | CSFunction(ntype,name,args,body) ->
         printTypeDescr buffer ntype;
         append buffer " ";
         append buffer name;
         append buffer "(";
         printList buffer printFunArg ", " args;
         append buffer ")";
         if params.is_header then begin
            append buffer ";";
            newline buffer;
            true
         end
         else begin
            append buffer "{ ";
            printStmt params buffer body |> ignore;
            append buffer "}";
            newline buffer;
            true
         end
      | CSReturn(e1) ->
         append buffer "return ";
         printExp params buffer e1;
         append buffer ";";
         true
      | CSWhile(cond,body) ->
         append buffer "while(";
         printExp params buffer cond;
         append buffer ")";
         printStmt params buffer body;
      | CSBlock(elems) ->
         append buffer "{";
         indent buffer;
         printStmtList buffer params elems;
         outdent buffer;
         append buffer "}";
         true
      | CSIf(cond,then_,None) ->
         append buffer "if";
         if isSimple cond then append buffer "(";
         printExp params buffer cond;
         if isSimple cond then append buffer ")";
         printStmt params buffer then_;
      | CSIf(cond,then_,Some(else_)) ->
         append buffer "if";
         if isSimple cond then append buffer "(";
         printExp params buffer cond;
         if isSimple cond then append buffer ")";
         printStmt params buffer then_ |> ignore;
         newline buffer;
         append buffer "else";
         newline buffer;
         printStmt params buffer else_ |> ignore;
         true
      | CSType(name,members) when params.is_header ->
         append buffer "typedef struct ";
         append buffer name;
         append buffer " {";
         indent buffer;
         List.iter
            (fun (typ, name) ->
                printTypeAndName true buffer typ name;
                append buffer ";";
                newline buffer;
            ) members;
         outdent buffer;
         append buffer "} ";
         append buffer name;
         append buffer ";";
         newline buffer;
         true
      | CSType(_,_) -> false
      | CSAlias(t1,t2) when params.is_header ->
         append buffer "typedef ";
         append buffer t1;
         append buffer " ";
         printTypeDescr buffer t2;
         append buffer ";";
         newline buffer;
         true
      | CSAlias(_,_) -> false
      | CSExtFunc(ntype,name,args) when params.is_header ->
         printTypeDescr buffer ntype;
         append buffer " ";
         append buffer name;
         append buffer "(";
         printList buffer printFunArg ", " args;
         append buffer ")";
         append buffer ";";
         newline buffer;
         true
      | CSExtFunc _ -> false
      | CSEmpty -> false

   and printStmtList buffer (params:parameters) (stmts:cstmt list) : unit =
      match stmts with
      | [] -> ()
      | h :: t ->
         let insert_new_line = printStmt params buffer h in
         if insert_new_line then newline buffer;
         printStmtList buffer params t

   let printChCode (params:parameters) (stmts:cstmt list) : string =
      let buffer = makePrintBuffer () in
      let _      = printStmtList buffer params stmts in
      let code   = contents buffer in
      Templates.apply params.template params.output code

end

let createParameters (args:arguments) : parameters =
   let () = DefaultReplacements.initialize () in
   let real     = match args.real with | "fixed" -> Fixed | _ -> Float in
   let template = Templates.get args.template in
   let output = Filename.basename args.output in
   let repl = Replacements.getReplacements args.real in
   { real = real; template = template; is_header = false; output = output; repl = repl }

(** Generates the .c and .h file contents for the given parsed files *)
let generateChCode (args:arguments) (parser_results:parser_results list) : (string * string) list =
   let stmts =
      parser_results
      |> List.map (
         fun a -> match a.presult with
            | `Ok(b) -> b
            | _ -> [] )
      |> List.flatten
   in
   let params = createParameters args in
   let clike_stmts = VultToCLike.convertStmtList params.repl stmts in
   let h   = PrintC.printChCode { params with is_header = true; template = Templates.Header } clike_stmts in
   let cpp = PrintC.printChCode { params with is_header = false; template = Templates.Implementation } clike_stmts in
   [h,"h"; cpp,"cpp"]
