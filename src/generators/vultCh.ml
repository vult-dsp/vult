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
      Pla.print
{pla|#ifndef <#file#s>_H
#define <#file#s>_H
#include <stdint.h>
#include <math.h>
#include "vultin.h"

<#code#s>

#endif // <#file#s>_H
|pla}

   let implementation (file:string) (code:string) : string =
      Pla.print
{pla|#include "<#file#s>.h"

<#code#s>
|pla}

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

   let rec printExp (params:parameters) (e:cexp) : Pla.template =
      match e with
      | CEFloat(n) when params.real = Fixed ->
         let sn = toFixed n in
         if n < 0.0 then {pla|(<#sn#s>)|pla} else Pla.string sn
      | CEFloat(n) ->
         if n < 0.0 then {pla|(<#n#f>f)|pla} else {pla|<#n#f>f|pla}
      | CEInt(n) ->
         if n < 0 then {pla|(<#n#i>)|pla} else {pla|<#n#i>|pla}
      | CEBool(v)   -> Pla.int (if v then 1 else 0)
      | CEString(s) -> {pla|"<#s#s>"|pla}
      | CEArray(elems) ->
         let telems = Pla.map_sep Pla.comma (printExp params) elems in
         {pla|{<#telems#>}|pla}
      | CECall(name,args) ->
         let targs = Pla.map_sep Pla.comma (printExp params) args in
         {pla|<#name#s>(<#targs#>)|pla}
      | CEUnOp(op,e) ->
         let te = printExp params e in
         {pla|(<#op#s> <#te#>)|pla}
      | CEOp(op,elems) ->
         let sop = {pla| <#op#s> |pla} in
         let telems = Pla.map_sep sop (printExp params) elems in
         {pla|(<#telems#>)|pla}
      | CEVar(name) ->
         Pla.string name
      | CEIf(cond,then_,else_) ->
         let tcond = printExp params cond in
         let tthen = printExp params then_ in
         let telse = printExp params else_ in
         {pla|(<#tcond#>?<#tthen#>:<#telse#>)|pla}
      | CENewObj -> Pla.string "{}"
      | CETuple(elems) ->
         let telems = Pla.map_sep Pla.comma (printChField params) elems in
         {pla|{ <#telems#> }|pla}

   and printChField params (name,value) =
      let tval = printExp params value in
      {pla|.<#name#s> = <#tval#>|pla}

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

   let printTypeDescr (typ:type_descr) : Pla.template =
      let kind, sizes = simplifyArray typ in
      match sizes with
      | [] -> Pla.string kind
      | _ ->
         let tsize = Pla.map_sep Pla.comma Pla.string sizes in
         {pla|<#kind#s>[<#tsize#>]|pla}

   let printTypeAndName (is_decl:bool) (typ:type_descr) (name:string) : Pla.template =
      let kind, sizes = simplifyArray typ in
      match is_decl, sizes with
      | true,[] -> {pla|<#kind#s> <#name#s>|pla}
      | true,_  ->
            let t_sizes = Pla.map_sep Pla.comma Pla.string sizes in
            {pla|<#kind#s> <#name#s>[<#t_sizes#>]|pla}
      | _,_ -> {pla|<#name#s>|pla}

   let printLhsExpTuple (var:string) (is_var:bool) (i:int) (e:clhsexp) : Pla.template =
      match e with
      | CLId(CTSimple(typ),name) ->
         if is_var then
            {pla|<#typ#s> <#name#s> = <#var#s>.field_<#i#i>;|pla}
         else
            {pla|<#name#s> = <#var#s>.field_<#i#i>;|pla}
      | CLId(typ,name) ->
         let tdecl = printTypeAndName is_var typ name in
         {pla|<#tdecl#> = <#var#s>.field_<#i#i>;|pla}
      | CLWild -> Pla.unit

      | _ -> failwith "printLhsExpTuple: All other cases should be already covered"

   let printArrayBinding params (var:string) (i:int) (e:cexp) : Pla.template =
      let te = printExp params e in
      {pla|<#var#s>[<#i#i>] = <#te#>; |pla}

   let printLhsExp (is_var:bool) (e:clhsexp) : Pla.template =
      match e with
      | CLId(CTSimple(typ),name) when is_var ->
         {pla|<#typ#s> <#name#s>|pla}
      | CLId(CTSimple(_),name) ->
         Pla.string name
      | CLId(typ,name) ->
         printTypeAndName is_var typ name
      | CLWild -> Pla.unit

      | _ -> failwith "printLhsExp: All other cases should be already covered"

   let printFunArg (ntype,name) : Pla.template =
      match ntype with
      | Var(typ) ->
         let tdescr = printTypeDescr typ in
         {pla|<#tdescr#> <#name#s>|pla}
      | Ref(typ) ->
         let tdescr = printTypeDescr typ in
         {pla|<#tdescr#> &<#name#s>|pla}

   let rec printStmt (params:parameters) (stmt:cstmt) : Pla.template option =
      match stmt with
      | CSVarDecl(CLWild,None) -> None
      | CSVarDecl(CLWild,Some(value)) ->
         let te = printExp params value in
         Some({pla|<#te#>;|pla})
      | CSVarDecl((CLId(_,_) as lhs),Some(value)) ->
         let tlhs = printLhsExp true lhs in
         let te   = printExp params value in
         Some({pla|<#tlhs#> = <#te#>;|pla})
      | CSVarDecl((CLId(_,_) as lhs),None) ->
         let tlhs = printLhsExp true lhs in
         Some({pla|<#tlhs#>;|pla})
      | CSVarDecl(CLTuple(elems),Some(CEVar(name))) ->
         let t = List.mapi (printLhsExpTuple name true) elems |> Pla.join in
         Some(t)
      | CSVarDecl(CLTuple(_),_) -> failwith "printStmt: invalid tuple assign"
      | CSBind(CLWild,value) ->
         let te = printExp params value in
         Some({pla|<#te#>;|pla})
      | CSBind(CLTuple(elems),CEVar(name)) ->
         let t =List.mapi (printLhsExpTuple name false) elems |> Pla.join in
         Some(t)
      | CSBind(CLTuple(_),_) -> failwith "printStmt: invalid tuple assign"
      | CSBind(CLId(_,name),CEArray(elems)) ->
         let t = List.mapi (printArrayBinding params name) elems |> Pla.join in
         Some(t)
      | CSBind(CLId(_,name),value) ->
         let te = printExp params value in
         Some({pla|<#name#s> = <#te#>;|pla})
      | CSFunction(ntype,name,args,(CSBlock(_) as body)) ->
         let ret = printTypeDescr ntype in
         let targs = Pla.map_sep Pla.commaspace printFunArg args in
         if params.is_header then begin
            Some({pla|<#ret#> <#name#s>(<#targs#>);<#>|pla})
         end
         else begin
            match printStmt params body with
            | Some(tbody) ->
                  Some({pla|<#ret#> <#name#s>(<#targs#>)<#tbody#><#>|pla})
            | None -> Some({pla|<#ret#> <#name#s>(<#targs#>);<#>|pla})
         end
      | CSFunction(ntype,name,args,body) ->
         let ret = printTypeDescr ntype in
         let targs = Pla.map_sep Pla.commaspace printFunArg args in
         if params.is_header then
            Some({pla|<#ret#> <#name#s>(<#targs#>);<#>|pla})
         else
            let tbody = CCOpt.get_or ~default:Pla.semi (printStmt params body) in
            Some({pla|<#ret#> <#name#s>(<#targs#>){ <#tbody#>}<#>|pla})
      | CSReturn(e1) ->
         let te = printExp params e1 in
         Some({pla|return <#te#>;|pla})
      | CSWhile(cond,body) ->
         let tcond = printExp params cond in
         let tbody = CCOpt.get_or ~default:Pla.semi (printStmt params body) in
         Some({pla|while(<#tcond#>)<#tbody#>|pla})
      | CSBlock(elems) ->
         let telems = printStmtList params elems in
         Some({pla|{<#telems#+>}|pla})
      | CSIf(cond,then_,None) ->
         let tcond = printExp params cond in
         let tcond = if isSimple cond then Pla.wrap (Pla.string "(") (Pla.string ")") tcond else tcond in
         let tthen = CCOpt.get_or ~default:Pla.semi (printStmt params then_) in
         Some({pla|if<#tcond#><#tthen#>|pla})
      | CSIf(cond,then_,Some(else_)) ->
         let tcond = printExp params cond in
         let tcond = if isSimple cond then Pla.wrap (Pla.string "(") (Pla.string ")") tcond else tcond in
         let tthen = CCOpt.get_or ~default:Pla.semi (printStmt params then_) in
         let telse = CCOpt.get_or ~default:Pla.semi (printStmt params else_) in
         Some({pla|if<#tcond#><#tthen#><#>else<#><#telse#>|pla})
      | CSType(name,members) when params.is_header ->
         let tmembers =
         Pla.map_sep_all Pla.newline
            (fun (typ, name) ->
                let tmember = printTypeAndName true typ name in
                {pla|<#tmember#>;|pla}
            ) members;
         in
         Some({pla|typedef struct <#name#s> {<#tmembers#+>} <#name#s>;<#>|pla})
      | CSType(_,_) -> None
      | CSAlias(t1,t2) when params.is_header ->
         let tdescr = printTypeDescr t2 in
         Some({pla|typedef <#t1#s> <#tdescr#>;<#>|pla})
      | CSAlias(_,_) -> None
      | CSExtFunc(ntype,name,args) when params.is_header ->
         let ret = printTypeDescr ntype in
         let targs = Pla.map_sep Pla.commaspace printFunArg args in
         Some({pla|<#ret#> <#name#s>(<#targs#>);|pla})
      | CSExtFunc _ -> None
      | CSEmpty -> None

   and printStmtList (params:parameters) (stmts:cstmt list) : Pla.template =
      let tstmts = CCList.filter_map (printStmt params) stmts in
      Pla.map_sep_all Pla.newline (fun a -> a) tstmts


   let printChCode (params:parameters) (stmts:cstmt list) : string =
      let t     = printStmtList params stmts in
      let code  = Pla.print t in
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
