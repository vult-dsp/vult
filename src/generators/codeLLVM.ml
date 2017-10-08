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

open Code
open Config

module Table = CCMap.Make(String)

type data =
   {
      (* Keeps track of the record index tables *)
      records : (int Table.t) Table.t;
      (* Keeps track of declared struct name *)
      record_aliases : string Table.t;
   }

let empty_data =
   {
      records = Table.empty;
      record_aliases = Table.empty;
   }

let getFields (s:data) (typ:string) : int Table.t =
   match Table.find typ s.record_aliases with
   | alias ->
      begin match Table.find alias s.records with
         | fields -> fields
         | exception Not_found -> failwith "CodeLLVM.getFields: the record alias type was not found"
      end
   | exception Not_found ->
      match Table.find typ s.records with
      | fields -> fields
      | exception Not_found -> failwith "CodeLLVM.getFields: the record type was not found"

let rec printType (typ:string) =
   match typ with
   | "int" -> "i32"
   | "real" -> "float"
   | "float" -> "float"
   | "bool" -> "i1"
   | "uint8_t" -> "i1"
   | "void" -> "void"
   | _ -> "%struct." ^ typ

(** Returns the representation of a type description *)
let rec printTypeDescr (typ:type_descr) : Pla.t =
   match typ with
   | CTSimple(typ) -> Pla.string (printType typ)
   | CTArray(tdescr, _) -> Pla.append (printTypeDescr tdescr) (Pla.string "*")

(** Returns a template the print the expression *)
let rec printExp (e:cexp) =
   match e with
   | CEInt(n)      -> {pla|i32 <#n#i>|pla}
   | CEBool(true)  -> {pla|i1 1|pla}
   | CEBool(false) -> {pla|i1 0|pla}
   | CEFloat(_, n)  -> {pla|float <#n#f>|pla}
   | CEVar([name], [typ]) ->
      let typ = printTypeDescr typ in
      {pla|<#typ#> %<#name#s>|pla}


   | _ -> failwith (Code.show_cexp e)

let printArgType arg =
   match arg with
   | Var(td) -> printTypeDescr td
   | Ref(td) -> Pla.append (printTypeDescr td) (Pla.string "*")

(*let declareFunctionArg (m, acc) (typ,name) =
   let m,var = Module.local m (printArgType typ) name in
   m, (var::acc)
*)

let getOp op typ =
   match op, typ with
   (* float operations *)
   | "+", CTSimple("float") -> "fadd"
   | "-", CTSimple("float") -> "fsub"
   | "*", CTSimple("float") -> "fmul"
   | "/", CTSimple("float") -> "fdiv"
   (* int operations *)
   | "+", CTSimple("int") -> "add"
   | "-", CTSimple("int") -> "sub"
   | "*", CTSimple("int") -> "mul"
   | "/", CTSimple("int") -> "div"
   | _ -> failwith "CodeLLVM.getOp: operator not implemented"

let is_reg s = String.get s 0 = '$'

let rec printStmt s (stmt:cstmt) =
   match stmt with
   (* Do not allocate the temporary variables *)
   | CSVar(CLId(_, [name]), None) when is_reg name -> s, None

   (* All other variables are allocated *)
   | CSVar(CLId([typ], [name]), None) ->
      let typ = printTypeDescr typ in
      s, Some {pla|%<#name#s> = alloca <#typ#>|pla}

   | CSBind(CLId(_, [lhs]), (( CEInt _ | CEFloat _ | CEBool _ | CEString _) as rhs)) ->
      let rhs = printExp rhs in
      s, Some {pla|store <#rhs#>, %<#lhs#s>|pla}

   | CSBind(CLId([t1; CTSimple t2], [lhs1; lhs2]), (( CEInt _ | CEFloat _ | CEBool _ | CEString _) as rhs)) ->
      let rhs = printExp rhs in
      s, Some {pla|<#lhs2#s> = getelementptr inbounds <#t2#s>, <#t2#s> <#lhs1#s>, i32 0, i32 0
                   store <#rhs#>, i32* <#lhs2#s>|pla}

   | CSBind(CLId(_, [lhs]), CEVar([rhs], [typ])) ->
      let typ = printTypeDescr typ in
      s, Some {pla|%<#lhs#s> = load <#typ#>, <#typ#>* %<#rhs#s>|pla}

   | CSBind(CLId([typ], [lhs]), CEOp(op, [e1;e2], _)) when is_reg lhs ->
      let op  = getOp op typ in
      let typ = printTypeDescr typ in
      let e1  = printExp e1 in
      let e2  = printExp e2 in
      s, Some {pla|%<#lhs#s> = <#op#s> <#typ#> <#e1#>, <#e2#>|pla}

   | CSBind(CLId(_typ, [_lhs]), CEOp(_, [_e1;_e2], _)) ->
      (*let typ'   = printTypeDescr typ in
        let e1'    = printExp m vars e1 in
        let e2'    = printExp m vars e2 in
        let var    = IdMap.find [lhs] vars in
        let m, tmp = Module.local m typ' "" in
        let _,res  = Instr.store tmp var in
        m, vars, [res; Instr.(tmp <-- (add e1' e2'))]; *)
      failwith ""

   | CSReturn(e) ->
      let e = printExp e in
      s, Some {pla|ret <#e#>|pla}

   | _ -> failwith (Code.show_cstmt stmt)

let printBody s (stmt:cstmt) : 'state * Pla.t =
   match stmt with
   | CSBlock([]) -> s, Pla.unit
   | CSBlock(body) ->
      List.fold_left
         (fun (s, p) stmt ->
             match printStmt s stmt with
             | s, (Some stmt) -> s, Pla.append p (Pla.append stmt Pla.newline)
             | s, None -> s, p)
         (s, Pla.unit) body
   | _ -> failwith ""

let printTopLevel s (stmt:cstmt) : 'state * Pla.t =
   match stmt with
   | CSFunction(_ntype, _name, _args, body) ->
      printBody s body

   | CSType (name, fields) ->
      let record_fields, _ = List.fold_left (fun (m, i) (_typ,name) -> Table.add name i m, i + 1) (Table.empty, 0) fields in
      let s = { s with records = Table.add name record_fields s.records } in
      let types = Pla.map_sep Pla.comma (fun (typ, _) -> printTypeDescr typ) fields in
      s, {pla|%struct.<#name#s> = type { <#types#> }|pla}

   | CSAlias(name, CTSimple source) ->
      let s = { s with record_aliases = Table.add name source s.record_aliases } in
      s, Pla.unit

   | _ -> failwith "invalid"


let print (_params:params) (stmts:Code.cstmt list) : (Pla.t * FileKind.t) list =
   let _, p =
      List.fold_left (fun (s, p) stmt ->
            let s, stmt = printTopLevel s stmt in
            s, Pla.append p (Pla.append stmt Pla.newline))
         (empty_data,Pla.unit) stmts
   in
   [p, FileKind.ExtOnly "ll"]