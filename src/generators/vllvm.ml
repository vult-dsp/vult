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

open GenerateParams
open CLike
open Ollvm_ez
module P = Ollvm.Printer

let rec printType (typ:string) : Type.t =
   match typ with
   | "int" -> Type.i32
   | "real" -> Type.float
   | "float" -> Type.float
   | "bool" -> Type.i1
   | "uint8_t" -> Type.i1
   | "void" -> Type.void
   | _ -> failwith ("unknown type "^typ)

(** Returns the representation of a type description *)
let rec printTypeDescr (typ:type_descr) : Type.t=
   match typ with
   | CTSimple(typ) -> printType typ
   | CTArray(tdescr,_) -> Type.pointer (printTypeDescr tdescr)


(** Returns a template the print the expression *)
let rec printExp m (e:cexp) =
   match e with
   | CEInt(n)      -> m, Value.i32 n
   | CEBool(true)  -> m, Value.i1 1
   | CEBool(false) -> m, Value.i1 0
   | CEFloat(_,n)  -> m, Value.float n
   | CEVar([name],typ) ->
      let typ' = printTypeDescr typ in
      Module.local m typ' name

   | _ -> failwith (CLike.show_cexp e)

let printArgType arg : Type.t =
   match arg with
   | Var(td) -> printTypeDescr td
   | Ref(td) -> Type.pointer (printTypeDescr td)

let declareFunctionArg (m,acc) (typ,name) =
   let m,var = Module.local m (printArgType typ) name in
   m, (var::acc)

let rec printStmt m (stmt:cstmt) =
   match stmt with
   (* Prints type x; *)
   | CSVar(CLId(typ,[name]), None) ->
      let typ' = printTypeDescr typ in
      let alloc = Instr.alloca typ' in
      let m, var = Module.local m typ' name in
      m, [Instr.(var <-- alloc)]

   | CSBind(CLId(typ,[lhs]),CEOp(_,[e1;e2],_)) ->
      let typ' = printTypeDescr typ in
      let m, e1' = printExp m e1 in
      let m, e2' = printExp m e2 in
      let m, tmp = Module.local m typ' "" in
      let m, var = Module.local m typ' lhs in
      let _,res = Instr.store tmp var in
      m, [res; Instr.(tmp <-- (add e1' e2'));]

   | CSReturn(e) ->
      let m, e' = printExp m e in
      let r = Instr.ret e' in
      m,[r]

   | _ -> failwith (CLike.show_cstmt stmt)

let printBody (m:Module.t) stmt : Module.t * Ollvm_ast.instr list =
   match stmt with
   | CSBlock([]) -> m, []
   | CSBlock(body) ->
      let m, body' =
         List.fold_left (fun (m,acc) stmt -> let m',s = printStmt m stmt in m',(s@acc)) (m,[]) body
      in
      m, List.rev body'
   | _ ->
      let m, s = printStmt m stmt in
      m, s

let printTopLevel m (stmt:cstmt) =
   match stmt with
   | CSFunction(ntype,name,args,body) ->
      let ret = printTypeDescr ntype in
      let m, fname = Module.global m ret name in
      let m, rev_args = List.fold_left declareFunctionArg (m,[]) args in
      let args' = List.rev rev_args in
      let m, fun_entry = Module.local m Type.label (name^"_entry") in
      let m, body' = printBody m body in
      let def = Block.define fname args' [Block.block fun_entry body'] in
      Module.definition m def

   | CSType _ -> m

   | _ -> failwith "invalid"


let print (_params:params) (stmts:CLike.cstmt list) : (Pla.t * filename) list =
   let m = Module.init
         "name"
         ("x86_64", "pc", "linux-gnu")
         "e-m:o-i64:64-f80:128-n8:16:32:64-S128"
   in
   let m = List.fold_left printTopLevel m stmts in
   let buffer = Buffer.create 0 in
   let formatter = Format.formatter_of_buffer buffer in
   let () = P.modul (P.empty_env ()) formatter m.Module.m_module in
   [Pla.string (Buffer.contents buffer), ExtOnly "ll"]