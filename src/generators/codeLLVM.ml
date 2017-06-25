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
open Code
open Ollvm_ez
open Maps

module P = Ollvm.Printer

type vars = Value.t IdMap.t

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
let rec printExp _m vars (e:cexp) =
   match e with
   | CEInt(n)      -> Value.i32 n
   | CEBool(true)  -> Value.i1 1
   | CEBool(false) -> Value.i1 0
   | CEFloat(_,n)  -> Value.float n
   | CEVar([name],_) ->
      let var = IdMap.find [name] vars in
      var

   | _ -> failwith (Code.show_cexp e)

let printArgType arg : Type.t =
   match arg with
   | Var(td) -> printTypeDescr td
   | Ref(td) -> Type.pointer (printTypeDescr td)

let declareFunctionArg (m,acc) (typ,name) =
   let m,var = Module.local m (printArgType typ) name in
   m, (var::acc)

let is_reg s =
   String.get s 0 = '$'

let rec printStmt (m:Module.t) (vars:vars) (stmt:cstmt) : Module.t * vars * 'a =
   match stmt with
   (* Do not allocate the temporary variables *)
   | CSVar(CLId(typ,[name]), None) when is_reg name ->
      let typ'   = printTypeDescr typ in
      let m, var = Module.local m typ' name in
      let vars   = IdMap.add [name] var vars in
      m, vars, []

   (* All other variables are allocated *)
   | CSVar(CLId(typ,[name]), None) ->
      let typ'   = printTypeDescr typ in
      let alloc  = Instr.alloca typ' in
      let m, var = Module.local m typ' name in
      let vars   = IdMap.add [name] var vars in
      m, vars, [Instr.(var <-- alloc)]

   | CSBind(CLId(_,[lhs]), ((CEInt _ | CEFloat _ | CEBool _ ) as rhs)) ->
      let rhs'    = printExp m vars rhs in
      let var    = IdMap.find [lhs] vars in
      let _,res  = Instr.store rhs' var in
      m, vars, [res]

   | CSBind(CLId(_,[lhs]), CEOp(_,[e1;e2],_)) when is_reg lhs ->
      let e1'    = printExp m vars e1 in
      let e2'    = printExp m vars e2 in
      let var    = IdMap.find [lhs] vars in
      m, vars, [Instr.(var <-- (add e1' e2'));]

   | CSBind(CLId(typ,[lhs]), CEOp(_,[e1;e2],_)) ->
      let typ'   = printTypeDescr typ in
      let e1'    = printExp m vars e1 in
      let e2'    = printExp m vars e2 in
      let var    = IdMap.find [lhs] vars in
      let m, tmp = Module.local m typ' "" in
      let _,res  = Instr.store tmp var in
      m, vars, [res; Instr.(tmp <-- (add e1' e2'))]

   | CSReturn(e) ->
      let e' = printExp m vars e in
      let r = Instr.ret e' in
      m,vars, [r]

   | _ -> failwith (Code.show_cstmt stmt)

let printBody (m:Module.t) (vars:vars) (stmt:cstmt)  : Module.t * vars * Ollvm_ast.instr list =
   match stmt with
   | CSBlock([]) -> m, vars, []
   | CSBlock(body) ->
      let m, vars, body' =
         List.fold_left
            (fun (m,vars,acc) stmt ->
                let m', vars',s = printStmt m vars stmt in
                m',vars',(s@acc))
            (m,vars,[]) body
      in
      m, vars, List.rev body'
   | _ ->
      let m, vars, s = printStmt m vars stmt in
      m, vars, s

let printTopLevel (m:Module.t) (vars:vars) (stmt:cstmt) : Module.t * vars =
   match stmt with
   | CSFunction(ntype,name,args,body) ->
      let ret            = printTypeDescr ntype in
      let m, fname       = Module.global m ret name in
      let m, rev_args    = List.fold_left declareFunctionArg (m,[]) args in
      let args'          = List.rev rev_args in
      let m, fun_entry   = Module.local m Type.label (name^"_entry") in
      let m, vars, body' = printBody m vars body in
      let def            = Block.define fname args' [Block.block fun_entry body'] in
      Module.definition m def, vars

   | CSType _ -> m, vars

   | _ -> failwith "invalid"


let print (_params:params) (stmts:Code.cstmt list) : (Pla.t * filename) list =
   let m = Module.init
         "name"
         ("x86_64", "pc", "linux-gnu")
         "e-m:o-i64:64-f80:128-n8:16:32:64-S128"
   in
   let m, _ =
      List.fold_left (fun (m ,vars) stmt ->printTopLevel m vars stmt) (m, IdMap.empty) stmts
   in
   let buffer = Buffer.create 0 in
   let formatter = Format.formatter_of_buffer buffer in
   let () = P.modul (P.empty_env ()) formatter m.Module.m_module in
   [Pla.string (Buffer.contents buffer), ExtOnly "ll"]