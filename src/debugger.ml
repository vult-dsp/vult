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

open TypesVult
open Lexing
open TypesUtil
open PrintTypes
open PrintBuffer


(** Return values of the interpreter *)
type value =
   | VUnit
   | VString  of string
   | VNum     of float
   | VBool    of bool
   | VTuple   of value list
   | VLazy    of instruction list
   | VId      of identifier

and instruction =
   | Value  of value            * int
   | Reg    of identifier       * int
   | Lazy   of instruction list * int
   | Call   of identifier       * int
   | Obj    of int              * int
   | Lambda of identifier list  * int
   | Mem    of int
   | Val    of int
   | Store  of int
   | Read   of int
   | If     of int
   | Ret    of int
   | Drop   of int
   | Loop   of int


(** Used to define types of functions: builtin and declared by the user *)
type function_body =
   | Builtin  of (value list -> value)
   | Declared of identifier list * value

(** Environment of the interpreter used to store all bindings and declarations *)
type env =
   {
      val_binds : (value IdentifierMap.t) list;
      mem_binds : value IdentifierMap.t;
      fun_bind  : env IdentifierMap.t;
      fun_decl  : function_body IdentifierMap.t;
   }

type debugger_state =
   {
      code        : instruction list;
      value_stack : value list;
      call_stack  : identifier list;
      env         : env;
      line        : int;
      return      : bool;
      state_stack : debugger_state list;
   }

type breakpoints =
   {
      lines : int list;
   }

(** Joins a list of strings *)
let rec joinStrings sep elems =
   match elems with
   | [] -> ""
   | [h] -> h
   | h::t -> h^sep^(joinStrings sep t)

(** Converts a value to string *)
let rec valueStr (value:value) : string =
   match value with
   | VUnit         -> "()"
   | VNum(v)       -> string_of_float v
   | VString(s)    -> "\""^s^"\""
   | VBool(true)   -> "true"
   | VBool(false)  -> "false"
   | VTuple(elems) ->
      let elems_s = elems
                    |> List.map valueStr
                    |> joinStrings ","
      in "("^elems_s^")"
   | VLazy(_)      -> "<lazy>"
   | VId(id)       -> identifierStr id

let append_nl buff line s =
   let _ = append buff (string_of_int line) in
   let _ = append buff ": " in
   let _ = append buff s in
   newline buff

let rec printIBuff buff i =
   match i with
   | Value(v,line)   -> append_nl buff line (valueStr v)
   | Reg(name,line)  -> append_nl buff line ("$"^identifierStr name)
   | Mem(line)       -> append_nl buff line "Mem"
   | Val(line)       -> append_nl buff line "Val"
   | Store(line)     -> append_nl buff line "Store"
   | Read(line)      -> append_nl buff line "Read"
   | Call(name,line) -> append_nl buff line ("Call("^(identifierStr name)^")")
   | Obj(n,line)     -> append_nl buff line ("Obj("^(string_of_int n)^")")
   | Ret(line)       -> append_nl buff line "Ret"
   | If(line)        -> append_nl buff line "If"
   | Lambda(vars,line) -> append_nl buff line ("Lambda("^(joinStrings "," (List.map identifierStr vars))^")")
   | Drop(line)      -> append_nl buff line "Drop"
   | Loop(line)      -> append_nl buff line "Loop"
   | Lazy(il,line)   ->
      append buff (string_of_int line);
      append buff ": [";
      indent buff;
      printIBuffList buff il;
      outdent buff;
      append buff "]";
      newline buff
and printIBuffList buff il =
   printList buff printIBuff "" il

let printInstructions il =
   let buffer = makePrintBuffer () in
   let _ = printIBuffList buffer il in
   print_string (contents buffer)


let locationLine (loc:location) : int =
   loc.start_pos.Lexing.pos_lnum

let getIdentifier (name:named_id) : identifier =
   match name with
   | SimpleId(id,_)    -> id
   | NamedId(id,_,_) -> id

let getNameLocation (name:named_id) : location =
   match name with
   | SimpleId(_,loc) -> loc
   | NamedId(_,_,loc) -> loc

let rec assemble (i0:instruction list) (exp:exp) =
   match exp with
   | PUnit(loc)   ->
      let line = locationLine loc in
      Value(VUnit,line)::i0
   | PBool(v,loc)   ->
      let line = locationLine loc in
      Value(VBool(v),line)::i0
   | PInt(v,loc)    ->
      let line = locationLine loc in
      Value(VNum(float_of_string v),line)::i0
   | PReal(v,loc)   ->
      let line = locationLine loc in
      Value(VNum(float_of_string v),line)::i0
   | PId(name,_,loc)    ->
      let line = locationLine loc in
      Read(line)::Reg(name,line)::i0
   | PTyped(e,_,_) -> assemble i0 e
   | PTuple(el,loc) ->
      let line = locationLine loc in
      let n = List.length el in
      let i1 = assembleListExp i0 el in
      Obj(n,line)::i1
   | PCall(name,fname,args,loc,_) ->
      let line = locationLine loc in
      let i1 = assembleListExp i0 args in
      Call(fname,line)::i1
   | PSeq(_,el,_) -> assembleListStmt i0 el
   | PIf(cond,then_,else_,loc) ->
      let line = locationLine loc in
      let then_line = getExpLocation then_ |> locationLine in
      let else_line = getExpLocation else_ |> locationLine in
      let then_i = Lazy(assembleRev then_,then_line) in
      let else_i = Lazy(assembleRev else_,else_line) in
      let i1 = assemble (then_i::else_i::i0) cond in
      If(line)::i1
   | StmtVal(PId(name,_,loc),None,_) ->
      let line = locationLine loc in
      Val(line)::Reg(name,line)::Value(VNum(0.0),line)::i0
   | StmtVal(PId(name,_,loc),Some(init),_) ->
      let line = locationLine loc in
      let i1 = assemble i0 init in
      Val(line)::Reg(name,line)::i1
   | StmtMem(PId(name,_,loc),None,None,_) ->
      let line = locationLine loc in
      Mem(line)::Reg(name,line)::Value(VNum(0.0),line)::i0
   | StmtMem(PId(name,_,loc),Some(init),None,_) ->
      let line = locationLine loc in
      let i1 = assemble i0 init in
      Mem(line)::Reg(name,line)::i1
   | StmtMem(PId(name,_,loc),_,Some(init),_) ->
      let line = locationLine loc in
      let i1 = assemble i0 init in
      Mem(line)::Reg(name,line)::i1
   | StmtVal(_,_,_) -> failwith "Complex bindings should be simplified"
   | StmtMem(_,_,_,_) -> failwith "Complex bindings should be simplified"
   | StmtBind(PId(name,_,loc),e2,_) ->
      let line = locationLine loc in
      let i1 = assemble i0 e2 in
      Store(line)::Reg(name,line)::i1
   | StmtBind(PUnit(_),e2,loc) ->
      assemble i0 e2
   | StmtBind(_,_,_) -> failwith "Complex bindings should be simplified"
   | StmtReturn(e,loc) ->
      let line = locationLine loc in
      let i1 = assemble i0 e in
      Ret(line)::i1
   | StmtBlock(_,el,loc) ->
      assembleListStmt i0 el
   | StmtIf(cond,then_,Some(else_),loc) ->
      let line = locationLine loc in
      let then_line = getExpLocation then_ |> locationLine in
      let else_line = getExpLocation else_ |> locationLine in
      let then_i = Lazy(assembleRev then_,then_line) in
      let else_i = Lazy(assembleRev else_,else_line) in
      let i1 = assemble (then_i::else_i::i0) cond in
      Drop(line)::If(line)::i1
   | StmtIf(cond,then_,None,loc) ->
      let line = locationLine loc in
      let then_line = getExpLocation then_ |> locationLine in
      let then_i = Lazy(assembleRev then_,then_line) in
      let else_i = Lazy([],line) in
      let i1 = assemble (then_i::else_i::i0) cond in
      Drop(line)::If(line)::i1
   | StmtFun(name,vars,body,type_exp,active,loc) ->
      let line = locationLine loc in
      let i1 = assembleRev body in
      let vars_id = List.map getIdentifier vars in
      Store(line)::Reg(name,line)::Lambda(vars_id,line)::Lazy(i1,line)::i0
   | StmtWhile(cond,body,loc) ->
      let line = locationLine loc in
      (* This needs to be reversed since is gonna be placed in a Lazy *)
      let cond_i = assembleRev cond in
      let body_i = assembleRev body in
      let cond_line = getExpLocation cond |> locationLine in
      let body_line = getExpLocation body |> locationLine in
      Loop(line)::Lazy(cond_i,cond_line)::Lazy(body_i,body_line)::i0
   | PGroup(e,loc) ->
      assemble i0 e
   | StmtType(_,_,_,_,_) -> i0
   | PUnOp(_,_,_)    -> failwith "No unary operations should remain"
   | PBinOp(_,_,_,_) -> failwith "No binary operations should remain"
   | PEmpty          -> failwith "No empty expressions should remain"
   | StmtEmpty       -> failwith "No empty statements should remain"

and assembleListExp (i0:instruction list) (exp_list:exp list) : instruction list =
   List.fold_left (fun i e -> assemble i e) i0 exp_list
and assembleListStmt (i0:instruction list) (exp_list:exp list) : instruction list =
   List.fold_left (fun i e -> assemble i e) i0 exp_list
   |> List.rev
and assembleRev (exp:exp) : instruction list =
   assemble [] exp

(** Returns the value for the given variable *)
let getExpValueFromEnv (loc:env) (name:identifier) : value =
   let rec loop locals =
      match locals with
      | [] ->
         if IdentifierMap.mem name loc.mem_binds then
            IdentifierMap.find name loc.mem_binds
         else
            failwith ("Undeclared variable "^(identifierStr name))
      | h::t ->
         if IdentifierMap.mem name h then
            IdentifierMap.find name h
         else loop t
   in loop loc.val_binds

(** Declares a variable name *)
let declVal (loc:env) (name:identifier) (value:value) : env =
   match loc.val_binds with
   | [] ->
      let new_env = IdentifierMap.add name value (IdentifierMap.empty) in
      { loc with val_binds = [new_env] }
   | h::t ->
      let new_env = IdentifierMap.add name value h in
      { loc with val_binds = new_env::t }

(** Declares a memory name *)
let declMem (loc:env) (name:identifier) (init:value) : env =
   if not (IdentifierMap.mem name loc.mem_binds) then
      { loc with mem_binds = IdentifierMap.add name init loc.mem_binds }
   else loc

(** Sets the value of a given variable *)
let setValMem (loc:env) (name:identifier) (value:value) : env =
   let rec loop locals acc =
      match locals with
      | [] ->
         if IdentifierMap.mem name loc.mem_binds then
            { loc with mem_binds = IdentifierMap.add name value loc.mem_binds }
         else
            failwith ("Undeclared variable "^(identifierStr name))
      | h::t ->
         if IdentifierMap.mem name h then
            { loc with val_binds = (List.rev acc)@[IdentifierMap.add name value h]@t}
         else loop t (h::acc)
   in loop loc.val_binds []

(** Returns true if the value is non zero *)
let isTrue (value:value) : bool =
   match value with
   | VNum(0.0)   -> false
   | VString("") -> false
   | VBool(v)    -> v
   | _           -> true

let take1 (stack:value list) =
   match stack with
   | []   -> failwith "The stack is empty"
   | h::t -> h,t

let take2 (stack:value list) =
   match stack with
   | []        -> failwith "The stack is empty"
   | v1::v2::t -> v1,v2,t
   | _         -> failwith "The stack does not contain 2 elements"

let take3 (stack:value list) =
   match stack with
   | []        -> failwith "The stack is empty"
   | v1::v2::v3::t
      -> v1,v2,v3,t
   | _         -> failwith "The stack does not contain 3 elements"

let rec takeN (stack:value list) n =
   match stack,n with
   | _,0                -> [],stack
   | v1::t,1            -> [v1],t
   | v1::v2::t,2        -> [v1;v2],t
   | v1::v2::v3::t,3    -> [v1;v2;v3],t
   | h::t,_ ->
      let elems,new_stack = takeN t (n-1) in
      h::elems,new_stack
   | _         -> failwith "The stack does not contain the required elements"

let step (state:debugger_state) =
   match state.code with
   | [] -> state
   | h::t ->
      match h with
      | Drop(line) ->
         let _,new_stack = take1 state.value_stack in
         { state with code = t; line = line; value_stack = new_stack }
      | Value(v,line) ->
         { state with code = t; line = line; value_stack = v::state.value_stack }
      | Reg(id,line) ->
         {state with code = t; line = line; value_stack = (VId(id))::state.value_stack }
      | Lazy(i,line) ->
         {state with code = t; line = line; value_stack = (VLazy(i))::state.value_stack }
      | Read(line) ->
         let id,new_stack = take1 state.value_stack in
         begin
            match id with
            | VId(name) ->
               let value = getExpValueFromEnv state.env name in
               {state with code = t; line = line; value_stack = value::new_stack }
            | _ -> failwith (Printf.sprintf "Cannot read from %s" (valueStr id))
         end
      | Mem(line) ->
         let id,value,new_stack = take2 state.value_stack in
         begin
            match id with
            | VId(name) ->
               let new_env = declMem state.env name value in
               {state with code = t; line = line; value_stack = new_stack; env = new_env }
            | _ -> failwith (Printf.sprintf "Cannot store %s in %s" (valueStr value) (valueStr id))
         end
      | Val(line) ->
         let id,value,new_stack = take2 state.value_stack in
         begin
            match id with
            | VId(name) ->
               let new_env = declVal state.env name value in
               {state with code = t; line = line; value_stack = new_stack; env = new_env }
            | _ -> failwith (Printf.sprintf "Cannot store %s in %s" (valueStr value) (valueStr id))
         end
      | Store(line) ->
         let id,value,new_stack = take2 state.value_stack in
         begin
            match id with
            | VId(name) ->
               let new_env = setValMem state.env name value in
               {state with code = t; line = line; value_stack = value::new_stack; env = new_env }
            | _ -> failwith (Printf.sprintf "Cannot store %s in %s" (valueStr value) (valueStr id))
         end
      | Obj(n,line) ->
         let elems,new_stack = takeN state.value_stack n in
         {state with code = t; line = line; value_stack = VTuple(elems)::new_stack }
      | Ret(line) ->
         {state with code = t; line = line; return = true }
      | Lambda(vars,line) ->
         let id,body,new_stack = take2 state.value_stack in
         begin
            match id,body with
            | VId(name),VLazy(_) ->
               let new_func = IdentifierMap.add name (Declared(vars,body)) state.env.fun_decl in
               {state with code = t; line = line; value_stack = new_stack; env = {state.env with fun_decl = new_func } }
            | _ -> failwith (Printf.sprintf "Cannot define function for %s" (valueStr id))
         end
      | If(line) ->
         begin
            match state.value_stack with
            | VLazy(i)::_ -> { state with code = i@state.code }
            | _ ->
               let cond,then_,else_,new_stack = take3 state.value_stack in
               if isTrue cond then
                  begin
                     match then_ with
                     | VLazy(i) ->
                        {state with code = i@t; line = line ; value_stack = new_stack }
                     | _ -> failwith "Not a valid if condition"
                  end
               else
                  begin
                     match else_ with
                     | VLazy(i) ->
                        {state with code = i@t; line = line ; value_stack = new_stack }
                     | _ -> failwith "Not a valid if condition"
                  end
         end
      | Loop(line) ->
         let cond,body,new_stack = take2 state.value_stack  in
         if isTrue cond then
            { state with line = line; value_stack = new_stack }
         else
            { state with code = t; line = line }
      | _ -> failwith "Unsupported instruction"


let rec loop state =
   let new_state = step state in
   match new_state.code with
   | [] -> print_string "ok\n"
   | _  -> loop new_state

let initialEnv () =
   {
      val_binds = [];
      mem_binds = IdentifierMap.empty;
      fun_bind  = IdentifierMap.empty;
      fun_decl  = IdentifierMap.empty;
   }

let initialState instructions =
   {
      code        = instructions;
      value_stack = [];
      call_stack  = [];
      env         = initialEnv();
      line        = 0;
      return      = false;
      state_stack = [];
   }


let debug (results:parser_results) : unit =
   let result = CCError.map
         (fun stmts ->
             let instructions = assembleListStmt [] stmts in
             let _ = printInstructions instructions in
             loop (initialState instructions)
         )
         results.presult
   in
   match result with
   | `Ok(ret) ->
      ()
   | `Error(error) ->
      ()

