
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

(** Transformations and optimizations of the syntax tree *)

open TypesVult
open TypesUtil


(** Generic type of transformations *)
type ('data,'value) transformation = 'data tstate -> 'value -> 'data tstate * 'value

(** Generic type of expanders *)
type ('data,'value) expander = 'data tstate -> 'value -> 'data tstate * 'value list

(** Generic type of folders *)
type ('data,'value) folder = 'data tstate -> 'value -> 'data tstate


(** Makes a chain of transformations. E.g. foo |-> bar will apply first foo then bar. *)
let (|->) : ('data,'value) transformation -> ('data,'value) transformation -> ('data,'value) transformation =
   fun a b ->
   fun state exp ->
      let new_state,new_exp = a state exp in
      b new_state new_exp

(** Makes a chain of fold functions. E.g. foo |*> bar will apply first foo then bar. *)
let (|*>) : ('data,'value) folder -> ('data,'value) folder -> ('data,'value) folder =
   fun a b ->
   fun state exp ->
      let new_state = a state exp in
      b new_state exp

(** Pipes a pair (state,value) into transformation functions *)
let (|+>) : ('state tstate * 'value) -> ('state, 'value) transformation -> ('state tstate * 'value) =
   fun (state,value) transformation ->
      transformation state value

(** Options to control the transformations *)
type options =
   {
      basic           : bool;
      inline          : bool;
      inline_weight   : int;
      simplify_return : bool;
      finalize        : bool;
      codegen         : bool;
      interpreter     : bool;
   }

let opt_full_transform =
   {
      basic           = true;
      inline          = true;
      inline_weight   = 1;
      simplify_return = true;
      finalize        = true;
      codegen         = false;
      interpreter     = false;
   }

let opt_simple_transform =
   {
      basic           = true;
      inline          = false;
      inline_weight   = 1;
      simplify_return = false;
      finalize        = false;
      codegen         = false;
      interpreter     = false;
   }

let opt_interpret =
   {
      basic           = true;
      inline          = false;
      inline_weight   = 1;
      simplify_return = false;
      finalize        = false;
      codegen         = false;
      interpreter     = true;
   }

let opt_no_transform =
   {
      basic           = false;
      inline          = false;
      inline_weight   = 1;
      simplify_return = false;
      finalize        = false;
      codegen         = false;
      interpreter     = false;
   }

(** Traversing state one *)
type pass_state =
   {
      functions       : exp IdentifierMap.t; (* Holds the body of the functions, used by inlining *)
      types           : exp IdentifierMap.t; (* Holds the body of the the types *)
      function_weight : int IdentifierMap.t; (* Weight (complexity) of each function , used by inlining *)
      counter         : int;                 (* Generic counter used to generate unique names *)
      options         : options;             (* Used to enable/disable/configure the passes *)
      function_mem    : exp list IdentifierMap.t;  (* Stores the mem declarations of each function*)
      instances       : (identifier list IdentifierMap.t) IdentifierMap.t; (* Stores the instances of each function *)
      type_function   : exp IdentifierMap.t; (* Stores for each function its corresponding non-simplified type *)
      type_mapping    : identifier IdentifierMap.t; (* Stores the simplified type of each function *)
   }

(** Increases the counter of a traversing state *)
let incState (state: pass_state tstate) : pass_state tstate =
   setState state { state.data with counter = state.data.counter+1 }


let lookupFunctionName (table:'a IdentifierMap.t) (state:pass_state tstate) (fname:identifier) : identifier option =
   let current_scope = getScope state |> List.rev in
   let rec loop rev_scope =
      let full_name = (List.rev rev_scope)@fname in
      if IdentifierMap.mem full_name table then
         Some(full_name)
      else
         match rev_scope with
         | []   -> None
         | _::t -> loop t
   in loop current_scope

(** Search a function in a table starting in the current scope and returns an Some if found *)
let lookupFunction (table:'a IdentifierMap.t) (state:pass_state tstate) (fname:identifier) : 'a option =
   match lookupFunctionName table state fname with
   | Some(full_name) ->
      Some(IdentifierMap.find full_name table)
   | _ -> None


(** Search a function in a table starting in the current scope and returns the default value if not found *)
let lookupFunctionDefault (table:'a IdentifierMap.t) (state:pass_state tstate) (fname:identifier) (default:'a) : 'a =
   match lookupFunction table state fname with
   | None    -> default
   | Some(a) -> a

(** Adds a newly generated type to the table *)
let addTypeOfFunction (state: pass_state tstate) (fname:identifier) (ftype:exp) =
   (*let _ = Printf.printf "Adding type to function %s\n%s\n" (identifierStr fname) (PrintTypes.expressionStr ftype) in*)
   let new_table = IdentifierMap.add fname ftype state.data.type_function in
   { state with data = { state.data with type_function = new_table } }

(** Registers a mem declaration in the current scope *)
let addMemToFunction (s:pass_state tstate) (names:exp list) =
   let scope = getScope s in
   (*let names_string = List.map PrintTypes.expressionStr names in
     let _ = Printf.printf "Adding mem %s to function %s\n" (joinSep ", " names_string) (identifierStr scope) in*)
   if IdentifierMap.mem scope s.data.function_mem then
      let current = IdentifierMap.find scope s.data.function_mem in
      let new_map = IdentifierMap.add scope (current@names) s.data.function_mem in
      { s with data = { s.data with function_mem = new_map } }
   else
      let new_map = IdentifierMap.add scope names s.data.function_mem in
      { s with data = { s.data with function_mem = new_map } }

(** Returns the mem variables in the function *)
let getMemDeclarations (s:pass_state tstate) (name:identifier) : exp list =
   if IdentifierMap.mem name s.data.function_mem then
      IdentifierMap.find name s.data.function_mem
   else
      []

(** Returns the instance names of a function *)
let getInstanceNames (s:pass_state tstate) (name:identifier) : identifier list =
   if IdentifierMap.mem name s.data.instances then
      let instance_table = IdentifierMap.find name s.data.instances in
      IdentifierMap.to_list instance_table |> List.map fst
   else
      []

(** Returns the name of the type that is declared for a function *)
let generateTypeName (id:identifier) : identifier =
   ["_type_"^(joinSep "_" id)]

let getFinalType (s:pass_state tstate) (name:identifier) : identifier option =
   match mapfindOption (generateTypeName name) s.data.type_mapping with
   | Some(final_type) -> Some(final_type)
   | _ -> None

(** Registers an instance in the current scope *)
let addInstanceToFunction (s:pass_state tstate) (name:identifier) (fname:identifier) =
   let scope             = getScope s in
   let instances_for_fun = mapfindDefault scope s.data.instances IdentifierMap.empty in
   let current_instance  = mapfindDefault name instances_for_fun [] in
   if List.exists (fun a-> a = fname) current_instance then
      s
   else
      (*let _ = Printf.printf "Adding instance '%s' of function '%s' to '%s'\n" (identifierStr name) (identifierStr fname) (identifierStr scope) in*)
      let new_instances     = IdentifierMap.add name (fname::current_instance) instances_for_fun in
      let new_inst_for_fun  = IdentifierMap.add scope new_instances s.data.instances in
      { s with data = { s.data with instances = new_inst_for_fun } }

let rec isActiveFunction (state: pass_state tstate) (name:identifier) : bool =
   (* this function can be cached by adding a pass that calculates every function *)
   (isMemFunction state name) || (isMemInstanceFunction state name)


and isMemFunction (state: pass_state tstate) (name:identifier) : bool =
   match lookupFunction state.data.function_mem state name with
   | None     -> false
   | Some([]) -> false
   | _        -> true

and isMemInstanceFunction (state:pass_state tstate) (name:identifier) : bool =
   match lookupFunction state.data.instances state name with
   | Some(instances_for_fun) ->
      IdentifierMap.fold (fun key types acc ->
            let current =
               types
               |> List.filter (fun a -> a <> name)
               |> List.exists (isActiveFunction state)
            in current || acc)
         instances_for_fun false
   | None -> false

(** Returns the output type of a function (currently only supports builtin functions) *)
let getFunctionType (state:'a tstate) (name:identifier) : exp option =
   match name with
   | ["tan"] | ["sin"] | ["cos"] | ["tanh"] -> Some(PId(["real"],None,default_loc))
   | ["'=='"] | ["'!='"] | ["'>'"] | ["'<'"] | ["'>='"] | ["'<='"] -> Some(PId(["bool"],None,default_loc))
   | _ -> None

(** Returs the name and type if an expression PId, fails on any other case *)
let getIdAndType (e:exp) : identifier * exp =
   match e with
   | PId(name,Some(tp),_) -> name,tp
   | PId(name,None,loc) -> name,PId(["real"],None,loc)
   | _ -> failwith "getIdAndType: not expected mem declaration"

let isReturn : ('data,exp) folder =
   fun state e ->
      match e with
      | StmtReturn(_,_) -> setState state true
      | _ -> state

let isIfStmt : ('data,exp) folder =
   fun state e ->
      match e with
      | StmtIf(_,_,_,_) -> setState state true
      | _ -> state

let skipPSeq (e:exp) : bool =
   match e with
   | PSeq(_,_,_) -> false
   | _ -> true

let skipFun (stmt:exp) : bool =
   match stmt with
   | StmtFun(_,_,_,_,_,_) -> false
   | _ -> true

let isFun (stmt:exp) : bool =
   match stmt with
   | StmtFun(_,_,_,_,_,_) -> true
   | _ -> false

let skipBlock (stmt:exp) : bool =
   match stmt with
   | StmtBlock(_,_,_) -> false
   | _ -> true

let skipIfStmt (stmt:exp) : bool =
   match stmt with
   | StmtIf(_) -> false
   | _ -> true

let skipPIf (stmt:exp) : bool =
   match stmt with
   | PIf(_) -> false
   | _ -> true

let hasReturn (stmt:exp) : bool =
   foldTopExp (Some(skipPSeq)) isReturn (createState false) stmt
   |> getState

(**  Counts the number of function calls (operations) expression list has *)
let getExpWeight (e:exp) : int =
   let count acc e =
      match e with
      | PBinOp(_,_,_,_)
      | PUnOp(_,_,_)
      | PCall(_)       -> setState acc (acc.data+1)
      | _ -> acc
   in
   foldTopExp (Some(skipFun)) count (createState 0) e
   |> getState

(** Adds all function definitions to a map in the state and also the weight of the function *)
let collectFunctionDefinitions : ('data,exp) folder =
   fun state exp ->
      match exp with
      | StmtFun(name,args,stmts,type_exp,active,loc) ->
         let weight = getExpWeight stmts in
         let full_name = getScope state in
         (*let _ = Printf.printf "*** Adding function '%s' with weight %i\n" (identifierStr full_name) weight in*)
         (*let _ = Printf.printf "%s\n" (PrintTypes.expressionStr exp) in*)
         let ret_state =
            {
               state.data with
               functions = IdentifierMap.add full_name exp state.data.functions;
               function_weight = IdentifierMap.add full_name weight state.data.function_weight
            }
         in setState state ret_state
      | _ -> state

(** Adds all type definitions to a map in the state *)
let collectTypeDefinitions : ('data,exp) folder =
   fun state exp ->
      match exp with
      | StmtAliasType(name,_,_,_)
      | StmtType(name,_,_,_) ->
         let full_name = (getScope state)@name in
         (*let _ = Printf.printf "*** Adding type '%s'\n" (identifierStr full_name) in*)
         (*let _ = Printf.printf "%s\n" (PrintTypes.expressionStr exp) in*)
         let ret_state =
            {
               state.data with
               types = IdentifierMap.add full_name exp state.data.types;
            }
         in setState state ret_state
      | _ -> state

let getTypeName (tp:exp) : identifier =
   match tp with
   | StmtType(name,_,_,_) -> name
   | StmtAliasType(name,_,_,_) -> name
   | _ -> failwith "getTypeName: invalid type"

