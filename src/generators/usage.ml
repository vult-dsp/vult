(*
   The MIT License (MIT)

   Copyright (c) 2014-2024 Leonardo Laguna Ruiz

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
open Core.Prog
module Set = Util.Maps.Set

type features = {fix16_math: bool; random: bool; strings: bool; serialization: bool; tuples: bool; lists: bool}

(* Generic traversal: applies [on_exp] to every expression node and [on_type]
   to the type of every expression, declaration, parameter, return value,
   struct member and constant of the program. [on_type] receives each type as
   found; it is responsible for recursing into sub-types if it needs to. *)
let iterProg ~(on_exp : exp -> unit) ~(on_type : type_ -> unit) (prog : prog) : unit =
  let rec iterExp (e : exp) =
    on_exp e ;
    on_type e.t ;
    match e.e with
    | EUnit | EEmptyValue | EBool _ | EInt _ | EReal _ | EFixed _ | EString _ | EId _ ->
        ()
    | EUnOp (_, e1) ->
        iterExp e1
    | EOp (_, e1, e2) ->
        iterExp e1 ; iterExp e2
    | EIndex {e; index} ->
        iterExp e ; iterExp index
    | EArray elems ->
        CCList.iter iterExp elems
    | ECall {args; _} ->
        CCList.iter iterExp args
    | EIf {cond; then_; else_} ->
        iterExp cond ; iterExp then_ ; iterExp else_
    | ETuple elems ->
        CCList.iter iterExp elems
    | EMember (e1, _) ->
        iterExp e1
    | ETMember (e1, _) ->
        iterExp e1
    | ERecord {elems; _} ->
        CCList.iter (fun (_, e1) -> iterExp e1) elems
  in
  let rec iterLExp (l : lexp) =
    on_type l.t ;
    match l.l with
    | LWild | LId _ ->
        ()
    | LMember (l1, _) ->
        iterLExp l1
    | LIndex {e; index} ->
        iterLExp e ; iterExp index
    | LTuple elems ->
        CCList.iter iterLExp elems
  in
  let iterDExp (d : dexp) = on_type d.t in
  let rec iterStmt (s : stmt) =
    match s.s with
    | StmtDecl (lhs, rhs) ->
        iterDExp lhs ; CCOption.iter iterExp rhs
    | StmtBind (lhs, rhs) ->
        iterLExp lhs ; iterExp rhs
    | StmtReturn e ->
        iterExp e
    | StmtBlock stmts ->
        CCList.iter iterStmt stmts
    | StmtIf (cond, then_, else_) ->
        iterExp cond ; iterStmt then_ ; CCOption.iter iterStmt else_
    | StmtWhile (cond, body) ->
        iterExp cond ; iterStmt body
    | StmtSwitch (e, cases, default) ->
        iterExp e ;
        CCList.iter (fun (case, body) -> iterExp case ; iterStmt body) cases ;
        CCOption.iter iterStmt default
  in
  let iterFunctionDef (def : function_def) =
    CCList.iter (fun (p : param) -> on_type p.t) def.args ;
    let args_t, ret = def.t in
    CCList.iter on_type args_t ; on_type ret
  in
  let iterTop (top : top_stmt) =
    match top.top with
    | TopExternal (def, _) ->
        iterFunctionDef def
    | TopFunction (def, body) ->
        iterFunctionDef def ; iterStmt body
    | TopType {members; _} ->
        CCList.iter (fun (_, t, _, _) -> on_type t) members
    | TopAlias _ ->
        ()
    | TopConstant (_, _, t, e, _) ->
        on_type t ; iterExp e
  in
  CCList.iter iterTop prog

let calledFunctions (prog : prog) : Set.t =
  let calls = ref Set.empty in
  let on_exp (e : exp) = match e.e with ECall {path; _} -> calls := Set.add path !calls | _ -> () in
  iterProg ~on_exp ~on_type:(fun _ -> ()) prog ;
  !calls

let existsExp (pred : exp -> bool) (prog : prog) : bool =
  let found = ref false in
  let on_exp e = if pred e then found := true in
  iterProg ~on_exp ~on_type:(fun _ -> ()) prog ;
  !found

let existsType (pred : type_ -> bool) (prog : prog) : bool =
  let found = ref false in
  let rec check (t : type_) =
    if pred t then found := true
    else
      match t.t with
      | TArray (_, sub) | TList sub ->
          check sub
      | TTuple elems | TVoid (Some elems) ->
          CCList.iter check elems
      | _ ->
          ()
  in
  iterProg ~on_exp:(fun _ -> ()) ~on_type:check prog ;
  !found

(* The names below are the ones found in the program after the replacements
   ([Replacements.Cpp]) and the serializer generation ([Core.Serializer]) have
   run, which is the form the C++ printer receives. *)

let fix16_math_functions =
  Set.of_list ["fix_exp"; "fix_sin"; "fix_cos"; "fix_tan"; "fix_sinh"; "fix_cosh"; "fix_tanh"; "fix_sqrt"]

let random_functions = Set.of_list ["float_random"; "fix_random"; "int_random"]

let string_functions = Set.of_list ["fix_to_string"; "bool_to_string"; "int16_to_string"; "std::to_string"]

let serialization_functions =
  Set.of_list
    [ "push_block_header"
    ; "push_header"
    ; "push_array"
    ; "push_int"
    ; "push_float"
    ; "push_string"
    ; "update_size"
    ; "search_field_name"
    ; "serialize_type_descr"
    ; "search_type_description"
    ; "deserialize_int"
    ; "deserialize_float"
    ; "deserialize_string"
    ; "goto_data"
    ; "first_array_element"
    ; "get_array_count"
    ; "get_field"
    ; "match_string"
    ; "next_object" ]

let serialization_types = Set.of_list ["CustomBuffer"; "CustomTypeDescr"]

let detect (prog : prog) : features =
  let fix16_math = ref false in
  let random = ref false in
  let strings = ref false in
  let serialization = ref false in
  let tuples = ref false in
  let lists = ref false in
  let on_exp (e : exp) =
    match e.e with
    | ECall {path; _} ->
        if Set.mem path fix16_math_functions then fix16_math := true
        else if Set.mem path random_functions then random := true
        else if Set.mem path string_functions then strings := true
        else if Set.mem path serialization_functions then serialization := true
    | _ ->
        ()
  in
  let rec on_type (t : type_) =
    match t.t with
    | TString ->
        strings := true
    | TTuple elems ->
        tuples := true ;
        CCList.iter on_type elems
    | TList sub ->
        lists := true ;
        on_type sub
    | TArray (_, sub) ->
        on_type sub
    | TStruct {path; _} ->
        (* Members are not visited here: user structs are covered by their
           [TopType] declaration and the runtime structs have no members in
           the program representation. *)
        if Set.mem path serialization_types then serialization := true
    | TVoid (Some elems) ->
        CCList.iter on_type elems
    | TVoid None | TInt | TInt16 | TReal | TBool | TFix16 | TEmptyType ->
        ()
  in
  iterProg ~on_exp ~on_type prog ;
  (* The serialization format stores type and field names as strings. *)
  let serialization = !serialization in
  { fix16_math= !fix16_math
  ; random= !random
  ; strings= !strings || serialization
  ; serialization
  ; tuples= !tuples
  ; lists= !lists }

let runtimeDefines (f : features) : Pla.t =
  let disable flag enabled = if enabled then None else Some {%pla|#define <#flag#s><#>|} in
  let defines =
    CCList.filter_map
      (fun x -> x)
      [ disable "VULT_NO_SERIALIZATION" f.serialization
      ; disable "VULT_NO_STRING" f.strings
      ; disable "VULT_NO_FIX16_MATH" f.fix16_math
      ; disable "VULT_NO_RANDOM" f.random
      ; disable "VULT_NO_TUPLE" f.tuples
      ; disable "VULT_NO_LIST" f.lists ]
  in
  if CCList.is_empty defines then Pla.unit
  else
    let defines = Pla.join defines in
    {%pla|/* Features disabled because this program does not use them.
   Compile with -DVULT_FULL_RUNTIME to enable the full runtime. */
#ifndef VULT_FULL_RUNTIME
<#defines#>#endif // VULT_FULL_RUNTIME
|}
