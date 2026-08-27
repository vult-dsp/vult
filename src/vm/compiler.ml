(*
   The MIT License (MIT)

   Copyright (c) 2014-2025 Leonardo Laguna Ruiz

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

open Core
open Prog
open Bytecode

exception Compile_error of string

let error (msg : string) : 'a = raise (Compile_error msg)

(* Compilation context *)
type ctx =
  { var_to_index: (string, int) Hashtbl.t
  ; mutable next_index: int
  ; struct_types: (string, Prog.struct_descr) Hashtbl.t
  ; constant_names: (string, int) Hashtbl.t
  ; function_names: (string, int) Hashtbl.t
  ; external_functions: (string, bool) Hashtbl.t
  ; mutable code: instruction list (* instructions in reverse *)
  ; mutable pc: int (* current program counter *)
  ; mutable constants: value list (* constants in reverse *)
  ; mutable next_const: int
  ; mutable functions: bc_func list (* functions in reverse *)
  ; mutable next_func: int }

let createCtx () : ctx =
  { var_to_index= Hashtbl.create 64
  ; next_index= 0
  ; struct_types= Hashtbl.create 32
  ; constant_names= Hashtbl.create 32
  ; function_names= Hashtbl.create 64
  ; external_functions= Hashtbl.create 32
  ; code= []
  ; pc= 0
  ; constants= []
  ; next_const= 0
  ; functions= []
  ; next_func= 0 }

(* Emit an instruction *)
let emit (ctx : ctx) (instr : instruction) : unit =
  ctx.code <- instr :: ctx.code ;
  ctx.pc <- ctx.pc + instrSize instr

(* Emit and return the PC of the emitted instruction (for patching) *)
let emitWithPc (ctx : ctx) (instr : instruction) : int =
  let pc = ctx.pc in
  emit ctx instr ; pc

(* Add a constant to the pool and return its index *)
let addConstant (ctx : ctx) (v : value) : int =
  let idx = ctx.next_const in
  ctx.constants <- v :: ctx.constants ;
  ctx.next_const <- ctx.next_const + 1 ;
  idx

(* Add a variable and return its index *)
let addVar (ctx : ctx) (name : string) : int =
  match Hashtbl.find_opt ctx.var_to_index name with
  | Some idx ->
      idx
  | None ->
      let idx = ctx.next_index in
      Hashtbl.replace ctx.var_to_index name idx ;
      ctx.next_index <- ctx.next_index + 1 ;
      idx

(* Get member index in a struct *)
let getMemberIndex (descr : struct_descr) (member_name : string) : int =
  let rec loop (i : int) (members : member list) : int =
    match members with
    | [] ->
        error ("Member not found: " ^ member_name ^ " in struct " ^ descr.path)
    | (name, _, _, _) :: _ when String.equal name member_name ->
        i
    | _ :: rest ->
        loop (i + 1) rest
  in
  loop 0 descr.members

(* Type predicates *)
let isIntType (typ : type_) : bool = match typ.t with TInt | TInt16 -> true | _ -> false

let isRealType (typ : type_) : bool = match typ.t with TReal | TFix16 -> true | _ -> false

let isInt16Type (typ : type_) : bool = match typ.t with TInt16 -> true | _ -> false

(* Patch a jump instruction at the given PC to point to the current PC *)
(* Since we store instructions in a list and will convert later, we use a
   mutable reference approach: store placeholder, then patch *)

(* We need a different approach for patching since we build a list.
   Use a ref cell in a wrapper. *)

type label = {mutable target: int}

let newLabel () : label = {target= -1}

let setLabel (ctx : ctx) (lbl : label) : unit = lbl.target <- ctx.pc

(* We'll use a two-pass approach: first emit with placeholder labels,
   then resolve. Store labels alongside instructions. *)

(* Actually, let's use a simpler approach: emit instructions into a Buffer-like
   growable array, and patch directly. *)

type code_builder = {mutable instrs: instruction array; mutable len: int; mutable pc: int (* encoded size counter *)}

let createBuilder () : code_builder = {instrs= Array.make 256 Halt; len= 0; pc= 0}

let builderEmit (b : code_builder) (instr : instruction) : unit =
  if b.len >= Array.length b.instrs then begin
    let new_arr = Array.make (b.len * 2) Halt in
    Array.blit b.instrs 0 new_arr 0 b.len ;
    b.instrs <- new_arr
  end ;
  b.instrs.(b.len) <- instr ;
  b.len <- b.len + 1 ;
  b.pc <- b.pc + instrSize instr

let builderEmitAt (b : code_builder) (idx : int) (instr : instruction) : unit = b.instrs.(idx) <- instr

let builderCurrentInstrIdx (b : code_builder) : int = b.len

let builderCurrentPc (b : code_builder) : int = b.pc

let builderToArray (b : code_builder) : instruction array = Array.sub b.instrs 0 b.len

(* Compilation state with proper patching support *)
type compile_state =
  { builder: code_builder
  ; var_to_index: (string, int) Hashtbl.t
  ; mutable next_index: int
  ; struct_types: (string, struct_descr) Hashtbl.t
  ; constant_names: (string, int) Hashtbl.t
  ; function_names: (string, int) Hashtbl.t
  ; external_functions: (string, bool) Hashtbl.t
  ; mutable constants: value list (* in reverse *)
  ; mutable next_const: int
  ; mutable functions: bc_func list (* in reverse *)
  ; mutable next_func: int
  ; external_name_map: (int, string) Hashtbl.t (* hash -> name for externals *)
  ; constant_exps: (string, Prog.exp) Hashtbl.t (* constants that need runtime evaluation *) }

let createState () : compile_state =
  { builder= createBuilder ()
  ; var_to_index= Hashtbl.create 64
  ; next_index= 0
  ; struct_types= Hashtbl.create 32
  ; constant_names= Hashtbl.create 32
  ; function_names= Hashtbl.create 64
  ; external_functions= Hashtbl.create 32
  ; constants= []
  ; next_const= 0
  ; functions= []
  ; next_func= 0
  ; external_name_map= Hashtbl.create 16
  ; constant_exps= Hashtbl.create 16 }

let stateEmit (st : compile_state) (instr : instruction) : unit = builderEmit st.builder instr

let stateEmitPlaceholder (st : compile_state) (make_instr : int -> instruction) : int =
  let idx = builderCurrentInstrIdx st.builder in
  stateEmit st (make_instr 0) ;
  idx

let statePatch (st : compile_state) (idx : int) (make_instr : int -> instruction) : unit =
  let target = builderCurrentPc st.builder in
  builderEmitAt st.builder idx (make_instr target)

let stateAddConstant (st : compile_state) (v : value) : int =
  let idx = st.next_const in
  st.constants <- v :: st.constants ;
  st.next_const <- st.next_const + 1 ;
  idx

(* Emit a Loadc instruction, using specialized opcodes for indices 0-3 *)
let stateEmitLoadc (st : compile_state) (idx : int) : unit =
  stateEmit st (match idx with 0 -> Loadc0 | 1 -> Loadc1 | 2 -> Loadc2 | 3 -> Loadc3 | _ -> Loadc idx)

let stateAddVar (st : compile_state) (name : string) : int =
  match Hashtbl.find_opt st.var_to_index name with
  | Some idx ->
      idx
  | None ->
      let idx = st.next_index in
      Hashtbl.replace st.var_to_index name idx ;
      st.next_index <- st.next_index + 1 ;
      idx

(* Create a fresh local scope for a function *)
let withFreshScope (st : compile_state) (f : unit -> 'a) : int * 'a =
  let saved_vars = Hashtbl.copy st.var_to_index in
  let saved_next = st.next_index in
  Hashtbl.clear st.var_to_index ;
  st.next_index <- 0 ;
  let result = f () in
  let n_locals = st.next_index in
  Hashtbl.reset st.var_to_index ;
  Hashtbl.iter (fun k v -> Hashtbl.replace st.var_to_index k v) saved_vars ;
  st.next_index <- saved_next ;
  (n_locals, result)

(* Resolve a builtin function call. Returns Some builtin_id if it's a builtin. *)
let resolveBuiltin (path : string) (nargs : int) (result_type : type_) : (builtin_id * int) option =
  match (path, nargs) with
  | "sin", 1 ->
      Some (BI_sin, 1)
  | "cos", 1 ->
      Some (BI_cos, 1)
  | "tan", 1 ->
      Some (BI_tan, 1)
  | "sinh", 1 ->
      Some (BI_sinh, 1)
  | "cosh", 1 ->
      Some (BI_cosh, 1)
  | "tanh", 1 ->
      Some (BI_tanh, 1)
  | "exp", 1 ->
      Some (BI_exp, 1)
  | "log", 1 ->
      Some (BI_log, 1)
  | "log10", 1 ->
      Some (BI_log10, 1)
  | "sqrt", 1 ->
      Some (BI_sqrt, 1)
  | "abs", 1 ->
      Some (BI_abs, 1)
  | "floor", 1 ->
      Some (BI_floor, 1)
  | "ceil", 1 ->
      Some (BI_ceil, 1)
  | "asin", 1 ->
      Some (BI_asin, 1)
  | "acos", 1 ->
      Some (BI_acos, 1)
  | "atan", 1 ->
      Some (BI_atan, 1)
  | "atan2", 2 ->
      Some (BI_atan2, 2)
  | "min", 2 ->
      Some (BI_min, 2)
  | "max", 2 ->
      Some (BI_max, 2)
  | "pow", 2 ->
      Some (BI_pow, 2)
  | "clip", 3 when isRealType result_type ->
      Some (BI_clip_real, 3)
  | "clip", 3 when isIntType result_type ->
      Some (BI_clip_int, 3)
  | "pi", 0 ->
      Some (BI_pi, 0)
  | "eps", 0 ->
      Some (BI_eps, 0)
  | "samplerate", 0 ->
      Some (BI_samplerate, 0)
  | "random", 0 ->
      Some (BI_random, 0)
  | "irandom", 0 ->
      Some (BI_irandom, 0)
  | "real", 1 ->
      Some (BI_real, 1)
  | "int", 1 ->
      Some (BI_int, 1)
  | "int16", 1 ->
      Some (BI_int16, 1)
  | "bool", 1 ->
      Some (BI_bool, 1)
  | "string", 1 ->
      Some (BI_string, 1)
  | "fix16", 1 ->
      Some (BI_fix16, 1)
  | "size", 1 ->
      Some (BI_size, 1)
  | "length", 1 ->
      Some (BI_length, 1)
  | "list_size", 1 ->
      Some (BI_list_size, 1)
  | "list_capacity", 1 ->
      Some (BI_list_capacity, 1)
  | "list_append", 2 ->
      Some (BI_list_append, 2)
  | "list_insert", 3 ->
      Some (BI_list_insert, 3)
  | "list_remove", 2 ->
      Some (BI_list_remove, 2)
  | "list_clear", 1 ->
      Some (BI_list_clear, 1)
  | "list_reserve", 2 ->
      Some (BI_list_reserve, 2)
  | "list_get", 2 ->
      Some (BI_list_get, 2)
  | "list_set", 3 ->
      Some (BI_list_set, 3)
  | _ ->
      None

(* Check if a function is an external runtime function *)
let isExternalRuntime (path : string) : bool =
  match path with
  | "push_block_header"
  | "push_int"
  | "push_float"
  | "update_size"
  | "push_array"
  | "push_string"
  | "serialize_type_descr"
  | "search_field_name"
  | "deserialize_int"
  | "deserialize_float"
  | "deserialize_bool"
  | "deserialize_string"
  | "search_type_description"
  | "first_array_element"
  | "get_array_count"
  | "next_object" ->
      true
  | _ ->
      false

(* Compile an expression - pushes exactly one value onto the stack *)
let rec compileExp (st : compile_state) (exp : Prog.exp) : unit =
  match exp.e with
  | EUnit ->
      let idx = stateAddConstant st Void in
      stateEmitLoadc st idx
  | EEmptyValue -> (
    match exp.t.t with
    | TList _ ->
        let idx = stateAddConstant st (List (ref [])) in
        stateEmitLoadc st idx
    | _ ->
        let idx = stateAddConstant st Void in
        stateEmitLoadc st idx )
  | EBool b ->
      let idx = stateAddConstant st (Bool b) in
      stateEmitLoadc st idx
  | EInt i ->
      let v = if isInt16Type exp.t then Int16 i else Int i in
      let idx = stateAddConstant st v in
      stateEmitLoadc st idx
  | EReal f ->
      let idx = stateAddConstant st (Real f) in
      stateEmitLoadc st idx
  | EFixed f ->
      let idx = stateAddConstant st (Real f) in
      stateEmitLoadc st idx
  | EString s ->
      let idx = stateAddConstant st (String s) in
      stateEmitLoadc st idx
  | EId name -> (
    match Hashtbl.find_opt st.var_to_index name with
    | Some var_idx ->
        stateEmit st
          ( match var_idx with
          | 0 ->
              LoadLocal0
          | 1 ->
              LoadLocal1
          | 2 ->
              LoadLocal2
          | 3 ->
              LoadLocal3
          | _ ->
              LoadLocal var_idx )
    | None -> (
      match Hashtbl.find_opt st.constant_names name with
      | Some const_idx ->
          stateEmitLoadc st const_idx
      | None -> (
        match Hashtbl.find_opt st.constant_exps name with
        | Some const_exp ->
            compileExp st const_exp
        | None ->
            error ("Variable or constant not found: " ^ name) ) ) )
  | EUnOp (UOpNeg, e) ->
      compileExp st e ;
      if isInt16Type exp.t then stateEmit st NegInt16
      else if isRealType exp.t then stateEmit st NegReal
      else stateEmit st NegInt
  | EUnOp (UOpNot, e) ->
      compileExp st e ; stateEmit st Not
  | EOp (OpAdd, e1, {e= EUnOp (UOpNeg, inner); _}) ->
      (* a + (-b) => a - b *)
      compileExp st e1 ;
      compileExp st inner ;
      if isInt16Type e1.t then stateEmit st SubInt16
      else if isRealType e1.t || isRealType inner.t then stateEmit st SubReal
      else stateEmit st SubInt
  | EOp (op, e1, e2) ->
      compileExp st e1 ; compileExp st e2 ; compileOp st op e1.t e2.t
  | EIndex {e; index} ->
      compileExp st e ; compileExp st index ; stateEmit st IndexLoad
  | EArray elems ->
      CCList.iter (compileExp st) elems ;
      stateEmit st (MakeArray (CCList.length elems))
  | ECall {path; args} ->
      compileCall st path args exp.t
  | EIf {cond; then_; else_} ->
      compileExp st cond ;
      let else_jump = stateEmitPlaceholder st (fun t -> JumpIfFalse t) in
      compileExp st then_ ;
      let end_jump = stateEmitPlaceholder st (fun t -> Jump t) in
      statePatch st else_jump (fun t -> JumpIfFalse t) ;
      compileExp st else_ ;
      statePatch st end_jump (fun t -> Jump t)
  | ETuple elems ->
      CCList.iter (compileExp st) elems ;
      stateEmit st (MakeTuple (CCList.length elems))
  | EMember ({e= EId name; t= {t= TStruct descr; _}; _}, member_name) -> (
      (* Fused LoadLocalMember when accessing a member of a local variable *)
      let member_idx = getMemberIndex descr member_name in
      match Hashtbl.find_opt st.var_to_index name with
      | Some var_idx ->
          stateEmit st (LoadLocalMember (var_idx, member_idx))
      | None -> (
        match Hashtbl.find_opt st.constant_names name with
        | Some const_idx ->
            stateEmit st (Loadc const_idx) ; stateEmit st (MemberLoad member_idx)
        | None -> (
          match Hashtbl.find_opt st.constant_exps name with
          | Some const_exp ->
              compileExp st const_exp ; stateEmit st (MemberLoad member_idx)
          | None ->
              error ("Variable or constant not found: " ^ name) ) ) )
  | EMember (e, member_name) -> (
    match e.t.t with
    | TStruct descr ->
        compileExp st e ;
        let member_idx = getMemberIndex descr member_name in
        stateEmit st (MemberLoad member_idx)
    | _ ->
        error "Member access on non-struct type" )
  | ETMember (e, idx) ->
      compileExp st e ; stateEmit st (MemberLoad idx)
  | ERecord {path; elems} -> (
    match Hashtbl.find_opt st.struct_types path with
    | Some descr ->
        let n_members = CCList.length descr.members in
        (* Push members in order *)
        let member_values = Array.make n_members None in
        CCList.iter
          (fun (name, exp) ->
            let idx = getMemberIndex descr name in
            member_values.(idx) <- Some exp )
          elems ;
        Array.iter
          (fun opt_exp ->
            match opt_exp with
            | Some exp ->
                compileExp st exp
            | None ->
                let idx = stateAddConstant st Void in
                stateEmitLoadc st idx )
          member_values ;
        stateEmit st (MakeStruct n_members)
    | None ->
        error ("Unknown struct type: " ^ path) )

and compileOp (st : compile_state) (op : operator) (t1 : type_) (t2 : type_) : unit =
  match op with
  | OpAdd when isInt16Type t1 && isInt16Type t2 ->
      stateEmit st AddInt16
  | OpAdd when isIntType t1 && isIntType t2 ->
      stateEmit st AddInt
  | OpAdd when isRealType t1 || isRealType t2 ->
      stateEmit st AddReal
  | OpAdd ->
      stateEmit st AddInt
  | OpSub when isInt16Type t1 && isInt16Type t2 ->
      stateEmit st SubInt16
  | OpSub when isIntType t1 && isIntType t2 ->
      stateEmit st SubInt
  | OpSub when isRealType t1 || isRealType t2 ->
      stateEmit st SubReal
  | OpSub ->
      stateEmit st SubInt
  | OpMul when isInt16Type t1 && isInt16Type t2 ->
      stateEmit st MulInt16
  | OpMul when isIntType t1 && isIntType t2 ->
      stateEmit st MulInt
  | OpMul when isRealType t1 || isRealType t2 ->
      stateEmit st MulReal
  | OpMul ->
      stateEmit st MulInt
  | OpDiv when isInt16Type t1 && isInt16Type t2 ->
      stateEmit st DivInt16
  | OpDiv when isIntType t1 && isIntType t2 ->
      stateEmit st DivInt
  | OpDiv when isRealType t1 || isRealType t2 ->
      stateEmit st DivReal
  | OpDiv ->
      stateEmit st DivInt
  | OpMod when isInt16Type t1 && isInt16Type t2 ->
      stateEmit st ModInt16
  | OpMod when isIntType t1 && isIntType t2 ->
      stateEmit st ModInt
  | OpMod when isRealType t1 || isRealType t2 ->
      stateEmit st ModReal
  | OpMod ->
      stateEmit st (BinOp BMod)
  | OpEq when isInt16Type t1 && isInt16Type t2 ->
      stateEmit st EqInt16
  | OpEq when isIntType t1 && isIntType t2 ->
      stateEmit st EqInt
  | OpEq when isRealType t1 || isRealType t2 ->
      stateEmit st EqReal
  | OpEq ->
      stateEmit st EqInt
  | OpLt when isInt16Type t1 && isInt16Type t2 ->
      stateEmit st LtInt16
  | OpLt when isIntType t1 && isIntType t2 ->
      stateEmit st LtInt
  | OpLt when isRealType t1 || isRealType t2 ->
      stateEmit st LtReal
  | OpLt ->
      stateEmit st LtInt
  | OpGt when isInt16Type t1 && isInt16Type t2 ->
      stateEmit st GtInt16
  | OpGt when isIntType t1 && isIntType t2 ->
      stateEmit st GtInt
  | OpGt when isRealType t1 || isRealType t2 ->
      stateEmit st GtReal
  | OpGt ->
      stateEmit st GtInt
  | OpLe ->
      stateEmit st (BinOp BLe)
  | OpGe ->
      stateEmit st (BinOp BGe)
  | OpNe ->
      stateEmit st (BinOp BNe)
  | OpLand ->
      stateEmit st (BinOp BLand)
  | OpLor ->
      stateEmit st (BinOp BLor)
  | OpBand ->
      stateEmit st (BinOp BBand)
  | OpBor ->
      stateEmit st (BinOp BBor)
  | OpBxor ->
      stateEmit st (BinOp BBxor)
  | OpLsh ->
      stateEmit st (BinOp BLsh)
  | OpRsh ->
      stateEmit st (BinOp BRsh)

and compileCall (st : compile_state) (path : string) (args : exp list) (result_type : type_) : unit =
  let nargs = CCList.length args in
  (* Check for builtins first *)
  match resolveBuiltin path nargs result_type with
  | Some (bi, _nargs) ->
      CCList.iter (compileExp st) args ;
      stateEmit st (CallBuiltin (bi, nargs))
  | None -> (
      if isExternalRuntime path then begin
        CCList.iter (compileExp st) args ;
        Hashtbl.replace st.external_name_map (Hashtbl.hash path) path ;
        stateEmit st (CallExternal (path, nargs))
      end
      else
        match Hashtbl.find_opt st.function_names path with
        | Some func_idx -> (
            CCList.iter (compileExp st) args ;
            match nargs with
            | 0 ->
                stateEmit st (Call0 func_idx)
            | 1 ->
                stateEmit st (Call1 func_idx)
            | 2 ->
                stateEmit st (Call2 func_idx)
            | 3 ->
                stateEmit st (Call3 func_idx)
            | _ ->
                stateEmit st (Call (func_idx, nargs)) )
        | None ->
            if Hashtbl.mem st.external_functions path then begin
              CCList.iter (compileExp st) args ;
              Hashtbl.replace st.external_name_map (Hashtbl.hash path) path ;
              stateEmit st (CallExternal (path, nargs))
            end
            else error ("Function not found: " ^ path) )

(* Compile a left-value expression for storing *)
and compileLexp (st : compile_state) (lexp : Prog.lexp) : unit =
  match lexp.l with
  | LWild ->
      stateEmit st Pop
  | LId name -> (
    match Hashtbl.find_opt st.var_to_index name with
    | Some var_idx ->
        stateEmit st
          ( match var_idx with
          | 0 ->
              StoreLocal0
          | 1 ->
              StoreLocal1
          | 2 ->
              StoreLocal2
          | 3 ->
              StoreLocal3
          | _ ->
              StoreLocal var_idx )
    | None ->
        error ("Variable not found in left-value: " ^ name) )
  | LMember (parent_lexp, member_name) -> (
    match parent_lexp.t.t with
    | TStruct descr ->
        let member_idx = getMemberIndex descr member_name in
        compileLexpMemberStore st parent_lexp member_idx
    | _ ->
        error "Member access on non-struct type in assignment" )
  | LIndex {e= arr_lexp; index} ->
      compileLexpIndexStore st arr_lexp index
  | LTuple lexps ->
      let n = CCList.length lexps in
      (* The tuple value is on the stack. We need to unpack it. *)
      (* For each element, dup the tuple, member-load, then store *)
      CCList.iteri
        (fun i lexp ->
          if i < n - 1 then stateEmit st Dup ;
          stateEmit st (MemberLoad i) ;
          compileLexp st lexp )
        lexps

(* Store into a member: load the container, do member store *)
and compileLexpMemberStore (st : compile_state) (parent : Prog.lexp) (member_idx : int) : unit =
  (* value is on the stack *)
  (* We need to load the parent container for mutation *)
  match parent.l with
  | LId name -> (
    match Hashtbl.find_opt st.var_to_index name with
    | Some var_idx ->
        stateEmit st (StoreLocalMember (var_idx, member_idx))
    | None ->
        error ("Variable not found: " ^ name) )
  | LMember (grandparent, gp_member_name) -> (
    match grandparent.t.t with
    | TStruct descr ->
        let gp_member_idx = getMemberIndex descr gp_member_name in
        compileLexpMemberStore st grandparent gp_member_idx ;
        stateEmit st (LoadLocal (getVarIdx st grandparent)) ;
        stateEmit st (MemberStore member_idx)
    | _ ->
        error "Nested member access on non-struct" )
  | _ ->
      error "Complex l-value member store not supported"

and getVarIdx (st : compile_state) (lexp : Prog.lexp) : int =
  match lexp.l with
  | LId name -> (
    match Hashtbl.find_opt st.var_to_index name with Some idx -> idx | None -> error ("Variable not found: " ^ name) )
  | _ ->
      error "Expected simple variable in l-value chain"

(* Store into an index *)
and compileLexpIndexStore (st : compile_state) (arr_lexp : Prog.lexp) (index : Prog.exp) : unit =
  (* value is on the stack *)
  compileExp st index ; compileLexpLoad st arr_lexp ; stateEmit st IndexStore

(* Load the value of an l-value expression (for reading the container) *)
and compileLexpLoad (st : compile_state) (lexp : Prog.lexp) : unit =
  match lexp.l with
  | LId name -> (
    match Hashtbl.find_opt st.var_to_index name with
    | Some var_idx ->
        stateEmit st (LoadLocal var_idx)
    | None ->
        error ("Variable not found: " ^ name) )
  | LMember (parent, member_name) -> (
    match parent.t.t with
    | TStruct descr ->
        compileLexpLoad st parent ;
        let idx = getMemberIndex descr member_name in
        stateEmit st (MemberLoad idx)
    | _ ->
        error "Member access on non-struct" )
  | LIndex {e= arr_lexp; index} ->
      compileLexpLoad st arr_lexp ; compileExp st index ; stateEmit st IndexLoad
  | _ ->
      error "Cannot load from this l-value"

(* Emit code to push a default value for the given type onto the stack *)
let rec emitDefaultValue (st : compile_state) (typ : Prog.type_) : unit =
  match typ.t with
  | TVoid _ ->
      let idx = stateAddConstant st Void in
      stateEmitLoadc st idx
  | TInt ->
      let idx = stateAddConstant st (Int 0) in
      stateEmitLoadc st idx
  | TInt16 ->
      let idx = stateAddConstant st (Int16 0) in
      stateEmitLoadc st idx
  | TReal | TFix16 ->
      let idx = stateAddConstant st (Real 0.0) in
      stateEmitLoadc st idx
  | TBool ->
      let idx = stateAddConstant st (Bool false) in
      stateEmitLoadc st idx
  | TString ->
      let idx = stateAddConstant st (String "") in
      stateEmitLoadc st idx
  | TArray (Some size, elem_type) ->
      for _ = 1 to size do
        emitDefaultValue st elem_type
      done ;
      stateEmit st (MakeArray size)
  | TArray (None, _) ->
      stateEmit st (MakeArray 0)
  | TList _ ->
      let idx = stateAddConstant st (List (ref [])) in
      stateEmitLoadc st idx
  | TStruct descr ->
      CCList.iter (fun (_, mtyp, _, _) -> emitDefaultValue st mtyp) descr.members ;
      stateEmit st (MakeStruct (CCList.length descr.members))
  | TTuple types ->
      CCList.iter (emitDefaultValue st) types ;
      stateEmit st (MakeTuple (CCList.length types))
  | TEmptyType ->
      let idx = stateAddConstant st Void in
      stateEmitLoadc st idx

(* Compile a statement *)
and compileStmt (st : compile_state) (stmt : Prog.stmt) : unit =
  match stmt.s with
  | StmtDecl (dexp, init_opt) -> (
    match dexp.d with
    | DId (name, dim_opt) -> (
        let _var_idx = stateAddVar st name in
        match init_opt with
        | Some init_exp ->
            compileExp st init_exp ;
            let var_idx = stateAddVar st name in
            stateEmit st
              ( match var_idx with
              | 0 ->
                  StoreLocal0
              | 1 ->
                  StoreLocal1
              | 2 ->
                  StoreLocal2
              | 3 ->
                  StoreLocal3
              | _ ->
                  StoreLocal var_idx )
        | None ->
            (* Initialize with default value based on type.
               For scalar types (int, real, bool, string), skip emitting the default:
               the locals slot is already Void from frame setup, and StmtBind will
               assign the real value before the variable is read. For compound types
               (struct, array, tuple, list), we must emit the default to create the
               proper container structure. *)
            let typ = match dim_opt with Some dim -> {dexp.t with t= TArray (Some dim, dexp.t)} | None -> dexp.t in
            let needs_default = match typ.t with TStruct _ | TArray _ | TTuple _ | TList _ -> true | _ -> false in
            if needs_default then begin
              emitDefaultValue st typ ;
              let var_idx = stateAddVar st name in
              stateEmit st
                ( match var_idx with
                | 0 ->
                    StoreLocal0
                | 1 ->
                    StoreLocal1
                | 2 ->
                    StoreLocal2
                | 3 ->
                    StoreLocal3
                | _ ->
                    StoreLocal var_idx )
            end ) )
  | StmtBind (lexp, exp) ->
      compileExp st exp ; compileLexp st lexp
  | StmtReturn exp ->
      compileExp st exp ; stateEmit st Return
  | StmtBlock stmts ->
      CCList.iter (compileStmt st) stmts
  | StmtIf (cond, then_stmt, else_stmt_opt) -> (
      compileExp st cond ;
      match else_stmt_opt with
      | None ->
          let end_jump = stateEmitPlaceholder st (fun t -> JumpIfFalse t) in
          compileStmt st then_stmt ;
          statePatch st end_jump (fun t -> JumpIfFalse t)
      | Some else_stmt ->
          let else_jump = stateEmitPlaceholder st (fun t -> JumpIfFalse t) in
          compileStmt st then_stmt ;
          let end_jump = stateEmitPlaceholder st (fun t -> Jump t) in
          statePatch st else_jump (fun t -> JumpIfFalse t) ;
          compileStmt st else_stmt ;
          statePatch st end_jump (fun t -> Jump t) )
  | StmtWhile (cond, body) ->
      let loop_pc = builderCurrentPc st.builder in
      compileExp st cond ;
      let exit_jump = stateEmitPlaceholder st (fun t -> JumpIfFalse t) in
      compileStmt st body ;
      stateEmit st (Jump loop_pc) ;
      statePatch st exit_jump (fun t -> JumpIfFalse t)
  | StmtSwitch (exp, cases, default_opt) ->
      compileExp st exp ;
      (* For each case: dup, compile case value, compare, jump if false to next *)
      let end_jumps = ref [] in
      CCList.iter
        (fun (case_exp, case_stmt) ->
          stateEmit st Dup ;
          compileExp st case_exp ;
          (* Emit comparison based on type *)
          let typ = exp.t in
          if isInt16Type typ then stateEmit st EqInt16
          else if isIntType typ then stateEmit st EqInt
          else if isRealType typ then stateEmit st EqReal
          else stateEmit st EqInt ;
          let next_case = stateEmitPlaceholder st (fun t -> JumpIfFalse t) in
          stateEmit st Pop ;
          (* Pop the duplicated switch value *)
          compileStmt st case_stmt ;
          let end_jump = stateEmitPlaceholder st (fun t -> Jump t) in
          end_jumps := end_jump :: !end_jumps ;
          statePatch st next_case (fun t -> JumpIfFalse t) )
        cases ;
      (* Default case *)
      stateEmit st Pop ;
      (* Pop the duplicated switch value *)
      ( match default_opt with
      | Some default_stmt ->
          compileStmt st default_stmt
      | None ->
          () ) ;
      (* Patch all end jumps *)
      CCList.iter (fun ej -> statePatch st ej (fun t -> Jump t)) !end_jumps

(* Compile a top-level statement *)
let rec compileTopStmt (st : compile_state) (top : Prog.top_stmt) : unit =
  match top.top with
  | TopType descr ->
      Hashtbl.replace st.struct_types descr.path descr
  | TopConstant (name, _, _, exp, _) ->
      let value = evalConstantExp st exp in
      if value <> Void then begin
        let const_idx = st.next_const in
        Hashtbl.replace st.constant_names name const_idx ;
        let _idx = stateAddConstant st value in
        ignore _idx
      end
      else
        (* Store expression for runtime evaluation when referenced *)
        Hashtbl.replace st.constant_exps name exp
  | TopFunction (def, body) ->
      (* Register function index before compiling body (for recursion) *)
      let func_idx = st.next_func in
      Hashtbl.replace st.function_names def.name func_idx ;
      st.next_func <- st.next_func + 1 ;
      (* Compile function body in fresh scope *)
      let entry_pc = builderCurrentPc st.builder in
      let n_locals, () =
        withFreshScope st (fun () ->
            (* Add parameters *)
            CCList.iter (fun (p : Prog.param) -> ignore (stateAddVar st p.name)) def.args ;
            (* Compile body *)
            compileStmt st body ;
            (* Add implicit return void if needed *)
            let ret_type = snd def.t in
            match ret_type.t with
            | TVoid _ ->
                stateEmit st ReturnVoid
            | _ ->
                (* Check if last instruction is already a return *)
                let last_idx = st.builder.len - 1 in
                if last_idx >= 0 then
                  match st.builder.instrs.(last_idx) with Return | ReturnVoid -> () | _ -> stateEmit st ReturnVoid
                else stateEmit st ReturnVoid )
      in
      let bc_func = {name= def.name; entry_pc; n_args= CCList.length def.args; n_locals} in
      st.functions <- bc_func :: st.functions
  | TopExternal (def, _) ->
      Hashtbl.replace st.external_functions def.name true
  | TopAlias _ ->
      ()

(* Evaluate a constant expression at compile time *)
and evalConstantExp (st : compile_state) (exp : Prog.exp) : value =
  match exp.e with
  | EUnit ->
      Void
  | EEmptyValue -> (
    match exp.t.t with TList _ -> List (ref []) | _ -> Void )
  | EBool b ->
      Bool b
  | EInt i ->
      if isInt16Type exp.t then Int16 i else Int i
  | EReal f ->
      Real f
  | EFixed f ->
      Real f
  | EString s ->
      String s
  | EId name -> (
    match Hashtbl.find_opt st.constant_names name with
    | Some idx -> (
        let consts = CCList.rev st.constants in
        match CCList.nth_opt consts idx with Some v -> v | None -> Void )
    | None ->
        Void )
  | EArray elems ->
      Array (Array.of_list (CCList.map (evalConstantExp st) elems))
  | ERecord {path; elems} -> (
    match Hashtbl.find_opt st.struct_types path with
    | Some descr ->
        let n_members = CCList.length descr.members in
        let arr = Array.make n_members Void in
        CCList.iter
          (fun (name, exp) ->
            let idx = getMemberIndex descr name in
            arr.(idx) <- evalConstantExp st exp )
          elems ;
        Struct arr
    | None ->
        Void )
  | EOp (OpAdd, e1, e2) -> (
    match (evalConstantExp st e1, evalConstantExp st e2) with
    | Int a, Int b ->
        Int (a + b)
    | Int16 a, Int16 b ->
        Int16 (max (-32768) (min 32767 (a + b)))
    | Real a, Real b ->
        Real (a +. b)
    | _ ->
        Void )
  | EOp (OpSub, e1, e2) -> (
    match (evalConstantExp st e1, evalConstantExp st e2) with
    | Int a, Int b ->
        Int (a - b)
    | Int16 a, Int16 b ->
        Int16 (max (-32768) (min 32767 (a - b)))
    | Real a, Real b ->
        Real (a -. b)
    | _ ->
        Void )
  | EOp (OpMul, e1, e2) -> (
    match (evalConstantExp st e1, evalConstantExp st e2) with
    | Int a, Int b ->
        Int (a * b)
    | Int16 a, Int16 b ->
        Int16 (max (-32768) (min 32767 (a * b)))
    | Real a, Real b ->
        Real (a *. b)
    | _ ->
        Void )
  | EOp (OpDiv, e1, e2) -> (
    match (evalConstantExp st e1, evalConstantExp st e2) with
    | Int a, Int b when b <> 0 ->
        Int (a / b)
    | Int16 a, Int16 b when b <> 0 ->
        Int16 (a / b)
    | Real a, Real b when b <> 0.0 ->
        Real (a /. b)
    | _ ->
        Void )
  | EOp (OpMod, e1, e2) -> (
    match (evalConstantExp st e1, evalConstantExp st e2) with
    | Int a, Int b when b <> 0 ->
        Int (a mod b)
    | Int16 a, Int16 b when b <> 0 ->
        Int16 (a mod b)
    | Real a, Real b when b <> 0.0 ->
        Real (mod_float a b)
    | _ ->
        Void )
  | EOp (OpLsh, e1, e2) -> (
    match (evalConstantExp st e1, evalConstantExp st e2) with
    | Int a, Int b ->
        Int (a lsl b)
    | Int16 a, Int16 b ->
        Int16 (max (-32768) (min 32767 (a lsl b)))
    | _ ->
        Void )
  | EOp (OpRsh, e1, e2) -> (
    match (evalConstantExp st e1, evalConstantExp st e2) with
    | Int a, Int b ->
        Int (a asr b)
    | Int16 a, Int16 b ->
        Int16 (a asr b)
    | _ ->
        Void )
  | EUnOp (UOpNeg, e) -> (
    match evalConstantExp st e with
    | Int a ->
        Int (-a)
    | Int16 a ->
        Int16 (max (-32768) (min 32767 (-a)))
    | Real a ->
        Real (-.a)
    | _ ->
        Void )
  | _ ->
      Void

(* Main compilation entry point *)
let compile (prog : Prog.top_stmt list) : bc_prog =
  let st = createState () in
  CCList.iter (compileTopStmt st) prog ;
  let code = builderToArray st.builder in
  let constants = Array.of_list (CCList.rev st.constants) in
  let functions = Array.of_list (CCList.rev st.functions) in
  let code, functions = Inline.inline code functions in
  let code, functions = Optimize.optimize code functions in
  let function_names = Hashtbl.create (Array.length functions) in
  Array.iter
    (fun (f : bc_func) -> Hashtbl.replace function_names f.name (Hashtbl.find st.function_names f.name))
    functions ;
  {code; constants; functions; function_names}

(* Extend an existing program with new top-level statements *)
let extendProgram (existing : bc_prog) (prog : Prog.top_stmt list) : bc_prog =
  let st = createState () in
  (* Import existing constants *)
  Array.iter (fun v -> ignore (stateAddConstant st v)) existing.constants ;
  (* Import existing functions *)
  Array.iter
    (fun (f : bc_func) ->
      Hashtbl.replace st.function_names f.name st.next_func ;
      st.next_func <- st.next_func + 1 ;
      st.functions <- f :: st.functions )
    existing.functions ;
  (* Import existing code *)
  Array.iter (fun instr -> builderEmit st.builder instr) existing.code ;
  (* Compile new statements *)
  CCList.iter (compileTopStmt st) prog ;
  let code = builderToArray st.builder in
  let constants = Array.of_list (CCList.rev st.constants) in
  let functions = Array.of_list (CCList.rev st.functions) in
  let code, functions = Inline.inline code functions in
  let code, functions = Optimize.optimize code functions in
  let function_names = Hashtbl.create (Array.length functions) in
  Array.iteri (fun i (f : bc_func) -> Hashtbl.replace function_names f.name i) functions ;
  {code; constants; functions; function_names}
