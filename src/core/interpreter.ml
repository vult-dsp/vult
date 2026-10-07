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

(*
   Vult Interpreter — Single-Pass Closure Compilation Engine
   ==========================================================

   This module implements a complete interpreter for the Vult DSP language. It compiles
   the Prog AST directly into OCaml closures in a single pass, eliminating the need for
   an intermediate IR (iexp/istmt/ilexp).

   Architecture Overview
   ---------------------

   The interpreter operates as a single compilation pass:

     Prog AST (from Toprog/Passes)
       |
       v
     compileExp / compileStmt / compileProgramFunction
       Converts Prog.exp/stmt directly into OCaml closures (compiled_result/compiled_stmt).
       - Resolves variable names to frame indices
       - Resolves struct member names to integer offsets
       - Specializes generic arithmetic by type at compile time
       - Inlines known builtin functions into optimized closures
       - Fuses common access patterns (var.field, var[idx])
       - Performs constant propagation and folding
       - Eliminates dead branches
       - Inlines small expression-only functions at call sites
       |
       v
     Execution
       callCompiledFunctionByIdx invokes compiled closures; each function call
       allocates a frame of cf_locals slots.

   Runtime Model
   -------------
   - Frame-based: the local variables and arguments of a call live in a dvalue array
     allocated for that call and indexed by variable index. Frames are short-lived,
     so they stay in the minor heap and writing to them is cheap for the GC.
   - DReal values are boxed floats, so real arithmetic allocates; DBool values are
     shared (see dbool).
   - Mutable aggregates: DArray and DStruct contain OCaml arrays that are mutated
     in-place for assignments. DList wraps a mutable ref to an OCaml list.
   - Call depth tracking: each call increments runtime.depth; exceeding max_call_depth
     raises a runtime error (prevents stack overflow from infinite recursion).

   Entry Points
   ------------
   - transformProgram: full pipeline from Prog AST to compiled iprog
   - evaluateMainExpression: parse, compile, and evaluate a single expression
   - renderAudioExpression:  compile and render audio to a WAV file
   - callFunctionEntry:      call a compiled function by index (external API)
*)

open Prog
open Util.Maps

(* ---- Runtime Value Representation ----

   dvalue is the universal value type used throughout interpretation.
   All values in the interpreter — locals, arguments, return values, struct fields,
   and array elements — are represented as dvalue.

   Design notes:
   - DVoid serves as both the unit value and the uninitialized sentinel.
   - DInt16 uses a regular OCaml int but clamps to [-32768, 32767] on arithmetic.
   - DArray and DStruct use mutable OCaml arrays for O(1) indexed access and in-place update.
   - DList uses a mutable ref to an immutable OCaml list, supporting append/insert/remove.
*)
type dvalue =
  | DVoid
  | DInt of int
  | DInt16 of int
  | DReal of float
  | DBool of bool
  | DString of string
  | DArray of dvalue array
  | DList of dvalue list ref (* Dynamic list with mutable reference *)
  | DStruct of dvalue array

(* Booleans are shared so that comparisons do not allocate *)
let dtrue = DBool true

let dfalse = DBool false

let dbool (b : bool) : dvalue = if b then dtrue else dfalse

(* ---- Runtime Infrastructure ----

   runtime holds the state of an evaluation that compiled code needs besides its frame: the
   functions being called (for error messages) and the sample rate. There is one per program;
   the closures that need it capture it when they are compiled, so it does not have to be passed
   around. The entry points reset it. A runtime error leaves depth unchanged, so the call stack
   at the point of the error can be reported by the entry point.
*)
type runtime =
  { mutable depth: int (* Number of active calls *)
  ; call_names: string array (* call_names.(i) is the function running at depth i + 1 *)
  ; mutable sample_rate: float option (* Sample rate from CLI args *) }

let max_call_depth = 50

(* Statement execution returns either Continue (fall through) or Return with a value.
   This replaces exception-based control flow for better performance. *)
type exec_result = Continue | Return of dvalue

(* ---- Compiled Closure Types ----

   After compilation, every expression and statement is represented as an OCaml closure.
   The closure signature is: frame -> result

   - compiled_exp: evaluates an expression, returns a dvalue
   - compiled_stmt: executes a statement, returns Continue or Return
   - compiled_func: bundles a compiled function body with its metadata
*)
type compiled_exp = dvalue array -> dvalue

type compiled_stmt = dvalue array -> exec_result

(* compiled_result is the return type of compileExp. It classifies expressions
   into three categories to enable compile-time optimizations:

   - CConstant v: The expression always evaluates to v. No closure is allocated.

   - CVar idx: The expression is a direct variable read from frame[idx].
     This is tracked separately from CDynamic because parent nodes can generate
     "fused" closures that inline the array access, eliminating one indirect call.

   - CDynamic f: A general runtime closure. Used when the expression depends on
     runtime state in a way that can't be simplified further. *)
type compiled_result = CConstant of dvalue | CVar of int | CDynamic of compiled_exp

type compiled_func = {cf_name: string; cf_args: int array; cf_locals: int; cf_body: compiled_stmt}

(* ---- Program State ----

   iprog holds all state for a compiled program. It is built incrementally by
   processStatement, which compiles each top-level definition in a single pass.

   compiled_functions is a resizable array. Function closures capture iprog.prog
   (the iprog record itself), so array resizing during compilation is safe.
*)
type iprog =
  { mutable ifunction_names: int Map.t (* Name -> index mapping *)
  ; mutable compiled_functions: compiled_func array (* Compiled closures *)
  ; mutable iconstants: dvalue array (* Global constants *)
  ; mutable iconstants_count: int (* Number of constants currently stored *)
  ; mutable struct_types: struct_descr Map.t (* Struct type definitions *)
  ; mutable constant_names: int Map.t (* Constant name -> index mapping *)
  ; mutable external_functions: Set.t (* External function names *)
  ; mutable inlinable: (string list * exp) Map.t (* Function name -> (param_names, body_exp) for inlining *)
  ; runtime: runtime }

(* ---- Compilation Context ----

   compile_ctx is used during compilation to resolve names to indices.
   var_to_index is mutable because new variables are added as declarations
   are encountered. The other fields are immutable references into the iprog.
*)
type compile_ctx =
  { mutable var_to_index: int Map.t (* Variable name -> frame index *)
  ; mutable next_index: int (* Next available frame slot *)
  ; struct_types: struct_descr Map.t (* Struct type definitions *)
  ; constant_names: int Map.t (* Constant name -> index *)
  ; function_names: int Map.t (* Function name -> index *)
  ; external_functions: Set.t (* External function names *)
  ; prog: iprog (* Back-reference to the program being built *) }

(* Exceptions *)
exception Runtime_error of string

let error (msg : string) : 'a = raise (Runtime_error msg)

(* Describes the active calls, innermost first *)
let callStackInfo (rt : runtime) : string =
  if rt.depth = 0 then ""
  else
    let lines = CCList.init rt.depth (fun i -> Printf.sprintf "  %d. %s" (i + 1) rt.call_names.(rt.depth - 1 - i)) in
    "\nCall stack:\n" ^ String.concat "\n" lines

(* Runs an evaluation from its entry point, adding the call stack to runtime errors *)
let runWithCallStack (rt : runtime) (sample_rate : float option) (f : unit -> 'a) : 'a =
  rt.depth <- 0 ;
  rt.sample_rate <- sample_rate ;
  try f () with Runtime_error msg -> raise (Runtime_error (msg ^ callStackInfo rt))

(* Creates an empty iprog for incremental building *)
let createEmptyProgram () : iprog =
  { ifunction_names= Map.empty
  ; compiled_functions= [||]
  ; iconstants= [||]
  ; iconstants_count= 0
  ; struct_types= Map.empty
  ; constant_names= Map.empty
  ; external_functions= Set.empty
  ; inlinable= Map.empty
  ; runtime= {depth= 0; call_names= Array.make max_call_depth ""; sample_rate= None} }

(* Adds a compiled function to the iprog, resizing the array as needed *)
let addCompiledFunction (prog : iprog) (func_idx : int) (cfunc : compiled_func) : unit =
  if func_idx >= Array.length prog.compiled_functions then (
    let new_size = max (func_idx + 1) ((Array.length prog.compiled_functions * 2) + 16) in
    let dummy = {cf_name= ""; cf_args= [||]; cf_locals= 0; cf_body= (fun _fr -> Continue)} in
    let new_array = Array.make new_size dummy in
    Array.blit prog.compiled_functions 0 new_array 0 (Array.length prog.compiled_functions) ;
    new_array.(func_idx) <- cfunc ;
    prog.compiled_functions <- new_array )
  else prog.compiled_functions.(func_idx) <- cfunc

(* Adds a constant to the iprog, resizing array as needed *)
let addConstant (prog : iprog) (value : dvalue) : unit =
  let const_idx = prog.iconstants_count in
  (* Resize constants array if needed using doubling strategy *)
  if const_idx >= Array.length prog.iconstants then (
    let new_size = if const_idx = 0 then 16 else Array.length prog.iconstants * 2 in
    let new_array = Array.make new_size DVoid in
    Array.blit prog.iconstants 0 new_array 0 prog.iconstants_count ;
    new_array.(const_idx) <- value ;
    prog.iconstants <- new_array )
  else prog.iconstants.(const_idx) <- value ;
  prog.iconstants_count <- prog.iconstants_count + 1

(* Adds a variable to the compilation context and returns its assigned index *)
let addVar (ctx : compile_ctx) (name : string) : int =
  match Map.find_opt name ctx.var_to_index with
  | Some idx ->
      idx
  | None ->
      let idx = ctx.next_index in
      ctx.var_to_index <- Map.add name idx ctx.var_to_index ;
      ctx.next_index <- ctx.next_index + 1 ;
      idx

(* Finds the index of a struct member by name within a struct descriptor *)
let getMemberIndex (struct_descr : struct_descr) (member_name : string) : int =
  let rec loop i = function
    | [] ->
        error ("Member not found: " ^ member_name ^ " in struct " ^ struct_descr.path)
    | (name, _, _, _) :: _ when name = member_name ->
        i
    | _ :: rest ->
        loop (i + 1) rest
  in
  loop 0 struct_descr.members

(* Printer functions for debugging and visualization *)

(* Converts a runtime dvalue to its string representation *)
let rec printDvalue (dv : dvalue) : string =
  match dv with
  | DVoid ->
      "void"
  | DInt i ->
      string_of_int i
  | DInt16 i ->
      string_of_int i
  | DReal f ->
      string_of_float f
  | DBool b ->
      string_of_bool b
  | DString s ->
      "\"" ^ s ^ "\""
  | DArray arr ->
      "[" ^ String.concat "; " (Array.to_list (Array.map printDvalue arr)) ^ "]"
  | DList list_ref ->
      "list[" ^ String.concat "; " (CCList.map printDvalue !list_ref) ^ "]"
  | DStruct arr ->
      "{" ^ String.concat "; " (Array.to_list (Array.mapi (fun i v -> string_of_int i ^ ":" ^ printDvalue v) arr)) ^ "}"

(* Determines if a type represents an integer value *)
let isIntType (typ : type_) : bool = match typ.t with TInt | TInt16 -> true | _ -> false

(* Determines if a type represents a real/floating-point value *)
let isRealType (typ : type_) : bool = match typ.t with TReal | TFix16 -> true | _ -> false

(* Determines if a type represents a 16-bit integer value *)
let isInt16Type (typ : type_) : bool = match typ.t with TInt16 -> true | _ -> false

(* Creates a default value for a given type *)
let rec defaultValue (typ : type_) : dvalue =
  match typ.t with
  | TVoid _ ->
      DVoid
  | TInt ->
      DInt 0
  | TInt16 ->
      DInt16 0
  | TReal ->
      DReal 0.0
  | TFix16 ->
      DReal 0.0
  | TBool ->
      DBool false
  | TString ->
      DString ""
  | TArray (Some size, elem_type) ->
      DArray (Array.init size (fun _ -> defaultValue elem_type))
  | TStruct descr ->
      DStruct (Array.of_list (CCList.map (fun (_, typ, _, _) -> defaultValue typ) descr.members))
  | TTuple types ->
      DArray (Array.of_list (CCList.map defaultValue types))
  | TEmptyType ->
      DVoid
  | TArray (None, _) ->
      error "Cannot create default value for unsized array"
  | TList _ ->
      DList (ref [])

(* Retrieves an element from an array using an index *)
let getArrayElement (arr : dvalue) (idx : dvalue) : dvalue =
  match (arr, idx) with
  | DArray elems, DInt i when i >= 0 && i < Array.length elems ->
      elems.(i)
  | DArray elems, DInt i ->
      error
        ( "getArrayElement: Invalid array access. size = "
        ^ string_of_int (Array.length elems)
        ^ " index = " ^ string_of_int i )
  | DList list_ref, DInt i -> (
    match CCList.nth_opt !list_ref i with
    | Some v ->
        v
    | None ->
        error
          ( "getArrayElement: List index out of bounds. size = "
          ^ string_of_int (CCList.length !list_ref)
          ^ " index = " ^ string_of_int i ) )
  | _ ->
      error "getArrayElement: Invalid array access. This is not an array or list"

(* Retrieves a member from a struct using a member index *)
let getStructMember (struct_val : dvalue) (member_idx : int) : dvalue =
  match struct_val with
  | DStruct members when member_idx >= 0 && member_idx < Array.length members ->
      members.(member_idx)
  | _ ->
      error "Invalid struct member access"

(* ---- Runtime Operations ----

   These functions implement the core runtime semantics: binary/unary operations.
   They are called from compiled closures for generic EOp/EUnOp nodes.
*)

(* Evaluates a binary operation on two runtime values *)
let evalBinop (op : operator) (v1 : dvalue) (v2 : dvalue) : dvalue =
  match (op, v1, v2) with
  | OpAdd, DInt a, DInt b ->
      DInt (a + b)
  | OpAdd, DInt16 a, DInt16 b ->
      DInt16 (max (-32768) (min 32767 (a + b)))
  | OpAdd, DReal a, DReal b ->
      DReal (a +. b)
  | OpAdd, DInt a, DReal b ->
      DReal (float_of_int a +. b)
  | OpAdd, DReal a, DInt b ->
      DReal (a +. float_of_int b)
  | OpAdd, DString a, DString b ->
      DString (a ^ b)
  | OpSub, DInt a, DInt b ->
      DInt (a - b)
  | OpSub, DInt16 a, DInt16 b ->
      DInt16 (max (-32768) (min 32767 (a - b)))
  | OpSub, DReal a, DReal b ->
      DReal (a -. b)
  | OpSub, DInt a, DReal b ->
      DReal (float_of_int a -. b)
  | OpSub, DReal a, DInt b ->
      DReal (a -. float_of_int b)
  | OpMul, DInt a, DInt b ->
      DInt (a * b)
  | OpMul, DInt16 a, DInt16 b ->
      DInt16 (max (-32768) (min 32767 (a * b)))
  | OpMul, DReal a, DReal b ->
      DReal (a *. b)
  | OpMul, DInt a, DReal b ->
      DReal (float_of_int a *. b)
  | OpMul, DReal a, DInt b ->
      DReal (a *. float_of_int b)
  | OpDiv, DInt a, DInt b when b <> 0 ->
      DInt (a / b)
  | OpDiv, DInt16 a, DInt16 b when b <> 0 ->
      DInt16 (max (-32768) (min 32767 (a / b)))
  | OpDiv, DReal a, DReal b when b <> 0.0 ->
      DReal (a /. b)
  | OpDiv, DInt a, DReal b when b <> 0.0 ->
      DReal (float_of_int a /. b)
  | OpDiv, DReal a, DInt b when b <> 0 ->
      DReal (a /. float_of_int b)
  | OpMod, DInt a, DInt b when b <> 0 ->
      DInt (a mod b)
  | OpMod, DInt16 a, DInt16 b when b <> 0 ->
      DInt16 (max (-32768) (min 32767 (a mod b)))
  | OpEq, DInt a, DInt b ->
      dbool (a = b)
  | OpEq, DInt16 a, DInt16 b ->
      dbool (a = b)
  | OpEq, DReal a, DReal b ->
      dbool (a = b)
  | OpEq, DBool a, DBool b ->
      dbool (a = b)
  | OpEq, DString a, DString b ->
      dbool (a = b)
  | OpNe, DInt a, DInt b ->
      dbool (a <> b)
  | OpNe, DInt16 a, DInt16 b ->
      dbool (a <> b)
  | OpNe, DReal a, DReal b ->
      dbool (a <> b)
  | OpNe, DBool a, DBool b ->
      dbool (a <> b)
  | OpNe, DString a, DString b ->
      dbool (a <> b)
  | OpLt, DInt a, DInt b ->
      dbool (a < b)
  | OpLt, DInt16 a, DInt16 b ->
      dbool (a < b)
  | OpLt, DReal a, DReal b ->
      dbool (a < b)
  | OpLe, DInt a, DInt b ->
      dbool (a <= b)
  | OpLe, DInt16 a, DInt16 b ->
      dbool (a <= b)
  | OpLe, DReal a, DReal b ->
      dbool (a <= b)
  | OpGt, DInt a, DInt b ->
      dbool (a > b)
  | OpGt, DInt16 a, DInt16 b ->
      dbool (a > b)
  | OpGt, DReal a, DReal b ->
      dbool (a > b)
  | OpGe, DInt a, DInt b ->
      dbool (a >= b)
  | OpGe, DInt16 a, DInt16 b ->
      dbool (a >= b)
  | OpGe, DReal a, DReal b ->
      dbool (a >= b)
  | OpLand, DBool a, DBool b ->
      dbool (a && b)
  | OpLor, DBool a, DBool b ->
      dbool (a || b)
  | OpBand, DInt a, DInt b ->
      DInt (a land b)
  | OpBand, DInt16 a, DInt16 b ->
      DInt16 (a land b)
  | OpBor, DInt a, DInt b ->
      DInt (a lor b)
  | OpBor, DInt16 a, DInt16 b ->
      DInt16 (a lor b)
  | OpBxor, DInt a, DInt b ->
      DInt (a lxor b)
  | OpBxor, DInt16 a, DInt16 b ->
      DInt16 (a lxor b)
  | OpLsh, DInt a, DInt b ->
      DInt (a lsl b)
  | OpLsh, DInt16 a, DInt16 b ->
      DInt16 (max (-32768) (min 32767 (a lsl b)))
  | OpRsh, DInt a, DInt b ->
      DInt (a lsr b)
  | OpRsh, DInt16 a, DInt16 b ->
      DInt16 (a lsr b)
  | OpMod, DReal a, DReal b ->
      DReal (Stdlib.mod_float a b)
  | _ ->
      let ops = Pla.print (Prog.Print.print_operator op) in
      let v1 = printDvalue v1 in
      let v2 = printDvalue v2 in
      error ("Unsupported operation: " ^ v1 ^ " " ^ ops ^ " " ^ v2)

(* Equality used to select the case of a switch *)
let valuesEqual (v1 : dvalue) (v2 : dvalue) : bool = match evalBinop OpEq v1 v2 with DBool b -> b | _ -> false

(* Evaluates a unary operation on a runtime value *)
let evalUnop (op : uoperator) (v : dvalue) : dvalue =
  match (op, v) with
  | UOpNeg, DInt i ->
      DInt (-i)
  | UOpNeg, DInt16 i ->
      DInt16 (max (-32768) (min 32767 (-i)))
  | UOpNeg, DReal f ->
      DReal (-.f)
  | UOpNot, DBool b ->
      dbool (not b)
  | UOpNot, DInt i ->
      dbool (i = 0)
  | UOpNot, DInt16 i ->
      dbool (i = 0)
  | _ ->
      error "Unsupported unary operation"

(* ---- Closure Compilation Helpers ----

   to_closure: converts compiled_result -> compiled_exp (materializes CConstant/CVar as closures)

   compile_binop_xx: create closures for binary operations on two compiled_exp values.
   compile_binop_xx_cp: constant-propagating wrappers that take compiled_result values.
   compile_unary_r_cp: same pattern for unary float operations.
*)

(* Convert a compiled_result to a compiled_exp closure. *)
let to_closure (r : compiled_result) : compiled_exp =
  match r with CConstant v -> fun _fr -> v | CVar idx -> fun fr -> Array.unsafe_get fr idx | CDynamic f -> f

(* Base closure-building helpers for binary and unary operations. *)
let[@inline always] compile_binop_ii (f1 : compiled_exp) (f2 : compiled_exp) (op : int -> int -> dvalue) (err : string)
    : compiled_exp =
 fun fr -> match (f1 fr, f2 fr) with DInt a, DInt b -> op a b | _ -> error err

let[@inline always] compile_binop_i16 (f1 : compiled_exp) (f2 : compiled_exp) (op : int -> int -> int) (err : string) :
    compiled_exp =
 fun fr ->
  match (f1 fr, f2 fr) with
  | DInt16 a, DInt16 b ->
      let result = op a b in
      DInt16 (max (-32768) (min 32767 result))
  | _ ->
      error err

let[@inline always] compile_binop_rr (f1 : compiled_exp) (f2 : compiled_exp) (op : float -> float -> dvalue)
    (err : string) : compiled_exp =
 fun fr -> match (f1 fr, f2 fr) with DReal a, DReal b -> op a b | _ -> error err

let[@inline always] compile_unary_r (f1 : compiled_exp) (op : float -> float) (err : string) : compiled_exp =
 fun fr -> match f1 fr with DReal f -> DReal (op f) | _ -> error err

(* Constant-propagating and CVar-fusing helpers. *)
let[@inline always] compile_binop_ii_cp (r1 : compiled_result) (r2 : compiled_result) (op : int -> int -> dvalue)
    (err : string) : compiled_result =
  match (r1, r2) with
  | CConstant (DInt a), CConstant (DInt b) -> (
    (* A failing operation (e.g. division by zero) is not folded: it must only raise if the
       code path actually runs *)
    try CConstant (op a b) with Division_by_zero -> CDynamic (fun _fr -> op a b) )
  | CVar idx1, CVar idx2 ->
      CDynamic
        (fun fr ->
          match (Array.unsafe_get fr idx1, Array.unsafe_get fr idx2) with DInt a, DInt b -> op a b | _ -> error err )
  | CVar idx, CConstant (DInt b) ->
      CDynamic (fun fr -> match Array.unsafe_get fr idx with DInt a -> op a b | _ -> error err)
  | CConstant (DInt a), CVar idx ->
      CDynamic (fun fr -> match Array.unsafe_get fr idx with DInt b -> op a b | _ -> error err)
  | _ ->
      CDynamic (compile_binop_ii (to_closure r1) (to_closure r2) op err)

let[@inline always] compile_binop_i16_cp (r1 : compiled_result) (r2 : compiled_result) (op : int -> int -> int)
    (err : string) : compiled_result =
  match (r1, r2) with
  | CConstant (DInt16 a), CConstant (DInt16 b) -> (
    try
      let result = op a b in
      CConstant (DInt16 (max (-32768) (min 32767 result)))
    with Division_by_zero -> CDynamic (fun _fr -> DInt16 (max (-32768) (min 32767 (op a b)))) )
  | _ ->
      CDynamic (compile_binop_i16 (to_closure r1) (to_closure r2) op err)

let[@inline always] compile_binop_rr_cp (r1 : compiled_result) (r2 : compiled_result) (op : float -> float -> dvalue)
    (err : string) : compiled_result =
  match (r1, r2) with
  | CConstant (DReal a), CConstant (DReal b) ->
      CConstant (op a b)
  | CVar idx1, CVar idx2 ->
      CDynamic
        (fun fr ->
          match (Array.unsafe_get fr idx1, Array.unsafe_get fr idx2) with DReal a, DReal b -> op a b | _ -> error err )
  | CVar idx, CConstant (DReal b) ->
      CDynamic (fun fr -> match Array.unsafe_get fr idx with DReal a -> op a b | _ -> error err)
  | CConstant (DReal a), CVar idx ->
      CDynamic (fun fr -> match Array.unsafe_get fr idx with DReal b -> op a b | _ -> error err)
  | _ ->
      CDynamic (compile_binop_rr (to_closure r1) (to_closure r2) op err)

(* Both operands are always evaluated, matching evalBinop *)
let[@inline always] compile_binop_bb_cp (r1 : compiled_result) (r2 : compiled_result) (op : bool -> bool -> bool)
    (err : string) : compiled_result =
  match (r1, r2) with
  | CConstant (DBool a), CConstant (DBool b) ->
      CConstant (dbool (op a b))
  | _ ->
      let f1 = to_closure r1 in
      let f2 = to_closure r2 in
      CDynamic (fun fr -> match (f1 fr, f2 fr) with DBool a, DBool b -> dbool (op a b) | _ -> error err)

let compile_ternop_rr_cp (r1 : compiled_result) (r2 : compiled_result) (r3 : compiled_result) (err : string) :
    compiled_result =
  match (r1, r2, r3) with
  | CConstant (DReal a), CConstant (DReal b), CConstant (DReal c) ->
      CConstant (DReal (a +. b +. c))
  | CVar idx1, CVar idx2, CVar idx3 ->
      CDynamic
        (fun fr ->
          match (Array.unsafe_get fr idx1, Array.unsafe_get fr idx2, Array.unsafe_get fr idx3) with
          | DReal a, DReal b, DReal c ->
              DReal (a +. b +. c)
          | _ ->
              error err )
  | _ ->
      let f1 = to_closure r1 in
      let f2 = to_closure r2 in
      let f3 = to_closure r3 in
      CDynamic
        (fun fr -> match (f1 fr, f2 fr, f3 fr) with DReal a, DReal b, DReal c -> DReal (a +. b +. c) | _ -> error err)

let compile_quaternop_rr_cp (r1 : compiled_result) (r2 : compiled_result) (r3 : compiled_result) (r4 : compiled_result)
    (err : string) : compiled_result =
  match (r1, r2, r3, r4) with
  | CConstant (DReal a), CConstant (DReal b), CConstant (DReal c), CConstant (DReal d) ->
      CConstant (DReal (a +. b +. c +. d))
  | CVar idx1, CVar idx2, CVar idx3, CVar idx4 ->
      CDynamic
        (fun fr ->
          match
            (Array.unsafe_get fr idx1, Array.unsafe_get fr idx2, Array.unsafe_get fr idx3, Array.unsafe_get fr idx4)
          with
          | DReal a, DReal b, DReal c, DReal d ->
              DReal (a +. b +. c +. d)
          | _ ->
              error err )
  | _ ->
      let f1 = to_closure r1 in
      let f2 = to_closure r2 in
      let f3 = to_closure r3 in
      let f4 = to_closure r4 in
      CDynamic
        (fun fr ->
          match (f1 fr, f2 fr, f3 fr, f4 fr) with
          | DReal a, DReal b, DReal c, DReal d ->
              DReal (a +. b +. c +. d)
          | _ ->
              error err )

let[@inline always] compile_unary_r_cp (r1 : compiled_result) (op : float -> float) (err : string) : compiled_result =
  match r1 with
  | CConstant (DReal f) ->
      CConstant (DReal (op f))
  | CVar idx ->
      CDynamic (fun fr -> match Array.unsafe_get fr idx with DReal f -> DReal (op f) | _ -> error err)
  | _ ->
      CDynamic (compile_unary_r (to_closure r1) op err)

(* ---- Function Frames ---- *)

(* Creates the frame holding the locals and arguments of a call to the function *)
let newFrame (cfunc : compiled_func) : dvalue array =
  (* Literal arrays are allocated inline, while Array.make goes through a C call *)
  match cfunc.cf_locals with
  | 0 ->
      [||]
  | 1 ->
      [|DVoid|]
  | 2 ->
      [|DVoid; DVoid|]
  | 3 ->
      [|DVoid; DVoid; DVoid|]
  | 4 ->
      [|DVoid; DVoid; DVoid; DVoid|]
  | 5 ->
      [|DVoid; DVoid; DVoid; DVoid; DVoid|]
  | 6 ->
      [|DVoid; DVoid; DVoid; DVoid; DVoid; DVoid|]
  | 7 ->
      [|DVoid; DVoid; DVoid; DVoid; DVoid; DVoid; DVoid|]
  | 8 ->
      [|DVoid; DVoid; DVoid; DVoid; DVoid; DVoid; DVoid; DVoid|]
  | 9 ->
      [|DVoid; DVoid; DVoid; DVoid; DVoid; DVoid; DVoid; DVoid; DVoid|]
  | 10 ->
      [|DVoid; DVoid; DVoid; DVoid; DVoid; DVoid; DVoid; DVoid; DVoid; DVoid|]
  | 11 ->
      [|DVoid; DVoid; DVoid; DVoid; DVoid; DVoid; DVoid; DVoid; DVoid; DVoid; DVoid|]
  | 12 ->
      [|DVoid; DVoid; DVoid; DVoid; DVoid; DVoid; DVoid; DVoid; DVoid; DVoid; DVoid; DVoid|]
  | n ->
      Array.make n DVoid

(* Runs the body of the function in a frame created by newFrame *)
let runFrame (cfunc : compiled_func) (fr : dvalue array) : dvalue =
  match cfunc.cf_body fr with Continue -> DVoid | Return v -> v

(* ---- Inlining Helpers at Prog.exp Level ---- *)

(* Size of a Prog expression tree *)
let rec progExpSize (exp : exp) : int =
  match exp.e with
  | EUnit | EEmptyValue | EBool _ | EInt _ | EReal _ | EFixed _ | EString _ | EId _ ->
      1
  | EUnOp (_, e1) ->
      1 + progExpSize e1
  | EOp (_, e1, e2) ->
      1 + progExpSize e1 + progExpSize e2
  | EIf {cond; then_; else_} ->
      1 + progExpSize cond + progExpSize then_ + progExpSize else_
  | EMember (e1, _) | ETMember (e1, _) ->
      1 + progExpSize e1
  | EIndex {e; index} ->
      1 + progExpSize e + progExpSize index
  | ECall {args; _} ->
      1 + CCList.fold_left (fun acc a -> acc + progExpSize a) 0 args
  | EArray elems | ETuple elems ->
      1 + CCList.fold_left (fun acc e1 -> acc + progExpSize e1) 0 elems
  | ERecord {elems; _} ->
      1 + CCList.fold_left (fun acc (_, e1) -> acc + progExpSize e1) 0 elems

(* Check if expression contains a call to a given function *)
let rec containsProgCall (func_name : string) (exp : exp) : bool =
  match exp.e with
  | ECall {path; args} ->
      String.equal path func_name || CCList.exists (containsProgCall func_name) args
  | EUnit | EEmptyValue | EBool _ | EInt _ | EReal _ | EFixed _ | EString _ | EId _ ->
      false
  | EUnOp (_, e1) | EMember (e1, _) | ETMember (e1, _) ->
      containsProgCall func_name e1
  | EOp (_, e1, e2) | EIndex {e= e1; index= e2} ->
      containsProgCall func_name e1 || containsProgCall func_name e2
  | EIf {cond; then_; else_} ->
      containsProgCall func_name cond || containsProgCall func_name then_ || containsProgCall func_name else_
  | EArray elems | ETuple elems ->
      CCList.exists (containsProgCall func_name) elems
  | ERecord {elems; _} ->
      CCList.exists (fun (_, e1) -> containsProgCall func_name e1) elems

(* Substitute EId references in a Prog expression *)
let rec substituteProgExp (subst : exp Map.t) (exp : exp) : exp =
  match exp.e with
  | EId name -> (
    match Map.find_opt name subst with Some replacement -> replacement | None -> exp )
  | EUnit | EEmptyValue | EBool _ | EInt _ | EReal _ | EFixed _ | EString _ ->
      exp
  | EUnOp (op, e1) ->
      {exp with e= EUnOp (op, substituteProgExp subst e1)}
  | EOp (op, e1, e2) ->
      {exp with e= EOp (op, substituteProgExp subst e1, substituteProgExp subst e2)}
  | EIf {cond; then_; else_} ->
      { exp with
        e=
          EIf
            { cond= substituteProgExp subst cond
            ; then_= substituteProgExp subst then_
            ; else_= substituteProgExp subst else_ } }
  | EMember (e1, name) ->
      {exp with e= EMember (substituteProgExp subst e1, name)}
  | ETMember (e1, idx) ->
      {exp with e= ETMember (substituteProgExp subst e1, idx)}
  | EIndex {e; index} ->
      {exp with e= EIndex {e= substituteProgExp subst e; index= substituteProgExp subst index}}
  | ECall {path; args} ->
      {exp with e= ECall {path; args= CCList.map (substituteProgExp subst) args}}
  | EArray elems ->
      {exp with e= EArray (CCList.map (substituteProgExp subst) elems)}
  | ETuple elems ->
      {exp with e= ETuple (CCList.map (substituteProgExp subst) elems)}
  | ERecord {path; elems} ->
      {exp with e= ERecord {path; elems= CCList.map (fun (n, e1) -> (n, substituteProgExp subst e1)) elems}}

(* Checks if a Prog expression is simple (cheap to duplicate for inlining) *)
let isSimpleProgExp (exp : exp) : bool =
  match exp.e with EId _ | EBool _ | EInt _ | EReal _ | EFixed _ | EString _ | EUnit -> true | _ -> false

(* Counts how many times EId name appears in an expression *)
let rec countProgIdUses (name : string) (exp : exp) : int =
  match exp.e with
  | EId n when String.equal n name ->
      1
  | EId _ | EUnit | EEmptyValue | EBool _ | EInt _ | EReal _ | EFixed _ | EString _ ->
      0
  | EUnOp (_, e1) | EMember (e1, _) | ETMember (e1, _) ->
      countProgIdUses name e1
  | EOp (_, e1, e2) | EIndex {e= e1; index= e2} ->
      countProgIdUses name e1 + countProgIdUses name e2
  | EIf {cond; then_; else_} ->
      countProgIdUses name cond + countProgIdUses name then_ + countProgIdUses name else_
  | ECall {args; _} ->
      CCList.fold_left (fun acc a -> acc + countProgIdUses name a) 0 args
  | EArray elems | ETuple elems ->
      CCList.fold_left (fun acc e1 -> acc + countProgIdUses name e1) 0 elems
  | ERecord {elems; _} ->
      CCList.fold_left (fun acc (_, e1) -> acc + countProgIdUses name e1) 0 elems

(* ---- Core Compilation: compileExp ----

   This function goes directly from Prog.exp to compiled_result in a single pass.
   It combines the work that was previously done by transformExp (Phase 1),
   inlineExp (Phase 2b), foldConstantsExp (Phase 2c), and compileIexp (Phase 3).
*)
let rec compileExp (ctx : compile_ctx) (exp : exp) : compiled_result =
  match exp.e with
  (* -- Literals: always CConstant -- *)
  | EUnit ->
      CConstant DVoid
  | EEmptyValue -> (
    match exp.t.t with TList _ -> CDynamic (fun _fr -> DList (ref [])) | _ -> CConstant DVoid )
  | EBool b ->
      CConstant (DBool b)
  | EInt i ->
      if isInt16Type exp.t then CConstant (DInt16 i) else CConstant (DInt i)
  | EReal f ->
      CConstant (DReal f)
  | EFixed f ->
      CConstant (DReal f)
  | EString s ->
      CConstant (DString s)
  | EId name -> (
    match Map.find_opt name ctx.var_to_index with
    | Some idx ->
        CVar idx
    | None -> (
      match Map.find_opt name ctx.constant_names with
      | Some const_idx ->
          CConstant ctx.prog.iconstants.(const_idx)
      | None ->
          error ("Variable or constant not found: " ^ name) ) )
  (* -- Specialized arithmetic operations based on types -- *)
  (* -- Fused 4-way real addition: (((a + b) + c) + d) -- *)
  | EOp
      ( OpAdd
      , {e= EOp (OpAdd, {e= EOp (OpAdd, e1, e2); t= {t= TReal | TFix16; _}; _}, e3); t= {t= TReal | TFix16; _}; _}
      , e4 ) ->
      compile_quaternop_rr_cp (compileExp ctx e1) (compileExp ctx e2) (compileExp ctx e3) (compileExp ctx e4)
        "Type mismatch in real addition"
  (* -- Fused 3-way real addition: ((a + b) + c) -- *)
  | EOp (OpAdd, {e= EOp (OpAdd, e1, e2); t= {t= TReal | TFix16; _}; _}, e3) ->
      compile_ternop_rr_cp (compileExp ctx e1) (compileExp ctx e2) (compileExp ctx e3) "Type mismatch in real addition"
  | EOp (OpAdd, e1, e2) when isInt16Type e1.t && isInt16Type e2.t ->
      compile_binop_i16_cp (compileExp ctx e1) (compileExp ctx e2) ( + ) "Type mismatch in int16 addition"
  | EOp (OpAdd, e1, e2) when isIntType e1.t && isIntType e2.t ->
      compile_binop_ii_cp (compileExp ctx e1) (compileExp ctx e2)
        (fun a b -> DInt (a + b))
        "Type mismatch in integer addition"
  | EOp (OpAdd, e1, e2) when isRealType e1.t || isRealType e2.t ->
      compile_binop_rr_cp (compileExp ctx e1) (compileExp ctx e2)
        (fun a b -> DReal (a +. b))
        "Type mismatch in real addition"
  | EOp (OpSub, e1, e2) when isInt16Type e1.t && isInt16Type e2.t ->
      compile_binop_i16_cp (compileExp ctx e1) (compileExp ctx e2) ( - ) "Type mismatch in int16 subtraction"
  | EOp (OpSub, e1, e2) when isIntType e1.t && isIntType e2.t ->
      compile_binop_ii_cp (compileExp ctx e1) (compileExp ctx e2)
        (fun a b -> DInt (a - b))
        "Type mismatch in integer subtraction"
  | EOp (OpSub, e1, e2) when isRealType e1.t || isRealType e2.t ->
      compile_binop_rr_cp (compileExp ctx e1) (compileExp ctx e2)
        (fun a b -> DReal (a -. b))
        "Type mismatch in real subtraction"
  | EOp (OpMul, e1, e2) when isInt16Type e1.t && isInt16Type e2.t ->
      compile_binop_i16_cp (compileExp ctx e1) (compileExp ctx e2) ( * ) "Type mismatch in int16 multiplication"
  | EOp (OpMul, e1, e2) when isIntType e1.t && isIntType e2.t ->
      compile_binop_ii_cp (compileExp ctx e1) (compileExp ctx e2)
        (fun a b -> DInt (a * b))
        "Type mismatch in integer multiplication"
  | EOp (OpMul, e1, e2) when isRealType e1.t || isRealType e2.t ->
      compile_binop_rr_cp (compileExp ctx e1) (compileExp ctx e2)
        (fun a b -> DReal (a *. b))
        "Type mismatch in real multiplication"
  | EOp (OpDiv, e1, e2) when isInt16Type e1.t && isInt16Type e2.t ->
      compile_binop_i16_cp (compileExp ctx e1) (compileExp ctx e2) ( / ) "Type mismatch in int16 division"
  | EOp (OpDiv, e1, e2) when isIntType e1.t && isIntType e2.t ->
      compile_binop_ii_cp (compileExp ctx e1) (compileExp ctx e2)
        (fun a b -> DInt (a / b))
        "Type mismatch in integer division"
  | EOp (OpDiv, e1, e2) when isRealType e1.t || isRealType e2.t ->
      compile_binop_rr_cp (compileExp ctx e1) (compileExp ctx e2)
        (fun a b -> DReal (a /. b))
        "Type mismatch in real division"
  (* Specialized comparisons *)
  | EOp (OpEq, e1, e2) when isInt16Type e1.t && isInt16Type e2.t ->
      compileEqInt16 ctx e1 e2
  | EOp (OpEq, e1, e2) when isIntType e1.t && isIntType e2.t ->
      compile_binop_ii_cp (compileExp ctx e1) (compileExp ctx e2)
        (fun a b -> dbool (a = b))
        "Type mismatch in integer equality"
  | EOp (OpEq, e1, e2) when isRealType e1.t || isRealType e2.t ->
      compile_binop_rr_cp (compileExp ctx e1) (compileExp ctx e2)
        (fun a b -> dbool (Float.equal a b))
        "Type mismatch in real equality"
  | EOp (OpLt, e1, e2) when isInt16Type e1.t && isInt16Type e2.t ->
      compileLtInt16 ctx e1 e2
  | EOp (OpLt, e1, e2) when isIntType e1.t && isIntType e2.t ->
      compile_binop_ii_cp (compileExp ctx e1) (compileExp ctx e2)
        (fun a b -> dbool (a < b))
        "Type mismatch in integer less than"
  | EOp (OpLt, e1, e2) when isRealType e1.t || isRealType e2.t ->
      compile_binop_rr_cp (compileExp ctx e1) (compileExp ctx e2)
        (fun a b -> dbool (a < b))
        "Type mismatch in real less than"
  | EOp (OpGt, e1, e2) when isInt16Type e1.t && isInt16Type e2.t ->
      compileGtInt16 ctx e1 e2
  | EOp (OpGt, e1, e2) when isIntType e1.t && isIntType e2.t ->
      compile_binop_ii_cp (compileExp ctx e1) (compileExp ctx e2)
        (fun a b -> dbool (a > b))
        "Type mismatch in integer greater than"
  | EOp (OpGt, e1, e2) when isRealType e1.t || isRealType e2.t ->
      compile_binop_rr_cp (compileExp ctx e1) (compileExp ctx e2)
        (fun a b -> dbool (a > b))
        "Type mismatch in real greater than"
  | EOp (OpLe, e1, e2) when isIntType e1.t && isIntType e2.t && not (isInt16Type e1.t) ->
      compile_binop_ii_cp (compileExp ctx e1) (compileExp ctx e2)
        (fun a b -> dbool (a <= b))
        "Type mismatch in integer less or equal"
  | EOp (OpLe, e1, e2) when isRealType e1.t && isRealType e2.t ->
      compile_binop_rr_cp (compileExp ctx e1) (compileExp ctx e2)
        (fun a b -> dbool (a <= b))
        "Type mismatch in real less or equal"
  | EOp (OpGe, e1, e2) when isIntType e1.t && isIntType e2.t && not (isInt16Type e1.t) ->
      compile_binop_ii_cp (compileExp ctx e1) (compileExp ctx e2)
        (fun a b -> dbool (a >= b))
        "Type mismatch in integer greater or equal"
  | EOp (OpGe, e1, e2) when isRealType e1.t && isRealType e2.t ->
      compile_binop_rr_cp (compileExp ctx e1) (compileExp ctx e2)
        (fun a b -> dbool (a >= b))
        "Type mismatch in real greater or equal"
  | EOp (OpNe, e1, e2) when isIntType e1.t && isIntType e2.t && not (isInt16Type e1.t) ->
      compile_binop_ii_cp (compileExp ctx e1) (compileExp ctx e2)
        (fun a b -> dbool (a <> b))
        "Type mismatch in integer inequality"
  | EOp (OpNe, e1, e2) when isRealType e1.t && isRealType e2.t ->
      compile_binop_rr_cp (compileExp ctx e1) (compileExp ctx e2)
        (fun a b -> dbool (a <> b))
        "Type mismatch in real inequality"
  | EOp (OpMod, e1, e2) when isIntType e1.t && isIntType e2.t && not (isInt16Type e1.t) ->
      compile_binop_ii_cp (compileExp ctx e1) (compileExp ctx e2)
        (fun a b -> DInt (a mod b))
        "Type mismatch in integer modulo"
  | EOp (OpMod, e1, e2) when isRealType e1.t && isRealType e2.t ->
      compile_binop_rr_cp (compileExp ctx e1) (compileExp ctx e2)
        (fun a b -> DReal (Stdlib.mod_float a b))
        "Type mismatch in real modulo"
  | EOp (OpBand, e1, e2) when isIntType e1.t && isIntType e2.t && not (isInt16Type e1.t) ->
      compile_binop_ii_cp (compileExp ctx e1) (compileExp ctx e2)
        (fun a b -> DInt (a land b))
        "Type mismatch in bitwise and"
  | EOp (OpBor, e1, e2) when isIntType e1.t && isIntType e2.t && not (isInt16Type e1.t) ->
      compile_binop_ii_cp (compileExp ctx e1) (compileExp ctx e2)
        (fun a b -> DInt (a lor b))
        "Type mismatch in bitwise or"
  | EOp (OpBxor, e1, e2) when isIntType e1.t && isIntType e2.t && not (isInt16Type e1.t) ->
      compile_binop_ii_cp (compileExp ctx e1) (compileExp ctx e2)
        (fun a b -> DInt (a lxor b))
        "Type mismatch in bitwise xor"
  | EOp (OpLand, e1, e2) ->
      compile_binop_bb_cp (compileExp ctx e1) (compileExp ctx e2) ( && ) "Type mismatch in logical and"
  | EOp (OpLor, e1, e2) ->
      compile_binop_bb_cp (compileExp ctx e1) (compileExp ctx e2) ( || ) "Type mismatch in logical or"
  (* Generic fallback for remaining ops *)
  | EOp (op, e1, e2) -> (
      let r1 = compileExp ctx e1 in
      let r2 = compileExp ctx e2 in
      match (r1, r2) with
      | CConstant v1, CConstant v2 -> (
        (* An operation that fails on constants (e.g. division by zero) is not folded: it must
             only raise if the code path actually runs *)
        try CConstant (evalBinop op v1 v2) with Runtime_error _ -> CDynamic (fun _fr -> evalBinop op v1 v2) )
      | _ ->
          let f1 = to_closure r1 in
          let f2 = to_closure r2 in
          CDynamic (fun fr -> evalBinop op (f1 fr) (f2 fr)) )
  | EUnOp (op, e1) -> (
      let r1 = compileExp ctx e1 in
      match r1 with
      | CConstant v ->
          CConstant (evalUnop op v)
      | _ ->
          let f1 = to_closure r1 in
          CDynamic (fun fr -> evalUnop op (f1 fr)) )
  (* -- Builtin functions recognized by name -- *)
  | ECall {path= "sin"; args= [a]} ->
      compile_unary_r_cp (compileExp ctx a) sin "Type mismatch in sin"
  | ECall {path= "cos"; args= [a]} ->
      compile_unary_r_cp (compileExp ctx a) cos "Type mismatch in cos"
  | ECall {path= "tan"; args= [a]} ->
      compile_unary_r_cp (compileExp ctx a) tan "Type mismatch in tan"
  | ECall {path= "sinh"; args= [a]} ->
      compile_unary_r_cp (compileExp ctx a) sinh "Type mismatch in sinh"
  | ECall {path= "cosh"; args= [a]} ->
      compile_unary_r_cp (compileExp ctx a) cosh "Type mismatch in cosh"
  | ECall {path= "tanh"; args= [a]} ->
      compile_unary_r_cp (compileExp ctx a) tanh "Type mismatch in tanh"
  | ECall {path= "exp"; args= [a]} ->
      compile_unary_r_cp (compileExp ctx a) Stdlib.exp "Type mismatch in exp"
  | ECall {path= "log"; args= [a]} ->
      compile_unary_r_cp (compileExp ctx a) log "Type mismatch in log"
  | ECall {path= "log10"; args= [a]} ->
      compile_unary_r_cp (compileExp ctx a) log10 "Type mismatch in log10"
  | ECall {path= "sqrt"; args= [a]} ->
      compile_unary_r_cp (compileExp ctx a) sqrt "Type mismatch in sqrt"
  | ECall {path= "floor"; args= [a]} ->
      compile_unary_r_cp (compileExp ctx a) floor "Type mismatch in floor"
  | ECall {path= "ceil"; args= [a]} ->
      compile_unary_r_cp (compileExp ctx a) ceil "Type mismatch in ceil"
  | ECall {path= "asin"; args= [a]} ->
      compile_unary_r_cp (compileExp ctx a) asin "Type mismatch in asin"
  | ECall {path= "acos"; args= [a]} ->
      compile_unary_r_cp (compileExp ctx a) acos "Type mismatch in acos"
  | ECall {path= "atan"; args= [a]} ->
      compile_unary_r_cp (compileExp ctx a) atan "Type mismatch in atan"
  | ECall {path= "atan2"; args= [a; b]} ->
      compile_binop_rr_cp (compileExp ctx a) (compileExp ctx b) (fun a b -> DReal (atan2 a b)) "Type mismatch in atan2"
  | ECall {path= "abs"; args= [a]} ->
      compileAbs ctx a
  | ECall {path= "pow"; args= [a; b]} ->
      compile_binop_rr_cp (compileExp ctx a) (compileExp ctx b) (fun a b -> DReal (a ** b)) "Type mismatch in pow"
  | ECall {path= "min"; args= [a; b]} when isRealType exp.t ->
      compile_binop_rr_cp (compileExp ctx a) (compileExp ctx b)
        (fun a b -> DReal (Float.min a b))
        "Type mismatch in min"
  | ECall {path= "min"; args= [a; b]} when isIntType exp.t ->
      compile_binop_ii_cp (compileExp ctx a) (compileExp ctx b) (fun a b -> DInt (Int.min a b)) "Type mismatch in min"
  | ECall {path= "max"; args= [a; b]} when isRealType exp.t ->
      compile_binop_rr_cp (compileExp ctx a) (compileExp ctx b)
        (fun a b -> DReal (Float.max a b))
        "Type mismatch in max"
  | ECall {path= "max"; args= [a; b]} when isIntType exp.t ->
      compile_binop_ii_cp (compileExp ctx a) (compileExp ctx b) (fun a b -> DInt (Int.max a b)) "Type mismatch in max"
  | ECall {path= "clip"; args= [x; mn; mx]} when isRealType exp.t ->
      compileClipReal ctx x mn mx
  | ECall {path= "clip"; args= [x; mn; mx]} when isIntType exp.t ->
      compileClipInt ctx x mn mx
  | ECall {path= "pi"; args= []} ->
      CConstant (DReal Float.pi)
  | ECall {path= "eps"; args= []} ->
      CConstant (DReal 1e-18)
  | ECall {path= "samplerate"; args= []} ->
      let rt = ctx.prog.runtime in
      CDynamic
        (fun _fr ->
          match rt.sample_rate with
          | Some fs ->
              DReal fs
          | None ->
              error "samplerate() requires the -samplerate flag. Use: vult file.vult -eval \"expr\" -samplerate 44100" )
  | ECall {path= "random"; args= []} ->
      CDynamic (fun _fr -> DReal (Random.float 1.0))
  | ECall {path= "irandom"; args= []} ->
      CDynamic (fun _fr -> DInt (Random.int Int.max_int))
  (* Type conversion functions *)
  | ECall {path= "real"; args= [a]} ->
      compileRealConversion ctx a
  | ECall {path= "int"; args= [a]} ->
      compileIntConversion ctx a
  | ECall {path= "int16"; args= [a]} ->
      compileInt16Conversion ctx a
  | ECall {path= "bool"; args= [a]} ->
      compileBoolConversion ctx a
  | ECall {path= "string"; args= [a]} ->
      compileStringConversion ctx a
  | ECall {path= "fix16"; args= [a]} ->
      compileFixedConversion ctx a
  (* Array/string functions *)
  | ECall {path= "size"; args= [a]} ->
      compileSizeBuiltin ctx a
  | ECall {path= "length"; args= [a]} ->
      compileLengthBuiltin ctx a
  (* List functions *)
  | ECall {path= "list_size"; args= [a]} ->
      compileListSizeBuiltin ctx a
  | ECall {path= "list_capacity"; args= [a]} ->
      compileListCapacityBuiltin ctx a
  | ECall {path= "list_append"; args= [l; v]} ->
      compileListAppend ctx l v
  | ECall {path= "list_insert"; args= [l; i; v]} ->
      compileListInsert ctx l i v
  | ECall {path= "list_remove"; args= [l; i]} ->
      compileListRemove ctx l i
  | ECall {path= "list_clear"; args= [a]} ->
      compileListClear ctx a
  | ECall {path= "list_reserve"; args= [_; _]} ->
      CConstant DVoid
  | ECall {path= "list_get"; args= [l; i]} ->
      compileListGet ctx l i
  | ECall {path= "list_set"; args= [l; i; v]} ->
      compileListSet ctx l i v
  (* External runtime functions *)
  | ECall
      { path=
          ( "push_block_header"
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
          | "next_object" ) as name
      ; args= _ } ->
      CDynamic (fun _fr -> error ("External evaluations are not possible: " ^ name))
  (* User function calls with inlining *)
  | ECall {path; args} ->
      compileCall ctx path args
  (* -- Conditional with dead branch elimination -- *)
  | EIf {cond; then_; else_} -> (
      let rc = compileExp ctx cond in
      match rc with
      | CConstant (DBool true) ->
          compileExp ctx then_
      | CConstant (DBool false) ->
          compileExp ctx else_
      | _ ->
          let fc = to_closure rc in
          let ft = to_closure (compileExp ctx then_) in
          let ff = to_closure (compileExp ctx else_) in
          CDynamic
            (fun fr -> match fc fr with DBool true -> ft fr | DBool false -> ff fr | _ -> error "Invalid condition") )
  (* -- Member access with var.field fusion -- *)
  | EMember (e, member_name) -> (
    match e.t.t with
    | TStruct descr -> (
        let member_idx = getMemberIndex descr member_name in
        let re = compileExp ctx e in
        match re with
        | CVar var_idx ->
            CDynamic
              (fun fr ->
                match Array.unsafe_get fr var_idx with
                | DStruct members ->
                    Array.unsafe_get members member_idx
                | _ ->
                    error "Invalid struct access" )
        | CConstant (DStruct members) when member_idx >= 0 && member_idx < Array.length members ->
            CConstant members.(member_idx)
        | _ ->
            let fe = to_closure re in
            CDynamic (fun fr -> getStructMember (fe fr) member_idx) )
    | _ ->
        error "Member access on non-struct type" )
  | ETMember (e, idx) -> (
      let re = compileExp ctx e in
      match re with
      | CVar var_idx ->
          CDynamic
            (fun fr ->
              match Array.unsafe_get fr var_idx with
              | DStruct members ->
                  Array.unsafe_get members idx
              | DArray arr ->
                  Array.unsafe_get arr idx
              | _ ->
                  error "Invalid tuple/struct member access" )
      | CConstant (DStruct members) when idx >= 0 && idx < Array.length members ->
          CConstant members.(idx)
      | CConstant (DArray arr) when idx >= 0 && idx < Array.length arr ->
          CConstant arr.(idx)
      | _ ->
          let fe = to_closure re in
          CDynamic (fun fr -> getStructMember (fe fr) idx) )
  (* -- Array indexing -- *)
  | EIndex {e; index} -> (
      let re = compileExp ctx e in
      let ri = compileExp ctx index in
      match (re, ri) with
      | CConstant (DArray arr), CConstant (DInt i) when i >= 0 && i < Array.length arr ->
          CConstant arr.(i)
      | _ ->
          let fe = to_closure re in
          let fi = to_closure ri in
          CDynamic (fun fr -> getArrayElement (fe fr) (fi fr)) )
  (* -- Array literal -- *)
  | EArray elems ->
      let relems = CCList.map (compileExp ctx) elems in
      if CCList.for_all (fun r -> match r with CConstant _ -> true | CVar _ | CDynamic _ -> false) relems then
        CConstant
          (DArray
             (Array.of_list
                (CCList.map (fun r -> match r with CConstant v -> v | CVar _ | CDynamic _ -> DVoid) relems) ) )
      else
        let felems = CCList.map to_closure relems in
        CDynamic (fun fr -> DArray (Array.of_list (CCList.map (fun f -> f fr) felems)))
  (* -- Tuple -- *)
  | ETuple elems ->
      let relems = CCList.map (compileExp ctx) elems in
      if CCList.for_all (fun r -> match r with CConstant _ -> true | CVar _ | CDynamic _ -> false) relems then
        CConstant
          (DArray
             (Array.of_list
                (CCList.map (fun r -> match r with CConstant v -> v | CVar _ | CDynamic _ -> DVoid) relems) ) )
      else
        let felems = CCList.map to_closure relems in
        CDynamic (fun fr -> DArray (Array.of_list (CCList.map (fun f -> f fr) felems)))
  (* -- Record literal -- *)
  | ERecord {path; elems} -> (
    match Map.find_opt path ctx.struct_types with
    | Some descr ->
        let n_members = CCList.length descr.members in
        let relems = CCList.map (fun (name, e) -> (getMemberIndex descr name, compileExp ctx e)) elems in
        if CCList.for_all (fun (_, r) -> match r with CConstant _ -> true | CVar _ | CDynamic _ -> false) relems then (
          let member_vals = Array.make n_members DVoid in
          CCList.iter
            (fun (idx, r) -> match r with CConstant v -> member_vals.(idx) <- v | CVar _ | CDynamic _ -> ())
            relems ;
          CConstant (DStruct member_vals) )
        else
          let felems = CCList.map (fun (idx, r) -> (idx, to_closure r)) relems in
          CDynamic
            (fun fr ->
              let member_vals = Array.make n_members DVoid in
              CCList.iter (fun (idx, f) -> member_vals.(idx) <- f fr) felems ;
              DStruct member_vals )
    | None ->
        error ("Unknown struct type: " ^ path) )

(* Int16 comparison helpers *)
and compileEqInt16 (ctx : compile_ctx) (e1 : exp) (e2 : exp) : compiled_result =
  let r1 = compileExp ctx e1 in
  let r2 = compileExp ctx e2 in
  match (r1, r2) with
  | CConstant (DInt16 a), CConstant (DInt16 b) ->
      CConstant (dbool (a = b))
  | _ ->
      let f1 = to_closure r1 in
      let f2 = to_closure r2 in
      CDynamic
        (fun fr ->
          match (f1 fr, f2 fr) with DInt16 a, DInt16 b -> dbool (a = b) | _ -> error "Type mismatch in int16 equality" )

and compileLtInt16 (ctx : compile_ctx) (e1 : exp) (e2 : exp) : compiled_result =
  let r1 = compileExp ctx e1 in
  let r2 = compileExp ctx e2 in
  match (r1, r2) with
  | CConstant (DInt16 a), CConstant (DInt16 b) ->
      CConstant (dbool (a < b))
  | _ ->
      let f1 = to_closure r1 in
      let f2 = to_closure r2 in
      CDynamic
        (fun fr ->
          match (f1 fr, f2 fr) with
          | DInt16 a, DInt16 b ->
              dbool (a < b)
          | _ ->
              error "Type mismatch in int16 less than" )

and compileGtInt16 (ctx : compile_ctx) (e1 : exp) (e2 : exp) : compiled_result =
  let r1 = compileExp ctx e1 in
  let r2 = compileExp ctx e2 in
  match (r1, r2) with
  | CConstant (DInt16 a), CConstant (DInt16 b) ->
      CConstant (dbool (a > b))
  | _ ->
      let f1 = to_closure r1 in
      let f2 = to_closure r2 in
      CDynamic
        (fun fr ->
          match (f1 fr, f2 fr) with
          | DInt16 a, DInt16 b ->
              dbool (a > b)
          | _ ->
              error "Type mismatch in int16 greater than" )

(* abs() handles both int and real *)
and compileAbs (ctx : compile_ctx) (a : exp) : compiled_result =
  let r1 = compileExp ctx a in
  match r1 with
  | CConstant (DReal f) ->
      CConstant (DReal (abs_float f))
  | CConstant (DInt i) ->
      CConstant (DInt (abs i))
  | CVar idx ->
      CDynamic
        (fun fr ->
          match Array.unsafe_get fr idx with
          | DReal f ->
              DReal (abs_float f)
          | DInt i ->
              DInt (abs i)
          | _ ->
              error "Type mismatch in abs" )
  | _ ->
      let f1 = to_closure r1 in
      CDynamic
        (fun fr ->
          match f1 fr with DReal f -> DReal (abs_float f) | DInt i -> DInt (abs i) | _ -> error "Type mismatch in abs" )

and compileClipReal (ctx : compile_ctx) (x : exp) (min_v : exp) (max_v : exp) : compiled_result =
  let rx = compileExp ctx x in
  let rmin = compileExp ctx min_v in
  let rmax = compileExp ctx max_v in
  match (rx, rmin, rmax) with
  | CConstant (DReal xv), CConstant (DReal minv), CConstant (DReal maxv) ->
      CConstant (DReal (Stdlib.min (Stdlib.max xv minv) maxv))
  | CVar idx, CConstant (DReal minv), CConstant (DReal maxv) ->
      CDynamic
        (fun fr ->
          match Array.unsafe_get fr idx with
          | DReal xv ->
              DReal (Stdlib.min (Stdlib.max xv minv) maxv)
          | _ ->
              error "Type mismatch in clip_real" )
  | _ ->
      let fx = to_closure rx in
      let fmin = to_closure rmin in
      let fmax = to_closure rmax in
      CDynamic
        (fun fr ->
          match (fx fr, fmin fr, fmax fr) with
          | DReal xv, DReal minv, DReal maxv ->
              DReal (Stdlib.min (Stdlib.max xv minv) maxv)
          | _ ->
              error "Type mismatch in clip_real" )

and compileClipInt (ctx : compile_ctx) (x : exp) (min_v : exp) (max_v : exp) : compiled_result =
  let rx = compileExp ctx x in
  let rmin = compileExp ctx min_v in
  let rmax = compileExp ctx max_v in
  match (rx, rmin, rmax) with
  | CConstant (DInt xv), CConstant (DInt minv), CConstant (DInt maxv) ->
      CConstant (DInt (Stdlib.min (Stdlib.max xv minv) maxv))
  | CVar idx, CConstant (DInt minv), CConstant (DInt maxv) ->
      CDynamic
        (fun fr ->
          match Array.unsafe_get fr idx with
          | DInt xv ->
              DInt (Stdlib.min (Stdlib.max xv minv) maxv)
          | _ ->
              error "Type mismatch in clip_int" )
  | _ ->
      let fx = to_closure rx in
      let fmin = to_closure rmin in
      let fmax = to_closure rmax in
      CDynamic
        (fun fr ->
          match (fx fr, fmin fr, fmax fr) with
          | DInt xv, DInt minv, DInt maxv ->
              DInt (Stdlib.min (Stdlib.max xv minv) maxv)
          | _ ->
              error "Type mismatch in clip_int" )

and compileRealConversion (ctx : compile_ctx) (a : exp) : compiled_result =
  let r1 = compileExp ctx a in
  match r1 with
  | CConstant (DInt i) ->
      CConstant (DReal (float_of_int i))
  | CConstant (DInt16 i) ->
      CConstant (DReal (float_of_int i))
  | CConstant (DBool b) ->
      CConstant (DReal (if b then 1.0 else 0.0))
  | CConstant (DReal f) ->
      CConstant (DReal f)
  | CVar idx ->
      CDynamic
        (fun fr ->
          match Array.unsafe_get fr idx with
          | DInt i ->
              DReal (float_of_int i)
          | DInt16 i ->
              DReal (float_of_int i)
          | DBool b ->
              DReal (if b then 1.0 else 0.0)
          | DReal f ->
              DReal f
          | _ ->
              error "Type mismatch in real conversion" )
  | _ ->
      let f1 = to_closure r1 in
      CDynamic
        (fun fr ->
          match f1 fr with
          | DInt i ->
              DReal (float_of_int i)
          | DInt16 i ->
              DReal (float_of_int i)
          | DBool b ->
              DReal (if b then 1.0 else 0.0)
          | DReal f ->
              DReal f
          | _ ->
              error "Type mismatch in real conversion" )

and compileIntConversion (ctx : compile_ctx) (a : exp) : compiled_result =
  let r1 = compileExp ctx a in
  match r1 with
  | CConstant (DReal f) ->
      CConstant (DInt (int_of_float f))
  | CConstant (DBool b) ->
      CConstant (DInt (if b then 1 else 0))
  | CConstant (DInt i) ->
      CConstant (DInt i)
  | CConstant (DInt16 i) ->
      CConstant (DInt i)
  | CVar idx ->
      CDynamic
        (fun fr ->
          match Array.unsafe_get fr idx with
          | DReal f ->
              DInt (int_of_float f)
          | DBool b ->
              DInt (if b then 1 else 0)
          | DInt i ->
              DInt i
          | DInt16 i ->
              DInt i
          | _ ->
              error "Type mismatch in int conversion" )
  | _ ->
      let f1 = to_closure r1 in
      CDynamic
        (fun fr ->
          match f1 fr with
          | DReal f ->
              DInt (int_of_float f)
          | DBool b ->
              DInt (if b then 1 else 0)
          | DInt i ->
              DInt i
          | DInt16 i ->
              DInt i
          | _ ->
              error "Type mismatch in int conversion" )

and compileInt16Conversion (ctx : compile_ctx) (a : exp) : compiled_result =
  let r1 = compileExp ctx a in
  match r1 with
  | CConstant (DReal f) ->
      CConstant (DInt16 (max (-32768) (min 32767 (int_of_float f))))
  | CConstant (DBool b) ->
      CConstant (DInt16 (if b then 1 else 0))
  | CConstant (DInt i) ->
      CConstant (DInt16 (max (-32768) (min 32767 i)))
  | CConstant (DInt16 i) ->
      CConstant (DInt16 i)
  | _ ->
      let f1 = to_closure r1 in
      CDynamic
        (fun fr ->
          match f1 fr with
          | DReal f ->
              DInt16 (max (-32768) (min 32767 (int_of_float f)))
          | DBool b ->
              DInt16 (if b then 1 else 0)
          | DInt i ->
              DInt16 (max (-32768) (min 32767 i))
          | DInt16 i ->
              DInt16 i
          | _ ->
              error "Type mismatch in int16 conversion" )

and compileBoolConversion (ctx : compile_ctx) (a : exp) : compiled_result =
  let r1 = compileExp ctx a in
  match r1 with
  | CConstant (DInt i) ->
      CConstant (dbool (i <> 0))
  | CConstant (DInt16 i) ->
      CConstant (dbool (i <> 0))
  | CConstant (DReal f) ->
      CConstant (dbool (f <> 0.0))
  | CConstant (DBool b) ->
      CConstant (DBool b)
  | _ ->
      let f1 = to_closure r1 in
      CDynamic
        (fun fr ->
          match f1 fr with
          | DInt i ->
              dbool (i <> 0)
          | DInt16 i ->
              dbool (i <> 0)
          | DReal f ->
              dbool (f <> 0.0)
          | DBool b ->
              DBool b
          | _ ->
              error "Type mismatch in bool conversion" )

and compileStringConversion (ctx : compile_ctx) (a : exp) : compiled_result =
  let r1 = compileExp ctx a in
  match r1 with
  | CConstant (DInt i) ->
      CConstant (DString (string_of_int i))
  | CConstant (DInt16 i) ->
      CConstant (DString (string_of_int i))
  | CConstant (DReal f) ->
      CConstant (DString (string_of_float f))
  | CConstant (DBool b) ->
      CConstant (DString (string_of_bool b))
  | CConstant (DString s) ->
      CConstant (DString s)
  | _ ->
      let f1 = to_closure r1 in
      CDynamic
        (fun fr ->
          match f1 fr with
          | DInt i ->
              DString (string_of_int i)
          | DInt16 i ->
              DString (string_of_int i)
          | DReal f ->
              DString (string_of_float f)
          | DBool b ->
              DString (string_of_bool b)
          | DString s ->
              DString s
          | _ ->
              error "Type mismatch in string conversion" )

and compileFixedConversion (ctx : compile_ctx) (a : exp) : compiled_result =
  let r1 = compileExp ctx a in
  match r1 with
  | CConstant (DReal f) ->
      CConstant (DReal f)
  | CConstant (DInt i) ->
      CConstant (DReal (float_of_int i))
  | _ ->
      let f1 = to_closure r1 in
      CDynamic
        (fun fr ->
          match f1 fr with
          | DReal f ->
              DReal f
          | DInt i ->
              DReal (float_of_int i)
          | _ ->
              error "Type mismatch in fixed conversion" )

and compileSizeBuiltin (ctx : compile_ctx) (a : exp) : compiled_result =
  let r1 = compileExp ctx a in
  match r1 with
  | CConstant (DArray arr) ->
      CConstant (DInt (Array.length arr))
  | _ ->
      let f1 = to_closure r1 in
      CDynamic (fun fr -> match f1 fr with DArray arr -> DInt (Array.length arr) | _ -> error "Type mismatch in size")

and compileLengthBuiltin (ctx : compile_ctx) (a : exp) : compiled_result =
  let r1 = compileExp ctx a in
  match r1 with
  | CConstant (DString s) ->
      CConstant (DInt (String.length s))
  | _ ->
      let f1 = to_closure r1 in
      CDynamic (fun fr -> match f1 fr with DString s -> DInt (String.length s) | _ -> error "Type mismatch in length")

and compileListSizeBuiltin (ctx : compile_ctx) (a : exp) : compiled_result =
  let f1 = to_closure (compileExp ctx a) in
  CDynamic
    (fun fr ->
      match f1 fr with
      | DArray arr ->
          DInt (Array.length arr)
      | DList list_ref ->
          DInt (CCList.length !list_ref)
      | _ ->
          error "Type mismatch in list_size" )

and compileListCapacityBuiltin (ctx : compile_ctx) (a : exp) : compiled_result =
  let f1 = to_closure (compileExp ctx a) in
  CDynamic
    (fun fr ->
      match f1 fr with
      | DArray arr ->
          DInt (Array.length arr)
      | DList list_ref ->
          DInt (CCList.length !list_ref)
      | _ ->
          error "Type mismatch in list_capacity" )

and compileListAppend (ctx : compile_ctx) (l : exp) (v : exp) : compiled_result =
  let fl = to_closure (compileExp ctx l) in
  let fv = to_closure (compileExp ctx v) in
  CDynamic
    (fun fr ->
      match fl fr with
      | DList list_ref ->
          list_ref := !list_ref @ [fv fr] ;
          DVoid
      | _ ->
          error "list_append requires list type" )

and compileListInsert (ctx : compile_ctx) (l : exp) (i : exp) (v : exp) : compiled_result =
  let fl = to_closure (compileExp ctx l) in
  let fi = to_closure (compileExp ctx i) in
  let fv = to_closure (compileExp ctx v) in
  CDynamic
    (fun fr ->
      match (fl fr, fi fr) with
      | DList list_ref, DInt index ->
          let before, after = CCList.take_drop index !list_ref in
          list_ref := before @ [fv fr] @ after ;
          DVoid
      | _ ->
          error "list_insert: invalid arguments" )

and compileListRemove (ctx : compile_ctx) (l : exp) (i : exp) : compiled_result =
  let fl = to_closure (compileExp ctx l) in
  let fi = to_closure (compileExp ctx i) in
  CDynamic
    (fun fr ->
      match (fl fr, fi fr) with
      | DList list_ref, DInt index ->
          let before, after = CCList.take_drop index !list_ref in
          list_ref := before @ CCList.drop 1 after ;
          DVoid
      | _ ->
          error "list_remove: invalid arguments" )

and compileListClear (ctx : compile_ctx) (a : exp) : compiled_result =
  let fl = to_closure (compileExp ctx a) in
  CDynamic
    (fun fr ->
      match fl fr with
      | DList list_ref ->
          list_ref := [] ;
          DVoid
      | _ ->
          error "list_clear requires list type" )

and compileListGet (ctx : compile_ctx) (l : exp) (i : exp) : compiled_result =
  let fl = to_closure (compileExp ctx l) in
  let fi = to_closure (compileExp ctx i) in
  CDynamic
    (fun fr ->
      match (fl fr, fi fr) with
      | DList list_ref, DInt index -> (
        match CCList.nth_opt !list_ref index with Some v -> v | None -> error "list_get: index out of bounds" )
      | _ ->
          error "list_get: invalid arguments" )

and compileListSet (ctx : compile_ctx) (l : exp) (i : exp) (v : exp) : compiled_result =
  let fl = to_closure (compileExp ctx l) in
  let fi = to_closure (compileExp ctx i) in
  let fv = to_closure (compileExp ctx v) in
  CDynamic
    (fun fr ->
      match (fl fr, fi fr) with
      | DList list_ref, DInt index ->
          let old_list = !list_ref in
          let len = CCList.length old_list in
          if index >= 0 && index < len then
            let before, after = CCList.take_drop index old_list in
            match after with
            | _ :: rest ->
                list_ref := before @ [fv fr] @ rest ;
                DVoid
            | [] ->
                error "list_set: index out of bounds"
          else error "list_set: index out of bounds"
      | _ ->
          error "list_set: invalid arguments" )

(* Function call compilation with inlining support *)
and compileCall (ctx : compile_ctx) (path : string) (args : exp list) : compiled_result =
  (* Check for inlining *)
  match Map.find_opt path ctx.prog.inlinable with
  | Some (param_names, body_exp) when shouldInline param_names body_exp args ->
      let subst = List.fold_left2 (fun m name arg -> Map.add name arg m) Map.empty param_names args in
      compileExp ctx (substituteProgExp subst body_exp)
  | _ -> (
    match Map.find_opt path ctx.function_names with
    | Some func_idx ->
        let fargs = Array.of_list (CCList.map (fun a -> to_closure (compileExp ctx a)) args) in
        let n_args = Array.length fargs in
        let prog = ctx.prog in
        let rt = prog.runtime in
        CDynamic
          (fun fr ->
            let cfunc =
              if func_idx < Array.length prog.compiled_functions then Array.unsafe_get prog.compiled_functions func_idx
              else error ("Internal error: the function '" ^ path ^ "' has not been compiled")
            in
            let () =
              if String.length cfunc.cf_name = 0 then
                error ("Internal error: the function '" ^ path ^ "' has not been compiled")
            in
            let callee_fr = newFrame cfunc in
            for i = 0 to n_args - 1 do
              let value = (Array.unsafe_get fargs i) fr in
              Array.unsafe_set callee_fr cfunc.cf_args.(i) value
            done ;
            let depth = rt.depth in
            if depth >= max_call_depth then error "Maximum call depth exceeded" ;
            Array.unsafe_set rt.call_names depth cfunc.cf_name ;
            rt.depth <- depth + 1 ;
            let result = runFrame cfunc callee_fr in
            rt.depth <- depth ;
            result )
    | None ->
        if Set.mem path ctx.external_functions then CDynamic (fun _fr -> error "External evaluations are not possible")
        else (
          Printf.eprintf "Function not found during compilation: %s\n" path ;
          Printf.eprintf "Available regular functions:\n" ;
          Map.iter (fun name idx -> Printf.eprintf "  %s -> %d\n" name idx) ctx.function_names ;
          Printf.eprintf "Available external functions:\n" ;
          Set.iter (fun name -> Printf.eprintf "  %s (external)\n" name) ctx.external_functions ;
          error ("Function not found during compilation: " ^ path) ) )

(* Checks if a function call should be inlined *)
and shouldInline (param_names : string list) (body_exp : exp) (args : exp list) : bool =
  progExpSize body_exp <= 30
  && (not (CCList.exists (fun pn -> containsProgCall pn body_exp) param_names))
  &&
  (* For params used >1 time, the arg must be simple *)
  let pairs = try List.combine param_names args with Invalid_argument _ -> [] in
  CCList.for_all
    (fun (pn, arg) ->
      let uses = countProgIdUses pn body_exp in
      uses <= 1 || isSimpleProgExp arg )
    pairs

(* ---- Statement Compilation ---- *)

and compileStmt (ctx : compile_ctx) (stmt : stmt) : compiled_stmt =
  match stmt.s with
  | StmtDecl (dexp, init_exp) -> (
    match dexp.d with
    | DId (name, _) -> (
        let var_idx = addVar ctx name in
        match init_exp with
        | Some exp ->
            let fexp = to_closure (compileExp ctx exp) in
            fun fr ->
              Array.unsafe_set fr var_idx (fexp fr) ;
              Continue
        | None -> (
            let typ = dexp.t in
            match typ.t with
            | TArray _ | TStruct _ | TTuple _ | TList _ ->
                (* Mutable aggregates need a fresh value on every execution *)
                fun fr ->
                  Array.unsafe_set fr var_idx (defaultValue typ) ;
                  Continue
            | TVoid _ | TInt | TInt16 | TReal | TString | TBool | TFix16 | TEmptyType ->
                let value = defaultValue typ in
                fun fr -> Array.unsafe_set fr var_idx value ; Continue ) ) )
  | StmtBind (lexp, exp) ->
      compileBind ctx lexp exp
  | StmtReturn exp ->
      let fexp = to_closure (compileExp ctx exp) in
      fun fr -> Return (fexp fr)
  | StmtBlock stmts ->
      let fstmts = Array.of_list (CCList.map (compileStmt ctx) stmts) in
      let n = Array.length fstmts in
      fun fr ->
        let rec loop (i : int) : exec_result =
          if i >= n then Continue
          else match (Array.unsafe_get fstmts i) fr with Continue -> loop (i + 1) | Return v -> Return v
        in
        loop 0
  | StmtIf (cond, then_stmt, else_stmt) -> (
      let rcond = compileExp ctx cond in
      match rcond with
      | CConstant (DBool true) ->
          compileStmt ctx then_stmt
      | CConstant (DBool false) -> (
        match else_stmt with Some else_s -> compileStmt ctx else_s | None -> fun _fr -> Continue )
      | _ -> (
          let fcond = to_closure rcond in
          let fthen = compileStmt ctx then_stmt in
          match else_stmt with
          | Some else_s -> (
              let felse = compileStmt ctx else_s in
              fun fr ->
                match fcond fr with DBool true -> fthen fr | DBool false -> felse fr | _ -> error "Invalid condition" )
          | None -> (
              fun fr -> match fcond fr with DBool true -> fthen fr | _ -> Continue ) ) )
  | StmtWhile (cond, body) ->
      let fcond = to_closure (compileExp ctx cond) in
      let fbody = compileStmt ctx body in
      fun fr ->
        let rec loop () : exec_result =
          match fcond fr with
          | DBool true -> (
            match fbody fr with Continue -> loop () | Return v -> Return v )
          | _ ->
              Continue
        in
        loop ()
  | StmtSwitch (exp, cases, default) ->
      let fexp = to_closure (compileExp ctx exp) in
      let fcases =
        Array.of_list
          (CCList.map
             (fun (case_exp, case_stmt) -> (to_closure (compileExp ctx case_exp), compileStmt ctx case_stmt))
             cases )
      in
      let n_cases = Array.length fcases in
      let fdefault = Option.map (compileStmt ctx) default in
      fun fr ->
        let exp_val = fexp fr in
        let rec try_cases (i : int) : exec_result =
          if i >= n_cases then match fdefault with Some fd -> fd fr | None -> Continue
          else
            let case_exp, case_stmt = Array.unsafe_get fcases i in
            let case_val = case_exp fr in
            if valuesEqual exp_val case_val then case_stmt fr else try_cases (i + 1)
        in
        try_cases 0

(* ---- Bind Compilation ----
   Handles StmtBind with pattern matching for efficient common cases. *)
and compileBind (ctx : compile_ctx) (lexp : lexp) (rhs : exp) : compiled_stmt =
  match lexp.l with
  | LWild ->
      let fexp = to_closure (compileExp ctx rhs) in
      fun fr ->
        let _ = fexp fr in
        Continue
  | LId name -> (
    match Map.find_opt name ctx.var_to_index with
    | Some idx ->
        let fexp = to_closure (compileExp ctx rhs) in
        fun fr ->
          Array.unsafe_set fr idx (fexp fr) ;
          Continue
    | None ->
        error ("Variable not found: " ^ name) )
  (* Fused var.member assignment *)
  | LMember ({l= LId var_name; t= var_type; _}, member_name) -> (
    match var_type.t with
    | TStruct descr -> (
        let member_idx = getMemberIndex descr member_name in
        match Map.find_opt var_name ctx.var_to_index with
        | Some var_idx ->
            let fexp = to_closure (compileExp ctx rhs) in
            fun fr ->
              let val_ = fexp fr in
              ( match Array.unsafe_get fr var_idx with
              | DStruct members ->
                  Array.unsafe_set members member_idx val_
              | _ ->
                  error "Invalid struct access in fused var-member assignment" ) ;
              Continue
        | None ->
            error ("Variable not found: " ^ var_name) )
    | _ ->
        (* Fall through to general case *)
        compileBindGeneral ctx lexp rhs )
  (* Fused var[index] assignment *)
  | LIndex {e= {l= LId var_name; _}; index= index_exp} -> (
    match Map.find_opt var_name ctx.var_to_index with
    | Some var_idx ->
        let fi = to_closure (compileExp ctx index_exp) in
        let fexp = to_closure (compileExp ctx rhs) in
        fun fr ->
          let val_ = fexp fr in
          ( match (Array.unsafe_get fr var_idx, fi fr) with
          | DArray arr, DInt i when i >= 0 && i < Array.length arr ->
              arr.(i) <- val_
          | _ ->
              error "Invalid array assignment" ) ;
          Continue
    | None ->
        error ("Variable not found: " ^ var_name) )
  (* Fused var.member[index] assignment *)
  | LIndex {e= {l= LMember ({l= LId var_name; t= var_type; _}, member_name); _}; index= index_exp} -> (
    match var_type.t with
    | TStruct descr -> (
        let member_idx = getMemberIndex descr member_name in
        match Map.find_opt var_name ctx.var_to_index with
        | Some var_idx ->
            let fi = to_closure (compileExp ctx index_exp) in
            let fexp = to_closure (compileExp ctx rhs) in
            fun fr ->
              let val_ = fexp fr in
              let arr =
                match Array.unsafe_get fr var_idx with
                | DStruct members ->
                    Array.unsafe_get members member_idx
                | _ ->
                    error "Invalid struct access"
              in
              ( match (arr, fi fr) with
              | DArray a, DInt i when i >= 0 && i < Array.length a ->
                  a.(i) <- val_
              | _ ->
                  error "Invalid array assignment" ) ;
              Continue
        | None ->
            error ("Variable not found: " ^ var_name) )
    | _ ->
        compileBindGeneral ctx lexp rhs )
  | _ ->
      compileBindGeneral ctx lexp rhs

(* General fallback for bind compilation using compileLexpAssign *)
and compileBindGeneral (ctx : compile_ctx) (lexp : lexp) (rhs : exp) : compiled_stmt =
  let fexp = to_closure (compileExp ctx rhs) in
  let fassign = compileLexpAssign ctx lexp in
  fun fr ->
    fassign fr (fexp fr) ;
    Continue

(* Compile an lexp for rvalue reading *)
and compileLexpAsRvalue (ctx : compile_ctx) (lexp : lexp) : compiled_exp =
  match lexp.l with
  | LWild ->
      fun _fr -> error "Cannot read wildcard"
  | LId name -> (
    match Map.find_opt name ctx.var_to_index with
    | Some idx ->
        fun fr -> Array.unsafe_get fr idx
    | None ->
        error ("Variable not found: " ^ name) )
  | LMember (({t= var_type; _} as e), member_name) -> (
    match var_type.t with
    | TStruct descr -> (
        let member_idx = getMemberIndex descr member_name in
        let fe = compileLexpAsRvalue ctx e in
        fun fr ->
          match fe fr with
          | DStruct members when member_idx >= 0 && member_idx < Array.length members ->
              members.(member_idx)
          | _ ->
              error "Invalid struct member access" )
    | _ ->
        error "Member access on non-struct type" )
  | LIndex {e; index} -> (
      let fe = compileLexpAsRvalue ctx e in
      let fi = to_closure (compileExp ctx index) in
      fun fr ->
        match (fe fr, fi fr) with
        | DArray arr, DInt i when i >= 0 && i < Array.length arr ->
            arr.(i)
        | _ ->
            error "Invalid array access" )
  | LTuple _ ->
      fun _fr -> error "Cannot read tuple lvalue"

(* Compile an assignment to an lvalue location *)
and compileLexpAssign (ctx : compile_ctx) (lexp : lexp) : dvalue array -> dvalue -> unit =
  match lexp.l with
  | LWild ->
      fun _fr _val -> ()
  | LId name -> (
    match Map.find_opt name ctx.var_to_index with
    | Some idx ->
        fun fr val_ -> Array.unsafe_set fr idx val_
    | None ->
        error ("Variable not found: " ^ name) )
  | LMember (({t= var_type; _} as e), member_name) -> (
    match var_type.t with
    | TStruct descr -> (
        let member_idx = getMemberIndex descr member_name in
        let fe = compileLexpAsRvalue ctx e in
        fun fr val_ ->
          match fe fr with
          | DStruct members when member_idx >= 0 && member_idx < Array.length members ->
              members.(member_idx) <- val_
          | _ ->
              error "Invalid struct member assignment" )
    | _ ->
        error "Member assignment on non-struct type" )
  | LIndex {e; index} -> (
      let fe = compileLexpAsRvalue ctx e in
      let fi = to_closure (compileExp ctx index) in
      fun fr val_ ->
        match (fe fr, fi fr) with
        | DArray arr, DInt i when i >= 0 && i < Array.length arr ->
            arr.(i) <- val_
        | DList list_ref, DInt i ->
            let old_list = !list_ref in
            let len = CCList.length old_list in
            if i >= 0 && i < len then
              let before, after = CCList.take_drop i old_list in
              match after with
              | _ :: rest ->
                  list_ref := before @ [val_] @ rest
              | [] ->
                  error "List index out of bounds for assignment"
            else error "List index out of bounds"
        | _ ->
            error "Invalid array or list assignment" )
  | LTuple lexps -> (
      let fassigns = CCList.mapi (fun i lv -> (i, compileLexpAssign ctx lv)) lexps in
      fun fr val_ ->
        match val_ with
        | DArray vals when Array.length vals = CCList.length fassigns ->
            CCList.iter (fun (i, fassign) -> fassign fr vals.(i)) fassigns
        | _ ->
            error "Tuple assignment type mismatch" )

(* Calls a compiled function from an entry point, binding the arguments in a new frame *)
let callCompiledFunctionByIdx (prog : iprog) (sample_rate : float option) (func_idx : int) (args : dvalue list) : dvalue
    =
  if func_idx < 0 || func_idx >= Array.length prog.compiled_functions then
    error (Printf.sprintf "Internal error: function index %d has not been compiled" func_idx)
  else
    let cfunc = Array.get prog.compiled_functions func_idx in
    let fr = newFrame cfunc in
    CCList.iteri (fun i arg_val -> fr.(cfunc.cf_args.(i)) <- arg_val) args ;
    runWithCallStack prog.runtime sample_rate (fun () -> runFrame cfunc fr)

(* ---- Program Processing ---- *)

(* Compile a single function definition from Prog into a compiled_func *)
let compileProgramFunction (prog : iprog) (def : function_def) (body : stmt) : compiled_func =
  let ctx =
    { var_to_index= Map.empty
    ; next_index= 0
    ; struct_types= prog.struct_types
    ; constant_names= prog.constant_names
    ; function_names= prog.ifunction_names
    ; external_functions= prog.external_functions
    ; prog }
  in
  let param_indices = CCList.map (fun (p : param) -> addVar ctx p.name) def.args in
  let compiled_body = compileStmt ctx body in
  {cf_name= def.name; cf_args= Array.of_list param_indices; cf_locals= ctx.next_index; cf_body= compiled_body}

(* Process a single top-level statement, compiling it into the iprog *)
let processStatement (prog : iprog) (stmt : top_stmt) : unit =
  match stmt.top with
  | TopType descr ->
      prog.struct_types <- Map.add descr.path descr prog.struct_types
  | TopAlias _ ->
      ()
  | TopExternal (def, _) ->
      prog.external_functions <- Set.add def.name prog.external_functions
  | TopConstant (name, _, _, exp, _) ->
      let const_idx = prog.iconstants_count in
      prog.constant_names <- Map.add name const_idx prog.constant_names ;
      let ctx =
        { var_to_index= Map.empty
        ; next_index= 0
        ; struct_types= prog.struct_types
        ; constant_names= prog.constant_names
        ; function_names= prog.ifunction_names
        ; external_functions= prog.external_functions
        ; prog }
      in
      let compiled = compileExp ctx exp in
      let value =
        match compiled with
        | CConstant v ->
            v
        | _ ->
            let f = to_closure compiled in
            let fr = Array.make ctx.next_index DVoid in
            runWithCallStack prog.runtime None (fun () -> f fr)
      in
      addConstant prog value
  | TopFunction (def, body) -> (
      let func_idx =
        match Map.find_opt def.name prog.ifunction_names with
        | Some idx ->
            idx
        | None ->
            let idx = Map.cardinal prog.ifunction_names in
            prog.ifunction_names <- Map.add def.name idx prog.ifunction_names ;
            idx
      in
      let cfunc = compileProgramFunction prog def body in
      addCompiledFunction prog func_idx cfunc ;
      (* Check if this function is a candidate for inlining *)
      match body.s with
      | StmtBlock [{s= StmtReturn ret_exp; _}] | StmtReturn ret_exp ->
          let param_names = CCList.map (fun (p : param) -> p.name) def.args in
          if progExpSize ret_exp <= 30 && not (containsProgCall def.name ret_exp) then
            prog.inlinable <- Map.add def.name (param_names, ret_exp) prog.inlinable
      | StmtBlock [{s= StmtDecl ({d= DId (let_name, _); _}, Some init_exp); _}; {s= StmtReturn ret_exp; _}] ->
          let param_names = CCList.map (fun (p : param) -> p.name) def.args in
          if not (CCList.mem ~eq:String.equal let_name param_names) then begin
            let subst = Map.singleton let_name init_exp in
            let expanded = substituteProgExp subst ret_exp in
            if progExpSize expanded <= 30 && not (containsProgCall def.name expanded) then
              prog.inlinable <- Map.add def.name (param_names, expanded) prog.inlinable
          end
      | _ ->
          () )

(* Collect the names of the functions called in an expression or statement *)
let rec callsInExp (acc : string list) (e : exp) : string list =
  match e.e with
  | EUnit | EEmptyValue | EBool _ | EInt _ | EReal _ | EFixed _ | EString _ | EId _ ->
      acc
  | EUnOp (_, e1) ->
      callsInExp acc e1
  | EOp (_, e1, e2) ->
      callsInExp (callsInExp acc e1) e2
  | EIndex {e= e1; index} ->
      callsInExp (callsInExp acc e1) index
  | EArray elems | ETuple elems ->
      CCList.fold_left callsInExp acc elems
  | ECall {path; args} ->
      CCList.fold_left callsInExp (path :: acc) args
  | EIf {cond; then_; else_} ->
      callsInExp (callsInExp (callsInExp acc cond) then_) else_
  | EMember (e1, _) | ETMember (e1, _) ->
      callsInExp acc e1
  | ERecord {elems; _} ->
      CCList.fold_left (fun acc (_, v) -> callsInExp acc v) acc elems

let rec callsInLexp (acc : string list) (l : lexp) : string list =
  match l.l with
  | LWild | LId _ ->
      acc
  | LMember (le, _) ->
      callsInLexp acc le
  | LIndex {e= le; index} ->
      callsInExp (callsInLexp acc le) index
  | LTuple elems ->
      CCList.fold_left callsInLexp acc elems

let rec callsInStmt (acc : string list) (s : stmt) : string list =
  match s.s with
  | StmtDecl (_, None) ->
      acc
  | StmtDecl (_, Some e) ->
      callsInExp acc e
  | StmtBind (l, e) ->
      callsInExp (callsInLexp acc l) e
  | StmtReturn e ->
      callsInExp acc e
  | StmtBlock stmts ->
      CCList.fold_left callsInStmt acc stmts
  | StmtIf (cond, then_, else_opt) -> (
      let acc = callsInStmt (callsInExp acc cond) then_ in
      match else_opt with Some s -> callsInStmt acc s | None -> acc )
  | StmtWhile (cond, body) ->
      callsInStmt (callsInExp acc cond) body
  | StmtSwitch (e, cases, default_opt) -> (
      let acc = callsInExp acc e in
      let acc = CCList.fold_left (fun acc (case_e, case_s) -> callsInStmt (callsInExp acc case_e) case_s) acc cases in
      match default_opt with Some s -> callsInStmt acc s | None -> acc )

(* Process a list of top-level statements. The passes sort constants before the functions,
   but constant initializers may call functions, so the functions a constant needs are
   compiled on demand (transitively) before the constant is evaluated. *)
let processStatements (prog : iprog) (stmts : top_stmt list) : unit =
  let addPending pending (stmt : top_stmt) =
    match stmt.top with TopFunction (def, body) -> Map.add def.name (def, body) pending | _ -> pending
  in
  (* Returns the functions that are still pending after compiling [name] and its callees *)
  let rec ensureCompiled (pending : (function_def * stmt) Map.t) (name : string) : (function_def * stmt) Map.t =
    match Map.find_opt name pending with
    | None ->
        pending
    | Some (def, body) ->
        (* Remove before recursing so recursive functions terminate *)
        let pending = CCList.fold_left ensureCompiled (Map.remove name pending) (callsInStmt [] body) in
        processStatement prog {top= TopFunction (def, body); loc= def.loc} ;
        pending
  in
  let processInOrder pending (stmt : top_stmt) =
    match stmt.top with
    | TopConstant (_, _, _, exp, _) ->
        let pending = CCList.fold_left ensureCompiled pending (callsInExp [] exp) in
        processStatement prog stmt ; pending
    | TopFunction (def, _) ->
        ensureCompiled pending def.name
    | _ ->
        processStatement prog stmt ; pending
  in
  let pending = CCList.fold_left addPending Map.empty stmts in
  let _ : (function_def * stmt) Map.t = CCList.fold_left processInOrder pending stmts in
  ()

(* Full pipeline from Prog AST to compiled iprog.
   Two-pass: first register all function names, then process everything. *)
let transformProgram (prog : top_stmt list) : iprog =
  let iprog = createEmptyProgram () in
  (* First pass: register all function names so forward references work *)
  CCList.iter
    (fun (stmt : top_stmt) ->
      match stmt.top with
      | TopFunction (def, _) ->
          if not (Map.mem def.name iprog.ifunction_names) then begin
            let func_idx = Map.cardinal iprog.ifunction_names in
            iprog.ifunction_names <- Map.add def.name func_idx iprog.ifunction_names
          end
      | _ ->
          () )
    prog ;
  (* Second pass: process all statements *)
  processStatements iprog prog ;
  iprog

(* ---- Public Entry Points ---- *)

(* Evaluate a single Vult expression string in the context of an existing program. *)
let evaluateMainExpression args env iprog exp : dvalue =
  let e = Pparser.Parse.parseString (Some "Main_.vult") (Pla.print {%pla|fun _main_() return <#exp#s>;|}) in
  let env, main = Typechecking.typecheck_single args env e in
  let _, main = Toprog.convert args env main in
  let main = Passes.run args main in
  (* Register function names first *)
  CCList.iter
    (fun (stmt : top_stmt) ->
      match stmt.top with
      | TopFunction (def, _) ->
          if not (Map.mem def.name iprog.ifunction_names) then begin
            let func_idx = Map.cardinal iprog.ifunction_names in
            iprog.ifunction_names <- Map.add def.name func_idx iprog.ifunction_names
          end
      | _ ->
          () )
    main ;
  processStatements iprog main ;
  (* Look for the new function Main___main_ *)
  let main_func_name = "Main___main_" in
  match Map.find_opt main_func_name iprog.ifunction_names with
  | Some _ -> (
      let call_args =
        let alloc_func_name = main_func_name ^ "_type_alloc" in
        match Map.find_opt alloc_func_name iprog.ifunction_names with
        | Some alloc_idx ->
            let state = callCompiledFunctionByIdx iprog args.fs alloc_idx [] in
            [state]
        | None ->
            []
      in
      match Map.find_opt main_func_name iprog.ifunction_names with
      | Some func_idx ->
          callCompiledFunctionByIdx iprog args.fs func_idx call_args
      | None ->
          error "Could not execute the expression" )
  | None ->
      error "Could not execute the expression"

(* ---- Audio Rendering ----

   Supports the @[render ...] tag: compiles an expression into a loop that fills
   a buffer, evaluates it, and writes the result to a WAV file. *)
type render_params = {file: string; samplerate: int; time: float; exp: string}

let default_render_params = {file= "output.wav"; samplerate= 48000; time= 1.0; exp= ""}

let parseRenderParams (tag_string : string) : render_params =
  let tag = Pparser.Parse.parseTagString tag_string in
  let params = Pparser.Ptags.[("file", TypeString); ("samplerate", TypeInt); ("time", TypeReal); ("exp", TypeString)] in
  match Pparser.Ptags.getParameterList [tag] "render" params with
  | [file_opt; samplerate_opt; time_opt; exp_opt] ->
      let file = match file_opt with Some (Pparser.Ptags.String s) -> s | _ -> default_render_params.file in
      let samplerate =
        match samplerate_opt with Some (Pparser.Ptags.Int i) -> i | _ -> default_render_params.samplerate
      in
      let time =
        match time_opt with
        | Some (Pparser.Ptags.Real f) ->
            f
        | Some (Pparser.Ptags.Int i) ->
            float_of_int i
        | _ ->
            default_render_params.time
      in
      let exp = match exp_opt with Some (Pparser.Ptags.String s) -> s | _ -> default_render_params.exp in
      {file; samplerate; time; exp}
  | _ ->
      error "Invalid render parameters format"

let generateRenderWrapper (params : render_params) : string =
  let n_samples = int_of_float (params.time *. float_of_int params.samplerate) in
  let exp = params.exp in
  Pla.print
    {%pla|fun _main() {
  val buffer : array(real, <#n_samples#i>);
  iter(i, size(buffer)) {
    buffer[i] = <#exp#s>;
  }
  return buffer;
}|}

let dvalueToFloat (dval : dvalue) : float =
  match dval with DReal f -> f | DInt i -> float_of_int i | _ -> error "Sample values must be numeric"

let writeResultToWav (result : dvalue) (params : render_params) : unit =
  match result with
  | DArray samples -> (
      let float_samples = Array.map dvalueToFloat samples in
      match Util.WaveFile.write_mono params.file float_samples ~sample_rate:params.samplerate () with
      | Ok () ->
          ()
      | Error msg ->
          error ("Failed to write WAV file: " ^ msg) )
  | _ ->
      error "Render function must return an array"

let renderAudioExpression (args : Util.Args.args) (env : Env.in_top) (iprog : iprog) (tag_string : string) :
    string * float =
  let start_time = Sys.time () in
  (* Parse render parameters *)
  let params = parseRenderParams tag_string in
  (* Always update args.fs *)
  args.fs <- Some (float_of_int params.samplerate) ;
  (* Generate wrapper function *)
  let wrapper_code = generateRenderWrapper params in
  (* Parse and compile wrapper function *)
  let e = Pparser.Parse.parseString (Some "Render_.vult") wrapper_code in
  let env, main = Typechecking.typecheck_single args env e in
  let _, main = Toprog.convert args env main in
  let main = Passes.run args main in
  (* Register function names first *)
  CCList.iter
    (fun (stmt : top_stmt) ->
      match stmt.top with
      | TopFunction (def, _) ->
          if not (Map.mem def.name iprog.ifunction_names) then begin
            let func_idx = Map.cardinal iprog.ifunction_names in
            iprog.ifunction_names <- Map.add def.name func_idx iprog.ifunction_names
          end
      | _ ->
          () )
    main ;
  processStatements iprog main ;
  (* Execute wrapper function *)
  let main_func_name = "Render___main" in
  (* Prepare call arguments *)
  let call_args =
    let alloc_func_name = main_func_name ^ "_type_alloc" in
    match Map.find_opt alloc_func_name iprog.ifunction_names with
    | Some alloc_idx ->
        let state = callCompiledFunctionByIdx iprog args.fs alloc_idx [] in
        [state]
    | None ->
        []
  in
  match Map.find_opt main_func_name iprog.ifunction_names with
  | Some func_idx ->
      let result = callCompiledFunctionByIdx iprog args.fs func_idx call_args in
      writeResultToWav result params ;
      let end_time = Sys.time () in
      let duration = end_time -. start_time in
      (params.file, duration)
  | None ->
      error "Could not execute render function"

(* External API: call a compiled function by index. *)
let callFunctionEntry (prog : iprog) (func_idx : int) (args : dvalue list) : dvalue =
  callCompiledFunctionByIdx prog None func_idx args
