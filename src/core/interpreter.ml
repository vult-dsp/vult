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
       - Resolves variable names to stack-frame indices
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
       callCompiledFunctionByIdx invokes compiled closures on a runtime stack.
       The stack is a flat dvalue array with a moving stack pointer; each function
       call allocates a frame of cf_locals slots.

   Runtime Model
   -------------
   - Stack-based: all local variables and function arguments live in a flat array
     indexed by (frame_start + variable_index).
   - No heap allocation for scalars: DInt, DReal, DBool are unboxed OCaml values
     inside the dvalue variant.
   - Mutable aggregates: DArray and DStruct contain OCaml arrays that are mutated
     in-place for assignments. DList wraps a mutable ref to an OCaml list.
   - Call depth tracking: each call increments ctx.depth; exceeding max_depth
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

(* ---- Runtime Infrastructure ----

   call_context carries per-call metadata: the call stack (for error messages),
   current recursion depth, maximum allowed depth, and the sample rate (set via CLI).
   It is passed through every closure but is lightweight — only depth is mutated
   (via a new record on each call).
*)
type call_context =
  { frames: string list (* Function names in call order, used only for error reporting *)
  ; depth: int (* Current call depth, O(1) tracking *)
  ; max_depth: int
  ; sample_rate: float option (* Sample rate from CLI args *) }

(* The runtime stack is a flat dvalue array shared across all function calls.
   Each function call allocates a frame of cf_locals slots starting at sp.
   stack.sp tracks the current top; max_size prevents overflow. *)
type runtime_stack = {stack: dvalue array; mutable sp: int (* Stack pointer *); max_size: int}

(* Statement execution returns either Continue (fall through) or Return with a value.
   This replaces exception-based control flow for better performance. *)
type exec_result = Continue | Return of dvalue

(* ---- Compiled Closure Types ----

   After compilation, every expression and statement is represented as an OCaml closure.
   The closure signature is: call_context -> runtime_stack -> frame_start -> result

   - compiled_exp: evaluates an expression, returns a dvalue
   - compiled_stmt: executes a statement, returns Continue or Return
   - compiled_func: bundles a compiled function body with its metadata
*)
type compiled_exp = call_context -> runtime_stack -> int -> dvalue

type compiled_stmt = call_context -> runtime_stack -> int -> exec_result

(* compiled_result is the return type of compileExp. It classifies expressions
   into three categories to enable compile-time optimizations:

   - CConstant v: The expression always evaluates to v. No closure is allocated.

   - CVar idx: The expression is a direct variable read from stack[frame_start + idx].
     This is tracked separately from CDynamic because parent nodes can generate
     "fused" closures that inline the array access, eliminating one indirect call.

   - CDynamic f: A general runtime closure. Used when the expression depends on
     runtime state in a way that can't be simplified further. *)
type compiled_result = CConstant of dvalue | CVar of int | CDynamic of compiled_exp

type compiled_func = {cf_name: string; cf_args: int list; cf_locals: int; cf_body: compiled_stmt}

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
  ; mutable inlinable: (string list * exp) Map.t (* Function name -> (param_names, body_exp) for inlining *) }

(* ---- Compilation Context ----

   compile_ctx is used during compilation to resolve names to indices.
   var_to_index is mutable because new variables are added as declarations
   are encountered. The other fields are immutable references into the iprog.
*)
type compile_ctx =
  { var_to_index: (string, int) Hashtbl.t (* Variable name -> stack frame index *)
  ; mutable next_index: int (* Next available stack slot *)
  ; struct_types: struct_descr Map.t (* Struct type definitions *)
  ; constant_names: int Map.t (* Constant name -> index *)
  ; function_names: int Map.t (* Function name -> index *)
  ; external_functions: Set.t (* External function names *)
  ; prog: iprog (* Back-reference to the program being built *) }

(* Exceptions *)
exception Runtime_error of string

(* Enhanced error function that includes call stack information *)
let error_with_context (ctx : call_context) (msg : string) : 'a =
  let call_stack_info =
    match ctx.frames with
    | [] ->
        ""
    | frames ->
        let stack_str =
          frames |> CCList.mapi (fun i func_name -> Printf.sprintf "  %d. %s" (i + 1) func_name) |> String.concat "\n"
        in
        Printf.sprintf "\nCall stack:\n%s" stack_str
  in
  let enhanced_msg = msg ^ call_stack_info in
  raise (Runtime_error enhanced_msg)

(* Legacy error function for backwards compatibility *)
let error (msg : string) : 'a = raise (Runtime_error msg)

(* Creates an empty iprog for incremental building *)
let createEmptyProgram () : iprog =
  { ifunction_names= Map.empty
  ; compiled_functions= [||]
  ; iconstants= [||]
  ; iconstants_count= 0
  ; struct_types= Map.empty
  ; constant_names= Map.empty
  ; external_functions= Set.empty
  ; inlinable= Map.empty }

(* Adds a compiled function to the iprog, resizing the array as needed *)
let addCompiledFunction (prog : iprog) (func_idx : int) (cfunc : compiled_func) : unit =
  if func_idx >= Array.length prog.compiled_functions then (
    let new_size = max (func_idx + 1) ((Array.length prog.compiled_functions * 2) + 16) in
    let dummy = {cf_name= ""; cf_args= []; cf_locals= 0; cf_body= (fun _ctx _stack _fs -> Continue)} in
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
  if Hashtbl.mem ctx.var_to_index name then Hashtbl.find ctx.var_to_index name
  else
    let idx = ctx.next_index in
    Hashtbl.add ctx.var_to_index name idx ;
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

(* Creates a new runtime stack with the specified size *)
let createStack (size : int) : runtime_stack = {stack= Array.make size DVoid; sp= 0; max_size= size}

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
let getArrayElement (ctx : call_context) (arr : dvalue) (idx : dvalue) : dvalue =
  match (arr, idx) with
  | DArray elems, DInt i when i >= 0 && i < Array.length elems ->
      elems.(i)
  | DArray elems, DInt i ->
      error_with_context ctx
        ( "getArrayElement: Invalid array access. size = "
        ^ string_of_int (Array.length elems)
        ^ " index = " ^ string_of_int i )
  | DList list_ref, DInt i -> (
    match CCList.nth_opt !list_ref i with
    | Some v ->
        v
    | None ->
        error_with_context ctx
          ( "getArrayElement: List index out of bounds. size = "
          ^ string_of_int (CCList.length !list_ref)
          ^ " index = " ^ string_of_int i ) )
  | _ ->
      error_with_context ctx "getArrayElement: Invalid array access. This is not an array or list"

(* Retrieves a member from a struct using a member index *)
let getStructMember (ctx : call_context) (struct_val : dvalue) (member_idx : int) : dvalue =
  match struct_val with
  | DStruct members when member_idx >= 0 && member_idx < Array.length members ->
      members.(member_idx)
  | _ ->
      error_with_context ctx "Invalid struct member access"

(* ---- Runtime Operations ----

   These functions implement the core runtime semantics: binary/unary operations.
   They are called from compiled closures for generic EOp/EUnOp nodes.
*)

(* Evaluates a binary operation on two runtime values *)
let evalBinop (ctx : call_context) (op : operator) (v1 : dvalue) (v2 : dvalue) : dvalue =
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
      DBool (a = b)
  | OpEq, DInt16 a, DInt16 b ->
      DBool (a = b)
  | OpEq, DReal a, DReal b ->
      DBool (a = b)
  | OpEq, DBool a, DBool b ->
      DBool (a = b)
  | OpEq, DString a, DString b ->
      DBool (a = b)
  | OpNe, DInt a, DInt b ->
      DBool (a <> b)
  | OpNe, DInt16 a, DInt16 b ->
      DBool (a <> b)
  | OpNe, DReal a, DReal b ->
      DBool (a <> b)
  | OpNe, DBool a, DBool b ->
      DBool (a <> b)
  | OpNe, DString a, DString b ->
      DBool (a <> b)
  | OpLt, DInt a, DInt b ->
      DBool (a < b)
  | OpLt, DInt16 a, DInt16 b ->
      DBool (a < b)
  | OpLt, DReal a, DReal b ->
      DBool (a < b)
  | OpLe, DInt a, DInt b ->
      DBool (a <= b)
  | OpLe, DInt16 a, DInt16 b ->
      DBool (a <= b)
  | OpLe, DReal a, DReal b ->
      DBool (a <= b)
  | OpGt, DInt a, DInt b ->
      DBool (a > b)
  | OpGt, DInt16 a, DInt16 b ->
      DBool (a > b)
  | OpGt, DReal a, DReal b ->
      DBool (a > b)
  | OpGe, DInt a, DInt b ->
      DBool (a >= b)
  | OpGe, DInt16 a, DInt16 b ->
      DBool (a >= b)
  | OpGe, DReal a, DReal b ->
      DBool (a >= b)
  | OpLand, DBool a, DBool b ->
      DBool (a && b)
  | OpLor, DBool a, DBool b ->
      DBool (a || b)
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
      error_with_context ctx ("Unsupported operation: " ^ v1 ^ " " ^ ops ^ " " ^ v2)

(* Evaluates a unary operation on a runtime value *)
let evalUnop (ctx : call_context) (op : uoperator) (v : dvalue) : dvalue =
  match (op, v) with
  | UOpNeg, DInt i ->
      DInt (-i)
  | UOpNeg, DInt16 i ->
      DInt16 (max (-32768) (min 32767 (-i)))
  | UOpNeg, DReal f ->
      DReal (-.f)
  | UOpNot, DBool b ->
      DBool (not b)
  | UOpNot, DInt i ->
      DBool (i = 0)
  | UOpNot, DInt16 i ->
      DBool (i = 0)
  | _ ->
      error_with_context ctx "Unsupported unary operation"

(* ---- Closure Compilation Helpers ----

   to_closure: converts compiled_result -> compiled_exp (materializes CConstant/CVar as closures)

   compile_binop_xx: create closures for binary operations on two compiled_exp values.
   compile_binop_xx_cp: constant-propagating wrappers that take compiled_result values.
   compile_unary_r_cp: same pattern for unary float operations.
*)

(* Convert a compiled_result to a compiled_exp closure. *)
let to_closure (r : compiled_result) : compiled_exp =
  match r with
  | CConstant v ->
      fun _ctx _stack _fs -> v
  | CVar idx ->
      fun _ctx stack fs -> Array.unsafe_get stack.stack (fs + idx)
  | CDynamic f ->
      f

(* Base closure-building helpers for binary and unary operations. *)
let compile_binop_ii (f1 : compiled_exp) (f2 : compiled_exp) (op : int -> int -> dvalue) (err : string) : compiled_exp =
 fun ctx stack fs ->
  match (f1 ctx stack fs, f2 ctx stack fs) with DInt a, DInt b -> op a b | _ -> error_with_context ctx err

let compile_binop_i16 (f1 : compiled_exp) (f2 : compiled_exp) (op : int -> int -> int) (err : string) : compiled_exp =
 fun ctx stack fs ->
  match (f1 ctx stack fs, f2 ctx stack fs) with
  | DInt16 a, DInt16 b ->
      let result = op a b in
      DInt16 (max (-32768) (min 32767 result))
  | _ ->
      error_with_context ctx err

let compile_binop_rr (f1 : compiled_exp) (f2 : compiled_exp) (op : float -> float -> dvalue) (err : string) :
    compiled_exp =
 fun ctx stack fs ->
  match (f1 ctx stack fs, f2 ctx stack fs) with DReal a, DReal b -> op a b | _ -> error_with_context ctx err

let compile_unary_r (f1 : compiled_exp) (op : float -> float) (err : string) : compiled_exp =
 fun ctx stack fs -> match f1 ctx stack fs with DReal f -> DReal (op f) | _ -> error_with_context ctx err

(* Constant-propagating and CVar-fusing helpers. *)
let compile_binop_ii_cp (r1 : compiled_result) (r2 : compiled_result) (op : int -> int -> dvalue) (err : string) :
    compiled_result =
  match (r1, r2) with
  | CConstant (DInt a), CConstant (DInt b) ->
      CConstant (op a b)
  | CVar idx1, CVar idx2 ->
      CDynamic
        (fun ctx stack fs ->
          match (Array.unsafe_get stack.stack (fs + idx1), Array.unsafe_get stack.stack (fs + idx2)) with
          | DInt a, DInt b ->
              op a b
          | _ ->
              error_with_context ctx err )
  | CVar idx, CConstant (DInt b) ->
      CDynamic
        (fun ctx stack fs ->
          match Array.unsafe_get stack.stack (fs + idx) with DInt a -> op a b | _ -> error_with_context ctx err )
  | CConstant (DInt a), CVar idx ->
      CDynamic
        (fun ctx stack fs ->
          match Array.unsafe_get stack.stack (fs + idx) with DInt b -> op a b | _ -> error_with_context ctx err )
  | _ ->
      CDynamic (compile_binop_ii (to_closure r1) (to_closure r2) op err)

let compile_binop_i16_cp (r1 : compiled_result) (r2 : compiled_result) (op : int -> int -> int) (err : string) :
    compiled_result =
  match (r1, r2) with
  | CConstant (DInt16 a), CConstant (DInt16 b) ->
      let result = op a b in
      CConstant (DInt16 (max (-32768) (min 32767 result)))
  | _ ->
      CDynamic (compile_binop_i16 (to_closure r1) (to_closure r2) op err)

let compile_binop_rr_cp (r1 : compiled_result) (r2 : compiled_result) (op : float -> float -> dvalue) (err : string) :
    compiled_result =
  match (r1, r2) with
  | CConstant (DReal a), CConstant (DReal b) ->
      CConstant (op a b)
  | CVar idx1, CVar idx2 ->
      CDynamic
        (fun ctx stack fs ->
          match (Array.unsafe_get stack.stack (fs + idx1), Array.unsafe_get stack.stack (fs + idx2)) with
          | DReal a, DReal b ->
              op a b
          | _ ->
              error_with_context ctx err )
  | CVar idx, CConstant (DReal b) ->
      CDynamic
        (fun ctx stack fs ->
          match Array.unsafe_get stack.stack (fs + idx) with DReal a -> op a b | _ -> error_with_context ctx err )
  | CConstant (DReal a), CVar idx ->
      CDynamic
        (fun ctx stack fs ->
          match Array.unsafe_get stack.stack (fs + idx) with DReal b -> op a b | _ -> error_with_context ctx err )
  | _ ->
      CDynamic (compile_binop_rr (to_closure r1) (to_closure r2) op err)

let compile_unary_r_cp (r1 : compiled_result) (op : float -> float) (err : string) : compiled_result =
  match r1 with
  | CConstant (DReal f) ->
      CConstant (DReal (op f))
  | CVar idx ->
      CDynamic
        (fun ctx stack fs ->
          match Array.unsafe_get stack.stack (fs + idx) with DReal f -> DReal (op f) | _ -> error_with_context ctx err )
  | _ ->
      CDynamic (compile_unary_r (to_closure r1) op err)

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
    match exp.t.t with TList _ -> CDynamic (fun _ctx _stack _fs -> DList (ref [])) | _ -> CConstant DVoid )
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
    match Hashtbl.find_opt ctx.var_to_index name with
    | Some idx ->
        CVar idx
    | None -> (
      match Map.find_opt name ctx.constant_names with
      | Some const_idx ->
          CConstant ctx.prog.iconstants.(const_idx)
      | None ->
          error ("Variable or constant not found: " ^ name) ) )
  (* -- Specialized arithmetic operations based on types -- *)
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
        (fun a b -> DBool (a = b))
        "Type mismatch in integer equality"
  | EOp (OpEq, e1, e2) when isRealType e1.t || isRealType e2.t ->
      compile_binop_rr_cp (compileExp ctx e1) (compileExp ctx e2)
        (fun a b -> DBool (Float.equal a b))
        "Type mismatch in real equality"
  | EOp (OpLt, e1, e2) when isInt16Type e1.t && isInt16Type e2.t ->
      compileLtInt16 ctx e1 e2
  | EOp (OpLt, e1, e2) when isIntType e1.t && isIntType e2.t ->
      compile_binop_ii_cp (compileExp ctx e1) (compileExp ctx e2)
        (fun a b -> DBool (a < b))
        "Type mismatch in integer less than"
  | EOp (OpLt, e1, e2) when isRealType e1.t || isRealType e2.t ->
      compile_binop_rr_cp (compileExp ctx e1) (compileExp ctx e2)
        (fun a b -> DBool (a < b))
        "Type mismatch in real less than"
  | EOp (OpGt, e1, e2) when isInt16Type e1.t && isInt16Type e2.t ->
      compileGtInt16 ctx e1 e2
  | EOp (OpGt, e1, e2) when isIntType e1.t && isIntType e2.t ->
      compile_binop_ii_cp (compileExp ctx e1) (compileExp ctx e2)
        (fun a b -> DBool (a > b))
        "Type mismatch in integer greater than"
  | EOp (OpGt, e1, e2) when isRealType e1.t || isRealType e2.t ->
      compile_binop_rr_cp (compileExp ctx e1) (compileExp ctx e2)
        (fun a b -> DBool (a > b))
        "Type mismatch in real greater than"
  (* Generic fallback for remaining ops *)
  | EOp (op, e1, e2) -> (
      let r1 = compileExp ctx e1 in
      let r2 = compileExp ctx e2 in
      match (r1, r2) with
      | CConstant v1, CConstant v2 ->
          let dummy_ctx = {frames= []; depth= 0; max_depth= 0; sample_rate= None} in
          CConstant (evalBinop dummy_ctx op v1 v2)
      | _ ->
          let f1 = to_closure r1 in
          let f2 = to_closure r2 in
          CDynamic (fun ctx stack fs -> evalBinop ctx op (f1 ctx stack fs) (f2 ctx stack fs)) )
  | EUnOp (op, e1) -> (
      let r1 = compileExp ctx e1 in
      match r1 with
      | CConstant v ->
          let dummy_ctx = {frames= []; depth= 0; max_depth= 0; sample_rate= None} in
          CConstant (evalUnop dummy_ctx op v)
      | _ ->
          let f1 = to_closure r1 in
          CDynamic (fun ctx stack fs -> evalUnop ctx op (f1 ctx stack fs)) )
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
  | ECall {path= "abs"; args= [a]} ->
      compileAbs ctx a
  | ECall {path= "pow"; args= [a; b]} ->
      compile_binop_rr_cp (compileExp ctx a) (compileExp ctx b) (fun a b -> DReal (a ** b)) "Type mismatch in pow"
  | ECall {path= "clip"; args= [x; mn; mx]} when isRealType exp.t ->
      compileClipReal ctx x mn mx
  | ECall {path= "clip"; args= [x; mn; mx]} when isIntType exp.t ->
      compileClipInt ctx x mn mx
  | ECall {path= "pi"; args= []} ->
      CConstant (DReal Float.pi)
  | ECall {path= "eps"; args= []} ->
      CConstant (DReal 1e-18)
  | ECall {path= "samplerate"; args= []} ->
      CDynamic
        (fun ctx _stack _fs ->
          match ctx.sample_rate with
          | Some fs ->
              DReal fs
          | None ->
              error_with_context ctx
                "samplerate() requires the -samplerate flag. Use: vult file.vult -eval \"expr\" -samplerate 44100" )
  | ECall {path= "random"; args= []} ->
      CDynamic (fun _ctx _stack _fs -> DReal (Random.float 1.0))
  | ECall {path= "irandom"; args= []} ->
      CDynamic (fun _ctx _stack _fs -> DInt (Random.int Int.max_int))
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
      CDynamic (fun ctx _stack _fs -> error_with_context ctx ("External evaluations are not possible: " ^ name))
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
            (fun ctx stack fs ->
              match fc ctx stack fs with
              | DBool true ->
                  ft ctx stack fs
              | DBool false ->
                  ff ctx stack fs
              | _ ->
                  error_with_context ctx "Invalid condition" ) )
  (* -- Member access with var.field fusion -- *)
  | EMember (e, member_name) -> (
    match e.t.t with
    | TStruct descr -> (
        let member_idx = getMemberIndex descr member_name in
        let re = compileExp ctx e in
        match re with
        | CVar var_idx ->
            CDynamic
              (fun ctx stack fs ->
                match Array.unsafe_get stack.stack (fs + var_idx) with
                | DStruct members ->
                    Array.unsafe_get members member_idx
                | _ ->
                    error_with_context ctx "Invalid struct access" )
        | CConstant (DStruct members) when member_idx >= 0 && member_idx < Array.length members ->
            CConstant members.(member_idx)
        | _ ->
            let fe = to_closure re in
            CDynamic (fun ctx stack fs -> getStructMember ctx (fe ctx stack fs) member_idx) )
    | _ ->
        error "Member access on non-struct type" )
  | ETMember (e, idx) -> (
      let re = compileExp ctx e in
      match re with
      | CVar var_idx ->
          CDynamic
            (fun ctx stack fs ->
              match Array.unsafe_get stack.stack (fs + var_idx) with
              | DStruct members ->
                  Array.unsafe_get members idx
              | DArray arr ->
                  Array.unsafe_get arr idx
              | _ ->
                  error_with_context ctx "Invalid tuple/struct member access" )
      | CConstant (DStruct members) when idx >= 0 && idx < Array.length members ->
          CConstant members.(idx)
      | CConstant (DArray arr) when idx >= 0 && idx < Array.length arr ->
          CConstant arr.(idx)
      | _ ->
          let fe = to_closure re in
          CDynamic (fun ctx stack fs -> getStructMember ctx (fe ctx stack fs) idx) )
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
          CDynamic (fun ctx stack fs -> getArrayElement ctx (fe ctx stack fs) (fi ctx stack fs)) )
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
        CDynamic (fun ctx stack fs -> DArray (Array.of_list (CCList.map (fun f -> f ctx stack fs) felems)))
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
        CDynamic (fun ctx stack fs -> DArray (Array.of_list (CCList.map (fun f -> f ctx stack fs) felems)))
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
            (fun ctx stack fs ->
              let member_vals = Array.make n_members DVoid in
              CCList.iter (fun (idx, f) -> member_vals.(idx) <- f ctx stack fs) felems ;
              DStruct member_vals )
    | None ->
        error ("Unknown struct type: " ^ path) )

(* Int16 comparison helpers *)
and compileEqInt16 (ctx : compile_ctx) (e1 : exp) (e2 : exp) : compiled_result =
  let r1 = compileExp ctx e1 in
  let r2 = compileExp ctx e2 in
  match (r1, r2) with
  | CConstant (DInt16 a), CConstant (DInt16 b) ->
      CConstant (DBool (a = b))
  | _ ->
      let f1 = to_closure r1 in
      let f2 = to_closure r2 in
      CDynamic
        (fun ctx stack fs ->
          match (f1 ctx stack fs, f2 ctx stack fs) with
          | DInt16 a, DInt16 b ->
              DBool (a = b)
          | _ ->
              error_with_context ctx "Type mismatch in int16 equality" )

and compileLtInt16 (ctx : compile_ctx) (e1 : exp) (e2 : exp) : compiled_result =
  let r1 = compileExp ctx e1 in
  let r2 = compileExp ctx e2 in
  match (r1, r2) with
  | CConstant (DInt16 a), CConstant (DInt16 b) ->
      CConstant (DBool (a < b))
  | _ ->
      let f1 = to_closure r1 in
      let f2 = to_closure r2 in
      CDynamic
        (fun ctx stack fs ->
          match (f1 ctx stack fs, f2 ctx stack fs) with
          | DInt16 a, DInt16 b ->
              DBool (a < b)
          | _ ->
              error_with_context ctx "Type mismatch in int16 less than" )

and compileGtInt16 (ctx : compile_ctx) (e1 : exp) (e2 : exp) : compiled_result =
  let r1 = compileExp ctx e1 in
  let r2 = compileExp ctx e2 in
  match (r1, r2) with
  | CConstant (DInt16 a), CConstant (DInt16 b) ->
      CConstant (DBool (a > b))
  | _ ->
      let f1 = to_closure r1 in
      let f2 = to_closure r2 in
      CDynamic
        (fun ctx stack fs ->
          match (f1 ctx stack fs, f2 ctx stack fs) with
          | DInt16 a, DInt16 b ->
              DBool (a > b)
          | _ ->
              error_with_context ctx "Type mismatch in int16 greater than" )

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
        (fun ctx stack fs ->
          match Array.unsafe_get stack.stack (fs + idx) with
          | DReal f ->
              DReal (abs_float f)
          | DInt i ->
              DInt (abs i)
          | _ ->
              error_with_context ctx "Type mismatch in abs" )
  | _ ->
      let f1 = to_closure r1 in
      CDynamic
        (fun ctx stack fs ->
          match f1 ctx stack fs with
          | DReal f ->
              DReal (abs_float f)
          | DInt i ->
              DInt (abs i)
          | _ ->
              error_with_context ctx "Type mismatch in abs" )

and compileClipReal (ctx : compile_ctx) (x : exp) (min_v : exp) (max_v : exp) : compiled_result =
  let rx = compileExp ctx x in
  let rmin = compileExp ctx min_v in
  let rmax = compileExp ctx max_v in
  match (rx, rmin, rmax) with
  | CConstant (DReal xv), CConstant (DReal minv), CConstant (DReal maxv) ->
      CConstant (DReal (Stdlib.min (Stdlib.max xv minv) maxv))
  | CVar idx, CConstant (DReal minv), CConstant (DReal maxv) ->
      CDynamic
        (fun ctx stack fs ->
          match Array.unsafe_get stack.stack (fs + idx) with
          | DReal xv ->
              DReal (Stdlib.min (Stdlib.max xv minv) maxv)
          | _ ->
              error_with_context ctx "Type mismatch in clip_real" )
  | _ ->
      let fx = to_closure rx in
      let fmin = to_closure rmin in
      let fmax = to_closure rmax in
      CDynamic
        (fun ctx stack fs ->
          match (fx ctx stack fs, fmin ctx stack fs, fmax ctx stack fs) with
          | DReal xv, DReal minv, DReal maxv ->
              DReal (Stdlib.min (Stdlib.max xv minv) maxv)
          | _ ->
              error_with_context ctx "Type mismatch in clip_real" )

and compileClipInt (ctx : compile_ctx) (x : exp) (min_v : exp) (max_v : exp) : compiled_result =
  let rx = compileExp ctx x in
  let rmin = compileExp ctx min_v in
  let rmax = compileExp ctx max_v in
  match (rx, rmin, rmax) with
  | CConstant (DInt xv), CConstant (DInt minv), CConstant (DInt maxv) ->
      CConstant (DInt (Stdlib.min (Stdlib.max xv minv) maxv))
  | CVar idx, CConstant (DInt minv), CConstant (DInt maxv) ->
      CDynamic
        (fun ctx stack fs ->
          match Array.unsafe_get stack.stack (fs + idx) with
          | DInt xv ->
              DInt (Stdlib.min (Stdlib.max xv minv) maxv)
          | _ ->
              error_with_context ctx "Type mismatch in clip_int" )
  | _ ->
      let fx = to_closure rx in
      let fmin = to_closure rmin in
      let fmax = to_closure rmax in
      CDynamic
        (fun ctx stack fs ->
          match (fx ctx stack fs, fmin ctx stack fs, fmax ctx stack fs) with
          | DInt xv, DInt minv, DInt maxv ->
              DInt (Stdlib.min (Stdlib.max xv minv) maxv)
          | _ ->
              error_with_context ctx "Type mismatch in clip_int" )

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
        (fun ctx stack fs ->
          match Array.unsafe_get stack.stack (fs + idx) with
          | DInt i ->
              DReal (float_of_int i)
          | DInt16 i ->
              DReal (float_of_int i)
          | DBool b ->
              DReal (if b then 1.0 else 0.0)
          | DReal f ->
              DReal f
          | _ ->
              error_with_context ctx "Type mismatch in real conversion" )
  | _ ->
      let f1 = to_closure r1 in
      CDynamic
        (fun ctx stack fs ->
          match f1 ctx stack fs with
          | DInt i ->
              DReal (float_of_int i)
          | DInt16 i ->
              DReal (float_of_int i)
          | DBool b ->
              DReal (if b then 1.0 else 0.0)
          | DReal f ->
              DReal f
          | _ ->
              error_with_context ctx "Type mismatch in real conversion" )

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
        (fun ctx stack fs ->
          match Array.unsafe_get stack.stack (fs + idx) with
          | DReal f ->
              DInt (int_of_float f)
          | DBool b ->
              DInt (if b then 1 else 0)
          | DInt i ->
              DInt i
          | DInt16 i ->
              DInt i
          | _ ->
              error_with_context ctx "Type mismatch in int conversion" )
  | _ ->
      let f1 = to_closure r1 in
      CDynamic
        (fun ctx stack fs ->
          match f1 ctx stack fs with
          | DReal f ->
              DInt (int_of_float f)
          | DBool b ->
              DInt (if b then 1 else 0)
          | DInt i ->
              DInt i
          | DInt16 i ->
              DInt i
          | _ ->
              error_with_context ctx "Type mismatch in int conversion" )

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
        (fun ctx stack fs ->
          match f1 ctx stack fs with
          | DReal f ->
              DInt16 (max (-32768) (min 32767 (int_of_float f)))
          | DBool b ->
              DInt16 (if b then 1 else 0)
          | DInt i ->
              DInt16 (max (-32768) (min 32767 i))
          | DInt16 i ->
              DInt16 i
          | _ ->
              error_with_context ctx "Type mismatch in int16 conversion" )

and compileBoolConversion (ctx : compile_ctx) (a : exp) : compiled_result =
  let r1 = compileExp ctx a in
  match r1 with
  | CConstant (DInt i) ->
      CConstant (DBool (i <> 0))
  | CConstant (DInt16 i) ->
      CConstant (DBool (i <> 0))
  | CConstant (DReal f) ->
      CConstant (DBool (f <> 0.0))
  | CConstant (DBool b) ->
      CConstant (DBool b)
  | _ ->
      let f1 = to_closure r1 in
      CDynamic
        (fun ctx stack fs ->
          match f1 ctx stack fs with
          | DInt i ->
              DBool (i <> 0)
          | DInt16 i ->
              DBool (i <> 0)
          | DReal f ->
              DBool (f <> 0.0)
          | DBool b ->
              DBool b
          | _ ->
              error_with_context ctx "Type mismatch in bool conversion" )

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
        (fun ctx stack fs ->
          match f1 ctx stack fs with
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
              error_with_context ctx "Type mismatch in string conversion" )

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
        (fun ctx stack fs ->
          match f1 ctx stack fs with
          | DReal f ->
              DReal f
          | DInt i ->
              DReal (float_of_int i)
          | _ ->
              error_with_context ctx "Type mismatch in fixed conversion" )

and compileSizeBuiltin (ctx : compile_ctx) (a : exp) : compiled_result =
  let r1 = compileExp ctx a in
  match r1 with
  | CConstant (DArray arr) ->
      CConstant (DInt (Array.length arr))
  | _ ->
      let f1 = to_closure r1 in
      CDynamic
        (fun ctx stack fs ->
          match f1 ctx stack fs with
          | DArray arr ->
              DInt (Array.length arr)
          | _ ->
              error_with_context ctx "Type mismatch in size" )

and compileLengthBuiltin (ctx : compile_ctx) (a : exp) : compiled_result =
  let r1 = compileExp ctx a in
  match r1 with
  | CConstant (DString s) ->
      CConstant (DInt (String.length s))
  | _ ->
      let f1 = to_closure r1 in
      CDynamic
        (fun ctx stack fs ->
          match f1 ctx stack fs with
          | DString s ->
              DInt (String.length s)
          | _ ->
              error_with_context ctx "Type mismatch in length" )

and compileListSizeBuiltin (ctx : compile_ctx) (a : exp) : compiled_result =
  let f1 = to_closure (compileExp ctx a) in
  CDynamic
    (fun ctx stack fs ->
      match f1 ctx stack fs with
      | DArray arr ->
          DInt (Array.length arr)
      | DList list_ref ->
          DInt (CCList.length !list_ref)
      | _ ->
          error_with_context ctx "Type mismatch in list_size" )

and compileListCapacityBuiltin (ctx : compile_ctx) (a : exp) : compiled_result =
  let f1 = to_closure (compileExp ctx a) in
  CDynamic
    (fun ctx stack fs ->
      match f1 ctx stack fs with
      | DArray arr ->
          DInt (Array.length arr)
      | DList list_ref ->
          DInt (CCList.length !list_ref)
      | _ ->
          error_with_context ctx "Type mismatch in list_capacity" )

and compileListAppend (ctx : compile_ctx) (l : exp) (v : exp) : compiled_result =
  let fl = to_closure (compileExp ctx l) in
  let fv = to_closure (compileExp ctx v) in
  CDynamic
    (fun ctx stack fs ->
      match fl ctx stack fs with
      | DList list_ref ->
          list_ref := !list_ref @ [fv ctx stack fs] ;
          DVoid
      | _ ->
          error_with_context ctx "list_append requires list type" )

and compileListInsert (ctx : compile_ctx) (l : exp) (i : exp) (v : exp) : compiled_result =
  let fl = to_closure (compileExp ctx l) in
  let fi = to_closure (compileExp ctx i) in
  let fv = to_closure (compileExp ctx v) in
  CDynamic
    (fun ctx stack fs ->
      match (fl ctx stack fs, fi ctx stack fs) with
      | DList list_ref, DInt index ->
          let before, after = CCList.take_drop index !list_ref in
          list_ref := before @ [fv ctx stack fs] @ after ;
          DVoid
      | _ ->
          error_with_context ctx "list_insert: invalid arguments" )

and compileListRemove (ctx : compile_ctx) (l : exp) (i : exp) : compiled_result =
  let fl = to_closure (compileExp ctx l) in
  let fi = to_closure (compileExp ctx i) in
  CDynamic
    (fun ctx stack fs ->
      match (fl ctx stack fs, fi ctx stack fs) with
      | DList list_ref, DInt index ->
          let before, after = CCList.take_drop index !list_ref in
          list_ref := before @ CCList.drop 1 after ;
          DVoid
      | _ ->
          error_with_context ctx "list_remove: invalid arguments" )

and compileListClear (ctx : compile_ctx) (a : exp) : compiled_result =
  let fl = to_closure (compileExp ctx a) in
  CDynamic
    (fun ctx stack fs ->
      match fl ctx stack fs with
      | DList list_ref ->
          list_ref := [] ;
          DVoid
      | _ ->
          error_with_context ctx "list_clear requires list type" )

and compileListGet (ctx : compile_ctx) (l : exp) (i : exp) : compiled_result =
  let fl = to_closure (compileExp ctx l) in
  let fi = to_closure (compileExp ctx i) in
  CDynamic
    (fun ctx stack fs ->
      match (fl ctx stack fs, fi ctx stack fs) with
      | DList list_ref, DInt index -> (
        match CCList.nth_opt !list_ref index with
        | Some v ->
            v
        | None ->
            error_with_context ctx "list_get: index out of bounds" )
      | _ ->
          error_with_context ctx "list_get: invalid arguments" )

and compileListSet (ctx : compile_ctx) (l : exp) (i : exp) (v : exp) : compiled_result =
  let fl = to_closure (compileExp ctx l) in
  let fi = to_closure (compileExp ctx i) in
  let fv = to_closure (compileExp ctx v) in
  CDynamic
    (fun ctx stack fs ->
      match (fl ctx stack fs, fi ctx stack fs) with
      | DList list_ref, DInt index ->
          let old_list = !list_ref in
          let len = CCList.length old_list in
          if index >= 0 && index < len then
            let before, after = CCList.take_drop index old_list in
            match after with
            | _ :: rest ->
                list_ref := before @ [fv ctx stack fs] @ rest ;
                DVoid
            | [] ->
                error_with_context ctx "list_set: index out of bounds"
          else error_with_context ctx "list_set: index out of bounds"
      | _ ->
          error_with_context ctx "list_set: invalid arguments" )

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
        CDynamic
          (fun ctx stack fs ->
            let new_depth = ctx.depth + 1 in
            if new_depth > ctx.max_depth then error_with_context ctx "Maximum call depth exceeded" ;
            let cfunc = Array.unsafe_get prog.compiled_functions func_idx in
            let new_ctx = {ctx with frames= cfunc.cf_name :: ctx.frames; depth= new_depth} in
            let arg_vals = List.init n_args (fun i -> (Array.unsafe_get fargs i) ctx stack fs) in
            callCompiledFunction new_ctx stack cfunc arg_vals )
    | None ->
        if Set.mem path ctx.external_functions then
          CDynamic (fun ctx _stack _fs -> error_with_context ctx "External evaluations are not possible")
        else (
          Printf.eprintf "Function not found during compilation: %s\n" path ;
          Printf.eprintf "Available regular functions:\n" ;
          Map.iter (fun name idx -> Printf.eprintf "  %s -> %d\n" name idx) ctx.function_names ;
          Printf.eprintf "Available external functions:\n" ;
          Set.iter (fun name -> Printf.eprintf "  %s (external)\n" name) ctx.external_functions ;
          error ("Function not found during compilation: " ^ path) ) )

(* Checks if a function call should be inlined *)
and shouldInline (param_names : string list) (body_exp : exp) (args : exp list) : bool =
  progExpSize body_exp <= 15
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
            fun ctx stack fs ->
              Array.unsafe_set stack.stack (fs + var_idx) (fexp ctx stack fs) ;
              Continue
        | None ->
            let typ = dexp.t in
            fun _ctx stack fs ->
              Array.unsafe_set stack.stack (fs + var_idx) (defaultValue typ) ;
              Continue ) )
  | StmtBind (lexp, exp) ->
      compileBind ctx lexp exp
  | StmtReturn exp ->
      let fexp = to_closure (compileExp ctx exp) in
      fun ctx stack fs -> Return (fexp ctx stack fs)
  | StmtBlock stmts ->
      let fstmts = Array.of_list (CCList.map (compileStmt ctx) stmts) in
      let n = Array.length fstmts in
      fun ctx stack fs ->
        let rec loop (i : int) : exec_result =
          if i >= n then Continue
          else match (Array.unsafe_get fstmts i) ctx stack fs with Continue -> loop (i + 1) | Return v -> Return v
        in
        loop 0
  | StmtIf (cond, then_stmt, else_stmt) -> (
      let rcond = compileExp ctx cond in
      match rcond with
      | CConstant (DBool true) ->
          compileStmt ctx then_stmt
      | CConstant (DBool false) -> (
        match else_stmt with Some else_s -> compileStmt ctx else_s | None -> fun _ctx _stack _fs -> Continue )
      | _ -> (
          let fcond = to_closure rcond in
          let fthen = compileStmt ctx then_stmt in
          match else_stmt with
          | Some else_s -> (
              let felse = compileStmt ctx else_s in
              fun ctx stack fs ->
                match fcond ctx stack fs with
                | DBool true ->
                    fthen ctx stack fs
                | DBool false ->
                    felse ctx stack fs
                | _ ->
                    error_with_context ctx "Invalid condition" )
          | None -> (
              fun ctx stack fs -> match fcond ctx stack fs with DBool true -> fthen ctx stack fs | _ -> Continue ) ) )
  | StmtWhile (cond, body) ->
      let fcond = to_closure (compileExp ctx cond) in
      let fbody = compileStmt ctx body in
      fun ctx stack fs ->
        let rec loop () : exec_result =
          match fcond ctx stack fs with
          | DBool true -> (
            match fbody ctx stack fs with Continue -> loop () | Return v -> Return v )
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
      fun ctx stack fs ->
        let exp_val = fexp ctx stack fs in
        let rec try_cases (i : int) : exec_result =
          if i >= n_cases then match fdefault with Some fd -> fd ctx stack fs | None -> Continue
          else
            let case_exp, case_stmt = Array.unsafe_get fcases i in
            let case_val = case_exp ctx stack fs in
            if evalBinop ctx OpEq exp_val case_val = DBool true then case_stmt ctx stack fs else try_cases (i + 1)
        in
        try_cases 0

(* ---- Bind Compilation ----
   Handles StmtBind with pattern matching for efficient common cases. *)
and compileBind (ctx : compile_ctx) (lexp : lexp) (rhs : exp) : compiled_stmt =
  match lexp.l with
  | LWild ->
      let fexp = to_closure (compileExp ctx rhs) in
      fun ctx stack fs ->
        let _ = fexp ctx stack fs in
        Continue
  | LId name -> (
    match Hashtbl.find_opt ctx.var_to_index name with
    | Some idx ->
        let fexp = to_closure (compileExp ctx rhs) in
        fun ctx stack fs ->
          Array.unsafe_set stack.stack (fs + idx) (fexp ctx stack fs) ;
          Continue
    | None ->
        error ("Variable not found: " ^ name) )
  (* Fused var.member assignment *)
  | LMember ({l= LId var_name; t= var_type; _}, member_name) -> (
    match var_type.t with
    | TStruct descr -> (
        let member_idx = getMemberIndex descr member_name in
        match Hashtbl.find_opt ctx.var_to_index var_name with
        | Some var_idx ->
            let fexp = to_closure (compileExp ctx rhs) in
            fun ctx stack fs ->
              let val_ = fexp ctx stack fs in
              ( match Array.unsafe_get stack.stack (fs + var_idx) with
              | DStruct members ->
                  Array.unsafe_set members member_idx val_
              | _ ->
                  error_with_context ctx "Invalid struct access in fused var-member assignment" ) ;
              Continue
        | None ->
            error ("Variable not found: " ^ var_name) )
    | _ ->
        (* Fall through to general case *)
        compileBindGeneral ctx lexp rhs )
  (* Fused var[index] assignment *)
  | LIndex {e= {l= LId var_name; _}; index= index_exp} -> (
    match Hashtbl.find_opt ctx.var_to_index var_name with
    | Some var_idx ->
        let fi = to_closure (compileExp ctx index_exp) in
        let fexp = to_closure (compileExp ctx rhs) in
        fun ctx stack fs ->
          let val_ = fexp ctx stack fs in
          ( match (Array.unsafe_get stack.stack (fs + var_idx), fi ctx stack fs) with
          | DArray arr, DInt i when i >= 0 && i < Array.length arr ->
              arr.(i) <- val_
          | _ ->
              error_with_context ctx "Invalid array assignment" ) ;
          Continue
    | None ->
        error ("Variable not found: " ^ var_name) )
  (* Fused var.member[index] assignment *)
  | LIndex {e= {l= LMember ({l= LId var_name; t= var_type; _}, member_name); _}; index= index_exp} -> (
    match var_type.t with
    | TStruct descr -> (
        let member_idx = getMemberIndex descr member_name in
        match Hashtbl.find_opt ctx.var_to_index var_name with
        | Some var_idx ->
            let fi = to_closure (compileExp ctx index_exp) in
            let fexp = to_closure (compileExp ctx rhs) in
            fun ctx stack fs ->
              let val_ = fexp ctx stack fs in
              let arr =
                match Array.unsafe_get stack.stack (fs + var_idx) with
                | DStruct members ->
                    Array.unsafe_get members member_idx
                | _ ->
                    error_with_context ctx "Invalid struct access"
              in
              ( match (arr, fi ctx stack fs) with
              | DArray a, DInt i when i >= 0 && i < Array.length a ->
                  a.(i) <- val_
              | _ ->
                  error_with_context ctx "Invalid array assignment" ) ;
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
  fun ctx stack fs ->
    fassign ctx stack fs (fexp ctx stack fs) ;
    Continue

(* Compile an lexp for rvalue reading *)
and compileLexpAsRvalue (ctx : compile_ctx) (lexp : lexp) : compiled_exp =
  match lexp.l with
  | LWild ->
      fun ctx _stack _fs -> error_with_context ctx "Cannot read wildcard"
  | LId name -> (
    match Hashtbl.find_opt ctx.var_to_index name with
    | Some idx ->
        fun _ctx stack fs -> Array.unsafe_get stack.stack (fs + idx)
    | None ->
        error ("Variable not found: " ^ name) )
  | LMember (({t= var_type; _} as e), member_name) -> (
    match var_type.t with
    | TStruct descr -> (
        let member_idx = getMemberIndex descr member_name in
        let fe = compileLexpAsRvalue ctx e in
        fun ctx stack fs ->
          match fe ctx stack fs with
          | DStruct members when member_idx >= 0 && member_idx < Array.length members ->
              members.(member_idx)
          | _ ->
              error_with_context ctx "Invalid struct member access" )
    | _ ->
        error "Member access on non-struct type" )
  | LIndex {e; index} -> (
      let fe = compileLexpAsRvalue ctx e in
      let fi = to_closure (compileExp ctx index) in
      fun ctx stack fs ->
        match (fe ctx stack fs, fi ctx stack fs) with
        | DArray arr, DInt i when i >= 0 && i < Array.length arr ->
            arr.(i)
        | _ ->
            error_with_context ctx "Invalid array access" )
  | LTuple _ ->
      fun ctx _stack _fs -> error_with_context ctx "Cannot read tuple lvalue"

(* Compile an assignment to an lvalue location *)
and compileLexpAssign (ctx : compile_ctx) (lexp : lexp) : call_context -> runtime_stack -> int -> dvalue -> unit =
  match lexp.l with
  | LWild ->
      fun _ctx _stack _fs _val -> ()
  | LId name -> (
    match Hashtbl.find_opt ctx.var_to_index name with
    | Some idx ->
        fun _ctx stack fs val_ -> Array.unsafe_set stack.stack (fs + idx) val_
    | None ->
        error ("Variable not found: " ^ name) )
  | LMember (({t= var_type; _} as e), member_name) -> (
    match var_type.t with
    | TStruct descr -> (
        let member_idx = getMemberIndex descr member_name in
        let fe = compileLexpAsRvalue ctx e in
        fun ctx stack fs val_ ->
          match fe ctx stack fs with
          | DStruct members when member_idx >= 0 && member_idx < Array.length members ->
              members.(member_idx) <- val_
          | _ ->
              error_with_context ctx "Invalid struct member assignment" )
    | _ ->
        error "Member assignment on non-struct type" )
  | LIndex {e; index} -> (
      let fe = compileLexpAsRvalue ctx e in
      let fi = to_closure (compileExp ctx index) in
      fun ctx stack fs val_ ->
        match (fe ctx stack fs, fi ctx stack fs) with
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
                  error_with_context ctx "List index out of bounds for assignment"
            else error_with_context ctx "List index out of bounds"
        | _ ->
            error_with_context ctx "Invalid array or list assignment" )
  | LTuple lexps -> (
      let fassigns = CCList.mapi (fun i lv -> (i, compileLexpAssign ctx lv)) lexps in
      fun ctx stack fs val_ ->
        match val_ with
        | DArray vals when Array.length vals = CCList.length fassigns ->
            CCList.iter (fun (i, fassign) -> fassign ctx stack fs vals.(i)) fassigns
        | _ ->
            error_with_context ctx "Tuple assignment type mismatch" )

(* Execute a compiled function: allocate a stack frame, bind arguments, run the body. *)
and callCompiledFunction (ctx : call_context) (stack : runtime_stack) (cfunc : compiled_func) (args : dvalue list) :
    dvalue =
  let frame_start = stack.sp in
  if stack.sp + cfunc.cf_locals > stack.max_size then error ("Stack overflow in function " ^ cfunc.cf_name) ;
  for i = 0 to cfunc.cf_locals - 1 do
    Array.unsafe_set stack.stack (stack.sp + i) DVoid
  done ;
  CCList.iter2
    (fun param_idx arg_val -> Array.unsafe_set stack.stack (frame_start + param_idx) arg_val)
    cfunc.cf_args args ;
  stack.sp <- stack.sp + cfunc.cf_locals ;
  let result = cfunc.cf_body ctx stack frame_start in
  stack.sp <- stack.sp - cfunc.cf_locals ;
  match result with Continue -> DVoid | Return v -> v

(* Calls a compiled function by index *)
let callCompiledFunctionByIdx (prog : iprog) (ctx : call_context) (stack : runtime_stack) (func_idx : int)
    (args : dvalue list) : dvalue =
  let cfunc = Array.unsafe_get prog.compiled_functions func_idx in
  callCompiledFunction ctx stack cfunc args

(* ---- Program Processing ---- *)

(* Compile a single function definition from Prog into a compiled_func *)
let compileProgramFunction (prog : iprog) (def : function_def) (body : stmt) : compiled_func =
  let ctx =
    { var_to_index= Hashtbl.create 32
    ; next_index= 0
    ; struct_types= prog.struct_types
    ; constant_names= prog.constant_names
    ; function_names= prog.ifunction_names
    ; external_functions= prog.external_functions
    ; prog }
  in
  let param_indices = CCList.map (fun (p : param) -> addVar ctx p.name) def.args in
  let compiled_body = compileStmt ctx body in
  {cf_name= def.name; cf_args= param_indices; cf_locals= ctx.next_index; cf_body= compiled_body}

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
        { var_to_index= Hashtbl.create 4
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
            let temp_ctx = {frames= []; depth= 0; max_depth= 50; sample_rate= None} in
            let temp_stack = createStack 100 in
            f temp_ctx temp_stack 0
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
          if progExpSize ret_exp <= 15 && not (containsProgCall def.name ret_exp) then
            prog.inlinable <- Map.add def.name (param_names, ret_exp) prog.inlinable
      | _ ->
          () )

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
  CCList.iter (processStatement iprog) prog ;
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
  CCList.iter (processStatement iprog) main ;
  (* Look for the new function Main___main_ *)
  let main_func_name = "Main___main_" in
  match Map.find_opt main_func_name iprog.ifunction_names with
  | Some _ -> (
      let initial_ctx = {frames= []; depth= 0; max_depth= 50; sample_rate= args.fs} in
      let stack = createStack 1000 in
      let call_args =
        let alloc_func_name = main_func_name ^ "_type_alloc" in
        match Map.find_opt alloc_func_name iprog.ifunction_names with
        | Some alloc_idx ->
            let state = callCompiledFunctionByIdx iprog initial_ctx stack alloc_idx [] in
            [state]
        | None ->
            []
      in
      match Map.find_opt main_func_name iprog.ifunction_names with
      | Some func_idx ->
          callCompiledFunctionByIdx iprog initial_ctx stack func_idx call_args
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
  CCList.iter (processStatement iprog) main ;
  (* Execute wrapper function *)
  let main_func_name = "Render___main" in
  let initial_ctx = {frames= []; depth= 0; max_depth= 50; sample_rate= args.fs} in
  let stack = createStack 10000 in
  (* Prepare call arguments *)
  let call_args =
    let alloc_func_name = main_func_name ^ "_type_alloc" in
    match Map.find_opt alloc_func_name iprog.ifunction_names with
    | Some alloc_idx ->
        let state = callCompiledFunctionByIdx iprog initial_ctx stack alloc_idx [] in
        [state]
    | None ->
        []
  in
  match Map.find_opt main_func_name iprog.ifunction_names with
  | Some func_idx ->
      let result = callCompiledFunctionByIdx iprog initial_ctx stack func_idx call_args in
      writeResultToWav result params ;
      let end_time = Sys.time () in
      let duration = end_time -. start_time in
      (params.file, duration)
  | None ->
      error "Could not execute render function"

(* External API: call a compiled function by index. *)
let callFunctionEntry (prog : iprog) (stack : runtime_stack) (func_idx : int) (args : dvalue list) : dvalue =
  let initial_ctx = {frames= []; depth= 0; max_depth= 50; sample_rate= None} in
  callCompiledFunctionByIdx prog initial_ctx stack func_idx args
