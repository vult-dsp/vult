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
   Vult Interpreter — Multi-Phase Evaluation Engine
   =================================================

   This module implements a complete interpreter for the Vult DSP language. It transforms
   the Prog AST into an internal IR, optimizes it, and either interprets or closure-compiles
   it for fast evaluation.

   Architecture Overview
   ---------------------

   The interpreter operates as a pipeline of phases:

     Prog AST (from Toprog/Passes)
       |
       v
     Phase 1: AST Transformation  [transformExp / transformStmt / transformFunction]
       Converts Prog.exp/stmt into interpreter IR (iexp/istmt/ilexp).
       - Resolves variable names to stack-frame indices (int)
       - Resolves struct member names to integer offsets
       - Specializes generic arithmetic (e.g., OpAdd) into typed variants
         (IEAddInt, IEAddReal, IEAddInt16) to eliminate runtime type dispatch
       - Inlines known builtin functions (sin, cos, clip, etc.) into dedicated
         iexp nodes (IEBuiltinSin, IEBuiltinClipReal, ...) to avoid function-call overhead
       - Fuses common access patterns: IEVarMember for var.field, ILVarMember for lvalue
       |
       v
     Phase 2a: Constant Evaluation  [evalConstantExpression]
       Evaluates top-level constant declarations at load time.
       Constants that depend on functions or other unevaluated constants
       are stored as lazy (Unevaluated) and resolved on first access.
       |
       v
     Phase 2b: Expression-Only Inlining  [inlineExp / inlineStmt]
       Inlines small, non-recursive functions at call sites when:
       - The function body is a single return statement
       - The body size is below a threshold (currently 100 AST nodes)
       - No recursive/self calls exist
       - Arguments are either used once or are "simple" (vars, constants)
       This reduces function call overhead for small utility functions.
       |
       v
     Phase 2c: Constant Folding  [foldConstantsExp / foldConstantsStmt]
       Bottom-up constant folding on the iexp tree:
       - Folds arithmetic on literal operands: IEAddInt(IEInt 3, IEInt 4) -> IEInt 7
       - Folds comparisons, type conversions, and math builtins on literals
       - Eliminates dead branches: IEIf(IEBool true, t, f) -> t
       - Eliminates identity operations: x + 0 -> x, x * 1 -> x
       |
       v
     Phase 3: Closure Conversion  [compileIexp / compileIstmt / compileProgram]
       Compiles the optimized iexp/istmt tree into OCaml closures for fast execution.
       Each iexp node becomes a compiled_result:
       - CConstant v:  compile-time known value, zero runtime cost
       - CVar idx:     direct stack-frame variable read, enables fused closures
       - CDynamic f:   a closure (call_context -> runtime_stack -> int -> dvalue)

       Key optimizations:
       (a) Constant propagation: literals, pi, eps, and lazy constants resolved at
           compile time are returned as CConstant, avoiding closure allocation entirely.
       (b) CVar fusion: when a builtin's argument is a variable, the variable read is
           fused into the builtin's closure, eliminating one indirect call per evaluation.
           Example: sin(x) with x = IEVar 3 becomes a single closure that reads
           stack[fs+3] and applies sin, rather than two chained closures.
       (c) Constant folding at closure level: operations where all children compile to
           CConstant are folded immediately (e.g., real(3) -> CConstant(DReal 3.0)).
       (d) Dead branch elimination: IEIf/IStmtIf with constant conditions compile
           only the taken branch.
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
   - transformProgram: full pipeline from Prog AST to optimized iprog
   - extendProgram:    incremental addition of new definitions to an existing iprog
   - compileProgram:   Phase 3 closure conversion for all functions
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

(* ---- Interpreter IR: Expressions ----

   iexp is the core expression type of the interpreter's internal representation.
   Unlike the source-level Prog.exp, all names have been resolved to integer indices:
   - Variables are stack-frame offsets (IEVar of int)
   - Struct members are array indices (IEMember of iexp * int)
   - Functions are indices into iprog.ifunctions_array (IECall of int * iexp list)
   - Constants are indices into iprog.iconstants (IEConstant of int)

   Arithmetic and comparisons are specialized by type (IEAddInt vs IEAddReal vs IEAddInt16)
   to eliminate runtime type dispatch — the tag check happens once during Phase 1 transformation,
   not on every evaluation.

   Builtin functions (sin, cos, clip, etc.) have dedicated nodes rather than being represented
   as function calls. This enables:
   - Direct inlining during closure conversion (no function lookup)
   - Type-specific constant folding in Phase 2c
   - CVar fusion in Phase 3 (e.g., IEBuiltinSin(IEVar idx) -> single fused closure)
*)
type iexp =
  | IEUnit
  | IEEmptyValue
  | IEEmptyList of type_ (* Empty list of the given element type *)
  | IEBool of bool
  | IEInt of int
  | IEReal of float
  | IEFixed of float
  | IEString of string
  | IEVar of int (* Variable index in frame *)
  | IEConstant of int (* Global constant index *)
  | IEUnOp of uoperator * iexp
  | IEOp of operator * iexp * iexp
  (* Specialized arithmetic operations *)
  | IEAddInt of iexp * iexp
  | IESubInt of iexp * iexp
  | IEMulInt of iexp * iexp
  | IEDivInt of iexp * iexp
  | IEAddInt16 of iexp * iexp
  | IESubInt16 of iexp * iexp
  | IEMulInt16 of iexp * iexp
  | IEDivInt16 of iexp * iexp
  | IEAddReal of iexp * iexp
  | IESubReal of iexp * iexp
  | IEMulReal of iexp * iexp
  | IEDivReal of iexp * iexp
  (* Specialized comparison operations *)
  | IEEqInt of iexp * iexp
  | IEEqInt16 of iexp * iexp
  | IEEqReal of iexp * iexp
  | IELtInt of iexp * iexp
  | IELtInt16 of iexp * iexp
  | IELtReal of iexp * iexp
  | IEGtInt of iexp * iexp
  | IEGtInt16 of iexp * iexp
  | IEGtReal of iexp * iexp
  (* Inlined built-in functions *)
  | IEBuiltinTanh of iexp
  | IEBuiltinCosh of iexp
  | IEBuiltinSinh of iexp
  | IEBuiltinSin of iexp
  | IEBuiltinCos of iexp
  | IEBuiltinTan of iexp
  | IEBuiltinExp of iexp
  | IEBuiltinLog of iexp
  | IEBuiltinLog10 of iexp
  | IEBuiltinSqrt of iexp
  | IEBuiltinAbs of iexp
  | IEBuiltinFloor of iexp
  | IEBuiltinPow of iexp * iexp
  | IEBuiltinClipReal of iexp * iexp * iexp
  | IEBuiltinClipInt of iexp * iexp * iexp
  (* Constants *)
  | IEBuiltinPi
  | IEBuiltinEps
  | IEBuiltinSamplerate
  (* Random functions *)
  | IEBuiltinRandom
  | IEBuiltinIrandom
  (* Type conversion functions *)
  | IEBuiltinReal of iexp
  | IEBuiltinInt of iexp
  | IEBuiltinInt16 of iexp
  | IEBuiltinBool of iexp
  | IEBuiltinString of iexp
  | IEBuiltinFixed of iexp
  (* Array/string functions *)
  | IEBuiltinSize of iexp
  | IEBuiltinLength of iexp
  (* List functions *)
  | IEBuiltinListSize of iexp
  | IEBuiltinListCapacity of iexp
  | IEBuiltinListAppend of iexp * iexp
  | IEBuiltinListInsert of iexp * iexp * iexp
  | IEBuiltinListRemove of iexp * iexp
  | IEBuiltinListClear of iexp
  | IEBuiltinListReserve of iexp * iexp
  | IEBuiltinListGet of iexp * iexp
  | IEBuiltinListSet of iexp * iexp * iexp
  | IEIndex of iexp * iexp
  | IEArray of iexp list
  | IECall of int * iexp list (* Function index and args *)
  | IECallExt of string * iexp list (* External function name and args *)
  | IEIf of iexp * iexp * iexp
  | IETuple of iexp list
  | IEMember of iexp * int (* Struct and member index *)
  | IEVarMember of int * int (* Fused var[idx].field[member_idx] — avoids intermediate evalIexp *)
  | IERecord of struct_descr * (int * iexp) list (* Type and (member_idx, value) list *)

(* ---- Interpreter IR: Left-Values ----

   ilexp represents assignable locations. Like iexp, all names are resolved to indices.
   ILVarMember is a fused form: instead of ILMember(ILVar idx, member), it directly
   encodes both the variable and member index for a single array lookup at runtime.
*)
and ilexp =
  | ILWild
  | ILVar of int (* Variable index in frame *)
  | ILVarMember of int * int (* Fused var[idx].field[member_idx] — direct struct member write *)
  | ILMember of ilexp * int (* Struct and member index *)
  | ILIndex of ilexp * iexp
  | ILTuple of ilexp list

(* ---- Interpreter IR: Statements ---- *)
and istmt =
  | IStmtDecl of int * type_ * iexp option (* var_index, type, init *)
  | IStmtBind of ilexp * iexp
  | IStmtReturn of iexp
  | IStmtBlock of istmt list
  | IStmtIf of iexp * istmt * istmt option
  | IStmtWhile of iexp * istmt
  | IStmtSwitch of iexp * (iexp * istmt) list * istmt option

(* ---- Interpreter IR: Function Definitions ----

   Each function tracks its parameter indices (iargs), the total number of
   stack slots needed (ilocals — includes both parameters and local variables),
   and the body statement. ilocals determines the frame size allocated on the
   runtime stack for each call.
*)
type ifunc_def =
  { iname: string
  ; iargs: int list (* Parameter indices *)
  ; iret_type: type_
  ; ilocals: int (* Number of local variables *)
  ; ibody: istmt }

(* ---- Lazy Constants ----

   Top-level constants (e.g., `val PI2 = 2.0 * pi;`) are evaluated eagerly when possible
   (Phase 2a). When a constant depends on functions or other unevaluated constants, it is
   stored as Unevaluated with a minimal eval_context capturing the function table and
   constants array. On first access (via evaluateLazyConstant), the expression is evaluated,
   the result cached as Evaluated, and returned. Subsequent accesses hit the cache.
*)
type constant_value =
  | Evaluated of dvalue
  | Unevaluated of iexp * eval_context (* Expression and minimal evaluation context *)

and eval_context =
  { ifunctions_array: ifunc_def array
  ; ifunction_names: int Map.t
  ; iconstants_ref: constant_value array ref (* Reference to allow mutation *) }

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

   After Phase 3, every expression and statement is represented as an OCaml closure.
   The closure signature is: call_context -> runtime_stack -> frame_start -> result

   - compiled_exp: evaluates an expression, returns a dvalue
   - compiled_stmt: executes a statement, returns Continue or Return
   - compiled_func: bundles a compiled function body with its metadata
*)
type compiled_exp = call_context -> runtime_stack -> int -> dvalue

type compiled_stmt = call_context -> runtime_stack -> int -> exec_result

(* compiled_result is the return type of compileIexp. It classifies expressions
   into three categories to enable compile-time optimizations:

   - CConstant v: The expression always evaluates to v. No closure is allocated.
     Examples: literals (IEInt 5), pi, eps, folded arithmetic (3+4=7), evaluated
     lazy constants (IEConstant idx where the constant is already resolved).

   - CVar idx: The expression is a direct variable read from stack[frame_start + idx].
     This is tracked separately from CDynamic because parent nodes can generate
     "fused" closures that inline the array access, eliminating one indirect call.
     Example: sin(x) where x = IEVar 3 compiles to a single closure that reads
     stack[fs+3] and applies sin, instead of two chained closures.

   - CDynamic f: A general runtime closure. Used when the expression depends on
     runtime state in a way that can't be simplified further. *)
type compiled_result = CConstant of dvalue | CVar of int | CDynamic of compiled_exp

type compiled_func = {cf_name: string; cf_args: int list; cf_locals: int; cf_body: compiled_stmt}

(* ---- Program State ----

   iprog holds all state for a compiled program. It is built incrementally:
   extendProgram adds new definitions, optimizeProgram runs Phases 2b-2c,
   and compileProgram runs Phase 3.

   The dual representation of functions (Map for lookup by name, Array for O(1)
   access by index) reflects the two access patterns: name-based during
   transformation, index-based during execution.
*)
type iprog =
  { mutable ifunctions: ifunc_def Map.t (* Name -> definition, for lookup during transformation *)
  ; mutable ifunctions_array: ifunc_def array (* Index -> definition, for O(1) access during execution *)
  ; mutable ifunction_names: int Map.t (* Name -> index mapping *)
  ; mutable iconstants: constant_value array (* Global constants with lazy evaluation *)
  ; mutable iconstants_count: int (* Number of constants currently stored *)
  ; mutable struct_types: struct_descr Map.t (* Struct type definitions *)
  ; mutable constant_names: int Map.t (* Constant name -> index mapping *)
  ; mutable external_functions: Set.t (* External function names *)
  ; mutable compiled_functions: compiled_func array (* Phase 3 output: compiled closures *) }

(* ---- Phase 1: Transformation Context ----

   transform_ctx is used during Phase 1 (transformExp/transformStmt) to resolve
   names to indices. var_to_index is mutable because new variables are added as
   declarations are encountered. The other fields are immutable and shared across
   all functions in the program.
*)
type transform_ctx =
  { var_to_index: (string, int) Hashtbl.t (* Variable name -> stack frame index *)
  ; mutable next_index: int (* Next available stack slot *)
  ; struct_types: struct_descr Map.t (* Struct type definitions *)
  ; constant_names: int Map.t (* Constant name -> index *)
  ; function_names: int Map.t (* Function name -> index *)
  ; external_functions: Set.t (* External function names *) }

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
  { ifunctions= Map.empty
  ; ifunctions_array= [||]
  ; ifunction_names= Map.empty
  ; iconstants= [||]
  ; iconstants_count= 0
  ; compiled_functions= [||]
  ; struct_types= Map.empty
  ; constant_names= Map.empty
  ; external_functions= Set.empty }

(* Adds a function to the iprog, resizing arrays as needed *)
let addFunction (prog : iprog) (name : string) (ifunc : ifunc_def) : unit =
  (* Get the function index - it should already exist since we add it before transforming *)
  let func_idx =
    match Map.find_opt name prog.ifunction_names with
    | Some idx ->
        idx
    | None ->
        (* This shouldn't happen if functions are processed correctly *)
        error ("Function index not found for: " ^ name)
  in
  prog.ifunctions <- Map.add name ifunc prog.ifunctions ;
  (* Resize function array if needed *)
  if func_idx >= Array.length prog.ifunctions_array then (
    let new_size = max (func_idx + 1) (Array.length prog.ifunctions_array * 2) in
    let new_array = Array.make new_size ifunc in
    Array.blit prog.ifunctions_array 0 new_array 0 (Array.length prog.ifunctions_array) ;
    new_array.(func_idx) <- ifunc ;
    prog.ifunctions_array <- new_array )
  else prog.ifunctions_array.(func_idx) <- ifunc

(* Adds a constant to the iprog, resizing array as needed *)
let addConstant (prog : iprog) (const_val : constant_value) : unit =
  let const_idx = prog.iconstants_count in
  (* Resize constants array if needed using doubling strategy *)
  if const_idx >= Array.length prog.iconstants then (
    let new_size = if const_idx = 0 then 16 else Array.length prog.iconstants * 2 in
    let new_array = Array.make new_size (Evaluated DVoid) in
    Array.blit prog.iconstants 0 new_array 0 prog.iconstants_count ;
    new_array.(const_idx) <- const_val ;
    prog.iconstants <- new_array )
  else prog.iconstants.(const_idx) <- const_val ;
  prog.iconstants_count <- prog.iconstants_count + 1

(* Adds a variable to the transformation context and returns its assigned index *)
let addVar (ctx : transform_ctx) (name : string) : int =
  if Hashtbl.mem ctx.var_to_index name then Hashtbl.find ctx.var_to_index name
  else
    let idx = ctx.next_index in
    Hashtbl.add ctx.var_to_index name idx ;
    ctx.next_index <- ctx.next_index + 1 ;
    idx

(* Retrieves the index of a variable from the transformation context *)
let getVarIndex (ctx : transform_ctx) (name : string) : int option = Hashtbl.find_opt ctx.var_to_index name

(* Retrieves the index of a constant from the transformation context *)
let getConstantIndex (ctx : transform_ctx) (name : string) : int option = Map.find_opt name ctx.constant_names

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

(* Converts a binary operator to its string representation *)
let printOperator (op : operator) : string =
  match op with
  | OpAdd ->
      "+"
  | OpSub ->
      "-"
  | OpMul ->
      "*"
  | OpDiv ->
      "/"
  | OpMod ->
      "%"
  | OpEq ->
      "=="
  | OpNe ->
      "!="
  | OpLt ->
      "<"
  | OpGt ->
      ">"
  | OpLe ->
      "<="
  | OpGe ->
      ">="
  | OpLand ->
      "&&"
  | OpLor ->
      "||"
  | OpLsh ->
      "<<"
  | OpRsh ->
      ">>"
  | OpBand ->
      "&"
  | OpBor ->
      "|"
  | OpBxor ->
      "^"

(* Converts a unary operator to its string representation *)
let printUoperator (op : uoperator) : string = match op with UOpNeg -> "-" | UOpNot -> "!"

(* Converts an optimized interpreter expression to its string representation *)
let rec printIexp (ie : iexp) : string =
  match ie with
  | IEUnit ->
      "()"
  | IEEmptyValue ->
      "empty"
  | IEEmptyList _ ->
      "empty_list"
  | IEBool b ->
      string_of_bool b
  | IEInt i ->
      string_of_int i
  | IEReal f ->
      string_of_float f
  | IEFixed f ->
      string_of_float f ^ "f"
  | IEString s ->
      "\"" ^ s ^ "\""
  | IEVar idx ->
      "var[" ^ string_of_int idx ^ "]"
  | IEConstant idx ->
      "const[" ^ string_of_int idx ^ "]"
  | IEUnOp (op, e) ->
      printUoperator op ^ "(" ^ printIexp e ^ ")"
  | IEOp (op, e1, e2) ->
      "(" ^ printIexp e1 ^ " " ^ printOperator op ^ " " ^ printIexp e2 ^ ")"
  (* Specialized arithmetic operations *)
  | IEAddInt (e1, e2) ->
      "(" ^ printIexp e1 ^ " +int " ^ printIexp e2 ^ ")"
  | IESubInt (e1, e2) ->
      "(" ^ printIexp e1 ^ " -int " ^ printIexp e2 ^ ")"
  | IEMulInt (e1, e2) ->
      "(" ^ printIexp e1 ^ " *int " ^ printIexp e2 ^ ")"
  | IEDivInt (e1, e2) ->
      "(" ^ printIexp e1 ^ " /int " ^ printIexp e2 ^ ")"
  | IEAddInt16 (e1, e2) ->
      "(" ^ printIexp e1 ^ " +int16 " ^ printIexp e2 ^ ")"
  | IESubInt16 (e1, e2) ->
      "(" ^ printIexp e1 ^ " -int16 " ^ printIexp e2 ^ ")"
  | IEMulInt16 (e1, e2) ->
      "(" ^ printIexp e1 ^ " *int16 " ^ printIexp e2 ^ ")"
  | IEDivInt16 (e1, e2) ->
      "(" ^ printIexp e1 ^ " /int16 " ^ printIexp e2 ^ ")"
  | IEAddReal (e1, e2) ->
      "(" ^ printIexp e1 ^ " +real " ^ printIexp e2 ^ ")"
  | IESubReal (e1, e2) ->
      "(" ^ printIexp e1 ^ " -real " ^ printIexp e2 ^ ")"
  | IEMulReal (e1, e2) ->
      "(" ^ printIexp e1 ^ " *real " ^ printIexp e2 ^ ")"
  | IEDivReal (e1, e2) ->
      "(" ^ printIexp e1 ^ " /real " ^ printIexp e2 ^ ")"
  (* Specialized comparison operations *)
  | IEEqInt (e1, e2) ->
      "(" ^ printIexp e1 ^ " ==int " ^ printIexp e2 ^ ")"
  | IEEqInt16 (e1, e2) ->
      "(" ^ printIexp e1 ^ " ==int16 " ^ printIexp e2 ^ ")"
  | IEEqReal (e1, e2) ->
      "(" ^ printIexp e1 ^ " ==real " ^ printIexp e2 ^ ")"
  | IELtInt (e1, e2) ->
      "(" ^ printIexp e1 ^ " <int " ^ printIexp e2 ^ ")"
  | IELtInt16 (e1, e2) ->
      "(" ^ printIexp e1 ^ " <int16 " ^ printIexp e2 ^ ")"
  | IELtReal (e1, e2) ->
      "(" ^ printIexp e1 ^ " <real " ^ printIexp e2 ^ ")"
  | IEGtInt (e1, e2) ->
      "(" ^ printIexp e1 ^ " >int " ^ printIexp e2 ^ ")"
  | IEGtInt16 (e1, e2) ->
      "(" ^ printIexp e1 ^ " >int16 " ^ printIexp e2 ^ ")"
  | IEGtReal (e1, e2) ->
      "(" ^ printIexp e1 ^ " >real " ^ printIexp e2 ^ ")"
  (* Inlined built-in functions *)
  | IEBuiltinTanh e ->
      "tanh(" ^ printIexp e ^ ")"
  | IEBuiltinSinh e ->
      "sinh(" ^ printIexp e ^ ")"
  | IEBuiltinSin e ->
      "sin(" ^ printIexp e ^ ")"
  | IEBuiltinCos e ->
      "cos(" ^ printIexp e ^ ")"
  | IEBuiltinCosh e ->
      "cosh(" ^ printIexp e ^ ")"
  | IEBuiltinTan e ->
      "tan(" ^ printIexp e ^ ")"
  | IEBuiltinExp e ->
      "exp(" ^ printIexp e ^ ")"
  | IEBuiltinLog e ->
      "log(" ^ printIexp e ^ ")"
  | IEBuiltinLog10 e ->
      "log10(" ^ printIexp e ^ ")"
  | IEBuiltinSqrt e ->
      "sqrt(" ^ printIexp e ^ ")"
  | IEBuiltinAbs e ->
      "abs(" ^ printIexp e ^ ")"
  | IEBuiltinFloor e ->
      "floor(" ^ printIexp e ^ ")"
  | IEBuiltinPow (e1, e2) ->
      "pow(" ^ printIexp e1 ^ ", " ^ printIexp e2 ^ ")"
  | IEBuiltinClipReal (x, min_v, max_v) ->
      "clip_real(" ^ printIexp x ^ ", " ^ printIexp min_v ^ ", " ^ printIexp max_v ^ ")"
  | IEBuiltinClipInt (x, min_v, max_v) ->
      "clip_int(" ^ printIexp x ^ ", " ^ printIexp min_v ^ ", " ^ printIexp max_v ^ ")"
  (* Constants *)
  | IEBuiltinPi ->
      "pi"
  | IEBuiltinEps ->
      "eps"
  | IEBuiltinSamplerate ->
      "samplerate"
  (* Random functions *)
  | IEBuiltinRandom ->
      "random()"
  | IEBuiltinIrandom ->
      "irandom()"
  (* Type conversion functions *)
  | IEBuiltinReal e ->
      "real(" ^ printIexp e ^ ")"
  | IEBuiltinInt e ->
      "int(" ^ printIexp e ^ ")"
  | IEBuiltinInt16 e ->
      "int16(" ^ printIexp e ^ ")"
  | IEBuiltinBool e ->
      "bool(" ^ printIexp e ^ ")"
  | IEBuiltinString e ->
      "string(" ^ printIexp e ^ ")"
  | IEBuiltinFixed e ->
      "fix16(" ^ printIexp e ^ ")"
  (* Array/string functions *)
  | IEBuiltinSize e ->
      "size(" ^ printIexp e ^ ")"
  | IEBuiltinLength e ->
      "length(" ^ printIexp e ^ ")"
  (* List functions *)
  | IEBuiltinListSize e ->
      "list_size(" ^ printIexp e ^ ")"
  | IEBuiltinListCapacity e ->
      "list_capacity(" ^ printIexp e ^ ")"
  | IEBuiltinListAppend (l, v) ->
      "list_append(" ^ printIexp l ^ ", " ^ printIexp v ^ ")"
  | IEBuiltinListInsert (l, i, v) ->
      "list_insert(" ^ printIexp l ^ ", " ^ printIexp i ^ ", " ^ printIexp v ^ ")"
  | IEBuiltinListRemove (l, i) ->
      "list_remove(" ^ printIexp l ^ ", " ^ printIexp i ^ ")"
  | IEBuiltinListClear e ->
      "list_clear(" ^ printIexp e ^ ")"
  | IEBuiltinListReserve (l, n) ->
      "list_reserve(" ^ printIexp l ^ ", " ^ printIexp n ^ ")"
  | IEBuiltinListGet (l, i) ->
      "list_get(" ^ printIexp l ^ ", " ^ printIexp i ^ ")"
  | IEBuiltinListSet (l, i, v) ->
      "list_set(" ^ printIexp l ^ ", " ^ printIexp i ^ ", " ^ printIexp v ^ ")"
  | IEIndex (arr, idx) ->
      printIexp arr ^ "[" ^ printIexp idx ^ "]"
  | IEArray exprs ->
      "[" ^ String.concat "; " (CCList.map printIexp exprs) ^ "]"
  | IECall (func_idx, args) ->
      "func[" ^ string_of_int func_idx ^ "](" ^ String.concat ", " (CCList.map printIexp args) ^ ")"
  | IECallExt (func_name, args) ->
      "external_" ^ func_name ^ "(" ^ String.concat ", " (CCList.map printIexp args) ^ ")"
  | IEIf (cond, then_e, else_e) ->
      "if " ^ printIexp cond ^ " then " ^ printIexp then_e ^ " else " ^ printIexp else_e
  | IETuple exprs ->
      "(" ^ String.concat ", " (CCList.map printIexp exprs) ^ ")"
  | IEMember (e, idx) ->
      printIexp e ^ ".field[" ^ string_of_int idx ^ "]"
  | IEVarMember (var_idx, member_idx) ->
      "var[" ^ string_of_int var_idx ^ "].field[" ^ string_of_int member_idx ^ "]"
  | IERecord (_, members) ->
      "{" ^ String.concat "; " (CCList.map (fun (idx, e) -> string_of_int idx ^ ":" ^ printIexp e) members) ^ "}"

(* Converts an interpreter left-value expression to its string representation *)
let rec printIlexp (il : ilexp) : string =
  match il with
  | ILWild ->
      "_"
  | ILVar idx ->
      "var[" ^ string_of_int idx ^ "]"
  | ILVarMember (var_idx, member_idx) ->
      "var[" ^ string_of_int var_idx ^ "].field[" ^ string_of_int member_idx ^ "]"
  | ILMember (lv, idx) ->
      printIlexp lv ^ ".field[" ^ string_of_int idx ^ "]"
  | ILIndex (lv, e) ->
      printIlexp lv ^ "[" ^ printIexp e ^ "]"
  | ILTuple lvs ->
      "(" ^ String.concat ", " (CCList.map printIlexp lvs) ^ ")"

(* Converts an interpreter statement to its string representation *)
let rec printIstmt (is : istmt) : string =
  match is with
  | IStmtDecl (idx, typ, None) ->
      "var[" ^ string_of_int idx ^ "] : " ^ Pla.print (Prog.Print.print_type_ typ)
  | IStmtDecl (idx, typ, Some init) ->
      "var[" ^ string_of_int idx ^ "] : " ^ Pla.print (Prog.Print.print_type_ typ) ^ " = " ^ printIexp init
  | IStmtBind (lv, e) ->
      printIlexp lv ^ " = " ^ printIexp e
  | IStmtReturn e ->
      "return " ^ printIexp e
  | IStmtBlock stmts ->
      "{\n" ^ String.concat ";\n" (CCList.map printIstmt stmts) ^ "\n}"
  | IStmtIf (cond, then_s, None) ->
      "if " ^ printIexp cond ^ " " ^ printIstmt then_s
  | IStmtIf (cond, then_s, Some else_s) ->
      "if " ^ printIexp cond ^ " " ^ printIstmt then_s ^ " else " ^ printIstmt else_s
  | IStmtWhile (cond, body) ->
      "while " ^ printIexp cond ^ " " ^ printIstmt body
  | IStmtSwitch (e, cases, default) ->
      let case_strs = CCList.map (fun (pattern, stmt) -> printIexp pattern ^ " -> " ^ printIstmt stmt) cases in
      let default_str = match default with None -> "" | Some s -> " | _ -> " ^ printIstmt s in
      "match " ^ printIexp e ^ " with " ^ String.concat " | " case_strs ^ default_str

(* Converts an interpreter function definition to its string representation *)
let printIfuncDef (fd : ifunc_def) : string =
  "function " ^ fd.iname ^ "("
  ^ String.concat ", " (CCList.map string_of_int fd.iargs)
  ^ ") : "
  ^ Pla.print (Prog.Print.print_type_ fd.iret_type)
  ^ " [locals:" ^ string_of_int fd.ilocals ^ "] "
  ^ Pla.print (Pla.indent (Pla.string (printIstmt fd.ibody)))

(* Converts an interpreter program to its string representation *)
let printIprog (prog : iprog) : string =
  let func_strs = Map.fold (fun _name fd acc -> printIfuncDef fd :: acc) prog.ifunctions [] in
  String.concat "\n\n" func_strs

(* Determines if a type represents an integer value *)
let isIntType (typ : type_) : bool = match typ.t with TInt | TInt16 -> true | _ -> false

(* Determines if a type represents a real/floating-point value *)
let isRealType (typ : type_) : bool = match typ.t with TReal | TFix16 -> true | _ -> false

(* Determines if a type represents a 16-bit integer value *)
let isInt16Type (typ : type_) : bool = match typ.t with TInt16 -> true | _ -> false

(* ---- Phase 1: AST Transformation ----

   transformExp converts Prog.exp (the typed AST from the compiler frontend) into
   iexp (the interpreter's internal representation). This is where:

   - Variable names are resolved to stack-frame indices via ctx.var_to_index
   - Arithmetic operators are specialized by type: OpAdd on TReal becomes IEAddReal,
     on TInt becomes IEAddInt, etc. This moves the type dispatch from runtime to
     compile time.
   - Known builtin functions (sin, cos, tanh, clip, abs, etc.) are recognized by name
     and converted to dedicated iexp nodes (IEBuiltinSin, IEBuiltinClipReal, ...)
   - Common access patterns are fused: a member access on a variable (var.field)
     becomes IEVarMember(var_idx, field_idx) instead of IEMember(IEVar var_idx, field_idx)
   - External function calls are separated from internal calls (IECallExt vs IECall)
*)
let rec transformExp (ctx : transform_ctx) (exp : exp) : iexp =
  match exp.e with
  | EUnit ->
      IEUnit
  | EEmptyValue -> (
    (* Check if the empty value is for a list type *)
    match exp.t.t with
    | TList _ ->
        IEEmptyList exp.t
    | _ ->
        IEEmptyValue )
  | EBool b ->
      IEBool b
  | EInt i ->
      IEInt i
  | EReal f ->
      IEReal f
  | EFixed f ->
      IEFixed f
  | EString s ->
      IEString s
  | EId name -> (
    match getVarIndex ctx name with
    | Some var_idx ->
        IEVar var_idx
    | None -> (
      match getConstantIndex ctx name with
      | Some const_idx ->
          IEConstant const_idx
      | None ->
          error ("Variable or constant not found: " ^ name) ) )
  | EUnOp (op, e) ->
      IEUnOp (op, transformExp ctx e)
  | EOp (op, e1, e2) -> (
      (* Specialize arithmetic operations based on types *)
      let te1 = transformExp ctx e1 in
      let te2 = transformExp ctx e2 in
      match op with
      | OpAdd when isInt16Type e1.t && isInt16Type e2.t ->
          IEAddInt16 (te1, te2)
      | OpAdd when isIntType e1.t && isIntType e2.t ->
          IEAddInt (te1, te2)
      | OpAdd when isRealType e1.t || isRealType e2.t ->
          IEAddReal (te1, te2)
      | OpSub when isInt16Type e1.t && isInt16Type e2.t ->
          IESubInt16 (te1, te2)
      | OpSub when isIntType e1.t && isIntType e2.t ->
          IESubInt (te1, te2)
      | OpSub when isRealType e1.t || isRealType e2.t ->
          IESubReal (te1, te2)
      | OpMul when isInt16Type e1.t && isInt16Type e2.t ->
          IEMulInt16 (te1, te2)
      | OpMul when isIntType e1.t && isIntType e2.t ->
          IEMulInt (te1, te2)
      | OpMul when isRealType e1.t || isRealType e2.t ->
          IEMulReal (te1, te2)
      | OpDiv when isInt16Type e1.t && isInt16Type e2.t ->
          IEDivInt16 (te1, te2)
      | OpDiv when isIntType e1.t && isIntType e2.t ->
          IEDivInt (te1, te2)
      | OpDiv when isRealType e1.t || isRealType e2.t ->
          IEDivReal (te1, te2)
      | OpEq when isInt16Type e1.t && isInt16Type e2.t ->
          IEEqInt16 (te1, te2)
      | OpEq when isIntType e1.t && isIntType e2.t ->
          IEEqInt (te1, te2)
      | OpEq when isRealType e1.t || isRealType e2.t ->
          IEEqReal (te1, te2)
      | OpLt when isInt16Type e1.t && isInt16Type e2.t ->
          IELtInt16 (te1, te2)
      | OpLt when isIntType e1.t && isIntType e2.t ->
          IELtInt (te1, te2)
      | OpLt when isRealType e1.t || isRealType e2.t ->
          IELtReal (te1, te2)
      | OpGt when isInt16Type e1.t && isInt16Type e2.t ->
          IEGtInt16 (te1, te2)
      | OpGt when isIntType e1.t && isIntType e2.t ->
          IEGtInt (te1, te2)
      | OpGt when isRealType e1.t || isRealType e2.t ->
          IEGtReal (te1, te2)
      | _ ->
          IEOp (op, te1, te2)
      (* Fall back to generic for other ops *) )
  | EIndex {e; index} ->
      IEIndex (transformExp ctx e, transformExp ctx index)
  | EArray elems ->
      IEArray (CCList.map (transformExp ctx) elems)
  | ECall {path; args} -> (
      let args' = CCList.map (transformExp ctx) args in
      (* Inline built-in functions for performance *)
      match (path, args') with
      (* Math functions *)
      | "tanh", [arg] ->
          IEBuiltinTanh arg
      | "cosh", [arg] ->
          IEBuiltinCosh arg
      | "sinh", [arg] ->
          IEBuiltinSinh arg
      | "sin", [arg] ->
          IEBuiltinSin arg
      | "cos", [arg] ->
          IEBuiltinCos arg
      | "tan", [arg] ->
          IEBuiltinTan arg
      | "exp", [arg] ->
          IEBuiltinExp arg
      | "log", [arg] ->
          IEBuiltinLog arg
      | "log10", [arg] ->
          IEBuiltinLog10 arg
      | "sqrt", [arg] ->
          IEBuiltinSqrt arg
      | "abs", [arg] ->
          IEBuiltinAbs arg
      | "floor", [arg] ->
          IEBuiltinFloor arg
      | "pow", [arg1; arg2] ->
          IEBuiltinPow (arg1, arg2)
      | "clip", [x; min_v; max_v] when isRealType exp.t ->
          IEBuiltinClipReal (x, min_v, max_v)
      | "clip", [x; min_v; max_v] when isIntType exp.t ->
          IEBuiltinClipInt (x, min_v, max_v)
      (* Constants *)
      | "pi", [] ->
          IEBuiltinPi
      | "eps", [] ->
          IEBuiltinEps
      | "samplerate", [] ->
          IEBuiltinSamplerate
      (* Random functions *)
      | "random", [] ->
          IEBuiltinRandom
      | "irandom", [] ->
          IEBuiltinIrandom
      (* Type conversion functions *)
      | "real", [arg] ->
          IEBuiltinReal arg
      | "int", [arg] ->
          IEBuiltinInt arg
      | "int16", [arg] ->
          IEBuiltinInt16 arg
      | "bool", [arg] ->
          IEBuiltinBool arg
      | "string", [arg] ->
          IEBuiltinString arg
      | "fix16", [arg] ->
          IEBuiltinFixed arg
      (* Array/string functions *)
      | "size", [arg] ->
          IEBuiltinSize arg
      | "length", [arg] ->
          IEBuiltinLength arg
      (* List functions *)
      | "list_size", [arg] ->
          IEBuiltinListSize arg
      | "list_capacity", [arg] ->
          IEBuiltinListCapacity arg
      | "list_append", [l; v] ->
          IEBuiltinListAppend (l, v)
      | "list_insert", [l; i; v] ->
          IEBuiltinListInsert (l, i, v)
      | "list_remove", [l; i] ->
          IEBuiltinListRemove (l, i)
      | "list_clear", [arg] ->
          IEBuiltinListClear arg
      | "list_reserve", [l; n] ->
          IEBuiltinListReserve (l, n)
      | "list_get", [l; i] ->
          IEBuiltinListGet (l, i)
      | "list_set", [l; i; v] ->
          IEBuiltinListSet (l, i, v)
      (* External runtime functions *)
      | "push_block_header", args ->
          IECallExt ("push_block_header", args)
      | "push_int", args ->
          IECallExt ("push_int", args)
      | "push_float", args ->
          IECallExt ("push_float", args)
      | "update_size", args ->
          IECallExt ("update_size", args)
      | "push_array", args ->
          IECallExt ("push_array", args)
      | "push_string", args ->
          IECallExt ("push_string", args)
      | "serialize_type_descr", args ->
          IECallExt ("serialize_type_descr", args)
      | "search_field_name", args ->
          IECallExt ("search_field_name", args)
      | "deserialize_int", args ->
          IECallExt ("deserialize_int", args)
      | "deserialize_float", args ->
          IECallExt ("deserialize_float", args)
      | "deserialize_bool", args ->
          IECallExt ("deserialize_bool", args)
      | "deserialize_string", args ->
          IECallExt ("deserialize_string", args)
      | "search_type_description", args ->
          IECallExt ("search_type_description", args)
      | "first_array_element", args ->
          IECallExt ("first_array_element", args)
      | "get_array_count", args ->
          IECallExt ("get_array_count", args)
      | "next_object", args ->
          IECallExt ("next_object", args)
      (* Fall back to regular call for non-builtins *)
      | _ -> (
        match Map.find_opt path ctx.function_names with
        | Some func_idx ->
            IECall (func_idx, args')
        | None ->
            if
              (* Check if it's an external function *)
              Set.mem path ctx.external_functions
            then IECallExt (path, args')
            else (
              (* Debug: show available functions when lookup fails *)
              Printf.eprintf "Function not found during transformation: %s\n" path ;
              Printf.eprintf "Available regular functions:\n" ;
              Map.iter (fun name idx -> Printf.eprintf "  %s -> %d\n" name idx) ctx.function_names ;
              Printf.eprintf "Available external functions:\n" ;
              Set.iter (fun name -> Printf.eprintf "  %s (external)\n" name) ctx.external_functions ;
              error ("Function not found during transformation: " ^ path) ) ) )
  | EIf {cond; then_; else_} ->
      IEIf (transformExp ctx cond, transformExp ctx then_, transformExp ctx else_)
  | ETuple elems ->
      IETuple (CCList.map (transformExp ctx) elems)
  | EMember (e, member_name) -> (
    match e.t.t with
    | TStruct descr -> (
        let member_idx = getMemberIndex descr member_name in
        let inner = transformExp ctx e in
        match inner with IEVar var_idx -> IEVarMember (var_idx, member_idx) | _ -> IEMember (inner, member_idx) )
    | _ ->
        error "Member access on non-struct type" )
  | ETMember (e, idx) -> (
      let inner = transformExp ctx e in
      match inner with IEVar var_idx -> IEVarMember (var_idx, idx) | _ -> IEMember (inner, idx) )
  | ERecord {path; elems} -> (
    match Map.find_opt path ctx.struct_types with
    | Some descr ->
        let elems' =
          CCList.map
            (fun (name, exp) ->
              let idx = getMemberIndex descr name in
              (idx, transformExp ctx exp) )
            elems
        in
        IERecord (descr, elems')
    | None ->
        error ("Unknown struct type: " ^ path) )

(* Transforms a Prog.lexp (assignable location) into ilexp.
   Fuses var.member patterns into ILVarMember for direct struct field writes. *)
and transformLexp (ctx : transform_ctx) (lexp : lexp) : ilexp =
  match lexp.l with
  | LWild ->
      ILWild
  | LId name -> (
    match getVarIndex ctx name with
    | Some var_idx ->
        ILVar var_idx
    | None ->
        error ("Variable not found in left-value: " ^ name) )
  | LMember (e, member_name) -> (
    match e.t.t with
    | TStruct descr -> (
        let member_idx = getMemberIndex descr member_name in
        let inner = transformLexp ctx e in
        match inner with ILVar var_idx -> ILVarMember (var_idx, member_idx) | _ -> ILMember (inner, member_idx) )
    | _ ->
        error "Member access on non-struct type" )
  | LIndex {e; index} ->
      ILIndex (transformLexp ctx e, transformExp ctx index)
  | LTuple lexps ->
      ILTuple (CCList.map (transformLexp ctx) lexps)

(* Transforms a Prog.stmt into istmt. Variable declarations allocate new stack slots. *)
and transformStmt (ctx : transform_ctx) (stmt : stmt) : istmt =
  match stmt.s with
  | StmtDecl (dexp, init_exp) -> (
    match dexp.d with
    | DId (name, _) ->
        let var_idx = addVar ctx name in
        let init_exp' = Option.map (transformExp ctx) init_exp in
        IStmtDecl (var_idx, dexp.t, init_exp') )
  | StmtBind (lexp, exp) ->
      IStmtBind (transformLexp ctx lexp, transformExp ctx exp)
  | StmtReturn exp ->
      IStmtReturn (transformExp ctx exp)
  | StmtBlock stmts ->
      IStmtBlock (CCList.map (transformStmt ctx) stmts)
  | StmtIf (cond, then_stmt, else_stmt) ->
      let cond' = transformExp ctx cond in
      let then_stmt' = transformStmt ctx then_stmt in
      let else_stmt' = Option.map (transformStmt ctx) else_stmt in
      IStmtIf (cond', then_stmt', else_stmt')
  | StmtWhile (cond, body) ->
      IStmtWhile (transformExp ctx cond, transformStmt ctx body)
  | StmtSwitch (exp, cases, default) ->
      let exp' = transformExp ctx exp in
      let cases' =
        CCList.map (fun (case_exp, case_stmt) -> (transformExp ctx case_exp, transformStmt ctx case_stmt)) cases
      in
      let default' = Option.map (transformStmt ctx) default in
      IStmtSwitch (exp', cases', default')

(* Transforms a single function definition from original Prog to interpreter AST *)
let transformFunction (global_types : struct_descr Map.t) (constant_names : int Map.t) (function_names : int Map.t)
    (external_functions : Set.t) (def : function_def) (body : stmt) : ifunc_def =
  (* Context creation timing *)
  let ctx =
    { var_to_index= Hashtbl.create 32
    ; next_index= 0
    ; struct_types= global_types
    ; constant_names
    ; function_names
    ; external_functions }
  in
  let param_indices = CCList.map (fun (param : param) -> addVar ctx param.name) def.args in
  let body' = transformStmt ctx body in
  (* Get return type *)
  let ret_type = snd def.t in
  {iname= def.name; iargs= param_indices; iret_type= ret_type; ilocals= ctx.next_index; ibody= body'}

(* ---- Phase 2a: Compile-Time Constant Evaluation ----

   Evaluates constant expressions at program load time (before any function runs).
   This is called from transformStatement for top-level `val` declarations.
   It operates on the iexp tree directly (not closures) and supports a limited
   subset: literals, arithmetic, comparisons, builtins, arrays, tuples, records,
   and references to previously-evaluated constants. If evaluation fails (e.g.,
   the expression references a function), the constant falls back to lazy evaluation
   via evaluateLazyConstant.
*)
let rec evalConstantExpression (constants : dvalue array) (exp : iexp) : dvalue =
  match exp with
  | IEUnit ->
      DVoid
  | IEEmptyValue ->
      DVoid
  | IEEmptyList _ ->
      DList (ref [])
  | IEBool b ->
      DBool b
  | IEInt i ->
      DInt i
  | IEReal f ->
      DReal f
  | IEFixed f ->
      DReal f
  | IEString s ->
      DString s
  | IEConstant idx ->
      if idx >= 0 && idx < Array.length constants then constants.(idx)
      else error ("Constant index out of bounds during evaluation: " ^ string_of_int idx)
  | IEVar _ ->
      error "Variables not allowed in constant expressions"
  (* Handle arithmetic operations for constants *)
  | IEAddInt (e1, e2) -> (
    match (evalConstantExpression constants e1, evalConstantExpression constants e2) with
    | DInt a, DInt b ->
        DInt (a + b)
    | _ ->
        error "Type mismatch in constant integer addition" )
  | IEAddInt16 (e1, e2) -> (
    match (evalConstantExpression constants e1, evalConstantExpression constants e2) with
    | DInt16 a, DInt16 b ->
        let result = a + b in
        let clamped = max (-32768) (min 32767 result) in
        DInt16 clamped
    | _ ->
        error "Type mismatch in constant int16 addition" )
  | IEAddReal (e1, e2) -> (
    match (evalConstantExpression constants e1, evalConstantExpression constants e2) with
    | DReal a, DReal b ->
        DReal (a +. b)
    | _ ->
        error "Type mismatch in constant real addition" )
  (* Subtraction *)
  | IESubInt (e1, e2) -> (
    match (evalConstantExpression constants e1, evalConstantExpression constants e2) with
    | DInt a, DInt b ->
        DInt (a - b)
    | _ ->
        error "Type mismatch in constant integer subtraction" )
  | IESubInt16 (e1, e2) -> (
    match (evalConstantExpression constants e1, evalConstantExpression constants e2) with
    | DInt16 a, DInt16 b ->
        let result = a - b in
        let clamped = max (-32768) (min 32767 result) in
        DInt16 clamped
    | _ ->
        error "Type mismatch in constant int16 subtraction" )
  | IESubReal (e1, e2) -> (
    match (evalConstantExpression constants e1, evalConstantExpression constants e2) with
    | DReal a, DReal b ->
        DReal (a -. b)
    | _ ->
        error "Type mismatch in constant real subtraction" )
  (* Multiplication *)
  | IEMulInt (e1, e2) -> (
    match (evalConstantExpression constants e1, evalConstantExpression constants e2) with
    | DInt a, DInt b ->
        DInt (a * b)
    | _ ->
        error "Type mismatch in constant integer multiplication" )
  | IEMulInt16 (e1, e2) -> (
    match (evalConstantExpression constants e1, evalConstantExpression constants e2) with
    | DInt16 a, DInt16 b ->
        let result = a * b in
        let clamped = max (-32768) (min 32767 result) in
        DInt16 clamped
    | _ ->
        error "Type mismatch in constant int16 multiplication" )
  | IEMulReal (e1, e2) -> (
    match (evalConstantExpression constants e1, evalConstantExpression constants e2) with
    | DReal a, DReal b ->
        DReal (a *. b)
    | _ ->
        error "Type mismatch in constant real multiplication" )
  (* Division *)
  | IEDivInt (e1, e2) -> (
    match (evalConstantExpression constants e1, evalConstantExpression constants e2) with
    | DInt a, DInt b ->
        DInt (a / b)
    | _ ->
        error "Type mismatch in constant integer division" )
  | IEDivInt16 (e1, e2) -> (
    match (evalConstantExpression constants e1, evalConstantExpression constants e2) with
    | DInt16 a, DInt16 b ->
        let result = a / b in
        let clamped = max (-32768) (min 32767 result) in
        DInt16 clamped
    | _ ->
        error "Type mismatch in constant int16 division" )
  | IEDivReal (e1, e2) -> (
    match (evalConstantExpression constants e1, evalConstantExpression constants e2) with
    | DReal a, DReal b ->
        DReal (a /. b)
    | _ ->
        error "Type mismatch in constant real division" )
  (* Negation *)
  | IEUnOp (UOpNeg, e) -> (
    match evalConstantExpression constants e with
    | DInt i ->
        DInt (-i)
    | DInt16 i ->
        DInt16 (max (-32768) (min 32767 (-i)))
    | DReal f ->
        DReal (-.f)
    | _ ->
        error "Type mismatch in constant negation" )
  (* Boolean not *)
  | IEUnOp (UOpNot, e) -> (
    match evalConstantExpression constants e with
    | DBool b ->
        DBool (not b)
    | DInt i ->
        DBool (i = 0)
    | DInt16 i ->
        DBool (i = 0)
    | _ ->
        error "Type mismatch in constant boolean not" )
  (* Bit shifts *)
  | IEOp (OpLsh, e1, e2) -> (
    match (evalConstantExpression constants e1, evalConstantExpression constants e2) with
    | DInt a, DInt b ->
        DInt (a lsl b)
    | DInt16 a, DInt16 b ->
        DInt16 (max (-32768) (min 32767 (a lsl b)))
    | _ ->
        error "Type mismatch in constant left shift" )
  | IEOp (OpRsh, e1, e2) -> (
    match (evalConstantExpression constants e1, evalConstantExpression constants e2) with
    | DInt a, DInt b ->
        DInt (a lsr b)
    | DInt16 a, DInt16 b ->
        DInt16 (a lsr b)
    | _ ->
        error "Type mismatch in constant right shift" )
  | IEArray elems ->
      let values = Array.of_list (CCList.map (evalConstantExpression constants) elems) in
      DArray values
  | IERecord (descr, elems) ->
      let member_vals = Array.make (CCList.length descr.members) DVoid in
      CCList.iter
        (fun (idx, exp) ->
          let val_ = evalConstantExpression constants exp in
          member_vals.(idx) <- val_ )
        elems ;
      DStruct member_vals
  (* For now, other expressions are not supported in constants *)
  | _ ->
      error "Unsupported expression in constant declaration"

(* Transforms a single top-level statement incrementally *)
let transformStatement (prog : iprog) (stmt : top_stmt) : unit =
  match stmt.top with
  | TopType descr ->
      prog.struct_types <- Map.add descr.path descr prog.struct_types
  | TopConstant (name, _, _, exp, _) -> (
      let const_idx = prog.iconstants_count in
      prog.constant_names <- Map.add name const_idx prog.constant_names ;
      (* Create context for transforming the constant expression *)
      let ctx =
        { var_to_index= Hashtbl.create 32
        ; next_index= 0
        ; struct_types= prog.struct_types
        ; constant_names= prog.constant_names
        ; function_names= prog.ifunction_names
        ; external_functions= prog.external_functions }
      in
      let iexp = transformExp ctx exp in
      (* Try to evaluate immediately, fall back to lazy evaluation *)
      try
        let value =
          evalConstantExpression
            (Array.map
               (function Evaluated v -> v | Unevaluated _ -> DVoid)
               (Array.sub prog.iconstants 0 prog.iconstants_count) )
            iexp
        in
        addConstant prog (Evaluated value)
      with _ ->
        (* Store as unevaluated for lazy evaluation later *)
        let eval_ctx =
          { ifunctions_array= prog.ifunctions_array
          ; ifunction_names= prog.ifunction_names
          ; iconstants_ref= ref prog.iconstants }
        in
        addConstant prog (Unevaluated (iexp, eval_ctx)) )
  | TopFunction (def, body) ->
      (* If not already registered (e.g. by the first pass in extendProgram), assign an index *)
      if not (Map.mem def.name prog.ifunction_names) then begin
        let func_idx = Map.cardinal prog.ifunction_names in
        prog.ifunction_names <- Map.add def.name func_idx prog.ifunction_names
      end ;
      (* Now transform the function body with the updated function mapping *)
      let ifunc =
        transformFunction prog.struct_types prog.constant_names prog.ifunction_names prog.external_functions def body
      in
      addFunction prog def.name ifunc
  | TopExternal (def, _) ->
      prog.external_functions <- Set.add def.name prog.external_functions
  | TopAlias _ ->
      () (* Type aliases don't need special handling in the interpreter *)

(* Incrementally add new top-level definitions to an existing iprog.
   Two-pass approach:
   1. First pass: register all function names (so constants can forward-reference functions)
   2. Second pass: transform function bodies, constants, types, and externals
   After both passes, updates lazy constant contexts with the completed function array. *)
let extendProgram iprog (prog : top_stmt list) =
  (* First pass: register all function names so constants can reference functions defined later *)
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
  (* Second pass: process all statements (function bodies, constants, types, etc.) *)
  CCList.iter (transformStatement iprog) prog ;
  (* Update lazy evaluation contexts with the completed function array *)
  for idx = 0 to iprog.iconstants_count - 1 do
    match iprog.iconstants.(idx) with
    | Unevaluated (iexp, eval_ctx) ->
        let updated_ctx = {eval_ctx with ifunctions_array= iprog.ifunctions_array} in
        iprog.iconstants.(idx) <- Unevaluated (iexp, updated_ctx)
    | Evaluated _ ->
        ()
  done ;
  iprog

(* ---- Phase 2b: Expression-Only Inlining ----

   This phase inlines small function calls at their call sites. It targets functions
   whose body is a single `return <expr>` statement (expression-only functions), which
   are common in Vult for utility computations like `saturate`, `soft_clip`, etc.

   Inlining criteria (all must hold):
   - Function body is IStmtReturn(expr) (single expression, no side effects)
   - Body size < 100 AST nodes (measured by iexpSize)
   - No recursive calls to the same function (checked by containsCall)
   - Each argument is either used at most once, or is "simple" (IEVar, literal, constant)
     to avoid duplicating expensive computations

   The substitution (substituteVars) replaces parameter references (IEVar idx matching
   a parameter index) with the corresponding argument expression. After inlining,
   the result is recursively processed to enable cascading inlines.

   This phase runs before constant folding (Phase 2c) so that inlined expressions
   can be further simplified.
*)

(* Counts AST nodes in an iexp tree — used to decide if a function is small enough to inline *)
let rec iexpSize (e : iexp) : int =
  match e with
  | IEUnit
  | IEEmptyValue
  | IEEmptyList _
  | IEBool _
  | IEInt _
  | IEReal _
  | IEFixed _
  | IEString _
  | IEVar _
  | IEConstant _
  | IEBuiltinPi
  | IEBuiltinEps
  | IEBuiltinSamplerate
  | IEBuiltinRandom
  | IEBuiltinIrandom
  | IEVarMember (_, _) ->
      1
  | IEUnOp (_, e1)
  | IEBuiltinTanh e1
  | IEBuiltinCosh e1
  | IEBuiltinSinh e1
  | IEBuiltinSin e1
  | IEBuiltinCos e1
  | IEBuiltinTan e1
  | IEBuiltinExp e1
  | IEBuiltinLog e1
  | IEBuiltinLog10 e1
  | IEBuiltinSqrt e1
  | IEBuiltinAbs e1
  | IEBuiltinFloor e1
  | IEBuiltinReal e1
  | IEBuiltinInt e1
  | IEBuiltinInt16 e1
  | IEBuiltinBool e1
  | IEBuiltinString e1
  | IEBuiltinFixed e1
  | IEBuiltinSize e1
  | IEBuiltinLength e1
  | IEBuiltinListSize e1
  | IEBuiltinListCapacity e1
  | IEBuiltinListClear e1 ->
      1 + iexpSize e1
  | IEOp (_, e1, e2)
  | IEAddInt (e1, e2)
  | IESubInt (e1, e2)
  | IEMulInt (e1, e2)
  | IEDivInt (e1, e2)
  | IEAddInt16 (e1, e2)
  | IESubInt16 (e1, e2)
  | IEMulInt16 (e1, e2)
  | IEDivInt16 (e1, e2)
  | IEAddReal (e1, e2)
  | IESubReal (e1, e2)
  | IEMulReal (e1, e2)
  | IEDivReal (e1, e2)
  | IEEqInt (e1, e2)
  | IEEqInt16 (e1, e2)
  | IEEqReal (e1, e2)
  | IELtInt (e1, e2)
  | IELtInt16 (e1, e2)
  | IELtReal (e1, e2)
  | IEGtInt (e1, e2)
  | IEGtInt16 (e1, e2)
  | IEGtReal (e1, e2)
  | IEBuiltinPow (e1, e2)
  | IEBuiltinListAppend (e1, e2)
  | IEBuiltinListRemove (e1, e2)
  | IEBuiltinListReserve (e1, e2)
  | IEBuiltinListGet (e1, e2)
  | IEIndex (e1, e2) ->
      1 + iexpSize e1 + iexpSize e2
  | IEBuiltinClipReal (e1, e2, e3)
  | IEBuiltinClipInt (e1, e2, e3)
  | IEBuiltinListInsert (e1, e2, e3)
  | IEBuiltinListSet (e1, e2, e3) ->
      1 + iexpSize e1 + iexpSize e2 + iexpSize e3
  | IEIf (c, t, f) ->
      1 + iexpSize c + iexpSize t + iexpSize f
  | IEMember (e1, _) ->
      1 + iexpSize e1
  | IECall (_, args) | IECallExt (_, args) | IEArray args | IETuple args ->
      1 + CCList.fold_left (fun acc a -> acc + iexpSize a) 0 args
  | IERecord (_, elems) ->
      1 + CCList.fold_left (fun acc (_, e1) -> acc + iexpSize e1) 0 elems

(* Checks if an expression is simple (cheap to duplicate) *)
let isSimpleExp (e : iexp) : bool =
  match e with
  | IEVar _
  | IEInt _
  | IEReal _
  | IEBool _
  | IEString _
  | IEConstant _
  | IEUnit
  | IEBuiltinPi
  | IEBuiltinEps
  | IEBuiltinSamplerate
  | IEVarMember (_, _) ->
      true
  | _ ->
      false

(* Counts how many times IEVar idx appears in an expression *)
let rec countVarUses (idx : int) (e : iexp) : int =
  match e with
  | IEVar i when i = idx ->
      1
  | IEVar _
  | IEUnit
  | IEEmptyValue
  | IEEmptyList _
  | IEBool _
  | IEInt _
  | IEReal _
  | IEFixed _
  | IEString _
  | IEConstant _
  | IEBuiltinPi
  | IEBuiltinEps
  | IEBuiltinSamplerate
  | IEBuiltinRandom
  | IEBuiltinIrandom
  | IEVarMember (_, _) ->
      0
  | IEUnOp (_, e1)
  | IEBuiltinTanh e1
  | IEBuiltinCosh e1
  | IEBuiltinSinh e1
  | IEBuiltinSin e1
  | IEBuiltinCos e1
  | IEBuiltinTan e1
  | IEBuiltinExp e1
  | IEBuiltinLog e1
  | IEBuiltinLog10 e1
  | IEBuiltinSqrt e1
  | IEBuiltinAbs e1
  | IEBuiltinFloor e1
  | IEBuiltinReal e1
  | IEBuiltinInt e1
  | IEBuiltinInt16 e1
  | IEBuiltinBool e1
  | IEBuiltinString e1
  | IEBuiltinFixed e1
  | IEBuiltinSize e1
  | IEBuiltinLength e1
  | IEBuiltinListSize e1
  | IEBuiltinListCapacity e1
  | IEBuiltinListClear e1 ->
      countVarUses idx e1
  | IEOp (_, e1, e2)
  | IEAddInt (e1, e2)
  | IESubInt (e1, e2)
  | IEMulInt (e1, e2)
  | IEDivInt (e1, e2)
  | IEAddInt16 (e1, e2)
  | IESubInt16 (e1, e2)
  | IEMulInt16 (e1, e2)
  | IEDivInt16 (e1, e2)
  | IEAddReal (e1, e2)
  | IESubReal (e1, e2)
  | IEMulReal (e1, e2)
  | IEDivReal (e1, e2)
  | IEEqInt (e1, e2)
  | IEEqInt16 (e1, e2)
  | IEEqReal (e1, e2)
  | IELtInt (e1, e2)
  | IELtInt16 (e1, e2)
  | IELtReal (e1, e2)
  | IEGtInt (e1, e2)
  | IEGtInt16 (e1, e2)
  | IEGtReal (e1, e2)
  | IEBuiltinPow (e1, e2)
  | IEBuiltinListAppend (e1, e2)
  | IEBuiltinListRemove (e1, e2)
  | IEBuiltinListReserve (e1, e2)
  | IEBuiltinListGet (e1, e2)
  | IEIndex (e1, e2) ->
      countVarUses idx e1 + countVarUses idx e2
  | IEBuiltinClipReal (e1, e2, e3)
  | IEBuiltinClipInt (e1, e2, e3)
  | IEBuiltinListInsert (e1, e2, e3)
  | IEBuiltinListSet (e1, e2, e3) ->
      countVarUses idx e1 + countVarUses idx e2 + countVarUses idx e3
  | IEIf (c, t, f) ->
      countVarUses idx c + countVarUses idx t + countVarUses idx f
  | IEMember (e1, _) ->
      countVarUses idx e1
  | IECall (_, args) | IECallExt (_, args) | IEArray args | IETuple args ->
      CCList.fold_left (fun acc a -> acc + countVarUses idx a) 0 args
  | IERecord (_, elems) ->
      CCList.fold_left (fun acc (_, e1) -> acc + countVarUses idx e1) 0 elems

(* Checks if an expression contains a call to the given function index (self-recursion guard) *)
let rec containsCall (func_idx : int) (e : iexp) : bool =
  match e with
  | IECall (idx, args) ->
      idx = func_idx || CCList.exists (containsCall func_idx) args
  | IEUnit
  | IEEmptyValue
  | IEEmptyList _
  | IEBool _
  | IEInt _
  | IEReal _
  | IEFixed _
  | IEString _
  | IEVar _
  | IEConstant _
  | IEBuiltinPi
  | IEBuiltinEps
  | IEBuiltinSamplerate
  | IEBuiltinRandom
  | IEBuiltinIrandom
  | IEVarMember (_, _) ->
      false
  | IEUnOp (_, e1)
  | IEBuiltinTanh e1
  | IEBuiltinCosh e1
  | IEBuiltinSinh e1
  | IEBuiltinSin e1
  | IEBuiltinCos e1
  | IEBuiltinTan e1
  | IEBuiltinExp e1
  | IEBuiltinLog e1
  | IEBuiltinLog10 e1
  | IEBuiltinSqrt e1
  | IEBuiltinAbs e1
  | IEBuiltinFloor e1
  | IEBuiltinReal e1
  | IEBuiltinInt e1
  | IEBuiltinInt16 e1
  | IEBuiltinBool e1
  | IEBuiltinString e1
  | IEBuiltinFixed e1
  | IEBuiltinSize e1
  | IEBuiltinLength e1
  | IEBuiltinListSize e1
  | IEBuiltinListCapacity e1
  | IEBuiltinListClear e1
  | IEMember (e1, _) ->
      containsCall func_idx e1
  | IEOp (_, e1, e2)
  | IEAddInt (e1, e2)
  | IESubInt (e1, e2)
  | IEMulInt (e1, e2)
  | IEDivInt (e1, e2)
  | IEAddInt16 (e1, e2)
  | IESubInt16 (e1, e2)
  | IEMulInt16 (e1, e2)
  | IEDivInt16 (e1, e2)
  | IEAddReal (e1, e2)
  | IESubReal (e1, e2)
  | IEMulReal (e1, e2)
  | IEDivReal (e1, e2)
  | IEEqInt (e1, e2)
  | IEEqInt16 (e1, e2)
  | IEEqReal (e1, e2)
  | IELtInt (e1, e2)
  | IELtInt16 (e1, e2)
  | IELtReal (e1, e2)
  | IEGtInt (e1, e2)
  | IEGtInt16 (e1, e2)
  | IEGtReal (e1, e2)
  | IEBuiltinPow (e1, e2)
  | IEBuiltinListAppend (e1, e2)
  | IEBuiltinListRemove (e1, e2)
  | IEBuiltinListReserve (e1, e2)
  | IEBuiltinListGet (e1, e2)
  | IEIndex (e1, e2) ->
      containsCall func_idx e1 || containsCall func_idx e2
  | IEBuiltinClipReal (e1, e2, e3)
  | IEBuiltinClipInt (e1, e2, e3)
  | IEBuiltinListInsert (e1, e2, e3)
  | IEBuiltinListSet (e1, e2, e3) ->
      containsCall func_idx e1 || containsCall func_idx e2 || containsCall func_idx e3
  | IEIf (c, t, f) ->
      containsCall func_idx c || containsCall func_idx t || containsCall func_idx f
  | IECallExt (_, args) | IEArray args | IETuple args ->
      CCList.exists (containsCall func_idx) args
  | IERecord (_, elems) ->
      CCList.exists (fun (_, e1) -> containsCall func_idx e1) elems

(* Substitutes IEVar i with args.(i) in an expression *)
let rec substituteVars (args : iexp array) (e : iexp) : iexp =
  match e with
  | IEVar i ->
      if i >= 0 && i < Array.length args then args.(i) else e
  | IEUnit
  | IEEmptyValue
  | IEEmptyList _
  | IEBool _
  | IEInt _
  | IEReal _
  | IEFixed _
  | IEString _
  | IEConstant _
  | IEBuiltinPi
  | IEBuiltinEps
  | IEBuiltinSamplerate
  | IEBuiltinRandom
  | IEBuiltinIrandom ->
      e
  | IEVarMember (var_idx, member_idx) ->
      if var_idx >= 0 && var_idx < Array.length args then IEMember (args.(var_idx), member_idx) else e
  | IEUnOp (op, e1) ->
      IEUnOp (op, substituteVars args e1)
  | IEOp (op, e1, e2) ->
      IEOp (op, substituteVars args e1, substituteVars args e2)
  | IEAddInt (e1, e2) ->
      IEAddInt (substituteVars args e1, substituteVars args e2)
  | IESubInt (e1, e2) ->
      IESubInt (substituteVars args e1, substituteVars args e2)
  | IEMulInt (e1, e2) ->
      IEMulInt (substituteVars args e1, substituteVars args e2)
  | IEDivInt (e1, e2) ->
      IEDivInt (substituteVars args e1, substituteVars args e2)
  | IEAddInt16 (e1, e2) ->
      IEAddInt16 (substituteVars args e1, substituteVars args e2)
  | IESubInt16 (e1, e2) ->
      IESubInt16 (substituteVars args e1, substituteVars args e2)
  | IEMulInt16 (e1, e2) ->
      IEMulInt16 (substituteVars args e1, substituteVars args e2)
  | IEDivInt16 (e1, e2) ->
      IEDivInt16 (substituteVars args e1, substituteVars args e2)
  | IEAddReal (e1, e2) ->
      IEAddReal (substituteVars args e1, substituteVars args e2)
  | IESubReal (e1, e2) ->
      IESubReal (substituteVars args e1, substituteVars args e2)
  | IEMulReal (e1, e2) ->
      IEMulReal (substituteVars args e1, substituteVars args e2)
  | IEDivReal (e1, e2) ->
      IEDivReal (substituteVars args e1, substituteVars args e2)
  | IEEqInt (e1, e2) ->
      IEEqInt (substituteVars args e1, substituteVars args e2)
  | IEEqInt16 (e1, e2) ->
      IEEqInt16 (substituteVars args e1, substituteVars args e2)
  | IEEqReal (e1, e2) ->
      IEEqReal (substituteVars args e1, substituteVars args e2)
  | IELtInt (e1, e2) ->
      IELtInt (substituteVars args e1, substituteVars args e2)
  | IELtInt16 (e1, e2) ->
      IELtInt16 (substituteVars args e1, substituteVars args e2)
  | IELtReal (e1, e2) ->
      IELtReal (substituteVars args e1, substituteVars args e2)
  | IEGtInt (e1, e2) ->
      IEGtInt (substituteVars args e1, substituteVars args e2)
  | IEGtInt16 (e1, e2) ->
      IEGtInt16 (substituteVars args e1, substituteVars args e2)
  | IEGtReal (e1, e2) ->
      IEGtReal (substituteVars args e1, substituteVars args e2)
  | IEBuiltinTanh e1 ->
      IEBuiltinTanh (substituteVars args e1)
  | IEBuiltinCosh e1 ->
      IEBuiltinCosh (substituteVars args e1)
  | IEBuiltinSinh e1 ->
      IEBuiltinSinh (substituteVars args e1)
  | IEBuiltinSin e1 ->
      IEBuiltinSin (substituteVars args e1)
  | IEBuiltinCos e1 ->
      IEBuiltinCos (substituteVars args e1)
  | IEBuiltinTan e1 ->
      IEBuiltinTan (substituteVars args e1)
  | IEBuiltinExp e1 ->
      IEBuiltinExp (substituteVars args e1)
  | IEBuiltinLog e1 ->
      IEBuiltinLog (substituteVars args e1)
  | IEBuiltinLog10 e1 ->
      IEBuiltinLog10 (substituteVars args e1)
  | IEBuiltinSqrt e1 ->
      IEBuiltinSqrt (substituteVars args e1)
  | IEBuiltinAbs e1 ->
      IEBuiltinAbs (substituteVars args e1)
  | IEBuiltinFloor e1 ->
      IEBuiltinFloor (substituteVars args e1)
  | IEBuiltinPow (e1, e2) ->
      IEBuiltinPow (substituteVars args e1, substituteVars args e2)
  | IEBuiltinClipReal (e1, e2, e3) ->
      IEBuiltinClipReal (substituteVars args e1, substituteVars args e2, substituteVars args e3)
  | IEBuiltinClipInt (e1, e2, e3) ->
      IEBuiltinClipInt (substituteVars args e1, substituteVars args e2, substituteVars args e3)
  | IEBuiltinReal e1 ->
      IEBuiltinReal (substituteVars args e1)
  | IEBuiltinInt e1 ->
      IEBuiltinInt (substituteVars args e1)
  | IEBuiltinInt16 e1 ->
      IEBuiltinInt16 (substituteVars args e1)
  | IEBuiltinBool e1 ->
      IEBuiltinBool (substituteVars args e1)
  | IEBuiltinString e1 ->
      IEBuiltinString (substituteVars args e1)
  | IEBuiltinFixed e1 ->
      IEBuiltinFixed (substituteVars args e1)
  | IEBuiltinSize e1 ->
      IEBuiltinSize (substituteVars args e1)
  | IEBuiltinLength e1 ->
      IEBuiltinLength (substituteVars args e1)
  | IEBuiltinListSize e1 ->
      IEBuiltinListSize (substituteVars args e1)
  | IEBuiltinListCapacity e1 ->
      IEBuiltinListCapacity (substituteVars args e1)
  | IEBuiltinListAppend (e1, e2) ->
      IEBuiltinListAppend (substituteVars args e1, substituteVars args e2)
  | IEBuiltinListInsert (e1, e2, e3) ->
      IEBuiltinListInsert (substituteVars args e1, substituteVars args e2, substituteVars args e3)
  | IEBuiltinListRemove (e1, e2) ->
      IEBuiltinListRemove (substituteVars args e1, substituteVars args e2)
  | IEBuiltinListClear e1 ->
      IEBuiltinListClear (substituteVars args e1)
  | IEBuiltinListReserve (e1, e2) ->
      IEBuiltinListReserve (substituteVars args e1, substituteVars args e2)
  | IEBuiltinListGet (e1, e2) ->
      IEBuiltinListGet (substituteVars args e1, substituteVars args e2)
  | IEBuiltinListSet (e1, e2, e3) ->
      IEBuiltinListSet (substituteVars args e1, substituteVars args e2, substituteVars args e3)
  | IEIndex (e1, e2) ->
      IEIndex (substituteVars args e1, substituteVars args e2)
  | IEArray elems ->
      IEArray (CCList.map (substituteVars args) elems)
  | IECall (func_idx, call_args) ->
      IECall (func_idx, CCList.map (substituteVars args) call_args)
  | IECallExt (name, call_args) ->
      IECallExt (name, CCList.map (substituteVars args) call_args)
  | IEIf (c, t, f) ->
      IEIf (substituteVars args c, substituteVars args t, substituteVars args f)
  | IETuple elems ->
      IETuple (CCList.map (substituteVars args) elems)
  | IEMember (e1, idx) ->
      IEMember (substituteVars args e1, idx)
  | IERecord (descr, elems) ->
      IERecord (descr, CCList.map (fun (idx, e1) -> (idx, substituteVars args e1)) elems)

(* Checks if a function is eligible for expression inlining *)
let isInlinableFunction (func : ifunc_def) : bool =
  match func.ibody with
  | IStmtReturn expr ->
      (* Only inline if no local variables beyond parameters *)
      func.ilocals = CCList.length func.iargs && iexpSize expr <= 15
  | _ ->
      false

(* Inlines expression-only functions in an iexp tree *)
let rec inlineExp (prog : iprog) (e : iexp) : iexp =
  match e with
  | IECall (func_idx, args) when func_idx >= 0 && func_idx < Array.length prog.ifunctions_array -> (
      let func = Array.unsafe_get prog.ifunctions_array func_idx in
      match func.ibody with
      | IStmtReturn body_expr
        when func.ilocals = CCList.length func.iargs
             && iexpSize body_expr <= 15
             && not (containsCall func_idx body_expr) ->
          let inlined_args = CCList.map (inlineExp prog) args in
          let arg_array = Array.of_list inlined_args in
          (* Check: for params used >1 time, arg must be simple *)
          let safe_to_inline =
            CCList.for_all
              (fun param_idx ->
                let uses = countVarUses param_idx body_expr in
                uses <= 1 || isSimpleExp (Array.unsafe_get arg_array param_idx) )
              func.iargs
          in
          if safe_to_inline then
            let result = substituteVars arg_array body_expr in
            inlineExp prog result
          else IECall (func_idx, inlined_args)
      | _ ->
          IECall (func_idx, CCList.map (inlineExp prog) args) )
  | IECall (func_idx, args) ->
      IECall (func_idx, CCList.map (inlineExp prog) args)
  | IECallExt (name, args) ->
      IECallExt (name, CCList.map (inlineExp prog) args)
  | IEUnOp (op, e1) ->
      IEUnOp (op, inlineExp prog e1)
  | IEOp (op, e1, e2) ->
      IEOp (op, inlineExp prog e1, inlineExp prog e2)
  | IEAddInt (e1, e2) ->
      IEAddInt (inlineExp prog e1, inlineExp prog e2)
  | IESubInt (e1, e2) ->
      IESubInt (inlineExp prog e1, inlineExp prog e2)
  | IEMulInt (e1, e2) ->
      IEMulInt (inlineExp prog e1, inlineExp prog e2)
  | IEDivInt (e1, e2) ->
      IEDivInt (inlineExp prog e1, inlineExp prog e2)
  | IEAddInt16 (e1, e2) ->
      IEAddInt16 (inlineExp prog e1, inlineExp prog e2)
  | IESubInt16 (e1, e2) ->
      IESubInt16 (inlineExp prog e1, inlineExp prog e2)
  | IEMulInt16 (e1, e2) ->
      IEMulInt16 (inlineExp prog e1, inlineExp prog e2)
  | IEDivInt16 (e1, e2) ->
      IEDivInt16 (inlineExp prog e1, inlineExp prog e2)
  | IEAddReal (e1, e2) ->
      IEAddReal (inlineExp prog e1, inlineExp prog e2)
  | IESubReal (e1, e2) ->
      IESubReal (inlineExp prog e1, inlineExp prog e2)
  | IEMulReal (e1, e2) ->
      IEMulReal (inlineExp prog e1, inlineExp prog e2)
  | IEDivReal (e1, e2) ->
      IEDivReal (inlineExp prog e1, inlineExp prog e2)
  | IEEqInt (e1, e2) ->
      IEEqInt (inlineExp prog e1, inlineExp prog e2)
  | IEEqInt16 (e1, e2) ->
      IEEqInt16 (inlineExp prog e1, inlineExp prog e2)
  | IEEqReal (e1, e2) ->
      IEEqReal (inlineExp prog e1, inlineExp prog e2)
  | IELtInt (e1, e2) ->
      IELtInt (inlineExp prog e1, inlineExp prog e2)
  | IELtInt16 (e1, e2) ->
      IELtInt16 (inlineExp prog e1, inlineExp prog e2)
  | IELtReal (e1, e2) ->
      IELtReal (inlineExp prog e1, inlineExp prog e2)
  | IEGtInt (e1, e2) ->
      IEGtInt (inlineExp prog e1, inlineExp prog e2)
  | IEGtInt16 (e1, e2) ->
      IEGtInt16 (inlineExp prog e1, inlineExp prog e2)
  | IEGtReal (e1, e2) ->
      IEGtReal (inlineExp prog e1, inlineExp prog e2)
  | IEBuiltinTanh e1 ->
      IEBuiltinTanh (inlineExp prog e1)
  | IEBuiltinCosh e1 ->
      IEBuiltinCosh (inlineExp prog e1)
  | IEBuiltinSinh e1 ->
      IEBuiltinSinh (inlineExp prog e1)
  | IEBuiltinSin e1 ->
      IEBuiltinSin (inlineExp prog e1)
  | IEBuiltinCos e1 ->
      IEBuiltinCos (inlineExp prog e1)
  | IEBuiltinTan e1 ->
      IEBuiltinTan (inlineExp prog e1)
  | IEBuiltinExp e1 ->
      IEBuiltinExp (inlineExp prog e1)
  | IEBuiltinLog e1 ->
      IEBuiltinLog (inlineExp prog e1)
  | IEBuiltinLog10 e1 ->
      IEBuiltinLog10 (inlineExp prog e1)
  | IEBuiltinSqrt e1 ->
      IEBuiltinSqrt (inlineExp prog e1)
  | IEBuiltinAbs e1 ->
      IEBuiltinAbs (inlineExp prog e1)
  | IEBuiltinFloor e1 ->
      IEBuiltinFloor (inlineExp prog e1)
  | IEBuiltinPow (e1, e2) ->
      IEBuiltinPow (inlineExp prog e1, inlineExp prog e2)
  | IEBuiltinClipReal (e1, e2, e3) ->
      IEBuiltinClipReal (inlineExp prog e1, inlineExp prog e2, inlineExp prog e3)
  | IEBuiltinClipInt (e1, e2, e3) ->
      IEBuiltinClipInt (inlineExp prog e1, inlineExp prog e2, inlineExp prog e3)
  | IEBuiltinReal e1 ->
      IEBuiltinReal (inlineExp prog e1)
  | IEBuiltinInt e1 ->
      IEBuiltinInt (inlineExp prog e1)
  | IEBuiltinInt16 e1 ->
      IEBuiltinInt16 (inlineExp prog e1)
  | IEBuiltinBool e1 ->
      IEBuiltinBool (inlineExp prog e1)
  | IEBuiltinString e1 ->
      IEBuiltinString (inlineExp prog e1)
  | IEBuiltinFixed e1 ->
      IEBuiltinFixed (inlineExp prog e1)
  | IEBuiltinSize e1 ->
      IEBuiltinSize (inlineExp prog e1)
  | IEBuiltinLength e1 ->
      IEBuiltinLength (inlineExp prog e1)
  | IEBuiltinListSize e1 ->
      IEBuiltinListSize (inlineExp prog e1)
  | IEBuiltinListCapacity e1 ->
      IEBuiltinListCapacity (inlineExp prog e1)
  | IEBuiltinListAppend (e1, e2) ->
      IEBuiltinListAppend (inlineExp prog e1, inlineExp prog e2)
  | IEBuiltinListInsert (e1, e2, e3) ->
      IEBuiltinListInsert (inlineExp prog e1, inlineExp prog e2, inlineExp prog e3)
  | IEBuiltinListRemove (e1, e2) ->
      IEBuiltinListRemove (inlineExp prog e1, inlineExp prog e2)
  | IEBuiltinListClear e1 ->
      IEBuiltinListClear (inlineExp prog e1)
  | IEBuiltinListReserve (e1, e2) ->
      IEBuiltinListReserve (inlineExp prog e1, inlineExp prog e2)
  | IEBuiltinListGet (e1, e2) ->
      IEBuiltinListGet (inlineExp prog e1, inlineExp prog e2)
  | IEBuiltinListSet (e1, e2, e3) ->
      IEBuiltinListSet (inlineExp prog e1, inlineExp prog e2, inlineExp prog e3)
  | IEIndex (e1, e2) ->
      IEIndex (inlineExp prog e1, inlineExp prog e2)
  | IEArray elems ->
      IEArray (CCList.map (inlineExp prog) elems)
  | IEIf (c, t, f) ->
      IEIf (inlineExp prog c, inlineExp prog t, inlineExp prog f)
  | IETuple elems ->
      IETuple (CCList.map (inlineExp prog) elems)
  | IEMember (e1, idx) ->
      IEMember (inlineExp prog e1, idx)
  | IERecord (descr, elems) ->
      IERecord (descr, CCList.map (fun (idx, e1) -> (idx, inlineExp prog e1)) elems)
  | _ ->
      e

(* Inlines expression-only functions in a statement tree *)
let rec inlineStmt (prog : iprog) (s : istmt) : istmt =
  match s with
  | IStmtDecl (idx, typ, init) ->
      IStmtDecl (idx, typ, Option.map (inlineExp prog) init)
  | IStmtBind (lv, e) ->
      IStmtBind (lv, inlineExp prog e)
  | IStmtReturn e ->
      IStmtReturn (inlineExp prog e)
  | IStmtBlock stmts ->
      IStmtBlock (CCList.map (inlineStmt prog) stmts)
  | IStmtIf (c, t, el) ->
      IStmtIf (inlineExp prog c, inlineStmt prog t, Option.map (inlineStmt prog) el)
  | IStmtWhile (c, body) ->
      IStmtWhile (inlineExp prog c, inlineStmt prog body)
  | IStmtSwitch (e, cases, default) ->
      IStmtSwitch
        ( inlineExp prog e
        , CCList.map (fun (ce, cs) -> (inlineExp prog ce, inlineStmt prog cs)) cases
        , Option.map (inlineStmt prog) default )

(* ---- Phase 2c: Constant Folding ----

   Bottom-up constant folding on the iexp tree. Traverses each expression recursively,
   folding children first, then checking if the parent can be simplified.

   Simplifications include:
   - Arithmetic on literals: IEAddInt(IEInt 3, IEInt 4) -> IEInt 7
   - Identity elimination: x + 0 -> x, x * 1 -> x, x - 0 -> x, x / 1 -> x
   - Zero elimination: x * 0 -> 0 (integers only, not floats due to NaN/Inf)
   - Comparison on literals: IELtReal(IEReal 1.0, IEReal 2.0) -> IEBool true
   - Type conversion on literals: IEBuiltinReal(IEInt 3) -> IEReal 3.0
   - Math builtins on literals: IEBuiltinSin(IEReal 0.0) -> IEReal 0.0
   - Dead branch elimination: IEIf(IEBool true, t, f) -> t

   This phase operates on the iexp tree (before closure conversion). It complements
   the compile-time constant propagation in Phase 3, which can additionally fold
   lazy constants (IEConstant) and propagate through CVar patterns.
*)

(* Folds constant sub-expressions bottom-up *)
let rec foldConstantsExp (e : iexp) : iexp =
  match e with
  (* Integer arithmetic *)
  | IEAddInt (e1, e2) -> (
    match (foldConstantsExp e1, foldConstantsExp e2) with
    | IEInt a, IEInt b ->
        IEInt (a + b)
    | fe1, fe2 ->
        IEAddInt (fe1, fe2) )
  | IESubInt (e1, e2) -> (
    match (foldConstantsExp e1, foldConstantsExp e2) with
    | IEInt a, IEInt b ->
        IEInt (a - b)
    | fe1, fe2 ->
        IESubInt (fe1, fe2) )
  | IEMulInt (e1, e2) -> (
    match (foldConstantsExp e1, foldConstantsExp e2) with
    | IEInt a, IEInt b ->
        IEInt (a * b)
    | fe1, fe2 ->
        IEMulInt (fe1, fe2) )
  | IEDivInt (e1, e2) -> (
    match (foldConstantsExp e1, foldConstantsExp e2) with
    | IEInt a, IEInt b when b <> 0 ->
        IEInt (a / b)
    | fe1, fe2 ->
        IEDivInt (fe1, fe2) )
  (* Real arithmetic *)
  | IEAddReal (e1, e2) -> (
    match (foldConstantsExp e1, foldConstantsExp e2) with
    | IEReal a, IEReal b ->
        IEReal (a +. b)
    | fe1, fe2 ->
        IEAddReal (fe1, fe2) )
  | IESubReal (e1, e2) -> (
    match (foldConstantsExp e1, foldConstantsExp e2) with
    | IEReal a, IEReal b ->
        IEReal (a -. b)
    | fe1, fe2 ->
        IESubReal (fe1, fe2) )
  | IEMulReal (e1, e2) -> (
    match (foldConstantsExp e1, foldConstantsExp e2) with
    | IEReal a, IEReal b ->
        IEReal (a *. b)
    | fe1, fe2 ->
        IEMulReal (fe1, fe2) )
  | IEDivReal (e1, e2) -> (
    match (foldConstantsExp e1, foldConstantsExp e2) with
    | IEReal a, IEReal b when b <> 0.0 ->
        IEReal (a /. b)
    | fe1, fe2 ->
        IEDivReal (fe1, fe2) )
  (* Unary *)
  | IEUnOp (UOpNeg, e1) -> (
    match foldConstantsExp e1 with IEInt i -> IEInt (-i) | IEReal f -> IEReal (-.f) | fe1 -> IEUnOp (UOpNeg, fe1) )
  | IEUnOp (UOpNot, e1) -> (
    match foldConstantsExp e1 with IEBool b -> IEBool (not b) | fe1 -> IEUnOp (UOpNot, fe1) )
  (* Conditional *)
  | IEIf (c, t, f) -> (
    match foldConstantsExp c with
    | IEBool true ->
        foldConstantsExp t
    | IEBool false ->
        foldConstantsExp f
    | fc ->
        IEIf (fc, foldConstantsExp t, foldConstantsExp f) )
  (* Type conversion *)
  | IEBuiltinReal e1 -> (
    match foldConstantsExp e1 with IEInt i -> IEReal (float_of_int i) | fe1 -> IEBuiltinReal fe1 )
  (* Comparisons *)
  | IEEqInt (e1, e2) -> (
    match (foldConstantsExp e1, foldConstantsExp e2) with
    | IEInt a, IEInt b ->
        IEBool (a = b)
    | fe1, fe2 ->
        IEEqInt (fe1, fe2) )
  | IEEqReal (e1, e2) -> (
    match (foldConstantsExp e1, foldConstantsExp e2) with
    | IEReal a, IEReal b ->
        IEBool (Float.equal a b)
    | fe1, fe2 ->
        IEEqReal (fe1, fe2) )
  | IELtInt (e1, e2) -> (
    match (foldConstantsExp e1, foldConstantsExp e2) with
    | IEInt a, IEInt b ->
        IEBool (a < b)
    | fe1, fe2 ->
        IELtInt (fe1, fe2) )
  | IELtReal (e1, e2) -> (
    match (foldConstantsExp e1, foldConstantsExp e2) with
    | IEReal a, IEReal b ->
        IEBool (a < b)
    | fe1, fe2 ->
        IELtReal (fe1, fe2) )
  | IEGtInt (e1, e2) -> (
    match (foldConstantsExp e1, foldConstantsExp e2) with
    | IEInt a, IEInt b ->
        IEBool (a > b)
    | fe1, fe2 ->
        IEGtInt (fe1, fe2) )
  | IEGtReal (e1, e2) -> (
    match (foldConstantsExp e1, foldConstantsExp e2) with
    | IEReal a, IEReal b ->
        IEBool (a > b)
    | fe1, fe2 ->
        IEGtReal (fe1, fe2) )
  (* Recurse into other binary ops *)
  | IEOp (op, e1, e2) ->
      IEOp (op, foldConstantsExp e1, foldConstantsExp e2)
  | IEAddInt16 (e1, e2) ->
      IEAddInt16 (foldConstantsExp e1, foldConstantsExp e2)
  | IESubInt16 (e1, e2) ->
      IESubInt16 (foldConstantsExp e1, foldConstantsExp e2)
  | IEMulInt16 (e1, e2) ->
      IEMulInt16 (foldConstantsExp e1, foldConstantsExp e2)
  | IEDivInt16 (e1, e2) ->
      IEDivInt16 (foldConstantsExp e1, foldConstantsExp e2)
  | IEEqInt16 (e1, e2) ->
      IEEqInt16 (foldConstantsExp e1, foldConstantsExp e2)
  | IELtInt16 (e1, e2) ->
      IELtInt16 (foldConstantsExp e1, foldConstantsExp e2)
  | IEGtInt16 (e1, e2) ->
      IEGtInt16 (foldConstantsExp e1, foldConstantsExp e2)
  (* Recurse into unary builtins *)
  | IEBuiltinTanh e1 ->
      IEBuiltinTanh (foldConstantsExp e1)
  | IEBuiltinCosh e1 ->
      IEBuiltinCosh (foldConstantsExp e1)
  | IEBuiltinSinh e1 ->
      IEBuiltinSinh (foldConstantsExp e1)
  | IEBuiltinSin e1 ->
      IEBuiltinSin (foldConstantsExp e1)
  | IEBuiltinCos e1 ->
      IEBuiltinCos (foldConstantsExp e1)
  | IEBuiltinTan e1 ->
      IEBuiltinTan (foldConstantsExp e1)
  | IEBuiltinExp e1 ->
      IEBuiltinExp (foldConstantsExp e1)
  | IEBuiltinLog e1 ->
      IEBuiltinLog (foldConstantsExp e1)
  | IEBuiltinLog10 e1 ->
      IEBuiltinLog10 (foldConstantsExp e1)
  | IEBuiltinSqrt e1 ->
      IEBuiltinSqrt (foldConstantsExp e1)
  | IEBuiltinAbs e1 ->
      IEBuiltinAbs (foldConstantsExp e1)
  | IEBuiltinFloor e1 ->
      IEBuiltinFloor (foldConstantsExp e1)
  | IEBuiltinPow (e1, e2) ->
      IEBuiltinPow (foldConstantsExp e1, foldConstantsExp e2)
  | IEBuiltinClipReal (e1, e2, e3) ->
      IEBuiltinClipReal (foldConstantsExp e1, foldConstantsExp e2, foldConstantsExp e3)
  | IEBuiltinClipInt (e1, e2, e3) ->
      IEBuiltinClipInt (foldConstantsExp e1, foldConstantsExp e2, foldConstantsExp e3)
  | IEBuiltinInt e1 ->
      IEBuiltinInt (foldConstantsExp e1)
  | IEBuiltinInt16 e1 ->
      IEBuiltinInt16 (foldConstantsExp e1)
  | IEBuiltinBool e1 ->
      IEBuiltinBool (foldConstantsExp e1)
  | IEBuiltinString e1 ->
      IEBuiltinString (foldConstantsExp e1)
  | IEBuiltinFixed e1 ->
      IEBuiltinFixed (foldConstantsExp e1)
  | IEBuiltinSize e1 ->
      IEBuiltinSize (foldConstantsExp e1)
  | IEBuiltinLength e1 ->
      IEBuiltinLength (foldConstantsExp e1)
  | IEBuiltinListSize e1 ->
      IEBuiltinListSize (foldConstantsExp e1)
  | IEBuiltinListCapacity e1 ->
      IEBuiltinListCapacity (foldConstantsExp e1)
  | IEBuiltinListAppend (e1, e2) ->
      IEBuiltinListAppend (foldConstantsExp e1, foldConstantsExp e2)
  | IEBuiltinListInsert (e1, e2, e3) ->
      IEBuiltinListInsert (foldConstantsExp e1, foldConstantsExp e2, foldConstantsExp e3)
  | IEBuiltinListRemove (e1, e2) ->
      IEBuiltinListRemove (foldConstantsExp e1, foldConstantsExp e2)
  | IEBuiltinListClear e1 ->
      IEBuiltinListClear (foldConstantsExp e1)
  | IEBuiltinListReserve (e1, e2) ->
      IEBuiltinListReserve (foldConstantsExp e1, foldConstantsExp e2)
  | IEBuiltinListGet (e1, e2) ->
      IEBuiltinListGet (foldConstantsExp e1, foldConstantsExp e2)
  | IEBuiltinListSet (e1, e2, e3) ->
      IEBuiltinListSet (foldConstantsExp e1, foldConstantsExp e2, foldConstantsExp e3)
  | IEIndex (e1, e2) ->
      IEIndex (foldConstantsExp e1, foldConstantsExp e2)
  | IEArray elems ->
      IEArray (CCList.map foldConstantsExp elems)
  | IECall (func_idx, args) ->
      IECall (func_idx, CCList.map foldConstantsExp args)
  | IECallExt (name, args) ->
      IECallExt (name, CCList.map foldConstantsExp args)
  | IETuple elems ->
      IETuple (CCList.map foldConstantsExp elems)
  | IEMember (e1, idx) ->
      IEMember (foldConstantsExp e1, idx)
  | IERecord (descr, elems) ->
      IERecord (descr, CCList.map (fun (idx, e1) -> (idx, foldConstantsExp e1)) elems)
  | _ ->
      e

(* Folds constants in a statement tree *)
let rec foldConstantsStmt (s : istmt) : istmt =
  match s with
  | IStmtDecl (idx, typ, init) ->
      IStmtDecl (idx, typ, Option.map foldConstantsExp init)
  | IStmtBind (lv, e) ->
      IStmtBind (lv, foldConstantsExp e)
  | IStmtReturn e ->
      IStmtReturn (foldConstantsExp e)
  | IStmtBlock stmts ->
      IStmtBlock (CCList.map foldConstantsStmt stmts)
  | IStmtIf (c, t, el) -> (
    match foldConstantsExp c with
    | IEBool true ->
        foldConstantsStmt t
    | IEBool false -> (
      match el with Some s -> foldConstantsStmt s | None -> IStmtBlock [] )
    | fc ->
        IStmtIf (fc, foldConstantsStmt t, Option.map foldConstantsStmt el) )
  | IStmtWhile (c, body) ->
      IStmtWhile (foldConstantsExp c, foldConstantsStmt body)
  | IStmtSwitch (e, cases, default) ->
      IStmtSwitch
        ( foldConstantsExp e
        , CCList.map (fun (ce, cs) -> (foldConstantsExp ce, foldConstantsStmt cs)) cases
        , Option.map foldConstantsStmt default )

(* Runs the optimization pass on all functions in the program *)
(* Run Phases 2b and 2c on all functions: inline small functions, then fold constants.
   Mutates prog.ifunctions_array and prog.ifunctions in place. *)
let optimizeProgram (prog : iprog) : unit =
  let num_funcs = Map.cardinal prog.ifunction_names in
  for i = 0 to num_funcs - 1 do
    if i < Array.length prog.ifunctions_array then begin
      let func = prog.ifunctions_array.(i) in
      let optimized_body = inlineStmt prog func.ibody in
      let folded_body = foldConstantsStmt optimized_body in
      prog.ifunctions_array.(i) <- {func with ibody= folded_body} ;
      prog.ifunctions <- Map.add func.iname prog.ifunctions_array.(i) prog.ifunctions
    end
  done

(* Full pipeline: Phase 1 (transform) + Phases 2a-2c (optimize).
   Does NOT run Phase 3 (closure conversion) — call compileProgram separately. *)
let transformProgram (prog : top_stmt list) : iprog =
  let iprog = extendProgram (createEmptyProgram ()) prog in
  optimizeProgram iprog ; iprog

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

(* Empty mutable list *)

(* Sets up a function call on the runtime stack and returns the frame start offset *)
let setupFunctionCall (stack : runtime_stack) (ifunc : ifunc_def) (args : dvalue list) : int =
  let frame_start = stack.sp in
  (* Check stack overflow *)
  if stack.sp + ifunc.ilocals > stack.max_size then error ("Stack overflow in function " ^ ifunc.iname) ;
  (* Initialize all locals to default values first *)
  for i = 0 to ifunc.ilocals - 1 do
    Array.unsafe_set stack.stack (stack.sp + i) DVoid
  done ;
  (* Initialize parameters *)
  CCList.iter2
    (fun param_idx arg_val -> Array.unsafe_set stack.stack (frame_start + param_idx) arg_val)
    ifunc.iargs args ;
  (* Move stack pointer *)
  stack.sp <- stack.sp + ifunc.ilocals ;
  frame_start

(* Cleans up a function call from the runtime stack by restoring the stack pointer *)
let cleanupFunctionCall (stack : runtime_stack) (ifunc : ifunc_def) : unit = stack.sp <- stack.sp - ifunc.ilocals

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

   These functions implement the core runtime semantics: binary/unary operations,
   function calls, statement execution, and expression evaluation. They are used
   both by the tree-walking interpreter (evalIexp/execIstmt) and indirectly by
   the closure-compiled path (evalBinop/evalUnop are called from compiled closures
   for generic IEOp/IEUnOp nodes).
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

(* Calls a function by index with the given arguments and returns the result *)
let rec callFunction : call_context -> iprog -> runtime_stack -> int -> dvalue list -> dvalue =
 fun ctx prog stack func_idx args ->
  let ifunc = Array.unsafe_get prog.ifunctions_array func_idx in
  let frame_start = setupFunctionCall stack ifunc args in
  let result = execIstmt ctx prog stack frame_start ifunc.ibody in
  cleanupFunctionCall stack ifunc ;
  match result with Continue -> DVoid (* Function completed without explicit return *) | Return v -> v

(* Executes a list of statements, stopping early if a return is encountered *)
and execStmtList : call_context -> iprog -> runtime_stack -> int -> istmt list -> exec_result =
 fun ctx prog stack frame_start stmts ->
  let rec loop = function
    | [] ->
        Continue
    | stmt :: rest -> (
      match execIstmt ctx prog stack frame_start stmt with Continue -> loop rest | Return v -> Return v )
  in
  loop stmts

(* Executes an interpreter statement *)
and execIstmt : call_context -> iprog -> runtime_stack -> int -> istmt -> exec_result =
 fun ctx prog stack frame_start stmt ->
  match stmt with
  | IStmtDecl (var_idx, typ, init_exp) ->
      let init_val =
        match init_exp with Some exp -> evalIexp ctx prog stack frame_start exp | None -> defaultValue typ
      in
      Array.unsafe_set stack.stack (frame_start + var_idx) init_val ;
      Continue
  | IStmtBind (lexp, exp) ->
      let val_ = evalIexp ctx prog stack frame_start exp in
      assignIlvalue ctx prog stack frame_start lexp val_ ;
      Continue
  | IStmtReturn exp ->
      let val_ = evalIexp ctx prog stack frame_start exp in
      Return val_
  | IStmtBlock stmts ->
      execStmtList ctx prog stack frame_start stmts
  | IStmtIf (cond, then_stmt, else_stmt) -> (
    match evalIexp ctx prog stack frame_start cond with
    | DBool true ->
        execIstmt ctx prog stack frame_start then_stmt
    | DBool false -> (
      match else_stmt with Some stmt -> execIstmt ctx prog stack frame_start stmt | None -> Continue )
    | _ ->
        error_with_context ctx "Invalid condition" )
  | IStmtWhile (cond, body) ->
      let rec loop () =
        match evalIexp ctx prog stack frame_start cond with
        | DBool true -> (
          match execIstmt ctx prog stack frame_start body with Continue -> loop () | Return v -> Return v )
        | _ ->
            Continue
      in
      loop ()
  | IStmtSwitch (exp, cases, default) ->
      let exp_val = evalIexp ctx prog stack frame_start exp in
      let rec try_cases = function
        | [] -> (
          match default with Some stmt -> execIstmt ctx prog stack frame_start stmt | None -> Continue )
        | (case_exp, case_stmt) :: rest ->
            let case_val = evalIexp ctx prog stack frame_start case_exp in
            if evalBinop ctx OpEq exp_val case_val = DBool true then execIstmt ctx prog stack frame_start case_stmt
            else try_cases rest
      in
      try_cases cases

(* Evaluates an lvalue expression as an rvalue (gets the value it points to) *)
and evalIlexpAsRvalue : call_context -> iprog -> runtime_stack -> int -> ilexp -> dvalue =
 fun ctx prog stack frame_start lexp ->
  match lexp with
  | ILWild ->
      error_with_context ctx "Cannot evaluate wildcard as rvalue"
  | ILVar idx ->
      Array.unsafe_get stack.stack (frame_start + idx)
  | ILVarMember (var_idx, member_idx) -> (
    match Array.unsafe_get stack.stack (frame_start + var_idx) with
    | DStruct members ->
        Array.unsafe_get members member_idx
    | _ ->
        error_with_context ctx "Invalid struct access in fused var-member lvalue" )
  | ILMember (e, member_idx) -> (
      let struct_val = evalIlexpAsRvalue ctx prog stack frame_start e in
      match struct_val with
      | DStruct members when member_idx >= 0 && member_idx < Array.length members ->
          members.(member_idx)
      | _ ->
          error_with_context ctx "Invalid struct member access" )
  | ILIndex (e, index) -> (
      let idx_val = evalIexp ctx prog stack frame_start index in
      let array_val = evalIlexpAsRvalue ctx prog stack frame_start e in
      match (array_val, idx_val) with
      | DArray arr, DInt i when i >= 0 && i < Array.length arr ->
          arr.(i)
      | _ ->
          error_with_context ctx "evalIlexpAsRvalue: Invalid array access" )
  | ILTuple _ ->
      error_with_context ctx "Cannot evaluate tuple lvalue as rvalue"

(* Assigns a value to an optimized interpreter left-value expression *)
and assignIlvalue : call_context -> iprog -> runtime_stack -> int -> ilexp -> dvalue -> unit =
 fun ctx prog stack frame_start lexp val_ ->
  match lexp with
  | ILWild ->
      ()
  | ILVar idx ->
      Array.unsafe_set stack.stack (frame_start + idx) val_
  | ILVarMember (var_idx, member_idx) -> (
    match Array.unsafe_get stack.stack (frame_start + var_idx) with
    | DStruct members ->
        Array.unsafe_set members member_idx val_
    | _ ->
        error_with_context ctx "Invalid struct access in fused var-member assignment" )
  | ILMember (e, member_idx) -> (
      (* First get the container struct by recursively evaluating the base expression *)
      let struct_val = evalIlexpAsRvalue ctx prog stack frame_start e in
      match struct_val with
      | DStruct members when member_idx >= 0 && member_idx < Array.length members ->
          members.(member_idx) <- val_
      | _ ->
          error_with_context ctx "Invalid struct member assignment" )
  | ILIndex (e, index) -> (
      let idx_val = evalIexp ctx prog stack frame_start index in
      (* Get the container array by recursively evaluating the base expression *)
      let array_val = evalIlexpAsRvalue ctx prog stack frame_start e in
      match (array_val, idx_val) with
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
          else
            error_with_context ctx
              ("List index out of bounds: index = " ^ string_of_int i ^ " size = " ^ string_of_int len)
      | _ ->
          error_with_context ctx "Invalid array or list assignment" )
  | ILTuple lexps -> (
    match val_ with
    | DArray vals when Array.length vals = CCList.length lexps ->
        CCList.iteri (fun i lexp -> assignIlvalue ctx prog stack frame_start lexp vals.(i)) lexps
    | _ ->
        error_with_context ctx "Tuple assignment type mismatch" )

(* Force evaluation of a lazy constant. On first access, evaluates the stored iexp
   using the tree-walking interpreter, caches the result as Evaluated, and returns it.
   Subsequent accesses return the cached value directly. This is called both at
   runtime (from evalIexp) and at compile time (from compileIexp for CConstant). *)
and evaluateLazyConstant (constants : constant_value array) (idx : int) : dvalue =
  match constants.(idx) with
  | Evaluated value ->
      value
  | Unevaluated (exp, ctx) ->
      (* Create a temporary program for evaluation *)
      let temp_prog =
        { ifunctions= Map.empty
        ; ifunctions_array= ctx.ifunctions_array
        ; ifunction_names= ctx.ifunction_names
        ; iconstants= !(ctx.iconstants_ref)
        ; iconstants_count= Array.length !(ctx.iconstants_ref)
        ; struct_types= Map.empty
        ; constant_names= Map.empty
        ; external_functions= Set.empty
        ; compiled_functions= [||] }
      in
      (* Create a minimal stack for pure function evaluation *)
      let temp_stack = createStack 100 in
      let temp_ctx = {frames= []; depth= 0; max_depth= 50; sample_rate= None} in
      let value = evalIexp temp_ctx temp_prog temp_stack 0 exp in
      (* Cache the evaluated value *)
      constants.(idx) <- Evaluated value ;
      value

(* ---- Tree-Walking Interpreter ----

   evalIexp is the original tree-walking interpreter. It dispatches on the iexp tag
   for every evaluation, which is simple but has overhead from repeated tag matching.
   This is now primarily used as a fallback for lazy constant evaluation
   (evaluateLazyConstant) and as a reference implementation. The main execution path
   uses the closure-compiled version from Phase 3 (compileIexp + callCompiledFunction).
*)
and evalIexp (ctx : call_context) (prog : iprog) (stack : runtime_stack) (frame_start : int) (exp : iexp) : dvalue =
  match exp with
  | IEUnit ->
      DVoid
  | IEEmptyValue ->
      DVoid
  | IEEmptyList _ ->
      DList (ref [])
  | IEBool b ->
      DBool b
  | IEInt i ->
      DInt i
  | IEReal f ->
      DReal f
  | IEFixed f ->
      DReal f
  | IEString s ->
      DString s
  | IEVar idx ->
      Array.unsafe_get stack.stack (frame_start + idx)
  | IEConstant idx ->
      evaluateLazyConstant prog.iconstants idx
  | IEUnOp (op, e) ->
      let v = evalIexp ctx prog stack frame_start e in
      evalUnop ctx op v
  | IEOp (op, e1, e2) ->
      let v1 = evalIexp ctx prog stack frame_start e1 in
      let v2 = evalIexp ctx prog stack frame_start e2 in
      evalBinop ctx op v1 v2
  (* Specialized fast arithmetic operations *)
  | IEAddInt (e1, e2) -> (
    match (evalIexp ctx prog stack frame_start e1, evalIexp ctx prog stack frame_start e2) with
    | DInt a, DInt b ->
        DInt (a + b)
    | _ ->
        error_with_context ctx "Type mismatch in integer addition" )
  | IESubInt (e1, e2) -> (
    match (evalIexp ctx prog stack frame_start e1, evalIexp ctx prog stack frame_start e2) with
    | DInt a, DInt b ->
        DInt (a - b)
    | _ ->
        error_with_context ctx "Type mismatch in integer subtraction" )
  | IEMulInt (e1, e2) -> (
    match (evalIexp ctx prog stack frame_start e1, evalIexp ctx prog stack frame_start e2) with
    | DInt a, DInt b ->
        DInt (a * b)
    | _ ->
        error_with_context ctx "Type mismatch in integer multiplication" )
  | IEDivInt (e1, e2) -> (
    match (evalIexp ctx prog stack frame_start e1, evalIexp ctx prog stack frame_start e2) with
    | DInt a, DInt b ->
        DInt (a / b)
    | _ ->
        error_with_context ctx "Type mismatch in integer division" )
  | IEAddInt16 (e1, e2) -> (
    match (evalIexp ctx prog stack frame_start e1, evalIexp ctx prog stack frame_start e2) with
    | DInt16 a, DInt16 b ->
        (* Clamp to int16 range (-32768 to 32767) *)
        let result = a + b in
        let clamped = max (-32768) (min 32767 result) in
        DInt16 clamped
    | _ ->
        error_with_context ctx "Type mismatch in int16 addition" )
  | IESubInt16 (e1, e2) -> (
    match (evalIexp ctx prog stack frame_start e1, evalIexp ctx prog stack frame_start e2) with
    | DInt16 a, DInt16 b ->
        let result = a - b in
        let clamped = max (-32768) (min 32767 result) in
        DInt16 clamped
    | _ ->
        error_with_context ctx "Type mismatch in int16 subtraction" )
  | IEMulInt16 (e1, e2) -> (
    match (evalIexp ctx prog stack frame_start e1, evalIexp ctx prog stack frame_start e2) with
    | DInt16 a, DInt16 b ->
        let result = a * b in
        let clamped = max (-32768) (min 32767 result) in
        DInt16 clamped
    | _ ->
        error_with_context ctx "Type mismatch in int16 multiplication" )
  | IEDivInt16 (e1, e2) -> (
    match (evalIexp ctx prog stack frame_start e1, evalIexp ctx prog stack frame_start e2) with
    | DInt16 a, DInt16 b ->
        let result = a / b in
        let clamped = max (-32768) (min 32767 result) in
        DInt16 clamped
    | _ ->
        error_with_context ctx "Type mismatch in int16 division" )
  | IEAddReal (e1, e2) -> (
    match (evalIexp ctx prog stack frame_start e1, evalIexp ctx prog stack frame_start e2) with
    | DReal a, DReal b ->
        DReal (a +. b)
    | _ ->
        error_with_context ctx "Type mismatch in real addition" )
  | IESubReal (e1, e2) -> (
    match (evalIexp ctx prog stack frame_start e1, evalIexp ctx prog stack frame_start e2) with
    | DReal a, DReal b ->
        DReal (a -. b)
    | _ ->
        error_with_context ctx "Type mismatch in real subtraction" )
  | IEMulReal (e1, e2) -> (
    match (evalIexp ctx prog stack frame_start e1, evalIexp ctx prog stack frame_start e2) with
    | DReal a, DReal b ->
        DReal (a *. b)
    | _ ->
        error_with_context ctx "Type mismatch in real multiplication" )
  | IEDivReal (e1, e2) -> (
    match (evalIexp ctx prog stack frame_start e1, evalIexp ctx prog stack frame_start e2) with
    | DReal a, DReal b ->
        DReal (a /. b)
    | _ ->
        error_with_context ctx "Type mismatch in real division" )
  (* Specialized fast comparison operations *)
  | IEEqInt (e1, e2) -> (
    match (evalIexp ctx prog stack frame_start e1, evalIexp ctx prog stack frame_start e2) with
    | DInt a, DInt b ->
        DBool (a = b)
    | _ ->
        error_with_context ctx "Type mismatch in integer equality" )
  | IEEqInt16 (e1, e2) -> (
    match (evalIexp ctx prog stack frame_start e1, evalIexp ctx prog stack frame_start e2) with
    | DInt16 a, DInt16 b ->
        DBool (a = b)
    | _ ->
        error_with_context ctx "Type mismatch in int16 equality" )
  | IEEqReal (e1, e2) -> (
    match (evalIexp ctx prog stack frame_start e1, evalIexp ctx prog stack frame_start e2) with
    | DReal a, DReal b ->
        DBool (Float.equal a b)
    | _ ->
        error_with_context ctx "Type mismatch in real equality" )
  | IELtInt (e1, e2) -> (
    match (evalIexp ctx prog stack frame_start e1, evalIexp ctx prog stack frame_start e2) with
    | DInt a, DInt b ->
        DBool (a < b)
    | _ ->
        error_with_context ctx "Type mismatch in integer less than" )
  | IELtInt16 (e1, e2) -> (
    match (evalIexp ctx prog stack frame_start e1, evalIexp ctx prog stack frame_start e2) with
    | DInt16 a, DInt16 b ->
        DBool (a < b)
    | _ ->
        error_with_context ctx "Type mismatch in int16 less than" )
  | IELtReal (e1, e2) -> (
    match (evalIexp ctx prog stack frame_start e1, evalIexp ctx prog stack frame_start e2) with
    | DReal a, DReal b ->
        DBool (a < b)
    | _ ->
        error_with_context ctx "Type mismatch in real less than" )
  | IEGtInt (e1, e2) -> (
    match (evalIexp ctx prog stack frame_start e1, evalIexp ctx prog stack frame_start e2) with
    | DInt a, DInt b ->
        DBool (a > b)
    | _ ->
        error_with_context ctx "Type mismatch in integer greater than" )
  | IEGtInt16 (e1, e2) -> (
    match (evalIexp ctx prog stack frame_start e1, evalIexp ctx prog stack frame_start e2) with
    | DInt16 a, DInt16 b ->
        DBool (a > b)
    | _ ->
        error_with_context ctx "Type mismatch in int16 greater than" )
  | IEGtReal (e1, e2) -> (
    match (evalIexp ctx prog stack frame_start e1, evalIexp ctx prog stack frame_start e2) with
    | DReal a, DReal b ->
        DBool (a > b)
    | _ ->
        error_with_context ctx "Type mismatch in real greater than" )
  (* Inlined built-in functions *)
  | IEBuiltinSin e -> (
    match evalIexp ctx prog stack frame_start e with
    | DReal f ->
        DReal (sin f)
    | _ ->
        error_with_context ctx "Type mismatch in sin" )
  | IEBuiltinSinh e -> (
    match evalIexp ctx prog stack frame_start e with
    | DReal f ->
        DReal (sinh f)
    | _ ->
        error_with_context ctx "Type mismatch in sin" )
  | IEBuiltinCosh e -> (
    match evalIexp ctx prog stack frame_start e with
    | DReal f ->
        DReal (cosh f)
    | _ ->
        error_with_context ctx "Type mismatch in cosh" )
  | IEBuiltinTanh e -> (
    match evalIexp ctx prog stack frame_start e with
    | DReal f ->
        DReal (tanh f)
    | _ ->
        error_with_context ctx "Type mismatch in sin" )
  | IEBuiltinCos e -> (
    match evalIexp ctx prog stack frame_start e with
    | DReal f ->
        DReal (cos f)
    | _ ->
        error_with_context ctx "Type mismatch in cos" )
  | IEBuiltinExp e -> (
    match evalIexp ctx prog stack frame_start e with
    | DReal f ->
        DReal (Stdlib.exp f)
    | _ ->
        error_with_context ctx "Type mismatch in exp" )
  | IEBuiltinLog e -> (
    match evalIexp ctx prog stack frame_start e with
    | DReal f ->
        DReal (log f)
    | _ ->
        error_with_context ctx "Type mismatch in log" )
  | IEBuiltinSqrt e -> (
    match evalIexp ctx prog stack frame_start e with
    | DReal f ->
        DReal (sqrt f)
    | _ ->
        error_with_context ctx "Type mismatch in sqrt" )
  | IEBuiltinAbs e -> (
    match evalIexp ctx prog stack frame_start e with
    | DReal f ->
        DReal (abs_float f)
    | DInt i ->
        DInt (abs i)
    | _ ->
        error_with_context ctx "Type mismatch in abs" )
  | IEBuiltinFloor e -> (
    match evalIexp ctx prog stack frame_start e with
    | DReal f ->
        DReal (floor f)
    | _ ->
        error_with_context ctx "Type mismatch in floor" )
  | IEBuiltinPow (e1, e2) -> (
    match (evalIexp ctx prog stack frame_start e1, evalIexp ctx prog stack frame_start e2) with
    | DReal x, DReal y ->
        DReal (x ** y)
    | _ ->
        error_with_context ctx "Type mismatch in pow" )
  | IEBuiltinTan e -> (
    match evalIexp ctx prog stack frame_start e with
    | DReal f ->
        DReal (tan f)
    | _ ->
        error_with_context ctx "Type mismatch in tan" )
  | IEBuiltinLog10 e -> (
    match evalIexp ctx prog stack frame_start e with
    | DReal f ->
        DReal (log10 f)
    | _ ->
        error_with_context ctx "Type mismatch in log10" )
  | IEBuiltinClipReal (x, min_v, max_v) -> (
    match
      ( evalIexp ctx prog stack frame_start x
      , evalIexp ctx prog stack frame_start min_v
      , evalIexp ctx prog stack frame_start max_v )
    with
    | DReal x_val, DReal min_val, DReal max_val ->
        DReal (min (max x_val min_val) max_val)
    | _ ->
        error_with_context ctx "Type mismatch in clip_real" )
  | IEBuiltinClipInt (x, min_v, max_v) -> (
    match
      ( evalIexp ctx prog stack frame_start x
      , evalIexp ctx prog stack frame_start min_v
      , evalIexp ctx prog stack frame_start max_v )
    with
    | DInt x_val, DInt min_val, DInt max_val ->
        DInt (min (max x_val min_val) max_val)
    | _ ->
        error_with_context ctx "Type mismatch in clip_int" )
  (* Constants *)
  | IEBuiltinPi ->
      DReal Float.pi
  | IEBuiltinEps ->
      DReal 1e-18
  | IEBuiltinSamplerate -> (
    match ctx.sample_rate with
    | Some fs ->
        DReal fs
    | None ->
        error_with_context ctx
          "samplerate() requires the -samplerate flag. Use: vult file.vult -eval \"expr\" -samplerate 44100" )
  (* Random functions *)
  | IEBuiltinRandom ->
      DReal (Random.float 1.0)
  | IEBuiltinIrandom ->
      DInt (Random.int Int.max_int)
  (* Type conversion functions *)
  | IEBuiltinReal e -> (
    match evalIexp ctx prog stack frame_start e with
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
  | IEBuiltinInt e -> (
    match evalIexp ctx prog stack frame_start e with
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
  | IEBuiltinInt16 e -> (
    match evalIexp ctx prog stack frame_start e with
    | DReal f ->
        let i = int_of_float f in
        let clamped = max (-32768) (min 32767 i) in
        DInt16 clamped
    | DBool b ->
        DInt16 (if b then 1 else 0)
    | DInt i ->
        let clamped = max (-32768) (min 32767 i) in
        DInt16 clamped
    | DInt16 i ->
        DInt16 i
    | _ ->
        error_with_context ctx "Type mismatch in int16 conversion" )
  | IEBuiltinBool e -> (
    match evalIexp ctx prog stack frame_start e with
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
  | IEBuiltinString e -> (
    match evalIexp ctx prog stack frame_start e with
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
  | IEBuiltinFixed e -> (
    match evalIexp ctx prog stack frame_start e with
    | DReal f ->
        DReal f
    | DInt i ->
        DReal (float_of_int i)
    | _ ->
        error_with_context ctx "Type mismatch in fixed conversion" )
  (* Array/string functions *)
  | IEBuiltinSize e -> (
    match evalIexp ctx prog stack frame_start e with
    | DArray arr ->
        DInt (Array.length arr)
    | _ ->
        error_with_context ctx "Type mismatch in size - expected array" )
  | IEBuiltinLength e -> (
    match evalIexp ctx prog stack frame_start e with
    | DString s ->
        DInt (String.length s)
    | _ ->
        error_with_context ctx "Type mismatch in length - expected string" )
  (* List functions *)
  | IEBuiltinListSize e -> (
    match evalIexp ctx prog stack frame_start e with
    | DArray arr ->
        DInt (Array.length arr)
    | DList list_ref ->
        DInt (CCList.length !list_ref)
    | _ ->
        error_with_context ctx "Type mismatch in list_size - expected list" )
  | IEBuiltinListCapacity e -> (
    (* Lists in the interpreter don't have a separate capacity, return size *)
    match evalIexp ctx prog stack frame_start e with
    | DArray arr ->
        DInt (Array.length arr)
    | DList list_ref ->
        DInt (CCList.length !list_ref)
    | _ ->
        error_with_context ctx "Type mismatch in list_capacity - expected list" )
  | IEBuiltinListAppend (l, v) -> (
      let list_val = evalIexp ctx prog stack frame_start l in
      let new_val = evalIexp ctx prog stack frame_start v in
      match list_val with
      | DList list_ref ->
          list_ref := !list_ref @ [new_val] ;
          DVoid
      | _ ->
          error_with_context ctx "list_append requires list type" )
  | IEBuiltinListInsert (l, i, v) -> (
      let list_val = evalIexp ctx prog stack frame_start l in
      let idx = evalIexp ctx prog stack frame_start i in
      let new_val = evalIexp ctx prog stack frame_start v in
      match (list_val, idx) with
      | DList list_ref, DInt index ->
          let before, after = CCList.take_drop index !list_ref in
          list_ref := before @ [new_val] @ after ;
          DVoid
      | _ ->
          error_with_context ctx "list_insert: invalid arguments" )
  | IEBuiltinListRemove (l, i) -> (
      let list_val = evalIexp ctx prog stack frame_start l in
      let idx = evalIexp ctx prog stack frame_start i in
      match (list_val, idx) with
      | DList list_ref, DInt index ->
          let before, after = CCList.take_drop index !list_ref in
          list_ref := before @ CCList.drop 1 after ;
          DVoid
      | _ ->
          error_with_context ctx "list_remove: invalid arguments" )
  | IEBuiltinListClear l -> (
    match evalIexp ctx prog stack frame_start l with
    | DList list_ref ->
        list_ref := [] ;
        DVoid
    | _ ->
        error_with_context ctx "list_clear requires list type" )
  | IEBuiltinListReserve _ ->
      (* Reserve is a hint, we can safely ignore it and return unit *)
      DVoid
  | IEBuiltinListGet (l, i) -> (
      let list_val = evalIexp ctx prog stack frame_start l in
      let idx = evalIexp ctx prog stack frame_start i in
      match (list_val, idx) with
      | DList list_ref, DInt index -> (
        match CCList.nth_opt !list_ref index with
        | Some v ->
            v
        | None ->
            error_with_context ctx
              ( "list_get: index out of bounds. size = "
              ^ string_of_int (CCList.length !list_ref)
              ^ " index = " ^ string_of_int index ) )
      | _ ->
          error_with_context ctx "list_get: invalid arguments" )
  | IEBuiltinListSet (l, i, v) -> (
      let list_val = evalIexp ctx prog stack frame_start l in
      let idx = evalIexp ctx prog stack frame_start i in
      let new_val = evalIexp ctx prog stack frame_start v in
      match (list_val, idx) with
      | DList list_ref, DInt index ->
          let old_list = !list_ref in
          let len = CCList.length old_list in
          if index >= 0 && index < len then
            let before, after = CCList.take_drop index old_list in
            match after with
            | _ :: rest ->
                list_ref := before @ [new_val] @ rest ;
                DVoid
            | [] ->
                error_with_context ctx "list_set: index out of bounds"
          else
            error_with_context ctx
              ("list_set: index out of bounds. size = " ^ string_of_int len ^ " index = " ^ string_of_int index)
      | _ ->
          error_with_context ctx "list_set: invalid arguments" )
  | IEIndex (e, index) ->
      let arr_val = evalIexp ctx prog stack frame_start e in
      let idx_val = evalIexp ctx prog stack frame_start index in
      getArrayElement ctx arr_val idx_val
  | IEArray elems ->
      let values = Array.of_list (CCList.map (evalIexp ctx prog stack frame_start) elems) in
      DArray values
  | IECall (func_idx, args) ->
      let new_depth = ctx.depth + 1 in
      if new_depth > ctx.max_depth then
        error_with_context ctx
          ("Maximum call depth exceeded in function " ^ (Array.unsafe_get prog.ifunctions_array func_idx).iname) ;
      let func_def = Array.unsafe_get prog.ifunctions_array func_idx in
      let new_ctx = {ctx with frames= func_def.iname :: ctx.frames; depth= new_depth} in
      let arg_vals = CCList.map (evalIexp ctx prog stack frame_start) args in
      callFunction new_ctx prog stack func_idx arg_vals
  | IECallExt _ ->
      error_with_context ctx "External evaluations are not possible"
  | IEIf (cond, then_, else_) -> (
    match evalIexp ctx prog stack frame_start cond with
    | DBool true ->
        evalIexp ctx prog stack frame_start then_
    | DBool false ->
        evalIexp ctx prog stack frame_start else_
    | _ ->
        error_with_context ctx "Invalid condition" )
  | IETuple elems ->
      let values = Array.of_list (CCList.map (evalIexp ctx prog stack frame_start) elems) in
      DArray values
  | IEMember (e, member_idx) ->
      let struct_val = evalIexp ctx prog stack frame_start e in
      getStructMember ctx struct_val member_idx
  | IEVarMember (var_idx, member_idx) -> (
    match Array.unsafe_get stack.stack (frame_start + var_idx) with
    | DStruct members ->
        Array.unsafe_get members member_idx
    | _ ->
        error_with_context ctx "Invalid struct access in fused var-member" )
  | IERecord (descr, elems) ->
      let member_vals = Array.make (CCList.length descr.members) DVoid in
      CCList.iter
        (fun (idx, exp) ->
          let val_ = evalIexp ctx prog stack frame_start exp in
          member_vals.(idx) <- val_ )
        elems ;
      DStruct member_vals

(* ---- Phase 3: Closure Conversion ----

   This phase compiles the optimized iexp/istmt tree into OCaml closures for fast execution.
   Instead of dispatching on the iexp tag on every evaluation (as evalIexp does), we traverse
   the tree once and produce a closure that directly encodes the computation.

   The key insight is that the tree structure is known at compile time, so we can:
   1. Eliminate tag dispatch: each node becomes a direct closure call
   2. Propagate constants: CConstant values are inlined, avoiding closure allocation
   3. Fuse variable access: CVar enables parent closures to inline array reads
   4. Fold constant branches: IEIf/IStmtIf with known conditions compile only the taken path

   The closure signature is uniform: (call_context -> runtime_stack -> frame_start -> result).
   This means every compiled expression/statement takes the same three arguments:
   - ctx: call context with error reporting info and sample rate
   - stack: the shared runtime stack (flat dvalue array)
   - fs: frame start offset — variables are at stack.stack[fs + var_idx]

   Helpers
   -------
   to_closure: converts compiled_result -> compiled_exp (materializes CConstant/CVar as closures)

   compile_binop_xx: create closures for binary operations on two compiled_exp values.
     These are the "base" helpers that take already-materialized closures.

   compile_binop_xx_cp: constant-propagating wrappers that take compiled_result values.
     They check for compile-time optimizations before falling back to the base helpers:
     - CConstant+CConstant: fold at compile time, return CConstant
     - CVar+CVar: fuse both variable reads into a single closure (no intermediate closures)
     - CVar+CConstant / CConstant+CVar: fuse the variable read with the constant
     - Otherwise: materialize via to_closure and delegate to the base helper

   compile_unary_r_cp: same pattern for unary float operations (sin, cos, sqrt, etc.)
*)

(* Convert a compiled_result to a compiled_exp closure.
   CConstant becomes a closure that ignores its arguments and returns the value.
   CVar becomes a direct array read. CDynamic is already a closure. *)
let to_closure (r : compiled_result) : compiled_exp =
  match r with
  | CConstant v ->
      fun _ctx _stack _fs -> v
  | CVar idx ->
      fun _ctx stack fs -> Array.unsafe_get stack.stack (fs + idx)
  | CDynamic f ->
      f

(* Base closure-building helpers for binary and unary operations.
   These take compiled_exp (already-materialized closures) and produce a new compiled_exp
   that evaluates both operands and applies the operation. They do no compile-time optimization
   — that's handled by the _cp variants below. *)
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

(* Constant-propagating and CVar-fusing helpers.
   These take compiled_result (which may be CConstant, CVar, or CDynamic) and try to
   produce the most efficient result:
   - Two constants: fold at compile time -> CConstant
   - One or both CVar: generate a fused closure with direct array access -> CDynamic
   - Otherwise: fall back to to_closure + base helper -> CDynamic *)
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

(* Compile an iexp into a compiled_result.
   This is the core of Phase 3. Each iexp node is classified:
   - CConstant: value known at compile time (literals, pi, folded arithmetic, lazy constants)
   - CVar: direct variable read (enables fusion in parent nodes)
   - CDynamic: runtime closure (everything else)

   For "conditionally constant" nodes (arithmetic, builtins, etc.), we first compile
   the children, then check if they're all constant. If so, we fold; otherwise we
   generate a closure, potentially fused if children are CVar. *)
let rec compileIexp (prog : iprog) (exp : iexp) : compiled_result =
  match exp with
  (* -- Literals: always CConstant, zero runtime cost -- *)
  | IEUnit ->
      CConstant DVoid
  | IEEmptyValue ->
      CConstant DVoid
  | IEBool b ->
      CConstant (DBool b)
  | IEInt i ->
      CConstant (DInt i)
  | IEReal f ->
      CConstant (DReal f)
  | IEFixed f ->
      CConstant (DReal f)
  | IEString s ->
      CConstant (DString s)
  | IEBuiltinPi ->
      CConstant (DReal Float.pi)
  | IEBuiltinEps ->
      CConstant (DReal 1e-18)
  (* -- Empty list: always dynamic because each `[]` must allocate a fresh mutable ref -- *)
  | IEEmptyList _ ->
      CDynamic (fun _ctx _stack _fs -> DList (ref []))
  (* -- Variable: CVar enables parent nodes to generate fused closures -- *)
  | IEVar idx ->
      CVar idx
  (* -- Fused variable.member access: always dynamic (depends on runtime stack) -- *)
  | IEVarMember (var_idx, member_idx) ->
      CDynamic
        (fun ctx stack fs ->
          match Array.unsafe_get stack.stack (fs + var_idx) with
          | DStruct members ->
              Array.unsafe_get members member_idx
          | _ ->
              error_with_context ctx "Invalid struct access" )
  (* -- Runtime-dependent builtins -- *)
  | IEBuiltinSamplerate ->
      CDynamic
        (fun ctx _stack _fs ->
          match ctx.sample_rate with
          | Some fs ->
              DReal fs
          | None ->
              error_with_context ctx "samplerate() requires the -samplerate flag" )
  (* -- Side-effectful: always dynamic -- *)
  | IEBuiltinRandom ->
      CDynamic (fun _ctx _stack _fs -> DReal (Random.float 1.0))
  | IEBuiltinIrandom ->
      CDynamic (fun _ctx _stack _fs -> DInt (Random.int Int.max_int))
  (* -- Lazy constant: force evaluation at compile time and cache as CConstant.
     This avoids creating a closure that calls evaluateLazyConstant on every access. -- *)
  | IEConstant idx ->
      CConstant (evaluateLazyConstant prog.iconstants idx)
  (* -- Generic operators: fold if both operands are CConstant, otherwise create closure.
     IEUnOp/IEOp are the "unspecialized" forms that survived Phase 1 (rare after
     type specialization, but possible for unusual types). -- *)
  | IEUnOp (op, e1) -> (
      let r1 = compileIexp prog e1 in
      match r1 with
      | CConstant v ->
          let dummy_ctx = {frames= []; depth= 0; max_depth= 0; sample_rate= None} in
          CConstant (evalUnop dummy_ctx op v)
      | _ ->
          let f1 = to_closure r1 in
          CDynamic (fun ctx stack fs -> evalUnop ctx op (f1 ctx stack fs)) )
  | IEOp (op, e1, e2) -> (
      let r1 = compileIexp prog e1 in
      let r2 = compileIexp prog e2 in
      match (r1, r2) with
      | CConstant v1, CConstant v2 ->
          let dummy_ctx = {frames= []; depth= 0; max_depth= 0; sample_rate= None} in
          CConstant (evalBinop dummy_ctx op v1 v2)
      | _ ->
          let f1 = to_closure r1 in
          let f2 = to_closure r2 in
          CDynamic (fun ctx stack fs -> evalBinop ctx op (f1 ctx stack fs) (f2 ctx stack fs)) )
  (* -- Specialized arithmetic: delegates to _cp helpers which handle
     CConstant folding, CVar fusion, and CVar+CConstant mixed cases -- *)
  | IEAddInt (e1, e2) ->
      compile_binop_ii_cp (compileIexp prog e1) (compileIexp prog e2)
        (fun a b -> DInt (a + b))
        "Type mismatch in integer addition"
  | IESubInt (e1, e2) ->
      compile_binop_ii_cp (compileIexp prog e1) (compileIexp prog e2)
        (fun a b -> DInt (a - b))
        "Type mismatch in integer subtraction"
  | IEMulInt (e1, e2) ->
      compile_binop_ii_cp (compileIexp prog e1) (compileIexp prog e2)
        (fun a b -> DInt (a * b))
        "Type mismatch in integer multiplication"
  | IEDivInt (e1, e2) ->
      compile_binop_ii_cp (compileIexp prog e1) (compileIexp prog e2)
        (fun a b -> DInt (a / b))
        "Type mismatch in integer division"
  | IEAddInt16 (e1, e2) ->
      compile_binop_i16_cp (compileIexp prog e1) (compileIexp prog e2) ( + ) "Type mismatch in int16 addition"
  | IESubInt16 (e1, e2) ->
      compile_binop_i16_cp (compileIexp prog e1) (compileIexp prog e2) ( - ) "Type mismatch in int16 subtraction"
  | IEMulInt16 (e1, e2) ->
      compile_binop_i16_cp (compileIexp prog e1) (compileIexp prog e2) ( * ) "Type mismatch in int16 multiplication"
  | IEDivInt16 (e1, e2) ->
      compile_binop_i16_cp (compileIexp prog e1) (compileIexp prog e2) ( / ) "Type mismatch in int16 division"
  | IEAddReal (e1, e2) ->
      compile_binop_rr_cp (compileIexp prog e1) (compileIexp prog e2)
        (fun a b -> DReal (a +. b))
        "Type mismatch in real addition"
  | IESubReal (e1, e2) ->
      compile_binop_rr_cp (compileIexp prog e1) (compileIexp prog e2)
        (fun a b -> DReal (a -. b))
        "Type mismatch in real subtraction"
  | IEMulReal (e1, e2) ->
      compile_binop_rr_cp (compileIexp prog e1) (compileIexp prog e2)
        (fun a b -> DReal (a *. b))
        "Type mismatch in real multiplication"
  | IEDivReal (e1, e2) ->
      compile_binop_rr_cp (compileIexp prog e1) (compileIexp prog e2)
        (fun a b -> DReal (a /. b))
        "Type mismatch in real division"
  | IEEqInt (e1, e2) ->
      compile_binop_ii_cp (compileIexp prog e1) (compileIexp prog e2)
        (fun a b -> DBool (a = b))
        "Type mismatch in integer equality"
  | IEEqInt16 (e1, e2) -> (
      let r1 = compileIexp prog e1 in
      let r2 = compileIexp prog e2 in
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
                  error_with_context ctx "Type mismatch in int16 equality" ) )
  | IEEqReal (e1, e2) ->
      compile_binop_rr_cp (compileIexp prog e1) (compileIexp prog e2)
        (fun a b -> DBool (Float.equal a b))
        "Type mismatch in real equality"
  | IELtInt (e1, e2) ->
      compile_binop_ii_cp (compileIexp prog e1) (compileIexp prog e2)
        (fun a b -> DBool (a < b))
        "Type mismatch in integer less than"
  | IELtInt16 (e1, e2) -> (
      let r1 = compileIexp prog e1 in
      let r2 = compileIexp prog e2 in
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
                  error_with_context ctx "Type mismatch in int16 less than" ) )
  | IELtReal (e1, e2) ->
      compile_binop_rr_cp (compileIexp prog e1) (compileIexp prog e2)
        (fun a b -> DBool (a < b))
        "Type mismatch in real less than"
  | IEGtInt (e1, e2) ->
      compile_binop_ii_cp (compileIexp prog e1) (compileIexp prog e2)
        (fun a b -> DBool (a > b))
        "Type mismatch in integer greater than"
  | IEGtInt16 (e1, e2) -> (
      let r1 = compileIexp prog e1 in
      let r2 = compileIexp prog e2 in
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
                  error_with_context ctx "Type mismatch in int16 greater than" ) )
  | IEGtReal (e1, e2) ->
      compile_binop_rr_cp (compileIexp prog e1) (compileIexp prog e2)
        (fun a b -> DBool (a > b))
        "Type mismatch in real greater than"
  (* -- Math builtins: delegate to compile_unary_r_cp which handles:
     - CConstant(DReal f) -> CConstant(DReal(op f))  (fold at compile time)
     - CVar idx -> fused closure: read stack[fs+idx], apply op  (one closure call)
     - CDynamic f -> closure chain: call f, apply op  (two closure calls) -- *)
  | IEBuiltinSin e ->
      compile_unary_r_cp (compileIexp prog e) sin "Type mismatch in sin"
  | IEBuiltinSinh e ->
      compile_unary_r_cp (compileIexp prog e) sinh "Type mismatch in sinh"
  | IEBuiltinCosh e ->
      compile_unary_r_cp (compileIexp prog e) cosh "Type mismatch in cosh"
  | IEBuiltinTanh e ->
      compile_unary_r_cp (compileIexp prog e) tanh "Type mismatch in tanh"
  | IEBuiltinCos e ->
      compile_unary_r_cp (compileIexp prog e) cos "Type mismatch in cos"
  | IEBuiltinTan e ->
      compile_unary_r_cp (compileIexp prog e) tan "Type mismatch in tan"
  | IEBuiltinExp e ->
      compile_unary_r_cp (compileIexp prog e) Stdlib.exp "Type mismatch in exp"
  | IEBuiltinLog e ->
      compile_unary_r_cp (compileIexp prog e) log "Type mismatch in log"
  | IEBuiltinLog10 e ->
      compile_unary_r_cp (compileIexp prog e) log10 "Type mismatch in log10"
  | IEBuiltinSqrt e ->
      compile_unary_r_cp (compileIexp prog e) sqrt "Type mismatch in sqrt"
  | IEBuiltinFloor e ->
      compile_unary_r_cp (compileIexp prog e) floor "Type mismatch in floor"
  | IEBuiltinAbs e -> (
      let r1 = compileIexp prog e in
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
                  error_with_context ctx "Type mismatch in abs" ) )
  | IEBuiltinPow (e1, e2) ->
      compile_binop_rr_cp (compileIexp prog e1) (compileIexp prog e2) (fun a b -> DReal (a ** b)) "Type mismatch in pow"
  (* clip(x, min, max) — very common in DSP: clip(signal, -1.0, 1.0).
     Three optimization tiers:
     1. All constant: fold at compile time
     2. CVar + constant bounds: single fused closure (common case: clip(x, 0.0, 1.0))
     3. General: three closure calls *)
  | IEBuiltinClipReal (x, min_v, max_v) -> (
      let rx = compileIexp prog x in
      let rmin = compileIexp prog min_v in
      let rmax = compileIexp prog max_v in
      match (rx, rmin, rmax) with
      | CConstant (DReal xv), CConstant (DReal minv), CConstant (DReal maxv) ->
          CConstant (DReal (Stdlib.min (Stdlib.max xv minv) maxv))
      (* Fused: read var directly, apply clip with constant bounds *)
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
                  error_with_context ctx "Type mismatch in clip_real" ) )
  | IEBuiltinClipInt (x, min_v, max_v) -> (
      let rx = compileIexp prog x in
      let rmin = compileIexp prog min_v in
      let rmax = compileIexp prog max_v in
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
                  error_with_context ctx "Type mismatch in clip_int" ) )
  (* -- Type conversions: fold CConstant values at compile time, fuse CVar reads.
     These are common in DSP code (e.g., real(counter) in a loop). -- *)
  | IEBuiltinReal e -> (
      let r1 = compileIexp prog e in
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
                  error_with_context ctx "Type mismatch in real conversion" ) )
  | IEBuiltinInt e -> (
      let r1 = compileIexp prog e in
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
                  error_with_context ctx "Type mismatch in int conversion" ) )
  | IEBuiltinInt16 e -> (
      let r1 = compileIexp prog e in
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
                  error_with_context ctx "Type mismatch in int16 conversion" ) )
  | IEBuiltinBool e -> (
      let r1 = compileIexp prog e in
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
                  error_with_context ctx "Type mismatch in bool conversion" ) )
  | IEBuiltinString e -> (
      let r1 = compileIexp prog e in
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
                  error_with_context ctx "Type mismatch in string conversion" ) )
  | IEBuiltinFixed e -> (
      let r1 = compileIexp prog e in
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
                  error_with_context ctx "Type mismatch in fixed conversion" ) )
  (* -- Array/string intrinsics: size and length are foldable on constant arrays/strings -- *)
  | IEBuiltinSize e -> (
      let r1 = compileIexp prog e in
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
                  error_with_context ctx "Type mismatch in size" ) )
  | IEBuiltinLength e -> (
      let r1 = compileIexp prog e in
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
                  error_with_context ctx "Type mismatch in length" ) )
  (* -- List functions: always dynamic because lists are mutable (DList wraps a ref).
     We still use to_closure on arguments to benefit from CVar/CConstant fusion
     in the argument expressions, but the list operation itself must be a closure. -- *)
  | IEBuiltinListSize e ->
      let f1 = to_closure (compileIexp prog e) in
      CDynamic
        (fun ctx stack fs ->
          match f1 ctx stack fs with
          | DArray arr ->
              DInt (Array.length arr)
          | DList list_ref ->
              DInt (CCList.length !list_ref)
          | _ ->
              error_with_context ctx "Type mismatch in list_size" )
  | IEBuiltinListCapacity e ->
      let f1 = to_closure (compileIexp prog e) in
      CDynamic
        (fun ctx stack fs ->
          match f1 ctx stack fs with
          | DArray arr ->
              DInt (Array.length arr)
          | DList list_ref ->
              DInt (CCList.length !list_ref)
          | _ ->
              error_with_context ctx "Type mismatch in list_capacity" )
  | IEBuiltinListAppend (l, v) ->
      let fl = to_closure (compileIexp prog l) in
      let fv = to_closure (compileIexp prog v) in
      CDynamic
        (fun ctx stack fs ->
          match fl ctx stack fs with
          | DList list_ref ->
              list_ref := !list_ref @ [fv ctx stack fs] ;
              DVoid
          | _ ->
              error_with_context ctx "list_append requires list type" )
  | IEBuiltinListInsert (l, i, v) ->
      let fl = to_closure (compileIexp prog l) in
      let fi = to_closure (compileIexp prog i) in
      let fv = to_closure (compileIexp prog v) in
      CDynamic
        (fun ctx stack fs ->
          match (fl ctx stack fs, fi ctx stack fs) with
          | DList list_ref, DInt index ->
              let before, after = CCList.take_drop index !list_ref in
              list_ref := before @ [fv ctx stack fs] @ after ;
              DVoid
          | _ ->
              error_with_context ctx "list_insert: invalid arguments" )
  | IEBuiltinListRemove (l, i) ->
      let fl = to_closure (compileIexp prog l) in
      let fi = to_closure (compileIexp prog i) in
      CDynamic
        (fun ctx stack fs ->
          match (fl ctx stack fs, fi ctx stack fs) with
          | DList list_ref, DInt index ->
              let before, after = CCList.take_drop index !list_ref in
              list_ref := before @ CCList.drop 1 after ;
              DVoid
          | _ ->
              error_with_context ctx "list_remove: invalid arguments" )
  | IEBuiltinListClear l ->
      let fl = to_closure (compileIexp prog l) in
      CDynamic
        (fun ctx stack fs ->
          match fl ctx stack fs with
          | DList list_ref ->
              list_ref := [] ;
              DVoid
          | _ ->
              error_with_context ctx "list_clear requires list type" )
  | IEBuiltinListReserve _ ->
      CConstant DVoid
  | IEBuiltinListGet (l, i) ->
      let fl = to_closure (compileIexp prog l) in
      let fi = to_closure (compileIexp prog i) in
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
  | IEBuiltinListSet (l, i, v) ->
      let fl = to_closure (compileIexp prog l) in
      let fi = to_closure (compileIexp prog i) in
      let fv = to_closure (compileIexp prog v) in
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
  (* -- Array indexing: constant if both the array and the index are known at compile time.
     This handles cases like `table[3]` where `table` is a constant array. -- *)
  | IEIndex (e, index) -> (
      let re = compileIexp prog e in
      let ri = compileIexp prog index in
      match (re, ri) with
      | CConstant (DArray arr), CConstant (DInt i) when i >= 0 && i < Array.length arr ->
          CConstant arr.(i)
      | _ ->
          let fe = to_closure re in
          let fi = to_closure ri in
          CDynamic (fun ctx stack fs -> getArrayElement ctx (fe ctx stack fs) (fi ctx stack fs)) )
  (* -- Array literal: if every element is CConstant, build the array at compile time.
     Otherwise, each element closure is evaluated at runtime. -- *)
  | IEArray elems ->
      let relems = CCList.map (compileIexp prog) elems in
      if CCList.for_all (fun r -> match r with CConstant _ -> true | CVar _ | CDynamic _ -> false) relems then
        CConstant
          (DArray
             (Array.of_list
                (CCList.map (fun r -> match r with CConstant v -> v | CVar _ | CDynamic _ -> DVoid) relems) ) )
      else
        let felems = CCList.map to_closure relems in
        CDynamic (fun ctx stack fs -> DArray (Array.of_list (CCList.map (fun f -> f ctx stack fs) felems)))
  (* -- Function calls: always dynamic. Arguments are materialized via to_closure.
     The compiled function is looked up from prog.compiled_functions at runtime
     (not compile time) to handle mutual recursion correctly. -- *)
  | IECall (func_idx, args) ->
      let fargs = Array.of_list (CCList.map (fun a -> to_closure (compileIexp prog a)) args) in
      let n_args = Array.length fargs in
      CDynamic
        (fun ctx stack fs ->
          let new_depth = ctx.depth + 1 in
          if new_depth > ctx.max_depth then error_with_context ctx "Maximum call depth exceeded" ;
          let cfunc = Array.unsafe_get prog.compiled_functions func_idx in
          let new_ctx = {ctx with frames= cfunc.cf_name :: ctx.frames; depth= new_depth} in
          let arg_vals = List.init n_args (fun i -> (Array.unsafe_get fargs i) ctx stack fs) in
          callCompiledFunction new_ctx stack cfunc arg_vals )
  | IECallExt _ ->
      CDynamic (fun ctx _stack _fs -> error_with_context ctx "External evaluations are not possible")
  (* -- Conditional: if the condition is a compile-time constant, eliminate the dead
     branch entirely — only the taken branch is compiled. This is especially useful
     after constant propagation resolves flag-like conditions. -- *)
  | IEIf (cond, then_, else_) -> (
      let rc = compileIexp prog cond in
      match rc with
      | CConstant (DBool true) ->
          compileIexp prog then_
      | CConstant (DBool false) ->
          compileIexp prog else_
      | _ ->
          let fc = to_closure rc in
          let ft = to_closure (compileIexp prog then_) in
          let ff = to_closure (compileIexp prog else_) in
          CDynamic
            (fun ctx stack fs ->
              match fc ctx stack fs with
              | DBool true ->
                  ft ctx stack fs
              | DBool false ->
                  ff ctx stack fs
              | _ ->
                  error_with_context ctx "Invalid condition" ) )
  (* -- Tuple: same strategy as array literal — fold if all elements are constant -- *)
  | IETuple elems ->
      let relems = CCList.map (compileIexp prog) elems in
      if CCList.for_all (fun r -> match r with CConstant _ -> true | CVar _ | CDynamic _ -> false) relems then
        CConstant
          (DArray
             (Array.of_list
                (CCList.map (fun r -> match r with CConstant v -> v | CVar _ | CDynamic _ -> DVoid) relems) ) )
      else
        let felems = CCList.map to_closure relems in
        CDynamic (fun ctx stack fs -> DArray (Array.of_list (CCList.map (fun f -> f ctx stack fs) felems)))
  (* -- Member access: if the struct is a compile-time constant, extract the field directly -- *)
  | IEMember (e, member_idx) -> (
      let re = compileIexp prog e in
      match re with
      | CConstant (DStruct members) when member_idx >= 0 && member_idx < Array.length members ->
          CConstant members.(member_idx)
      | _ ->
          let fe = to_closure re in
          CDynamic (fun ctx stack fs -> getStructMember ctx (fe ctx stack fs) member_idx) )
  (* -- Record literal: if all member values are CConstant, build the struct at compile time -- *)
  | IERecord (descr, elems) ->
      let n_members = CCList.length descr.members in
      let relems = CCList.map (fun (idx, e) -> (idx, compileIexp prog e)) elems in
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

(* Execute a compiled function: allocate a stack frame, bind arguments, run the body.
   The frame is cf_locals slots starting at stack.sp. After execution, the frame is
   deallocated by restoring sp. Returns DVoid for void functions (Continue), or the
   returned value for Return v. *)
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

(* Compile an ilexp for rvalue reading.
   This is used when we need to read the current value at an lvalue location,
   e.g., reading a struct member before writing a sibling, or reading an array
   element for an indexed assignment. Returns compiled_exp (not compiled_result)
   because lvalues are always dynamic — they depend on the runtime stack. *)
and compileIlexpAsRvalue (prog : iprog) (lexp : ilexp) : compiled_exp =
  match lexp with
  | ILWild ->
      fun ctx _stack _fs -> error_with_context ctx "Cannot evaluate wildcard as rvalue"
  | ILVar idx ->
      fun _ctx stack fs -> Array.unsafe_get stack.stack (fs + idx)
  | ILVarMember (var_idx, member_idx) -> (
      fun ctx stack fs ->
        match Array.unsafe_get stack.stack (fs + var_idx) with
        | DStruct members ->
            Array.unsafe_get members member_idx
        | _ ->
            error_with_context ctx "Invalid struct access in fused var-member lvalue" )
  | ILMember (e, member_idx) -> (
      let fe = compileIlexpAsRvalue prog e in
      fun ctx stack fs ->
        match fe ctx stack fs with
        | DStruct members when member_idx >= 0 && member_idx < Array.length members ->
            members.(member_idx)
        | _ ->
            error_with_context ctx "Invalid struct member access" )
  | ILIndex (e, index) -> (
      let fe = compileIlexpAsRvalue prog e in
      let fi = to_closure (compileIexp prog index) in
      fun ctx stack fs ->
        match (fe ctx stack fs, fi ctx stack fs) with
        | DArray arr, DInt i when i >= 0 && i < Array.length arr ->
            arr.(i)
        | _ ->
            error_with_context ctx "Invalid array access" )
  | ILTuple _ ->
      fun ctx _stack _fs -> error_with_context ctx "Cannot evaluate tuple lvalue as rvalue"

(* Compile an assignment to an lvalue location.
   Returns a closure that takes (ctx, stack, frame_start, value) and writes the value
   to the appropriate location. For tuples, the value is destructured and each element
   is assigned to the corresponding lvalue. Index expressions in ILIndex are compiled
   via compileIexp and materialized with to_closure. *)
and compileAssignIlvalue (prog : iprog) (lexp : ilexp) : call_context -> runtime_stack -> int -> dvalue -> unit =
  match lexp with
  | ILWild ->
      fun _ctx _stack _fs _val -> ()
  | ILVar idx ->
      fun _ctx stack fs val_ -> Array.unsafe_set stack.stack (fs + idx) val_
  | ILVarMember (var_idx, member_idx) -> (
      fun ctx stack fs val_ ->
        match Array.unsafe_get stack.stack (fs + var_idx) with
        | DStruct members ->
            Array.unsafe_set members member_idx val_
        | _ ->
            error_with_context ctx "Invalid struct access in fused var-member assignment" )
  | ILMember (e, member_idx) -> (
      let fe = compileIlexpAsRvalue prog e in
      fun ctx stack fs val_ ->
        match fe ctx stack fs with
        | DStruct members when member_idx >= 0 && member_idx < Array.length members ->
            members.(member_idx) <- val_
        | _ ->
            error_with_context ctx "Invalid struct member assignment" )
  | ILIndex (e, index) -> (
      let fe = compileIlexpAsRvalue prog e in
      let fi = to_closure (compileIexp prog index) in
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
  | ILTuple lexps -> (
      let fassigns = CCList.mapi (fun i lv -> (i, compileAssignIlvalue prog lv)) lexps in
      fun ctx stack fs val_ ->
        match val_ with
        | DArray vals when Array.length vals = CCList.length fassigns ->
            CCList.iter (fun (i, fassign) -> fassign ctx stack fs vals.(i)) fassigns
        | _ ->
            error_with_context ctx "Tuple assignment type mismatch" )

(* Compile a statement to a closure.
   Each statement becomes a compiled_stmt: (ctx -> stack -> fs -> exec_result).
   Expressions within statements are compiled via compileIexp and materialized
   with to_closure. The constant propagation from compileIexp carries through:
   - IStmtIf with a constant condition compiles only the taken branch
   - IStmtDecl/IStmtBind with a constant initializer still creates a closure
     (to write to the stack), but the value computation is free *)
and compileIstmt (prog : iprog) (stmt : istmt) : compiled_stmt =
  match stmt with
  | IStmtDecl (var_idx, typ, init_exp) -> (
    match init_exp with
    | Some exp ->
        let fexp = to_closure (compileIexp prog exp) in
        fun ctx stack fs ->
          Array.unsafe_set stack.stack (fs + var_idx) (fexp ctx stack fs) ;
          Continue
    | None ->
        (* Must call defaultValue at runtime to avoid sharing mutable structs/arrays *)
        fun _ctx stack fs ->
          Array.unsafe_set stack.stack (fs + var_idx) (defaultValue typ) ;
          Continue )
  (* Specialized StmtBind cases: fuse common LHS patterns to avoid the
     indirect fassign closure call on every execution. *)
  | IStmtBind (ILWild, exp) ->
      let fexp = to_closure (compileIexp prog exp) in
      fun ctx stack fs ->
        let _ = fexp ctx stack fs in
        Continue
  | IStmtBind (ILVar idx, exp) ->
      let fexp = to_closure (compileIexp prog exp) in
      fun ctx stack fs ->
        Array.unsafe_set stack.stack (fs + idx) (fexp ctx stack fs) ;
        Continue
  | IStmtBind (ILVarMember (var_idx, member_idx), exp) ->
      let fexp = to_closure (compileIexp prog exp) in
      fun ctx stack fs ->
        let val_ = fexp ctx stack fs in
        ( match Array.unsafe_get stack.stack (fs + var_idx) with
        | DStruct members ->
            Array.unsafe_set members member_idx val_
        | _ ->
            error_with_context ctx "Invalid struct access in fused var-member assignment" ) ;
        Continue
  | IStmtBind (ILIndex (ILVar var_idx, index_exp), exp) ->
      let fi = to_closure (compileIexp prog index_exp) in
      let fexp = to_closure (compileIexp prog exp) in
      fun ctx stack fs ->
        let val_ = fexp ctx stack fs in
        ( match (Array.unsafe_get stack.stack (fs + var_idx), fi ctx stack fs) with
        | DArray arr, DInt i when i >= 0 && i < Array.length arr ->
            arr.(i) <- val_
        | _ ->
            error_with_context ctx "Invalid array assignment" ) ;
        Continue
  | IStmtBind (ILIndex (ILVarMember (var_idx, member_idx), index_exp), exp) ->
      let fi = to_closure (compileIexp prog index_exp) in
      let fexp = to_closure (compileIexp prog exp) in
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
  | IStmtBind (lexp, exp) ->
      let fexp = to_closure (compileIexp prog exp) in
      let fassign = compileAssignIlvalue prog lexp in
      fun ctx stack fs ->
        fassign ctx stack fs (fexp ctx stack fs) ;
        Continue
  | IStmtReturn exp ->
      let fexp = to_closure (compileIexp prog exp) in
      fun ctx stack fs -> Return (fexp ctx stack fs)
  (* Block: compile all sub-statements into an array and loop through them at runtime.
     Early return (Return v) short-circuits the loop. *)
  | IStmtBlock stmts ->
      let fstmts = Array.of_list (CCList.map (compileIstmt prog) stmts) in
      let n = Array.length fstmts in
      fun ctx stack fs ->
        let rec loop (i : int) : exec_result =
          if i >= n then Continue
          else match (Array.unsafe_get fstmts i) ctx stack fs with Continue -> loop (i + 1) | Return v -> Return v
        in
        loop 0
  (* If: check condition at compile time for dead branch elimination *)
  | IStmtIf (cond, then_stmt, else_stmt) -> (
      let rcond = compileIexp prog cond in
      match rcond with
      (* Constant true: compile only the then-branch, discard else entirely *)
      | CConstant (DBool true) ->
          compileIstmt prog then_stmt
      (* Constant false: compile only the else-branch (or no-op if absent) *)
      | CConstant (DBool false) -> (
        match else_stmt with Some else_s -> compileIstmt prog else_s | None -> fun _ctx _stack _fs -> Continue )
      | _ -> (
          let fcond = to_closure rcond in
          let fthen = compileIstmt prog then_stmt in
          match else_stmt with
          | Some else_s -> (
              let felse = compileIstmt prog else_s in
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
  | IStmtWhile (cond, body) ->
      let fcond = to_closure (compileIexp prog cond) in
      let fbody = compileIstmt prog body in
      fun ctx stack fs ->
        let rec loop () : exec_result =
          match fcond ctx stack fs with
          | DBool true -> (
            match fbody ctx stack fs with Continue -> loop () | Return v -> Return v )
          | _ ->
              Continue
        in
        loop ()
  | IStmtSwitch (exp, cases, default) ->
      let fexp = to_closure (compileIexp prog exp) in
      let fcases =
        Array.of_list
          (CCList.map
             (fun (case_exp, case_stmt) -> (to_closure (compileIexp prog case_exp), compileIstmt prog case_stmt))
             cases )
      in
      let n_cases = Array.length fcases in
      let fdefault = Option.map (compileIstmt prog) default in
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

(* Compile a single function definition into a compiled_func closure bundle. *)
let compileFunction (prog : iprog) (func : ifunc_def) : compiled_func =
  {cf_name= func.iname; cf_args= func.iargs; cf_locals= func.ilocals; cf_body= compileIstmt prog func.ibody}

(* Compile all functions in the program into closure form.
   This is the main entry point for Phase 3. After this, callCompiledFunctionByIdx
   can be used to execute any function. Must be called after extendProgram and
   optimizeProgram to ensure the iexp trees are fully optimized. *)
let compileProgram (prog : iprog) : unit =
  let num_funcs = Map.cardinal prog.ifunction_names in
  let cfuncs =
    Array.init num_funcs (fun i ->
        if i < Array.length prog.ifunctions_array then compileFunction prog prog.ifunctions_array.(i)
        else {cf_name= ""; cf_args= []; cf_locals= 0; cf_body= (fun _ctx _stack _fs -> Continue)} )
  in
  prog.compiled_functions <- cfuncs

(* Calls a compiled function by index *)
let callCompiledFunctionByIdx (prog : iprog) (ctx : call_context) (stack : runtime_stack) (func_idx : int)
    (args : dvalue list) : dvalue =
  let cfunc = Array.unsafe_get prog.compiled_functions func_idx in
  callCompiledFunction ctx stack cfunc args

(* ---- Public Entry Points ---- *)

(* Evaluate a single Vult expression string in the context of an existing program.
   Wraps the expression in a temporary function, type-checks, lowers, optimizes,
   extends the iprog, compiles to closures, and executes. Used by the -eval CLI flag. *)
let evaluateMainExpression args env iprog exp : dvalue =
  let e = Pparser.Parse.parseString (Some "Main_.vult") (Pla.print {%pla|fun _main_() return <#exp#s>;|}) in
  let env, main = Typechecking.typecheck_single args env e in
  let _, main = Toprog.convert args env main in
  let main = Passes.run args main in
  (*let () = print_endline (Pla.print (Prog.Print.print_prog main)) in*)
  let iprog = extendProgram iprog main in
  (* Compile to closures for fast evaluation *)
  compileProgram iprog ;
  (* Look for the new function Main___main_ in the bytecode function table *)
  let main_func_name = "Main___main_" in
  match Map.find_opt main_func_name iprog.ifunctions with
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
  let iprog = extendProgram iprog main in
  (* Compile to closures for fast evaluation *)
  compileProgram iprog ;
  (* Execute wrapper function *)
  let main_func_name = "Render___main" in
  let initial_ctx = {frames= []; depth= 0; max_depth= 50; sample_rate= args.fs} in
  let stack = createStack 10000 in
  (* Prepare call arguments - CRITICAL ADDITION *)
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

(* External API: call a compiled function by index. Compiles all functions on first call
   if not already compiled (lazy initialization). Used by the test harness and benchmarks. *)
let callFunctionEntry (prog : iprog) (stack : runtime_stack) (func_idx : int) (args : dvalue list) : dvalue =
  (* Compile if needed *)
  if Array.length prog.compiled_functions = 0 then compileProgram prog ;
  let initial_ctx = {frames= []; depth= 0; max_depth= 50; sample_rate= None} in
  callCompiledFunctionByIdx prog initial_ctx stack func_idx args
