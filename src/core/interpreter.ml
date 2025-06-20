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

open Prog
open Util.Maps

(* Runtime values *)
type dvalue =
  | DVoid
  | DInt of int
  | DInt16 of int
  | DReal of float
  | DBool of bool
  | DString of string
  | DArray of dvalue array
  | DStruct of dvalue array

(* Expression with resolved indices *)
type iexp =
  | IEUnit
  | IEEmptyValue
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
  | IEIndex of iexp * iexp
  | IEArray of iexp list
  | IECall of int * iexp list (* Function index and args *)
  | IECallExt of string * iexp list (* External function name and args *)
  | IEIf of iexp * iexp * iexp
  | IETuple of iexp list
  | IEMember of iexp * int (* Struct and member index *)
  | IERecord of struct_descr * (int * iexp) list (* Type and (member_idx, value) list *)

(* Left-value with resolved indices *)
and ilexp =
  | ILWild
  | ILVar of int (* Variable index in frame *)
  | ILMember of ilexp * int (* Struct and member index *)
  | ILIndex of ilexp * iexp
  | ILTuple of ilexp list

(* Statement *)
and istmt =
  | IStmtDecl of int * type_ * iexp option (* var_index, type, init *)
  | IStmtBind of ilexp * iexp
  | IStmtReturn of iexp
  | IStmtBlock of istmt list
  | IStmtIf of iexp * istmt * istmt option
  | IStmtWhile of iexp * istmt
  | IStmtSwitch of iexp * (iexp * istmt) list * istmt option

(* Function definition *)
type ifunc_def =
  { iname : string
  ; iargs : int list (* Parameter indices *)
  ; iret_type : type_
  ; ilocals : int (* Number of local variables *)
  ; ibody : istmt
  }

(* Forward declaration for lazy evaluation *)
type constant_value =
  | Evaluated of dvalue
  | Unevaluated of iexp * eval_context (* Expression and minimal evaluation context *)

(* Evaluation context for lazy constants *)
and eval_context =
  { ifunctions_array : ifunc_def array
  ; ifunction_names : int Map.t
  ; iconstants_ref : constant_value array ref (* Reference to allow mutation *)
  }

(* Program *)
type iprog =
  { mutable ifunctions : ifunc_def Map.t (* Map for function definitions *)
  ; mutable ifunctions_array : ifunc_def array (* Function array for O(1) access by index *)
  ; mutable ifunction_names : int Map.t (* Function name to index mapping *)
  ; mutable iconstants : constant_value array (* Global constants array with lazy evaluation *)
  ; mutable struct_types : struct_descr Map.t (* Struct type definitions *)
  ; mutable constant_names : int Map.t (* Constant name to index mapping *)
  ; mutable external_functions : Set.t (* External function names *)
  }

(* Variable resolution context for transformation *)
type transform_ctx =
  { var_to_index : (string, int) Hashtbl.t (* Keep mutable for function-local variables *)
  ; mutable next_index : int
  ; struct_types : struct_descr Map.t (* Immutable - can be shared *)
  ; constant_names : int Map.t (* Immutable - can be shared *)
  ; function_names : int Map.t (* Immutable - can be shared *)
  ; external_functions : Set.t (* Immutable - can be shared *)
  }

(* Exceptions *)
exception Runtime_error of string

(* Result type for statement execution to replace exception-based returns *)
type exec_result =
  | Continue
  | Return of dvalue

let error (msg : string) : 'a = raise (Runtime_error msg)

(* Creates an empty iprog for incremental building *)
let createEmptyProgram () : iprog =
  { ifunctions = Map.empty
  ; ifunctions_array = [||]
  ; ifunction_names = Map.empty
  ; iconstants = [||]
  ; struct_types = Map.empty
  ; constant_names = Map.empty
  ; external_functions = Set.empty
  }


(* Adds a function to the iprog, resizing arrays as needed *)
let addFunction (prog : iprog) (name : string) (ifunc : ifunc_def) : unit =
  let func_idx = Map.cardinal prog.ifunction_names in
  prog.ifunction_names <- Map.add name func_idx prog.ifunction_names;
  prog.ifunctions <- Map.add name ifunc prog.ifunctions;
  (* Resize function array if needed *)
  let new_array = Array.make (func_idx + 1) ifunc in
  Array.blit prog.ifunctions_array 0 new_array 0 (Array.length prog.ifunctions_array);
  new_array.(func_idx) <- ifunc;
  prog.ifunctions_array <- new_array


(* Adds a constant to the iprog, resizing array as needed *)
let addConstant (prog : iprog) (const_val : constant_value) : unit =
  let const_idx = Array.length prog.iconstants in
  let new_array = Array.make (const_idx + 1) const_val in
  Array.blit prog.iconstants 0 new_array 0 (Array.length prog.iconstants);
  new_array.(const_idx) <- const_val;
  prog.iconstants <- new_array


(* Adds a variable to the transformation context and returns its assigned index *)
let addVar (ctx : transform_ctx) (name : string) : int =
  if Hashtbl.mem ctx.var_to_index name then
    Hashtbl.find ctx.var_to_index name
  else
    let idx = ctx.next_index in
    Hashtbl.add ctx.var_to_index name idx;
    ctx.next_index <- ctx.next_index + 1;
    idx


(* Retrieves the index of a variable from the transformation context *)
let getVarIndex (ctx : transform_ctx) (name : string) : int option = Hashtbl.find_opt ctx.var_to_index name

(* Retrieves the index of a constant from the transformation context *)
let getConstantIndex (ctx : transform_ctx) (name : string) : int option = Map.find_opt name ctx.constant_names

(* Finds the index of a struct member by name within a struct descriptor *)
let getMemberIndex (struct_descr : struct_descr) (member_name : string) : int =
  let rec loop i = function
    | [] -> error ("Member not found: " ^ member_name ^ " in struct " ^ struct_descr.path)
    | (name, _, _, _) :: _ when name = member_name -> i
    | _ :: rest -> loop (i + 1) rest
  in
  loop 0 struct_descr.members


(* Printer functions for debugging and visualization *)

(* Converts a runtime dvalue to its string representation *)
let rec printDvalue (dv : dvalue) : string =
  match dv with
  | DVoid -> "void"
  | DInt i -> string_of_int i
  | DInt16 i -> string_of_int i
  | DReal f -> string_of_float f
  | DBool b -> string_of_bool b
  | DString s -> "\"" ^ s ^ "\""
  | DArray arr -> "[" ^ String.concat "; " (Array.to_list (Array.map printDvalue arr)) ^ "]"
  | DStruct arr ->
    "{" ^ String.concat "; " (Array.to_list (Array.mapi (fun i v -> string_of_int i ^ ":" ^ printDvalue v) arr)) ^ "}"


(* Converts a binary operator to its string representation *)
let printOperator (op : operator) : string =
  match op with
  | OpAdd -> "+"
  | OpSub -> "-"
  | OpMul -> "*"
  | OpDiv -> "/"
  | OpMod -> "%"
  | OpEq -> "=="
  | OpNe -> "!="
  | OpLt -> "<"
  | OpGt -> ">"
  | OpLe -> "<="
  | OpGe -> ">="
  | OpLand -> "&&"
  | OpLor -> "||"
  | OpLsh -> "<<"
  | OpRsh -> ">>"
  | OpBand -> "&"
  | OpBor -> "|"
  | OpBxor -> "^"


(* Converts a unary operator to its string representation *)
let printUoperator (op : uoperator) : string =
  match op with
  | UOpNeg -> "-"
  | UOpNot -> "!"


(* Converts an optimized interpreter expression to its string representation *)
let rec printIexp (ie : iexp) : string =
  match ie with
  | IEUnit -> "()"
  | IEEmptyValue -> "empty"
  | IEBool b -> string_of_bool b
  | IEInt i -> string_of_int i
  | IEReal f -> string_of_float f
  | IEFixed f -> string_of_float f ^ "f"
  | IEString s -> "\"" ^ s ^ "\""
  | IEVar idx -> "var[" ^ string_of_int idx ^ "]"
  | IEConstant idx -> "const[" ^ string_of_int idx ^ "]"
  | IEUnOp (op, e) -> printUoperator op ^ "(" ^ printIexp e ^ ")"
  | IEOp (op, e1, e2) -> "(" ^ printIexp e1 ^ " " ^ printOperator op ^ " " ^ printIexp e2 ^ ")"
  (* Specialized arithmetic operations *)
  | IEAddInt (e1, e2) -> "(" ^ printIexp e1 ^ " +int " ^ printIexp e2 ^ ")"
  | IESubInt (e1, e2) -> "(" ^ printIexp e1 ^ " -int " ^ printIexp e2 ^ ")"
  | IEMulInt (e1, e2) -> "(" ^ printIexp e1 ^ " *int " ^ printIexp e2 ^ ")"
  | IEDivInt (e1, e2) -> "(" ^ printIexp e1 ^ " /int " ^ printIexp e2 ^ ")"
  | IEAddInt16 (e1, e2) -> "(" ^ printIexp e1 ^ " +int16 " ^ printIexp e2 ^ ")"
  | IESubInt16 (e1, e2) -> "(" ^ printIexp e1 ^ " -int16 " ^ printIexp e2 ^ ")"
  | IEMulInt16 (e1, e2) -> "(" ^ printIexp e1 ^ " *int16 " ^ printIexp e2 ^ ")"
  | IEDivInt16 (e1, e2) -> "(" ^ printIexp e1 ^ " /int16 " ^ printIexp e2 ^ ")"
  | IEAddReal (e1, e2) -> "(" ^ printIexp e1 ^ " +real " ^ printIexp e2 ^ ")"
  | IESubReal (e1, e2) -> "(" ^ printIexp e1 ^ " -real " ^ printIexp e2 ^ ")"
  | IEMulReal (e1, e2) -> "(" ^ printIexp e1 ^ " *real " ^ printIexp e2 ^ ")"
  | IEDivReal (e1, e2) -> "(" ^ printIexp e1 ^ " /real " ^ printIexp e2 ^ ")"
  (* Specialized comparison operations *)
  | IEEqInt (e1, e2) -> "(" ^ printIexp e1 ^ " ==int " ^ printIexp e2 ^ ")"
  | IEEqInt16 (e1, e2) -> "(" ^ printIexp e1 ^ " ==int16 " ^ printIexp e2 ^ ")"
  | IEEqReal (e1, e2) -> "(" ^ printIexp e1 ^ " ==real " ^ printIexp e2 ^ ")"
  | IELtInt (e1, e2) -> "(" ^ printIexp e1 ^ " <int " ^ printIexp e2 ^ ")"
  | IELtInt16 (e1, e2) -> "(" ^ printIexp e1 ^ " <int16 " ^ printIexp e2 ^ ")"
  | IELtReal (e1, e2) -> "(" ^ printIexp e1 ^ " <real " ^ printIexp e2 ^ ")"
  | IEGtInt (e1, e2) -> "(" ^ printIexp e1 ^ " >int " ^ printIexp e2 ^ ")"
  | IEGtInt16 (e1, e2) -> "(" ^ printIexp e1 ^ " >int16 " ^ printIexp e2 ^ ")"
  | IEGtReal (e1, e2) -> "(" ^ printIexp e1 ^ " >real " ^ printIexp e2 ^ ")"
  (* Inlined built-in functions *)
  | IEBuiltinTanh e -> "tanh(" ^ printIexp e ^ ")"
  | IEBuiltinSinh e -> "sinh(" ^ printIexp e ^ ")"
  | IEBuiltinSin e -> "sin(" ^ printIexp e ^ ")"
  | IEBuiltinCos e -> "cos(" ^ printIexp e ^ ")"
  | IEBuiltinCosh e -> "cosh(" ^ printIexp e ^ ")"
  | IEBuiltinTan e -> "tan(" ^ printIexp e ^ ")"
  | IEBuiltinExp e -> "exp(" ^ printIexp e ^ ")"
  | IEBuiltinLog e -> "log(" ^ printIexp e ^ ")"
  | IEBuiltinLog10 e -> "log10(" ^ printIexp e ^ ")"
  | IEBuiltinSqrt e -> "sqrt(" ^ printIexp e ^ ")"
  | IEBuiltinAbs e -> "abs(" ^ printIexp e ^ ")"
  | IEBuiltinFloor e -> "floor(" ^ printIexp e ^ ")"
  | IEBuiltinPow (e1, e2) -> "pow(" ^ printIexp e1 ^ ", " ^ printIexp e2 ^ ")"
  | IEBuiltinClipReal (x, min_v, max_v) ->
    "clip_real(" ^ printIexp x ^ ", " ^ printIexp min_v ^ ", " ^ printIexp max_v ^ ")"
  | IEBuiltinClipInt (x, min_v, max_v) ->
    "clip_int(" ^ printIexp x ^ ", " ^ printIexp min_v ^ ", " ^ printIexp max_v ^ ")"
  (* Constants *)
  | IEBuiltinPi -> "pi"
  | IEBuiltinEps -> "eps"
  | IEBuiltinSamplerate -> "samplerate"
  (* Random functions *)
  | IEBuiltinRandom -> "random()"
  | IEBuiltinIrandom -> "irandom()"
  (* Type conversion functions *)
  | IEBuiltinReal e -> "real(" ^ printIexp e ^ ")"
  | IEBuiltinInt e -> "int(" ^ printIexp e ^ ")"
  | IEBuiltinInt16 e -> "int16(" ^ printIexp e ^ ")"
  | IEBuiltinBool e -> "bool(" ^ printIexp e ^ ")"
  | IEBuiltinString e -> "string(" ^ printIexp e ^ ")"
  | IEBuiltinFixed e -> "fix16(" ^ printIexp e ^ ")"
  (* Array/string functions *)
  | IEBuiltinSize e -> "size(" ^ printIexp e ^ ")"
  | IEBuiltinLength e -> "length(" ^ printIexp e ^ ")"
  | IEIndex (arr, idx) -> printIexp arr ^ "[" ^ printIexp idx ^ "]"
  | IEArray exprs -> "[" ^ String.concat "; " (CCList.map printIexp exprs) ^ "]"
  | IECall (func_idx, args) ->
    "func[" ^ string_of_int func_idx ^ "](" ^ String.concat ", " (CCList.map printIexp args) ^ ")"
  | IECallExt (func_name, args) -> "external_" ^ func_name ^ "(" ^ String.concat ", " (CCList.map printIexp args) ^ ")"
  | IEIf (cond, then_e, else_e) -> "if " ^ printIexp cond ^ " then " ^ printIexp then_e ^ " else " ^ printIexp else_e
  | IETuple exprs -> "(" ^ String.concat ", " (CCList.map printIexp exprs) ^ ")"
  | IEMember (e, idx) -> printIexp e ^ ".field[" ^ string_of_int idx ^ "]"
  | IERecord (_, members) ->
    "{" ^ String.concat "; " (CCList.map (fun (idx, e) -> string_of_int idx ^ ":" ^ printIexp e) members) ^ "}"


(* Converts an interpreter left-value expression to its string representation *)
let rec printIlexp (il : ilexp) : string =
  match il with
  | ILWild -> "_"
  | ILVar idx -> "var[" ^ string_of_int idx ^ "]"
  | ILMember (lv, idx) -> printIlexp lv ^ ".field[" ^ string_of_int idx ^ "]"
  | ILIndex (lv, e) -> printIlexp lv ^ "[" ^ printIexp e ^ "]"
  | ILTuple lvs -> "(" ^ String.concat ", " (CCList.map printIlexp lvs) ^ ")"


(* Converts an interpreter statement to its string representation *)
let rec printIstmt (is : istmt) : string =
  match is with
  | IStmtDecl (idx, typ, None) -> "var[" ^ string_of_int idx ^ "] : " ^ Pla.print (Prog.Print.print_type_ typ)
  | IStmtDecl (idx, typ, Some init) ->
    "var[" ^ string_of_int idx ^ "] : " ^ Pla.print (Prog.Print.print_type_ typ) ^ " = " ^ printIexp init
  | IStmtBind (lv, e) -> printIlexp lv ^ " = " ^ printIexp e
  | IStmtReturn e -> "return " ^ printIexp e
  | IStmtBlock stmts -> "{\n" ^ String.concat ";\n" (CCList.map printIstmt stmts) ^ "\n}"
  | IStmtIf (cond, then_s, None) -> "if " ^ printIexp cond ^ " " ^ printIstmt then_s
  | IStmtIf (cond, then_s, Some else_s) ->
    "if " ^ printIexp cond ^ " " ^ printIstmt then_s ^ " else " ^ printIstmt else_s
  | IStmtWhile (cond, body) -> "while " ^ printIexp cond ^ " " ^ printIstmt body
  | IStmtSwitch (e, cases, default) ->
    let case_strs = CCList.map (fun (pattern, stmt) -> printIexp pattern ^ " -> " ^ printIstmt stmt) cases in
    let default_str =
      match default with
      | None -> ""
      | Some s -> " | _ -> " ^ printIstmt s
    in
    "match " ^ printIexp e ^ " with " ^ String.concat " | " case_strs ^ default_str


(* Converts an interpreter function definition to its string representation *)
let printIfuncDef (fd : ifunc_def) : string =
  "function "
  ^ fd.iname
  ^ "("
  ^ String.concat ", " (CCList.map string_of_int fd.iargs)
  ^ ") : "
  ^ Pla.print (Prog.Print.print_type_ fd.iret_type)
  ^ " [locals:"
  ^ string_of_int fd.ilocals
  ^ "] "
  ^ Pla.print (Pla.indent (Pla.string (printIstmt fd.ibody)))


(* Converts an interpreter program to its string representation *)
let printIprog (prog : iprog) : string =
  let func_strs = Map.fold (fun _name fd acc -> printIfuncDef fd :: acc) prog.ifunctions [] in
  String.concat "\n\n" func_strs


(* Determines if a type represents an integer value *)
let isIntType (typ : type_) : bool =
  match typ.t with
  | TInt | TInt16 -> true
  | _ -> false


(* Determines if a type represents a real/floating-point value *)
let isRealType (typ : type_) : bool =
  match typ.t with
  | TReal | TFix16 -> true
  | _ -> false


(* Determines if a type represents a 16-bit integer value *)
let isInt16Type (typ : type_) : bool =
  match typ.t with
  | TInt16 -> true
  | _ -> false


(* Transforms an original Prog expression into an optimized interpreter expression with type specialization *)
let rec transformExp (ctx : transform_ctx) (exp : exp) : iexp =
  match exp.e with
  | EUnit -> IEUnit
  | EEmptyValue -> IEEmptyValue
  | EBool b -> IEBool b
  | EInt i -> IEInt i
  | EReal f -> IEReal f
  | EFixed f -> IEFixed f
  | EString s -> IEString s
  | EId name -> (
    match getVarIndex ctx name with
    | Some var_idx -> IEVar var_idx
    | None -> (
      match getConstantIndex ctx name with
      | Some const_idx -> IEConstant const_idx
      | None -> error ("Variable or constant not found: " ^ name)))
  | EUnOp (op, e) -> IEUnOp (op, transformExp ctx e)
  | EOp (op, e1, e2) -> (
    (* Specialize arithmetic operations based on types *)
    let te1 = transformExp ctx e1 in
    let te2 = transformExp ctx e2 in
    match op with
    | OpAdd when isInt16Type e1.t && isInt16Type e2.t -> IEAddInt16 (te1, te2)
    | OpAdd when isIntType e1.t && isIntType e2.t -> IEAddInt (te1, te2)
    | OpAdd when isRealType e1.t || isRealType e2.t -> IEAddReal (te1, te2)
    | OpSub when isInt16Type e1.t && isInt16Type e2.t -> IESubInt16 (te1, te2)
    | OpSub when isIntType e1.t && isIntType e2.t -> IESubInt (te1, te2)
    | OpSub when isRealType e1.t || isRealType e2.t -> IESubReal (te1, te2)
    | OpMul when isInt16Type e1.t && isInt16Type e2.t -> IEMulInt16 (te1, te2)
    | OpMul when isIntType e1.t && isIntType e2.t -> IEMulInt (te1, te2)
    | OpMul when isRealType e1.t || isRealType e2.t -> IEMulReal (te1, te2)
    | OpDiv when isInt16Type e1.t && isInt16Type e2.t -> IEDivInt16 (te1, te2)
    | OpDiv when isIntType e1.t && isIntType e2.t -> IEDivInt (te1, te2)
    | OpDiv when isRealType e1.t || isRealType e2.t -> IEDivReal (te1, te2)
    | OpEq when isInt16Type e1.t && isInt16Type e2.t -> IEEqInt16 (te1, te2)
    | OpEq when isIntType e1.t && isIntType e2.t -> IEEqInt (te1, te2)
    | OpEq when isRealType e1.t || isRealType e2.t -> IEEqReal (te1, te2)
    | OpLt when isInt16Type e1.t && isInt16Type e2.t -> IELtInt16 (te1, te2)
    | OpLt when isIntType e1.t && isIntType e2.t -> IELtInt (te1, te2)
    | OpLt when isRealType e1.t || isRealType e2.t -> IELtReal (te1, te2)
    | OpGt when isInt16Type e1.t && isInt16Type e2.t -> IEGtInt16 (te1, te2)
    | OpGt when isIntType e1.t && isIntType e2.t -> IEGtInt (te1, te2)
    | OpGt when isRealType e1.t || isRealType e2.t -> IEGtReal (te1, te2)
    | _ -> IEOp (op, te1, te2)
    (* Fall back to generic for other ops *))
  | EIndex { e; index } -> IEIndex (transformExp ctx e, transformExp ctx index)
  | EArray elems -> IEArray (CCList.map (transformExp ctx) elems)
  | ECall { path; args } -> (
    let args' = CCList.map (transformExp ctx) args in
    (* Inline built-in functions for performance *)
    match path, args' with
    (* Math functions *)
    | "tanh", [ arg ] -> IEBuiltinTanh arg
    | "cosh", [ arg ] -> IEBuiltinCosh arg
    | "sinh", [ arg ] -> IEBuiltinSinh arg
    | "sin", [ arg ] -> IEBuiltinSin arg
    | "cos", [ arg ] -> IEBuiltinCos arg
    | "tan", [ arg ] -> IEBuiltinTan arg
    | "exp", [ arg ] -> IEBuiltinExp arg
    | "log", [ arg ] -> IEBuiltinLog arg
    | "log10", [ arg ] -> IEBuiltinLog10 arg
    | "sqrt", [ arg ] -> IEBuiltinSqrt arg
    | "abs", [ arg ] -> IEBuiltinAbs arg
    | "floor", [ arg ] -> IEBuiltinFloor arg
    | "pow", [ arg1; arg2 ] -> IEBuiltinPow (arg1, arg2)
    | "clip", [ x; min_v; max_v ] when isRealType exp.t -> IEBuiltinClipReal (x, min_v, max_v)
    | "clip", [ x; min_v; max_v ] when isIntType exp.t -> IEBuiltinClipInt (x, min_v, max_v)
    (* Constants *)
    | "pi", [] -> IEBuiltinPi
    | "eps", [] -> IEBuiltinEps
    | "samplerate", [] -> IEBuiltinSamplerate
    (* Random functions *)
    | "random", [] -> IEBuiltinRandom
    | "irandom", [] -> IEBuiltinIrandom
    (* Type conversion functions *)
    | "real", [ arg ] -> IEBuiltinReal arg
    | "int", [ arg ] -> IEBuiltinInt arg
    | "int16", [ arg ] -> IEBuiltinInt16 arg
    | "bool", [ arg ] -> IEBuiltinBool arg
    | "string", [ arg ] -> IEBuiltinString arg
    | "fix16", [ arg ] -> IEBuiltinFixed arg
    (* Array/string functions *)
    | "size", [ arg ] -> IEBuiltinSize arg
    | "length", [ arg ] -> IEBuiltinLength arg
    (* External runtime functions *)
    | "push_block_header", args -> IECallExt ("push_block_header", args)
    | "push_int", args -> IECallExt ("push_int", args)
    | "push_float", args -> IECallExt ("push_float", args)
    | "update_size", args -> IECallExt ("update_size", args)
    | "push_array", args -> IECallExt ("push_array", args)
    | "push_string", args -> IECallExt ("push_string", args)
    | "serialize_type_descr", args -> IECallExt ("serialize_type_descr", args)
    | "search_field_name", args -> IECallExt ("search_field_name", args)
    | "deserialize_int", args -> IECallExt ("deserialize_int", args)
    | "deserialize_float", args -> IECallExt ("deserialize_float", args)
    | "deserialize_bool", args -> IECallExt ("deserialize_bool", args)
    | "deserialize_string", args -> IECallExt ("deserialize_string", args)
    | "search_type_description", args -> IECallExt ("search_type_description", args)
    | "first_array_element", args -> IECallExt ("first_array_element", args)
    | "next_object", args -> IECallExt ("next_object", args)
    (* Fall back to regular call for non-builtins *)
    | _ -> (
      match Map.find_opt path ctx.function_names with
      | Some func_idx -> IECall (func_idx, args')
      | None ->
        if
          (* Check if it's an external function *)
          Set.mem path ctx.external_functions
        then
          IECallExt (path, args')
        else (
          (* Debug: show available functions when lookup fails *)
          Printf.eprintf "Function not found during transformation: %s\n" path;
          Printf.eprintf "Available regular functions:\n";
          Map.iter (fun name idx -> Printf.eprintf "  %s -> %d\n" name idx) ctx.function_names;
          Printf.eprintf "Available external functions:\n";
          Set.iter (fun name -> Printf.eprintf "  %s (external)\n" name) ctx.external_functions;
          error ("Function not found during transformation: " ^ path))))
  | EIf { cond; then_; else_ } -> IEIf (transformExp ctx cond, transformExp ctx then_, transformExp ctx else_)
  | ETuple elems -> IETuple (CCList.map (transformExp ctx) elems)
  | EMember (e, member_name) -> (
    match e.t.t with
    | TStruct descr ->
      let member_idx = getMemberIndex descr member_name in
      IEMember (transformExp ctx e, member_idx)
    | _ -> error "Member access on non-struct type")
  | ETMember (e, idx) -> IEMember (transformExp ctx e, idx)
  | ERecord { path; elems } -> (
    match Map.find_opt path ctx.struct_types with
    | Some descr ->
      let elems' =
        CCList.map
          (fun (name, exp) ->
            let idx = getMemberIndex descr name in
            idx, transformExp ctx exp)
          elems
      in
      IERecord (descr, elems')
    | None -> error ("Unknown struct type: " ^ path))


(* Transforms an original Prog left-value expression into an interpreter left-value expression *)
and transformLexp (ctx : transform_ctx) (lexp : lexp) : ilexp =
  match lexp.l with
  | LWild -> ILWild
  | LId name -> (
    match getVarIndex ctx name with
    | Some var_idx -> ILVar var_idx
    | None -> error ("Variable not found in left-value: " ^ name))
  | LMember (e, member_name) -> (
    match e.t.t with
    | TStruct descr ->
      let member_idx = getMemberIndex descr member_name in
      ILMember (transformLexp ctx e, member_idx)
    | _ -> error "Member access on non-struct type")
  | LIndex { e; index } -> ILIndex (transformLexp ctx e, transformExp ctx index)
  | LTuple lexps -> ILTuple (CCList.map (transformLexp ctx) lexps)


(* Transforms an original Prog statement into an interpreter statement *)
and transformStmt (ctx : transform_ctx) (stmt : stmt) : istmt =
  match stmt.s with
  | StmtDecl (dexp, init_exp) -> (
    match dexp.d with
    | DId (name, _) ->
      let var_idx = addVar ctx name in
      let init_exp' = Option.map (transformExp ctx) init_exp in
      IStmtDecl (var_idx, dexp.t, init_exp'))
  | StmtBind (lexp, exp) -> IStmtBind (transformLexp ctx lexp, transformExp ctx exp)
  | StmtReturn exp -> IStmtReturn (transformExp ctx exp)
  | StmtBlock stmts -> IStmtBlock (CCList.map (transformStmt ctx) stmts)
  | StmtIf (cond, then_stmt, else_stmt) ->
    let cond' = transformExp ctx cond in
    let then_stmt' = transformStmt ctx then_stmt in
    let else_stmt' = Option.map (transformStmt ctx) else_stmt in
    IStmtIf (cond', then_stmt', else_stmt')
  | StmtWhile (cond, body) -> IStmtWhile (transformExp ctx cond, transformStmt ctx body)
  | StmtSwitch (exp, cases, default) ->
    let exp' = transformExp ctx exp in
    let cases' =
      CCList.map (fun (case_exp, case_stmt) -> transformExp ctx case_exp, transformStmt ctx case_stmt) cases
    in
    let default' = Option.map (transformStmt ctx) default in
    IStmtSwitch (exp', cases', default')


(* Transforms a single function definition from original Prog to interpreter AST *)
let transformFunction (global_types : struct_descr Map.t) (constant_names : int Map.t) (function_names : int Map.t)
    (external_functions : Set.t) (def : function_def) (body : stmt) : ifunc_def =
  (* Context creation timing *)
  let ctx =
    { var_to_index = Hashtbl.create 32
    ; next_index = 0
    ; struct_types = global_types
    ; constant_names
    ; function_names
    ; external_functions
    }
  in
  let param_indices = CCList.map (fun (param : param) -> addVar ctx param.name) def.args in
  let body' = transformStmt ctx body in
  (* Get return type *)
  let ret_type = snd def.t in
  { iname = def.name; iargs = param_indices; iret_type = ret_type; ilocals = ctx.next_index; ibody = body' }


(* Evaluates constant expressions at compile time *)
let rec evalConstantExpression (constants : dvalue array) (exp : iexp) : dvalue =
  match exp with
  | IEUnit -> DVoid
  | IEEmptyValue -> DVoid
  | IEBool b -> DBool b
  | IEInt i -> DInt i
  | IEReal f -> DReal f
  | IEFixed f -> DReal f
  | IEString s -> DString s
  | IEConstant idx ->
    if idx >= 0 && idx < Array.length constants then
      constants.(idx)
    else
      error ("Constant index out of bounds during evaluation: " ^ string_of_int idx)
  | IEVar _ -> error "Variables not allowed in constant expressions"
  (* Handle arithmetic operations for constants *)
  | IEAddInt (e1, e2) -> (
    match evalConstantExpression constants e1, evalConstantExpression constants e2 with
    | DInt a, DInt b -> DInt (a + b)
    | _ -> error "Type mismatch in constant integer addition")
  | IEAddInt16 (e1, e2) -> (
    match evalConstantExpression constants e1, evalConstantExpression constants e2 with
    | DInt16 a, DInt16 b ->
      let result = a + b in
      let clamped = max (-32768) (min 32767 result) in
      DInt16 clamped
    | _ -> error "Type mismatch in constant int16 addition")
  | IEAddReal (e1, e2) -> (
    match evalConstantExpression constants e1, evalConstantExpression constants e2 with
    | DReal a, DReal b -> DReal (a +. b)
    | _ -> error "Type mismatch in constant real addition")
  (* Add more arithmetic operations as needed *)
  | IEArray elems ->
    let values = Array.of_list (CCList.map (evalConstantExpression constants) elems) in
    DArray values
  | IERecord (descr, elems) ->
    let member_vals = Array.make (CCList.length descr.members) DVoid in
    CCList.iter
      (fun (idx, exp) ->
        let val_ = evalConstantExpression constants exp in
        member_vals.(idx) <- val_)
      elems;
    DStruct member_vals
  (* For now, other expressions are not supported in constants *)
  | _ -> error "Unsupported expression in constant declaration"


(* Transforms a single top-level statement incrementally *)
let transformStatement (prog : iprog) (stmt : top_stmt) : unit =
  match stmt.top with
  | TopType descr -> prog.struct_types <- Map.add descr.path descr prog.struct_types
  | TopConstant (name, _, _, exp) -> (
    let const_idx = Array.length prog.iconstants in
    prog.constant_names <- Map.add name const_idx prog.constant_names;
    (* Create context for transforming the constant expression *)
    let ctx =
      { var_to_index = Hashtbl.create 32
      ; next_index = 0
      ; struct_types = prog.struct_types
      ; constant_names = prog.constant_names
      ; function_names = prog.ifunction_names
      ; external_functions = prog.external_functions
      }
    in
    let iexp = transformExp ctx exp in
    (* Try to evaluate immediately, fall back to lazy evaluation *)
    try
      let value =
        evalConstantExpression
          (Array.map
             (function
               | Evaluated v -> v
               | Unevaluated _ -> DVoid)
             prog.iconstants)
          iexp
      in
      addConstant prog (Evaluated value)
    with
    | _ ->
      (* Store as unevaluated for lazy evaluation later *)
      let eval_ctx =
        { ifunctions_array = prog.ifunctions_array
        ; ifunction_names = prog.ifunction_names
        ; iconstants_ref = ref prog.iconstants
        }
      in
      addConstant prog (Unevaluated (iexp, eval_ctx)))
  | TopFunction (def, body) ->
    let ifunc =
      transformFunction prog.struct_types prog.constant_names prog.ifunction_names prog.external_functions def body
    in
    addFunction prog def.name ifunc
  | TopExternal (def, _) -> prog.external_functions <- Set.add def.name prog.external_functions
  | TopAlias _ -> () (* Type aliases don't need special handling in the interpreter *)


(* Transforms an entire program from original AST to optimized interpreter AST using single-pass incremental approach *)
let transformProgram (prog : top_stmt list) : iprog =
  let iprog = createEmptyProgram () in
  CCList.iter (transformStatement iprog) prog;
  (* Update lazy evaluation contexts with the completed function array *)
  Array.iteri
    (fun idx const_val ->
      match const_val with
      | Unevaluated (iexp, eval_ctx) ->
        let updated_ctx = { eval_ctx with ifunctions_array = iprog.ifunctions_array } in
        iprog.iconstants.(idx) <- Unevaluated (iexp, updated_ctx)
      | Evaluated _ -> ())
    iprog.iconstants;
  iprog


(* Runtime stack for function execution *)
type runtime_stack =
  { stack : dvalue array
  ; mutable sp : int (* Stack pointer *)
  ; max_size : int
  }

(* Creates a new runtime stack with the specified size *)
let createStack (size : int) : runtime_stack = { stack = Array.make size DVoid; sp = 0; max_size = size }

(* Creates a default value for a given type *)
let rec defaultValue (typ : type_) : dvalue =
  match typ.t with
  | TVoid _ -> DVoid
  | TInt -> DInt 0
  | TInt16 -> DInt16 0
  | TReal -> DReal 0.0
  | TFix16 -> DReal 0.0
  | TBool -> DBool false
  | TString -> DString ""
  | TArray (Some size, elem_type) -> DArray (Array.init size (fun _ -> defaultValue elem_type))
  | TStruct descr -> DStruct (Array.of_list (CCList.map (fun (_, typ, _, _) -> defaultValue typ) descr.members))
  | TTuple types -> DArray (Array.of_list (CCList.map defaultValue types))
  | TEmptyType -> DVoid
  | TArray (None, _) -> error "Cannot create default value for unsized array"


(* Sets up a function call on the runtime stack and returns the frame start offset *)
let setupFunctionCall (stack : runtime_stack) (ifunc : ifunc_def) (args : dvalue list) : int =
  let frame_start = stack.sp in
  (* Check stack overflow *)
  if stack.sp + ifunc.ilocals > stack.max_size then
    error ("Stack overflow in function " ^ ifunc.iname);
  (* Initialize all locals to default values first *)
  for i = 0 to ifunc.ilocals - 1 do
    stack.stack.(stack.sp + i) <- DVoid
  done;
  (* Initialize parameters *)
  CCList.iter2 (fun param_idx arg_val -> stack.stack.(frame_start + param_idx) <- arg_val) ifunc.iargs args;
  (* Move stack pointer *)
  stack.sp <- stack.sp + ifunc.ilocals;
  frame_start


(* Cleans up a function call from the runtime stack by restoring the stack pointer *)
let cleanupFunctionCall (stack : runtime_stack) (ifunc : ifunc_def) : unit = stack.sp <- stack.sp - ifunc.ilocals

(* Retrieves an element from an array using an index *)
let getArrayElement (arr : dvalue) (idx : dvalue) : dvalue =
  match arr, idx with
  | DArray elems, DInt i when i >= 0 && i < Array.length elems -> elems.(i)
  | _ -> error "Invalid array access"


(* Retrieves a member from a struct using a member index *)
let getStructMember (struct_val : dvalue) (member_idx : int) : dvalue =
  match struct_val with
  | DStruct members when member_idx >= 0 && member_idx < Array.length members -> members.(member_idx)
  | _ -> error "Invalid struct member access"


(* Evaluates a binary operation on two runtime values *)
let evalBinop (op : operator) (v1 : dvalue) (v2 : dvalue) : dvalue =
  match op, v1, v2 with
  | OpAdd, DInt a, DInt b -> DInt (a + b)
  | OpAdd, DInt16 a, DInt16 b -> DInt16 (max (-32768) (min 32767 (a + b)))
  | OpAdd, DReal a, DReal b -> DReal (a +. b)
  | OpAdd, DInt a, DReal b -> DReal (float_of_int a +. b)
  | OpAdd, DReal a, DInt b -> DReal (a +. float_of_int b)
  | OpSub, DInt a, DInt b -> DInt (a - b)
  | OpSub, DInt16 a, DInt16 b -> DInt16 (max (-32768) (min 32767 (a - b)))
  | OpSub, DReal a, DReal b -> DReal (a -. b)
  | OpSub, DInt a, DReal b -> DReal (float_of_int a -. b)
  | OpSub, DReal a, DInt b -> DReal (a -. float_of_int b)
  | OpMul, DInt a, DInt b -> DInt (a * b)
  | OpMul, DInt16 a, DInt16 b -> DInt16 (max (-32768) (min 32767 (a * b)))
  | OpMul, DReal a, DReal b -> DReal (a *. b)
  | OpMul, DInt a, DReal b -> DReal (float_of_int a *. b)
  | OpMul, DReal a, DInt b -> DReal (a *. float_of_int b)
  | OpDiv, DInt a, DInt b when b <> 0 -> DInt (a / b)
  | OpDiv, DInt16 a, DInt16 b when b <> 0 -> DInt16 (max (-32768) (min 32767 (a / b)))
  | OpDiv, DReal a, DReal b when b <> 0.0 -> DReal (a /. b)
  | OpDiv, DInt a, DReal b when b <> 0.0 -> DReal (float_of_int a /. b)
  | OpDiv, DReal a, DInt b when b <> 0 -> DReal (a /. float_of_int b)
  | OpMod, DInt a, DInt b when b <> 0 -> DInt (a mod b)
  | OpMod, DInt16 a, DInt16 b when b <> 0 -> DInt16 (max (-32768) (min 32767 (a mod b)))
  | OpEq, DInt a, DInt b -> DBool (a = b)
  | OpEq, DInt16 a, DInt16 b -> DBool (a = b)
  | OpEq, DReal a, DReal b -> DBool (a = b)
  | OpEq, DBool a, DBool b -> DBool (a = b)
  | OpEq, DString a, DString b -> DBool (a = b)
  | OpNe, DInt a, DInt b -> DBool (a <> b)
  | OpNe, DInt16 a, DInt16 b -> DBool (a <> b)
  | OpNe, DReal a, DReal b -> DBool (a <> b)
  | OpNe, DBool a, DBool b -> DBool (a <> b)
  | OpNe, DString a, DString b -> DBool (a <> b)
  | OpLt, DInt a, DInt b -> DBool (a < b)
  | OpLt, DInt16 a, DInt16 b -> DBool (a < b)
  | OpLt, DReal a, DReal b -> DBool (a < b)
  | OpLe, DInt a, DInt b -> DBool (a <= b)
  | OpLe, DInt16 a, DInt16 b -> DBool (a <= b)
  | OpLe, DReal a, DReal b -> DBool (a <= b)
  | OpGt, DInt a, DInt b -> DBool (a > b)
  | OpGt, DInt16 a, DInt16 b -> DBool (a > b)
  | OpGt, DReal a, DReal b -> DBool (a > b)
  | OpGe, DInt a, DInt b -> DBool (a >= b)
  | OpGe, DInt16 a, DInt16 b -> DBool (a >= b)
  | OpGe, DReal a, DReal b -> DBool (a >= b)
  | OpLand, DBool a, DBool b -> DBool (a && b)
  | OpLor, DBool a, DBool b -> DBool (a || b)
  | OpBand, DInt a, DInt b -> DInt (a land b)
  | OpBand, DInt16 a, DInt16 b -> DInt16 (a land b)
  | OpBor, DInt a, DInt b -> DInt (a lor b)
  | OpBor, DInt16 a, DInt16 b -> DInt16 (a lor b)
  | OpBxor, DInt a, DInt b -> DInt (a lxor b)
  | OpBxor, DInt16 a, DInt16 b -> DInt16 (a lxor b)
  | OpLsh, DInt a, DInt b -> DInt (a lsl b)
  | OpLsh, DInt16 a, DInt16 b -> DInt16 (max (-32768) (min 32767 (a lsl b)))
  | OpRsh, DInt a, DInt b -> DInt (a lsr b)
  | OpRsh, DInt16 a, DInt16 b -> DInt16 (a lsr b)
  | OpMod, DReal a, DReal b -> DReal (Stdlib.mod_float a b)
  | _ ->
    let ops = Pla.print (Prog.Print.print_operator op) in
    let v1 = printDvalue v1 in
    let v2 = printDvalue v2 in
    error ("Unsupported operation: " ^ v1 ^ " " ^ ops ^ " " ^ v2)


(* Evaluates a unary operation on a runtime value *)
let evalUnop (op : uoperator) (v : dvalue) : dvalue =
  match op, v with
  | UOpNeg, DInt i -> DInt (-i)
  | UOpNeg, DInt16 i -> DInt16 (max (-32768) (min 32767 (-i)))
  | UOpNeg, DReal f -> DReal (-.f)
  | UOpNot, DBool b -> DBool (not b)
  | UOpNot, DInt i -> DBool (i = 0)
  | UOpNot, DInt16 i -> DBool (i = 0)
  | _ -> error "Unsupported unary operation"


(* Calls a function by index with the given arguments and returns the result *)
let rec callFunction : iprog -> runtime_stack -> int -> dvalue list -> dvalue =
 fun prog stack func_idx args ->
  let ifunc = prog.ifunctions_array.(func_idx) in
  let frame_start = setupFunctionCall stack ifunc args in
  let result = execIstmt prog stack frame_start ifunc.ibody in
  cleanupFunctionCall stack ifunc;
  match result with
  | Continue -> DVoid (* Function completed without explicit return *)
  | Return v -> v


(* Executes a list of statements, stopping early if a return is encountered *)
and execStmtList : iprog -> runtime_stack -> int -> istmt list -> exec_result =
 fun prog stack frame_start stmts ->
  let rec loop = function
    | [] -> Continue
    | stmt :: rest -> (
      match execIstmt prog stack frame_start stmt with
      | Continue -> loop rest
      | Return v -> Return v)
  in
  loop stmts


(* Executes an interpreter statement *)
and execIstmt : iprog -> runtime_stack -> int -> istmt -> exec_result =
 fun prog stack frame_start stmt ->
  match stmt with
  | IStmtDecl (var_idx, typ, init_exp) ->
    let init_val =
      match init_exp with
      | Some exp -> evalIexp prog stack frame_start exp
      | None -> defaultValue typ
    in
    stack.stack.(frame_start + var_idx) <- init_val;
    Continue
  | IStmtBind (lexp, exp) ->
    let val_ = evalIexp prog stack frame_start exp in
    assignIlvalue prog stack frame_start lexp val_;
    Continue
  | IStmtReturn exp ->
    let val_ = evalIexp prog stack frame_start exp in
    Return val_
  | IStmtBlock stmts -> execStmtList prog stack frame_start stmts
  | IStmtIf (cond, then_stmt, else_stmt) -> (
    match evalIexp prog stack frame_start cond with
    | DBool true -> execIstmt prog stack frame_start then_stmt
    | DBool false -> (
      match else_stmt with
      | Some stmt -> execIstmt prog stack frame_start stmt
      | None -> Continue)
    | _ -> error "Invalid condition")
  | IStmtWhile (cond, body) ->
    let rec loop () =
      match evalIexp prog stack frame_start cond with
      | DBool true -> (
        match execIstmt prog stack frame_start body with
        | Continue -> loop ()
        | Return v -> Return v)
      | _ -> Continue
    in
    loop ()
  | IStmtSwitch (exp, cases, default) ->
    let exp_val = evalIexp prog stack frame_start exp in
    let rec try_cases = function
      | [] -> (
        match default with
        | Some stmt -> execIstmt prog stack frame_start stmt
        | None -> Continue)
      | (case_exp, case_stmt) :: rest ->
        let case_val = evalIexp prog stack frame_start case_exp in
        if evalBinop OpEq exp_val case_val = DBool true then
          execIstmt prog stack frame_start case_stmt
        else
          try_cases rest
    in
    try_cases cases


(* Evaluates an lvalue expression as an rvalue (gets the value it points to) *)
and evalIlexpAsRvalue : iprog -> runtime_stack -> int -> ilexp -> dvalue =
 fun prog stack frame_start lexp ->
  match lexp with
  | ILWild -> error "Cannot evaluate wildcard as rvalue"
  | ILVar idx -> stack.stack.(frame_start + idx)
  | ILMember (e, member_idx) -> (
    let struct_val = evalIlexpAsRvalue prog stack frame_start e in
    match struct_val with
    | DStruct members when member_idx >= 0 && member_idx < Array.length members -> members.(member_idx)
    | _ -> error "Invalid struct member access")
  | ILIndex (e, index) -> (
    let idx_val = evalIexp prog stack frame_start index in
    let array_val = evalIlexpAsRvalue prog stack frame_start e in
    match array_val, idx_val with
    | DArray arr, DInt i when i >= 0 && i < Array.length arr -> arr.(i)
    | _ -> error "Invalid array access")
  | ILTuple _ -> error "Cannot evaluate tuple lvalue as rvalue"


(* Assigns a value to an optimized interpreter left-value expression *)
and assignIlvalue : iprog -> runtime_stack -> int -> ilexp -> dvalue -> unit =
 fun prog stack frame_start lexp val_ ->
  match lexp with
  | ILWild -> ()
  | ILVar idx -> stack.stack.(frame_start + idx) <- val_
  | ILMember (e, member_idx) -> (
    (* First get the container struct by recursively evaluating the base expression *)
    let struct_val = evalIlexpAsRvalue prog stack frame_start e in
    match struct_val with
    | DStruct members when member_idx >= 0 && member_idx < Array.length members -> members.(member_idx) <- val_
    | _ -> error "Invalid struct member assignment")
  | ILIndex (e, index) -> (
    let idx_val = evalIexp prog stack frame_start index in
    (* Get the container array by recursively evaluating the base expression *)
    let array_val = evalIlexpAsRvalue prog stack frame_start e in
    match array_val, idx_val with
    | DArray arr, DInt i when i >= 0 && i < Array.length arr -> arr.(i) <- val_
    | _ -> error "Invalid array assignment")
  | ILTuple lexps -> (
    match val_ with
    | DArray vals when Array.length vals = CCList.length lexps ->
      CCList.iteri (fun i lexp -> assignIlvalue prog stack frame_start lexp vals.(i)) lexps
    | _ -> error "Tuple assignment type mismatch")


(* Evaluates a lazy constant, caching the result *)
and evaluateLazyConstant (constants : constant_value array) (idx : int) : dvalue =
  match constants.(idx) with
  | Evaluated value -> value
  | Unevaluated (exp, ctx) ->
    (* Create a temporary program for evaluation *)
    let temp_prog =
      { ifunctions = Map.empty
      ; ifunctions_array = ctx.ifunctions_array
      ; ifunction_names = ctx.ifunction_names
      ; iconstants = !(ctx.iconstants_ref)
      ; struct_types = Map.empty
      ; constant_names = Map.empty
      ; external_functions = Set.empty
      }
    in
    (* Create a minimal stack for pure function evaluation *)
    let temp_stack = createStack 100 in
    let value = evalIexp temp_prog temp_stack 0 exp in
    (* Cache the evaluated value *)
    constants.(idx) <- Evaluated value;
    value


(* Evaluates expressions *)
and evalIexp (prog : iprog) (stack : runtime_stack) (frame_start : int) (exp : iexp) : dvalue =
  match exp with
  | IEUnit -> DVoid
  | IEEmptyValue -> DVoid
  | IEBool b -> DBool b
  | IEInt i -> DInt i
  | IEReal f -> DReal f
  | IEFixed f -> DReal f
  | IEString s -> DString s
  | IEVar idx -> stack.stack.(frame_start + idx)
  | IEConstant idx -> evaluateLazyConstant prog.iconstants idx
  | IEUnOp (op, e) ->
    let v = evalIexp prog stack frame_start e in
    evalUnop op v
  | IEOp (op, e1, e2) ->
    let v1 = evalIexp prog stack frame_start e1 in
    let v2 = evalIexp prog stack frame_start e2 in
    evalBinop op v1 v2
  (* Specialized fast arithmetic operations *)
  | IEAddInt (e1, e2) -> (
    match evalIexp prog stack frame_start e1, evalIexp prog stack frame_start e2 with
    | DInt a, DInt b -> DInt (a + b)
    | _ -> error "Type mismatch in integer addition")
  | IESubInt (e1, e2) -> (
    match evalIexp prog stack frame_start e1, evalIexp prog stack frame_start e2 with
    | DInt a, DInt b -> DInt (a - b)
    | _ -> error "Type mismatch in integer subtraction")
  | IEMulInt (e1, e2) -> (
    match evalIexp prog stack frame_start e1, evalIexp prog stack frame_start e2 with
    | DInt a, DInt b -> DInt (a * b)
    | _ -> error "Type mismatch in integer multiplication")
  | IEDivInt (e1, e2) -> (
    match evalIexp prog stack frame_start e1, evalIexp prog stack frame_start e2 with
    | DInt a, DInt b -> DInt (a / b)
    | _ -> error "Type mismatch in integer division")
  | IEAddInt16 (e1, e2) -> (
    match evalIexp prog stack frame_start e1, evalIexp prog stack frame_start e2 with
    | DInt16 a, DInt16 b ->
      (* Clamp to int16 range (-32768 to 32767) *)
      let result = a + b in
      let clamped = max (-32768) (min 32767 result) in
      DInt16 clamped
    | _ -> error "Type mismatch in int16 addition")
  | IESubInt16 (e1, e2) -> (
    match evalIexp prog stack frame_start e1, evalIexp prog stack frame_start e2 with
    | DInt16 a, DInt16 b ->
      let result = a - b in
      let clamped = max (-32768) (min 32767 result) in
      DInt16 clamped
    | _ -> error "Type mismatch in int16 subtraction")
  | IEMulInt16 (e1, e2) -> (
    match evalIexp prog stack frame_start e1, evalIexp prog stack frame_start e2 with
    | DInt16 a, DInt16 b ->
      let result = a * b in
      let clamped = max (-32768) (min 32767 result) in
      DInt16 clamped
    | _ -> error "Type mismatch in int16 multiplication")
  | IEDivInt16 (e1, e2) -> (
    match evalIexp prog stack frame_start e1, evalIexp prog stack frame_start e2 with
    | DInt16 a, DInt16 b ->
      let result = a / b in
      let clamped = max (-32768) (min 32767 result) in
      DInt16 clamped
    | _ -> error "Type mismatch in int16 division")
  | IEAddReal (e1, e2) -> (
    match evalIexp prog stack frame_start e1, evalIexp prog stack frame_start e2 with
    | DReal a, DReal b -> DReal (a +. b)
    | _ -> error "Type mismatch in real addition")
  | IESubReal (e1, e2) -> (
    match evalIexp prog stack frame_start e1, evalIexp prog stack frame_start e2 with
    | DReal a, DReal b -> DReal (a -. b)
    | _ -> error "Type mismatch in real subtraction")
  | IEMulReal (e1, e2) -> (
    match evalIexp prog stack frame_start e1, evalIexp prog stack frame_start e2 with
    | DReal a, DReal b -> DReal (a *. b)
    | _ -> error "Type mismatch in real multiplication")
  | IEDivReal (e1, e2) -> (
    match evalIexp prog stack frame_start e1, evalIexp prog stack frame_start e2 with
    | DReal a, DReal b -> DReal (a /. b)
    | _ -> error "Type mismatch in real division")
  (* Specialized fast comparison operations *)
  | IEEqInt (e1, e2) -> (
    match evalIexp prog stack frame_start e1, evalIexp prog stack frame_start e2 with
    | DInt a, DInt b -> DBool (a = b)
    | _ -> error "Type mismatch in integer equality")
  | IEEqInt16 (e1, e2) -> (
    match evalIexp prog stack frame_start e1, evalIexp prog stack frame_start e2 with
    | DInt16 a, DInt16 b -> DBool (a = b)
    | _ -> error "Type mismatch in int16 equality")
  | IEEqReal (e1, e2) -> (
    match evalIexp prog stack frame_start e1, evalIexp prog stack frame_start e2 with
    | DReal a, DReal b -> DBool (Float.equal a b)
    | _ -> error "Type mismatch in real equality")
  | IELtInt (e1, e2) -> (
    match evalIexp prog stack frame_start e1, evalIexp prog stack frame_start e2 with
    | DInt a, DInt b -> DBool (a < b)
    | _ -> error "Type mismatch in integer less than")
  | IELtInt16 (e1, e2) -> (
    match evalIexp prog stack frame_start e1, evalIexp prog stack frame_start e2 with
    | DInt16 a, DInt16 b -> DBool (a < b)
    | _ -> error "Type mismatch in int16 less than")
  | IELtReal (e1, e2) -> (
    match evalIexp prog stack frame_start e1, evalIexp prog stack frame_start e2 with
    | DReal a, DReal b -> DBool (a < b)
    | _ -> error "Type mismatch in real less than")
  | IEGtInt (e1, e2) -> (
    match evalIexp prog stack frame_start e1, evalIexp prog stack frame_start e2 with
    | DInt a, DInt b -> DBool (a > b)
    | _ -> error "Type mismatch in integer greater than")
  | IEGtInt16 (e1, e2) -> (
    match evalIexp prog stack frame_start e1, evalIexp prog stack frame_start e2 with
    | DInt16 a, DInt16 b -> DBool (a > b)
    | _ -> error "Type mismatch in int16 greater than")
  | IEGtReal (e1, e2) -> (
    match evalIexp prog stack frame_start e1, evalIexp prog stack frame_start e2 with
    | DReal a, DReal b -> DBool (a > b)
    | _ -> error "Type mismatch in real greater than")
  (* Inlined built-in functions *)
  | IEBuiltinSin e -> (
    match evalIexp prog stack frame_start e with
    | DReal f -> DReal (sin f)
    | _ -> error "Type mismatch in sin")
  | IEBuiltinSinh e -> (
    match evalIexp prog stack frame_start e with
    | DReal f -> DReal (sinh f)
    | _ -> error "Type mismatch in sin")
  | IEBuiltinCosh e -> (
    match evalIexp prog stack frame_start e with
    | DReal f -> DReal (cosh f)
    | _ -> error "Type mismatch in cosh")
  | IEBuiltinTanh e -> (
    match evalIexp prog stack frame_start e with
    | DReal f -> DReal (tanh f)
    | _ -> error "Type mismatch in sin")
  | IEBuiltinCos e -> (
    match evalIexp prog stack frame_start e with
    | DReal f -> DReal (cos f)
    | _ -> error "Type mismatch in cos")
  | IEBuiltinExp e -> (
    match evalIexp prog stack frame_start e with
    | DReal f -> DReal (Stdlib.exp f)
    | _ -> error "Type mismatch in exp")
  | IEBuiltinLog e -> (
    match evalIexp prog stack frame_start e with
    | DReal f -> DReal (log f)
    | _ -> error "Type mismatch in log")
  | IEBuiltinSqrt e -> (
    match evalIexp prog stack frame_start e with
    | DReal f -> DReal (sqrt f)
    | _ -> error "Type mismatch in sqrt")
  | IEBuiltinAbs e -> (
    match evalIexp prog stack frame_start e with
    | DReal f -> DReal (abs_float f)
    | DInt i -> DInt (abs i)
    | _ -> error "Type mismatch in abs")
  | IEBuiltinFloor e -> (
    match evalIexp prog stack frame_start e with
    | DReal f -> DReal (floor f)
    | _ -> error "Type mismatch in floor")
  | IEBuiltinPow (e1, e2) -> (
    match evalIexp prog stack frame_start e1, evalIexp prog stack frame_start e2 with
    | DReal x, DReal y -> DReal (x ** y)
    | _ -> error "Type mismatch in pow")
  | IEBuiltinTan e -> (
    match evalIexp prog stack frame_start e with
    | DReal f -> DReal (tan f)
    | _ -> error "Type mismatch in tan")
  | IEBuiltinLog10 e -> (
    match evalIexp prog stack frame_start e with
    | DReal f -> DReal (log10 f)
    | _ -> error "Type mismatch in log10")
  | IEBuiltinClipReal (x, min_v, max_v) -> (
    match
      evalIexp prog stack frame_start x, evalIexp prog stack frame_start min_v, evalIexp prog stack frame_start max_v
    with
    | DReal x_val, DReal min_val, DReal max_val -> DReal (min (max x_val min_val) max_val)
    | _ -> error "Type mismatch in clip_real")
  | IEBuiltinClipInt (x, min_v, max_v) -> (
    match
      evalIexp prog stack frame_start x, evalIexp prog stack frame_start min_v, evalIexp prog stack frame_start max_v
    with
    | DInt x_val, DInt min_val, DInt max_val -> DInt (min (max x_val min_val) max_val)
    | _ -> error "Type mismatch in clip_int")
  (* Constants *)
  | IEBuiltinPi -> DReal Float.pi
  | IEBuiltinEps -> DReal 1e-18
  | IEBuiltinSamplerate -> failwith "samplerate()"
  (* Random functions *)
  | IEBuiltinRandom -> DReal (Random.float 1.0)
  | IEBuiltinIrandom -> DInt (Random.int Int.max_int)
  (* Type conversion functions *)
  | IEBuiltinReal e -> (
    match evalIexp prog stack frame_start e with
    | DInt i -> DReal (float_of_int i)
    | DInt16 i -> DReal (float_of_int i)
    | DBool b ->
      DReal
        (if b then
           1.0
         else
           0.0)
    | DReal f -> DReal f
    | _ -> error "Type mismatch in real conversion")
  | IEBuiltinInt e -> (
    match evalIexp prog stack frame_start e with
    | DReal f -> DInt (int_of_float f)
    | DBool b ->
      DInt
        (if b then
           1
         else
           0)
    | DInt i -> DInt i
    | DInt16 i -> DInt i
    | _ -> error "Type mismatch in int conversion")
  | IEBuiltinInt16 e -> (
    match evalIexp prog stack frame_start e with
    | DReal f ->
      let i = int_of_float f in
      let clamped = max (-32768) (min 32767 i) in
      DInt16 clamped
    | DBool b ->
      DInt16
        (if b then
           1
         else
           0)
    | DInt i ->
      let clamped = max (-32768) (min 32767 i) in
      DInt16 clamped
    | DInt16 i -> DInt16 i
    | _ -> error "Type mismatch in int16 conversion")
  | IEBuiltinBool e -> (
    match evalIexp prog stack frame_start e with
    | DInt i -> DBool (i <> 0)
    | DInt16 i -> DBool (i <> 0)
    | DReal f -> DBool (f <> 0.0)
    | DBool b -> DBool b
    | _ -> error "Type mismatch in bool conversion")
  | IEBuiltinString e -> (
    match evalIexp prog stack frame_start e with
    | DInt i -> DString (string_of_int i)
    | DInt16 i -> DString (string_of_int i)
    | DReal f -> DString (string_of_float f)
    | DBool b -> DString (string_of_bool b)
    | DString s -> DString s
    | _ -> error "Type mismatch in string conversion")
  | IEBuiltinFixed e -> (
    match evalIexp prog stack frame_start e with
    | DReal f -> DReal f
    | DInt i -> DReal (float_of_int i)
    | _ -> error "Type mismatch in fixed conversion")
  (* Array/string functions *)
  | IEBuiltinSize e -> (
    match evalIexp prog stack frame_start e with
    | DArray arr -> DInt (Array.length arr)
    | _ -> error "Type mismatch in size - expected array")
  | IEBuiltinLength e -> (
    match evalIexp prog stack frame_start e with
    | DString s -> DInt (String.length s)
    | _ -> error "Type mismatch in length - expected string")
  | IEIndex (e, index) ->
    let arr_val = evalIexp prog stack frame_start e in
    let idx_val = evalIexp prog stack frame_start index in
    getArrayElement arr_val idx_val
  | IEArray elems ->
    let values = Array.of_list (CCList.map (evalIexp prog stack frame_start) elems) in
    DArray values
  | IECall (func_idx, args) ->
    let arg_vals = CCList.map (evalIexp prog stack frame_start) args in
    callFunction prog stack func_idx arg_vals
  | IECallExt _ -> error "Extenal evaluations are not possible"
  | IEIf (cond, then_, else_) -> (
    match evalIexp prog stack frame_start cond with
    | DBool true -> evalIexp prog stack frame_start then_
    | DBool false -> evalIexp prog stack frame_start else_
    | _ -> error "Invalid condition")
  | IETuple elems ->
    let values = Array.of_list (CCList.map (evalIexp prog stack frame_start) elems) in
    DArray values
  | IEMember (e, member_idx) ->
    let struct_val = evalIexp prog stack frame_start e in
    getStructMember struct_val member_idx
  | IERecord (descr, elems) ->
    let member_vals = Array.make (CCList.length descr.members) DVoid in
    CCList.iter
      (fun (idx, exp) ->
        let val_ = evalIexp prog stack frame_start exp in
        member_vals.(idx) <- val_)
      elems;
    DStruct member_vals


let evalProgram iprog (main_func_name_original : string) (args : dvalue list) : dvalue =
  let stack = createStack 1000 in
  let main_func_name = CCString.replace ~sub:"." ~by:"_" main_func_name_original in
  match Map.find_opt main_func_name iprog.ifunctions with
  | Some ifunc -> (
    let expected_args = CCList.length ifunc.iargs in
    let provided_args = CCList.length args in
    if expected_args = 1 && provided_args = 0 then (
      (* Try to find allocation function *)
      let alloc_func_name = main_func_name ^ "_type_alloc" in
      match Map.find_opt alloc_func_name iprog.ifunction_names with
      | Some alloc_idx -> (
        let state = callFunction iprog stack alloc_idx [] in
        match Map.find_opt main_func_name iprog.ifunction_names with
        | Some main_idx -> callFunction iprog stack main_idx [ state ]
        | None -> error ("Function not found: " ^ main_func_name))
      | None ->
        (* Debug: show available functions *)
        print_endline "Available functions:";
        Map.iter (fun name _ -> print_endline ("  " ^ name)) iprog.ifunctions;
        error ("Allocation function not found: " ^ alloc_func_name))
    else
      match Map.find_opt main_func_name iprog.ifunction_names with
      | Some func_idx -> callFunction iprog stack func_idx args
      | None -> error ("Function not found: " ^ main_func_name))
  | None -> error ("Function not found: " ^ main_func_name)
