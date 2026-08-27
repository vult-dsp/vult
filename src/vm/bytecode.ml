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

(* Runtime values for the bytecode VM *)
type value =
  | Void
  | Int of int
  | Int16 of int
  | Real of float
  | Bool of bool
  | String of string
  | Array of value array
  | List of value list ref
  | Struct of value array

(* Built-in function identifiers *)
type builtin_id =
  | BI_sin
  | BI_cos
  | BI_tan
  | BI_sinh
  | BI_cosh
  | BI_tanh
  | BI_exp
  | BI_log
  | BI_log10
  | BI_sqrt
  | BI_abs
  | BI_floor
  | BI_pow
  | BI_clip_real
  | BI_clip_int
  | BI_pi
  | BI_eps
  | BI_samplerate
  | BI_random
  | BI_irandom
  | BI_real
  | BI_int
  | BI_int16
  | BI_bool
  | BI_string
  | BI_fix16
  | BI_size
  | BI_length
  | BI_list_size
  | BI_list_capacity
  | BI_list_append
  | BI_list_insert
  | BI_list_remove
  | BI_list_clear
  | BI_list_reserve
  | BI_list_get
  | BI_list_set
  | BI_ceil
  | BI_atan2
  | BI_asin
  | BI_acos
  | BI_atan
  | BI_min
  | BI_max

(* Binary operator tags for the generic BinOp instruction *)
type binop_tag = BLe | BGe | BNe | BLand | BLor | BBand | BBor | BBxor | BLsh | BRsh | BMod

(* Bytecode instructions (ADT form for compilation and debugging) *)
type instruction =
  (* Stack & Memory *)
  | LoadLocal of int
  | StoreLocal of int
  | Loadc of int
  | Pop
  | Dup
  (* Type-specialized arithmetic *)
  | AddInt
  | SubInt
  | MulInt
  | DivInt
  | AddInt16
  | SubInt16
  | MulInt16
  | DivInt16
  | AddReal
  | SubReal
  | MulReal
  | DivReal
  | ModInt
  | ModInt16
  | ModReal
  (* Unary *)
  | NegInt
  | NegReal
  | NegInt16
  | Not
  (* Type-specialized comparisons *)
  | EqInt
  | EqInt16
  | EqReal
  | LtInt
  | LtInt16
  | LtReal
  | GtInt
  | GtInt16
  | GtReal
  (* Generic binary ops *)
  | BinOp of binop_tag
  (* Control flow *)
  | Jump of int
  | JumpIfFalse of int
  | JumpIfTrue of int
  | Halt
  (* Function calls *)
  | Call of int * int
  | Return
  | ReturnVoid
  | CallBuiltin of builtin_id * int
  (* Data structures *)
  | MakeArray of int
  | MakeStruct of int
  | MakeTuple of int
  | IndexLoad
  | IndexStore
  | MemberLoad of int
  | MemberStore of int
  | UnpackTuple of int * int list
  | MakeRecord of int * int
  (* External calls *)
  | CallExternal of string * int
  (* Fused member access opcodes *)
  | LoadLocalMember of int * int
  | StoreLocalMember of int * int
  (* Fused dup+store opcodes *)
  | DupStoreLocal of int
  | DupStoreLocalMember of int * int
  (* Specialized call opcodes for common arities *)
  | Call0 of int
  | Call1 of int
  | Call2 of int
  | Call3 of int
  (* Specialized local access opcodes for indices 0-3 *)
  | LoadLocal0
  | LoadLocal1
  | LoadLocal2
  | LoadLocal3
  | StoreLocal0
  | StoreLocal1
  | StoreLocal2
  | StoreLocal3
  | Loadc0
  | Loadc1
  | Loadc2
  | Loadc3
  (* Specialized dup+store for indices 0-3 *)
  | DupStoreLocal0
  | DupStoreLocal1
  | DupStoreLocal2
  | DupStoreLocal3
  (* Fused compare+branch opcodes *)
  | LtIntJumpIfFalse of int
  | GtIntJumpIfFalse of int
  | EqIntJumpIfFalse of int
  | LtRealJumpIfFalse of int
  | GtRealJumpIfFalse of int
  | EqRealJumpIfFalse of int

(* Compiled function *)
type bc_func = {name: string; entry_pc: int; n_args: int; n_locals: int}

(* Compiled program *)
type bc_prog =
  {code: instruction array; constants: value array; functions: bc_func array; function_names: (string, int) Hashtbl.t}

(* Opcode encoding constants *)
let op_load_local = 0

let op_store_local = 1

let op_loadc = 2

let op_pop = 3

let op_dup = 4

let op_add_int = 5

let op_sub_int = 6

let op_mul_int = 7

let op_div_int = 8

let op_add_int16 = 9

let op_sub_int16 = 10

let op_mul_int16 = 11

let op_div_int16 = 12

let op_add_real = 13

let op_sub_real = 14

let op_mul_real = 15

let op_div_real = 16

let op_mod_int = 17

let op_mod_int16 = 18

let op_mod_real = 19

let op_neg_int = 20

let op_neg_real = 21

let op_neg_int16 = 22

let op_not = 23

let op_eq_int = 24

let op_eq_int16 = 25

let op_eq_real = 26

let op_lt_int = 27

let op_lt_int16 = 28

let op_lt_real = 29

let op_gt_int = 30

let op_gt_int16 = 31

let op_gt_real = 32

let op_binop = 33

let op_jump = 34

let op_jump_if_false = 35

let op_jump_if_true = 36

let op_halt = 37

let op_call = 38

let op_return = 39

let op_return_void = 40

let op_call_builtin = 41

let op_make_array = 42

let op_make_struct = 43

let op_make_tuple = 44

let op_index_load = 45

let op_index_store = 46

let op_member_load = 47

let op_member_store = 48

let op_unpack_tuple = 49

let op_make_record = 50

let op_call_external = 51

let op_load_local_member = 52

let op_store_local_member = 53

let op_dup_store_local = 54

let op_dup_store_local_member = 55

let op_call0 = 56

let op_call1 = 57

let op_call2 = 58

let op_call3 = 59

let op_load_local0 = 60

let op_load_local1 = 61

let op_load_local2 = 62

let op_load_local3 = 63

let op_store_local0 = 64

let op_store_local1 = 65

let op_store_local2 = 66

let op_store_local3 = 67

let op_loadc0 = 68

let op_loadc1 = 69

let op_loadc2 = 70

let op_loadc3 = 71

let op_dup_store_local0 = 72

let op_dup_store_local1 = 73

let op_dup_store_local2 = 74

let op_dup_store_local3 = 75

let op_lt_int_jump_if_false = 76

let op_gt_int_jump_if_false = 77

let op_eq_int_jump_if_false = 78

let op_lt_real_jump_if_false = 79

let op_gt_real_jump_if_false = 80

let op_eq_real_jump_if_false = 81

(* Encode binop_tag to int *)
let encodeBinopTag (tag : binop_tag) : int =
  match tag with
  | BLe ->
      0
  | BGe ->
      1
  | BNe ->
      2
  | BLand ->
      3
  | BLor ->
      4
  | BBand ->
      5
  | BBor ->
      6
  | BBxor ->
      7
  | BLsh ->
      8
  | BRsh ->
      9
  | BMod ->
      10

(* Encode builtin_id to int *)
let encodeBuiltinId (id : builtin_id) : int =
  match id with
  | BI_sin ->
      0
  | BI_cos ->
      1
  | BI_tan ->
      2
  | BI_sinh ->
      3
  | BI_cosh ->
      4
  | BI_tanh ->
      5
  | BI_exp ->
      6
  | BI_log ->
      7
  | BI_log10 ->
      8
  | BI_sqrt ->
      9
  | BI_abs ->
      10
  | BI_floor ->
      11
  | BI_pow ->
      12
  | BI_clip_real ->
      13
  | BI_clip_int ->
      14
  | BI_pi ->
      15
  | BI_eps ->
      16
  | BI_samplerate ->
      17
  | BI_random ->
      18
  | BI_irandom ->
      19
  | BI_real ->
      20
  | BI_int ->
      21
  | BI_int16 ->
      22
  | BI_bool ->
      23
  | BI_string ->
      24
  | BI_fix16 ->
      25
  | BI_size ->
      26
  | BI_length ->
      27
  | BI_list_size ->
      28
  | BI_list_capacity ->
      29
  | BI_list_append ->
      30
  | BI_list_insert ->
      31
  | BI_list_remove ->
      32
  | BI_list_clear ->
      33
  | BI_list_reserve ->
      34
  | BI_list_get ->
      35
  | BI_list_set ->
      36
  | BI_ceil ->
      37
  | BI_atan2 ->
      38
  | BI_asin ->
      39
  | BI_acos ->
      40
  | BI_atan ->
      41
  | BI_min ->
      42
  | BI_max ->
      43

(* Encode a single instruction into a list of ints (appended to acc in reverse) *)
let encodeInstruction (instr : instruction) (acc : int list) : int list =
  match instr with
  | LoadLocal idx ->
      idx :: op_load_local :: acc
  | StoreLocal idx ->
      idx :: op_store_local :: acc
  | Loadc idx ->
      idx :: op_loadc :: acc
  | Pop ->
      op_pop :: acc
  | Dup ->
      op_dup :: acc
  | AddInt ->
      op_add_int :: acc
  | SubInt ->
      op_sub_int :: acc
  | MulInt ->
      op_mul_int :: acc
  | DivInt ->
      op_div_int :: acc
  | AddInt16 ->
      op_add_int16 :: acc
  | SubInt16 ->
      op_sub_int16 :: acc
  | MulInt16 ->
      op_mul_int16 :: acc
  | DivInt16 ->
      op_div_int16 :: acc
  | AddReal ->
      op_add_real :: acc
  | SubReal ->
      op_sub_real :: acc
  | MulReal ->
      op_mul_real :: acc
  | DivReal ->
      op_div_real :: acc
  | ModInt ->
      op_mod_int :: acc
  | ModInt16 ->
      op_mod_int16 :: acc
  | ModReal ->
      op_mod_real :: acc
  | NegInt ->
      op_neg_int :: acc
  | NegReal ->
      op_neg_real :: acc
  | NegInt16 ->
      op_neg_int16 :: acc
  | Not ->
      op_not :: acc
  | EqInt ->
      op_eq_int :: acc
  | EqInt16 ->
      op_eq_int16 :: acc
  | EqReal ->
      op_eq_real :: acc
  | LtInt ->
      op_lt_int :: acc
  | LtInt16 ->
      op_lt_int16 :: acc
  | LtReal ->
      op_lt_real :: acc
  | GtInt ->
      op_gt_int :: acc
  | GtInt16 ->
      op_gt_int16 :: acc
  | GtReal ->
      op_gt_real :: acc
  | BinOp tag ->
      encodeBinopTag tag :: op_binop :: acc
  | Jump target ->
      target :: op_jump :: acc
  | JumpIfFalse target ->
      target :: op_jump_if_false :: acc
  | JumpIfTrue target ->
      target :: op_jump_if_true :: acc
  | Halt ->
      op_halt :: acc
  | Call (func_idx, nargs) ->
      nargs :: func_idx :: op_call :: acc
  | Return ->
      op_return :: acc
  | ReturnVoid ->
      op_return_void :: acc
  | CallBuiltin (id, nargs) ->
      nargs :: encodeBuiltinId id :: op_call_builtin :: acc
  | MakeArray n ->
      n :: op_make_array :: acc
  | MakeStruct n ->
      n :: op_make_struct :: acc
  | MakeTuple n ->
      n :: op_make_tuple :: acc
  | IndexLoad ->
      op_index_load :: acc
  | IndexStore ->
      op_index_store :: acc
  | MemberLoad idx ->
      idx :: op_member_load :: acc
  | MemberStore idx ->
      idx :: op_member_store :: acc
  | UnpackTuple (n, offsets) ->
      let acc = op_unpack_tuple :: acc in
      let acc = n :: acc in
      CCList.fold_left (fun a o -> o :: a) acc offsets
  | MakeRecord (struct_idx, n) ->
      n :: struct_idx :: op_make_record :: acc
  | CallExternal (name, nargs) ->
      (* Encode external name as hash for dispatch *)
      nargs :: Hashtbl.hash name :: op_call_external :: acc
  | LoadLocalMember (local_idx, member_idx) ->
      member_idx :: local_idx :: op_load_local_member :: acc
  | StoreLocalMember (local_idx, member_idx) ->
      member_idx :: local_idx :: op_store_local_member :: acc
  | DupStoreLocal idx ->
      idx :: op_dup_store_local :: acc
  | DupStoreLocalMember (local_idx, member_idx) ->
      member_idx :: local_idx :: op_dup_store_local_member :: acc
  | Call0 func_idx ->
      func_idx :: op_call0 :: acc
  | Call1 func_idx ->
      func_idx :: op_call1 :: acc
  | Call2 func_idx ->
      func_idx :: op_call2 :: acc
  | Call3 func_idx ->
      func_idx :: op_call3 :: acc
  | LoadLocal0 ->
      op_load_local0 :: acc
  | LoadLocal1 ->
      op_load_local1 :: acc
  | LoadLocal2 ->
      op_load_local2 :: acc
  | LoadLocal3 ->
      op_load_local3 :: acc
  | StoreLocal0 ->
      op_store_local0 :: acc
  | StoreLocal1 ->
      op_store_local1 :: acc
  | StoreLocal2 ->
      op_store_local2 :: acc
  | StoreLocal3 ->
      op_store_local3 :: acc
  | Loadc0 ->
      op_loadc0 :: acc
  | Loadc1 ->
      op_loadc1 :: acc
  | Loadc2 ->
      op_loadc2 :: acc
  | Loadc3 ->
      op_loadc3 :: acc
  | DupStoreLocal0 ->
      op_dup_store_local0 :: acc
  | DupStoreLocal1 ->
      op_dup_store_local1 :: acc
  | DupStoreLocal2 ->
      op_dup_store_local2 :: acc
  | DupStoreLocal3 ->
      op_dup_store_local3 :: acc
  | LtIntJumpIfFalse target ->
      target :: op_lt_int_jump_if_false :: acc
  | GtIntJumpIfFalse target ->
      target :: op_gt_int_jump_if_false :: acc
  | EqIntJumpIfFalse target ->
      target :: op_eq_int_jump_if_false :: acc
  | LtRealJumpIfFalse target ->
      target :: op_lt_real_jump_if_false :: acc
  | GtRealJumpIfFalse target ->
      target :: op_gt_real_jump_if_false :: acc
  | EqRealJumpIfFalse target ->
      target :: op_eq_real_jump_if_false :: acc

(* Encode instruction list to int array *)
let encode (instrs : instruction list) : int array =
  let rev_ints = CCList.fold_left (fun acc instr -> encodeInstruction instr acc) [] instrs in
  Array.of_list (CCList.rev rev_ints)

(* Size of an instruction in encoded ints *)
let instrSize (instr : instruction) : int =
  match instr with
  | LoadLocal _ | StoreLocal _ | Loadc _ ->
      2
  | Pop | Dup ->
      1
  | AddInt | SubInt | MulInt | DivInt ->
      1
  | AddInt16 | SubInt16 | MulInt16 | DivInt16 ->
      1
  | AddReal | SubReal | MulReal | DivReal ->
      1
  | ModInt | ModInt16 | ModReal ->
      1
  | NegInt | NegReal | NegInt16 | Not ->
      1
  | EqInt | EqInt16 | EqReal ->
      1
  | LtInt | LtInt16 | LtReal ->
      1
  | GtInt | GtInt16 | GtReal ->
      1
  | BinOp _ ->
      2
  | Jump _ | JumpIfFalse _ | JumpIfTrue _ ->
      2
  | Halt ->
      1
  | Call _ ->
      3
  | Return | ReturnVoid ->
      1
  | CallBuiltin _ ->
      3
  | MakeArray _ | MakeStruct _ | MakeTuple _ ->
      2
  | IndexLoad | IndexStore ->
      1
  | MemberLoad _ | MemberStore _ ->
      2
  | UnpackTuple (n, _) ->
      2 + n
  | MakeRecord _ ->
      3
  | CallExternal _ ->
      3
  | LoadLocalMember _ | StoreLocalMember _ ->
      3
  | DupStoreLocal _ ->
      2
  | DupStoreLocalMember _ ->
      3
  | Call0 _ | Call1 _ | Call2 _ | Call3 _ ->
      2
  | LoadLocal0 | LoadLocal1 | LoadLocal2 | LoadLocal3 ->
      1
  | StoreLocal0 | StoreLocal1 | StoreLocal2 | StoreLocal3 ->
      1
  | Loadc0 | Loadc1 | Loadc2 | Loadc3 ->
      1
  | DupStoreLocal0 | DupStoreLocal1 | DupStoreLocal2 | DupStoreLocal3 ->
      1
  | LtIntJumpIfFalse _ | GtIntJumpIfFalse _ | EqIntJumpIfFalse _ ->
      2
  | LtRealJumpIfFalse _ | GtRealJumpIfFalse _ | EqRealJumpIfFalse _ ->
      2

(* Print a value for output *)
let rec printValue (v : value) : string =
  match v with
  | Void ->
      "void"
  | Int i ->
      string_of_int i
  | Int16 i ->
      string_of_int i
  | Real f ->
      string_of_float f
  | Bool b ->
      string_of_bool b
  | String s ->
      "\"" ^ s ^ "\""
  | Array arr ->
      "[" ^ String.concat "; " (CCList.map printValue (Array.to_list arr)) ^ "]"
  | List list_ref ->
      "list[" ^ String.concat "; " (CCList.map printValue !list_ref) ^ "]"
  | Struct arr ->
      "{" ^ String.concat "; " (Array.to_list (Array.mapi (fun i v -> string_of_int i ^ ":" ^ printValue v) arr)) ^ "}"

(* Print builtin name *)
let printBuiltinId (id : builtin_id) : string =
  match id with
  | BI_sin ->
      "sin"
  | BI_cos ->
      "cos"
  | BI_tan ->
      "tan"
  | BI_sinh ->
      "sinh"
  | BI_cosh ->
      "cosh"
  | BI_tanh ->
      "tanh"
  | BI_exp ->
      "exp"
  | BI_log ->
      "log"
  | BI_log10 ->
      "log10"
  | BI_sqrt ->
      "sqrt"
  | BI_abs ->
      "abs"
  | BI_floor ->
      "floor"
  | BI_pow ->
      "pow"
  | BI_clip_real ->
      "clip_real"
  | BI_clip_int ->
      "clip_int"
  | BI_pi ->
      "pi"
  | BI_eps ->
      "eps"
  | BI_samplerate ->
      "samplerate"
  | BI_random ->
      "random"
  | BI_irandom ->
      "irandom"
  | BI_real ->
      "real"
  | BI_int ->
      "int"
  | BI_int16 ->
      "int16"
  | BI_bool ->
      "bool"
  | BI_string ->
      "string"
  | BI_fix16 ->
      "fix16"
  | BI_size ->
      "size"
  | BI_length ->
      "length"
  | BI_list_size ->
      "list_size"
  | BI_list_capacity ->
      "list_capacity"
  | BI_list_append ->
      "list_append"
  | BI_list_insert ->
      "list_insert"
  | BI_list_remove ->
      "list_remove"
  | BI_list_clear ->
      "list_clear"
  | BI_list_reserve ->
      "list_reserve"
  | BI_list_get ->
      "list_get"
  | BI_list_set ->
      "list_set"
  | BI_ceil ->
      "ceil"
  | BI_atan2 ->
      "atan2"
  | BI_asin ->
      "asin"
  | BI_acos ->
      "acos"
  | BI_atan ->
      "atan"
  | BI_min ->
      "min"
  | BI_max ->
      "max"

(* Print binop tag *)
let printBinopTag (tag : binop_tag) : string =
  match tag with
  | BLe ->
      "<="
  | BGe ->
      ">="
  | BNe ->
      "<>"
  | BLand ->
      "&&"
  | BLor ->
      "||"
  | BBand ->
      "&"
  | BBor ->
      "|"
  | BBxor ->
      "^"
  | BLsh ->
      "<<"
  | BRsh ->
      ">>"
  | BMod ->
      "%"

(* Print a single instruction *)
let printInstruction (instr : instruction) : string =
  match instr with
  | LoadLocal idx ->
      Printf.sprintf "LoadLocal %d" idx
  | StoreLocal idx ->
      Printf.sprintf "StoreLocal %d" idx
  | Loadc idx ->
      Printf.sprintf "Loadc %d" idx
  | Pop ->
      "Pop"
  | Dup ->
      "Dup"
  | AddInt ->
      "AddInt"
  | SubInt ->
      "SubInt"
  | MulInt ->
      "MulInt"
  | DivInt ->
      "DivInt"
  | AddInt16 ->
      "AddInt16"
  | SubInt16 ->
      "SubInt16"
  | MulInt16 ->
      "MulInt16"
  | DivInt16 ->
      "DivInt16"
  | AddReal ->
      "AddReal"
  | SubReal ->
      "SubReal"
  | MulReal ->
      "MulReal"
  | DivReal ->
      "DivReal"
  | ModInt ->
      "ModInt"
  | ModInt16 ->
      "ModInt16"
  | ModReal ->
      "ModReal"
  | NegInt ->
      "NegInt"
  | NegReal ->
      "NegReal"
  | NegInt16 ->
      "NegInt16"
  | Not ->
      "Not"
  | EqInt ->
      "EqInt"
  | EqInt16 ->
      "EqInt16"
  | EqReal ->
      "EqReal"
  | LtInt ->
      "LtInt"
  | LtInt16 ->
      "LtInt16"
  | LtReal ->
      "LtReal"
  | GtInt ->
      "GtInt"
  | GtInt16 ->
      "GtInt16"
  | GtReal ->
      "GtReal"
  | BinOp tag ->
      Printf.sprintf "BinOp %s" (printBinopTag tag)
  | Jump target ->
      Printf.sprintf "Jump %d" target
  | JumpIfFalse target ->
      Printf.sprintf "JumpIfFalse %d" target
  | JumpIfTrue target ->
      Printf.sprintf "JumpIfTrue %d" target
  | Halt ->
      "Halt"
  | Call (func_idx, nargs) ->
      Printf.sprintf "Call %d %d" func_idx nargs
  | Return ->
      "Return"
  | ReturnVoid ->
      "ReturnVoid"
  | CallBuiltin (id, nargs) ->
      Printf.sprintf "CallBuiltin %s %d" (printBuiltinId id) nargs
  | MakeArray n ->
      Printf.sprintf "MakeArray %d" n
  | MakeStruct n ->
      Printf.sprintf "MakeStruct %d" n
  | MakeTuple n ->
      Printf.sprintf "MakeTuple %d" n
  | IndexLoad ->
      "IndexLoad"
  | IndexStore ->
      "IndexStore"
  | MemberLoad idx ->
      Printf.sprintf "MemberLoad %d" idx
  | MemberStore idx ->
      Printf.sprintf "MemberStore %d" idx
  | UnpackTuple (n, offsets) ->
      Printf.sprintf "UnpackTuple %d [%s]" n (String.concat "," (CCList.map string_of_int offsets))
  | MakeRecord (struct_idx, n) ->
      Printf.sprintf "MakeRecord %d %d" struct_idx n
  | CallExternal (name, nargs) ->
      Printf.sprintf "CallExternal %s %d" name nargs
  | LoadLocalMember (local_idx, member_idx) ->
      Printf.sprintf "LoadLocalMember %d %d" local_idx member_idx
  | StoreLocalMember (local_idx, member_idx) ->
      Printf.sprintf "StoreLocalMember %d %d" local_idx member_idx
  | DupStoreLocal idx ->
      Printf.sprintf "DupStoreLocal %d" idx
  | DupStoreLocalMember (local_idx, member_idx) ->
      Printf.sprintf "DupStoreLocalMember %d %d" local_idx member_idx
  | Call0 func_idx ->
      Printf.sprintf "Call0 %d" func_idx
  | Call1 func_idx ->
      Printf.sprintf "Call1 %d" func_idx
  | Call2 func_idx ->
      Printf.sprintf "Call2 %d" func_idx
  | Call3 func_idx ->
      Printf.sprintf "Call3 %d" func_idx
  | LoadLocal0 ->
      "LoadLocal0"
  | LoadLocal1 ->
      "LoadLocal1"
  | LoadLocal2 ->
      "LoadLocal2"
  | LoadLocal3 ->
      "LoadLocal3"
  | StoreLocal0 ->
      "StoreLocal0"
  | StoreLocal1 ->
      "StoreLocal1"
  | StoreLocal2 ->
      "StoreLocal2"
  | StoreLocal3 ->
      "StoreLocal3"
  | Loadc0 ->
      "Loadc0"
  | Loadc1 ->
      "Loadc1"
  | Loadc2 ->
      "Loadc2"
  | Loadc3 ->
      "Loadc3"
  | DupStoreLocal0 ->
      "DupStoreLocal0"
  | DupStoreLocal1 ->
      "DupStoreLocal1"
  | DupStoreLocal2 ->
      "DupStoreLocal2"
  | DupStoreLocal3 ->
      "DupStoreLocal3"
  | LtIntJumpIfFalse target ->
      Printf.sprintf "LtIntJumpIfFalse %d" target
  | GtIntJumpIfFalse target ->
      Printf.sprintf "GtIntJumpIfFalse %d" target
  | EqIntJumpIfFalse target ->
      Printf.sprintf "EqIntJumpIfFalse %d" target
  | LtRealJumpIfFalse target ->
      Printf.sprintf "LtRealJumpIfFalse %d" target
  | GtRealJumpIfFalse target ->
      Printf.sprintf "GtRealJumpIfFalse %d" target
  | EqRealJumpIfFalse target ->
      Printf.sprintf "EqRealJumpIfFalse %d" target

(* Dump an entire program *)
let dump (prog : bc_prog) : string =
  let buf = Buffer.create 1024 in
  Buffer.add_string buf "=== Constants ===\n" ;
  Array.iteri (fun i v -> Buffer.add_string buf (Printf.sprintf "  [%d] %s\n" i (printValue v))) prog.constants ;
  Buffer.add_string buf "\n=== Functions ===\n" ;
  Array.iteri
    (fun i f ->
      Buffer.add_string buf
        (Printf.sprintf "  [%d] %s (entry=%d, args=%d, locals=%d)\n" i f.name f.entry_pc f.n_args f.n_locals) )
    prog.functions ;
  Buffer.add_string buf "\n=== Code ===\n" ;
  let pc = ref 0 in
  let code = prog.code in
  let len = Array.length code in
  while !pc < len do
    let instr = code.(!pc) in
    Buffer.add_string buf (Printf.sprintf "  %04d: %s\n" !pc (printInstruction instr)) ;
    pc := !pc + 1
  done ;
  Buffer.contents buf
