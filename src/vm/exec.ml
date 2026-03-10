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

open Bytecode

exception VM_error of string

let error (msg : string) : 'a = raise (VM_error msg)

(* VM state — call stack uses parallel int arrays to avoid heap allocation *)
type vm_state =
  { code: int array (* encoded bytecode *)
  ; mutable pc: int (* program counter into encoded array *)
  ; stack: value array (* operand stack *)
  ; mutable sp: int (* stack pointer *)
  ; locals: value array (* all frames contiguous *)
  ; mutable fp: int (* frame pointer *)
  ; mutable locals_sp: int (* next free slot in locals *)
  ; cs_return_pcs: int array (* call stack: return PCs *)
  ; cs_saved_fps: int array (* call stack: saved frame pointers *)
  ; cs_saved_sps: int array (* call stack: saved stack pointers *)
  ; cs_saved_locals_sps: int array (* call stack: saved locals stack pointers *)
  ; mutable csp: int (* call stack pointer *)
  ; constants: value array (* constant pool *)
  ; functions: bc_func array (* function table *)
  ; function_names: (string, int) Hashtbl.t
  ; sample_rate: float option
  ; prog: bc_prog (* keep the program for debugging *) }

(* Create a VM from a compiled program *)
let createVM ?(sample_rate : float option) (prog : bc_prog) : vm_state =
  let encoded = encode (Array.to_list prog.code) in
  { code= encoded
  ; pc= 0
  ; stack= Array.make 65536 Void
  ; sp= 0
  ; locals= Array.make 65536 Void
  ; fp= 0
  ; locals_sp= 0
  ; cs_return_pcs= Array.make 1024 0
  ; cs_saved_fps= Array.make 1024 0
  ; cs_saved_sps= Array.make 1024 0
  ; cs_saved_locals_sps= Array.make 1024 0
  ; csp= 0
  ; constants= prog.constants
  ; functions= prog.functions
  ; function_names= prog.function_names
  ; sample_rate
  ; prog }

(* Push a value onto the operand stack *)
let push (vm : vm_state) (v : value) : unit =
  Array.unsafe_set vm.stack vm.sp v ;
  vm.sp <- vm.sp + 1

(* Pop a value from the operand stack *)
let pop (vm : vm_state) : value =
  vm.sp <- vm.sp - 1 ;
  Array.unsafe_get vm.stack vm.sp

(* Execute a built-in function *)
let execBuiltin (vm : vm_state) (id : int) (nargs : int) : unit =
  match id with
  (* Math functions - 1 arg *)
  | 0 -> (
      (* sin *)
      let v = pop vm in
      match v with Real f -> push vm (Real (sin f)) | _ -> error "sin: expected real" )
  | 1 -> (
      (* cos *)
      let v = pop vm in
      match v with Real f -> push vm (Real (cos f)) | _ -> error "cos: expected real" )
  | 2 -> (
      (* tan *)
      let v = pop vm in
      match v with Real f -> push vm (Real (tan f)) | _ -> error "tan: expected real" )
  | 3 -> (
      (* sinh *)
      let v = pop vm in
      match v with Real f -> push vm (Real (sinh f)) | _ -> error "sinh: expected real" )
  | 4 -> (
      (* cosh *)
      let v = pop vm in
      match v with Real f -> push vm (Real (cosh f)) | _ -> error "cosh: expected real" )
  | 5 -> (
      (* tanh *)
      let v = pop vm in
      match v with Real f -> push vm (Real (tanh f)) | _ -> error "tanh: expected real" )
  | 6 -> (
      (* exp *)
      let v = pop vm in
      match v with Real f -> push vm (Real (exp f)) | _ -> error "exp: expected real" )
  | 7 -> (
      (* log *)
      let v = pop vm in
      match v with Real f -> push vm (Real (log f)) | _ -> error "log: expected real" )
  | 8 -> (
      (* log10 *)
      let v = pop vm in
      match v with Real f -> push vm (Real (log10 f)) | _ -> error "log10: expected real" )
  | 9 -> (
      (* sqrt *)
      let v = pop vm in
      match v with Real f -> push vm (Real (sqrt f)) | _ -> error "sqrt: expected real" )
  | 10 -> (
      (* abs *)
      let v = pop vm in
      match v with
      | Real f ->
          push vm (Real (abs_float f))
      | Int i ->
          push vm (Int (abs i))
      | Int16 i ->
          push vm (Int16 (abs i))
      | _ ->
          error "abs: expected numeric" )
  | 11 -> (
      (* floor *)
      let v = pop vm in
      match v with Real f -> push vm (Real (floor f)) | _ -> error "floor: expected real" )
  | 12 -> (
      (* pow *)
      let e = pop vm in
      let b = pop vm in
      match (b, e) with Real a, Real p -> push vm (Real (a ** p)) | _ -> error "pow: expected reals" )
  | 13 -> (
      (* clip_real *)
      let max_v = pop vm in
      let min_v = pop vm in
      let x = pop vm in
      match (x, min_v, max_v) with
      | Real xf, Real minf, Real maxf ->
          push vm (Real (max minf (min maxf xf)))
      | _ ->
          error "clip_real: expected reals" )
  | 14 -> (
      (* clip_int *)
      let max_v = pop vm in
      let min_v = pop vm in
      let x = pop vm in
      match (x, min_v, max_v) with
      | Int xi, Int mini, Int maxi ->
          push vm (Int (max mini (min maxi xi)))
      | Int16 xi, Int16 mini, Int16 maxi ->
          push vm (Int16 (max mini (min maxi xi)))
      | _ ->
          error "clip_int: expected ints" )
  | 15 ->
      (* pi *)
      push vm (Real Float.pi)
  | 16 ->
      (* eps *)
      push vm (Real Float.epsilon)
  | 17 -> (
    (* samplerate *)
    match vm.sample_rate with
    | Some sr ->
        push vm (Real sr)
    | None ->
        push vm (Real 44100.0) )
  | 18 ->
      (* random *)
      push vm (Real (Random.float 1.0))
  | 19 ->
      (* irandom *)
      push vm (Int (Random.int Int.max_int))
  | 20 -> (
      (* real *)
      let v = pop vm in
      match v with
      | Real f ->
          push vm (Real f)
      | Int i ->
          push vm (Real (float_of_int i))
      | Int16 i ->
          push vm (Real (float_of_int i))
      | Bool b ->
          push vm (Real (if b then 1.0 else 0.0))
      | _ ->
          error "real: cannot convert" )
  | 21 -> (
      (* int *)
      let v = pop vm in
      match v with
      | Int i ->
          push vm (Int i)
      | Int16 i ->
          push vm (Int i)
      | Real f ->
          push vm (Int (int_of_float f))
      | Bool b ->
          push vm (Int (if b then 1 else 0))
      | _ ->
          error "int: cannot convert" )
  | 22 -> (
      (* int16 *)
      let v = pop vm in
      match v with
      | Int i ->
          push vm (Int16 (max (-32768) (min 32767 i)))
      | Int16 i ->
          push vm (Int16 i)
      | Real f ->
          push vm (Int16 (max (-32768) (min 32767 (int_of_float f))))
      | Bool b ->
          push vm (Int16 (if b then 1 else 0))
      | _ ->
          error "int16: cannot convert" )
  | 23 -> (
      (* bool *)
      let v = pop vm in
      match v with
      | Bool b ->
          push vm (Bool b)
      | Int i ->
          push vm (Bool (i <> 0))
      | Int16 i ->
          push vm (Bool (i <> 0))
      | Real f ->
          push vm (Bool (f <> 0.0))
      | _ ->
          error "bool: cannot convert" )
  | 24 ->
      (* string *)
      let v = pop vm in
      push vm (String (printValue v))
  | 25 -> (
      (* fix16 *)
      let v = pop vm in
      match v with
      | Real f ->
          push vm (Real f)
      | Int i ->
          push vm (Real (float_of_int i))
      | _ ->
          error "fix16: cannot convert" )
  | 26 -> (
      (* size *)
      let v = pop vm in
      match v with Array arr -> push vm (Int (Array.length arr)) | _ -> error "size: expected array" )
  | 27 -> (
      (* length *)
      let v = pop vm in
      match v with String s -> push vm (Int (String.length s)) | _ -> error "length: expected string" )
  | 28 -> (
      (* list_size *)
      let v = pop vm in
      match v with List lr -> push vm (Int (CCList.length !lr)) | _ -> error "list_size: expected list" )
  | 29 -> (
      (* list_capacity *)
      let v = pop vm in
      match v with List lr -> push vm (Int (CCList.length !lr)) | _ -> error "list_capacity: expected list" )
  | 30 -> (
      (* list_append *)
      let elem = pop vm in
      let lst = pop vm in
      match lst with
      | List lr ->
          lr := !lr @ [elem] ;
          push vm Void
      | _ ->
          error "list_append: expected list" )
  | 31 -> (
      (* list_insert *)
      let elem = pop vm in
      let idx = pop vm in
      let lst = pop vm in
      match (lst, idx) with
      | List lr, Int i ->
          let before = CCList.take i !lr in
          let after = CCList.drop i !lr in
          lr := before @ [elem] @ after ;
          push vm Void
      | _ ->
          error "list_insert: expected list and int" )
  | 32 -> (
      (* list_remove *)
      let idx = pop vm in
      let lst = pop vm in
      match (lst, idx) with
      | List lr, Int i ->
          let before = CCList.take i !lr in
          let after = CCList.drop (i + 1) !lr in
          lr := before @ after ;
          push vm Void
      | _ ->
          error "list_remove: expected list and int" )
  | 33 -> (
      (* list_clear *)
      let lst = pop vm in
      match lst with
      | List lr ->
          lr := [] ;
          push vm Void
      | _ ->
          error "list_clear: expected list" )
  | 34 -> (
      (* list_reserve *)
      let _n = pop vm in
      let lst = pop vm in
      match lst with List _ -> push vm Void (* No-op for OCaml lists *) | _ -> error "list_reserve: expected list" )
  | 35 -> (
      (* list_get *)
      let idx = pop vm in
      let lst = pop vm in
      match (lst, idx) with
      | List lr, Int i -> (
        match CCList.nth_opt !lr i with Some v -> push vm v | None -> error "list_get: index out of bounds" )
      | _ ->
          error "list_get: expected list and int" )
  | 36 -> (
      (* list_set *)
      let elem = pop vm in
      let idx = pop vm in
      let lst = pop vm in
      match (lst, idx) with
      | List lr, Int i ->
          lr := CCList.mapi (fun j v -> if j = i then elem else v) !lr ;
          push vm Void
      | _ ->
          error "list_set: expected list, int, and value" )
  | _ ->
      ignore nargs ;
      error (Printf.sprintf "Unknown builtin id: %d" id)

(* Execute a generic binary operation *)
let execBinOp (vm : vm_state) (tag : int) : unit =
  let v2 = pop vm in
  let v1 = pop vm in
  match tag with
  | 0 -> (
    (* Le *)
    match (v1, v2) with
    | Int a, Int b ->
        push vm (Bool (a <= b))
    | Int16 a, Int16 b ->
        push vm (Bool (a <= b))
    | Real a, Real b ->
        push vm (Bool (a <= b))
    | Int a, Real b ->
        push vm (Bool (float_of_int a <= b))
    | Real a, Int b ->
        push vm (Bool (a <= float_of_int b))
    | _ ->
        error "<=: type mismatch" )
  | 1 -> (
    (* Ge *)
    match (v1, v2) with
    | Int a, Int b ->
        push vm (Bool (a >= b))
    | Int16 a, Int16 b ->
        push vm (Bool (a >= b))
    | Real a, Real b ->
        push vm (Bool (a >= b))
    | Int a, Real b ->
        push vm (Bool (float_of_int a >= b))
    | Real a, Int b ->
        push vm (Bool (a >= float_of_int b))
    | _ ->
        error ">=: type mismatch" )
  | 2 -> (
    (* Ne *)
    match (v1, v2) with
    | Int a, Int b ->
        push vm (Bool (a <> b))
    | Int16 a, Int16 b ->
        push vm (Bool (a <> b))
    | Real a, Real b ->
        push vm (Bool (a <> b))
    | Bool a, Bool b ->
        push vm (Bool (a <> b))
    | String a, String b ->
        push vm (Bool (not (String.equal a b)))
    | _ ->
        error "<>: type mismatch" )
  | 3 -> (
    (* Land *)
    match (v1, v2) with
    | Bool a, Bool b ->
        push vm (Bool (a && b))
    | _ ->
        error "&&: expected bools" )
  | 4 -> (
    (* Lor *)
    match (v1, v2) with
    | Bool a, Bool b ->
        push vm (Bool (a || b))
    | _ ->
        error "||: expected bools" )
  | 5 -> (
    (* Band *)
    match (v1, v2) with
    | Int a, Int b ->
        push vm (Int (a land b))
    | Int16 a, Int16 b ->
        push vm (Int16 (a land b))
    | _ ->
        error "&: expected ints" )
  | 6 -> (
    (* Bor *)
    match (v1, v2) with
    | Int a, Int b ->
        push vm (Int (a lor b))
    | Int16 a, Int16 b ->
        push vm (Int16 (a lor b))
    | _ ->
        error "|: expected ints" )
  | 7 -> (
    (* Bxor *)
    match (v1, v2) with
    | Int a, Int b ->
        push vm (Int (a lxor b))
    | Int16 a, Int16 b ->
        push vm (Int16 (a lxor b))
    | _ ->
        error "^: expected ints" )
  | 8 -> (
    (* Lsh *)
    match (v1, v2) with
    | Int a, Int b ->
        push vm (Int (a lsl b))
    | Int16 a, Int16 b ->
        push vm (Int16 (max (-32768) (min 32767 (a lsl b))))
    | _ ->
        error "<<: expected ints" )
  | 9 -> (
    (* Rsh *)
    match (v1, v2) with
    | Int a, Int b ->
        push vm (Int (a asr b))
    | Int16 a, Int16 b ->
        push vm (Int16 (a asr b))
    | _ ->
        error ">>: expected ints" )
  | 10 -> (
    (* Mod *)
    match (v1, v2) with
    | Int a, Int b when b <> 0 ->
        push vm (Int (a mod b))
    | Int16 a, Int16 b when b <> 0 ->
        push vm (Int16 (a mod b))
    | Real a, Real b when b <> 0.0 ->
        push vm (Real (mod_float a b))
    | _ ->
        error "%: type mismatch or division by zero" )
  | _ ->
      error (Printf.sprintf "Unknown binop tag: %d" tag)

(* Main dispatch loop - operates on encoded int array for jump-table dispatch.
   Uses Array.unsafe_get/set on hot-path arrays since all indices are VM-controlled. *)
let run (vm : vm_state) : value =
  let code = vm.code in
  let len = Array.length code in
  let stack = vm.stack in
  let locals = vm.locals in
  let constants = vm.constants in
  let functions = vm.functions in
  let cs_return_pcs = vm.cs_return_pcs in
  let cs_saved_fps = vm.cs_saved_fps in
  let cs_saved_sps = vm.cs_saved_sps in
  let cs_saved_locals_sps = vm.cs_saved_locals_sps in
  let[@inline] ipush (v : value) : unit =
    let sp = vm.sp in
    Array.unsafe_set stack sp v ;
    vm.sp <- sp + 1
  in
  let[@inline] ipop () : value =
    let sp = vm.sp - 1 in
    vm.sp <- sp ;
    Array.unsafe_get stack sp
  in
  let rec loop () : value =
    if vm.pc >= len then if vm.sp > 0 then ipop () else Void
    else
      let opcode = Array.unsafe_get code vm.pc in
      match opcode with
      | 0 ->
          (* LoadLocal *)
          let idx = Array.unsafe_get code (vm.pc + 1) in
          vm.pc <- vm.pc + 2 ;
          ipush (Array.unsafe_get locals (vm.fp + idx)) ;
          loop ()
      | 1 ->
          (* StoreLocal *)
          let idx = Array.unsafe_get code (vm.pc + 1) in
          vm.pc <- vm.pc + 2 ;
          Array.unsafe_set locals (vm.fp + idx) (ipop ()) ;
          loop ()
      | 2 ->
          (* Loadc *)
          let idx = Array.unsafe_get code (vm.pc + 1) in
          vm.pc <- vm.pc + 2 ;
          ipush (Array.unsafe_get constants idx) ;
          loop ()
      | 3 ->
          (* Pop *)
          vm.pc <- vm.pc + 1 ;
          vm.sp <- vm.sp - 1 ;
          loop ()
      | 4 ->
          (* Dup *)
          vm.pc <- vm.pc + 1 ;
          ipush (Array.unsafe_get stack (vm.sp - 1)) ;
          loop ()
      | 5 ->
          (* AddInt *)
          vm.pc <- vm.pc + 1 ;
          let b = ipop () in
          let a = ipop () in
          (match (a, b) with Int x, Int y -> ipush (Int (x + y)) | _ -> error "AddInt: type mismatch") ;
          loop ()
      | 6 ->
          (* SubInt *)
          vm.pc <- vm.pc + 1 ;
          let b = ipop () in
          let a = ipop () in
          (match (a, b) with Int x, Int y -> ipush (Int (x - y)) | _ -> error "SubInt: type mismatch") ;
          loop ()
      | 7 ->
          (* MulInt *)
          vm.pc <- vm.pc + 1 ;
          let b = ipop () in
          let a = ipop () in
          (match (a, b) with Int x, Int y -> ipush (Int (x * y)) | _ -> error "MulInt: type mismatch") ;
          loop ()
      | 8 ->
          (* DivInt *)
          vm.pc <- vm.pc + 1 ;
          let b = ipop () in
          let a = ipop () in
          ( match (a, b) with
          | Int x, Int y when y <> 0 ->
              ipush (Int (x / y))
          | _ ->
              error "DivInt: division by zero or type mismatch" ) ;
          loop ()
      | 9 ->
          (* AddInt16 *)
          vm.pc <- vm.pc + 1 ;
          let b = ipop () in
          let a = ipop () in
          ( match (a, b) with
          | Int16 x, Int16 y ->
              ipush (Int16 (max (-32768) (min 32767 (x + y))))
          | _ ->
              error "AddInt16: type mismatch" ) ;
          loop ()
      | 10 ->
          (* SubInt16 *)
          vm.pc <- vm.pc + 1 ;
          let b = ipop () in
          let a = ipop () in
          ( match (a, b) with
          | Int16 x, Int16 y ->
              ipush (Int16 (max (-32768) (min 32767 (x - y))))
          | _ ->
              error "SubInt16: type mismatch" ) ;
          loop ()
      | 11 ->
          (* MulInt16 *)
          vm.pc <- vm.pc + 1 ;
          let b = ipop () in
          let a = ipop () in
          ( match (a, b) with
          | Int16 x, Int16 y ->
              ipush (Int16 (max (-32768) (min 32767 (x * y))))
          | _ ->
              error "MulInt16: type mismatch" ) ;
          loop ()
      | 12 ->
          (* DivInt16 *)
          vm.pc <- vm.pc + 1 ;
          let b = ipop () in
          let a = ipop () in
          ( match (a, b) with
          | Int16 x, Int16 y when y <> 0 ->
              ipush (Int16 (max (-32768) (min 32767 (x / y))))
          | _ ->
              error "DivInt16: division by zero or type mismatch" ) ;
          loop ()
      | 13 ->
          (* AddReal *)
          vm.pc <- vm.pc + 1 ;
          let b = ipop () in
          let a = ipop () in
          ( match (a, b) with
          | Real x, Real y ->
              ipush (Real (x +. y))
          | Int x, Real y ->
              ipush (Real (float_of_int x +. y))
          | Real x, Int y ->
              ipush (Real (x +. float_of_int y))
          | _ ->
              error "AddReal: type mismatch" ) ;
          loop ()
      | 14 ->
          (* SubReal *)
          vm.pc <- vm.pc + 1 ;
          let b = ipop () in
          let a = ipop () in
          ( match (a, b) with
          | Real x, Real y ->
              ipush (Real (x -. y))
          | Int x, Real y ->
              ipush (Real (float_of_int x -. y))
          | Real x, Int y ->
              ipush (Real (x -. float_of_int y))
          | _ ->
              error "SubReal: type mismatch" ) ;
          loop ()
      | 15 ->
          (* MulReal *)
          vm.pc <- vm.pc + 1 ;
          let b = ipop () in
          let a = ipop () in
          ( match (a, b) with
          | Real x, Real y ->
              ipush (Real (x *. y))
          | Int x, Real y ->
              ipush (Real (float_of_int x *. y))
          | Real x, Int y ->
              ipush (Real (x *. float_of_int y))
          | _ ->
              error "MulReal: type mismatch" ) ;
          loop ()
      | 16 ->
          (* DivReal *)
          vm.pc <- vm.pc + 1 ;
          let b = ipop () in
          let a = ipop () in
          ( match (a, b) with
          | Real x, Real y when y <> 0.0 ->
              ipush (Real (x /. y))
          | Int x, Real y when y <> 0.0 ->
              ipush (Real (float_of_int x /. y))
          | Real x, Int y when y <> 0 ->
              ipush (Real (x /. float_of_int y))
          | _ ->
              error "DivReal: division by zero or type mismatch" ) ;
          loop ()
      | 17 ->
          (* ModInt *)
          vm.pc <- vm.pc + 1 ;
          let b = ipop () in
          let a = ipop () in
          (match (a, b) with Int x, Int y when y <> 0 -> ipush (Int (x mod y)) | _ -> error "ModInt: type mismatch") ;
          loop ()
      | 18 ->
          (* ModInt16 *)
          vm.pc <- vm.pc + 1 ;
          let b = ipop () in
          let a = ipop () in
          ( match (a, b) with
          | Int16 x, Int16 y when y <> 0 ->
              ipush (Int16 (x mod y))
          | _ ->
              error "ModInt16: type mismatch" ) ;
          loop ()
      | 19 ->
          (* ModReal *)
          vm.pc <- vm.pc + 1 ;
          let b = ipop () in
          let a = ipop () in
          ( match (a, b) with
          | Real x, Real y when y <> 0.0 ->
              ipush (Real (mod_float x y))
          | _ ->
              error "ModReal: type mismatch" ) ;
          loop ()
      | 20 ->
          (* NegInt *)
          vm.pc <- vm.pc + 1 ;
          let v = ipop () in
          (match v with Int x -> ipush (Int (-x)) | _ -> error "NegInt: type mismatch") ;
          loop ()
      | 21 ->
          (* NegReal *)
          vm.pc <- vm.pc + 1 ;
          let v = ipop () in
          (match v with Real x -> ipush (Real (-.x)) | _ -> error "NegReal: type mismatch") ;
          loop ()
      | 22 ->
          (* NegInt16 *)
          vm.pc <- vm.pc + 1 ;
          let v = ipop () in
          ( match v with
          | Int16 x ->
              ipush (Int16 (max (-32768) (min 32767 (-x))))
          | _ ->
              error "NegInt16: type mismatch" ) ;
          loop ()
      | 23 ->
          (* Not *)
          vm.pc <- vm.pc + 1 ;
          let v = ipop () in
          (match v with Bool b -> ipush (Bool (not b)) | _ -> error "Not: expected bool") ;
          loop ()
      | 24 ->
          (* EqInt *)
          vm.pc <- vm.pc + 1 ;
          let b = ipop () in
          let a = ipop () in
          ( match (a, b) with
          | Int x, Int y ->
              ipush (Bool (x = y))
          | Bool x, Bool y ->
              ipush (Bool (x = y))
          | _ ->
              error "EqInt: type mismatch" ) ;
          loop ()
      | 25 ->
          (* EqInt16 *)
          vm.pc <- vm.pc + 1 ;
          let b = ipop () in
          let a = ipop () in
          (match (a, b) with Int16 x, Int16 y -> ipush (Bool (x = y)) | _ -> error "EqInt16: type mismatch") ;
          loop ()
      | 26 ->
          (* EqReal *)
          vm.pc <- vm.pc + 1 ;
          let b = ipop () in
          let a = ipop () in
          (match (a, b) with Real x, Real y -> ipush (Bool (Float.equal x y)) | _ -> error "EqReal: type mismatch") ;
          loop ()
      | 27 ->
          (* LtInt *)
          vm.pc <- vm.pc + 1 ;
          let b = ipop () in
          let a = ipop () in
          (match (a, b) with Int x, Int y -> ipush (Bool (x < y)) | _ -> error "LtInt: type mismatch") ;
          loop ()
      | 28 ->
          (* LtInt16 *)
          vm.pc <- vm.pc + 1 ;
          let b = ipop () in
          let a = ipop () in
          (match (a, b) with Int16 x, Int16 y -> ipush (Bool (x < y)) | _ -> error "LtInt16: type mismatch") ;
          loop ()
      | 29 ->
          (* LtReal *)
          vm.pc <- vm.pc + 1 ;
          let b = ipop () in
          let a = ipop () in
          (match (a, b) with Real x, Real y -> ipush (Bool (x < y)) | _ -> error "LtReal: type mismatch") ;
          loop ()
      | 30 ->
          (* GtInt *)
          vm.pc <- vm.pc + 1 ;
          let b = ipop () in
          let a = ipop () in
          (match (a, b) with Int x, Int y -> ipush (Bool (x > y)) | _ -> error "GtInt: type mismatch") ;
          loop ()
      | 31 ->
          (* GtInt16 *)
          vm.pc <- vm.pc + 1 ;
          let b = ipop () in
          let a = ipop () in
          (match (a, b) with Int16 x, Int16 y -> ipush (Bool (x > y)) | _ -> error "GtInt16: type mismatch") ;
          loop ()
      | 32 ->
          (* GtReal *)
          vm.pc <- vm.pc + 1 ;
          let b = ipop () in
          let a = ipop () in
          (match (a, b) with Real x, Real y -> ipush (Bool (x > y)) | _ -> error "GtReal: type mismatch") ;
          loop ()
      | 33 ->
          (* BinOp *)
          let tag = Array.unsafe_get code (vm.pc + 1) in
          vm.pc <- vm.pc + 2 ;
          execBinOp vm tag ;
          loop ()
      | 34 ->
          (* Jump *)
          vm.pc <- Array.unsafe_get code (vm.pc + 1) ;
          loop ()
      | 35 ->
          (* JumpIfFalse *)
          let target = Array.unsafe_get code (vm.pc + 1) in
          vm.pc <- vm.pc + 2 ;
          let v = ipop () in
          (match v with Bool false -> vm.pc <- target | Bool true -> () | _ -> error "JumpIfFalse: expected bool") ;
          loop ()
      | 36 ->
          (* JumpIfTrue *)
          let target = Array.unsafe_get code (vm.pc + 1) in
          vm.pc <- vm.pc + 2 ;
          let v = ipop () in
          (match v with Bool true -> vm.pc <- target | Bool false -> () | _ -> error "JumpIfTrue: expected bool") ;
          loop ()
      | 37 ->
          (* Halt *)
          if vm.sp > 0 then ipop () else Void
      | 38 ->
          (* Call *)
          let func_idx = Array.unsafe_get code (vm.pc + 1) in
          let nargs = Array.unsafe_get code (vm.pc + 2) in
          vm.pc <- vm.pc + 3 ;
          let func = Array.unsafe_get functions func_idx in
          (* Save call frame using parallel int arrays — zero allocation *)
          let csp = vm.csp in
          Array.unsafe_set cs_return_pcs csp vm.pc ;
          Array.unsafe_set cs_saved_fps csp vm.fp ;
          Array.unsafe_set cs_saved_sps csp (vm.sp - nargs) ;
          Array.unsafe_set cs_saved_locals_sps csp vm.locals_sp ;
          vm.csp <- csp + 1 ;
          (* Set up new frame at the next free slot in locals *)
          let new_fp = vm.locals_sp in
          (* Copy args from stack to locals *)
          for i = 0 to nargs - 1 do
            Array.unsafe_set locals (new_fp + i) (Array.unsafe_get stack (vm.sp - nargs + i))
          done ;
          vm.sp <- vm.sp - nargs ;
          vm.fp <- new_fp ;
          vm.locals_sp <- new_fp + func.n_locals ;
          vm.pc <- func.entry_pc ;
          loop ()
      | 39 ->
          (* Return *)
          let result = ipop () in
          if vm.csp <= 0 then result
          else begin
            let csp = vm.csp - 1 in
            vm.csp <- csp ;
            vm.pc <- Array.unsafe_get cs_return_pcs csp ;
            vm.fp <- Array.unsafe_get cs_saved_fps csp ;
            vm.sp <- Array.unsafe_get cs_saved_sps csp ;
            vm.locals_sp <- Array.unsafe_get cs_saved_locals_sps csp ;
            ipush result ;
            loop ()
          end
      | 40 ->
          (* ReturnVoid *)
          if vm.csp <= 0 then Void
          else begin
            let csp = vm.csp - 1 in
            vm.csp <- csp ;
            vm.pc <- Array.unsafe_get cs_return_pcs csp ;
            vm.fp <- Array.unsafe_get cs_saved_fps csp ;
            vm.sp <- Array.unsafe_get cs_saved_sps csp ;
            vm.locals_sp <- Array.unsafe_get cs_saved_locals_sps csp ;
            ipush Void ;
            loop ()
          end
      | 41 ->
          (* CallBuiltin *)
          let id = Array.unsafe_get code (vm.pc + 1) in
          let nargs = Array.unsafe_get code (vm.pc + 2) in
          vm.pc <- vm.pc + 3 ;
          execBuiltin vm id nargs ;
          loop ()
      | 42 ->
          (* MakeArray *)
          let n = Array.unsafe_get code (vm.pc + 1) in
          vm.pc <- vm.pc + 2 ;
          let arr = Array.make n Void in
          for i = n - 1 downto 0 do
            arr.(i) <- ipop ()
          done ;
          ipush (Array arr) ;
          loop ()
      | 43 ->
          (* MakeStruct *)
          let n = Array.unsafe_get code (vm.pc + 1) in
          vm.pc <- vm.pc + 2 ;
          let arr = Array.make n Void in
          for i = n - 1 downto 0 do
            arr.(i) <- ipop ()
          done ;
          ipush (Struct arr) ;
          loop ()
      | 44 ->
          (* MakeTuple *)
          let n = Array.unsafe_get code (vm.pc + 1) in
          vm.pc <- vm.pc + 2 ;
          let arr = Array.make n Void in
          for i = n - 1 downto 0 do
            arr.(i) <- ipop ()
          done ;
          ipush (Array arr) ;
          loop ()
      | 45 ->
          (* IndexLoad *)
          vm.pc <- vm.pc + 1 ;
          let idx = ipop () in
          let arr = ipop () in
          ( match (arr, idx) with
          | Array a, Int i when i >= 0 && i < Array.length a ->
              ipush a.(i)
          | List lr, Int i -> (
            match CCList.nth_opt !lr i with Some v -> ipush v | None -> error "IndexLoad: list index out of bounds" )
          | _ ->
              error "IndexLoad: invalid array access" ) ;
          loop ()
      | 46 ->
          (* IndexStore *)
          vm.pc <- vm.pc + 1 ;
          let arr = ipop () in
          let idx = ipop () in
          let v = ipop () in
          ( match (arr, idx) with
          | Array a, Int i when i >= 0 && i < Array.length a ->
              a.(i) <- v
          | List lr, Int i ->
              lr := CCList.mapi (fun j old -> if j = i then v else old) !lr
          | _ ->
              error "IndexStore: invalid array access" ) ;
          loop ()
      | 47 ->
          (* MemberLoad *)
          let idx = Array.unsafe_get code (vm.pc + 1) in
          vm.pc <- vm.pc + 2 ;
          let s = ipop () in
          ( match s with
          | Struct members when idx >= 0 && idx < Array.length members ->
              ipush members.(idx)
          | Array members when idx >= 0 && idx < Array.length members ->
              ipush members.(idx)
          | _ ->
              error (Printf.sprintf "MemberLoad: invalid struct access (idx=%d)" idx) ) ;
          loop ()
      | 48 ->
          (* MemberStore *)
          let idx = Array.unsafe_get code (vm.pc + 1) in
          vm.pc <- vm.pc + 2 ;
          let s = ipop () in
          let v = ipop () in
          ( match s with
          | Struct members when idx >= 0 && idx < Array.length members ->
              members.(idx) <- v
          | Array members when idx >= 0 && idx < Array.length members ->
              members.(idx) <- v
          | _ ->
              error "MemberStore: invalid struct access" ) ;
          loop ()
      | 49 ->
          (* UnpackTuple *)
          let n = Array.unsafe_get code (vm.pc + 1) in
          let tuple = ipop () in
          ( match tuple with
          | Array arr | Struct arr ->
              for i = 0 to n - 1 do
                let offset = Array.unsafe_get code (vm.pc + 2 + i) in
                if i < Array.length arr then Array.unsafe_set locals (vm.fp + offset) arr.(i)
              done
          | _ ->
              error "UnpackTuple: expected tuple" ) ;
          vm.pc <- vm.pc + 2 + n ;
          loop ()
      | 50 ->
          (* MakeRecord *)
          let _struct_idx = Array.unsafe_get code (vm.pc + 1) in
          let n = Array.unsafe_get code (vm.pc + 2) in
          vm.pc <- vm.pc + 3 ;
          let arr = Array.make n Void in
          for i = n - 1 downto 0 do
            arr.(i) <- ipop ()
          done ;
          ipush (Struct arr) ;
          loop ()
      | 51 ->
          (* CallExternal *)
          let _name_hash = Array.unsafe_get code (vm.pc + 1) in
          let nargs = Array.unsafe_get code (vm.pc + 2) in
          vm.pc <- vm.pc + 3 ;
          for _ = 1 to nargs do
            ignore (ipop ())
          done ;
          ipush Void ;
          loop ()
      | 52 ->
          (* LoadLocalMember: fused LoadLocal + MemberLoad *)
          let local_idx = Array.unsafe_get code (vm.pc + 1) in
          let member_idx = Array.unsafe_get code (vm.pc + 2) in
          vm.pc <- vm.pc + 3 ;
          let s = Array.unsafe_get locals (vm.fp + local_idx) in
          ( match s with
          | Struct fields ->
              ipush (Array.unsafe_get fields member_idx)
          | Array fields ->
              ipush (Array.unsafe_get fields member_idx)
          | _ ->
              error (Printf.sprintf "LoadLocalMember: not a struct (local=%d, member=%d)" local_idx member_idx) ) ;
          loop ()
      | 53 ->
          (* StoreLocalMember: fused LoadLocal + MemberStore *)
          let local_idx = Array.unsafe_get code (vm.pc + 1) in
          let member_idx = Array.unsafe_get code (vm.pc + 2) in
          vm.pc <- vm.pc + 3 ;
          let v = ipop () in
          let s = Array.unsafe_get locals (vm.fp + local_idx) in
          ( match s with
          | Struct fields ->
              Array.unsafe_set fields member_idx v
          | Array fields ->
              Array.unsafe_set fields member_idx v
          | _ ->
              error (Printf.sprintf "StoreLocalMember: not a struct (local=%d, member=%d)" local_idx member_idx) ) ;
          loop ()
      | 54 ->
          (* DupStoreLocal: dup TOS, store copy to local *)
          let idx = Array.unsafe_get code (vm.pc + 1) in
          vm.pc <- vm.pc + 2 ;
          let v = Array.unsafe_get stack (vm.sp - 1) in
          Array.unsafe_set locals (vm.fp + idx) v ;
          loop ()
      | 55 ->
          (* DupStoreLocalMember: dup TOS, store copy to struct member *)
          let local_idx = Array.unsafe_get code (vm.pc + 1) in
          let member_idx = Array.unsafe_get code (vm.pc + 2) in
          vm.pc <- vm.pc + 3 ;
          let v = Array.unsafe_get stack (vm.sp - 1) in
          let s = Array.unsafe_get locals (vm.fp + local_idx) in
          ( match s with
          | Struct fields ->
              Array.unsafe_set fields member_idx v
          | Array fields ->
              Array.unsafe_set fields member_idx v
          | _ ->
              error "DupStoreLocalMember: not a struct" ) ;
          loop ()
      | 56 ->
          (* Call0 — no arguments *)
          let func_idx = Array.unsafe_get code (vm.pc + 1) in
          vm.pc <- vm.pc + 2 ;
          let func = Array.unsafe_get functions func_idx in
          let csp = vm.csp in
          Array.unsafe_set cs_return_pcs csp vm.pc ;
          Array.unsafe_set cs_saved_fps csp vm.fp ;
          Array.unsafe_set cs_saved_sps csp vm.sp ;
          Array.unsafe_set cs_saved_locals_sps csp vm.locals_sp ;
          vm.csp <- csp + 1 ;
          let new_fp = vm.locals_sp in
          vm.fp <- new_fp ;
          vm.locals_sp <- new_fp + func.n_locals ;
          vm.pc <- func.entry_pc ;
          loop ()
      | 57 ->
          (* Call1 — one argument *)
          let func_idx = Array.unsafe_get code (vm.pc + 1) in
          vm.pc <- vm.pc + 2 ;
          let func = Array.unsafe_get functions func_idx in
          let csp = vm.csp in
          Array.unsafe_set cs_return_pcs csp vm.pc ;
          Array.unsafe_set cs_saved_fps csp vm.fp ;
          Array.unsafe_set cs_saved_sps csp (vm.sp - 1) ;
          Array.unsafe_set cs_saved_locals_sps csp vm.locals_sp ;
          vm.csp <- csp + 1 ;
          let new_fp = vm.locals_sp in
          Array.unsafe_set locals new_fp (Array.unsafe_get stack (vm.sp - 1)) ;
          vm.sp <- vm.sp - 1 ;
          vm.fp <- new_fp ;
          vm.locals_sp <- new_fp + func.n_locals ;
          vm.pc <- func.entry_pc ;
          loop ()
      | 58 ->
          (* Call2 — two arguments *)
          let func_idx = Array.unsafe_get code (vm.pc + 1) in
          vm.pc <- vm.pc + 2 ;
          let func = Array.unsafe_get functions func_idx in
          let csp = vm.csp in
          Array.unsafe_set cs_return_pcs csp vm.pc ;
          Array.unsafe_set cs_saved_fps csp vm.fp ;
          Array.unsafe_set cs_saved_sps csp (vm.sp - 2) ;
          Array.unsafe_set cs_saved_locals_sps csp vm.locals_sp ;
          vm.csp <- csp + 1 ;
          let new_fp = vm.locals_sp in
          Array.unsafe_set locals new_fp (Array.unsafe_get stack (vm.sp - 2)) ;
          Array.unsafe_set locals (new_fp + 1) (Array.unsafe_get stack (vm.sp - 1)) ;
          vm.sp <- vm.sp - 2 ;
          vm.fp <- new_fp ;
          vm.locals_sp <- new_fp + func.n_locals ;
          vm.pc <- func.entry_pc ;
          loop ()
      | 59 ->
          (* Call3 — three arguments *)
          let func_idx = Array.unsafe_get code (vm.pc + 1) in
          vm.pc <- vm.pc + 2 ;
          let func = Array.unsafe_get functions func_idx in
          let csp = vm.csp in
          Array.unsafe_set cs_return_pcs csp vm.pc ;
          Array.unsafe_set cs_saved_fps csp vm.fp ;
          Array.unsafe_set cs_saved_sps csp (vm.sp - 3) ;
          Array.unsafe_set cs_saved_locals_sps csp vm.locals_sp ;
          vm.csp <- csp + 1 ;
          let new_fp = vm.locals_sp in
          Array.unsafe_set locals new_fp (Array.unsafe_get stack (vm.sp - 3)) ;
          Array.unsafe_set locals (new_fp + 1) (Array.unsafe_get stack (vm.sp - 2)) ;
          Array.unsafe_set locals (new_fp + 2) (Array.unsafe_get stack (vm.sp - 1)) ;
          vm.sp <- vm.sp - 3 ;
          vm.fp <- new_fp ;
          vm.locals_sp <- new_fp + func.n_locals ;
          vm.pc <- func.entry_pc ;
          loop ()
      | 60 ->
          (* LoadLocal0 *)
          vm.pc <- vm.pc + 1 ;
          ipush (Array.unsafe_get locals vm.fp) ;
          loop ()
      | 61 ->
          (* LoadLocal1 *)
          vm.pc <- vm.pc + 1 ;
          ipush (Array.unsafe_get locals (vm.fp + 1)) ;
          loop ()
      | 62 ->
          (* LoadLocal2 *)
          vm.pc <- vm.pc + 1 ;
          ipush (Array.unsafe_get locals (vm.fp + 2)) ;
          loop ()
      | 63 ->
          (* LoadLocal3 *)
          vm.pc <- vm.pc + 1 ;
          ipush (Array.unsafe_get locals (vm.fp + 3)) ;
          loop ()
      | 64 ->
          (* StoreLocal0 *)
          vm.pc <- vm.pc + 1 ;
          Array.unsafe_set locals vm.fp (ipop ()) ;
          loop ()
      | 65 ->
          (* StoreLocal1 *)
          vm.pc <- vm.pc + 1 ;
          Array.unsafe_set locals (vm.fp + 1) (ipop ()) ;
          loop ()
      | 66 ->
          (* StoreLocal2 *)
          vm.pc <- vm.pc + 1 ;
          Array.unsafe_set locals (vm.fp + 2) (ipop ()) ;
          loop ()
      | 67 ->
          (* StoreLocal3 *)
          vm.pc <- vm.pc + 1 ;
          Array.unsafe_set locals (vm.fp + 3) (ipop ()) ;
          loop ()
      | 68 ->
          (* Loadc0 *)
          vm.pc <- vm.pc + 1 ;
          ipush (Array.unsafe_get constants 0) ;
          loop ()
      | 69 ->
          (* Loadc1 *)
          vm.pc <- vm.pc + 1 ;
          ipush (Array.unsafe_get constants 1) ;
          loop ()
      | 70 ->
          (* Loadc2 *)
          vm.pc <- vm.pc + 1 ;
          ipush (Array.unsafe_get constants 2) ;
          loop ()
      | 71 ->
          (* Loadc3 *)
          vm.pc <- vm.pc + 1 ;
          ipush (Array.unsafe_get constants 3) ;
          loop ()
      | 72 ->
          (* DupStoreLocal0 *)
          vm.pc <- vm.pc + 1 ;
          Array.unsafe_set locals vm.fp (Array.unsafe_get stack (vm.sp - 1)) ;
          loop ()
      | 73 ->
          (* DupStoreLocal1 *)
          vm.pc <- vm.pc + 1 ;
          Array.unsafe_set locals (vm.fp + 1) (Array.unsafe_get stack (vm.sp - 1)) ;
          loop ()
      | 74 ->
          (* DupStoreLocal2 *)
          vm.pc <- vm.pc + 1 ;
          Array.unsafe_set locals (vm.fp + 2) (Array.unsafe_get stack (vm.sp - 1)) ;
          loop ()
      | 75 ->
          (* DupStoreLocal3 *)
          vm.pc <- vm.pc + 1 ;
          Array.unsafe_set locals (vm.fp + 3) (Array.unsafe_get stack (vm.sp - 1)) ;
          loop ()
      | 76 ->
          (* LtIntJumpIfFalse *)
          let target = Array.unsafe_get code (vm.pc + 1) in
          vm.pc <- vm.pc + 2 ;
          let b = ipop () in
          let a = ipop () in
          ( match (a, b) with
          | Int x, Int y ->
              if not (x < y) then vm.pc <- target
          | _ ->
              error "LtIntJumpIfFalse: type mismatch" ) ;
          loop ()
      | 77 ->
          (* GtIntJumpIfFalse *)
          let target = Array.unsafe_get code (vm.pc + 1) in
          vm.pc <- vm.pc + 2 ;
          let b = ipop () in
          let a = ipop () in
          ( match (a, b) with
          | Int x, Int y ->
              if not (x > y) then vm.pc <- target
          | _ ->
              error "GtIntJumpIfFalse: type mismatch" ) ;
          loop ()
      | 78 ->
          (* EqIntJumpIfFalse *)
          let target = Array.unsafe_get code (vm.pc + 1) in
          vm.pc <- vm.pc + 2 ;
          let b = ipop () in
          let a = ipop () in
          ( match (a, b) with
          | Int x, Int y ->
              if x <> y then vm.pc <- target
          | Bool x, Bool y ->
              if x <> y then vm.pc <- target
          | _ ->
              error "EqIntJumpIfFalse: type mismatch" ) ;
          loop ()
      | 79 ->
          (* LtRealJumpIfFalse *)
          let target = Array.unsafe_get code (vm.pc + 1) in
          vm.pc <- vm.pc + 2 ;
          let b = ipop () in
          let a = ipop () in
          ( match (a, b) with
          | Real x, Real y ->
              if not (x < y) then vm.pc <- target
          | _ ->
              error "LtRealJumpIfFalse: type mismatch" ) ;
          loop ()
      | 80 ->
          (* GtRealJumpIfFalse *)
          let target = Array.unsafe_get code (vm.pc + 1) in
          vm.pc <- vm.pc + 2 ;
          let b = ipop () in
          let a = ipop () in
          ( match (a, b) with
          | Real x, Real y ->
              if not (x > y) then vm.pc <- target
          | _ ->
              error "GtRealJumpIfFalse: type mismatch" ) ;
          loop ()
      | 81 ->
          (* EqRealJumpIfFalse *)
          let target = Array.unsafe_get code (vm.pc + 1) in
          vm.pc <- vm.pc + 2 ;
          let b = ipop () in
          let a = ipop () in
          ( match (a, b) with
          | Real x, Real y ->
              if not (Float.equal x y) then vm.pc <- target
          | _ ->
              error "EqRealJumpIfFalse: type mismatch" ) ;
          loop ()
      | _ ->
          error (Printf.sprintf "Unknown opcode: %d at pc=%d" opcode vm.pc)
  in
  loop ()

(* Run a specific function by index with arguments *)
let runFunction (vm : vm_state) (func_idx : int) (args : value list) : value =
  let func = vm.functions.(func_idx) in
  (* Set up frame *)
  vm.fp <- 0 ;
  vm.locals_sp <- func.n_locals ;
  CCList.iteri (fun i arg -> vm.locals.(i) <- arg) args ;
  for i = CCList.length args to func.n_locals - 1 do
    vm.locals.(i) <- Void
  done ;
  vm.pc <- func.entry_pc ;
  vm.sp <- 0 ;
  vm.csp <- 0 ;
  run vm

(* Evaluate a main expression using the bytecode VM *)
let evaluateMainExpression (args : Util.Args.args) (env : Core.Env.in_top) (bc_prog : bc_prog) (exp_str : string) :
    value =
  let e = Pparser.Parse.parseString (Some "Main_.vult") (Pla.print {%pla|fun _main_() return <#exp_str#s>;|}) in
  let env, main = Core.Typechecking.typecheck_single args env e in
  let _, main = Core.Toprog.convert args env main in
  let main = Core.Passes.run args main in
  let bc_prog = Compiler.extendProgram bc_prog main in
  let main_func_name = "Main___main_" in
  match Hashtbl.find_opt bc_prog.function_names main_func_name with
  | Some func_idx ->
      let vm = createVM ?sample_rate:args.fs bc_prog in
      (* Check for alloc function *)
      let call_args =
        let alloc_name = main_func_name ^ "_type_alloc" in
        match Hashtbl.find_opt bc_prog.function_names alloc_name with
        | Some alloc_idx ->
            let state = runFunction vm alloc_idx [] in
            [state]
        | None ->
            []
      in
      let vm = createVM ?sample_rate:args.fs bc_prog in
      runFunction vm func_idx call_args
  | None ->
      error "Could not execute the expression"

(* Render audio expression to WAV file using bytecode VM *)
let rec renderAudioExpression (args : Util.Args.args) (env : Core.Env.in_top) (bc_prog : bc_prog) (tag_string : string)
    : string * float =
  let start_time = Sys.time () in
  let params = Core.Interpreter.parseRenderParams tag_string in
  args.fs <- Some (float_of_int params.samplerate) ;
  let wrapper_code = Core.Interpreter.generateRenderWrapper params in
  let e = Pparser.Parse.parseString (Some "Render_.vult") wrapper_code in
  let env, main = Core.Typechecking.typecheck_single args env e in
  let _, main = Core.Toprog.convert args env main in
  let main = Core.Passes.run args main in
  let bc_prog = Compiler.extendProgram bc_prog main in
  let main_func_name = "Render___main" in
  match Hashtbl.find_opt bc_prog.function_names main_func_name with
  | Some func_idx ->
      let vm = createVM ~sample_rate:(float_of_int params.samplerate) bc_prog in
      let call_args =
        let alloc_name = main_func_name ^ "_type_alloc" in
        match Hashtbl.find_opt bc_prog.function_names alloc_name with
        | Some alloc_idx ->
            let state = runFunction vm alloc_idx [] in
            [state]
        | None ->
            []
      in
      let vm = createVM ~sample_rate:(float_of_int params.samplerate) bc_prog in
      let result = runFunction vm func_idx call_args in
      let () = Core.Interpreter.writeResultToWav (valueToInterpreterDvalue result) params in
      let end_time = Sys.time () in
      (params.file, end_time -. start_time)
  | None ->
      error "Could not execute render function"

(* Convert bytecode value to interpreter dvalue for WAV writing *)
and valueToInterpreterDvalue (v : value) : Core.Interpreter.dvalue =
  match v with
  | Void ->
      Core.Interpreter.DVoid
  | Int i ->
      Core.Interpreter.DInt i
  | Int16 i ->
      Core.Interpreter.DInt16 i
  | Real f ->
      Core.Interpreter.DReal f
  | Bool b ->
      Core.Interpreter.DBool b
  | String s ->
      Core.Interpreter.DString s
  | Array arr ->
      Core.Interpreter.DArray (Array.map valueToInterpreterDvalue arr)
  | List lr ->
      Core.Interpreter.DList (ref (CCList.map valueToInterpreterDvalue !lr))
  | Struct arr ->
      Core.Interpreter.DStruct (Array.map valueToInterpreterDvalue arr)
