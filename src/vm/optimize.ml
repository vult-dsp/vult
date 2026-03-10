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

(* Build a map from encoded PC to instruction index *)
let buildPcToIndex (instrs : instruction array) : (int, int) Hashtbl.t =
  let len = Array.length instrs in
  let tbl = Hashtbl.create len in
  let pc = ref 0 in
  for i = 0 to len - 1 do
    Hashtbl.replace tbl !pc i ;
    pc := !pc + instrSize instrs.(i)
  done ;
  tbl

(* Build a map from instruction index to encoded PC *)
let buildIndexToPc (instrs : instruction array) : int array =
  let len = Array.length instrs in
  let arr = Array.make len 0 in
  let pc = ref 0 in
  for i = 0 to len - 1 do
    arr.(i) <- !pc ;
    pc := !pc + instrSize instrs.(i)
  done ;
  arr

(* Build a set of encoded PCs that are jump targets or function entry points.
   We must not merge instruction pairs where the second instruction is a jump target,
   because another control flow path may enter at that instruction with a different stack state. *)
let buildJumpTargets (instrs : instruction array) (funcs : bc_func array) : (int, unit) Hashtbl.t =
  let targets = Hashtbl.create 64 in
  Array.iter
    (fun (instr : instruction) ->
      match instr with
      | Jump target | JumpIfFalse target | JumpIfTrue target ->
          Hashtbl.replace targets target ()
      | LtIntJumpIfFalse target | GtIntJumpIfFalse target | EqIntJumpIfFalse target ->
          Hashtbl.replace targets target ()
      | LtRealJumpIfFalse target | GtRealJumpIfFalse target | EqRealJumpIfFalse target ->
          Hashtbl.replace targets target ()
      | _ ->
          () )
    instrs ;
  Array.iter (fun (f : bc_func) -> Hashtbl.replace targets f.entry_pc ()) funcs ;
  targets

(* Apply peephole rules in a single left-to-right scan.
   Returns (new instruction array, old_index_to_new_index mapping).
   The mapping has length = old_len + 1. Entry [old_len] maps to new_len
   (the "past the end" sentinel for jump targets).
   Jump targets still contain old encoded PCs at this point. *)
let applyRules (instrs : instruction array) (funcs : bc_func array) : instruction array * int array =
  let len = Array.length instrs in
  let targets = buildJumpTargets instrs funcs in
  (* Build encoded PC for each instruction index *)
  let index_to_pc = buildIndexToPc instrs in
  let buf = Array.make len Halt in
  let buf_len = ref 0 in
  (* old_to_new.(i) = new index that old instruction i maps to *)
  let old_to_new = Array.make (len + 1) 0 in
  let add (instr : instruction) : unit =
    buf.(!buf_len) <- instr ;
    incr buf_len
  in
  let is_target (idx : int) : bool = Hashtbl.mem targets index_to_pc.(idx) in
  let i = ref 0 in
  while !i < len do
    if !i + 1 < len && not (is_target (!i + 1)) then (
      match (instrs.(!i), instrs.(!i + 1)) with
      | StoreLocal n1, LoadLocal n2 when n1 = n2 ->
          old_to_new.(!i) <- !buf_len ;
          old_to_new.(!i + 1) <- !buf_len ;
          add (DupStoreLocal n1) ;
          i := !i + 2
      | StoreLocal0, LoadLocal0 ->
          old_to_new.(!i) <- !buf_len ;
          old_to_new.(!i + 1) <- !buf_len ;
          add DupStoreLocal0 ;
          i := !i + 2
      | StoreLocal1, LoadLocal1 ->
          old_to_new.(!i) <- !buf_len ;
          old_to_new.(!i + 1) <- !buf_len ;
          add DupStoreLocal1 ;
          i := !i + 2
      | StoreLocal2, LoadLocal2 ->
          old_to_new.(!i) <- !buf_len ;
          old_to_new.(!i + 1) <- !buf_len ;
          add DupStoreLocal2 ;
          i := !i + 2
      | StoreLocal3, LoadLocal3 ->
          old_to_new.(!i) <- !buf_len ;
          old_to_new.(!i + 1) <- !buf_len ;
          add DupStoreLocal3 ;
          i := !i + 2
      | StoreLocalMember (l1, m1), LoadLocalMember (l2, m2) when l1 = l2 && m1 = m2 ->
          old_to_new.(!i) <- !buf_len ;
          old_to_new.(!i + 1) <- !buf_len ;
          add (DupStoreLocalMember (l1, m1)) ;
          i := !i + 2
      | LtInt, JumpIfFalse target ->
          old_to_new.(!i) <- !buf_len ;
          old_to_new.(!i + 1) <- !buf_len ;
          add (LtIntJumpIfFalse target) ;
          i := !i + 2
      | GtInt, JumpIfFalse target ->
          old_to_new.(!i) <- !buf_len ;
          old_to_new.(!i + 1) <- !buf_len ;
          add (GtIntJumpIfFalse target) ;
          i := !i + 2
      | EqInt, JumpIfFalse target ->
          old_to_new.(!i) <- !buf_len ;
          old_to_new.(!i + 1) <- !buf_len ;
          add (EqIntJumpIfFalse target) ;
          i := !i + 2
      | LtReal, JumpIfFalse target ->
          old_to_new.(!i) <- !buf_len ;
          old_to_new.(!i + 1) <- !buf_len ;
          add (LtRealJumpIfFalse target) ;
          i := !i + 2
      | GtReal, JumpIfFalse target ->
          old_to_new.(!i) <- !buf_len ;
          old_to_new.(!i + 1) <- !buf_len ;
          add (GtRealJumpIfFalse target) ;
          i := !i + 2
      | EqReal, JumpIfFalse target ->
          old_to_new.(!i) <- !buf_len ;
          old_to_new.(!i + 1) <- !buf_len ;
          add (EqRealJumpIfFalse target) ;
          i := !i + 2
      | NegReal, AddReal ->
          old_to_new.(!i) <- !buf_len ;
          old_to_new.(!i + 1) <- !buf_len ;
          add SubReal ;
          i := !i + 2
      | NegInt, AddInt ->
          old_to_new.(!i) <- !buf_len ;
          old_to_new.(!i + 1) <- !buf_len ;
          add SubInt ;
          i := !i + 2
      | NegInt16, AddInt16 ->
          old_to_new.(!i) <- !buf_len ;
          old_to_new.(!i + 1) <- !buf_len ;
          add SubInt16 ;
          i := !i + 2
      | _ ->
          old_to_new.(!i) <- !buf_len ;
          add instrs.(!i) ;
          i := !i + 1 )
    else begin
      old_to_new.(!i) <- !buf_len ;
      add instrs.(!i) ;
      i := !i + 1
    end
  done ;
  (* Sentinel: old index [len] maps to new_len (past-the-end) *)
  old_to_new.(len) <- !buf_len ;
  (Array.sub buf 0 !buf_len, old_to_new)

(* Fix all jump targets and function entry_pc values.
   old_pc_to_index: maps old encoded PC → old instruction index
   old_to_new: maps old instruction index → new instruction index
   new_index_to_pc: maps new instruction index → new encoded PC *)
let fixupCode (old_instrs : instruction array) (new_instrs : instruction array) (old_to_new : int array) :
    instruction array =
  let old_pc_to_index = buildPcToIndex old_instrs in
  let new_index_to_pc = buildIndexToPc new_instrs in
  let new_len = Array.length new_instrs in
  (* Compute new end PC for the sentinel *)
  let new_end_pc = if new_len > 0 then new_index_to_pc.(new_len - 1) + instrSize new_instrs.(new_len - 1) else 0 in
  let translate (old_pc : int) : int =
    match Hashtbl.find_opt old_pc_to_index old_pc with
    | Some old_idx ->
        let new_idx = old_to_new.(old_idx) in
        if new_idx < new_len then new_index_to_pc.(new_idx) else new_end_pc
    | None ->
        (* Check if it's the past-the-end PC *)
        let old_len = Array.length old_instrs in
        let old_end_pc_val =
          if old_len > 0 then
            let old_idx_to_pc = buildIndexToPc old_instrs in
            old_idx_to_pc.(old_len - 1) + instrSize old_instrs.(old_len - 1)
          else 0
        in
        if old_pc = old_end_pc_val then
          let new_idx = old_to_new.(old_len) in
          if new_idx < new_len then new_index_to_pc.(new_idx) else new_end_pc
        else old_pc
  in
  Array.map
    (fun (instr : instruction) ->
      match instr with
      | Jump target ->
          Jump (translate target)
      | JumpIfFalse target ->
          JumpIfFalse (translate target)
      | JumpIfTrue target ->
          JumpIfTrue (translate target)
      | LtIntJumpIfFalse target ->
          LtIntJumpIfFalse (translate target)
      | GtIntJumpIfFalse target ->
          GtIntJumpIfFalse (translate target)
      | EqIntJumpIfFalse target ->
          EqIntJumpIfFalse (translate target)
      | LtRealJumpIfFalse target ->
          LtRealJumpIfFalse (translate target)
      | GtRealJumpIfFalse target ->
          GtRealJumpIfFalse (translate target)
      | EqRealJumpIfFalse target ->
          EqRealJumpIfFalse (translate target)
      | other ->
          other )
    new_instrs

(* Fix function entry_pc values *)
let fixupFunctions (old_instrs : instruction array) (funcs : bc_func array) (new_instrs : instruction array)
    (old_to_new : int array) : bc_func array =
  let old_pc_to_index = buildPcToIndex old_instrs in
  let new_index_to_pc = buildIndexToPc new_instrs in
  let new_len = Array.length new_instrs in
  let new_end_pc = if new_len > 0 then new_index_to_pc.(new_len - 1) + instrSize new_instrs.(new_len - 1) else 0 in
  Array.map
    (fun (f : bc_func) ->
      match Hashtbl.find_opt old_pc_to_index f.entry_pc with
      | Some old_idx ->
          let new_idx = old_to_new.(old_idx) in
          let new_pc = if new_idx < new_len then new_index_to_pc.(new_idx) else new_end_pc in
          {f with entry_pc= new_pc}
      | None ->
          f )
    funcs

(* Main entry point: optimize an instruction array and fix up function entry points *)
let optimize (instrs : instruction array) (funcs : bc_func array) : instruction array * bc_func array =
  let old_instrs = instrs in
  let new_instrs, old_to_new = applyRules instrs funcs in
  if Array.length new_instrs = Array.length old_instrs then (instrs, funcs)
  else
    let new_instrs = fixupCode old_instrs new_instrs old_to_new in
    let new_funcs = fixupFunctions old_instrs funcs new_instrs old_to_new in
    (new_instrs, new_funcs)
