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

(* Maximum body size (excluding final Return) for a function to be inlinable *)
let max_inline_body_size : int = 32

(* Extract each function's instruction body as a range [start, end) in the instruction array.
   Functions are sorted by entry_pc to determine boundaries. *)
let extractFunctionBodies (code : instruction array) (funcs : bc_func array) : (int * int) array =
  let n_funcs = Array.length funcs in
  if n_funcs = 0 then [||]
  else
    let code_len = Array.length code in
    let pc_to_index = Optimize.buildPcToIndex code in
    let func_starts =
      Array.map
        (fun (f : bc_func) -> match Hashtbl.find_opt pc_to_index f.entry_pc with Some idx -> idx | None -> code_len)
        funcs
    in
    let sorted_indices = Array.init n_funcs (fun i -> i) in
    Array.sort (fun (a : int) (b : int) -> compare func_starts.(a) func_starts.(b)) sorted_indices ;
    let bodies = Array.make n_funcs (0, 0) in
    for si = 0 to n_funcs - 1 do
      let fi = sorted_indices.(si) in
      let start_idx = func_starts.(fi) in
      let end_idx = if si + 1 < n_funcs then func_starts.(sorted_indices.(si + 1)) else code_len in
      bodies.(fi) <- (start_idx, end_idx)
    done ;
    bodies

(* Check if a function body is eligible for inlining.
   Relaxed criteria: allows branches, non-leaf calls, and multiple Returns.
   Disallows: ReturnVoid anywhere (can't push Void without constant pool access). *)
let isInlinable (code : instruction array) (start_idx : int) (end_idx : int) : bool =
  if start_idx >= end_idx then false
  else
    let body_len = end_idx - start_idx in
    (* Must end with Return (value-returning function only) *)
    let last_is_return = match code.(end_idx - 1) with Return -> true | _ -> false in
    if not last_is_return then false
    else
      let body_without_return = body_len - 1 in
      if body_without_return > max_inline_body_size || body_without_return = 0 then false
      else
        (* No ReturnVoid anywhere in the body *)
        let valid = ref true in
        for i = start_idx to end_idx - 2 do
          match code.(i) with ReturnVoid -> valid := false | _ -> ()
        done ;
        !valid

(* Normalize a function body: convert jump targets from encoded PCs to body-relative
   instruction indices, and convert mid-body Returns to Jump body_len (end sentinel). *)
let normalizeBody (code : instruction array) (start_idx : int) (end_idx : int) (pc_to_index : (int, int) Hashtbl.t) :
    instruction array =
  let body_len = end_idx - start_idx - 1 in
  Array.init body_len (fun (i : int) ->
      let instr = code.(start_idx + i) in
      match instr with
      | Jump target ->
          let target_idx = Hashtbl.find pc_to_index target in
          Jump (target_idx - start_idx)
      | JumpIfFalse target ->
          let target_idx = Hashtbl.find pc_to_index target in
          JumpIfFalse (target_idx - start_idx)
      | JumpIfTrue target ->
          let target_idx = Hashtbl.find pc_to_index target in
          JumpIfTrue (target_idx - start_idx)
      | LtIntJumpIfFalse target ->
          let target_idx = Hashtbl.find pc_to_index target in
          LtIntJumpIfFalse (target_idx - start_idx)
      | GtIntJumpIfFalse target ->
          let target_idx = Hashtbl.find pc_to_index target in
          GtIntJumpIfFalse (target_idx - start_idx)
      | EqIntJumpIfFalse target ->
          let target_idx = Hashtbl.find pc_to_index target in
          EqIntJumpIfFalse (target_idx - start_idx)
      | LtRealJumpIfFalse target ->
          let target_idx = Hashtbl.find pc_to_index target in
          LtRealJumpIfFalse (target_idx - start_idx)
      | GtRealJumpIfFalse target ->
          let target_idx = Hashtbl.find pc_to_index target in
          GtRealJumpIfFalse (target_idx - start_idx)
      | EqRealJumpIfFalse target ->
          let target_idx = Hashtbl.find pc_to_index target in
          EqRealJumpIfFalse (target_idx - start_idx)
      | Return ->
          Jump body_len
      | _ ->
          instr )

(* Find all inlinable functions. Returns a hashtable mapping func_idx to
   the normalized body instructions (excluding final Return, jumps body-relative). *)
let findCandidates (code : instruction array) (funcs : bc_func array) : (int, instruction array) Hashtbl.t =
  let bodies = extractFunctionBodies code funcs in
  let pc_to_index = Optimize.buildPcToIndex code in
  let candidates = Hashtbl.create 16 in
  Array.iteri
    (fun (fi : int) ((start_idx, end_idx) : int * int) ->
      if isInlinable code start_idx end_idx then begin
        let body = normalizeBody code start_idx end_idx pc_to_index in
        Hashtbl.replace candidates fi body
      end )
    bodies ;
  candidates

(* Build a map from instruction index to the func_idx that contains it, or -1 for top-level *)
let buildCallerMap (code : instruction array) (funcs : bc_func array) : int array =
  let code_len = Array.length code in
  let caller_map = Array.make code_len (-1) in
  let bodies = extractFunctionBodies code funcs in
  Array.iteri
    (fun (fi : int) ((start_idx, end_idx) : int * int) ->
      for i = start_idx to end_idx - 1 do
        caller_map.(i) <- fi
      done )
    bodies ;
  caller_map

(* Remap local indices in an instruction by adding an offset.
   Does not modify jump targets (those use body-relative indices). *)
let remapLoadLocal (i : int) (offset : int) : instruction =
  let idx = i + offset in
  match idx with 0 -> LoadLocal0 | 1 -> LoadLocal1 | 2 -> LoadLocal2 | 3 -> LoadLocal3 | _ -> LoadLocal idx

let remapStoreLocal (i : int) (offset : int) : instruction =
  let idx = i + offset in
  match idx with 0 -> StoreLocal0 | 1 -> StoreLocal1 | 2 -> StoreLocal2 | 3 -> StoreLocal3 | _ -> StoreLocal idx

let remapDupStoreLocal (i : int) (offset : int) : instruction =
  let idx = i + offset in
  match idx with
  | 0 ->
      DupStoreLocal0
  | 1 ->
      DupStoreLocal1
  | 2 ->
      DupStoreLocal2
  | 3 ->
      DupStoreLocal3
  | _ ->
      DupStoreLocal idx

let remapLocals (instr : instruction) (offset : int) : instruction =
  match instr with
  | LoadLocal i ->
      remapLoadLocal i offset
  | StoreLocal i ->
      remapStoreLocal i offset
  | LoadLocal0 ->
      remapLoadLocal 0 offset
  | LoadLocal1 ->
      remapLoadLocal 1 offset
  | LoadLocal2 ->
      remapLoadLocal 2 offset
  | LoadLocal3 ->
      remapLoadLocal 3 offset
  | StoreLocal0 ->
      remapStoreLocal 0 offset
  | StoreLocal1 ->
      remapStoreLocal 1 offset
  | StoreLocal2 ->
      remapStoreLocal 2 offset
  | StoreLocal3 ->
      remapStoreLocal 3 offset
  | LoadLocalMember (i, m) ->
      LoadLocalMember (i + offset, m)
  | StoreLocalMember (i, m) ->
      StoreLocalMember (i + offset, m)
  | DupStoreLocal i ->
      remapDupStoreLocal i offset
  | DupStoreLocal0 ->
      remapDupStoreLocal 0 offset
  | DupStoreLocal1 ->
      remapDupStoreLocal 1 offset
  | DupStoreLocal2 ->
      remapDupStoreLocal 2 offset
  | DupStoreLocal3 ->
      remapDupStoreLocal 3 offset
  | DupStoreLocalMember (i, m) ->
      DupStoreLocalMember (i + offset, m)
  | UnpackTuple (n, offsets) ->
      UnpackTuple (n, CCList.map (fun (o : int) -> o + offset) offsets)
  | other ->
      other

(* Main inlining pass *)
let inline (code : instruction array) (funcs : bc_func array) : instruction array * bc_func array =
  let candidates = findCandidates code funcs in
  if Hashtbl.length candidates = 0 then (code, funcs)
  else
    let old_code = code in
    let old_len = Array.length old_code in
    let caller_map = buildCallerMap old_code funcs in
    let extra_locals = Array.make (Array.length funcs) 0 in
    (* Build new instruction array *)
    let buf = Array.make (old_len * 4) Halt in
    let buf_len = ref 0 in
    let old_to_new = Array.make (old_len + 1) 0 in
    (* Track inlined jumps: new_buf_idx → body_emit_start.
       These jumps have body-relative targets that need separate fixup. *)
    let inlined_jumps : (int, int) Hashtbl.t = Hashtbl.create 64 in
    let add (instr : instruction) : unit =
      if !buf_len >= Array.length buf then begin
        let new_size = !buf_len * 2 in
        let new_arr = Array.make new_size Halt in
        Array.blit buf 0 new_arr 0 !buf_len ; Array.blit new_arr 0 buf 0 new_size
      end ;
      buf.(!buf_len) <- instr ;
      incr buf_len
    in
    for i = 0 to old_len - 1 do
      match old_code.(i) with
      | (Call (_, _) | Call0 _ | Call1 _ | Call2 _ | Call3 _) as call_instr
        when let fi = match call_instr with Call (f, _) | Call0 f | Call1 f | Call2 f | Call3 f -> f | _ -> -1 in
             Hashtbl.mem candidates fi ->
          let func_idx, nargs =
            match call_instr with
            | Call (f, n) ->
                (f, n)
            | Call0 f ->
                (f, 0)
            | Call1 f ->
                (f, 1)
            | Call2 f ->
                (f, 2)
            | Call3 f ->
                (f, 3)
            | _ ->
                (-1, 0)
          in
          let body = Hashtbl.find candidates func_idx in
          let caller_idx = caller_map.(i) in
          old_to_new.(i) <- !buf_len ;
          if caller_idx >= 0 then begin
            let callee = funcs.(func_idx) in
            let base = funcs.(caller_idx).n_locals + extra_locals.(caller_idx) in
            (* Pop args from stack into new local slots (reverse order: last arg is TOS) *)
            for a = nargs - 1 downto 0 do
              add (StoreLocal (base + a))
            done ;
            (* Emit remapped body, recording jump positions *)
            let body_emit_start = !buf_len in
            Array.iter
              (fun (instr : instruction) ->
                let new_idx = !buf_len in
                let remapped = remapLocals instr base in
                ( match remapped with
                | Jump _
                | JumpIfFalse _
                | JumpIfTrue _
                | LtIntJumpIfFalse _
                | GtIntJumpIfFalse _
                | EqIntJumpIfFalse _
                | LtRealJumpIfFalse _
                | GtRealJumpIfFalse _
                | EqRealJumpIfFalse _ ->
                    Hashtbl.replace inlined_jumps new_idx body_emit_start
                | _ ->
                    () ) ;
                add remapped )
              body ;
            extra_locals.(caller_idx) <- extra_locals.(caller_idx) + callee.n_locals
          end
          else add call_instr
      | instr ->
          old_to_new.(i) <- !buf_len ;
          add instr
    done ;
    old_to_new.(old_len) <- !buf_len ;
    let new_code = Array.sub buf 0 !buf_len in
    (* Fixup all jump targets and function entry_pcs in a single pass *)
    let old_pc_to_index = Optimize.buildPcToIndex old_code in
    let new_index_to_pc = Optimize.buildIndexToPc new_code in
    let new_len = Array.length new_code in
    let new_end_pc = if new_len > 0 then new_index_to_pc.(new_len - 1) + instrSize new_code.(new_len - 1) else 0 in
    (* Translate an old encoded PC to a new encoded PC via old_to_new *)
    let old_end_pc =
      if old_len > 0 then
        let old_index_to_pc = Optimize.buildIndexToPc old_code in
        old_index_to_pc.(old_len - 1) + instrSize old_code.(old_len - 1)
      else 0
    in
    let translateOld (old_pc : int) : int =
      match Hashtbl.find_opt old_pc_to_index old_pc with
      | Some old_idx ->
          let new_idx = old_to_new.(old_idx) in
          if new_idx < new_len then new_index_to_pc.(new_idx) else new_end_pc
      | None ->
          if old_pc = old_end_pc then
            let new_idx = old_to_new.(old_len) in
            if new_idx < new_len then new_index_to_pc.(new_idx) else new_end_pc
          else old_pc
    in
    (* Translate a body-relative target to a new encoded PC *)
    let translateInlined (body_relative_target : int) (body_emit_start : int) : int =
      let target_new_idx = body_emit_start + body_relative_target in
      if target_new_idx < new_len then new_index_to_pc.(target_new_idx) else new_end_pc
    in
    (* Fix up all jumps *)
    let new_code =
      Array.mapi
        (fun (i : int) (instr : instruction) ->
          match instr with
          | Jump target ->
              if Hashtbl.mem inlined_jumps i then Jump (translateInlined target (Hashtbl.find inlined_jumps i))
              else Jump (translateOld target)
          | JumpIfFalse target ->
              if Hashtbl.mem inlined_jumps i then JumpIfFalse (translateInlined target (Hashtbl.find inlined_jumps i))
              else JumpIfFalse (translateOld target)
          | JumpIfTrue target ->
              if Hashtbl.mem inlined_jumps i then JumpIfTrue (translateInlined target (Hashtbl.find inlined_jumps i))
              else JumpIfTrue (translateOld target)
          | LtIntJumpIfFalse target ->
              if Hashtbl.mem inlined_jumps i then
                LtIntJumpIfFalse (translateInlined target (Hashtbl.find inlined_jumps i))
              else LtIntJumpIfFalse (translateOld target)
          | GtIntJumpIfFalse target ->
              if Hashtbl.mem inlined_jumps i then
                GtIntJumpIfFalse (translateInlined target (Hashtbl.find inlined_jumps i))
              else GtIntJumpIfFalse (translateOld target)
          | EqIntJumpIfFalse target ->
              if Hashtbl.mem inlined_jumps i then
                EqIntJumpIfFalse (translateInlined target (Hashtbl.find inlined_jumps i))
              else EqIntJumpIfFalse (translateOld target)
          | LtRealJumpIfFalse target ->
              if Hashtbl.mem inlined_jumps i then
                LtRealJumpIfFalse (translateInlined target (Hashtbl.find inlined_jumps i))
              else LtRealJumpIfFalse (translateOld target)
          | GtRealJumpIfFalse target ->
              if Hashtbl.mem inlined_jumps i then
                GtRealJumpIfFalse (translateInlined target (Hashtbl.find inlined_jumps i))
              else GtRealJumpIfFalse (translateOld target)
          | EqRealJumpIfFalse target ->
              if Hashtbl.mem inlined_jumps i then
                EqRealJumpIfFalse (translateInlined target (Hashtbl.find inlined_jumps i))
              else EqRealJumpIfFalse (translateOld target)
          | other ->
              other )
        new_code
    in
    (* Fix up function entry_pcs *)
    let new_funcs = Optimize.fixupFunctions old_code funcs new_code old_to_new in
    let new_funcs =
      Array.mapi (fun (i : int) (f : bc_func) -> {f with n_locals= f.n_locals + extra_locals.(i)}) new_funcs
    in
    (new_code, new_funcs)
