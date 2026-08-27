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
open Util
module Tags = Pparser.Ptags

let makeFloat (t : type_) x : exp =
  match t.t with
  | TReal ->
      C.ereal x
  | TFix16 ->
      C.efix16 x
  | TInt16 ->
      C.eint16 (int_of_float (x *. 65536.0))
  | _ ->
      failwith "invalid type"

let makeInt (t : type_) x : exp =
  match t.t with TInt -> C.eint x | TInt16 -> C.eint16 x | _ -> failwith "invalid type"

let makeArrayType precision dim : type_ = C.array_t ~dim precision

let makeRealTableDecl loc fname name precision data =
  let varname = fname ^ "_" ^ name in
  let size = CCList.length data in
  let t = makeArrayType precision size in
  let elems = CCList.map (makeFloat precision) data in
  {top= TopConstant (varname, Some size, t, C.earray elems t, None); loc}

let makeIntTableDecl loc fname name int_type data =
  let varname = fname ^ "_" ^ name in
  let size = CCList.length data in
  let t = makeArrayType int_type size in
  let elems = CCList.map (makeInt int_type) data in
  {top= TopConstant (varname, Some size, t, C.earray elems t, None); loc}

let coefficientTableName fname = fname ^ "_c"

(* The coefficients of one table point are stored contiguously
   ([c0.(0); c1.(0); c2.(0); c0.(1); ...]) rather than in one array per coefficient, so that a
   lookup reads a single region of the table instead of one region per coefficient. On a part
   with a small flash data cache, such as the 8 x 16 byte cache of the STM32F4 ART accelerator,
   a scattered lookup then costs one line fill instead of one per coefficient. The stored values
   are unchanged. *)
let interleave (coefficients : float list list) : float list =
  let columns = CCList.map Array.of_list coefficients in
  match columns with
  | [] ->
      []
  | first :: _ ->
      let points = Array.length first in
      CCList.init points (fun i -> CCList.map (fun column -> column.(i)) columns) |> CCList.flatten

let makeCoefficientTableDecl loc fname precision coefficients =
  makeRealTableDecl loc fname "c" precision (interleave coefficients)

(* Offset of coefficient [c] within the point selected by [index]. The generated bodies keep
   "index" already multiplied by the stride, so each coefficient is one constant offset away. *)
let coefficientOffset index c = if c = 0 then index else C.eadd index (C.eint c)

let generateRawAccessFunction loc full_name ~stride ~size c t =
  let n = string_of_int c in
  let table_name = coefficientTableName full_name in
  let function_name = full_name ^ "_raw_c" ^ n in
  let atype = makeArrayType t (size * stride) in
  let offset = coefficientOffset (C.emul (C.eid "index" C.int_t) (C.eint stride)) c in
  let r = C.eindex (C.eid table_name atype) offset t in
  let body = {s= StmtReturn r; loc} in
  let args = [C.param ~const:true "index" C.int_t] in
  let t = ([C.int_t], t) in
  let info = {original_name= None; is_root= false} in
  {top= TopFunction ({name= function_name; args; t; tags= []; loc; info}, body); loc}

(* One raw accessor per coefficient, all reading the shared interleaved table. *)
let generateRawAccessFunctions loc full_name ~stride ~size t =
  CCList.init stride (fun c -> generateRawAccessFunction loc full_name ~stride ~size c t)

let getCoefficients1 l =
  match l with [x1; x2] -> (x1, x2) | _ -> failwith "the curve fitting returned more than three points"

let getCoefficients2 l =
  match l with [x1; x2; x3] -> (x1, x2, x3) | _ -> failwith "the curve fitting returned more than three points"

let rec fitDataOrder1 data index acc0 acc1 =
  if index < 0 then (acc0, acc1)
  else
    let p1 = data.(index) in
    let p2 = data.(index + 1) in
    let x = [fst p1; fst p2] in
    let y = [snd p1; snd p2] in
    let c0, c1 = Fitting.lagrange x y |> getCoefficients1 in
    fitDataOrder1 data (index - 1) (c0 :: acc0) (c1 :: acc1)

let rec fitDataOrder2 data index acc0 acc1 acc2 =
  if index < 0 then (acc0, acc1, acc2)
  else
    let p1 = data.(index * 2) in
    let p2 = data.((index * 2) + 1) in
    let p3 = data.((index * 2) + 2) in
    let x = [fst p1; fst p2; fst p3] in
    let y = [snd p1; snd p2; snd p3] in
    let c0, c1, c2 = Fitting.lagrange x y |> getCoefficients2 in
    fitDataOrder2 data (index - 1) (c0 :: acc0) (c1 :: acc1) (c2 :: acc2)

let getRealResult (x : Core.Interpreter.dvalue) =
  match x with
  | DReal y ->
      y
  | _ ->
      failwith "getRealResult: Function returned an unexpected type. This should not happen."

let getIntResult (x : Core.Interpreter.dvalue) =
  match x with
  | DInt y ->
      y
  | DInt16 y ->
      y
  | _ ->
      failwith "getIntResult: Function returned an unexpected type. This should not happen."

let calculateIntRealTables loc iprog name min max precision =
  let size = max - min in
  let fun_index = Util.Maps.Map.find name iprog.Core.Interpreter.ifunction_names in
  let stack = Core.Interpreter.createStack 256 in
  let data =
    CCList.init (size + 1) (fun i ->
        let x = min + i in
        getRealResult (Core.Interpreter.callFunctionEntry iprog stack fun_index [DInt x]) )
  in
  [makeRealTableDecl loc name "table" precision data]

let calculateIntIntTables loc iprog name (int_type : type_) min max =
  let size = max - min in
  let fun_index = Util.Maps.Map.find name iprog.Core.Interpreter.ifunction_names in
  let stack = Core.Interpreter.createStack 256 in
  let makeInt i : Core.Interpreter.dvalue =
    match int_type with
    | {t= TInt; _} ->
        DInt i
    | {t= TInt16; _} ->
        DInt16 i
    | _ ->
        failwith "makeInt: invalid input type"
  in
  let data =
    CCList.init (size + 1) (fun i ->
        let x = min + i in
        getIntResult (Core.Interpreter.callFunctionEntry iprog stack fun_index [makeInt x]) )
  in
  [makeIntTableDecl loc name "table" int_type data]

let calculateTablesOrder1 loc iprog name size min max precision =
  let map x x0 x1 y0 y1 = ((x -. x0) *. (y1 -. y0) /. (x1 -. x0)) +. y0 in
  let map_x x = map x 0. (float_of_int size) min max in
  let fun_index = Util.Maps.Map.find name iprog.Core.Interpreter.ifunction_names in
  let stack = Core.Interpreter.createStack 256 in
  let data =
    Array.init (size + 1) (fun i ->
        let x = map_x (float_of_int i) in
        (x, getRealResult (Core.Interpreter.callFunctionEntry iprog stack fun_index [DReal x])) )
  in
  let acc0, acc1 = fitDataOrder1 data (size - 1) [] [] in
  [makeCoefficientTableDecl loc name precision [acc0; acc1]]

let calculateTablesOrder1Fixed loc iprog name size min max precision =
  let map x x0 x1 y0 y1 = ((x -. x0) *. (y1 -. y0) /. (x1 -. x0)) +. y0 in
  let map_x x = map x 0. (float_of_int size) min max in
  let fun_index = Util.Maps.Map.find name iprog.Core.Interpreter.ifunction_names in
  let stack = Core.Interpreter.createStack 256 in
  let data =
    CCList.init (size + 1) (fun i ->
        let x = map_x (float_of_int i) in
        (x, getRealResult (Core.Interpreter.callFunctionEntry iprog stack fun_index [DReal x])) )
  in
  let rec increments data =
    match data with [] -> [0.0] | [_] -> [0.0] | (_, y1) :: ((_, y2) :: _ as t) -> (y2 -. y1) :: increments t
  in
  let acc0 = CCList.map snd data in
  let acc1 = increments data in
  [makeCoefficientTableDecl loc name precision [acc0; acc1]]

let calculateTablesOrder2 loc iprog name size min max precision =
  let map x x0 x1 y0 y1 = ((x -. x0) *. (y1 -. y0) /. (x1 -. x0)) +. y0 in
  let map_x x = map x 0. (float_of_int size) min max in
  let fun_index = Util.Maps.Map.find name iprog.Core.Interpreter.ifunction_names in
  let stack = Core.Interpreter.createStack 256 in
  let data =
    Array.init
      ((size * 2) + 2)
      (fun i ->
        let x = map_x (float_of_int i /. 2.0) in
        (x, getRealResult (Core.Interpreter.callFunctionEntry iprog stack fun_index [DReal x])) )
  in
  let acc0, acc1, acc2 = fitDataOrder2 data (size - 1) [] [] [] in
  [makeCoefficientTableDecl loc name precision [acc0; acc1; acc2]]

let getCastIndexFunction (in_precision : type_) =
  match in_precision.t with
  | TFix16 ->
      "fix_to_int"
  | TInt16 ->
      "int16_to_int"
  | TReal ->
      "float_to_int"
  | _ ->
      failwith "invalid input precision"

let getIndex in_precision bound_check ~stride size value =
  let clip_call i = if bound_check then C.ecall "clip" [i; C.eint 0; C.eint (size - 1)] C.int_t else i in
  let int_call i = C.ecall (getCastIndexFunction in_precision) [i] C.int_t in
  let scale i = if stride = 1 then i else C.emul i (C.eint stride) in
  [C.sdecl ~init:(scale (clip_call (int_call value))) "index" C.int_t]

(* Reads coefficient [c] of the point selected by "index", which already includes the stride. *)
let makeGetCoeff fname ~stride ~size t =
  let atype = makeArrayType t (size * stride) in
  let arr = C.eid (coefficientTableName fname) atype in
  fun c -> C.eindex arr (coefficientOffset (C.eid "index" C.int_t) c) t

let castInputVarPrecision (in_precision : type_) (out_precision : type_) (input : exp) : exp =
  match (in_precision.t, out_precision.t) with
  | TReal, TReal ->
      input
  | TFix16, TFix16 ->
      input
  | TInt16, TInt16 ->
      input
  | TReal, TFix16 ->
      C.ecall "float_to_fix" [input] C.fix16_t
  | TFix16, TReal ->
      C.ecall "fix_to_float" [input] C.real_t
  | TInt16, TReal ->
      C.ecall "int16_to_float" [input] C.real_t
  | TReal, TInt16 ->
      C.ecall "float_to_int16" [input] C.int16_t
  | TInt16, TFix16 ->
      C.ecall "int16_to_fix" [input] C.fix16_t
  | TFix16, TInt16 ->
      C.ecall "fix_to_int16" [input] C.int16_t
  | _ ->
      failwith "castInputVarPrecision: invalid input"

let makeNuber (t : type_) v =
  match t.t with TFix16 -> C.efix16 v | TReal -> C.ereal v | _ -> failwith "invalid input precision"

let makeSub t e1 e2 =
  if e2 = 0.0 then e1 else if e2 < 0.0 then C.eadd e1 (makeNuber t (-.e2)) else C.esub e1 (makeNuber t e2)

let makeMul t e1 e2 = if e2 = 1.0 then e1 else C.emul e1 (makeNuber t e2)

let makeNewBody1 bound_check fname size in_precision t min max input =
  let stride = 2 in
  let getCoeff = makeGetCoeff fname ~stride ~size t in
  let initial_index = float_of_int size /. (max -. min) in
  let value = makeMul in_precision (makeSub in_precision input min) initial_index in
  let index_stmts = getIndex in_precision bound_check ~stride size value in
  let input = castInputVarPrecision in_precision t input in
  let return = C.sreturn (C.eadd (getCoeff 0) (C.emul input (getCoeff 1))) in
  C.sblock (index_stmts @ [return])

let makeNewBody1Fixed _bound_check fname size in_precision t min max input =
  let stride = 2 in
  let getCoeff = makeGetCoeff fname ~stride ~size t in
  let initial_index = float_of_int size /. (max -. min) in
  (* Clamp the input, not the scaled value: scaling first overflows the fixed-point range for
     inputs well outside [min, max], and the clamp would then be applied to a wrapped value. *)
  let clamped = C.ecall "clip" [input; makeFloat in_precision min; makeFloat in_precision max] in_precision in
  let value = makeMul in_precision (makeSub in_precision clamped min) initial_index in
  let value_decl = C.sdecl_bind "value" value in_precision in
  let decimal =
    C.sdecl_bind "decimal"
      (C.esub (C.eid "value" in_precision) (C.ecall "floor" [C.eid "value" in_precision] in_precision))
      in_precision
  in
  let index =
    C.sdecl_bind "index" (C.emul (C.ecall "int" [C.eid "value" in_precision] C.int_t) (C.eint stride)) C.int_t
  in
  let return = C.sreturn (C.eadd (getCoeff 0) (C.emul (getCoeff 1) (C.eid "decimal" in_precision))) in
  C.sblock (value_decl @ index @ decimal @ [return])

let makeNewBody2 bound_check fname size in_precision t min max input =
  let stride = 3 in
  let getCoeff = makeGetCoeff fname ~stride ~size t in
  let initial_index = float_of_int size /. (max -. min) in
  let value = makeMul in_precision (makeSub in_precision input min) initial_index in
  let index_stmts = getIndex in_precision bound_check ~stride size value in
  let input = castInputVarPrecision in_precision t input in
  let k2 = C.emul (getCoeff 2) input in
  let k1 = C.emul input (C.eadd (getCoeff 1) k2) in
  let return = C.sreturn (C.eadd (getCoeff 0) k1) in
  C.sblock (index_stmts @ [return])

let makeIntAccessBody fname out_type min max input =
  let atype = makeArrayType out_type (max - min) in
  let index = C.ecall "int_clip" [input; C.eint min; C.eint max] C.int_t in
  let index = C.eadd index (C.eint (-min)) in
  C.sreturn (C.eindex (C.eid (fname ^ "_table") atype) index out_type)

let getBoundCheckValue t =
  match t with Some (Tags.Bool v) -> v | None -> true | _ -> failwith "Invalid value of 'bound_check' tag"

let getOrderValue t =
  match t with Some (Tags.Int v) -> v | None -> 2 | _ -> failwith "Invalid value of 'bound_check' tag"

let checkInputVariables (loc : Loc.t) (args : param list) : exp =
  match args with
  | [{name; t; _}] ->
      C.eid name t
  | _ ->
      let msg =
        "Table generation attribute requires a function with exactly one parameter (e.g., 'fun foo(x:type) : type')"
      in
      Error.raiseError msg loc

let optimizeSize (n : int) =
  let l = Float.log2 (float_of_int n) in
  if l = floor l then n + 1 else n

let makeTable vm (def : function_def) =
  let params =
    Tags.[("size", TypeInt); ("min", TypeReal); ("max", TypeReal); ("order", TypeInt); ("bound_check", TypeBool)]
  in
  let loc = def.loc in
  match Tags.getParameterList def.tags "table" params with
  | Tags.[Some (Int size); Some (Real min); Some (Real max); order; bound_check] -> (
      let bound_check = getBoundCheckValue bound_check in
      let out_precision = snd def.t in
      let var = checkInputVariables def.loc def.args in
      let in_precision = var.t in
      match (order, in_precision, out_precision) with
      | _, {t= TFix16; _}, {t= TFix16; _} ->
          let size = optimizeSize size in
          let result = calculateTablesOrder1Fixed loc vm def.name size min max out_precision in
          let new_body = makeNewBody1Fixed bound_check def.name size in_precision out_precision min max var in
          let raw = generateRawAccessFunctions loc def.name ~stride:2 ~size out_precision in
          result @ raw @ [{top= TopFunction (def, new_body); loc}]
      | Some (Int 1), _, _ ->
          let result = calculateTablesOrder1 loc vm def.name size min max out_precision in
          let new_body = makeNewBody1 bound_check def.name size in_precision out_precision min max var in
          let raw = generateRawAccessFunctions loc def.name ~stride:2 ~size out_precision in
          result @ raw @ [{top= TopFunction (def, new_body); loc}]
      | _ ->
          let result = calculateTablesOrder2 loc vm def.name size min max out_precision in
          let new_body = makeNewBody2 bound_check def.name size in_precision out_precision min max var in
          let raw = generateRawAccessFunctions loc def.name ~stride:3 ~size out_precision in
          result @ raw @ [{top= TopFunction (def, new_body); loc}] )
  | _ ->
      let msg =
        "The attribute 'table' requires specific parameters. e.g. 'table(size = 128, min = 0.0, max = 1.0, [order = \
         2], [bound_check = true])'"
      in
      Util.Error.raiseError msg def.loc

let makeIntTable vm (def : function_def) =
  let params = Tags.[("min", TypeInt); ("max", TypeInt)] in
  let loc = def.loc in
  match (Tags.getParameterList def.tags "table" params, def.t) with
  | Tags.[Some (Int min); Some (Int max)], (_, ({t= TReal | TFix16; _} as out_precision)) ->
      let var = checkInputVariables def.loc def.args in
      let result = calculateIntRealTables loc vm def.name min max out_precision in
      let new_body = makeIntAccessBody def.name out_precision min max var in
      result @ [{top= TopFunction (def, new_body); loc}]
  | Tags.[Some (Int min); Some (Int max)], ([({t= TInt | TInt16; _} as arg_type)], ({t= TInt | TInt16; _} as int_type))
    ->
      let () =
        if compare arg_type.t int_type.t = 0 then ()
        else
          let msg = "To generate the table, the function requires the same types as input and output" in
          Util.Error.raiseError msg def.loc
      in
      let var = checkInputVariables def.loc def.args in
      let result = calculateIntIntTables loc vm def.name int_type min max in
      let new_body = makeIntAccessBody def.name int_type min max var in
      result @ [{top= TopFunction (def, new_body); loc}]
  | _ ->
      let msg =
        "The attribute 'table' on integer tables requires specific parameters. e.g. 'table(min = 0, max = 16)'"
      in
      Util.Error.raiseError msg def.loc

let readFile (loc : Loc.t) (includes : string list) (file : string) : WaveFile.wave =
  match FileIO.findFile includes file with
  | Some filename -> (
    match WaveFile.read filename with
    | Ok wave ->
        wave
    | Error read_msg ->
        let msg = "Failed to read the wav file '" ^ file ^ "': " ^ read_msg in
        Error.raiseError msg loc )
  | None ->
      let msg = "The file '" ^ file ^ "' was not found in any of the include locations" in
      Error.raiseError msg loc

let checkNumberOfChannels (loc : Loc.t) (channels : int) (wave : WaveFile.wave) : unit =
  if wave.WaveFile.channels <> channels then
    let msg =
      "The given number of channels (" ^ string_of_int channels
      ^ ") does not match the actual number of the channels in the file (" ^ string_of_int wave.WaveFile.channels ^ ")"
    in
    Error.raiseError msg loc

let getDeclarations loc name (wav_data : WaveFile.wave) precision : top_stmt list =
  Array.mapi
    (fun i v -> makeRealTableDecl loc name ("chan_" ^ string_of_int i) precision (Array.to_list v))
    wav_data.WaveFile.data
  |> Array.to_list

let checkWaveInputVariables (loc : Loc.t) (args : param list) : exp * exp =
  match args with
  | [{name= channel; t= channel_t; _}; {name= index; t= index_t; _}] ->
      (C.eid channel channel_t, C.eid index index_t)
  | _ ->
      let msg =
        "This attribute requires the function to have the following arguments:\n\
         \"external wave(channel:int, index:int) : real\""
      in
      Error.raiseError msg loc

let accessChannel (fname : string) (channel : exp) (index : exp) (samples : int) t (i : int) : stmt =
  let table_name = fname ^ "_" ^ "chan_" ^ string_of_int i in
  let table = C.eid table_name t in
  let i = C.eint i in
  let samples_e = C.eint samples in
  let cond = C.eeq channel i in
  let ret = C.eindex table (C.emod index samples_e) t in
  C.sif cond (C.sreturn ret) None

let makeNewBody (def : function_def) (wave : WaveFile.wave) precision : stmt =
  let channel, index = checkWaveInputVariables def.loc def.args in
  let stmts =
    CCList.init wave.WaveFile.channels (accessChannel def.name channel index wave.WaveFile.samples precision)
  in
  let default = C.sreturn (C.ereal 0.0) in
  C.sblock (stmts @ [default])

let makeSizeFunction (def : function_def) (size : int) : top_stmt =
  let size_name = def.name ^ "_samples" in
  let body = C.sreturn (C.eint size) in
  let info = {original_name= None; is_root= false} in
  {top= TopFunction ({name= size_name; args= []; t= ([], C.int_t); tags= []; loc= def.loc; info}, body); loc= def.loc}

let makeWave (args : Args.args) _vm (def : function_def) =
  let params = Tags.[("channels", TypeInt); ("file", TypeString)] in
  match Tags.getParameterList def.tags "wave" params with
  | [Some (Int channels); Some (String file)] ->
      let precision = snd def.t in
      let wave = readFile def.loc args.includes file in
      let () = checkNumberOfChannels def.loc channels wave in
      let result = getDeclarations def.loc def.name wave precision in
      let body = makeNewBody def wave precision in
      let size_fun = makeSizeFunction def wave.WaveFile.samples in
      result @ [size_fun; {top= TopFunction (def, body); loc= def.loc}]
  | _ ->
      let msg = "The attribute 'wave' requires specific parameters. e.g. 'wave(channels=1, file=\"file.wav\")'" in
      Util.Error.raiseError msg def.loc

(* Wavetables are cyclic, so points past either end wrap around. *)
let wrapGet data index =
  let n = Array.length data in
  data.(((index mod n) + n) mod n)

let rec fitWavetableData data index acc0 acc1 acc2 =
  if index < 0 then (acc0, acc1, acc2)
  else
    let p1 = wrapGet data index in
    let p2 = wrapGet data (index + 1) in
    let p3 = wrapGet data (index + 2) in
    let x = [fst p1; fst p2; fst p3] in
    let y = [snd p1; snd p2; snd p3] in
    let c0, c1, c2 = Fitting.lagrange x y |> getCoefficients2 in
    fitWavetableData data (index - 1) (c0 :: acc0) (c1 :: acc1) (c2 :: acc2)

let makeWavetableOrder2 loc name size precision data =
  let acc0, acc1, acc2 = fitWavetableData data size [] [] [] in
  [makeCoefficientTableDecl loc name precision [acc0; acc1; acc2]]

let makeWavetable (args : Args.args) _vm (def : function_def) =
  let params = Tags.[("file", TypeString); ("bound_check", TypeBool)] in
  match Tags.getParameterList def.tags "wavetable" params with
  | [Some (String file); bound_check] ->
      let bound_check = getBoundCheckValue bound_check in
      let out_precision = snd def.t in
      let var = checkInputVariables def.loc def.args in
      let in_precision = var.t in
      let wave = readFile def.loc args.includes file in
      let () = checkNumberOfChannels def.loc 1 wave in
      let data = wave.data.(0) in
      let size_n = Array.length data in
      let size = float_of_int size_n in
      let data = Array.mapi (fun x y -> (float_of_int x /. (size -. 1.0), y)) data in
      let result = makeWavetableOrder2 def.loc def.name size_n out_precision data in
      let new_body = makeNewBody2 bound_check def.name size_n in_precision out_precision 0.0 1.0 var in
      let raw = generateRawAccessFunctions def.loc def.name ~stride:3 ~size:size_n out_precision in
      let size_fun = makeSizeFunction def wave.WaveFile.samples in
      result @ (size_fun :: raw) @ [{top= TopFunction (def, new_body); loc= def.loc}]
  | _ ->
      let msg = "This attribute can only be applied to functions returning 'real'" in
      Util.Error.raiseError msg def.loc

let replaceFunction (args : Args.args) vm stmt =
  match stmt.top with
  | TopFunction (({t= [{t= TInt | TInt16; _}], _; _} as def), _) when Tags.has def.tags "table" ->
      makeIntTable vm def
  | TopFunction (def, _) when Tags.has def.tags "table" ->
      makeTable vm def
  | TopExternal (def, _) when Tags.has def.tags "wave" ->
      makeWave args vm def
  | TopExternal (def, _) when Tags.has def.tags "wavetable" ->
      makeWavetable args vm def
  | _ ->
      [stmt]

let create (args : Args.args) vm stmts = CCList.flat_map (replaceFunction args vm) stmts
