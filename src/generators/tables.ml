open Code
open Core
open Util
open Vm
module Tags = Pparser.Ptags

let makeFloat (t : type_) x : exp =
  match t.t with
  | TReal -> { e = Real x; t }
  | TFix16 -> { e = Fixed x; t }
  | _ -> failwith "invalid type"


let makeInt x : exp = { e = Int x; t = Prog.C.int_t }
let makeArrayType precision dim : type_ = Prog.C.array_t ~dim precision

let makeRealTableDecl loc fname name precision data =
  let varname = fname ^ "_" ^ name in
  let size = List.length data in
  let t = makeArrayType precision size in
  let elems = CCList.map (makeFloat precision) data in
  { top = TopDecl ({ d = DId (varname, Some size); t }, { e = Array elems; t }); loc }


let makeIntTableDecl loc fname name data =
  let varname = fname ^ "_" ^ name in
  let size = List.length data in
  let t = makeArrayType Prog.C.int_t size in
  let elems = CCList.map makeInt data in
  { top = TopDecl ({ d = DId (varname, Some size); t }, { e = Array elems; t }); loc }


let generateRawAccessFunction loc full_name c t =
  let n = string_of_int c in
  let table_name = full_name ^ "_c" ^ n in
  let function_name = full_name ^ "_raw_c" ^ n in
  let r = { e = Index { e = { e = Id table_name; t }; index = { e = Id "index"; t = Prog.C.int_t } }; t } in
  let body = StmtReturn r in
  let args = [ "index", Prog.C.int_t ] in
  let t = [ Prog.C.int_t ], t in
  let info = { original_name = None; is_root = false } in
  { top = TopFunction ({ name = function_name; args; t; tags = []; loc; info }, body); loc }


let getCoefficients1 l =
  match l with
  | [ x1; x2 ] -> x1, x2
  | _ -> failwith "the curve fitting returned more than three points"


let getCoefficients2 l =
  match l with
  | [ x1; x2; x3 ] -> x1, x2, x3
  | _ -> failwith "the curve fitting returned more than three points"


let rec fitDataOrder1 data index acc0 acc1 =
  if index < 0 then
    acc0, acc1
  else (
    let p1 = data.(index) in
    let p2 = data.(index + 1) in
    let x = [ fst p1; fst p2 ] in
    let y = [ snd p1; snd p2 ] in
    let c0, c1 = Fitting.lagrange x y |> getCoefficients1 in
    fitDataOrder1 data (index - 1) (c0 :: acc0) (c1 :: acc1))


let rec fitDataOrder2 data index acc0 acc1 acc2 =
  if index < 0 then
    acc0, acc1, acc2
  else (
    let p1 = data.(index * 2) in
    let p2 = data.((index * 2) + 1) in
    let p3 = data.((index * 2) + 2) in
    let x = [ fst p1; fst p2; fst p3 ] in
    let y = [ snd p1; snd p2; snd p3 ] in
    let c0, c1, c2 = Fitting.lagrange x y |> getCoefficients2 in
    fitDataOrder2 data (index - 1) (c0 :: acc0) (c1 :: acc1) (c2 :: acc2))


let getRealResult (x : Vmv.rvalue) =
  match x with
  | Real y -> y
  | _ -> failwith "Function returned an unexpected type. This should not happen."


let getIntResult (x : Vmv.rvalue) =
  match x with
  | Int y -> y
  | _ -> failwith "Function returned an unexpected type. This should not happen."


let calculateIntRealTables loc vm name min max precision =
  let size = max - min in
  let data =
    List.init (size + 1) (fun i ->
      let x = min + i in
      getRealResult (Interpreter.callFunction vm name Vmv.[ Int x ]))
  in
  [ makeRealTableDecl loc name "table" precision data ]


let calculateIntIntTables loc vm name min max =
  let size = max - min in
  let data =
    List.init (size + 1) (fun i ->
      let x = min + i in
      getIntResult (Interpreter.callFunction vm name Vmv.[ Int x ]))
  in
  [ makeIntTableDecl loc name "table" data ]


let calculateTablesOrder1 loc vm name size min max precision =
  let map x x0 x1 y0 y1 = ((x -. x0) *. (y1 -. y0) /. (x1 -. x0)) +. y0 in
  let map_x x = map x 0. (float_of_int size) min max in
  let data =
    Array.init (size + 1) (fun i ->
      let x = map_x (float_of_int i) in
      x, getRealResult (Interpreter.callFunction vm name Vmv.[ Real x ]))
  in
  let acc0, acc1 = fitDataOrder1 data (size - 1) [] [] in
  [ makeRealTableDecl loc name "c0" precision acc0; makeRealTableDecl loc name "c1" precision acc1 ]


let calculateTablesOrder2 loc vm name size min max precision =
  let map x x0 x1 y0 y1 = ((x -. x0) *. (y1 -. y0) /. (x1 -. x0)) +. y0 in
  let map_x x = map x 0. (float_of_int size) min max in
  let data =
    Array.init
      ((size * 2) + 2)
      (fun i ->
        let x = map_x (float_of_int i /. 2.0) in
        x, getRealResult (Interpreter.callFunction vm name Vmv.[ Real x ]))
  in
  let acc0, acc1, acc2 = fitDataOrder2 data (size - 1) [] [] [] in
  [ makeRealTableDecl loc name "c0" precision acc0
  ; makeRealTableDecl loc name "c1" precision acc1
  ; makeRealTableDecl loc name "c2" precision acc2
  ]


let getCastIndexFunction (in_precision : type_) =
  match in_precision.t with
  | TFix16 -> "fix_to_int"
  | TReal -> "float_to_int"
  | _ -> failwith "invalid input precision"


let getIndex in_precision bound_check size value =
  let clip_call i =
    if bound_check then
      { e =
          Call
            { path = "int_clip"
            ; args = [ i; { e = Int 0; t = Prog.C.int_t }; { e = Int (size - 1); t = Prog.C.int_t } ]
            }
      ; t = Prog.C.int_t
      }
    else
      i
  in
  let int_call i = { e = Call { path = getCastIndexFunction in_precision; args = [ i ] }; t = Prog.C.int_t } in
  [ StmtDecl ({ d = DId ("index", None); t = Prog.C.int_t }, Some (clip_call (int_call value))) ]


let castInputVarPrecision (in_precision : type_) (out_precision : type_) (input : exp) : exp =
  match in_precision.t, out_precision.t with
  | TReal, TReal -> input
  | TFix16, TFix16 -> input
  | TReal, TFix16 -> { e = Call { path = "float_to_fix"; args = [ input ] }; t = Prog.C.fix16_t }
  | TFix16, TReal -> { e = Call { path = "fix_to_float"; args = [ input ] }; t = Prog.C.real_t }
  | _ -> failwith "castInputVarPrecision: invalid input"


let makeNuber (t : type_) v =
  match t.t with
  | TFix16 -> { e = Int (int_of_float (float_of_int 0x00010000 *. v)); t }
  | TReal -> { e = Real v; t }
  | _ -> failwith "invalid input precision"


let makeSub t e1 e2 = if e2 = 0.0 then e1 else { e = Op (Sub, e1, makeNuber t e2); t }
let makeMul t e1 e2 = if e2 = 1.0 then e1 else { e = Op (Mul, e1, makeNuber t e2); t }

let makeNewBody1 bound_check fname size in_precision t min max input =
  let atype = makeArrayType t size in
  let rindex = { e = Id "index"; t = Prog.C.int_t } in
  let getCoeff a =
    let arr = { e = Id (fname ^ "_" ^ a); t = atype } in
    { e = Index { e = arr; index = rindex }; t }
  in
  let initial_index = (float_of_int size -. 1.0) /. (max -. min) in
  let value = makeMul in_precision (makeSub in_precision input min) initial_index in
  let index_stmts = getIndex in_precision bound_check size value in
  let input = castInputVarPrecision in_precision t input in
  let return = StmtReturn { e = Op (Add, getCoeff "c0", { e = Op (Mul, input, getCoeff "c1"); t }); t } in
  StmtBlock (index_stmts @ [ return ])


let makeNewBody2 bound_check fname size in_precision t min max input =
  let atype = makeArrayType t size in
  let rindex = { e = Id "index"; t = Prog.C.int_t } in
  let getCoeff a =
    let arr = { e = Id (fname ^ "_" ^ a); t = atype } in
    { e = Index { e = arr; index = rindex }; t }
  in
  let initial_index = (float_of_int size -. 1.0) /. (max -. min) in
  let value = makeMul in_precision (makeSub in_precision input min) initial_index in
  let index_stmts = getIndex in_precision bound_check size value in
  let input = castInputVarPrecision in_precision t input in
  let k2 = { e = Op (Mul, getCoeff "c2", input); t } in
  let k1 = { e = Op (Mul, input, { e = Op (Add, getCoeff "c1", k2); t }); t } in
  let return = StmtReturn { e = Op (Add, getCoeff "c0", k1); t } in
  StmtBlock (index_stmts @ [ return ])


let makeIntAccessBody fname out_type min max input =
  let atype = makeArrayType out_type (max - min) in
  let index =
    { e =
        Call
          { path = "int_clip"; args = [ input; { e = Int min; t = Prog.C.int_t }; { e = Int max; t = Prog.C.int_t } ] }
    ; t = Prog.C.int_t
    }
  in
  let index = { e = Op (Add, index, makeInt (-min)); t = Prog.C.int_t } in
  StmtReturn { e = Index { e = { e = Id (fname ^ "_table"); t = atype }; index }; t = out_type }


let getBoundCheckValue t =
  match t with
  | Some (Tags.Bool v) -> v
  | None -> true
  | _ -> failwith "Invalid value of 'bound_check' tag"


let getOrderValue t =
  match t with
  | Some (Tags.Int v) -> v
  | None -> 2
  | _ -> failwith "Invalid value of 'bound_check' tag"


let checkInputVariables (loc : Loc.t) (args : param list) : exp =
  match args with
  | [ (name, t) ] -> { e = Id name; t }
  | _ ->
    let msg = "This attribute requires the function to have only one argument:\n\"fun foo(x:real) : real\"" in
    Error.raiseError msg loc


let makeTable vm (def : function_def) =
  let params = Tags.[ "size", TypeInt; "min", TypeReal; "max", TypeReal; "order", TypeInt; "bound_check", TypeBool ] in
  let loc = def.loc in
  match Tags.getParameterList def.tags "table" params with
  | Tags.[ Some (Int size); Some (Real min); Some (Real max); order; bound_check ] -> (
    let bound_check = getBoundCheckValue bound_check in
    let out_precision = snd def.t in
    let var = checkInputVariables def.loc def.args in
    let in_precision = var.t in
    match order with
    | Some (Int 1) ->
      let result = calculateTablesOrder1 loc vm def.name size min max out_precision in
      let new_body = makeNewBody1 bound_check def.name size in_precision out_precision min max var in
      let c0 = generateRawAccessFunction loc def.name 0 out_precision in
      let c1 = generateRawAccessFunction loc def.name 1 out_precision in
      result @ [ c0; c1 ] @ [ { top = TopFunction (def, new_body); loc } ]
    | _ ->
      let result = calculateTablesOrder2 loc vm def.name size min max out_precision in
      let new_body = makeNewBody2 bound_check def.name size in_precision out_precision min max var in
      let c0 = generateRawAccessFunction loc def.name 0 out_precision in
      let c1 = generateRawAccessFunction loc def.name 1 out_precision in
      let c2 = generateRawAccessFunction loc def.name 2 out_precision in
      result @ [ c0; c1; c2 ] @ [ { top = TopFunction (def, new_body); loc } ])
  | _ ->
    let msg =
      "The attribute 'table' requires specific parameters. e.g. 'table(size = 128, min = 0.0, max = 1.0, [order = 2], \
       [bound_check = true])'"
    in
    Util.Error.raiseError msg def.loc


let makeIntTable vm (def : function_def) =
  let params = Tags.[ "min", TypeInt; "max", TypeInt ] in
  let loc = def.loc in
  match Tags.getParameterList def.tags "table" params, def.t with
  | Tags.[ Some (Int min); Some (Int max) ], (_, ({ t = TReal | TFix16; _ } as out_precision)) ->
    let var = checkInputVariables def.loc def.args in
    let result = calculateIntRealTables loc vm def.name min max out_precision in
    let new_body = makeIntAccessBody def.name out_precision min max var in
    result @ [ { top = TopFunction (def, new_body); loc } ]
  | Tags.[ Some (Int min); Some (Int max) ], (_, { t = TInt; _ }) ->
    let var = checkInputVariables def.loc def.args in
    let result = calculateIntIntTables loc vm def.name min max in
    let new_body = makeIntAccessBody def.name Prog.C.int_t min max var in
    result @ [ { top = TopFunction (def, new_body); loc } ]
  | _ ->
    let msg = "The attribute 'table' on integer tables requires specific parameters. e.g. 'table(min = 0, max = 16)'" in
    Util.Error.raiseError msg def.loc


let readFile (loc : Loc.t) (includes : string list) (file : string) : WaveFile.wave =
  match FileIO.findFile includes file with
  | Some filename -> (
    match WaveFile.read filename with
    | Ok wave -> wave
    | Error read_msg ->
      let msg = "Failed to read the wav file '" ^ file ^ "': " ^ read_msg in
      Error.raiseError msg loc)
  | None ->
    let msg = "The file '" ^ file ^ "' was not found in any of the include locations" in
    Error.raiseError msg loc


let checkNumberOfChannels (loc : Loc.t) (channels : int) (wave : WaveFile.wave) : unit =
  if wave.WaveFile.channels <> channels then (
    let msg =
      "The given number of channels ("
      ^ string_of_int channels
      ^ ") does not match the actual number of the channels in the file ("
      ^ string_of_int wave.WaveFile.channels
      ^ ")"
    in
    Error.raiseError msg loc)


let getDeclarations loc name (wav_data : WaveFile.wave) precision : top_stmt list =
  Array.mapi
    (fun i v -> makeRealTableDecl loc name ("chan_" ^ string_of_int i) precision (Array.to_list v))
    wav_data.WaveFile.data
  |> Array.to_list


let checkWaveInputVariables (loc : Loc.t) (args : param list) : exp * exp =
  match args with
  | [ (channel, channel_t); (index, index_t) ] -> { e = Id channel; t = channel_t }, { e = Id index; t = index_t }
  | _ ->
    let msg =
      "This attribute requires the function to have the following arguments:\n\
       \"external wave(channel:int, index:int) : real\""
    in
    Error.raiseError msg loc


let accessChannel (fname : string) (channel : exp) (index : exp) (samples : int) t (i : int) : stmt =
  let table_name = fname ^ "_" ^ "chan_" ^ string_of_int i in
  (*let table = { e = Call { path = getWrapArrayName t; args = [ { e = Id table_name; t } ] }; t } in*)
  let table = { e = Id table_name; t } in
  let i = { e = Int i; t = Prog.C.int_t } in
  let samples_e = { e = Int samples; t = Prog.C.int_t } in
  let cond = { e = Op (Eq, channel, i); t = Prog.C.bool_t } in
  let ret = { e = Index { e = table; index = { e = Op (Mod, index, samples_e); t = Prog.C.int_t } }; t } in
  StmtIf (cond, StmtReturn ret, None)


let makeNewBody (def : function_def) (wave : WaveFile.wave) precision : stmt =
  let channel, index = checkWaveInputVariables def.loc def.args in
  let stmts =
    CCList.init wave.WaveFile.channels (accessChannel def.name channel index wave.WaveFile.samples precision)
  in
  let default = StmtReturn { e = Real 0.0; t = Prog.C.real_t } in
  StmtBlock (stmts @ [ default ])


let makeSizeFunction (def : function_def) (size : int) : top_stmt =
  let size_name = def.name ^ "_samples" in
  let body = StmtReturn { e = Int size; t = Prog.C.int_t } in
  let info = { original_name = None; is_root = false } in
  { top = TopFunction ({ name = size_name; args = []; t = [], Prog.C.int_t; tags = []; loc = def.loc; info }, body)
  ; loc = def.loc
  }


let makeWave (args : Args.args) _vm (def : function_def) =
  let params = Tags.[ "channels", TypeInt; "file", TypeString ] in
  match Tags.getParameterList def.tags "wave" params with
  | [ Some (Int channels); Some (String file) ] ->
    let precision = snd def.t in
    let wave = readFile def.loc args.includes file in
    let () = checkNumberOfChannels def.loc channels wave in
    let result = getDeclarations def.loc def.name wave precision in
    let body = makeNewBody def wave precision in
    let size_fun = makeSizeFunction def wave.WaveFile.samples in
    result @ [ size_fun; { top = TopFunction (def, body); loc = def.loc } ]
  | _ ->
    let msg = "The attribute 'wave' requires specific parameters. e.g. 'wave(channels=1, file=\"file.wav\")'" in
    Util.Error.raiseError msg def.loc


let wrapGet data index =
  if index >= Array.length data then
    data.(index - Array.length data)
  else if index < 0 then
    data.(Array.length data - index)
  else
    data.(index)


let rec fitWavetableData data index acc0 acc1 acc2 =
  if index < 0 then
    acc0, acc1, acc2
  else (
    let p1 = wrapGet data index in
    let p2 = wrapGet data (index + 1) in
    let p3 = wrapGet data (index + 2) in
    let x = [ fst p1; fst p2; fst p3 ] in
    let y = [ snd p1; snd p2; snd p3 ] in
    let c0, c1, c2 = Fitting.lagrange x y |> getCoefficients2 in
    fitWavetableData data (index - 1) (c0 :: acc0) (c1 :: acc1) (c2 :: acc2))


let makeWavetableOrder2 loc name size precision data =
  let acc0, acc1, acc2 = fitWavetableData data size [] [] [] in
  [ makeRealTableDecl loc name "c0" precision acc0
  ; makeRealTableDecl loc name "c1" precision acc1
  ; makeRealTableDecl loc name "c2" precision acc2
  ]


let makeWavetable (args : Args.args) _vm (def : function_def) =
  let params = Tags.[ "file", TypeString; "bound_check", TypeBool ] in
  match Tags.getParameterList def.tags "wavetable" params with
  | [ Some (String file); bound_check ] ->
    let bound_check = getBoundCheckValue bound_check in
    let out_precision = snd def.t in
    let var = checkInputVariables def.loc def.args in
    let in_precision = var.t in
    let wave = readFile def.loc args.includes file in
    let () = checkNumberOfChannels def.loc 1 wave in
    let data = wave.data.(0) in
    let size_n = Array.length data in
    let size = float_of_int size_n in
    let data = Array.mapi (fun x y -> float_of_int x /. (size -. 1.0), y) data in
    let result = makeWavetableOrder2 def.loc def.name size_n out_precision data in
    let new_body = makeNewBody2 bound_check def.name size_n in_precision out_precision 0.0 1.0 var in
    let c0 = generateRawAccessFunction def.loc def.name 0 out_precision in
    let c1 = generateRawAccessFunction def.loc def.name 1 out_precision in
    let c2 = generateRawAccessFunction def.loc def.name 2 out_precision in
    result @ [ c0; c1; c2 ] @ [ { top = TopFunction (def, new_body); loc = def.loc } ]
  | _ ->
    let msg = "This attribute can only be applied to functions returning 'real'" in
    Util.Error.raiseError msg def.loc


let replaceFunction (args : Args.args) vm stmt =
  match stmt.top with
  | TopFunction (({ t = [ { t = TInt; _ } ], _; _ } as def), _) when Tags.has def.tags "table" -> makeIntTable vm def
  | TopFunction (def, _) when Tags.has def.tags "table" -> makeTable vm def
  | TopExternal (def, _) when Tags.has def.tags "wave" -> makeWave args vm def
  | TopExternal (def, _) when Tags.has def.tags "wavetable" -> makeWavetable args vm def
  | _ -> [ stmt ]


let create (args : Args.args) vm stmts = CCList.flat_map (replaceFunction args vm) stmts
