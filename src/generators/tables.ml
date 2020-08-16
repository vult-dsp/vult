open Code
open Util
open Vm
module Tags = Parser.Tags

let makeFloat (t : type_) x : exp = { e = Real x; t }

let makeArrayType precision size : type_ = Array (size, precision)

let makeDecl fname name precision data =
  let varname = fname ^ "_" ^ name in
  let size = List.length data in
  let t = makeArrayType precision size in
  let elems = CCList.map (makeFloat precision) data in
  TopDecl ({ d = DId (varname, Some size); t }, { e = Array elems; t })


let generateRawAccessFunction full_name c t loc =
  let n = string_of_int c in
  let table_name = full_name ^ "_c" ^ n in
  let function_name = full_name ^ "_raw_c" ^ n in
  let r = { e = Index { e = { e = Id table_name; t }; index = { e = Id "index"; t = Int } }; t } in
  let body = StmtReturn r in
  let args = [ "index", (Int : type_) ] in
  let t = [ (Int : type_) ], t in
  TopFunction ({ name = function_name; args; t; tags = []; loc }, body)


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
  else
    let p1 = data.(index) in
    let p2 = data.(index + 1) in
    let x = [ fst p1; fst p2 ] in
    let y = [ snd p1; snd p2 ] in
    let c0, c1 = Fitting.lagrange x y |> getCoefficients1 in
    fitDataOrder1 data (index - 1) (c0 :: acc0) (c1 :: acc1)


let rec fitDataOrder2 data index acc0 acc1 acc2 =
  if index < 0 then
    acc0, acc1, acc2
  else
    let p1 = data.(index * 2) in
    let p2 = data.((index * 2) + 1) in
    let p3 = data.((index * 2) + 2) in
    let x = [ fst p1; fst p2; fst p3 ] in
    let y = [ snd p1; snd p2; snd p3 ] in
    let c0, c1, c2 = Fitting.lagrange x y |> getCoefficients2 in
    fitDataOrder2 data (index - 1) (c0 :: acc0) (c1 :: acc1) (c2 :: acc2)


let evaluateFunction vm name x =
  match Interpreter.callFunction vm name Vmv.[ Real x ] with
  | Real y -> y
  | _ -> failwith "Failed to evaluate during fitting"


let calculateTablesOrder1 vm name size min max precision =
  let map x x0 x1 y0 y1 = ((x -. x0) *. (y1 -. y0) /. (x1 -. x0)) +. y0 in
  let map_x x = map x 0. (float_of_int size) min max in
  let data =
    Array.init (size + 1) (fun i ->
        let x = map_x (float_of_int i) in
        x, evaluateFunction vm name x)
  in
  let acc0, acc1 = fitDataOrder1 data (size - 1) [] [] in
  [ makeDecl name "c0" precision acc0; makeDecl name "c1" precision acc1 ]


let calculateTablesOrder2 vm name size min max precision =
  let map x x0 x1 y0 y1 = ((x -. x0) *. (y1 -. y0) /. (x1 -. x0)) +. y0 in
  let map_x x = map x 0. (float_of_int size) min max in
  let data =
    Array.init
      ((size * 2) + 2)
      (fun i ->
        let x = map_x (float_of_int i /. 2.0) in
        x, evaluateFunction vm name x)
  in
  let acc0, acc1, acc2 = fitDataOrder2 data (size - 1) [] [] [] in
  [ makeDecl name "c0" precision acc0; makeDecl name "c1" precision acc1; makeDecl name "c2" precision acc2 ]


let getIndex bound_check size value =
  let clip_call i =
    if bound_check then
      { e = Call { path = "clip"; args = [ i; { e = Int 0; t = Int }; { e = Int (size - 1); t = Int } ] }; t = Int }
    else
      i
  in
  let int_call i = { e = Call { path = "int"; args = [ i ] }; t = Int } in
  [ StmtDecl ({ d = DId ("index", None); t = Int }, Some (clip_call (int_call value))) ]


let castInputVarPrecision (in_precision : type_) (out_precision : type_) (input : exp) : exp =
  match in_precision, out_precision with
  | Real, Real -> input
  | Fixed, Fixed -> input
  | _ ->
      (*| Real, Fixed -> PCall (NoInst, [ "Fixed" ], [ input ], Tables.attr_real out_precision)
           | Fixed, Real -> PCall (NoInst, [ "real" ], [ input ], Tables.attr_real out_precision)
      *)
      failwith "castInputVarPrecision: invalid input"


let makeNewBody1 bound_check fname size in_precision t min max input =
  let atype = makeArrayType t size in
  let rindex = { e = Id "index"; t = Int } in
  let getCoeff a =
    let arr = { e = Call { path = "wrap_array"; args = [ { e = Id (fname ^ "_" ^ a); t = atype } ] }; t = atype } in
    { e = Index { e = arr; index = rindex }; t }
  in
  let initial_index = { e = Real ((float_of_int size -. 1.0) /. (max -. min)); t = in_precision } in
  let value = { e = Op (Mul, initial_index, { e = Op (Sub, input, { e = Real min; t }); t }); t } in
  let index_stmts = getIndex bound_check size value in
  let input = castInputVarPrecision in_precision t input in
  let return = StmtReturn { e = Op (Add, getCoeff "c0", { e = Op (Mul, input, getCoeff "c1"); t }); t } in
  StmtBlock (index_stmts @ [ return ])


let makeNewBody2 bound_check fname size in_precision t min max input =
  let atype = makeArrayType t size in
  let rindex = { e = Id "index"; t = Int } in
  let getCoeff a =
    let arr = { e = Call { path = "wrap_array"; args = [ { e = Id (fname ^ "_" ^ a); t = atype } ] }; t = atype } in
    { e = Index { e = arr; index = rindex }; t }
  in
  let initial_index = { e = Real ((float_of_int size -. 1.0) /. (max -. min)); t = in_precision } in
  let value = { e = Op (Mul, initial_index, { e = Op (Sub, input, { e = Real min; t }); t }); t } in
  let index_stmts = getIndex bound_check size value in
  let input = castInputVarPrecision in_precision t input in
  let k2 = { e = Op (Mul, getCoeff "c2", input); t } in
  let k1 = { e = Op (Mul, input, { e = Op (Add, getCoeff "c1", k2); t }); t } in
  let return = StmtReturn { e = Op (Add, getCoeff "c0", k1); t } in
  StmtBlock (index_stmts @ [ return ])


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
  match Tags.getParameterList def.tags "table" params with
  | Tags.[ Some (Int size); Some (Real min); Some (Real max); order; bound_check ] ->
      let bound_check = getBoundCheckValue bound_check in
      let out_precision = snd def.t in
      let var = checkInputVariables def.loc def.args in
      let in_precision = var.t in
      ( match order with
      | Some (Int 1) ->
          let result = calculateTablesOrder1 vm def.name size min max out_precision in
          let new_body = makeNewBody1 bound_check def.name size in_precision out_precision min max var in
          let c0 = generateRawAccessFunction def.name 0 out_precision def.loc in
          let c1 = generateRawAccessFunction def.name 1 out_precision def.loc in
          result @ [ c0; c1 ] @ [ TopFunction (def, new_body) ]
      | _ ->
          let result = calculateTablesOrder2 vm def.name size min max out_precision in
          let new_body = makeNewBody2 bound_check def.name size in_precision out_precision min max var in
          let c0 = generateRawAccessFunction def.name 0 out_precision def.loc in
          let c1 = generateRawAccessFunction def.name 1 out_precision def.loc in
          let c2 = generateRawAccessFunction def.name 2 out_precision def.loc in
          result @ [ c0; c1; c2 ] @ [ TopFunction (def, new_body) ] )
  | _ ->
      let _msg =
        "The attribute 'table' requires specific parameters. e.g. 'table(size=128,min=0.0,max=1.0,[order=2])'"
      in
      failwith ""


let replaceFunction vm stmt =
  match stmt with
  | TopFunction (def, _) when Tags.has def.tags "table" -> makeTable vm def
  | _ -> [ stmt ]


let create vm stmts = CCList.flat_map (replaceFunction vm) stmts
