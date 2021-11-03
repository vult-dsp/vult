open Pparser.Syntax

let loc = Util.Loc.default

module TypeMap = Map.Make (struct
  type t = type_

  let compare = compare
end)

type state =
  { max_array_size : int
  ; max_tuple_size : int
  ; max_type_levels : int
  ; max_int : int
  ; max_real : float
  ; nest_prob : float
  ; get_array_type : bool
  ; get_tuple_type : bool
  ; get_if_exp : bool
  ; vars : string list TypeMap.t
  ; return_type : type_ option
  }

type condition = state -> bool

type probability = state -> float

type 'a creator = state -> 'a

let default_state =
  { max_array_size = 100
  ; max_tuple_size = 3
  ; max_int = 100
  ; max_real = 1.0
  ; get_array_type = true
  ; get_tuple_type = true
  ; max_type_levels = 2
  ; nest_prob = 0.7
  ; vars = TypeMap.empty
  ; get_if_exp = true
  ; return_type = None
  }


let rec fold_init f s n =
  if n > 0 then
    let s', e = f s in
    let s', e' = fold_init f s' (n - 1) in
    s', e :: e'
  else
    s, []


let rec get_elem state min max (n : float) (elems : (condition * probability * 'a creator) list) =
  match elems with
  | h :: ((_, p, _) :: _ as t) ->
      if n >= min && n <= max then
        h
      else
        get_elem state max (max +. p state) n t
  | h :: _ ->
      if n >= min && n <= max then
        h
      else
        failwith "get_elem: invalid input"
  | [] -> failwith "get_elem: invalid input"


let rec filter_count state acc l =
  match l with
  | [] ->
      let n, e = acc in
      n, List.rev e
  | ((c, p, _) as h) :: t ->
      let n, e = acc in
      if c state then
        let prob = p state in
        if prob > 0.0 then
          filter_count state (n +. prob, h :: e) t
        else
          filter_count state acc t
      else
        filter_count state acc t


let pick_one state (elems : (condition * probability * 'a creator) list) : 'a =
  let total, elems' = filter_count state (0.0, []) elems in
  let n = Random.float total in
  let _, first_p, _ = List.hd elems' in
  let _, _, f = get_elem state 0.0 (first_p state) n elems' in
  f state


let makeArray state t =
  let n = Random.int (state.max_array_size - 1) + 1 in
  let size = { t = STSize n; loc } in
  { t = STComposed ("array", [ t; size ]); loc }


let int_type = { t = STId { id = "int"; n = None; loc }; loc }

let real_type = { t = STId { id = "real"; n = None; loc }; loc }

let bool_type = { t = STId { id = "bool"; n = None; loc }; loc }

let makeTuple elems = { t = STComposed ("tuple", elems); loc }

let normal_p _ = 1.0

let high_p _ = 2.0

let low_p _ = 0.3

let nest_p state = state.nest_prob

let always _ = true

let with_array state = state.get_array_type

let with_if_exp state = state.get_if_exp

let with_tuple state = state.get_tuple_type && state.max_type_levels > 0

let no_array state = { state with get_array_type = false }

let no_tuple state = { state with get_tuple_type = false }

let no_if_exp state = { state with get_if_exp = false }

let decr_level state = { state with max_type_levels = state.max_type_levels - 1 }

let decr_nest state = { state with nest_prob = state.nest_prob *. 0.5 }

let rec newType state =
  pick_one
    state
    [ (always, normal_p, fun _ -> int_type)
    ; (always, normal_p, fun _ -> real_type)
    ; (always, normal_p, fun _ -> bool_type)
    ; (* Array *)
      ( with_array
      , low_p
      , fun state ->
          let state' = no_tuple (no_array state) in
          let t = newType state' in
          makeArray state' t )
      (*; (* Tuple *)
        ( with_tuple
        , low_p
        , fun state ->
            let nelems = 2 + Random.int (state.max_tuple_size - 2) in
            let state' = decr_level (no_array state) in
            let t = newTypeList nelems state' in
            makeTuple t )*)
    ]


and newTypeList n state =
  if n = 0 then
    []
  else
    newType state :: newTypeList (n - 1) state


let isInt typ _ = compare int_type typ = 0

let isReal typ _ = compare real_type typ = 0

let isBool typ _ = compare bool_type typ = 0

let isNum typ state = isReal typ state || isInt typ state

let isArray (typ : type_) _ =
  match typ with
  | { t = STComposed ("array", _); _ } -> true
  | _ -> false


let arrayTypeAndSize (typ : type_) =
  match typ with
  | { t = STComposed ("array", [ t; { t = STSize n; _ } ]); _ } -> t, n
  | _ -> failwith "not an array"


let isArrayOfType (subtype : type_) (typ : type_) =
  match typ with
  | { t = STComposed ("array", [ t1; { t = STSize _; _ } ]); _ } -> compare subtype t1 = 0
  | _ -> false


let isTuple (typ : type_) _ =
  match typ with
  | { t = STComposed ("tuple", _); _ } -> true
  | _ -> false


let tupleTypes (typ : type_) =
  match typ with
  | { t = STComposed ("tuple", elems); _ } -> elems
  | _ -> failwith "tupleTypes: invalid input"


let pickVar state typ =
  let elems = TypeMap.find typ state.vars in
  let size = List.length elems in
  let n = Random.int size in
  List.nth elems n


let hasType typ state = TypeMap.mem typ state.vars

let hasArrayType typ state = TypeMap.exists (fun key _ -> isArrayOfType typ key) state.vars

let pickArrayVar state typ =
  TypeMap.bindings state.vars
  |> List.filter (fun (key, _) -> isArrayOfType typ key)
  |> List.map (fun (key, names) -> List.map (fun n -> key, n) names)
  |> List.flatten
  |> List.map (fun x -> always, normal_p, fun _ -> x)
  |> pick_one state


let hasVar state name = TypeMap.exists (fun _ names -> List.exists (fun n -> n = name) names) state.vars

let addVar state var typ =
  match TypeMap.find typ state.vars with
  | elems ->
      let vars = TypeMap.add typ (var :: elems) state.vars in
      { state with vars }
  | exception Not_found ->
      let vars = TypeMap.add typ [ var ] state.vars in
      { state with vars }


let newNumBiOp state =
  pick_one
    state
    [ (always, normal_p, fun _ -> "+")
    ; (always, normal_p, fun _ -> "-")
    ; (always, normal_p, fun _ -> "*")
    ; (always, normal_p, fun _ -> "/")
    ; (always, normal_p, fun _ -> "%")
    ]


let newBoolBiOp state = pick_one state [ (always, normal_p, fun _ -> "&&"); (always, normal_p, fun _ -> "||") ]

let newLogicBiOp state =
  pick_one
    state
    [ (always, normal_p, fun _ -> ">")
    ; (always, normal_p, fun _ -> "<")
    ; (always, normal_p, fun _ -> ">=")
    ; (always, normal_p, fun _ -> "<=")
    ; (always, normal_p, fun _ -> "==")
    ; (always, normal_p, fun _ -> "<>")
    ]


let newBuiltinFun state =
  pick_one
    state
    [ (always, normal_p, fun _ -> { id = "abs"; n = None; loc })
    ; (always, normal_p, fun _ -> { id = "exp"; n = None; loc })
    ; (always, normal_p, fun _ -> { id = "sin"; n = None; loc })
    ; (always, normal_p, fun _ -> { id = "cos"; n = None; loc })
    ; (always, normal_p, fun _ -> { id = "floor"; n = None; loc })
    ; (always, normal_p, fun _ -> { id = "tanh"; n = None; loc })
    ]


let newBuiltinFunNoArgs state =
  pick_one
    state
    [ (always, normal_p, fun _ -> { id = "eps"; n = None; loc })
    ; (always, normal_p, fun _ -> { id = "pi"; n = None; loc })
    ]


let rec newExp state typ : exp =
  pick_one
    state
    [ (isInt typ, normal_p, fun state -> { e = SEInt (Random.int state.max_int + 1); loc })
    ; (isReal typ, normal_p, fun state -> { e = SEReal (Random.float state.max_real +. 1.0); loc })
    ; (isBool typ, low_p, fun _ -> { e = SEBool (Random.bool ()); loc })
    ; (hasType typ, high_p, fun state -> { e = SEId (pickVar state typ); loc })
    ; (* literal array *)
      ( isArray typ
      , low_p
      , fun state ->
          let array_type, size = arrayTypeAndSize typ in
          let elems = newExpList size state array_type in
          { e = SEArray elems; loc } )
    ; (* call real builtin *)
      ( isReal typ
      , nest_p
      , fun state ->
          let state' = decr_nest state in
          let elem = newExp state' typ in
          let path = newBuiltinFun state' in
          { e = SECall { instance = None; path; args = [ elem ] }; loc } )
    ; (* call real builtin no args *)
      ( isReal typ
      , nest_p
      , fun state ->
          let state' = decr_nest state in
          let path = newBuiltinFunNoArgs state' in
          { e = SECall { instance = None; path; args = [] }; loc } )
    ; (* call to get array*)
      ( hasArrayType typ
      , nest_p
      , fun state ->
          let array_type, var = pickArrayVar state typ in
          let _, size = arrayTypeAndSize array_type in
          let index = Random.int size in
          { e = SEGroup { e = SEIndex { e = { e = SEId var; loc }; index = { e = SEInt index; loc } }; loc }; loc } )
    ; (* tuples *)
      ( isTuple typ
      , low_p
      , fun state ->
          let types = tupleTypes typ in
          let elems = List.map (newExp state) types in
          { e = SEGroup { e = SETuple elems; loc }; loc } )
    ; (* operators *)
      ( isNum typ
      , nest_p
      , fun state ->
          let state' = decr_nest state in
          let e1 = newExp state' typ in
          let e2 = newExp state' typ in
          let op = newNumBiOp state' in
          { e = SEGroup { e = SEOp (op, e1, e2); loc }; loc } )
    ; ( isBool typ
      , nest_p
      , fun state ->
          let state' = decr_nest state in
          let e1 = newExp state' typ in
          let e2 = newExp state' typ in
          let op = newBoolBiOp state' in
          { e = SEGroup { e = SEOp (op, e1, e2); loc }; loc } )
    ; ( isBool typ
      , nest_p
      , fun state ->
          let state' = decr_nest state in
          let t = newType state' in
          if isNum t state then
            let e1 = newExp state' t in
            let e2 = newExp state' t in
            let op = newLogicBiOp state' in
            { e = SEGroup { e = SEOp (op, e1, e2); loc }; loc }
          else
            newExp state typ )
    ; (* if-expression *)
      ( with_if_exp
      , nest_p
      , fun state ->
          let cond = newExp state bool_type in
          let state' = decr_nest state in
          let then_ = newExp state' typ in
          let else_ = newExp state' typ in
          { e = SEGroup { e = SEIf { cond; then_; else_ }; loc }; loc } )
    ]


and newExpList n state typ =
  if n = 0 then
    []
  else
    newExp state typ :: newExpList (n - 1) state typ


let rec getName state =
  let random_char _ =
    let n = Random.int (Char.code 'z' - Char.code 'a') + Char.code 'a' in
    Char.chr n
  in
  let number = String.init 10 random_char in
  let name = "tmp_" ^ number in
  if hasVar state name then
    getName state
  else
    name


let rec newDExpDecl state typ =
  pick_one
    state
    [ (* new variable *)
      ( always
      , normal_p
      , fun state ->
          let name = getName state in
          let state' = addVar state name typ in
          { d = SDId (name, None); loc }, state' )
    ; (* wild *)
      (always, low_p, fun state -> { d = SDGroup { d = SDTyped ({ d = SDWild; loc }, typ); loc }; loc }, state)
    ; (* tuple *)
      ( isTuple typ
      , normal_p
      , fun state ->
          let types = tupleTypes typ in
          let state', lhs_elems =
            List.fold_left
              (fun (s, acc) t ->
                let t', s' = newDExpDecl s t in
                s', t' :: acc)
              (state, [])
              types
          in
          { d = SDGroup { d = SDTuple (List.rev lhs_elems); loc }; loc }, state' )
    ]


let rec newLExpBind state typ =
  pick_one
    state
    [ (* pick variable *)
      ( hasType typ
      , high_p
      , fun state ->
          let name = pickVar state typ in
          { l = SLId name; loc }, state )
    ; (* wild *)
      (always, low_p, fun state -> { l = SLWild; loc }, state)
    ; (* tuple *)
      ( isTuple typ
      , normal_p
      , fun state ->
          let types = tupleTypes typ in
          let state', lhs_elems =
            List.fold_left
              (fun (s, acc) t ->
                let t', s' = newLExpBind s t in
                s', t' :: acc)
              (state, [])
              types
          in
          { l = SLGroup { l = SLTuple (List.rev lhs_elems); loc }; loc }, state' )
    ]


let returnType state =
  match state.return_type with
  | Some t -> state, t
  | _ ->
      let t = newType state in
      let state' = { state with return_type = Some t } in
      state', t


let restoreVars new_vars old = { new_vars with vars = old.vars }

let rec newStmt state =
  pick_one
    state
    [ (* val *)
      ( always
      , normal_p
      , fun state ->
          let t = newType state in
          let dhs, state' = newDExpDecl state t in
          (* here use the old state to not pick the new variable *)
          let rhs = newExp state t in
          state', { s = SStmtVal (dhs, Some rhs); loc } )
    ; ( always
      , low_p
      , fun state ->
          let t = newType state in
          let lhs, state' = newDExpDecl state t in
          let lhs' = { d = SDTyped (lhs, t); loc } in
          state', { s = SStmtVal (lhs', None); loc } )
    ; (* mem *)
      ( always
      , normal_p
      , fun state ->
          let t = newType state in
          let lhs, state' = newDExpDecl state t in
          (* here use the new state to make possible picking the new variable *)
          let rhs = newExp state' t in
          let lhs' = { d = SDTyped (lhs, t); loc } in
          state', { s = SStmtMem (lhs', Some rhs, []); loc } )
    ; ( always
      , low_p
      , fun state ->
          let t = newType state in
          let lhs, state' = newDExpDecl state t in
          (* here use the new state to make possible picking the new variable *)
          let lhs' = { d = SDTyped (lhs, t); loc } in
          state', { s = SStmtMem (lhs', None, []); loc } )
    ; (* bind *)
      ( always
      , normal_p
      , fun state ->
          let t = newType state in
          if hasType t state then
            let lhs, state' = newLExpBind state t in
            (* here use the new state to make possible picking the new variable *)
            let rhs = newExp state' t in
            state', { s = SStmtBind (lhs, rhs); loc }
          else
            newStmt state )
    ; (* return *)
      ( always
      , low_p
      , fun state ->
          let state', t = returnType state in
          let e = newExp state t in
          state', { s = SStmtReturn e; loc } )
    ; (* if statements *)
      ( always
      , low_p
      , fun state ->
          let cond = newExp state bool_type in
          let state', e1 = newStmtList 3 state in
          let state', e2 = newStmtList 3 (restoreVars state' state) in
          let state' = restoreVars state' state in
          state', { s = SStmtIf (cond, { s = SStmtBlock e1; loc }, Some { s = SStmtBlock e2; loc }); loc } )
    ; ( always
      , low_p
      , fun state ->
          let cond = newExp state bool_type in
          let state', e1 = newStmtList 3 state in
          let state' = restoreVars state' state in
          state', { s = SStmtIf (cond, { s = SStmtBlock e1; loc }, None); loc } )
    ; ( always
      , low_p
      , fun state ->
          let cond = newExp state bool_type in
          let state', e1 = newStmtList 3 state in
          let state' = restoreVars state' state in
          state', { s = SStmtWhile (cond, { s = SStmtBlock e1; loc }); loc } )
    ]


and newStmtList n state : state * stmt list = fold_init newStmt state n

let newFunction () =
  let name = "foo_" ^ string_of_int (Random.int 10000) in
  let _, stmts = newStmtList 4 default_state in
  let def = { next = None; name; args = []; t = None; loc; tags = [] } in
  let body = { s = SStmtBlock stmts; loc } in
  { top = STopFunction (def, body); loc }


let test seed =
  Random.init seed ;
  let stmts = [ newFunction () ] in
  let code = Print.print stmts in
  code


let run seed = test seed
