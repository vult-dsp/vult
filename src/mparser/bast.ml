(* Constructor functions for the AST types *)

open Pparser.Syntax
open Pparser.Ptags

type state = string

let list : ('state -> 'state * 'value) list -> 'state -> 'state * 'value list =
 fun elems state ->
  let state, r =
    CCList.fold_left
      (fun (state, acc) e ->
        let state, v = e state in
        state, v :: acc)
      (state, [])
      elems
  in
  state, CCList.rev r


let olist : ('state -> 'state * 'value) list option -> 'state -> 'state * 'value list =
 fun elems state ->
  match elems with
  | None -> state, []
  | Some elems ->
    let state, r =
      CCList.fold_left
        (fun (state, acc) e ->
          let state, v = e state in
          state, v :: acc)
        (state, [])
        elems
    in
    state, CCList.rev r


let option : ('state -> 'state * 'value) option -> 'state -> 'state * 'value option =
 fun v state ->
  match v with
  | None -> state, None
  | Some v ->
    let state, v = v state in
    state, Some v


let bypass state e = state, e

(* Helper function to convert Menhir location to Util.Loc.t *)
let mk_loc file (start_pos, end_pos) = { Util.Loc.start_pos; end_pos; source = Util.Loc.File file }

(* Constructor functions for path type *)
let path_Path (loc : Lexing.position * Lexing.position) (ids : string list) (state : 'state) : 'state * path =
  match ids with
  | [ single_id ] -> state, { id = single_id; n = None; loc = mk_loc state loc }
  | [ first; second ] -> state, { id = first; n = Some second; loc = mk_loc state loc }
  | _ -> failwith "path_Path"


(* Constructor functions for type_expr type *)
let type_expr_TypeId (loc : Lexing.position * Lexing.position) (p : 'state -> 'state * path) (state : 'state) :
    'state * type_ =
  let state, p = p state in
  state, { t = STId p; loc = mk_loc state loc }


let type_expr_TypeSize (loc : Lexing.position * Lexing.position) (size : string) (state : 'state) : 'state * type_ =
  state, { t = STSize (int_of_string size); loc = mk_loc state loc }


let type_expr_TypeComposed (loc : Lexing.position * Lexing.position) (p : 'state -> 'state * path)
    (types : ('state -> 'state * type_) list) (state : 'state) : 'state * type_ =
  let state, p = p state in
  let state, types = list types state in
  state, { t = STComposed (p.id, types); loc = mk_loc state loc }


(* Constructor functions for tag_exp type *)
let tag_exp_TagExpBool (loc : Lexing.position * Lexing.position) (b : bool) (state : 'state) : 'state * tag =
  state, { g = TagBool b; loc = mk_loc state loc }


let tag_exp_TagExpInt (loc : Lexing.position * Lexing.position) (s : string) (state : 'state) : 'state * tag =
  state, { g = TagInt (int_of_string s); loc = mk_loc state loc }


let tag_exp_TagExpXInt (loc : Lexing.position * Lexing.position) (s : string) (state : 'state) : 'state * tag =
  state, { g = TagInt (int_of_string s); loc = mk_loc state loc }


let tag_exp_TagExpReal (loc : Lexing.position * Lexing.position) (s : string) (state : 'state) : 'state * tag =
  state, { g = TagReal (float_of_string s); loc = mk_loc state loc }


let tag_exp_TagExpFixed (loc : Lexing.position * Lexing.position) (s : string) (state : 'state) : 'state * tag =
  state, { g = TagReal (float_of_string s); loc = mk_loc state loc }


let tag_exp_TagExpString (loc : Lexing.position * Lexing.position) (s : string) (state : 'state) : 'state * tag =
  state, { g = TagString s; loc = mk_loc state loc }


let tag_exp_TagExpCall (_loc : Lexing.position * Lexing.position) (call : 'state -> 'state * tag) (state : 'state) :
    'state * tag =
  let state, call = call state in
  state, call


let tag_exp_TagExpUop (loc : Lexing.position * Lexing.position) (op : string) (exp : 'state -> 'state * tag)
    (state : 'state) : 'state * tag =
  let state, exp = exp state in
  match op, exp.g with
  | "-", TagInt i -> state, { g = TagInt (-i); loc = mk_loc state loc }
  | "-", TagReal r -> state, { g = TagReal (-.r); loc = mk_loc state loc }
  | _ -> state, exp


(* Constructor functions for tag_arg type *)
let tag_arg_TagArg (loc : Lexing.position * Lexing.position) (name : string) (exp : 'state -> 'state * tag)
    (state : 'state) : 'state * (string * tag * Util.Loc.t) =
  let state, exp = exp state in
  state, (name, exp, mk_loc state loc)


(* Constructor functions for tag_call type *)
let tag_call_TagCall (loc : Lexing.position * Lexing.position) (name : string)
    (args : ('state -> 'state * (string * tag * Util.Loc.t)) list option) (state : 'state) : 'state * tag =
  let state, args = olist args state in
  let tag =
    match args with
    | [] -> { g = TagId name; loc = mk_loc state loc }
    | _ -> { g = TagCall { name; args }; loc = mk_loc state loc }
  in
  state, tag


(* Constructor functions for tag type *)
let tag_Tag (_loc : Lexing.position * Lexing.position) (calls : ('state -> 'state * tag) list) (state : 'state) :
    'state * tags =
  let state, calls = list calls state in
  state, calls


(* Constructor functions for dexp type *)
let dexp_DexpWild (loc : Lexing.position * Lexing.position) (state : 'state) : 'state * dexp =
  state, { d = SDWild; loc = mk_loc state loc }


let dexp_DexpId (loc : Lexing.position * Lexing.position) (id : string) (state : 'state) : 'state * dexp =
  state, { d = SDId (id, None); loc = mk_loc state loc }


let dexp_DexpArray (loc : Lexing.position * Lexing.position) (id : string) (size : string) (state : 'state) :
    'state * dexp =
  state, { d = SDId (id, Some (int_of_string size)); loc = mk_loc state loc }


let dexp_DexpCons (loc : Lexing.position * Lexing.position) (left : 'state -> 'state * dexp)
    (right : 'state -> 'state * dexp) (state : 'state) : 'state * dexp =
  let state, left = left state in
  let state, right = right state in
  let elems =
    match right with
    | { d = SDTuple eright; _ } -> left :: eright
    | _ -> left :: [ right ]
  in
  state, { d = SDTuple elems; loc = mk_loc state loc }


let dexp_DexpTyped (loc : Lexing.position * Lexing.position) (d : 'state -> 'state * dexp)
    (t : 'state -> 'state * type_) (state : 'state) : 'state * dexp =
  let state, d = d state in
  let state, t = t state in
  state, { d = SDTyped (d, t); loc = mk_loc state loc }


let dexp_DexpGroup (loc : Lexing.position * Lexing.position) (d : 'state -> 'state * dexp) (state : 'state) :
    'state * dexp =
  let state, d = d state in
  state, { d = SDGroup d; loc = mk_loc state loc }


(* Constructor functions for lexp type *)
let lexp_LexpWild (loc : Lexing.position * Lexing.position) (state : 'state) : 'state * lexp =
  state, { l = SLWild; loc = mk_loc state loc }


let lexp_LexpId (loc : Lexing.position * Lexing.position) (id : string) (state : 'state) : 'state * lexp =
  state, { l = SLId id; loc = mk_loc state loc }


let lexp_LexpCons (loc : Lexing.position * Lexing.position) (left : 'state -> 'state * lexp)
    (right : 'state -> 'state * lexp) (state : 'state) : 'state * lexp =
  let state, left = left state in
  let state, right = right state in
  let elems =
    match right with
    | { l = SLTuple eright; _ } -> left :: eright
    | _ -> left :: [ right ]
  in
  state, { l = SLTuple elems; loc = mk_loc state loc }


let lexp_LexpGroup (loc : Lexing.position * Lexing.position) (d : 'state -> 'state * lexp) (state : 'state) :
    'state * lexp =
  let state, d = d state in
  state, { l = SLGroup d; loc = mk_loc state loc }


let lexp_LexpIndex (loc : Lexing.position * Lexing.position) (lexp : 'state -> 'state * lexp)
    (exp : 'state -> 'state * exp) (state : 'state) : 'state * lexp =
  let state, lexp = lexp state in
  let state, exp = exp state in
  state, { l = SLIndex { e = lexp; index = exp }; loc = mk_loc state loc }


let lexp_LexpMember (loc : Lexing.position * Lexing.position) (lexp : 'state -> 'state * lexp) (member : string)
    (state : 'state) : 'state * lexp =
  let state, lexp = lexp state in
  state, { l = SLMember (lexp, member); loc = mk_loc state loc }


(* Constructor functions for exp type *)
let exp_ExpBool (loc : Lexing.position * Lexing.position) (b : bool) (state : 'state) : 'state * exp =
  state, { e = SEBool b; loc = mk_loc state loc }


let exp_ExpInt (loc : Lexing.position * Lexing.position) (s : string) (state : 'state) : 'state * exp =
  state, { e = SEInt s; loc = mk_loc state loc }


let exp_ExpXInt (loc : Lexing.position * Lexing.position) (s : string) (state : 'state) : 'state * exp =
  state, { e = SEInt s; loc = mk_loc state loc }


let exp_ExpReal (loc : Lexing.position * Lexing.position) (s : string) (state : 'state) : 'state * exp =
  state, { e = SEReal s; loc = mk_loc state loc }


let exp_ExpFixed (loc : Lexing.position * Lexing.position) (s : string) (state : 'state) : 'state * exp =
  state, { e = SEFixed s; loc = mk_loc state loc }


let exp_ExpString (loc : Lexing.position * Lexing.position) (s : string) (state : 'state) : 'state * exp =
  state, { e = SEString s; loc = mk_loc state loc }


let exp_ExpId (loc : Lexing.position * Lexing.position) (id : string) (state : 'state) : 'state * exp =
  state, { e = SEId id; loc = mk_loc state loc }


let exp_ExpGroup (loc : Lexing.position * Lexing.position) (e : 'state -> 'state * exp) (state : 'state) : 'state * exp
    =
  let state, e = e state in
  state, { e = SEGroup e; loc = mk_loc state loc }


let exp_ExpIndex (loc : Lexing.position * Lexing.position) (arr : 'state -> 'state * exp) (idx : 'state -> 'state * exp)
    (state : 'state) : 'state * exp =
  let state, arr = arr state in
  let state, idx = idx state in
  state, { e = SEIndex { e = arr; index = idx }; loc = mk_loc state loc }


let rec convertExpToCall func =
  match func with
  | { e = SEId id; loc } -> None, { id; n = None; loc }, false
  | { e = SEMember ({ e = SEId n; _ }, id); loc } -> None, { id; n = Some n; loc }, false
  | { e = SENamed ({ e = SEId instance; _ }, { e = SEId id; _ }); loc } ->
    Some (instance, None), { id; n = None; loc }, false
  | { e = SENamed ({ e = SEId instance; _ }, { e = SEMember ({ e = SEId n; _ }, id); _ }); loc } ->
    Some (instance, None), { id; n = Some n; loc }, false
  | { e = SENamed ({ e = SEIndex { e = { e = SEId instance; _ }; index }; _ }, { e = SEId id; _ }); loc } ->
    Some (instance, Some index), { id; n = None; loc }, false
  | { e = SEMember ({ e = SENamed ({ e = SEId instance; _ }, { e = SEId n; _ }); _ }, id); loc } ->
    Some (instance, None), { id; n = Some n; loc }, false
  | { e = SEUnOp ("-", func); _ } ->
    let instance, path, _ = convertExpToCall func in
    instance, path, true
  | { e = SEMember ({ e = SEUnOp ("-", { e = SEId n; _ }); _ }, id); loc } -> None, { id; n = Some n; loc }, true
  | _ -> failwith ("convertExpToCall: " ^ Pla.print (Pparser.Syntax.Print.exp func))


let exp_ExpCall (loc : Lexing.position * Lexing.position) (func : 'state -> 'state * exp)
    (args : ('state -> 'state * exp) option) (state : 'state) : 'state * exp =
  let state, func = func state in
  let state, args = option args state in
  let instance, path, neg = convertExpToCall func in
  let args_list =
    match args with
    | None -> []
    | Some { e = SETuple args; _ } -> args
    | Some a -> [ a ]
  in
  let call = { e = SECall { instance; path; args = args_list }; loc = mk_loc state loc } in
  let call =
    if neg then
      { e = SEUnOp ("-", call); loc = func.loc }
    else
      call
  in
  state, call


let exp_ExpRecord (loc : Lexing.position * Lexing.position) (record : 'state -> 'state * exp)
    (fields : (string * ('state -> 'state * exp)) list) (state : 'state) : 'state * exp =
  let state, record = record state in
  let state, fields =
    CCList.fold_left
      (fun (state, acc) (s, v) ->
        let state, v = v state in
        let path = { id = s; n = None; loc = mk_loc state loc } in
        state, (path, v) :: acc)
      (state, [])
      fields
  in
  let path =
    match record with
    | { e = SEId id; _ } -> { id; n = None; loc = mk_loc state loc }
    | _ -> { id = "unknown"; n = None; loc = mk_loc state loc }
  in
  state, { e = SERecord { path; elems = CCList.rev fields }; loc = mk_loc state loc }


let exp_ExpCons (loc : Lexing.position * Lexing.position) (left : 'state -> 'state * exp)
    (right : 'state -> 'state * exp) (state : 'state) : 'state * exp =
  let state, left = left state in
  let state, right = right state in
  let elems =
    match right with
    | { e = SETuple eright; _ } -> left :: eright
    | _ -> left :: [ right ]
  in
  state, { e = SETuple elems; loc = mk_loc state loc }


let exp_ExpArray (loc : Lexing.position * Lexing.position) (elements : 'state -> 'state * exp) (state : 'state) :
    'state * exp =
  let state, elements =
    match elements state with
    | state, { e = SETuple elements; _ } -> state, elements
    | state, e -> state, [ e ]
  in
  state, { e = SEArray elements; loc = mk_loc state loc }


let exp_ExpWithName (loc : Lexing.position * Lexing.position) (e : 'state -> 'state * exp)
    (name : 'state -> 'state * exp) (state : 'state) : 'state * exp =
  let state, e = e state in
  let state, name = name state in
  state, { e = SENamed (e, name); loc = mk_loc state loc }


let exp_ExpMember (loc : Lexing.position * Lexing.position) (e : 'state -> 'state * exp) (member : string)
    (state : 'state) : 'state * exp =
  let state, e = e state in
  state, { e = SEMember (e, member); loc = mk_loc state loc }


let exp_ExpBop (loc : Lexing.position * Lexing.position) (left : 'state -> 'state * exp) (op : string)
    (right : 'state -> 'state * exp) (state : 'state) : 'state * exp =
  let state, left = left state in
  let state, right = right state in
  state, { e = SEOp (op, left, right); loc = mk_loc state loc }


let exp_ExpUop (loc : Lexing.position * Lexing.position) (op : string) (e : 'state -> 'state * exp) (state : 'state) :
    'state * exp =
  let state, e = e state in
  state, { e = SEUnOp (op, e); loc = mk_loc state loc }


let exp_ExpIf (loc : Lexing.position * Lexing.position) (cond : 'state -> 'state * exp)
    (then_exp : 'state -> 'state * exp) (else_exp : 'state -> 'state * exp) (state : 'state) : 'state * exp =
  let state, cond = cond state in
  let state, then_exp = then_exp state in
  let state, else_exp = else_exp state in
  state, { e = SEIf { cond; then_ = then_exp; else_ = else_exp }; loc = mk_loc state loc }


(* Constructor functions for patt type *)
let patt_PattWild (loc : Lexing.position * Lexing.position) (state : 'state) : 'state * pattern =
  state, { p = SPWild; loc = mk_loc state loc }


let patt_PattInt (loc : Lexing.position * Lexing.position) (s : string) (state : 'state) : 'state * pattern =
  state, { p = SPInt s; loc = mk_loc state loc }


let patt_PattXInt (loc : Lexing.position * Lexing.position) (s : string) (state : 'state) : 'state * pattern =
  state, { p = SPInt s; loc = mk_loc state loc }


let patt_PattReal (loc : Lexing.position * Lexing.position) (s : string) (state : 'state) : 'state * pattern =
  state, { p = SPReal s; loc = mk_loc state loc }


let patt_PattFixed (loc : Lexing.position * Lexing.position) (s : string) (state : 'state) : 'state * pattern =
  state, { p = SPFixed s; loc = mk_loc state loc }


let patt_PattString (loc : Lexing.position * Lexing.position) (s : string) (state : 'state) : 'state * pattern =
  state, { p = SPString s; loc = mk_loc state loc }


let patt_PattId (loc : Lexing.position * Lexing.position) (p : 'state -> 'state * path) (state : 'state) :
    'state * pattern =
  let state, p = p state in
  state, { p = SPEnum p; loc = mk_loc state loc }


let patt_PattGroup (loc : Lexing.position * Lexing.position) (p : 'state -> 'state * pattern) (state : 'state) :
    'state * pattern =
  let state, p = p state in
  state, { p = SPGroup p; loc = mk_loc state loc }


let patt_PattCons (loc : Lexing.position * Lexing.position) (left : 'state -> 'state * pattern)
    (right : 'state -> 'state * pattern) (state : 'state) : 'state * pattern =
  let state, left = left state in
  let state, right = right state in
  let elems =
    match right with
    | { p = SPTuple eright; _ } -> left :: eright
    | _ -> left :: [ right ]
  in
  state, { p = SPTuple elems; loc = mk_loc state loc }


(* Constructor functions for match_case type *)
let match_case_MatchCase (_loc : Lexing.position * Lexing.position) (p : 'state -> 'state * pattern)
    (s : 'state -> 'state * stmt) (state : 'state) : 'state * (pattern * stmt) =
  let state, p = p state in
  let state, s = s state in
  state, (p, s)


(* Constructor functions for stmt type *)
let stmt_StmtVal (loc : Lexing.position * Lexing.position) (d : 'state -> 'state * dexp)
    (e : ('state -> 'state * exp) option) (state : 'state) : 'state * stmt =
  let state, d = d state in
  let state, e = option e state in
  state, { s = SStmtVal (d, e); loc = mk_loc state loc }


let stmt_StmtMem (loc : Lexing.position * Lexing.position) (d : 'state -> 'state * dexp)
    (e : ('state -> 'state * exp) option) (t : ('state -> 'state * tags) option) (state : 'state) : 'state * stmt =
  let state, d = d state in
  let state, e = option e state in
  let state, t = option t state in
  let tags =
    match t with
    | None -> []
    | Some tags -> tags
  in
  state, { s = SStmtMem (d, e, tags); loc = mk_loc state loc }


let stmt_StmtBind (loc : Lexing.position * Lexing.position) (l : 'state -> 'state * lexp) (e : 'state -> 'state * exp)
    (state : 'state) : 'state * stmt =
  let state, l = l state in
  let state, e = e state in
  state, { s = SStmtBind (l, e); loc = mk_loc state loc }


let stmt_StmtBlock (loc : Lexing.position * Lexing.position) (stmts : ('state -> 'state * stmt) list) (state : 'state) :
    'state * stmt =
  let state, s = list stmts state in
  state, { s = SStmtBlock s; loc = mk_loc state loc }


let stmt_StmtReturn (loc : Lexing.position * Lexing.position) (e : 'state -> 'state * exp) (state : 'state) :
    'state * stmt =
  let state, e = e state in
  state, { s = SStmtReturn e; loc = mk_loc state loc }


let makeBlock stmt =
  match stmt with
  | { s = SStmtBlock _; _ } -> stmt
  | _ -> { s = SStmtBlock [ stmt ]; loc = stmt.loc }


let stmt_StmtIf (loc : Lexing.position * Lexing.position) (cond : 'state -> 'state * exp)
    (then_stmt : 'state -> 'state * stmt) (else_stmt : ('state -> 'state * stmt) option) (state : 'state) :
    'state * stmt =
  let state, cond = cond state in
  let state, then_stmt = then_stmt state in
  let state, else_stmt = option else_stmt state in
  state, { s = SStmtIf (cond, makeBlock then_stmt, CCOption.map makeBlock else_stmt); loc = mk_loc state loc }


let stmt_StmtWhile (loc : Lexing.position * Lexing.position) (cond : 'state -> 'state * exp)
    (body : 'state -> 'state * stmt) (state : 'state) : 'state * stmt =
  let state, cond = cond state in
  let state, body = body state in
  state, { s = SStmtWhile (cond, makeBlock body); loc = mk_loc state loc }


let stmt_StmtIter (loc : Lexing.position * Lexing.position) (id : string) (e : 'state -> 'state * exp)
    (body : 'state -> 'state * stmt) (state : 'state) : 'state * stmt =
  let state, e = e state in
  let state, body = body state in
  state, { s = SStmtIter { id = id, mk_loc state loc; value = e; body = makeBlock body }; loc = mk_loc state loc }


let stmt_StmtCall (loc : Lexing.position * Lexing.position) (func : 'state -> 'state * exp)
    (args : ('state -> 'state * exp) option) (state : 'state) : 'state * stmt =
  let state, func = func state in
  let state, args = option args state in
  let instance, path, neg = convertExpToCall func in
  let args_list =
    match args with
    | None -> []
    | Some { e = SETuple args; _ } -> args
    | Some a -> [ a ]
  in
  let call = { e = SECall { instance; path; args = args_list }; loc = mk_loc state loc } in
  let call =
    if neg then
      { e = SEUnOp ("-", call); loc = func.loc }
    else
      call
  in
  state, { s = SStmtBind ({ l = SLWild; loc = mk_loc state loc }, call); loc = mk_loc state loc }


let stmt_StmtMatch (loc : Lexing.position * Lexing.position) (e : 'state -> 'state * exp)
    (cases : ('state -> 'state * (pattern * stmt)) list) (state : 'state) : 'state * stmt =
  let state, e = e state in
  let state, cases = list cases state in
  state, { s = SStmtMatch { e; cases }; loc = mk_loc state loc }


let stmt_StmtError (loc : Lexing.position * Lexing.position) (state : 'state) : 'state * stmt =
  state, { s = SStmtError; loc = mk_loc state loc }


(* Constructor functions for type_member type *)
let type_member_TypeMember (loc : Lexing.position * Lexing.position) (name : string) (t : 'state -> 'state * type_)
    (state : 'state) : 'state * (string * type_ * tags * Util.Loc.t) =
  let state, t = t state in
  state, (name, t, [], mk_loc state loc)


(* Constructor functions for fun_arg type *)
let fun_arg_FunArg (loc : Lexing.position * Lexing.position) (name : string) (t : ('state -> 'state * type_) option)
    (state : 'state) : 'state * arg =
  let state, t = option t state in
  state, (name, t, mk_loc state loc)


(* Constructor functions for ext_arg type *)
let ext_arg_ExtArg (loc : Lexing.position * Lexing.position) (name : string) (t : 'state -> 'state * type_)
    (state : 'state) : 'state * arg =
  let state, t = t state in
  state, (name, Some t, mk_loc state loc)


(* Constructor functions for fun_def type *)
let fun_def_FunDef (loc : Lexing.position * Lexing.position) (name : string) (args : ('state -> 'state * arg) list)
    (ret_type : ('state -> 'state * type_) option) (t : ('state -> 'state * tags) option)
    (body : 'state -> 'state * stmt) (state : 'state) : 'state * function_def =
  let state, args = list args state in
  let state, ret_type = option ret_type state in
  let state, t = option t state in
  let state, body = body state in
  let tags =
    match t with
    | None -> []
    | Some tags -> tags
  in
  state, { name; args; t = ret_type; next = None; loc = mk_loc state loc; tags; body }


(* Constructor functions for top_stmt type *)
let top_stmt_Fun (loc : Lexing.position * Lexing.position) (main : 'state -> 'state * function_def)
    (alts : ('state -> 'state * function_def) list) (state : 'state) : 'state * top_stmt =
  let state, main = main state in
  let state, alts = list alts state in
  (* Convert alternatives to linked function definitions *)
  let main_with_next = CCList.fold_left (fun next def -> Some { def with next }) None (CCList.rev (main :: alts)) in
  let def = CCOption.get_exn_or "Empty functions. This should not happen" main_with_next in
  (* placeholder body *)
  state, { top = STopFunction def; loc = mk_loc state loc }


let top_stmt_External (loc : Lexing.position * Lexing.position) (name : string) (args : ('state -> 'state * arg) list)
    (ret_type : 'state -> 'state * type_) (impl : string option) (t : ('state -> 'state * tags) option) (state : 'state)
    : 'state * top_stmt =
  let state, args = list args state in
  let state, ret_type = ret_type state in
  let state, t = option t state in
  let tags =
    match t with
    | None -> []
    | Some tags -> tags
  in
  let ext_def = { name; args; t = Some ret_type; loc = mk_loc state loc; tags } in
  state, { top = STopExternal (ext_def, impl); loc = mk_loc state loc }


let top_stmt_Constant (loc : Lexing.position * Lexing.position) (name : string) (e : 'state -> 'state * exp)
    (state : 'state) : 'state * top_stmt =
  let state, e = e state in
  let d = { d = SDId (name, None); loc = mk_loc state loc } in
  state, { top = STopConstant (d, e); loc = mk_loc state loc }


let top_stmt_TypeDef (loc : Lexing.position * Lexing.position) (name : string)
    (members : ('state -> 'state * (string * type_ * tags * Util.Loc.t)) list) (state : 'state) : 'state * top_stmt =
  let state, members = list members state in
  state, { top = STopType { name; members }; loc = mk_loc state loc }


let top_stmt_TypeDefEmpty (loc : Lexing.position * Lexing.position) (name : string) (state : 'state) : 'state * top_stmt
    =
  state, { top = STopType { name; members = [] }; loc = mk_loc state loc }


let top_stmt_Enum (loc : Lexing.position * Lexing.position) (name : string) (values : string list) (state : 'state) :
    'state * top_stmt =
  let members = CCList.map (fun v -> v, mk_loc state loc) values in
  state, { top = STopEnum { name; members }; loc = mk_loc state loc }


(* Constructor functions for program type *)
let program_Program (_loc : Lexing.position * Lexing.position) (stmts : ('state -> 'state * top_stmt) list)
    (state : 'state) : 'state * stmts =
  let state, stmts = list stmts state in
  state, stmts
