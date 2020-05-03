(*
   The MIT License (MIT)

   Copyright (c) 2020 Leonardo Laguna Ruiz

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
open Util
module Map = CCMap.Make (String)

module Compile = struct
  type env =
    { functions : int Map.t
    ; locals : int Map.t
    ; lcount : int
    ; fcount : int
    }

  let default_env = { locals = Map.empty; lcount = 0; functions = Map.empty; fcount = 0 }

  let addLocal env name = { env with locals = Map.add name env.lcount env.locals; lcount = env.lcount + 1 }

  type op =
    | OpAdd
    | OpSub
    | OpDiv
    | OpMul
    | OpMod
    | OpLand
    | OpLor
    | OpBor
    | OpBand
    | OpBxor
    | OpLsh
    | OpRsh
    | OpEq
    | OpNe
    | OpLt
    | OpLe
    | OpGt
    | OpGe

  type rvalue_d =
    | RVoid
    | RInt    of int
    | RReal   of float
    | RBool   of bool
    | RString of string
    | RRef    of int * string
    | RObject of rvalue array
    | ROp     of op * rvalue * rvalue
    | RNot    of rvalue
    | RNeg    of rvalue
    | RIf     of rvalue * rvalue * rvalue
    | RMember of rvalue * int * string
    | RCall   of int * string * rvalue list
    | RIndex  of rvalue * rvalue

  and rvalue =
    { r : rvalue_d
    ; loc : Loc.t
    }

  type lvalue_d =
    | LVoid
    | LRef    of int * string
    | LTuple  of lvalue array
    | LMember of lvalue * int * string
    | LIndex  of lvalue * rvalue

  and lvalue =
    { l : lvalue_d
    ; loc : Loc.t
    }

  type instr_d =
    | Store  of lvalue * rvalue
    | Return of rvalue
    | If     of rvalue * instr list * instr list
    | While  of rvalue * instr list

  and instr =
    { i : instr_d
    ; loc : Loc.t
    }

  type segment =
    | External
    | Function of
        { name : string
        ; body : instr list
        ; locals : int
        ; n_args : int
        }

  type segments = segment array [@@deriving show]

  type bytecode =
    { table : int Map.t
    ; code : segments
    }

  let list f (env : 'env) (l : 'e) =
    let env, i_rev =
      List.fold_left
        (fun (env, instr) d ->
          let env, i = f env d in
          env, i :: instr)
        (env, [])
        l
    in
    env, List.flatten (List.rev i_rev)


  let getIndex name elems =
    let rec loop elems index =
      match elems with
      | [] -> failwith "index not found"
      | (current, _, _) :: _ when current = name -> index
      | _ :: t -> loop t (index + 1)
    in
    loop elems 0


  let rec dexp_to_lexp (d : dexp) : lexp =
    let t = d.t in
    let loc = d.loc in
    match d.d with
    | DWild -> { l = LWild; t; loc }
    | DId (name, _) -> { l = LId name; t; loc }
    | DTuple l ->
        let l = List.map dexp_to_lexp l in
        { l = LTuple l; t; loc }


  let rec compile_dexp (env : env) d =
    match d.d with
    | DWild -> env
    | DId (name, _) -> addLocal env name
    | DTuple l -> List.fold_left compile_dexp env l


  let rec compile_lexp (env : env) (l : lexp) : lvalue =
    let loc = l.loc in
    match l.l with
    | LWild -> { l = LVoid; loc }
    | LId name ->
        let index = Map.find name env.locals in
        { l = LRef (index, name); loc }
    | LTuple l ->
        let l = List.map (compile_lexp env) l |> Array.of_list in
        { l = LTuple l; loc }
    | LMember (e, s) ->
        begin
          match e.t.t with
          | TStruct descr ->
              let index = getIndex s descr.members in
              let e = compile_lexp env e in
              { l = LMember (e, index, s); loc }
          | _ -> failwith "type error"
        end
    | LIndex { e; index } ->
        let e = compile_lexp env e in
        let index = compile_exp env index in
        { l = LIndex (e, index); loc }


  and compile_exp (env : env) e : rvalue =
    let loc = e.loc in
    match e.e with
    | EUnit -> { r = RVoid; loc }
    | EBool v -> { r = RBool v; loc }
    | EInt v -> { r = RInt v; loc }
    | EReal v -> { r = RReal v; loc }
    | EString v -> { r = RString v; loc }
    | EId id ->
        let index = Map.find id env.locals in
        { r = RRef (index, id); loc }
    | EOp (op, e1, e2) ->
        let e1 = compile_exp env e1 in
        let e2 = compile_exp env e2 in
        { r = makeOp op e1 e2; loc }
    | EIndex { e; index } ->
        let e1 = compile_exp env e in
        let e2 = compile_exp env index in
        { r = RIndex (e1, e2); loc }
    | EUnOp (UOpNeg, e) ->
        let e = compile_exp env e in
        { r = RNeg e; loc }
    | EUnOp (UOpNot, e) ->
        let e = compile_exp env e in
        { r = RNot e; loc }
    | EIf { cond; then_; else_ } ->
        let cond = compile_exp env cond in
        let then_ = compile_exp env then_ in
        let else_ = compile_exp env else_ in
        { r = RIf (cond, then_, else_); loc }
    | ETuple elems ->
        let elems = List.map (compile_exp env) elems in
        { r = RObject (Array.of_list elems); loc }
    | EArray elems ->
        let elems = List.map (compile_exp env) elems in
        { r = RObject (Array.of_list elems); loc }
    | EMember (e, m) ->
        begin
          match e.t.t with
          | TStruct descr ->
              let index = getIndex m descr.members in
              let e = compile_exp env e in
              { r = RMember (e, index, m); loc }
          | _ -> failwith "type error"
        end
    | ECall { path; args } ->
        let args = List.map (compile_exp env) args in
        ( match Map.find_opt path env.functions with
        | Some index -> { r = RCall (index, path, args); loc }
        | None ->
            print_endline ("Function not found " ^ path) ;
            { r = RVoid; loc } )


  and makeOp op e1 e2 =
    match op with
    | OpAdd -> ROp (OpAdd, e1, e2)
    | OpSub -> ROp (OpAdd, e1, e2)
    | OpMul -> ROp (OpMul, e1, e2)
    | OpDiv -> ROp (OpDiv, e1, e2)
    | OpMod -> ROp (OpMod, e1, e2)
    | OpLand -> ROp (OpLand, e1, e2)
    | OpLor -> ROp (OpLor, e1, e2)
    | OpBor -> ROp (OpBor, e1, e2)
    | OpBand -> ROp (OpBand, e1, e2)
    | OpBxor -> ROp (OpBxor, e1, e2)
    | OpLsh -> ROp (OpLsh, e1, e2)
    | OpRsh -> ROp (OpRsh, e1, e2)
    | OpEq -> ROp (OpEq, e1, e2)
    | OpNe -> ROp (OpNe, e1, e2)
    | OpLt -> ROp (OpLt, e1, e2)
    | OpLe -> ROp (OpLe, e1, e2)
    | OpGt -> ROp (OpGt, e1, e2)
    | OpGe -> ROp (OpGe, e1, e2)


  let rec compile_stmt (env : env) (stmt : stmt) =
    let loc = stmt.loc in
    match stmt.s with
    | StmtDecl lhs ->
        let env = compile_dexp env lhs in
        env, []
    | StmtBind (lhs, rhs) ->
        let lhs = compile_lexp env lhs in
        let rhs = compile_exp env rhs in
        env, [ { i = Store (lhs, rhs); loc } ]
    | StmtReturn e ->
        let e = compile_exp env e in
        env, [ { i = Return e; loc } ]
    | StmtBlock stmts ->
        let env, instr = list compile_stmt env stmts in
        env, instr
    | StmtIf (cond, then_, Some else_) ->
        let cond = compile_exp env cond in
        let env, then_ = compile_stmt env then_ in
        let env, else_ = compile_stmt env else_ in
        env, [ { i = If (cond, then_, else_); loc } ]
    | StmtIf (cond, then_, None) ->
        let cond = compile_exp env cond in
        let env, then_ = compile_stmt env then_ in
        env, [ { i = If (cond, then_, []); loc } ]
    | StmtWhile (cond, body) ->
        let cond = compile_exp env cond in
        let env, body = compile_stmt env body in
        env, [ { i = While (cond, body); loc } ]


  let getNOutputs (t : type_) =
    match t.t with
    | TVoid
     |TInt
     |TReal
     |TString
     |TBool
     |TFixed
     |TArray _
     |TStruct _ ->
        1
    | TTuple elems -> List.length elems


  let compile_top (env : env) (s : top_stmt) =
    match s.top with
    | TopExternal ({ name; _ }, _) ->
        let index = env.fcount in
        let functions = Map.add name index env.functions in
        let env = { env with functions; fcount = env.fcount + 1 } in
        env, [ External ]
    | TopType _ -> env, []
    | TopFunction ({ name; args; _ }, body) ->
        let index = env.fcount in
        let functions = Map.add name index env.functions in
        let env = { locals = Map.empty; lcount = 0; functions; fcount = env.fcount + 1 } in
        let env = List.fold_left (fun env (n, _, _) -> addLocal env n) env args in
        let env, body = compile_stmt env body in
        let n_args = List.length args in
        env, [ Function { name; body; locals = env.lcount - n_args; n_args } ]


  let compile stmts = list compile_top default_env stmts

  let print_op op =
    match op with
    | OpAdd -> Pla.string "+"
    | OpSub -> Pla.string "-"
    | OpMul -> Pla.string "*"
    | OpDiv -> Pla.string "/"
    | OpMod -> Pla.string "%"
    | OpLand -> Pla.string "&&"
    | OpLor -> Pla.string "||"
    | OpBor -> Pla.string "|"
    | OpBand -> Pla.string "&"
    | OpBxor -> Pla.string "^"
    | OpLsh -> Pla.string "<<"
    | OpRsh -> Pla.string ">>"
    | OpEq -> Pla.string "=="
    | OpNe -> Pla.string "<>"
    | OpLt -> Pla.string "<"
    | OpLe -> Pla.string "<="
    | OpGt -> Pla.string ">"
    | OpGe -> Pla.string ">="


  let rec print_rvalue r =
    match r.r with
    | RVoid -> Pla.string "()"
    | RInt n -> Pla.int n
    | RReal n -> Pla.float n
    | RBool n -> Pla.string (if n then "true" else "false")
    | RString s -> Pla.string_quoted s
    | RRef (n, s) -> {pla|[<#n#i>:<#s#s>]|pla}
    | RObject elems ->
        let elems = Pla.map_sep Pla.commaspace print_rvalue (Array.to_list elems) in
        {pla|{ <#elems#> }|pla}
    | RIndex (e, i) ->
        let e = print_rvalue e in
        let i = print_rvalue i in
        {pla|<#e#>[<#i#>]|pla}
    | RCall (i, s, args) ->
        let args = Pla.map_sep Pla.commaspace print_rvalue args in
        {pla|<#i#i>:<#s#s>(<#args#>)|pla}
    | RMember (e, i, s) ->
        let e = print_rvalue e in
        {pla|<#e#>.[<#i#i>:<#s#s>]|pla}
    | RNot e ->
        let e = print_rvalue e in
        {pla|not(<#e#>)|pla}
    | RNeg e ->
        let e = print_rvalue e in
        {pla|(-<#e#>)|pla}
    | RIf (cond, then_, else_) ->
        let cond = print_rvalue cond in
        let then_ = print_rvalue then_ in
        let else_ = print_rvalue else_ in
        {pla|(if <#cond#> then <#then_#> else <#else_#>)|pla}
    | ROp (op, e1, e2) ->
        let e1 = print_rvalue e1 in
        let e2 = print_rvalue e2 in
        let op = print_op op in
        {pla|<#e1#> <#op#> <#e2#>|pla}


  let rec print_lvalue (l : lvalue) =
    match l.l with
    | LVoid -> Pla.string "_"
    | LRef (n, s) -> {pla|[<#n#i>:<#s#s>]|pla}
    | LTuple elems ->
        let elems = Pla.map_sep Pla.commaspace print_lvalue (Array.to_list elems) in
        {pla|{ <#elems#> }|pla}
    | LMember (e, i, s) ->
        let e = print_lvalue e in
        {pla|<#e#>.[<#i#i>:<#s#s>]|pla}
    | LIndex (e, i) ->
        let e = print_lvalue e in
        let i = print_rvalue i in
        {pla|<#e#>[<#i#>]|pla}


  let rec print_instr (i : instr) =
    match i.i with
    | Store (lvalue, rvalue) ->
        let lvalue = print_lvalue lvalue in
        let rvalue = print_rvalue rvalue in
        {pla|<#lvalue#> <- <#rvalue#>|pla}
    | Return rvalue ->
        let rvalue = print_rvalue rvalue in
        {pla|return <#rvalue#>|pla}
    | If (cond, then_, else_) ->
        let cond = print_rvalue cond in
        let then_ = print_instr_list then_ in
        let else_ = print_instr_list else_ in
        {pla|if <#cond#><#then_#+>else<#else_#+>|pla}
    | While (cond, body) ->
        let cond = print_rvalue cond in
        let body = print_instr_list body in
        {pla|while <#cond#><#body#+>|pla}


  and print_instr_list stmts = Pla.map_sep_all Pla.newline print_instr stmts

  let print_segment (s : segment) =
    match s with
    | Function { name; body; locals; n_args } ->
        let body = print_instr_list body in
        {pla|function <#name#s> : args = <#n_args#i>, locals = <#locals#i><#body#+>|pla}
    | External -> Pla.string "external"


  let print_segments s = Pla.map_sep_all Pla.newline print_segment (Array.to_list s)

  let print_table t =
    let f (n, i) = {pla|<#i#i>: <#n#s>|pla} in
    let elems = List.sort (fun (_, n1) (_, n2) -> compare n1 n2) (Map.to_list t) in
    Pla.map_sep_all Pla.newline f elems


  let print_bytecode b =
    let table = print_table b.table in
    let segments = print_segments b.code in
    {pla|<#table#><#><#segments#>|pla}
end

module Eval = struct
  type rvalue =
    | Void
    | Int    of int
    | Real   of float
    | Bool   of bool
    | String of string
    | Ref    of int
    | Object of rvalue array

  type lvalue =
    | LVoid
    | LRef    of int
    | LTuple  of lvalue array
    | LMember of lvalue * int
    | LIndex  of lvalue * rvalue

  type vm =
    { stack : rvalue array
    ; mutable sp : int
    ; mutable frame : int
    ; table : int Map.t
    ; code : Compile.segment array
    }

  let rec print_value r : Pla.t =
    match r with
    | Void -> Pla.string "#"
    | Int n -> Pla.int n
    | Real n -> Pla.float n
    | Bool v -> Pla.string (if v then "true" else "false")
    | String s -> Pla.string_quoted s
    | Ref n -> {pla|ref(<#n#i>)|pla}
    | Object elems ->
        let elems =
          List.mapi
            (fun i e ->
              let e = print_value e in
              {pla|'<#i#i>': <#e#>|pla})
            (Array.to_list elems)
        in
        let elems = Pla.join_sep Pla.commaspace elems in
        {pla|{ <#elems#> }|pla}


  let print_stack (vm : vm) =
    let rec loop n =
      if n <= vm.sp then begin
        if vm.frame = n then print_string "->" else print_string "  " ;
        print_int n ;
        print_string " : " ;
        print_endline (Pla.print (print_value vm.stack.(n))) ;
        loop (n + 1)
      end
    in
    loop 0


  let new_vm (compiled : Compile.bytecode) =
    { stack = Array.init 1024 (fun _ -> Void); sp = -1; frame = 0; table = compiled.table; code = compiled.code }


  let numeric i f e1 e2 : rvalue =
    match e1, e2 with
    | Int n1, Int n2 -> Int (i n1 n2)
    | Real n1, Real n2 -> Real (f n1 n2)
    | _ -> failwith "numeric: argument mismatch"


  let relation i f e1 e2 : rvalue =
    match e1, e2 with
    | Int n1, Int n2 -> Bool (i n1 n2)
    | Real n1, Real n2 -> Bool (f n1 n2)
    | _ -> failwith "relation: argument mismatch"


  let logic f e1 e2 : rvalue =
    match e1, e2 with
    | Bool n1, Bool n2 -> Bool (f n1 n2)
    | _ -> failwith "logic: argument mismatch"


  let bitwise f e1 e2 : rvalue =
    match e1, e2 with
    | Int n1, Int n2 -> Int (f n1 n2)
    | _ -> failwith "bitwise: argument mismatch"


  let not e : rvalue =
    match e with
    | Int n -> Int (lnot n)
    | Bool n -> Bool (not n)
    | _ -> failwith "not: argument mismatch"


  let neg e : rvalue =
    match e with
    | Int n -> Int (-n)
    | Real n -> Real (-.n)
    | _ -> failwith "not: argument mismatch"


  let push (vm : vm) (value : rvalue) =
    vm.sp <- vm.sp + 1 ;
    vm.stack.(vm.sp) <- value


  let pop (vm : vm) : rvalue =
    let ret = vm.stack.(vm.sp) in
    vm.sp <- vm.sp - 1 ;
    ret


  let loadRef (vm : vm) n : rvalue = vm.stack.(vm.frame + n)

  let storeRef (vm : vm) n v = vm.stack.(vm.frame + n) <- v

  let storeRefObject (vm : vm) n i v =
    match vm.stack.(vm.frame + n) with
    | Object elems -> elems.(i) <- v
    | _ -> failwith "storeRefMember: invalid input"


  let pushArgs (vm : vm) (args : rvalue list) = List.iter (fun v -> push vm v) args

  let allocate (vm : vm) n = vm.sp <- vm.sp + n

  let eval_op (op : Compile.op) =
    match op with
    | OpAdd -> numeric ( + ) ( +. )
    | OpSub -> numeric ( - ) ( -. )
    | OpDiv -> numeric ( / ) ( /. )
    | OpMul -> numeric ( * ) ( *. )
    | OpMod -> numeric ( mod ) mod_float
    | OpEq -> relation ( = ) ( = )
    | OpNe -> relation ( <> ) ( <> )
    | OpLt -> relation ( < ) ( < )
    | OpGt -> relation ( > ) ( > )
    | OpLe -> relation ( <= ) ( <= )
    | OpGe -> relation ( >= ) ( >= )
    | OpLand -> logic ( && )
    | OpLor -> logic ( || )
    | OpBor -> bitwise ( lor )
    | OpBand -> bitwise ( land )
    | OpBxor -> bitwise ( lxor )
    | OpLsh -> bitwise ( lsl )
    | OpRsh -> bitwise ( lsr )


  let isTrue (cond : rvalue) =
    match cond with
    | Bool true -> true
    | Bool false -> false
    | _ -> failwith "invalid condition"


  let rec eval_rvalue (vm : vm) (r : Compile.rvalue) : rvalue =
    match r.r with
    | RVoid -> Void
    | RInt n -> Int n
    | RReal n -> Real n
    | RBool n -> Bool n
    | RString s -> String s
    | RRef (n, _) -> loadRef vm n
    | ROp (op, e1, e2) ->
        let e1 = eval_rvalue vm e1 in
        let e2 = eval_rvalue vm e2 in
        (eval_op op) e1 e2
    | RNeg e ->
        let e = eval_rvalue vm e in
        neg e
    | RNot e ->
        let e = eval_rvalue vm e in
        not e
    | RIf (cond, then_, else_) ->
        let cond = eval_rvalue vm cond in
        if isTrue cond then
          eval_rvalue vm then_
        else
          eval_rvalue vm else_
    | RObject elems ->
        let elems = Array.map (eval_rvalue vm) elems in
        Object elems
    | RIndex (e, index) ->
        let e = eval_rvalue vm e in
        let index = eval_rvalue vm index in
        begin
          match e, index with
          | Object elems, Int index -> elems.(index)
          | _ -> failwith "index not evaluated correctly"
        end
    | RCall (index, _, args) ->
        let args = List.map (eval_rvalue vm) args in
        eval_call vm index args
    | RMember (e, index, _) ->
        let e = eval_rvalue vm e in
        begin
          match e with
          | Object elems -> elems.(index)
          | _ -> failwith "member: not a struct"
        end


  and eval_lvalue (vm : vm) (l : Compile.lvalue) : lvalue =
    match l.l with
    | LVoid -> LVoid
    | LRef (n, _) -> LRef n
    | LTuple elems ->
        let elems = Array.map (eval_lvalue vm) elems in
        LTuple elems
    | LMember (e, m, _) ->
        let e = eval_lvalue vm e in
        LMember (e, m)
    | LIndex (e, i) ->
        let e = eval_lvalue vm e in
        let i = eval_rvalue vm i in
        LIndex (e, i)


  and eval_call (vm : vm) findex (args : rvalue list) =
    match vm.code.(findex) with
    | Function { body; locals; _ } ->
        let pre_frame = vm.frame in
        vm.frame <- vm.sp + 1 ;
        pushArgs vm args ;
        allocate vm locals ;
        eval_instr vm body ;
        let ret = pop vm in
        vm.sp <- vm.frame - 1 ;
        vm.frame <- pre_frame ;
        ret
    | External -> failwith ""


  and eval_instr (vm : vm) (instr : Compile.instr list) =
    match instr with
    | [] -> ()
    | { i = Return e; _ } :: _ ->
        let e = eval_rvalue vm e in
        push vm e
    | { i = If (cond, then_, else_); _ } :: t ->
        let cond = eval_rvalue vm cond in
        if isTrue cond then
          let () = eval_instr vm then_ in
          eval_instr vm t
        else
          let () = eval_instr vm else_ in
          eval_instr vm t
    | { i = While (cond, body); _ } :: t ->
        let rec loop () =
          let result = eval_rvalue vm cond in
          if isTrue result then
            let () = eval_instr vm body in
            loop ()
          else
            eval_instr vm t
        in
        loop ()
    | { i = Store (l, r); _ } :: t ->
        let l = eval_lvalue vm l in
        let r = eval_rvalue vm r in
        store vm l r ;
        eval_instr vm t


  and store (vm : vm) (l : lvalue) (r : rvalue) =
    match l, r with
    | LVoid, _ -> ()
    | LRef n, _ -> storeRef vm n r
    | LMember (LRef n, m), _ -> storeRefObject vm n m r
    | LIndex (LRef n, Int i), _ -> storeRefObject vm n i r
    | LTuple l_elems, Object r_elems -> Array.iter2 (store vm) l_elems r_elems
    | _ -> failwith "invalid store"
end

type bytecode = Compile.bytecode

let compile stmts : bytecode =
  let env, functions = Compile.compile stmts in
  Compile.{ table = env.functions; code = Array.of_list functions }


let main_path = "Main___main__type"

let rec getTypes stmts =
  match stmts with
  | [] -> []
  | { top = TopType descr; _ } :: t -> descr :: getTypes t
  | _ :: t -> getTypes t


let rec valueOfDescr (d : struct_descr) : Eval.rvalue =
  let elems = List.map (fun (_, t, _) -> valueOfType t) d.members in
  Object (Array.of_list elems)


and valueOfType (t : type_) : Eval.rvalue =
  match t.t with
  | TVoid -> Void
  | TInt -> Int 0
  | TReal -> Real 0.0
  | TFixed -> Real 0.0
  | TBool -> Bool false
  | TString -> String ""
  | TArray (dim, t) ->
      let elems = Array.init dim (fun _ -> valueOfType t) in
      Object elems
  | TTuple elems ->
      let elems = List.map valueOfType elems in
      Object (Array.of_list elems)
  | TStruct descr -> valueOfDescr descr


let createArgument stmts =
  let types = getTypes stmts in
  match List.find_opt (fun (s : struct_descr) -> s.path = main_path) types with
  | Some d -> [ valueOfDescr d ]
  | None -> []


let run (env : Env.in_top) (prog : top_stmt list) (exp : string) =
  let e = Parser.Parse.parseString (Some "Main_.vult") (Pla.print {pla|fun _main_() return <#exp#s>;|pla}) in
  let env, main = Inference.infer_single env e in
  let main = Prog.convert env main in
  let bytecode = compile (prog @ main) in
  let vm = Eval.new_vm bytecode in
  let findex = Map.find "Main___main_" vm.table in
  let args = createArgument main in
  let result = Eval.eval_call vm findex args in
  let str = Pla.print (Eval.print_value result) in
  str
