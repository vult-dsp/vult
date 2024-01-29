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
open Core
open Prog
open Compile
open Util.Maps

type t =
  { stack : rvalue array
  ; mutable sp : int
  ; mutable frame : int
  ; table : f Map.t
  ; code : segment array
  ; constants : rvalue array
  }

let newVM (compiled : bytecode) =
  { stack = Array.init 1024 (fun _ -> RVoid)
  ; sp = -1
  ; frame = 0
  ; table = compiled.table
  ; code = compiled.code
  ; constants = compiled.constants
  }


let code (vm : t) i = vm.code.(i)

let push (vm : t) (value : rvalue) : t =
  vm.sp <- vm.sp + 1;
  vm.stack.(vm.sp) <- value;
  vm


let pop (vm : t) : t * rvalue =
  let ret = vm.stack.(vm.sp) in
  vm.sp <- vm.sp - 1;
  vm, ret


let loadRef (vm : t) (n : int) : rvalue = vm.stack.(vm.frame + n)
let loadConst (vm : t) (n : int) : rvalue = vm.constants.(n)

let storeRef (vm : t) (n : int) (v : rvalue) : t =
  vm.stack.(vm.frame + n) <- v;
  vm


let storeRefObject (vm : t) (n : int) (i : int list) (v : rvalue) : t =
  match vm.stack.(vm.frame + n) with
  | RObject elems ->
    let rec loop l elems =
      match l with
      | [ i ] -> elems.(i) <- v
      | i :: l -> (
        match elems.(i) with
        | RObject elems -> loop l elems
        | _ -> failwith "storeRefObject: not an object")
      | _ -> failwith "storeRefObject: empty indices"
    in
    loop i elems;
    vm
  | _ -> failwith "storeRefObject: not an object"


let allocate (vm : t) (n : int) : t =
  vm.sp <- vm.sp + n;
  vm


let newFrame (vm : t) : t * int =
  let frame = vm.frame in
  vm.frame <- vm.sp + 1;
  vm, frame


let restoreFrame (vm : t) (frame : int) : t =
  vm.sp <- vm.frame - 1;
  vm.frame <- frame;
  vm


let findSegment (vm : t) (name : string) : f = Map.find name vm.table

let printStack (vm : t) =
  let rec loop n =
    if n <= vm.sp then (
      if vm.frame = n then print_string "->" else print_string "  ";
      print_int n;
      print_string " : ";
      print_endline (Pla.print (print_rvalue vm.stack.(n)));
      loop (n + 1))
  in
  loop 0


let pushValues (vm : t) (args : rvalue list) : t = List.fold_left (fun vm v -> push vm v) vm args

let[@inline always] numeric i f e1 e2 : rvalue =
  match e1, e2 with
  | RInt n1, RInt n2 -> RInt (i n1 n2)
  | RReal n1, RReal n2 -> RReal (f n1 n2)
  | _ ->
    let se1 = Pla.print (print_rvalue e1) in
    let se2 = Pla.print (print_rvalue e2) in
    failwith ("numeric: argument mismatch: " ^ se1 ^ " op " ^ se2)


let[@inline always] relation i f b e1 e2 : rvalue =
  match e1, e2 with
  | RInt n1, RInt n2 -> RBool (i n1 n2)
  | RReal n1, RReal n2 -> RBool (f n1 n2)
  | RBool n1, RBool n2 -> RBool (b n1 n2)
  | _ -> failwith "relation: argument mismatch"


let[@inline always] logic f e1 e2 : rvalue =
  match e1, e2 with
  | RBool n1, RBool n2 -> RBool (f n1 n2)
  | _ -> failwith "logic: argument mismatch"


let[@inline always] bitwise f e1 e2 : rvalue =
  match e1, e2 with
  | RInt n1, RInt n2 -> RInt (f n1 n2)
  | _ -> failwith "bitwise: argument mismatch"


let[@inline always] not e : rvalue =
  match e with
  | RInt n -> RInt (lnot n)
  | RBool n -> RBool (not n)
  | _ -> failwith "not: argument mismatch"


let[@inline always] neg e : rvalue =
  match e with
  | RInt n -> RInt (-n)
  | RReal n -> RReal (-.n)
  | _ -> failwith "not: argument mismatch"


let eval_array f (vm : t) (a : 'b array) : t * 'value array =
  let vm, a =
    Array.fold_left
      (fun (vm, acc) a ->
        let vm, a = f vm a in
        vm, a :: acc)
      (vm, [])
      a
  in
  vm, Array.of_list (List.rev a)


let eval_op (op : op) =
  match op with
  | OpAdd -> numeric ( + ) ( +. )
  | OpSub -> numeric ( - ) ( -. )
  | OpDiv -> numeric ( / ) ( /. )
  | OpMul -> numeric ( * ) ( *. )
  | OpMod -> numeric ( mod ) mod_float
  | OpEq -> relation ( = ) ( = ) ( = )
  | OpNe -> relation ( <> ) ( <> ) ( <> )
  | OpLt -> relation ( < ) ( < ) ( < )
  | OpGt -> relation ( > ) ( > ) ( > )
  | OpLe -> relation ( <= ) ( <= ) ( <= )
  | OpGe -> relation ( >= ) ( >= ) ( >= )
  | OpLand -> logic ( && )
  | OpLor -> logic ( || )
  | OpBor -> bitwise ( lor )
  | OpBand -> bitwise ( land )
  | OpBxor -> bitwise ( lxor )
  | OpLsh -> bitwise ( lsl )
  | OpRsh -> bitwise ( lsr )


let[@inline always] isTrue (cond : rvalue) =
  match cond with
  | RBool true -> true
  | RBool false -> false
  | _ -> failwith "invalid condition"


let rec eval_rvalue (vm : t) (r : rvalue) : t * rvalue =
  match r with
  | RVoid | RInt _ | RReal _ | RBool _ | RString _ -> vm, r
  | RRef (n, _) -> vm, loadRef vm n
  | ROp (op, e1, e2) ->
    let vm, e1 = eval_rvalue vm e1 in
    let vm, e2 = eval_rvalue vm e2 in
    vm, (eval_op op) e1 e2
  | RNeg e ->
    let vm, e = eval_rvalue vm e in
    vm, neg e
  | RNot e ->
    let vm, e = eval_rvalue vm e in
    vm, not e
  | RIf (cond, then_, else_) ->
    let vm, cond = eval_rvalue vm cond in
    if isTrue cond then eval_rvalue vm then_ else eval_rvalue vm else_
  | RObject elems ->
    let vm, elems = eval_array eval_rvalue vm elems in
    vm, RObject elems
  | RIndex (e, index) -> (
    let vm, e = eval_rvalue vm e in
    let vm, index = eval_rvalue vm index in
    match e, index with
    | RObject elems, RInt index -> vm, elems.(index)
    | _ -> failwith "index not evaluated correctly")
  | RCall (index, _, args) ->
    let vm, args = eval_rvalue_list vm args in
    eval_call vm index args
  | RMember (e, index, _) -> (
    let vm, e = eval_rvalue vm e in
    match e with
    | RObject elems -> vm, elems.(index)
    | _ -> failwith "member: not a struct")
  | RTMember (e, index) -> (
    let vm, e = eval_rvalue vm e in
    match e with
    | RObject elems -> vm, elems.(index)
    | _ -> failwith "member: not a struct")
  | RConst (i, _) -> vm, loadConst vm i


and eval_rvalue_list vm a =
  let vm, a =
    List.fold_left
      (fun (vm, acc) a ->
        let vm, a = eval_rvalue vm a in
        vm, a :: acc)
      (vm, [])
      a
  in
  vm, List.rev a


and eval_lvalue (vm : t) (l : lvalue) : t * lvalue =
  match l with
  | LVoid -> vm, LVoid
  | LRef (n, s) -> vm, LRef (n, s)
  | LTuple elems ->
    let vm, elems = eval_array eval_lvalue vm elems in
    vm, LTuple elems
  | LMember (e, m, n) ->
    let vm, e = eval_lvalue vm e in
    vm, LMember (e, m, n)
  | LIndex (e, i) ->
    let vm, e = eval_lvalue vm e in
    let vm, i = eval_rvalue vm i in
    vm, LIndex (e, i)


and eval_call (vm : t) findex (args : rvalue list) : t * rvalue =
  match findex with
  | F findex -> (
    match code vm findex with
    | Function { body; locals; _ } ->
      let vm, frame = newFrame vm in
      let vm = pushValues vm args in
      let vm = allocate vm locals in
      let vm = eval_instr vm body in
      let vm, ret = pop vm in
      let vm = restoreFrame vm frame in
      vm, ret
    | External -> failwith "The interpreter cannot evaluate external functions")
  | B f -> (
    match f, args with
    | Pi, [] -> vm, RReal Float.pi
    | Pi, _ -> failwith "invalid arguments to 'pi' function"
    | Eps, [] -> vm, RReal 1e-18
    | Eps, _ -> failwith "invalid arguments to 'eps' function"
    | Random, [] -> vm, RReal (Random.float 1.0)
    | Random, _ -> failwith "invalid arguments to 'random' function"
    | IRandom, [] -> vm, RInt (Random.int Int.max_int)
    | IRandom, _ -> failwith "invalid arguments to 'irandom' function"
    (* casting of numbers *)
    | Real, [ RInt n ] -> vm, RReal (float_of_int n)
    | Real, [ RReal n ] -> vm, RReal n
    | Real, [ RBool n ] -> vm, RReal (if n then 1.0 else 0.0)
    | Real, _ -> failwith "invalid arguments to 'real' function"
    | String, [ RInt n ] -> vm, RString (string_of_int n)
    | String, [ RReal n ] -> vm, RString (string_of_float n)
    | String, [ RBool n ] -> vm, RString (if n then "true" else "false")
    | String, _ -> failwith "invalid arguments to 'string' function"
    | Fixed, [ RInt n ] -> vm, RReal (float_of_int n)
    | Fixed, [ RReal n ] -> vm, RReal n
    | Fixed, [ RBool n ] -> vm, RReal (if n then 1.0 else 0.0)
    | Fixed, _ -> failwith "invalid arguments to 'fixed' function"
    | Int, [ RInt n ] -> vm, RInt n
    | Int, [ RReal n ] -> vm, RInt (int_of_float n)
    | Int, [ RBool n ] -> vm, RInt (if n then 1 else 0)
    | Int, _ -> failwith "invalid arguments to 'int' function"
    | Bool, [ RInt n ] -> vm, RBool (n <> 0)
    | Bool, [ RReal n ] -> vm, RBool (n <> 0.0)
    | Bool, [ RBool n ] -> vm, RBool n
    | Bool, _ -> failwith "invalid arguments to 'bool' function"
    (* trigonometric *)
    | Exp, [ RReal x ] -> vm, RReal (exp x)
    | Exp, _ -> failwith "invalid arguments to 'exp' function"
    | Sin, [ RReal x ] -> vm, RReal (sin x)
    | Sin, _ -> failwith "invalid arguments to 'sin' function"
    | Cos, [ RReal x ] -> vm, RReal (cos x)
    | Cos, _ -> failwith "invalid arguments to 'cos' function"
    | Tan, [ RReal x ] -> vm, RReal (tan x)
    | Tan, _ -> failwith "invalid arguments to 'tan' function"
    | Sinh, [ RReal x ] -> vm, RReal (sinh x)
    | Sinh, _ -> failwith "invalid arguments to 'sinh' function"
    | Cosh, [ RReal x ] -> vm, RReal (cosh x)
    | Cosh, _ -> failwith "invalid arguments to 'cosh' function"
    | Tanh, [ RReal x ] -> vm, RReal (tanh x)
    | Tanh, _ -> failwith "invalid arguments to 'tanh' function"
    | Sqrt, [ RReal x ] -> vm, RReal (sqrt x)
    | Sqrt, _ -> failwith "invalid arguments to 'sqrt' function"
    (* other function *)
    | Abs, [ RReal x ] -> vm, RReal (abs_float x)
    | Abs, [ RInt x ] -> vm, RInt (abs x)
    | Abs, _ -> failwith "invalid arguments to 'abs' function"
    | Log10, [ RReal x ] -> vm, RReal (log10 x)
    | Log10, _ -> failwith "invalid arguments to 'log10' function"
    | Floor, [ RReal x ] -> vm, RReal (floor x)
    | Floor, _ -> failwith "invalid arguments to 'floor' function"
    | Clip, [ RReal x; RReal min_v; RReal max_v ] -> vm, RReal (min (max x min_v) max_v)
    | Clip, [ RInt x; RInt min_v; RInt max_v ] -> vm, RInt (min (max x min_v) max_v)
    | Clip, _ -> failwith "invalid arguments to 'clip' function"
    | Pow, [ RReal x; RReal y ] -> vm, RReal (x ** y)
    | Pow, _ -> failwith "invalid arguments to 'pow' function"
    | Size, [ RObject x ] -> vm, RInt (Array.length x)
    | Size, _ -> failwith "invalid arguments to 'size' function"
    | Length, [ RString x ] -> vm, RInt (String.length x)
    | Length, _ -> failwith "invalid arguments to 'length' function"
    | Get, [ RObject x; RInt n ] -> vm, x.(n)
    | Get, _ -> failwith "invalid arguments to 'get' function"
    | Set, [ RObject x; RInt n; v ] ->
      x.(n) <- v;
      vm, RVoid
    | Set, _ -> failwith "invalid arguments to 'set' function"
    | Samplerate, _ -> failwith "samplerate() not implemented in the interpreter")


and eval_instr (vm : t) (instr : instr list) : t =
  let trace vm i =
    if false then (
      printStack vm;
      print_endline (Pla.print (print_instr i)))
  in
  match instr with
  | [] -> vm
  | (Return e as h) :: _ ->
    trace vm h;
    let vm, e = eval_rvalue vm e in
    push vm e
  | (If (cond, then_, else_) as h) :: t ->
    trace vm h;
    let vm, cond = eval_rvalue vm cond in
    if isTrue cond then (
      let vm = eval_instr vm then_ in
      eval_instr vm t)
    else (
      let vm = eval_instr vm else_ in
      eval_instr vm t)
  | (While (cond, body) as h) :: t ->
    trace vm h;
    let rec loop vm =
      let vm, result = eval_rvalue vm cond in
      if isTrue result then (
        let vm = eval_instr vm body in
        loop vm)
      else
        eval_instr vm t
    in
    loop vm
  | (Store (l, r) as h) :: t ->
    trace vm h;
    let vm, l = eval_lvalue vm l in
    let vm, r = eval_rvalue vm r in
    let vm = store vm l r in
    eval_instr vm t


and store (vm : t) (l : lvalue) (r : rvalue) : t =
  match l, r with
  | LVoid, _ -> vm
  | LRef (n, _), _ -> storeRef vm n r
  | LMember (LRef (n, _), m, _), _ -> storeRefObject vm n [ m ] r
  | LIndex (LRef (n, _), RInt i), _ -> storeRefObject vm n [ i ] r
  | LTuple l_elems, RObject r_elems ->
    List.fold_left2 (fun vm l r -> store vm l r) vm (Array.to_list l_elems) (Array.to_list r_elems)
  | _ ->
    let n, i = getIndirectAccess vm l in
    storeRefObject vm n i r


and getIndirectAccess (vm : t) (l : lvalue) =
  match l with
  | LVoid -> failwith "getIndirectAccess: void"
  | LRef _ -> failwith "getIndirectAccess: not indirect"
  | LTuple _ -> failwith "getIndirectAccess: tuple"
  | LMember (LRef (n, _), m, _) -> n, [ m ]
  | LIndex (LRef (n, _), RInt i) -> n, [ i ]
  | LMember (l, m, _) ->
    let n, i = getIndirectAccess vm l in
    n, i @ [ m ]
  | LIndex (l, RInt m) ->
    let n, i = getIndirectAccess vm l in
    n, i @ [ m ]
  | LIndex (_, _) -> failwith "getIndirectAccess: index is not evaluated"


type bytecode = Compile.bytecode

let getConstants (env : env) =
  Map.to_list env.constants.c
  |> List.map snd
  |> List.sort (fun (_, a) (_, b) -> compare a b)
  |> List.map fst
  |> Array.of_list


let compile stmts : bytecode =
  let env, segments = compile stmts in
  let constants = getConstants env in
  { table = env.functions; code = Array.of_list segments; constants }


let main_path = "Main___main__type"

let rec getTypes stmts =
  match stmts with
  | [] -> []
  | { top = TopType descr; _ } :: t -> descr :: getTypes t
  | _ :: t -> getTypes t


let rec valueOfDescr (d : struct_descr) : rvalue =
  let elems = List.map (fun (_, t, _, _) -> valueOfType t) d.members in
  RObject (Array.of_list elems)


and valueOfType (t : type_) : rvalue =
  match t.t with
  | TVoid _ -> RVoid
  | TInt -> RInt 0
  | TReal -> RReal 0.0
  | TFix16 -> RReal 0.0
  | TBool -> RBool false
  | TString -> RString ""
  | TArray (Some dim, t) ->
    let elems = Array.init dim (fun _ -> valueOfType t) in
    RObject elems
  | TTuple elems ->
    let elems = List.map valueOfType elems in
    RObject (Array.of_list elems)
  | TStruct descr -> valueOfDescr descr
  | TArray (None, _) -> failwith "valueOfType: array with no dimensions"


let createArgument stmts =
  let types = getTypes stmts in
  match List.find_opt (fun (s : struct_descr) -> s.path = main_path) types with
  | Some d -> [ valueOfDescr d ]
  | None -> []


let createVm prog =
  let env, segments = Compile.compile prog in
  let constants = getConstants env in
  let bytecode = { table = env.functions; code = Array.of_list segments; constants } in
  newVM bytecode, bytecode


let callFunction vm name args =
  let findex = findSegment vm name in
  try
    let _, result = eval_call vm findex args in
    result
  with
  | e ->
    print_endline ("Failed to evaluate function:" ^ name);
    raise e


let run (iargs : Util.Args.args) (env : Env.in_top) (prog : top_stmt list) (exp : string) =
  let e = Pparser.Parse.parseString (Some "Main_.vult") (Pla.print {%pla|fun _main_() return <#exp#s>;|}) in
  let env, main = Inference.infer_single iargs env e in
  let _, main = Toprog.convert iargs env main in
  let bytecode = compile (prog @ main) in
  let vm = newVM bytecode in
  let findex = findSegment vm "Main___main_" in
  let args = createArgument main in
  let _, result = eval_call vm findex args in
  let str = Pla.print (print_rvalue result) in
  str
