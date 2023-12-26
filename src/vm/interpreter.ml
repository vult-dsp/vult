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
open Core
open Prog
open Vmv

module MakeVM (VM : VM) = struct
  let pushValues (vm : VM.t) (args : rvalue list) : VM.t = List.fold_left (fun vm v -> VM.push vm v) vm args

  let numeric i f e1 e2 : rvalue =
    match e1, e2 with
    | Int n1, Int n2 -> Int (i n1 n2)
    | Real n1, Real n2 -> Real (f n1 n2)
    | _ ->
      let se1 = Pla.print (print_value e1) in
      let se2 = Pla.print (print_value e2) in
      failwith ("numeric: argument mismatch: " ^ se1 ^ " op " ^ se2)


  let relation i f b e1 e2 : rvalue =
    match e1, e2 with
    | Int n1, Int n2 -> Bool (i n1 n2)
    | Real n1, Real n2 -> Bool (f n1 n2)
    | Bool n1, Bool n2 -> Bool (b n1 n2)
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


  let eval_array f (vm : VM.t) (a : 'b array) : VM.t * 'value array =
    let vm, a =
      Array.fold_left
        (fun (vm, acc) a ->
          let vm, a = f vm a in
          vm, a :: acc)
        (vm, [])
        a
    in
    vm, Array.of_list (List.rev a)


  let eval_op (op : Compile.op) =
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


  let isTrue (cond : rvalue) =
    match cond with
    | Bool true -> true
    | Bool false -> false
    | _ -> failwith "invalid condition"


  let rec eval_rvalue (vm : VM.t) (r : Compile.rvalue) : VM.t * rvalue =
    match r.r with
    | RVoid -> vm, Void
    | RInt n -> vm, Int n
    | RReal n -> vm, Real n
    | RBool n -> vm, Bool n
    | RString s -> vm, String s
    | RRef (n, _) -> vm, VM.loadRef vm n
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
      vm, Object elems
    | RIndex (e, index) -> (
      let vm, e = eval_rvalue vm e in
      let vm, index = eval_rvalue vm index in
      match e, index with
      | Object elems, Int index -> vm, elems.(index)
      | _ -> failwith "index not evaluated correctly")
    | RCall (index, _, args) ->
      let vm, args = eval_rvalue_list vm args in
      eval_call vm index args
    | RMember (e, index, _) -> (
      let vm, e = eval_rvalue vm e in
      match e with
      | Object elems -> vm, elems.(index)
      | _ -> failwith "member: not a struct")
    | RTMember (e, index) -> (
      let vm, e = eval_rvalue vm e in
      match e with
      | Object elems -> vm, elems.(index)
      | _ -> failwith "member: not a struct")


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


  and eval_lvalue (vm : VM.t) (l : Compile.lvalue) : VM.t * lvalue =
    match l.l with
    | LVoid -> vm, LVoid
    | LRef (n, _) -> vm, LRef n
    | LTuple elems ->
      let vm, elems = eval_array eval_lvalue vm elems in
      vm, LTuple elems
    | LMember (e, m, _) ->
      let vm, e = eval_lvalue vm e in
      vm, LMember (e, m)
    | LIndex (e, i) ->
      let vm, e = eval_lvalue vm e in
      let vm, i = eval_rvalue vm i in
      vm, LIndex (e, i)


  and eval_call (vm : VM.t) findex (args : rvalue list) : VM.t * rvalue =
    match findex with
    | F findex -> (
      match VM.code vm findex with
      | Function { body; locals; _ } ->
        let vm, frame = VM.newFrame vm in
        let vm = pushValues vm args in
        let vm = VM.allocate vm locals in
        let vm = eval_instr vm body in
        let vm, ret = VM.pop vm in
        let vm = VM.restoreFrame vm frame in
        vm, ret
      | External -> failwith "")
    | B f -> (
      match f, args with
      | Pi, [] -> vm, Real Float.pi
      | Pi, _ -> failwith "invalid arguments to 'pi' function"
      | Eps, [] -> vm, Real 1e-18
      | Eps, _ -> failwith "invalid arguments to 'eps' function"
      | Random, [] -> vm, Real (Random.float 1.0)
      | Random, _ -> failwith "invalid arguments to 'random' function"
      | IRandom, [] -> vm, Int (Random.int Int.max_int)
      | IRandom, _ -> failwith "invalid arguments to 'irandom' function"
      | Real, [ Int n ] -> vm, Real (float_of_int n)
      | Real, [ Real n ] -> vm, Real n
      | Real, [ Bool n ] -> vm, Real (if n then 1.0 else 0.0)
      | Real, _ -> failwith "invalid arguments to 'real' function"
      | String, [ Int n ] -> vm, String (string_of_int n)
      | String, [ Real n ] -> vm, String (string_of_float n)
      | String, [ Bool n ] -> vm, String (if n then "true" else "false")
      | String, _ -> failwith "invalid arguments to 'string' function"
      | Fixed, [ Int n ] -> vm, Real (float_of_int n)
      | Fixed, [ Real n ] -> vm, Real n
      | Fixed, [ Bool n ] -> vm, Real (if n then 1.0 else 0.0)
      | Fixed, _ -> failwith "invalid arguments to 'fixed' function"
      | Int, [ Int n ] -> vm, Int n
      | Int, [ Real n ] -> vm, Int (int_of_float n)
      | Int, [ Bool n ] -> vm, Int (if n then 1 else 0)
      | Int, _ -> failwith "invalid arguments to 'int' function"
      | Exp, [ Real x ] -> vm, Real (exp x)
      | Exp, _ -> failwith "invalid arguments to 'exp' function"
      | Sin, [ Real x ] -> vm, Real (sin x)
      | Sin, _ -> failwith "invalid arguments to 'sin' function"
      | Cos, [ Real x ] -> vm, Real (cos x)
      | Cos, _ -> failwith "invalid arguments to 'cos' function"
      | Tan, [ Real x ] -> vm, Real (tan x)
      | Tan, _ -> failwith "invalid arguments to 'tan' function"
      | Sinh, [ Real x ] -> vm, Real (sinh x)
      | Sinh, _ -> failwith "invalid arguments to 'sinh' function"
      | Cosh, [ Real x ] -> vm, Real (cosh x)
      | Cosh, _ -> failwith "invalid arguments to 'cosh' function"
      | Tanh, [ Real x ] -> vm, Real (tanh x)
      | Tanh, _ -> failwith "invalid arguments to 'tanh' function"
      | Sqrt, [ Real x ] -> vm, Real (sqrt x)
      | Sqrt, _ -> failwith "invalid arguments to 'sqrt' function"
      | Abs, [ Real x ] -> vm, Real (abs_float x)
      | Abs, [ Int x ] -> vm, Int (abs x)
      | Abs, _ -> failwith "invalid arguments to 'abs' function"
      | Log10, [ Real x ] -> vm, Real (log10 x)
      | Log10, _ -> failwith "invalid arguments to 'log10' function"
      | Floor, [ Real x ] -> vm, Real (floor x)
      | Floor, _ -> failwith "invalid arguments to 'floor' function"
      | Clip, [ Real x; Real min_v; Real max_v ] -> vm, Real (min (max x min_v) max_v)
      | Clip, [ Int x; Int min_v; Int max_v ] -> vm, Int (min (max x min_v) max_v)
      | Clip, _ -> failwith "invalid arguments to 'clip' function"
      | Pow, [ Real x; Real y ] -> vm, Real (x ** y)
      | Pow, _ -> failwith "invalid arguments to 'pow' function"
      | Size, [ Object x ] -> vm, Int (Array.length x)
      | Size, _ -> failwith "invalid arguments to 'size' function"
      | Get, [ Object x; Int n ] -> vm, x.(n)
      | Get, _ -> failwith "invalid arguments to 'get' function"
      | Set, [ Object x; Int n; v ] ->
        x.(n) <- v;
        vm, Void
      | Set, _ -> failwith "invalid arguments to 'set' function"
      | Samplerate, _ -> failwith "samplerate() not implemented in the interpreter")


  and eval_instr (vm : VM.t) (instr : Compile.instr list) : VM.t =
    let trace vm i =
      if false then (
        VM.printStack vm;
        print_endline (Pla.print (Compile.print_instr i)))
    in
    match instr with
    | [] -> vm
    | ({ i = Return e; _ } as h) :: _ ->
      trace vm h;
      let vm, e = eval_rvalue vm e in
      VM.push vm e
    | ({ i = If (cond, then_, else_); _ } as h) :: t ->
      trace vm h;
      let vm, cond = eval_rvalue vm cond in
      if isTrue cond then (
        let vm = eval_instr vm then_ in
        eval_instr vm t)
      else (
        let vm = eval_instr vm else_ in
        eval_instr vm t)
    | ({ i = While (cond, body); _ } as h) :: t ->
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
    | ({ i = Store (l, r); _ } as h) :: t ->
      trace vm h;
      let vm, l = eval_lvalue vm l in
      let vm, r = eval_rvalue vm r in
      let vm = store vm l r in
      eval_instr vm t


  and store (vm : VM.t) (l : lvalue) (r : rvalue) : VM.t =
    match l, r with
    | LVoid, _ -> vm
    | LRef n, _ -> VM.storeRef vm n r
    | LMember (LRef n, m), _ -> VM.storeRefObject vm n [ m ] r
    | LIndex (LRef n, Int i), _ -> VM.storeRefObject vm n [ i ] r
    | LTuple l_elems, Object r_elems ->
      List.fold_left2 (fun vm l r -> store vm l r) vm (Array.to_list l_elems) (Array.to_list r_elems)
    | _ ->
      let n, i = getIndirectAccess vm l in
      VM.storeRefObject vm n i r


  and getIndirectAccess (vm : VM.t) (l : lvalue) =
    match l with
    | LVoid -> failwith "getIndirectAccess: void"
    | LRef _ -> failwith "getIndirectAccess: not indirect"
    | LTuple _ -> failwith "getIndirectAccess: tuple"
    | LMember (LRef n, m) -> n, [ m ]
    | LIndex (LRef n, Int i) -> n, [ i ]
    | LMember (l, m) ->
      let n, i = getIndirectAccess vm l in
      n, i @ [ m ]
    | LIndex (l, Int m) ->
      let n, i = getIndirectAccess vm l in
      n, i @ [ m ]
    | LIndex (_, _) -> failwith "getIndirectAccess: index is not evaluated"


  let newVM = VM.newVM
  let findSegment = VM.findSegment
end

module VMV = MakeVM (Mutable)

type bytecode = Compile.bytecode

let compile stmts : bytecode =
  let env, segments = Compile.compile stmts in
  Compile.{ table = env.functions; code = Array.of_list segments }


let main_path = "Main___main__type"

let rec getTypes stmts =
  match stmts with
  | [] -> []
  | { top = TopType descr; _ } :: t -> descr :: getTypes t
  | _ :: t -> getTypes t


let rec valueOfDescr (d : struct_descr) : rvalue =
  let elems = List.map (fun (_, t, _) -> valueOfType t) d.members in
  Object (Array.of_list elems)


and valueOfType (t : type_) : rvalue =
  match t.t with
  | TVoid _ -> Void
  | TInt -> Int 0
  | TReal -> Real 0.0
  | TFixed -> Real 0.0
  | TBool -> Bool false
  | TString -> String ""
  | TArray (Some dim, t) ->
    let elems = Array.init dim (fun _ -> valueOfType t) in
    Object elems
  | TTuple elems ->
    let elems = List.map valueOfType elems in
    Object (Array.of_list elems)
  | TStruct descr -> valueOfDescr descr
  | TArray (None, _) -> failwith "valueOfType: array with no dimensions"


let createArgument stmts =
  let types = getTypes stmts in
  match List.find_opt (fun (s : struct_descr) -> s.path = main_path) types with
  | Some d -> [ valueOfDescr d ]
  | None -> []


let createVm prog =
  let env, segments = Compile.compile prog in
  let bytecode = Compile.{ table = env.functions; code = Array.of_list segments } in
  VMV.newVM bytecode, bytecode


let callFunction vm name args =
  let findex = VMV.findSegment vm name in
  try
    let _, result = VMV.eval_call vm findex args in
    result
  with
  | e ->
    print_endline ("Failed to evaluate function:" ^ name);
    raise e


let run (iargs : Util.Args.args) (env : Env.in_top) (prog : top_stmt list) (exp : string) =
  let e = Pparser.Parse.parseString (Some "Main_.vult") (Pla.print [%pla {|fun _main_() return <#exp#s>;|}]) in
  let _, main = Inference.infer_single iargs env e in
  let bytecode = compile (prog @ main) in
  let vm = VMV.newVM bytecode in
  let findex = VMV.findSegment vm "Main___main_" in
  let args = createArgument main in
  let _, result = VMV.eval_call vm findex args in
  let str = Pla.print (print_value result) in
  str
