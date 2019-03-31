

module VM = struct

   type builtin =
      | Exp
      | Sin
      | Cos
      | Tanh
      | SampleRate


   type instr =
      | PushReal of float
      | PushInt of int
      | PushBool of bool
      | PushString of string

      | PushVar of int

      | StoreVar of int

      | Drop
      | Explode

      | Add
      | Sub
      | Mul
      | Div
      | Mod
      | Neg

      | Eq
      | NEq
      | Lt
      | Gt
      | LtEq
      | GtEq

      | Call of int
      | BuiltinCall of builtin

      | Return

      | Halt

   type symbol =
      {
         name        : string;
         module_name : string;
         location    : int;
         local_count : int;
         locals      : (string * int) list;
      }

   let show_instr i =
      match i with
      | PushReal v -> (string_of_float v) ^ "f -> $"
      | PushInt v -> (string_of_int v) ^ " -> $"
      | PushBool v -> (if v then "true" else "false") ^ " -> $"
      | PushString v -> "\"" ^ v ^ "\" -> $"

      | PushVar i -> "var[" ^(string_of_int i) ^ "] -> $"

      | StoreVar i -> "$ -> var[" ^(string_of_int i) ^ "]"

      | Drop -> "drop"
      | Explode -> "explode"

      | Add -> "+"
      | Sub -> "-"
      | Mul -> "*"
      | Div -> "/"
      | Mod -> "%"
      | Neg -> "~"

      | Eq -> "=="
      | NEq -> "<>"
      | Lt -> "<"
      | Gt -> ">"
      | LtEq -> "<="
      | GtEq -> ">="

      | Call i -> "call[" ^(string_of_int i) ^ "]"
      | BuiltinCall call ->
         begin match call with
            | Exp -> "$exp"
            | Sin -> "$sin"
            | Cos -> "$cos"
            | Tanh -> "$tanh"
            | SampleRate -> "$samplerate"
         end

      | Return -> "return"

      | Halt -> "halt"

   let show_numbered_instr n i =
      (string_of_int n) ^ ": " ^ (show_instr i)

   type prog = instr array

   module IntMap = Map.Make(struct type t = int let compare = compare end)

   let show_prog symbols p =
      let symbol_table = List.fold_left (fun table s -> IntMap.add s.location s table) IntMap.empty symbols in
      let print index i =
         match IntMap.find_opt index symbol_table with
         | None -> show_numbered_instr index i
         | Some s ->
            let l = string_of_int s.local_count in
            "\n# " ^ s.name ^ "(" ^ l ^ ")\n" ^ show_numbered_instr index i
      in
      Array.mapi print p |> Array.to_list |> String.concat "\n"

   type value =
      | Real of float
      | Bool of bool
      | Int of int
      | String of string

   type state =
      {
         prog : instr array;
         mutable pc : int;
         mutable stack : value list;

         mutable call_stack : (int * int) list;

         mutable local_ptr : int;
         local : value array;
      }

   let show_value value =
      match value with
      | Real n -> string_of_float n
      | Bool n -> if n then "true" else "false"
      | Int n -> string_of_int n
      | String n -> "'" ^ n ^ "'"

   let default_state prog =
      {
         prog; pc = 0; stack = []; call_stack = [];
         local_ptr = 0; local = Array.init 100 (fun _ -> Real 0.0);
      }

   let push v state =
      state.stack <- v :: state.stack

   let pop1 state =
      match state.stack with
      | e1 :: rest ->
         state.stack <- rest;
         e1
      | _ -> failwith "pop1"

   let pop2 state =
      match state.stack with
      | e1 :: e2 :: rest ->
         state.stack <- rest;
         e1, e2
      | _ -> failwith "pop2"


   let inc_pc state =
      state.pc <- state.pc + 1

   let call function_pointer mem_ptr local_ptr state =
      state.call_stack <- (state.pc + 1, state.local_ptr) :: state.call_stack;
      state.pc <- function_pointer;
      state.local_ptr <- local_ptr

   let return state =
      match state.call_stack with
      | (pc, local_ptr) :: t ->
         state.call_stack <- t;
         state.pc <- pc;
         state.local_ptr <- local_ptr
      | _ -> failwith "return"


   let getLocal n state =
      let value = Array.unsafe_get state.local (state.local_ptr + n) in
      push value state;
      inc_pc state

   let setLocal n state =
      let value = pop1 state in
      Array.unsafe_set state.local (state.local_ptr + n) value;
      inc_pc state

   let add state =
      let e1, e2 = pop2 state in
      match e1, e2 with
      | Real e1, Real e2->
         let result = e1 +. e2 in
         push (Real result) state;
         inc_pc state
      | Int e1, Int e2->
         let result = e1 + e2 in
         push (Int result) state;
         inc_pc state
      | _ -> failwith "add: invalid input"

   let sub state =
      let e1, e2 = pop2 state in
      match e1, e2 with
      | Real e1, Real e2->
         let result = e1 -. e2 in
         push (Real result) state;
         inc_pc state
      | Int e1, Int e2->
         let result = e1 - e2 in
         push (Int result) state;
         inc_pc state
      | _ -> failwith "sub: invalid input"

   let neg state =
      let e1 = pop1 state in
      match e1 with
      | Real e1->
         let result = -. e1 in
         push (Real result) state;
         inc_pc state
      | Int e1 ->
         let result = - e1 in
         push (Int result) state;
         inc_pc state
      | _ -> failwith "neg: invalid input"

   let mul state =
      let e1, e2 = pop2 state in
      match e1, e2 with
      | Real e1, Real e2->
         let result = e1 *. e2 in
         push (Real result) state;
         inc_pc state
      | Int e1, Int e2->
         let result = e1 * e2 in
         push (Int result) state;
         inc_pc state
      | _ -> failwith "mul: invalid input"

   let div state =
      let e1, e2 = pop2 state in
      match e1, e2 with
      | Real e1, Real e2->
         let result = e1 /. e2 in
         push (Real result) state;
         inc_pc state
      | Int e1, Int e2->
         let result = e1 / e2 in
         push (Int result) state;
         inc_pc state
      | _ -> failwith "div: invalid input"

   let mod_op state =
      let e1, e2 = pop2 state in
      match e1, e2 with
      | Real e1, Real e2->
         let result = mod_float e1 e2 in
         push (Real result) state;
         inc_pc state
      | Int e1, Int e2->
         let result = e1 mod e2 in
         push (Int result) state;
         inc_pc state
      | _ -> failwith "mod: invalid input"

   let logic op state =
      let e1, e2 = pop2 state in
      match e1, e2 with
      | Real e1, Real e2->
         let result = op (compare e1 e2) in
         push (Bool result) state;
         inc_pc state
      | Int e1, Int e2->
         let result = op (compare e1 e2) in
         push (Bool result) state;
         inc_pc state
      | String e1, String e2->
         let result = op (compare e1 e2) in
         push (Bool result) state;
         inc_pc state
      | Bool e1, Bool e2->
         let result = op (compare e1 e2) in
         push (Bool result) state;
         inc_pc state
      | _ -> failwith "mul: invalid input"

   let pushReal n state =
      push (Real n) state;
      inc_pc state

   let pushInt n state =
      push (Int n) state;
      inc_pc state

   let pushBool n state =
      push (Bool n) state;
      inc_pc state

   let pushString n state =
      push (String n) state;
      inc_pc state

   exception HaltProgram of state


   let execBuiltin call state =
      match call with
      | SampleRate -> push (Real 0.0) state
      | _ ->
         let e1 = pop1 state in
         let () =
            match call, e1 with
            | Exp, Real v -> push (Real (exp v)) state
            | Exp, _ -> failwith "cannot apply Exp"
            | Sin, Real v -> push (Real (sin v)) state
            | Sin, _ -> failwith "cannot apply Exp"
            | Cos, Real v -> push (Real (cos v)) state
            | Cos, _ -> failwith "cannot apply Exp"
            | Tanh, Real v -> push (Real (tanh v)) state
            | Tanh, _ -> failwith "cannot apply Exp"
            | SampleRate, _ -> failwith "cannot apply SampleRate"
         in
         inc_pc state


   let rec exec state =
      try
         let inst = try Array.get state.prog state.pc with Invalid_argument _ -> raise (HaltProgram state) in
         let () =
            match inst with
            | Add -> add state
            | Sub -> sub state
            | Mul -> mul state
            | Div -> div state
            | Mod -> mod_op state
            | Neg -> neg state

            | Eq -> logic ((=) 0) state
            | NEq -> logic ((<>) 0) state
            | Lt -> logic ((>) 0) state
            | Gt -> logic ((<) 0) state
            | GtEq -> logic ((<) 0) state
            | LtEq -> logic ((<) 0) state

            | PushReal n -> pushReal n state
            | PushInt n -> pushInt n state
            | PushBool n -> pushBool n state
            | PushString n -> pushString n state

            | StoreVar a -> setLocal a state
            | PushVar a -> getLocal a state

            | Return -> return state

            | Drop -> ignore(pop1 state)

            | BuiltinCall call -> execBuiltin call state

            | Explode -> failwith "Explode not implemented"
            | Call _ -> failwith "Explode not implemented"

            | Halt -> raise (HaltProgram state)
         in
         exec state
      with HaltProgram state -> state



   let print_stack state =
      List.iteri (fun i v -> let s = show_value v in Printf.printf "%i: %s\n" i s) state.stack

end


module Compiler = struct

   type env =
      {
         module_name : string;
         code : VM.instr list;
         pc : int;
         local_count : int;
         local : (string * int) list;
         symbols : VM.symbol list;
      }

   let findCall (env:env) name =
      List.find (fun s -> s.VM.name = name) env.symbols

   let default_env module_name =
      { module_name; code = []; pc = 0; local_count = 0; local = []; symbols = [] }

   let addInst inst env =
      { env with code = inst :: env.code; pc = env.pc + 1 }

   let rec compileExp env (exp:Code.cexp) =
      match exp with
      | CEEmpty        -> env
      | CEBool v       -> addInst (PushBool v) env
      | CEInt v        -> addInst (PushInt v) env
      | CEFloat (_, v) -> addInst (PushReal v) env
      | CEString v     -> addInst (PushString v) env
      | CEVar ([id], _) ->
         let id = id in
         begin
            match List.assoc_opt id env.local with
            | Some n -> addInst (PushVar n) env
            | None -> failwith "Cannot compile var"
         end
      | CEOp ("+", args, _) ->
         let env = compileExpList env args in
         addInst Add env
      | CEOp ("*", args, _) ->
         let env = compileExpList env args in
         addInst Mul env
      | CEOp ("-", args, _) ->
         let env = compileExpList env args in
         addInst Sub env
      | CEOp ("/", args, _) ->
         let env = compileExpList env args in
         addInst Div env
      | CEOp ("%", args, _) ->
         let env = compileExpList env args in
         addInst Mod env

      | CEUnOp("-", e, _) ->
         let env = compileExp env e in
         addInst Neg env

      | CEOp ("<>", args, _) ->
         let env = compileExpList env args in
         addInst NEq env
      | CEOp ("==", args, _) ->
         let env = compileExpList env args in
         addInst Eq env
      | CEOp ("<", args, _) ->
         let env = compileExpList env args in
         addInst Lt env
      | CEOp (">", args, _) ->
         let env = compileExpList env args in
         addInst Gt env
      | CEOp ("<=", args, _) ->
         let env = compileExpList env args in
         addInst LtEq env
      | CEOp (">=", args, _) ->
         let env = compileExpList env args in
         addInst GtEq env


      | CECall ("exp", args, _) ->
         let env = compileExpList env args in
         addInst (BuiltinCall Exp) env
      | CECall ("sin", args, _) ->
         let env = compileExpList env args in
         addInst (BuiltinCall Sin) env
      | CECall ("cos", args, _) ->
         let env = compileExpList env args in
         addInst (BuiltinCall Cos) env
      | CECall ("tanh", args, _) ->
         let env = compileExpList env args in
         addInst (BuiltinCall Tanh) env
      | CECall ("samplerate", [], _) ->
         addInst (BuiltinCall SampleRate) env

      | CECall (name, args, _) ->
         let env    = compileExpList env args in
         let symbol = findCall env name in
         addInst (Call symbol.location) env

      | CETuple (elems, _) -> compileExpList env (List.map snd elems)

      | _ ->
         print_endline ("Cannot compile expression:" ^ (Code.show_cexp exp));
         failwith "compileExp"

   and compileExpList env (expl:Code.cexp list) =
      List.fold_left compileExp env expl

   let rec declare (lhs:Code.clhsexp) env =
      match lhs with
      | CLWild -> env
      | CLId (_, [id]) ->
         let n    = env.local_count in
         let next = env.local_count + 1 in
         {
            env with
            local_count = next;
            local = (id, n) :: env.local
         }
      | CLTuple elems ->
         List.fold_left (fun env lhs -> declare lhs env) env elems
      | CLIndex _ -> failwith "declare"


   let rec bind env (lhs:Code.clhsexp) =
      match lhs with
      | CLWild -> addInst Drop env
      | CLTuple elems ->
         let env = addInst Explode env in
         List.fold_left bind env elems
      | CLIndex _ -> failwith ""
      | CLId (_, [id]) ->
         begin match List.assoc_opt id env.local with
            | Some n -> addInst (StoreVar n) env
            | None -> failwith "Variable not found"
         end
      | _ -> failwith ("Failed to create binding for " ^ Code.show_clhsexp lhs)

   let rec compileArgs env (args:(Code.arg_type * string) list) =
      match args with
      | [] -> env
      | ( (Var _, name) | (Ref _, name) ) :: t ->
         let env = compileArgs env t in
         let lhs = Code.CLId ([], [name]) in
         let env = declare lhs env in
         let env = bind env lhs in
         env


   let makeSymbol (name:string) (env:env) (fenv:env) : VM.symbol =
      {
         module_name = fenv.module_name;
         name        = name;
         local_count = fenv.local_count - env.local_count;
         locals      = fenv.local;
         location    = env.pc;
      }

   let freshEnv (env:env) : env =
      {
         env with
         local_count = 0;
         local = [];
      }

   let rec compileStmt (env:env) (stmt:Code.cstmt) =
      let open Prog in
      match stmt with
      | CSVar (lhs, None) ->
         let env = declare lhs env in
         env

      | CSVar (lhs, Some rhs) ->
         let env = declare lhs env in
         let env = compileExp env rhs in
         let env = bind env lhs in
         env

      | CSBind (lhs, rhs) ->
         let env = compileExp env rhs in
         let env = bind env lhs in
         env

      | CSBlock stmts ->
         compileStmtList env stmts

      | CSReturn exp ->
         let env = compileExp env exp in
         let env = addInst Return env in
         env

      | CSFunction (_, name, args, body, _) ->
         let fenv = freshEnv env in
         let fenv = compileArgs fenv args in
         let fenv = compileStmt fenv body in
         let symbol = makeSymbol name env fenv in
         { env with symbols = symbol :: env.symbols; code = fenv.code; pc = fenv.pc }

      | CSType _ -> env
      | CSAlias _ -> env

      | _ ->
         let () = print_endline ("Cannot compile:\n" ^ (Code.show_cstmt stmt)) in
         failwith "compileStmt"

   and compileStmtList env stmts =
      List.fold_left compileStmt env stmts

   let getCode env : VM.prog =
      List.rev env.code |> Array.of_list
end

let test =
   {|
fun foo(x) {
   return x + 1;
}

fun counter() {
   mem x = x + 1;
   return x;
}
|}

let main () =
   let parser_results = Parser.parseString None test in
   let args           = { Args.default_arguments with code = CCode } in
   let stmts, used    = Passes.applyTransformations args [parser_results] in
   let params         = Generate.createParameters stmts args in
   let cparams        = Generate.makeParams args params used in
   let all_stmts      = List.flatten (List.map (fun a -> a.Prog.presult) stmts) in
   let clike_stmts    = ProgToCode.convert cparams all_stmts in
   let env = Compiler.compileStmtList (Compiler.default_env "Main") clike_stmts in
   let prog = Compiler.getCode env in
   let () = print_endline (VM.show_prog env.symbols prog) in
   (*let state = default_state prog in
     let state = exec state in
     print_stack state
   *)
   ()
;;

main ();;

