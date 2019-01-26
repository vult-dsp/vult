

type prog =
   | LoadReal of float
   | LoadInt of int
   | LoadBool of bool
   | LoadString of string

   | LoadVar of int
   | LoadMem of int

   | StoreVar of int
   | StoreMem of int

   | Drop
   | Explode

   | Add
   | Sub
   | Mul
   | Div
   | Mod
   | Call of int
   | Return

   | Halt

type value =
   | Real of float
   | Bool of bool
   | Int of int
   | String of string

type state =
   {
      prog : prog array;
      mutable pc : int;
      mutable stack : value list;

      mutable call_stack : (int * int * int) list;

      mutable mem_ptr : int;
      mem : value array;

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
      mem_ptr = 0; mem = Array.init 100 (fun _ -> Real 0.0);
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

let call p mem_ptr local_ptr state =
   state.call_stack <- (state.pc + 1, state.mem_ptr, state.local_ptr) :: state.call_stack;
   state.pc <- p;
   state.mem_ptr <- mem_ptr;
   state.local_ptr <- local_ptr

let return state =
   match state.call_stack with
   | (pc, mem_ptr, local_ptr) :: t ->
      state.call_stack <- t;
      state.pc <- pc;
      state.mem_ptr <- mem_ptr;
      state.local_ptr <- local_ptr
   | _ -> failwith "return"


let getMem n state =
   let value = Array.unsafe_get state.mem (state.mem_ptr + n) in
   push value state;
   inc_pc state

let setMem n state =
   let value = pop1 state in
   Array.unsafe_set state.mem (state.mem_ptr + n) value;
   inc_pc state


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

let loadReal n state =
   push (Real n) state;
   inc_pc state

let loadInt n state =
   push (Int n) state;
   inc_pc state

let loadBool n state =
   push (Bool n) state;
   inc_pc state

let rec exec state =
   let inst = try Array.get state.prog state.pc  with _ -> Halt in
   match inst with
   | Add -> add state; exec state
   | Sub -> sub state; exec state
   | Mul -> mul state; exec state
   | Div -> div state; exec state

   | LoadReal n -> loadReal n state; exec state
   | LoadInt n -> loadInt n state; exec state
   | LoadBool n -> loadBool n state; exec state

   | StoreVar a -> setLocal a state; exec state
   | StoreMem a -> setMem a state; exec state

   | LoadVar a -> getLocal a state; exec state
   | LoadMem a -> getMem a state; exec state

   | Halt -> state

   | _ -> failwith "Unknown function"


let print_stack state =
   List.iteri (fun i v -> let s = show_value v in Printf.printf "%i: %s\n" i s) state.stack

type env =
   {
      code : prog list;
      pc : int;
      locals_count : int;
      locals : (string * int) list;
      mem_count : int;
      mem : (string * int) list;
   }

let default_env =
   { code = []; pc = 0; locals_count = 0; locals = []; mem_count = 0; mem = [] }

let addInst inst env =
   { env with code = inst :: env.code; pc = env.pc + 1 }

let rec compileExp env exp =
   let open Prog in
   match exp with
   | PUnit _ -> env
   | PEmpty -> env
   | PBool (v, _)   -> addInst (LoadBool v) env
   | PInt (v, _)    -> addInst (LoadInt v) env
   | PReal (v, _)   -> addInst (LoadReal v) env
   | PString (v, _) -> addInst (LoadString v) env
   | PId (id, _) ->
      let id = Id.show id in
      begin
         match List.assoc_opt id env.locals with
         | Some n -> addInst (LoadVar n) env
         | None ->
            match List.assoc_opt id env.mem with
            | Some n -> addInst (LoadMem n) env
            | None -> failwith ""
      end
   | POp ("+", args, _) ->
      let env = compileExpList args env in
      addInst Add env

   | _ -> failwith "compileExp"

and compileExpList (expl:Prog.exp list) env =
   List.fold_left compileExp env expl

let rec declare kind lhs env =
   let open Prog in
   match lhs with
   | LWild _ -> env
   | LId (id, _, _) ->
      let id   = Id.show id in
      if kind = `Val then
         let n    = env.locals_count in
         let next = env.locals_count + 1 in
         {
            env with
            locals_count = next;
            locals = (id, n) :: env.locals
         }
      else
         let n    = env.mem_count in
         let next = env.mem_count + 1 in
         {
            env with
            mem_count = next;
            mem = (id, n) :: env.mem
         }
   | LTyped (lhs, _, _) | LGroup (lhs, _) -> declare kind lhs env
   | LTuple (elems, _) ->
      List.fold_left (fun env lhs -> declare kind lhs env) env elems
   | LIndex _ -> failwith "declare"


let rec bind env lhs =
   let open Prog in
   match lhs with
   | LWild _ -> addInst Drop env
   | LGroup (lhs, _) | LTyped (lhs, _, _) -> bind env lhs
   | LTuple (elems, _) ->
      let env = addInst Explode env in
      List.fold_left bind env elems
   | LIndex _ -> failwith ""
   | LId (id, _, _) ->
      let id = Id.show id in
      match List.assoc_opt id env.locals with
      | Some n -> addInst (StoreVar n) env
      | None ->
         match List.assoc_opt id env.mem with
         | Some n -> addInst (StoreMem n) env
         | None -> failwith ""

let rec compileStmt env stmt =
   let open Prog in
   match stmt with
   | StmtVal (lhs, rhs, _ ) ->
      let env = declare `Val lhs env in
      let env = CCOpt.map_or ~default:env (compileExp env) rhs in
      let env = bind env lhs in
      env

   | StmtMem (lhs, rhs, _ ) ->
      let env = declare `Mem lhs env in
      let env = CCOpt.map_or ~default:env (compileExp env) rhs in
      let env = bind env lhs in
      env

   | StmtBlock(_, stmts, _) ->
      compileStmtList env stmts

   | _ -> failwith ""

and compileStmtList env stmts =
   List.fold_left compileStmt env stmts

let test =
   {|
{
   mem x = 1;
   val y = 2;
   val z = x + y;
}
   |}

let getCode env =
   List.rev env.code |> Array.of_list

let main () =
   let p = Parser.parseStmtList test in
   let prog = getCode @@ compileStmt default_env p in
   let state = default_state prog in
   let state = exec state in
   print_stack state
;;

main ();;

