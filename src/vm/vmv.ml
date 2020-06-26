open Util.Maps

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


module type VM = sig
  type t

  val newVM : Compile.bytecode -> t

  val code : t -> int -> Compile.segment

  val push : t -> rvalue -> t

  val pop : t -> t * rvalue

  val loadRef : t -> int -> rvalue

  val storeRef : t -> int -> rvalue -> t

  val storeRefObject : t -> int -> int -> rvalue -> t

  val allocate : t -> int -> t

  val newFrame : t -> t * int

  val restoreFrame : t -> int -> t

  val printStack : t -> unit

  val findSegment : t -> string -> int
end

module Mutable : VM = struct
  type t =
    { stack : rvalue array
    ; mutable sp : int
    ; mutable frame : int
    ; table : int Map.t
    ; code : Compile.segment array
    }

  let newVM (compiled : Compile.bytecode) =
    { stack = Array.init 1024 (fun _ -> Void); sp = -1; frame = 0; table = compiled.table; code = compiled.code }


  let code (vm : t) i = vm.code.(i)

  let push (vm : t) (value : rvalue) : t =
    vm.sp <- vm.sp + 1 ;
    vm.stack.(vm.sp) <- value ;
    vm


  let pop (vm : t) : t * rvalue =
    let ret = vm.stack.(vm.sp) in
    vm.sp <- vm.sp - 1 ;
    vm, ret


  let loadRef (vm : t) (n : int) : rvalue = vm.stack.(vm.frame + n)

  let storeRef (vm : t) (n : int) (v : rvalue) : t =
    vm.stack.(vm.frame + n) <- v ;
    vm


  let storeRefObject (vm : t) (n : int) (i : int) (v : rvalue) : t =
    match vm.stack.(vm.frame + n) with
    | Object elems ->
        elems.(i) <- v ;
        vm
    | _ -> failwith "storeRefMember: invalid input"


  let allocate (vm : t) (n : int) : t =
    vm.sp <- vm.sp + n ;
    vm


  let newFrame (vm : t) : t * int =
    let frame = vm.frame in
    vm.frame <- vm.sp + 1 ;
    vm, frame


  let restoreFrame (vm : t) (frame : int) : t =
    vm.sp <- vm.frame - 1 ;
    vm.frame <- frame ;
    vm


  let findSegment (vm : t) (name : string) : int = Map.find name vm.table

  let printStack (vm : t) =
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
end
