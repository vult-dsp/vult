
let get_sign s =
   if String.get s 0 = '-' then 1, 1 else 0, 0

let rec move_to_mantisa s i =
   let c = String.get s i in
   if c = '.' then
      i + 1
   else if c = 'p' then
      i
   else
      move_to_mantisa s (i + 1)

let get_mantisa s i =
   if String.get s i = 'p' then
      0, i + 1
   else
      let o = Bytes.init 8 (fun _ -> '0') in
      let r = ref 0 in
      let () = Bytes.set o 1 'x' in
      let rec loop i c =
         let ch = String.get s i in
         if String.get s i = 'p' then
            i + 1
         else if c < 8 then
            let () = Bytes.set o c ch in
            loop (i + 1) (c + 1)
         else if c = 8 then
            let round = Char.code ch in
            let () = r:= if round > 7 then 1 else 0 in
            loop (i + 1) (c + 1)
         else
            loop (i + 1) (c + 1)
      in
      let i = loop i 2 in
      !r + int_of_string (Bytes.to_string o), i

let get_exponent s i =
   127 + int_of_string (String.sub s i (String.length s - i))

let fix_float n =
   match classify_float n with
   | FP_normal -> n
   | FP_subnormal -> 0.0
   | FP_zero -> 0.0
   | _ -> failwith "cannot convert this number"

let get_parts n =
   try
      let f = fix_float n in
      if f = 0.0 then
         0, 0, 0
      else
         let s       = Printf.sprintf "%h" (fix_float n) in
         let sign, i = get_sign s in
         let i       = move_to_mantisa s i in
         let m, i    = get_mantisa s i in
         let e       = get_exponent s i in
         sign, m lsr 1, e
   with
   | _ ->
      let s       = Printf.sprintf "%h" n in
      failwith ("Failed to convert " ^ (string_of_float n) ^ " : " ^ s)

let get_binary_float n : Int64.t =
   let s, m, e = get_parts n in
   Int64.logor (Int64.logor (Int64.of_int m) (Int64.shift_left (Int64.of_int e) 23) ) (Int64.shift_left (Int64.of_int s) 31)

let to_int n =
   get_binary_float n |> Int64.to_int

let c1 = Int64.shift_left (Int64.of_int 0xFF) 24
let c2 = Int64.shift_left (Int64.of_int 0xFF) 16
let c3 = Int64.shift_left (Int64.of_int 0xFF) 8
let c4 = Int64.shift_left (Int64.of_int 0xFF) 0

let float_to_bin_string n =
   let i = get_binary_float n in
   let b = Bytes.create 4 in
   let n1 = Char.chr @@ Int64.to_int @@ Int64.shift_right (Int64.logand i c1) 24 in
   let n2 = Char.chr @@ Int64.to_int @@ Int64.shift_right (Int64.logand i c2) 16 in
   let n3 = Char.chr @@ Int64.to_int @@ Int64.shift_right (Int64.logand i c3) 8 in
   let n4 = Char.chr @@ Int64.to_int @@ Int64.shift_right (Int64.logand i c4) 0 in
   let () = Bytes.set b 0 n1 in
   let () = Bytes.set b 1 n2 in
   let () = Bytes.set b 2 n3 in
   let () = Bytes.set b 3 n4 in
   Bytes.to_string b
