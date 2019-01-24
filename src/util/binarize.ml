
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
      let () = Bytes.set o 1 'x' in
      let rec loop i c =
         let ch = String.get s i in
         if String.get s i = 'p' then
            i + 1
         else if c < 8 then
            let () = Bytes.set o c ch in
            loop (i + 1) (c + 1)
         else loop (i + 1) (c + 1)
      in
      let i = loop i 2 in
      int_of_string (Bytes.to_string o), i

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

let get_binary_float n =
   let s, m, e = get_parts n in
   m lor (e lsl 23) lor (s lsl 32)

let float_to_bin_string n =
   let i = get_binary_float n in
   let b = Bytes.create 4 in
   let n1 = Char.chr @@ (i land 0xFF000000) lsr 24 in
   let n2 = Char.chr @@(i land 0x00FF0000) lsr 16 in
   let n3 = Char.chr @@(i land 0x0000FF00) lsr 8 in
   let n4 = Char.chr @@(i land 0x000000FF) lsr 0 in
   let () = Bytes.set b 0 n1 in
   let () = Bytes.set b 1 n2 in
   let () = Bytes.set b 2 n3 in
   let () = Bytes.set b 3 n4 in
   Bytes.to_string b
