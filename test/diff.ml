(*
   The MIT License (MIT)

   Copyright (c) 2014-2024 Leonardo Laguna Ruiz, Carl Jönsson

   Permission is hereby granted, free of charge, to any person obtaining a copy
   of this software and associated documentation files (the "Software"), to deal
   in the Software without restriction, including without limitation the rights
   to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
   copies of the Software, and to permit persons to whom the Software is
   furnished to do so, subject to the following conditions:

   The above copyright notice and this permission notice shall be included in
   all copies or substantial 95portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
   IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
   FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
   AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
   LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
   OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
   THE SOFTWARE.
*)

let rec join_buff (buff : Buffer.t) (sep : string) (l : string list) : unit =
  match l with
  | [] -> ()
  | [ h ] -> Buffer.add_string buff h
  | h :: t ->
    Buffer.add_string buff h;
    Buffer.add_string buff sep;
    join_buff buff sep t


let stringJoin (l : string list) : string =
  let buff = Buffer.create 128 in
  join_buff buff "" l;
  Buffer.contents buff


let stringJoinWith (sep : string) (l : string list) : string =
  let buff = Buffer.create 128 in
  join_buff buff sep l;
  Buffer.contents buff


let stringSplit (sep : string) (s : string) : string list = Str.split (Str.regexp_string sep) s

(** Longest Common Sequence *)
let lcs xs' ys' =
  let longest xs ys = if List.length xs > List.length ys then xs else ys in
  let xs = Array.of_list xs'
  and ys = Array.of_list ys' in
  let n = Array.length xs
  and m = Array.length ys in
  let a = Array.make_matrix (n + 1) (m + 1) [] in
  for i = n - 1 downto 0 do
    for j = m - 1 downto 0 do
      a.(i).(j) <- (if xs.(i) = ys.(j) then xs.(i) :: a.(i + 1).(j + 1) else longest a.(i).(j + 1) a.(i + 1).(j))
    done
  done;
  a.(0).(0)


let rec interleaveLineDiff file i a b l =
  match a, b, l with
  | _, _, [] -> List.map (fun x -> "- " ^ x) a @ List.map (fun x -> "+ " ^ x) b
  | [], _, _ :: _ | _, [], _ :: _ ->
    failwith "Diff.interleaveLineDiff: The results obtained from 'Diff.lcs' function are incorrect"
  | ah :: at, bh :: bt, lh :: lt -> (
    match ah = lh, bh = lh with
    | true, true -> interleaveLineDiff file (i + 1) at bt lt
    | true, false ->
      ("\nFile : " ^ file ^ " : " ^ string_of_int i ^ "\n + " ^ bh) :: interleaveLineDiff file (i + 1) a bt l
    | false, true ->
      ("\nFile : " ^ file ^ " : " ^ string_of_int i ^ "\n - " ^ ah) :: interleaveLineDiff file (i + 1) at b l
    | false, false ->
      ("\nFile : " ^ file ^ " : " ^ string_of_int i ^ "\n + " ^ bh)
      :: (" - " ^ ah)
      :: interleaveLineDiff file (i + 1) at bt l)


let lineDiff file str1 str2 =
  let lines1 = stringSplit "\n" str1 in
  let lines2 = stringSplit "\n" str2 in
  let lseq = lcs lines1 lines2 in
  let final_seq = interleaveLineDiff file 1 lines1 lines2 lseq in
  stringJoinWith "\n" final_seq


let tokenString t =
  let open BaseTok in
  match t with
  | Id s -> s
  | Int i -> Int64.to_string i
  | Float f -> string_of_float f
  | Other s -> s
  | Hex i -> string_of_int i
  | EOF -> ""


let tokenizeLine line =
  let stream = Lexing.from_string line in
  let tokens = ref [] in
  let () =
    let rec loop () =
      match BaseTok.read stream with
      | BaseTok.EOF -> ()
      | t ->
        tokens := t :: !tokens;
        loop ()
    in
    loop ()
  in
  List.rev !tokens


let equalToken t1 t2 =
  let open BaseTok in
  match t1, t2 with
  | Float f1, Float f2 ->
    let result =
      if abs_float f2 < 1e-4 && abs_float f1 < 1e-4 then
        true
      else (
        let delta = abs_float (f1 -. f2) in
        delta < 1e-4)
    in
    let () =
      if not result then (
        let msg = Printf.sprintf "diff %f %f\n" f1 f2 in
        prerr_endline msg)
    in
    result
  | Int n1, Int n2 when n1 = n2 -> true
  | Hex n1, Hex n2 -> abs (n1 - n2) <= 1
  | Id n1, Id n2 when n1 = n2 -> true
  | Other n1, Other n2 when n1 = n2 -> true
  | EOF, EOF -> true
  | _ ->
    let msg = Printf.sprintf "%s <> %s" (tokenString t1) (tokenString t2) in
    prerr_endline msg;
    false


let compareLine line1 line2 =
  let s1 = tokenizeLine line1 in
  let s2 = tokenizeLine line2 in
  try List.for_all2 equalToken s1 s2 with
  | Invalid_argument _ -> false


let compare str1 str2 =
  let lines1 = stringSplit "\n" str1 in
  let lines2 = stringSplit "\n" str2 in
  try List.for_all2 compareLine lines1 lines2 with
  | Invalid_argument _ -> false
