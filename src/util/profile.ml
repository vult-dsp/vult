let table : (string, float) Hashtbl.t = Hashtbl.create 0

let getTime label =
  match Hashtbl.find_opt table label with
  | Some t -> t
  | None -> 0.0
;;

let time label f =
  let t1 = Sys.time () in
  let ret = f () in
  let t2 = Sys.time () in
  let current = getTime label in
  let acc = current +. (t2 -. t1) in
  Hashtbl.replace table label acc;
  ret
;;

let show () =
  let elems = Hashtbl.fold (fun key value acc -> (key, value) :: acc) table [] in
  let sorted = List.sort (fun (_, a) (_, b) -> compare b a) elems in
  List.iter (fun (label, value) -> prerr_endline (label ^ ": " ^ string_of_float value)) sorted
;;
