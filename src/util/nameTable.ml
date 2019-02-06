
type t =
   {
      mutable n : Random.State.t;
      table : (string, string) Hashtbl.t;
   }

let make () =
   { n = Random.State.make [|0|]; table = Hashtbl.create 128 }

let generateName (t:t) id =
   match Hashtbl.find_opt t.table id with
   | Some name -> name
   | None ->
      let is_generated = String.get id 0 = '_' in
      let name = String.init 12 (fun _ -> Char.chr (97 + (Random.State.int t.n 26))) in
      let name = if is_generated then "_" ^ name else name in
      let () = Hashtbl.add t.table id name in
      name

let registerName (t:t) id =
   let () = Hashtbl.add t.table id id in
   id

let getOrRegister (t:t) id =
   match Hashtbl.find_opt t.table id with
   | Some name -> name
   | None -> registerName t id
