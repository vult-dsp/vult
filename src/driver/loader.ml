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

(** This module contains routines used to automatically load a vult file and all it's depdendencies *)

open Pparser
open Util
open Syntax
open Args

(* Gets the list of modules used in this syntax tree *)
module Dependencies = struct
  module Set = CCSet.Make (String)

  let list f set l = List.fold_left f set l

  let option f set e =
    match e with
    | None -> set
    | Some e -> f set e


  let path (set : Set.t) (p : path) : Set.t =
    match p.n with
    | Some n -> Set.add n set
    | None -> set


  let rec type_ set (t : Syntax.type_) =
    match t.t with
    | STUnbound -> set
    | STId p -> path set p
    | STSize _ -> set
    | STComposed (_, subs) -> list type_ set subs


  let rec exp set e =
    match e.e with
    | SEBool _ -> set
    | SEInt _ -> set
    | SEReal _ -> set
    | SEFixed _ -> set
    | SEString _ -> set
    | SEId _ -> set
    | SEIndex { e; index } -> exp (exp set e) index
    | SEArray elems -> list exp set elems
    | SECall { path = p; args } -> list exp (path set p) args
    | SEUnOp (_, e) -> exp set e
    | SEOp (_, e1, e2) -> exp (exp set e1) e2
    | SEIf { cond; then_; else_ } -> exp (exp (exp set cond) then_) else_
    | SETuple elems -> list exp set elems
    | SEMember (e, _) -> exp set e
    | SEGroup e -> exp set e
    | SEEnum p -> path set p


  let rec pattern set e =
    match e.p with
    | SPWild -> set
    | SPBool _ -> set
    | SPInt _ -> set
    | SPReal _ -> set
    | SPFixed _ -> set
    | SPString _ -> set
    | SPTuple elems -> list pattern set elems
    | SPGroup e -> pattern set e
    | SPEnum p -> path set p


  let rec dexp set d =
    match d.d with
    | SDWild -> set
    | SDId _ -> set
    | SDTuple elems -> list dexp set elems
    | SDGroup e -> dexp set e
    | SDTyped (d, t) -> type_ (dexp set d) t


  let arg set (_, t, _) =
    match t with
    | None -> set
    | Some t -> type_ set t


  let rec function_def set (def, body) =
    match def with
    | { args; t; next } ->
      let set = stmt set body in
      let set = list arg set args in
      let set = option type_ set t in
      option function_def set next


  and ext_def set (def, body) =
    match def with
    | { args; t } ->
      let set = stmt set body in
      let set = list arg set args in
      option type_ set t


  and stmt set s =
    match s.s with
    | SStmtError -> set
    | SStmtVal (d, e) -> option exp (dexp set d) e
    | SStmtMem (d, e, _) -> option exp (dexp set d) e
    | SStmtBind (_, e) -> exp set e
    | SStmtReturn e -> exp set e
    | SStmtBlock elems -> list stmt set elems
    | SStmtIf (cond, then_, else_) -> option stmt (stmt (exp set cond) then_) else_
    | SStmtWhile (cond, s) -> stmt (exp set cond) s
    | SStmtIter { value; body } -> stmt (exp set value) body
    | SStmtMatch { e; cases } ->
      let set = exp set e in
      let case set (p, case) =
        let set = pattern set p in
        stmt set case
      in
      list case set cases


  and top_stmt set s =
    match s.top with
    | STopError -> set
    | STopExternal (def, _) -> ext_def set (def, { s = SStmtError; loc = s.loc })
    | STopFunction (def, body) -> function_def set (def, body)
    | STopType { members } -> list (fun set (_, t, _, _) -> type_ set t) set members
    | STopEnum _ -> set
    | STopConstant (d, e) -> exp (dexp set d) e


  let get s = Set.to_list (list top_stmt Set.empty s)
end

(** Given a module name, it looks for a matching file in all include directories *)
let rec findModule (includes : string list) (module_name : string) : string option =
  match includes with
  | [] -> None
  | h :: t ->
    (* first checks an uncapitalized file *)
    let file1 = Filename.concat h (String.uncapitalize_ascii module_name ^ ".vult") in
    if FileIO.exists file1 then
      Some file1
    else (
      (* then checks a file with the same name as the module *)
      let file2 = Filename.concat h (module_name ^ ".vult") in
      if FileIO.exists file2 then Some file2 else findModule t module_name)


(** Returns a list with all the possible directories where files can be found *)
let getIncludes (arguments : args) (files : input list) : string list =
  let current = FileIO.cwd () in
  (* the directories of the input files are considered include paths *)
  let implicit_dirs =
    List.map
      (fun input ->
        match input with
        | File f | Code (f, _) -> Filename.dirname f)
      files
  in
  (* these are the extra include paths passed in the arguments *)
  let explicit_dir =
    List.map (fun a -> if Filename.is_relative a then Filename.concat current a else a) arguments.includes
  in
  List.sort_uniq compare ((current :: implicit_dirs) @ explicit_dir)


(* main function that iterates the input files, gets the dependencies and searchs for the dependencies locations *)
let rec loadFiles_loop (includes : string list) file_deps dependencies parsed visited (files : input list) =
  let basename h = Filename.(chop_extension (basename h)) in
  match files with
  | [] -> dependencies, file_deps, parsed
  | ((File h | Code (h, _)) as input) :: t ->
    (* check that the file has not been visited before *)
    let h_module = Parse.moduleName h in
    if not (Hashtbl.mem visited h_module) then (
      let () = Hashtbl.add visited h_module true in
      let h_parsed =
        match input with
        | File _ -> Parse.parseFile h
        | Code (file, txt) -> Parse.parseString (Some file) txt
      in
      let () = Hashtbl.add parsed h_module h_parsed in
      (* gets the depencies based on the modules used *)
      let h_deps = Dependencies.get h_parsed.stmts in
      (* finds all the files for the used modules *)
      let h_dep_files = CCList.filter_map (findModule includes) h_deps |> List.filter (fun a -> a <> h) in
      let h_dep_files_input = List.map (fun a -> File a) h_dep_files in
      (* updates the tables *)
      let () = Hashtbl.add dependencies h_module h_deps in
      let () = Hashtbl.add file_deps (basename h) (List.map basename h_dep_files) in
      loadFiles_loop includes file_deps dependencies parsed visited (t @ h_dep_files_input))
    else
      loadFiles_loop includes file_deps dependencies parsed visited t


(** Raises an error if the modules have circular dependencies *)
let rec checkComponents (comps : string list list) : unit =
  match comps with
  | [] -> ()
  | [ _ ] :: t -> checkComponents t
  | h :: _ ->
    (* in this case one of the components has more than one module *)
    let msg = "The following modules have circular dependencies: " ^ String.concat ", " h in
    Error.raiseErrorMsg msg


module C = Components.Make (struct
    type key = string
    type data = string

    let get s = s
  end)

(* Given a list of files, finds and parses all the dependencies and returns the parsed contents in order *)
let loadFiles (arguments : args) (files : input list) =
  let includes = getIncludes arguments files in
  arguments.includes <- includes;
  let dependencies, file_deps, parsed =
    loadFiles_loop includes (Hashtbl.create 8) (Hashtbl.create 8) (Hashtbl.create 8) (Hashtbl.create 8) files
  in
  let dep_list = Hashtbl.fold (fun a b acc -> (a, b) :: acc) dependencies [] in
  let comps = C.calculate dep_list in
  let () = checkComponents comps in
  let sorted_deps = List.map List.hd comps in
  let sorted_files =
    CCList.filter_map
      (fun module_name ->
        match Hashtbl.find parsed module_name with
        | found -> Some found
        | exception Not_found -> None)
      sorted_deps
  in
  sorted_files, file_deps
