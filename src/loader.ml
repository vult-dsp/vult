(*
The MIT License (MIT)

Copyright (c) 2016 Leonardo Laguna Ruiz

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

open TypesVult

(** Given a module name, it looks for a matching file in all include directories *)
let rec findModule (includes:string list) (module_name:string) : string =
   match includes with
   | [] ->
      Error.raiseErrorMsg ("A file for the module '" ^ module_name ^ "' was not found in any of the included locations")
   | h::t ->
      (* first checks an uncapitalized file *)
      let file1 = Filename.concat h ((String.uncapitalize_ascii module_name) ^ ".vult") in
      if FileIO.exists file1 then file1
      else
         (* then checks a file with the same name as the module *)
         let file2 = Filename.concat h (module_name ^ ".vult") in
         if FileIO.exists file2 then file2
         else findModule t module_name

(** Returns a list with all the possible directories where files can be found *)
let getIncludes (arguments:arguments) (files:input list) : string list =
   let current = FileIO.cwd () in
   (* the directories of the input files are considered include paths *)
   let implicit_dirs = List.map (fun input -> match input with | File(f) | Code(f,_) -> Filename.dirname f) files in
   (* these are the extra include paths passed in the arguments *)
   let explicit_dir = List.map  (Filename.concat current) arguments.includes in
   List.sort_uniq compare (current :: implicit_dirs @ explicit_dir)

(* main function that iterates the input files, gets the dependencies and searchs for the dependencies locations *)
let rec loadFiles_loop (includes:string list) dependencies parsed (files:input list) =
   match files with
   | [] -> dependencies, parsed
   | ((File(h) | Code(h,_)) as input)::t ->
      (* check that the file has not been visited before *)
      if not (Hashtbl.mem parsed h) then
         let h_module    = moduleName h in
         let h_parsed    =
            match input with
            | File(_) -> ParserVult.parseFile h
            | Code(_,txt) -> ParserVult.parseString (Some(h)) txt
         in
         (* gets the depencies based on the modules used *)
         let h_deps =
            Common.GetDependencies.fromStmts h_parsed.presult
            |> IdSet.to_list
            |> List.map List.hd
         in
         (* finds all the files for the used modules *)
         let h_dep_files =
            List.map (findModule includes) h_deps
            |> List.filter (fun a -> a<>h)
            |> List.map (fun a -> File(a))
         in
         (* updates the tables *)
         let ()          = Hashtbl.add dependencies h_module h_deps in
         let ()          = Hashtbl.add parsed h_module h_parsed in
         loadFiles_loop includes dependencies parsed (t @ h_dep_files)
      else
         loadFiles_loop includes dependencies parsed t

(** Raises an error if the modules have circular dependencies *)
let rec checkComponents (comps:string list list) : unit =
   match comps with
   | [] -> ()
   | [_]::t -> checkComponents t
   | h::_ ->
      (* in this case one of the components has more than one module *)
      let msg = "The following modules have circular dependencies: " ^ (String.concat ", " h) in
      Error.raiseErrorMsg msg

(* Given a list of files, finds and parses all the dependencies and returns the parsed contents in order *)
let loadFiles (arguments:arguments) (files:input list) : parser_results list =
   let includes = getIncludes arguments files in
   let dependencies, parsed = loadFiles_loop includes (Hashtbl.create 8) (Hashtbl.create 8) files in
   let dep_list = Hashtbl.fold (fun a b acc -> (a,b)::acc) dependencies [] in
   let comps = Components.components dep_list in
   let () = checkComponents comps in
   let sorted_deps = List.map List.hd comps in
   List.map (fun module_name -> Hashtbl.find parsed module_name) sorted_deps

