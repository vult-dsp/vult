(*
The MIT License (MIT)

Copyright (c) 2014 Leonardo Laguna Ruiz, Carl Jönsson

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

open Js

(** Main file when running Vult with node. It replaces the FileIO functions to have access to the file system.
*)

(* declares the node.js object 'process' *)
class type process = object
   (* returns the current working directory *)
   method cwd : js_string t meth
end

(* declares the node.js object 'buffer' *)
class type buffer = object
   (* converts the buffer object to string *)
   method toString : js_string t meth
end

(* declares the node.js object 'fs' *)
class type fs = object
   (* returns the contents of a file *)
   method readFileSync : js_string t -> buffer t meth
   (* writes a string to a file *)
   method writeFileSync : js_string t -> js_string t -> unit t meth
   (* checks if a file exists *)
   method existsSync : js_string t -> bool t meth
end

(* declares the variable 'fs' as: fs = require('fs') *)
let fs : fs t = Unsafe.fun_call (Unsafe.js_expr "require") [|Unsafe.inject (string "fs")|]

(* declares the variable 'process' as: process = require('process') *)
let process : process t = Unsafe.fun_call (Unsafe.js_expr "require") [|Unsafe.inject (string "process")|]

(* This is a wrapper that allows calling the node.js function from ocaml *)
let read_fn (path:string) : string option =
   let exist    = fs##existsSync (string path) in
   if to_bool exist then
      let buffer   = fs##readFileSync (string path) in
      let contents =  buffer##toString in
      Some(to_string contents)
   else
      None

(* This is a wrapper that allows calling the node.js function from ocaml *)
let write_fn (path:string) (text:string) : bool =
   let _ = fs##writeFileSync (string path) (string text) in
   to_bool (fs##existsSync (string path))

(* This is a wrapper that allows calling the node.js function from ocaml *)
let exists_fn (path:string)  : bool =
   to_bool (fs##existsSync (string path))

(* This is a wrapper that allows calling the node.js function from ocaml *)
let cwd_fn (path:string)  : string =
   to_string (process##cwd)
;;

(* replaces all the native functions for the node.js versions *)
FileIO.setRead read_fn ;;
FileIO.setWrite write_fn ;;
FileIO.setExists exists_fn ;;

(* calls the main function to start the execution *)
Vult_main.main () ;;

