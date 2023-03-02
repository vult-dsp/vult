(*
   The MIT License (MIT)
   Copyright (c) 2014 Leonardo Laguna Ruiz
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

open Js_of_ocaml
open Util

(** Main file when running Vult with node. It replaces the FileIO functions to have access to the file system.
*)

(* declares the node.js object 'process' *)
class type process =
  object
    (* returns the current working directory *)
    method cwd : Js.js_string Js.t Js.meth
  end

(* declares the node.js object 'buffer' *)
class type buffer =
  object
    (* converts the buffer object to string *)
    method toString : Js.js_string Js.t -> Js.js_string Js.t Js.meth
    method length : int Js.readonly_prop
  end

(* declares the node.js object 'fs' *)
class type fs =
  object
    (* returns the contents of a file *)
    method readFileSync : Js.js_string Js.t -> buffer Js.t Js.meth

    (* writes a string to a file *)
    method writeFileSync : Js.js_string Js.t -> Js.js_string Js.t -> unit Js.t Js.meth

    (* checks if a file exists *)
    method existsSync : Js.js_string Js.t -> bool Js.t Js.meth
  end

(* declares the variable 'fs' as: fs = require('fs') *)
let fs : fs Js.t = Js.Unsafe.fun_call (Js.Unsafe.js_expr "require") [| Js.Unsafe.inject (Js.string "fs") |]

(* declares the variable 'process' as: process = require('process') *)
let process : process Js.t = Js.Unsafe.js_expr "process"

let rec copyData size bytes array index =
  if index > size then
    bytes
  else (
    match Js.Optdef.to_option (Js.array_get array index) with
    | Some value ->
      Bytes.set bytes index value;
      copyData size bytes array (index + 1)
    | None -> bytes)


(* This is a wrapper that allows calling the node.js function from ocaml *)
let read_bytes_fn (path : string) : Buffer.t option =
  let exist = fs##existsSync (Js.string path) in
  if Js.to_bool exist then (
    let buffer = fs##readFileSync (Js.string path) in
    let size = buffer##.length in
    let array = Js.Unsafe.coerce buffer in
    let bytes = Bytes.create size in
    let contents = copyData size bytes array 0 in
    let buffer = Buffer.create size in
    let () = Buffer.add_bytes buffer contents in
    Some buffer)
  else
    None


(* This is a wrapper that allows calling the node.js function from ocaml *)
let read_fn (path : string) : string option =
  let exist = fs##existsSync (Js.string path) in
  if Js.to_bool exist then (
    let buffer = fs##readFileSync (Js.string path) in
    let contents = buffer##toString (Js.string "ascii") in
    Some (Js.to_string contents))
  else
    None


(* This is a wrapper that allows calling the node.js function from ocaml *)
let write_fn (path : string) (text : string) : bool =
  let _ = fs##writeFileSync (Js.string path) (Js.string text) in
  Js.to_bool (fs##existsSync (Js.string path))


(* This is a wrapper that allows calling the node.js function from ocaml *)
let exists_fn (path : string) : bool = Js.to_bool (fs##existsSync (Js.string path))

(* This is a wrapper that allows calling the node.js function from ocaml *)
let cwd_fn () : string = Js.to_string process##cwd

(* replaces all the native functions for the node.js versions *)
let replaceFunctions () =
  FileIO.setRead read_fn;
  FileIO.setWrite write_fn;
  FileIO.setExists exists_fn;
  FileIO.setCwd cwd_fn;
  FileIO.setReadBytes read_bytes_fn
