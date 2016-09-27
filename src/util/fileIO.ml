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

(** This file provides a hackish way of replacing the file IO functions for Node.js
    implementations that in order to run Vult with node *)

let read_fun =
   ref (fun path ->
         if Sys.file_exists path then
            let file = open_in path in
            let buffer = Buffer.create 16 in
            try
               while true do
                  let c = input_char file in
                  Buffer.add_char buffer c
               done;
               None
            with | _ ->
               close_in file;
               Some(Buffer.contents buffer)
         else None)

let setRead f = read_fun := f

let read (path:string) : string option =
   !read_fun path

let write_fun =
   ref (fun path text ->
      try
         let file = open_out path in
         output_string file text;
         close_out file;
         true
      with | _ -> false)

let setWrite f = write_fun := f

let write (path:string) (text:string) : bool =
   !write_fun path text
