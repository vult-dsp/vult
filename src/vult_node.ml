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

open TypesVult

let generateJSCode s =
   Driver.parseStringGenerateCode
      ({ default_arguments with jscode = true; real = "js";template = "browser"; files = ["live"]})
      (Js.to_string s)
   |> Js.string

let parsePrint s =
   Driver.parsePrintCode (Js.to_string s) |> Js.string

let checkCode s =
   Driver.checkCode (Js.to_string s)
   |> List.map
      (fun (msg,_,line,col) ->
          Js.Unsafe.obj
             [| ("text",Js.Unsafe.inject (Js.string msg));
                ("row",Js.Unsafe.inject (Js.string (string_of_int (line - 1))));
                ("column",Js.Unsafe.inject (Js.string (string_of_int (col - 1))));
                ("type",Js.Unsafe.inject (Js.string "error"));
                ("raw",Js.Unsafe.inject (Js.string msg));
             |])
   |> Array.of_list |> Js.array
;;

Js.Unsafe.set Js.Unsafe.global "plop" (Js.wrap_callback parsePrint) ;;
Js.Unsafe.set Js.Unsafe.global "jscode" (Js.wrap_callback generateJSCode) ;;
Js.Unsafe.set Js.Unsafe.global "checkCode" (Js.wrap_callback checkCode) ;;

