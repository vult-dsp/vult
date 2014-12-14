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

open Types
open ParserVult

let parseText s =
  let p = Driver.parseStringRun (Js.to_string s) in
  match p.iresult with
  | `Ok(result) ->
  	"Results of running the program:\n"^result |> Js.string
  | `Error(_) ->
    let error_strings:string list = Errors.reportErrors p.iresult p.lines in
    let result =List.fold_left (fun s a -> s^"\n"^a) "" error_strings in
    "Errors in the program:\n"^result |> Js.string

;;

(*let _ = Js.Unsafe.set (Js.Unsafe.variable "cover_functions") (Js.string "parseText") (Js.wrap_callback parseText)*)
Js.Unsafe.global##plop <- (Js.wrap_callback parseText)