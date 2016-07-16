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

open Ollvm.Ez
module P = Ollvm.Printer

open CLike


let rec typ t =
   match t with
   | CTSimple("int") -> Type.i32
   | CTSimple("float") -> Type.float
   | CTSimple("bool") -> Type.i1
   | CTArray(subt,size) -> Type.array size (typ subt)
   | _ -> failwith "Vllvm.typ: unknow type"

let rec exp e =
   match e with
   | CEInt(n) -> Value.i32 n
   | CEFloat(_,f) -> Value.float f
   | CEBool(false) -> Value.i1 0
   | CEBool(true) -> Value.i1 1
   | _ -> failwith ""




let run () =
   exp (CEFloat("",1.0))
   |> snd
   |> P.value (P.empty_env ()) Format.std_formatter
