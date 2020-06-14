(*
   The MIT License (MIT)

   Copyright (c) 2020 Leonardo Laguna Ruiz

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

open Prog

type state = { s : int }

type env = { e : int }

let default_state : state = { s = 0 }

let default_env : env = { e = 0 }

module Simplify = struct
  let exp =
    Mapper.make
    @@ fun _env state e ->
    match e with
    | _ -> state, e


  let mapper = { Mapper.identity with exp }
end

let mapper = Simplify.mapper

let run (prog : prog) : prog =
  let _state, prog = Mapper.prog mapper default_env default_state prog in
  prog
