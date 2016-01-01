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

open TypesVult

let unit_type   () = ref (TId(["unit"],None))
let bool_type   () = ref (TId(["bool"],None))
let int_type    () = ref (TId(["int"],None))
let real_type   () = ref (TId(["real"],None))
let string_type () = ref (TId(["string"],None))

let num_type    () = ref (TExpAlt([real_type(); int_type()]))

let num_num () =
   let num = num_type () in
   ref (TArrow(num,num,None))

let num_num_num () =
   let num = num_type () in
   ref (TArrow(num,ref (TArrow(num,num,None)),None))

let num_num_bool () =
   let num = num_type () in
   ref (TArrow(num,ref (TArrow(num,bool_type (),None)),None))

let num_num_num_num () =
   let num = num_type () in
   ref (TArrow(num,ref (TArrow(num,ref (TArrow(num,num,None)),None)),None))

let bool_bool () = ref (TArrow(bool_type(),bool_type(),None))

let bool_bool_bool () =
   ref (TArrow(bool_type(),ref (TArrow(bool_type(),bool_type (),None)),None))
