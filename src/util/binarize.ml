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

(** Converts a float to a 4-byte big-endian binary string using IEEE-754 single-precision. *)
let float_to_bin_string (n : float) : string =
  let bits : int32 = Int32.bits_of_float n in
  let b : bytes = Bytes.create 4 in
  let () = Bytes.set b 0 (Char.chr (Int32.to_int (Int32.shift_right_logical bits 24) land 0xFF)) in
  let () = Bytes.set b 1 (Char.chr (Int32.to_int (Int32.shift_right_logical bits 16) land 0xFF)) in
  let () = Bytes.set b 2 (Char.chr (Int32.to_int (Int32.shift_right_logical bits 8) land 0xFF)) in
  let () = Bytes.set b 3 (Char.chr (Int32.to_int bits land 0xFF)) in
  Bytes.to_string b

(** Converts an int to a 4-byte big-endian binary string. *)
let int_to_bin_string (n : int) : string =
  let b : bytes = Bytes.create 4 in
  let () = Bytes.set b 0 (Char.chr ((n lsr 24) land 0xFF)) in
  let () = Bytes.set b 1 (Char.chr ((n lsr 16) land 0xFF)) in
  let () = Bytes.set b 2 (Char.chr ((n lsr 8) land 0xFF)) in
  let () = Bytes.set b 3 (Char.chr (n land 0xFF)) in
  Bytes.to_string b
