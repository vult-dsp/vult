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

(** Disambiguation of emitted names whose readable encodings are not injective. *)

(** Picks an emitted name for the entity identified by [owner], starting from the readable
    [base] name. [taken] reports the owner of a name that is already in use. When the
    candidate is taken by a different owner, the name is extended with a digest of the owner
    key; equality is always decided by the owner key, so even a digest collision falls
    through to the next counter value. The chosen name is not registered: the caller records
    it in its own table. *)
let disambiguate ~(taken : string -> string option) (owner : string) (base : string) : string =
  let rec claim candidate n =
    match taken candidate with
    | None ->
        candidate
    | Some existing when String.equal existing owner ->
        candidate
    | Some _ ->
        let tag = String.sub (Digest.to_hex (Digest.string (owner ^ "#" ^ string_of_int n))) 0 8 in
        claim (Printf.sprintf "%s_%s" base tag) (n + 1)
  in
  claim base 0
