(*
   The MIT License (MIT)

   Copyright (c) 2017 Leonardo Laguna Ruiz

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
module IdMap = CCMap.Make (struct
      type t = Id.t

      let compare = compare
   end)

module PathMap = CCMap.Make (struct
      type t = Id.path

      let compare = compare
   end)

module PathSet = CCSet.Make (struct
      type t = Id.path

      let compare = compare
   end)

module IdSet = CCSet.Make (struct
      type t = Id.t

      let compare = compare
   end)

type id_type = Id.t * Typ.t [@@deriving show, eq, ord]

module IdTypeSet = CCSet.Make (struct
      type t = id_type

      let compare = compare_id_type
   end)

module TypeSet = CCSet.Make (struct
      type t = Typ.t

      let compare = Typ.compare
   end)

module TypeMap = CCMap.Make (struct
      type t = Typ.t

      let compare = Typ.compare
   end)
