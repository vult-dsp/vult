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

module SimpleMap = CCMap.Make(String)
module PairMap = CCMap.Make(struct type t = string * string let compare = compare end)

let makeKeywords = SimpleMap.of_list

let makeTypes = SimpleMap.of_list

let makeCasts = PairMap.of_list

let makeFunctions = PairMap.of_list

let makeOperators = PairMap.of_list

let makeArrayInitializations = SimpleMap.of_list

let makeRealToString = SimpleMap.of_list

type t =
   {
      keywords   : string SimpleMap.t;
      types      : string SimpleMap.t;
      cast       : string PairMap.t;
      op_to_fun  : string PairMap.t;
      op_to_op   : string PairMap.t;
      fun_to_fun : string PairMap.t;
      array_init : string SimpleMap.t;
      real_string: (float -> string) SimpleMap.t;
   }

let global_replacement_map :  (t SimpleMap.t) ref = ref (SimpleMap.empty)

let registerReplacements (name:string) (t:t) : unit =
   global_replacement_map := SimpleMap.add name t !global_replacement_map

let getReplacements (name:string) : t =
   SimpleMap.find name !global_replacement_map

(**
   Auxiliary function to merge two maps.
   Note: this function gives preference to the value in the second map,
   that way as maps are merged the last value added is kept.
*)
let takeSecond _ (opt1:'a option) (opt2:'a option) : 'a option =
   match opt1, opt2 with
   | _, Some(_) -> opt2
   | _ -> opt1

let extendReplacements (first:t) (second:t) : t =
   {
      keywords    = SimpleMap.merge takeSecond first.keywords    second.keywords;
      types       = SimpleMap.merge takeSecond first.types       second.types;
      cast        = PairMap.merge   takeSecond first.cast        second.cast;
      op_to_fun   = PairMap.merge   takeSecond first.op_to_fun   second.op_to_fun;
      op_to_op    = PairMap.merge   takeSecond first.op_to_op    second.op_to_op;
      fun_to_fun  = PairMap.merge   takeSecond first.fun_to_fun  second.fun_to_fun;
      array_init  = SimpleMap.merge takeSecond first.array_init  second.array_init;
      real_string = SimpleMap.merge takeSecond first.real_string second.real_string;
   }

let empty =
   {
      keywords    = SimpleMap.empty;
      types       = SimpleMap.empty;
      cast        = PairMap.empty;
      op_to_fun   = PairMap.empty;
      op_to_op    = PairMap.empty;
      fun_to_fun  = PairMap.empty;
      array_init  = SimpleMap.empty;
      real_string = SimpleMap.empty;
   }

let getKeyword (t:t) (name:string) : string =
   match SimpleMap.get name t.keywords with
   | Some(new_keyword) -> new_keyword
   | None -> name

let getType (t:t) (name:string) : string =
   match SimpleMap.get name t.types with
   | Some(new_type) -> new_type
   | None -> name

let getCast (t:t) (from_t:string) (to_t:string) : string =
   let to_tt   = getType t to_t in
   let from_tt = getType t from_t in
   match PairMap.get (from_tt,to_tt) t.cast with
   | Some(cast) -> cast
   | None -> to_tt

let getFunctionForOperator (t:t) (op:string) (typ:string) : string option =
   let typ_t = getType t typ in
   PairMap.get (op,typ_t) t.op_to_fun

let getOperator (t:t) (op:string) (typ:string) : string =
   let typ_t = getType t typ in
   match PairMap.get (op,typ_t) t.op_to_op with
   | Some(new_op) -> new_op
   | None -> op

let getArrayInit (t:t) (name:string) : string option =
   SimpleMap.get name t.array_init

let getFunction (t:t) (op:string) (typ:string) : string =
   let typ_t = getType t typ in
   match PairMap.get (op,typ_t) t.fun_to_fun with
   | Some(new_op) -> new_op
   | None -> op

let getRealToString (t:t) (value:float) (typ:string) : string =
   let typ_t = getType t typ in
   match SimpleMap.get typ_t t.real_string with
   | Some(fn) -> fn value
   | None -> string_of_float value

