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

type t =
   | Int
   | Real
   | Id

let rec has (attr:attr_exp list) (id:id) =
   match attr with
   | [] -> false
   | AId(name,_)::_ when id = name -> true
   | AFun(name,_,_)::_ when id = name -> true
   | _::t -> has t id

let getLocation (attr:attr_exp) : Loc.t =
   match attr with
   | AInt(_,loc)   -> loc
   | AId(_,loc)    -> loc
   | AReal(_,loc)  -> loc
   | AFun(_,_,loc) -> loc

let getType (attr:attr_exp) : string =
   match attr with
   | AInt(_,_)   -> "integer"
   | AId(_,_)    -> "identifier"
   | AReal(_,_)  -> "real"
   | AFun(_,_,_) -> "attribute"

let getTypeLiteral (t:t) : string =
   match t with
   | Int  -> "integer"
   | Real -> "real"
   | Id   -> "identifier"

let rec getParam (remaining:(id * attr_exp) list) (args:(id * attr_exp) list) (id:id) =
   match args with
   | [] -> remaining, None
   | (name,value)::t when name = id ->
      (remaining@t), Some(value)
   | h::t -> getParam (h::remaining) t id

let getTypedParam (args:(id * attr_exp) list) (id,typ) =
   match getParam [] args id with
   | r, Some(AReal(_,_) as value) when typ = Real -> r, Some(value)
   | r, Some(AInt(_,_) as value)  when typ = Int  -> r, Some(value)
   | r, Some(AId(_,_) as value)   when typ = Id   -> r, Some(value)
   | _, Some(value) ->
      let loc = getLocation value in
      let msg = Printf.sprintf "The parameter '%s' was expected to be of type '%s' but it is '%s'" (PrintTypes.identifierStr id) (getTypeLiteral typ) (getType value) in
      Error.raiseError msg loc
   | r, None -> r, None

let getParameterList loc (args:(id * attr_exp) list) (params: (id * t) list) : (id * attr_exp) list * attr_exp list =
   let rec loop remaning found params =
      match params with
      | [] -> remaning, List.rev found
      | h::t ->
         match getTypedParam remaning h with
         | remaning', Some(value) ->
            loop remaning' (value::found) t
         | _, None ->
            let name, typ = h in
            let msg = Printf.sprintf "The annotation was expected to have a parameter with name '%s' and type '%s'" (PrintTypes.identifierStr name) (getTypeLiteral typ) in
            Error.raiseError msg loc
   in loop args [] params
