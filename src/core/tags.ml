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

open Prog

type t =
   | Int
   | Real
   | Id
   | String

let rec has (attr:attr_exp list) (id:Id.t) =
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
   | AString(_,loc) -> loc

let getType (attr:attr_exp) : string =
   match attr with
   | AInt(_,_)   -> "integer"
   | AId(_,_)    -> "identifier"
   | AReal(_,_)  -> "real"
   | AFun(_,_,_) -> "tag"
   | AString(_,_) -> "string"

let getTypeLiteral (t:t) : string =
   match t with
   | Int  -> "integer"
   | Real -> "real"
   | Id   -> "identifier"
   | String   -> "string"

let rec getParam (remaining:(Id.t * attr_exp) list) (args:(Id.t * attr_exp) list) (id:string) =
   match args with
   | [] -> remaining, None
   | (name,value)::t when name = [id] ->
      (remaining@t), Some(value)
   | h::t -> getParam (h::remaining) t id

let getTypedParam (args:(Id.t * attr_exp) list) (id,typ) =
   let lattr loc = { emptyAttr with loc} in
   match getParam [] args id with
   | r, Some(AReal(value,loc)) when typ = Real ->
      r, Some(PReal(float_of_string value, lattr loc))
   | r, Some(AInt(value,loc)) when typ = Int ->
      r, Some(PInt(int_of_string value, lattr loc))
   | r, Some(AId(value,loc)) when typ = Id ->
      r, Some(PId(value, lattr loc))
   | r, Some(AString(value,loc)) when typ = String ->
      r, Some(PString(value, lattr loc))

   | _, Some(value) ->
      let loc = getLocation value in
      let msg = Printf.sprintf "The parameter '%s' was expected to be of type '%s' but it is '%s'" id (getTypeLiteral typ) (getType value) in
      Error.raiseError msg loc
   | r, None -> r, None

let getParameterList (loc:Loc.t) (args:(Id.t * attr_exp) list) (params: (string * t) list) : (Id.t * attr_exp) list * exp list =
   let rec loop remaning found params =
      match params with
      | [] -> remaning, List.rev found
      | h::t ->
         match getTypedParam remaning h with
         | remaning', Some(value) ->
            loop remaning' (value::found) t
         | _, None ->
            let name, typ = h in
            let msg = Printf.sprintf "The tag was expected to have a parameter with name '%s' and type '%s'" name (getTypeLiteral typ) in
            Error.raiseError msg loc
   in loop args [] params


let getTableIndividualParams (loc:Loc.t) params msg args =
   let remaining, found = getParameterList loc args params in
   match remaining with
   | _::_ ->
      let params_s =  List.map (fun (id,_) -> PrintProg.identifierStr id) remaining |> String.concat ", " in
      let msg = "The following arguments are unknown for the current tag: "^ params_s in
      Error.raiseError msg loc
   | [] ->
      if List.length found = List.length params then
         found
      else
         Error.raiseError msg loc

let rec getTableParams (fname:string) (params:(string * t) list) (msg:string) (attr:attr_exp list) =
   match attr with
   | [] -> None
   | AFun(name,args,loc)::_ when name = [fname] ->
      Some(loc, getTableIndividualParams loc params msg args)
   | _ :: t -> getTableParams fname params msg t

let rec removeAttrFunc (fname:string) (attr:attr_exp list) : attr_exp list =
   match attr with
   | [] -> []
   | AFun(name,_,_)::t when name = [fname] ->
      removeAttrFunc fname t
   | h::t -> h :: (removeAttrFunc fname t)
