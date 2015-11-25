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

(** Transformations and optimizations of the syntax tree *)

open TypesVult
open Env

module PassData = struct

   type t =
      {
         gen_init_ctx : IdSet.t; (** Context for which a init function has been generated *)
      }

   let hasInitFunction (t:t) (id:id) : bool =
      IdSet.mem id t.gen_init_ctx

   let markInitFunction (t:t) (id:id) : t =
      { gen_init_ctx = IdSet.add id t.gen_init_ctx }

   let empty = { gen_init_ctx = IdSet.empty }

end



(*
module ConstantSimplification = struct
   let biOpReal (op:string) : float -> float -> float =
      match op with
      | "+" -> ( +. )
      | "-" -> ( -. )
      | "*" -> ( *. )
      | "/" -> ( /. )
      | _ -> failwith (Printf.sprintf "biOpReal: Unknown operator %s" op)

   let biOpInt (op:string) : int -> int -> int =
      match op with
      | "+" -> ( + )
      | "-" -> ( - )
      | "*" -> ( * )
      | "/" -> ( / )
      | _ -> failwith (Printf.sprintf "biOpReal: Unknown operator %s" op)

   let exp : ('a,exp) Mapper.mapper_func =
      Mapper.make @@ fun state e ->
         match e with
         | POp(op,[PInt(v1,_);PInt(v2,_)],attr) ->
            let f = biOpInt op in
            state,PInt(f v1 v2,attr)
         | POp(op,[PInt(v1,_);PReal(v2,_)],attr) ->
            let f = biOpReal op in
            state,PReal(f (float_of_int v1) v2,attr)
         | POp(op,[PReal(v1,_);PInt(v2,_)],attr) ->
            let f = biOpReal op in
            state,PReal(f v1 (float_of_int v2),attr)
         | POp(op,[PReal(v1,_);PReal(v2,_)],attr) ->
            let f = biOpReal op in
            state,PReal(f v1 v2,attr)
         | _ -> state,e

   let mapper =
      { Mapper.default_mapper with Mapper.exp = exp }
end
*)
module CollectContext = struct

   let lhs_exp : ('a Env.t,lhs_exp) Mapper.mapper_func =
      Mapper.make @@ fun state exp ->
         match exp with
         | LId(id,_) ->
            let tp = TId(["real"],emptyAttr) in
            let state' = Env.addMemToContext state id tp in
            state',exp
         (*
         | LTyped(LId(id,_),tp,_) -> (** Currently the id's are untyped *)
            let state' = Env.addMemToContext state id in
            state',exp
         *)
         | _ -> state,exp

   let reg_mem_mapper =
      { Mapper.default_mapper with Mapper.lhs_exp = lhs_exp }

   let stmt : ('a Env.t,stmt) Mapper.mapper_func =
      Mapper.make @@ fun state stmt ->
         match stmt with
         | StmtFun(_,_,_,_,attr) when attr.fun_and ->
            let state' = Env.includeFunctionInContext state in
            state',stmt
         | StmtFun(_,_,_,_,_) ->
            let state' = Env.makeNewContext state in
            state',stmt
         | StmtMem(lhs,_,_,_) ->
            let state',_ = Mapper.map_lhs_exp reg_mem_mapper state lhs in
            state',stmt
         | _ -> state,stmt

   let exp : ('a Env.t,exp) Mapper.mapper_func =
      Mapper.make @@ fun state exp ->
         match exp with
         | PCall(id,kind,args,attr) ->
            let state',id' = Env.addInstanceToContext state id kind in
            state',PCall(id',kind,args,attr)
         | _ -> state,exp

   let mapper =
      { Mapper.default_mapper with Mapper.stmt = stmt; Mapper.exp = exp  }

end

module InsertContext = struct

   let stmt : ('a Env.t,stmt) Mapper.mapper_func =
      Mapper.make @@ fun state stmt ->
         match stmt with
         | StmtFun(name,args,body,rettype,attr) ->
            if Env.isActive state name then
               let context = Env.getContext state name in
               let arg0 = TypedId(["$ctx"],TId(context,attr),attr) in
               state,StmtFun(name,arg0::args,body,rettype,attr)
            else
               state, stmt
         | _ -> state, stmt

   let exp : ('a Env.t,exp) Mapper.mapper_func =
      Mapper.make @@ fun state exp ->
         match exp with
         | PCall(Some(id),kind,args,attr) ->
            state,PCall(None,kind,PId(id,attr)::args,attr)
         | PId(id,attr) when Env.isLocalInstanceOrMem state id ->
            state, PId("$ctx"::id,attr)
         | _ -> state,exp

   let lhs_exp : ('a Env.t,lhs_exp) Mapper.mapper_func =
      Mapper.make @@ fun state exp ->
         match exp with
         | LId(id,attr) when Env.isLocalInstanceOrMem state id ->
            state, LId("$ctx"::id,attr)
         | _ -> state,exp

   let mapper =
      { Mapper.default_mapper with Mapper.stmt = stmt; Mapper.exp = exp; Mapper.lhs_exp = lhs_exp }
end

(** Removes type information until type inference is in place *)
module Untype = struct

   let lhs_exp : ('a Env.t,lhs_exp) Mapper.mapper_func =
      Mapper.make @@ fun state exp ->
         match exp with
         | LTyped(e,_,_) -> state,e
         | _ -> state,exp

   let mapper =
      { Mapper.default_mapper with Mapper.lhs_exp = lhs_exp }

end

(** Splits mem declarations with binding to two statements *)
module SplitMem = struct

   let stmt_x : ('a Env.t,stmt) Mapper.expand_func =
      Mapper.makeExpander @@ fun state stmt ->
         match stmt with
         | StmtMem(lhs,init,Some(rhs),attr) ->
            state, [ StmtMem(lhs,init,None,attr); StmtBind(lhs,rhs,attr) ]
         | _ -> state, [stmt]

   let mapper =
      { Mapper.default_mapper with Mapper.stmt_x = stmt_x }

end

module CreateInitFunction = struct

   module StmtSet = Set.Make(struct type t = stmt let compare = compare_stmt end)

   let rec getInitFunctioName (id:id) : id =
      match id with
      | [] -> failwith "getInitFunctioName: empty id"
      | [last] -> [ last^"_init" ]
      | h::t -> h :: (getInitFunctioName t)

   let getInitValue (tp:type_exp) : exp =
      match tp with
      | TId(["real"],_) -> PReal(0.0,emptyAttr)
      | TId(["int"],_) -> PInt(0,emptyAttr)
      | _ -> PReal(0.0,emptyAttr)

   let callInitFunction state (inst:id) (tp:type_exp) : exp =
      match tp with
      | TId(name,_) ->
         let fun_ctx = Env.getContext state name in
         PCall(None,getInitFunctioName fun_ctx,[PId(inst,emptyAttr)],emptyAttr)
      | _ -> failwith "CreateInitFunction.callInitFunction: cannot initialize this yet"

   let generateInitFunction (state:'a Env.t) (name:id) : stmt =
      let mem_vars, instances = Env.getMemAndInstances state name in
      let ctx = Env.getContext state name in
      let ctx_name = ["$ctx"] in
      let new_stmts_set =
         IdTypeSet.fold
            (fun (name,tp) acc ->
               let new_stmt = StmtBind(LId(ctx_name @ name,emptyAttr), getInitValue tp, emptyAttr) in
               StmtSet.add new_stmt acc)
            mem_vars StmtSet.empty
      in
      let new_stmts_set' =
         IdTypeSet.fold
            (fun (name,tp) acc ->
               let new_stmt = StmtBind(LWild(emptyAttr), callInitFunction state (ctx_name @ name) tp, emptyAttr) in
               StmtSet.add new_stmt acc)
            instances new_stmts_set
      in
      let stmts = StmtSet.fold (fun a acc -> a::acc) new_stmts_set' [] in
      StmtFun(getInitFunctioName ctx, [TypedId(ctx_name,TId(ctx,emptyAttr),emptyAttr)], StmtBlock(None, stmts, emptyAttr), None, andAttr)

   let stmt_x : ('a Env.t,stmt) Mapper.expand_func =
      Mapper.makeExpander @@ fun state stmt ->
         match stmt with
         | StmtFun(name,_,_,_,_) ->
            if Env.isActive state name then
               let data = Env.get state in
               let ctx = Env.getContext state name in
               if PassData.hasInitFunction data ctx then
                  state, [stmt]
               else
                  let init_funct = generateInitFunction state name in
                  let data' = PassData.markInitFunction data ctx in
                  Env.set state data', [stmt; init_funct]
            else
               state, [stmt]

         | _ -> state, [stmt]

   let mapper =
      { Mapper.default_mapper with Mapper.stmt_x = stmt_x }

end


(* Basic transformations *)
let pass1 (state,stmts) =
   let mapper =
      Untype.mapper
      |> Mapper.seq SplitMem.mapper
      |> Mapper.seq CollectContext.mapper
      |> Mapper.seq InsertContext.mapper
   in
   Mapper.map_stmt_list mapper state stmts

let pass2 (state,stmts) =
   let mapper =
      InsertContext.mapper
      |> Mapper.seq CreateInitFunction.mapper
   in
   Mapper.map_stmt_list mapper state stmts

let dump (state,stmts) =
   Env.dump state;
   state,stmts

let applyTransformations (results:parser_results) =
   let module_name =
      results.file
      |> Filename.basename
      |> Filename.chop_extension
      |> String.capitalize
      |> fun a -> [a]
   in
   let initial_state =
      Env.empty module_name PassData.empty
      |> Env.makeNewContext
   in

   let passes stmts =
      (initial_state,stmts)
      |> pass1
      |> dump
      |> pass2
      |> dump
      |> snd
   in

   let new_stmts = CCError.map passes results.presult in
   { results with presult = new_stmts }
