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

open Env
open Args
open Maps

type pass_options =
   {
      eval  : bool;
      pass1 : bool;
      pass2 : bool;
      pass3 : bool;
      pass4 : bool;
      pass5 : bool;
      tuples: bool;
   }

let default_options =
   {
      eval  = true;
      pass1 = true;
      pass2 = true;
      pass3 = true;
      pass4 = true;
      pass5 = true;
      tuples = true;
   }

let interpreter_options =
   {
      eval  = true;
      pass1 = true;
      pass2 = false;
      pass3 = false;
      pass4 = false;
      pass5 = false;
      tuples = false;
   }

module PassData = struct

   type t =
      {
         gen_init_ctx : PathSet.t; (** Context for which a init function has been generated *)
         add_ctx      : PathSet.t;
         used_tuples  : TypeSet.t;
         repeat       : bool;
         args         : args;
         interp_env   : Interpreter.Env.env;
         used_code    : bool IdMap.t;
      }

   let hasInitFunction (t:t) (path:Id.path) : bool =
      PathSet.mem path t.gen_init_ctx

   let hasContextArgument (t:t) (path:Id.path) : bool =
      PathSet.mem path t.add_ctx

   let markInitFunction (t:t) (path:Id.path) : t =
      { t with gen_init_ctx = PathSet.add path t.gen_init_ctx }

   let markContextArgument (t:t) (path:Id.path) : t =
      { t with add_ctx = PathSet.add path t.add_ctx }

   let reapply (t:t) : t =
      { t with repeat = true }

   let reset (t:t) : t =
      { t with repeat = false }

   let shouldReapply (t:t) : bool =
      t.repeat

   let addTuple (t:t) (tup:Typ.t) : t =
      { t with used_tuples = TypeSet.add tup t.used_tuples }

   let getTuples (t:t) : TypeSet.t =
      t.used_tuples

   let addRoot (t:t) (id:Id.t) : t =
      { t with used_code = IdMap.add id false t.used_code }

   let markRoot (t:t) (id:Id.t) : t =
      { t with used_code = IdMap.add id true t.used_code }

   let empty args =
      let roots =
         List.flatten @@
         List.map
            (fun a ->
                let id = Parser.parseId a in
                [id, false; Id.postfix id "_init", false])
            args.roots
      in
      {
         gen_init_ctx = PathSet.empty;
         repeat       = false;
         add_ctx      = PathSet.empty;
         used_tuples  = TypeSet.empty;
         args         = args;
         interp_env   = Interpreter.getEnv ();
         used_code    = IdMap.of_list roots;
      }

end

let reapply (state:PassData.t Env.t) : PassData.t Env.t =
   let data = Env.get state in
   Env.set state (PassData.reapply data)

let reset (state:PassData.t Env.t) : PassData.t Env.t =
   let data = Env.get state in
   Env.set state (PassData.reset data)

let shouldReapply (state:PassData.t Env.t) : bool =
   PassData.shouldReapply (Env.get state)

let newState (state:'a Env.t) (data:'b) : 'b Env.t =
   Env.derive state data

let restoreState (original:'a Env.t) (current:'b Env.t) : 'a Env.t * 'b =
   let current_data = Env.get current in
   let original_data = Env.get original in
   Env.derive current original_data, current_data
