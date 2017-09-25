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

open PassCommon
open Env
open Prog
open Args
open Maps

let getInterpEnv state =
   let env = (Env.get state).PassData.interp_env in
   match Id.getPathModule (Env.currentScope state) with
   | None-> env
   | Some(module_) -> Interpreter.Env.enterModule env [module_]

module Evaluate = struct

   let avoid = IdSet.of_list [["random"]; ["irandom"]; ["log"]]

   let isConst exp =
      match exp with
      | PInt _ | PReal _ | PBool _ -> true
      | _ -> false

   let markAsEvaluated (call:exp) : exp =
      match call with
      | PCall(inst, name, args, attr) ->
         let attr = { attr with evaluated = true } in
         PCall(inst, name, args, attr)
      | _ -> call

   let exp : ('a Env.t, exp) Mapper.mapper_func =
      Mapper.make "Simplify.exp" @@ fun state exp ->
      match exp with
      | PCall(None, name, args, attr) when
           not attr.evaluated
           && List.for_all isConst args
           && not (IdSet.mem name avoid) ->
         let env     = getInterpEnv state in
         begin match Interpreter.evalExp env exp with
            | new_exp ->
               state, markAsEvaluated new_exp
            | exception Interpreter.Abort ->
               state, markAsEvaluated exp
         end
      | PReal(v, attr) ->
         begin
            match classify_float v with
            | FP_normal -> state, exp
            | FP_subnormal -> state, exp
            | FP_zero -> state, exp
            | FP_infinite -> state, PReal(3.40282347E+38, attr)
            | FP_nan ->
               let msg = "The number is NaN, this can be the result of a simplification" in
               Error.raiseError msg attr.loc
         end
      | _ -> state, exp

   let mapper = Mapper.{ default_mapper with exp = exp }

end

(*
module Inline = struct

   let makePrefix (attr:attr) : string=
      let line = Loc.line attr.loc in
      let col = Loc.startColumn attr.loc in
      Printf.sprintf "_inlined_%i_%i" line col

   let inline prefix (args:exp list) (fargs:typed_id list) =
      List.map2
         (fun lhs rhs ->
             match lhs with
             | SimpleId(lhs, _, _) -> failwith ""
             | TypedId(lhs,typ, _,attr) ->
                let lhs_exp =LId(Id.postfix lhs prefix, Some typ, attr) in
                StmtVal(lhs_exp, Some rhs, attr))
         fargs args

   let exp : ('a Env.t,exp) Mapper.mapper_func =
      Mapper.make "Simplify.exp" @@ fun state exp ->
      match exp with
      | PCall(None, name, args, attr) when not (Env.isBuiltin name) ->
         let env = getInterpEnv state in
         let _ = Env.lookupRaise Scope.Function state name attr.loc in
         let exp =
            match Interpreter.Env.lookupFunction env name with
            | Some (_, (Interpreter.Env.Declared (StmtFun(_, fargs, body, _, fattr)))) when Tags.is_empty fattr.tags ->
               let prefix = makePrefix attr in
               let r = inline prefix args fargs in
               PSeq(None, StmtBlock(None, r, attr), attr)
            | _ -> PCall(None, name, args, { attr with no_inline = true })
         in
         state, exp

      | _ -> state, exp

   let mapper = Mapper.{ default_mapper with exp = exp }

end
*)

module Tables = struct

   let int_type = Typ.Const.int_type
   let real_type = Typ.Const.real_type
   let attr_int = { emptyAttr with typ = Some(int_type) }
   let attr_real = { emptyAttr with typ = Some(real_type) }

   let real_array_type size =
      ref (Typ.TComposed(["array"], [real_type; ref (Typ.TInt(size, None))], None))

   let attr_array size =
      { emptyAttr with typ = Some(real_array_type size) }

   let makeFloat loc x =
      PReal(x, { emptyAttr with loc })

   let makeDecl attr fname name data =
      let varname = Id.joinSep "_" fname name in
      let size = List.length data in
      let atype = real_array_type size in
      let arr = PArray((CCList.map (makeFloat attr.loc) data |> Array.of_list), { attr with typ = Some(atype) }) in
      StmtVal(LId(varname, Some [atype], attr_array size), Some(arr), { emptyAttr with const = true})

end

module MakeTables = struct

   let getFloat x =
      match x with
      | PReal(value, _) -> value
      | _ -> failwith "The result of the evaluation is not a float"

   let getCoefficients l =
      match l with
      | [x1; x2; x3] -> x1, x2, x3
      | _ -> failwith "the curve fitting returned more than three points"

   let checkRealReturn typ : bool =
      match typ with
      | None -> failwith "the type is not defined"
      | Some(t) -> Typ.isRealType t

   let getInputVar arg =
      match arg with
      | SimpleId(id, _, _)
      | TypedId(id, _, _, _) -> PId(id,Tables.attr_real)

   let makeNewBody fname size min max input =
      let lindex = LId(["index"],Some [Tables.int_type], Tables.attr_int) in
      let rindex = PId(["index"], Tables.attr_int) in
      let getCoeff a =
         let arr = PCall(None, ["wrap_array"], [PId(Id.joinSep "_" fname [a], Tables.attr_array size)], Tables.attr_real) in
         PIndex(arr, rindex, Tables.attr_real)
      in
      let initial_index = PReal(((float_of_int size) -. 1.0) /. (max -. min), Tables.attr_real) in
      StmtBlock(
         None,
         [
            StmtVal(lindex,None,emptyAttr);
            StmtBind(lindex,
                     PCall(None, ["clip"],
                           [
                              PCall(None, ["int"],
                                    [POp("*",
                                         [
                                            initial_index;
                                            POp("-", [input; PReal(min,Tables.attr_real)], Tables.attr_real)
                                         ], Tables.attr_real)], Tables.attr_int);
                              PInt(0, Tables.attr_int);
                              PInt(size-1, Tables.attr_int);
                           ], Tables.attr_int),
                     emptyAttr);
            StmtReturn(
               POp("+",
                   [getCoeff "c0";
                    POp("*", [input;
                              (POp("+",
                                   [getCoeff "c1";
                                    POp("*",
                                        [getCoeff "c2";input], Tables.attr_real)], Tables.attr_real))],
                        Tables.attr_real)], Tables.attr_real),emptyAttr)
         ], emptyAttr)

   let evaluateFunction env (name:Id.t) (x:float) =
      match Id.getNameNoModule name with
      | Some fname ->
         let exp = PCall(None, [fname], [PReal(x, emptyAttr)], emptyAttr) in
         let value = Interpreter.evalExp env exp in
         getFloat value
      | _ -> failwith "evaluateFunction: the function should be a full path"

   let rec fitData data index acc0 acc1 acc2 =
      if index < 0 then
         acc0, acc1, acc2
      else
         let p1 = Array.get data (index *  2) in
         let p2 = Array.get data (index * 2 + 1) in
         let p3 = Array.get data (index * 2 + 2) in
         let x = [ fst p1; fst p2; fst p3] in
         let y = [ snd p1; snd p2; snd p3] in
         let c0, c1, c2 = Fitting.lagrange x y |> getCoefficients in
         fitData data (index-1) (c0::acc0) (c1::acc1) (c2::acc2)

   let calculateTables env attr name size min max =
      let map x x0 x1 y0 y1 = (x -. x0) *. (y1 -. y0) /. (x1 -. x0) +. y0 in
      let map_x x = map x 0. (float_of_int size) min max in
      let data =
         Array.init ((size * 2) + 4)
            (fun i ->
                let x = map_x ((float_of_int i) /. 2.0) in
                x, evaluateFunction env name x
            )
      in
      let acc0, acc1, acc2 = fitData data size [] [] [] in
      [
         Tables.makeDecl attr name ["c0"] acc0;
         Tables.makeDecl attr name ["c1"] acc1;
         Tables.makeDecl attr name ["c2"] acc2
      ]

   let checkInputVariables loc args =
      match args with
      | [ var ] -> getInputVar var
      | _ ->
         let msg = "This attribute requires the function to have only one argument:\n\"fun foo(x:real) : real\"" in
         Error.raiseError msg loc

   let stmt_x : ('a Env.t,stmt) Mapper.expand_func =
      Mapper.makeExpander "MakeTables.stmt_x" @@ fun state stmt ->
      match stmt with
      | StmtFun(name, args, _, ret, attr) ->
         begin
            let params = Tags.["size", Int; "min", Real; "max", Real] in
            let msg    = "The attribute 'table' requires specific parameters. e.g. 'table(size=128,min=0.0,max=1.0)'" in
            match Tags.getTableParams "table" params msg attr.tags with
            | None -> state, [stmt]
            | Some(_, [PInt(size, _); PReal(min, _); PReal(max, _)]) when checkRealReturn ret ->
               let var    = checkInputVariables attr.loc args in
               let env           = getInterpEnv state in
               let Id.Path(path) = Env.currentScope state in
               let full_path     = path@name in
               let result        = calculateTables env attr full_path size min max in
               let attr'         = { attr with tags = Tags.removeAttrFunc "table" attr.tags } in
               let body'         = makeNewBody full_path size min max var in
               reapply state, result @ [StmtFun(name, args, body', ret, attr')]
            | Some(loc, _) ->
               let msg = "This attribute can only be applied to functions returning 'real'" in
               Error.raiseError msg loc
         end
      | _ -> state, [stmt]

   let mapper = Mapper.{ default_mapper with stmt_x }

end

module EmbedWavFile = struct

   let readFile (loc:Loc.t) (includes:string list) (file:string) : WavFile.wave =
      match FileIO.findFile includes file with
      | Some filename ->
         begin match WavFile.read filename with
            | Result.Ok (wave) -> wave
            | Result.Error read_msg ->
               let msg = "Failed to read the wav file '" ^ file ^ "': " ^ read_msg in
               Error.raiseError msg loc
         end
      | None ->
         let msg = "The file '"^file ^ "' was not found in any of the include locations" in
         Error.raiseError msg loc


   let checkNumberOfChannels (loc:Loc.t) (channels:int) (wave:WavFile.wave) : unit =
      if wave.WavFile.channels <> channels then
         let msg = "The given number of channels (" ^ (string_of_int channels) ^ ") does not match the actual number of the channels in the file (" ^ (string_of_int wave.WavFile.channels) ^ ")" in
         Error.raiseError msg loc


   let getDeclarations (attr:attr) (name:Id.t) (wav_data:WavFile.wave) : stmt list =
      Array.mapi
         (fun i v -> Tables.makeDecl attr name ["chan_" ^ (string_of_int i)] (Array.to_list v))
         wav_data.WavFile.data
      |> Array.to_list


   (** Verifies that the arguments of the attribute correspond to the necessary *)
   let checkInputVariables (loc:Loc.t) (args:typed_id list) : exp * exp =
      match args with
      | [ channel ; index ] -> MakeTables.getInputVar channel, MakeTables.getInputVar index
      | _ ->
         let msg = "This attribute requires the function to have the following arguments:\n\"external wave(channel:int, index:int) : real\"" in
         Error.raiseError msg loc


   (** Generates the statement that reads the arrays if the reuqested channel matches *)
   let accessChannel (fname:Id.t) (attr:attr) (channel:exp) (index:exp) (samples:int) (i:int) : stmt =
      let attr_bool  = { emptyAttr with typ = Some(Typ.Const.bool_type) } in
      let attr_real  = { emptyAttr with typ = Some(Typ.Const.real_type) } in
      let attr_int   = { emptyAttr with typ = Some(Typ.Const.int_type) } in
      let table_name = Id.joinSep "_" fname ["chan_" ^ (string_of_int i)] in
      let table      = PCall(None, ["wrap_array"], [PId(table_name, Tables.attr_array samples)], attr_real) in
      let i          = PInt(i, Tables.attr_int) in
      let samples_e  = PInt(samples, Tables.attr_int) in
      StmtIf(
         POp("==", [channel; i],attr_bool),
         StmtReturn(PIndex(table, POp("%", [index; samples_e],attr_int), attr_real),attr),
         None,
         attr)


   (** Generates the function that access the data of the wave file *)
   let makeNewBody (fname:Id.t) (attr:attr) (args:typed_id list) (wave:WavFile.wave) : stmt =
      let attr_real  = { emptyAttr with typ = Some(Typ.Const.real_type) } in
      let channel, index = checkInputVariables attr.loc args in
      let stmts   = CCList.init wave.WavFile.channels (accessChannel fname attr channel index  wave.WavFile.samples) in
      let default = StmtReturn( PReal(0.0,attr_real),attr) in
      StmtBlock(None, stmts @ [default], attr)


   (** Generates a function <name>_samples that return the size of the wav file *)
   let makeSizeFunction (fname:Id.t) (attr:attr) (size:int) : stmt =
      let attr_int = { emptyAttr with typ = Some(Typ.Const.int_type) } in
      let size_name = Id.postfix fname "_samples" in
      StmtFun(size_name, [], StmtReturn(PInt(size,attr_int),attr), Some(Typ.Const.int_type), attr)


   let stmt_x : ('a Env.t,stmt) Mapper.expand_func =
      Mapper.makeExpander "MakeTables.stmt_x" @@ fun state stmt ->
      match stmt with
      | StmtExternal(name, args, ret, _, attr) ->
         begin
            let params = Tags.["channels", Int; "file", String] in
            let msg    = "The attribute 'wave' requires specific parameters. e.g. 'wave(channels=1,file=\"file.wav\")'" in
            match Tags.getTableParams "wave" params msg attr.tags with
            | None -> state, [stmt]
            | Some(loc, [PInt(channels, _); PString(file, _)]) when Typ.isRealType ret ->
               let Id.Path(path) = Env.currentScope state in
               let full_path  = path @ name in
               let includes   = (Env.get state).PassData.args.includes in
               let wave       = readFile loc includes file in
               let ()         = checkNumberOfChannels loc channels wave in
               let result     = getDeclarations attr full_path wave in
               let body       = makeNewBody full_path attr args wave in
               let attr'      = { attr with tags = Tags.removeAttrFunc "wave" attr.tags; ext_fn = None } in
               let size_fun   = makeSizeFunction name attr wave.WavFile.samples in
               reapply state, result @ [size_fun; StmtFun(full_path, args, body, Some ret, attr')]
            | Some (loc, _) ->
               let msg = "This attribute can only be applied to functions returning 'real'" in
               Error.raiseError msg loc
         end
      | _ -> state, [stmt]


   let mapper = Mapper.{ default_mapper with stmt_x }

end

let run =
   Pass1.Simplify.mapper
   |> Mapper.seq Evaluate.mapper
   |> Mapper.seq MakeTables.mapper
   |> Mapper.seq EmbedWavFile.mapper
