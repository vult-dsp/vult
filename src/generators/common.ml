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
open Core.Prog

let legend =
  let version = Core.Version.version in
  {%pla|/* This code was generated by the Vult compiler <#version#s> https://github.com/vult-dsp/vult */|}


let setExt ext output =
  match output with
  | None -> "output" ^ ext
  | Some file -> file ^ ext


let splitArray into elems =
  let rec loop current acc count elems =
    match elems with
    | [] -> List.rev (List.rev current :: acc)
    | h :: t when count < into -> loop (h :: current) acc (count + 1) t
    | h :: t -> loop [ h ] (List.rev current :: acc) 0 t
  in
  loop [] [] 0 elems


let cast ~(from : type_) ~(to_ : type_) (value : Pla.t) =
  match from.t, to_.t with
  | TInt, TReal -> {%pla|(float)<#value#>|}
  | TInt, TBool -> {%pla|(bool)<#value#>|}
  | TInt, TFix16 -> {%pla|int_to_fix(<#value#>)|}
  | TReal, TInt -> {%pla|(int)<#value#>|}
  | TReal, TBool -> {%pla|(<#value#> != 0.0f)|}
  | TReal, TFix16 -> {%pla|float_to_fix(<#value#>)|}
  | TBool, TInt -> {%pla|(int)<#value#>|}
  | TBool, TReal -> {%pla|(<#value#> ? 1.0f : 0.0f)|}
  | TBool, TFix16 -> {%pla|(<#value#> ? int_to_fix(1) : int_to_fix(0))|}
  | TFix16, TInt -> {%pla|fix_to_int(<#value#>)|}
  | TFix16, TReal -> {%pla|fix_to_float(<#value#>)|}
  | TFix16, TBool -> {%pla|(<#value#> != 0)|}
  (* no cast *)
  | TReal, TReal -> value
  | TInt, TInt -> value
  | TBool, TBool -> value
  | TFix16, TFix16 -> value
  | _ -> failwith "Unknown cast"


let toFixed ?(comment = true) (n : float) : string =
  let () =
    if n > 32767.0 || n < -32768.0 then (
      let msg = Printf.sprintf "This value '%f' cannot be represented with fixed-point numbers" n in
      Util.Error.raiseErrorMsg msg)
  in
  if n < 0.0 then (
    let value = Int32.of_float (-.n *. float_of_int 0x10000) in
    if comment then Printf.sprintf "-0x%lx /* %f */" value n else Printf.sprintf "-0x%lx" value)
  else (
    let value = Int32.of_float (n *. float_of_int 0x10000) in
    if comment then
      Printf.sprintf "0x%lx /* %f */" value n
    else
      Printf.sprintf "0x%lx" value)


let splitByFile (stmts : top_stmt list) =
  let getFile loc =
    match loc.Util.Loc.source with
    | Util.Loc.Text _ -> failwith ""
    | File file -> file |> Filename.basename |> Filename.chop_extension
  in
  let index = ref 0 in
  List.fold_left
    (fun map (stmt : top_stmt) ->
      Util.Maps.Map.update
        (getFile stmt.loc)
        (fun found_opt ->
          match found_opt with
          | None ->
            incr index;
            Some (index, [ stmt ])
          | Some (index, stmts) -> Some (index, stmt :: stmts))
        map)
    Util.Maps.Map.empty
    stmts
  |> Util.Maps.Map.to_list
  |> List.sort (fun (_, (a, _)) (_, (b, _)) -> compare a b)
  |> List.map (fun (file, (_, stmts)) -> file, List.rev stmts)
