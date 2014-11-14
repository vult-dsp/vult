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
open LexerVult
open ParserVult
open Types

(** Stores the options passed to the command line *)
type arguments =
{
  files  : string list;
  dparse : bool;
}

(** Returns a 'arguments' type containing the options passed in the command line *)
let processArguments () =
  let files  = ref [] in
  let dparse = ref false in
  let opts = [
      "-dparse", (Arg.Unit   (fun () -> dparse:=true)), "Dumps the parse tree (default: off)";
      ]
  in
  let _ = Arg.parse opts (fun a -> files:=a::(!files)) "Usage: vultc file.vlt\n" in
  let _ = files := List.rev (!files) in (* Put the files in the correct order  *)
  { files = !files ; dparse = !dparse }

let main () =
  let args = processArguments () in
  let parsed_file = List.map parseFile args.files in
  let _ = if args.dparse then
    List.iter (fun a-> match a with Some(b) -> PrintTypes.stmtListStr b |> print_string | _ -> () ) parsed_file
  in

  ()
;;
main ();;
