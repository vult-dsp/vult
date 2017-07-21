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

open Args
open Tcommon


let in_test_directory _ = failwith ""


let compileFile (file:string) =
   let basename = Filename.chop_extension (Filename.basename file) in
   let cmd = Printf.sprintf "gcc -Werror -I. -I%s -O3 -c %s -o %s.o" (in_test_directory "../runtime") file basename in
   if Sys.command cmd <> 0 then
      failwith ("Failed to compile "^file)

let linkFiles (output:string) (files:string list) =
   let lflags = if os = "Linux" then "-lm" else "" in
   let cmd = Printf.sprintf "gcc -o %s %s %s" output (String.concat " " files) lflags in
   if Sys.command cmd <> 0 then
      failwith ("Failed to link ")

let generateC (filename:string) (output:string) (real_type:string) : unit =
   let args = { default_arguments with files = [File filename]; ccode = true; output = output; real = real_type } in
   let parser_results = Parser.parseFile filename  in
   let gen = Generate.generateCode [parser_results] args in
   writeFiles args gen

let generateJs (filename:string) (output:string) : unit =
   let args = { default_arguments with files = [File filename]; template = "node"; jscode = true; output = output } in
   let parser_results = Parser.parseFile filename  in
   let gen = Generate.generateCode [parser_results] args in
   writeFiles args gen

let generateLua (filename:string) (output:string) : unit =
   let args = { default_arguments with files = [File filename]; luacode = true; output = output } in
   let parser_results = Parser.parseFile filename  in
   let gen = Generate.generateCode [parser_results] args in
   writeFiles args gen

let run real_type _ =
   let vultfile = in_test_directory ("../test/bench/bench.vult") in
   let output   = Filename.chop_extension (Filename.basename vultfile) in
   Sys.chdir tmp_dir;
   generateC vultfile output real_type;
   if (Sys.file_exists (output^".cpp")) then failwith "No code generated";
   compileFile (output^".cpp");
   compileFile (in_test_directory "../runtime/vultin.c");
   compileFile (in_test_directory "../test/bench/main.cpp");
   linkFiles ("bench_"^real_type) ["vultin.o";"bench.o";"main.o"];
   print_endline ("### Real numbers: "^real_type ^ "");
   ignore (Sys.command ("./bench_"^real_type));
   Sys.remove (output^".cpp");
   Sys.remove (output^".h");
   Sys.chdir initial_dir

let runJsLua _ =
   let vultfile = in_test_directory ("../test/bench/bench.vult") in
   let output   = Filename.chop_extension (Filename.basename vultfile) in

   Sys.chdir (in_test_directory "bench");
   generateJs vultfile output;
   generateLua vultfile output;
   ignore (Sys.command "node main.js");
   ignore (Sys.command "luajit main.lua");
   Sys.chdir initial_dir