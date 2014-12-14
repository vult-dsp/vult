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
open OUnit2

open ParserVult
open Types

let test_string reference current = assert_equal ~printer:(fun a->a) reference current ;;

let run_program s =
   let result = Driver.parseStringRun s in
   match result.iresult with
   | `Ok(s) -> s
   | `Error(errors) ->
      let error_strings = Errors.reportErrors result.iresult result.lines in
      List.hd error_strings

(** Expression parsing tests *)
let parseExpTest1 context =
   test_string      "a"
      (parseDumpExp "a")
;;
let parseExpTest2 context =
   test_string      "a:b"
      (parseDumpExp "a:b")
;;
let parseExpTest3 context =
   test_string      "a(1,2)"
      (parseDumpExp "a(1,2)")
;;
let parseExpTest4 context =
   test_string      "a:b()"
      (parseDumpExp "a:b()")
;;
let parseExpTest5 context =
   test_string      "a:b(1,2)"
      (parseDumpExp "a:b(1,2)")
;;
let parseExpTest6 context =
   test_string      "(((-a(1,2))+(b*c))+(d/2))"
      (parseDumpExp "-a(1,2)+b*c+d/2")
;;
let parseExpTest7 context =
   test_string      "((((a+b)>0)&&(a/b))||(a==b))"
      (parseDumpExp "a+b>0&&a/b||a==b")
;;
let parseExpTest8 context =
   test_string      "()"
      (parseDumpExp "()")
;;
let parseExpTest9 context =
   test_string      "(a)"
      (parseDumpExp "(a)")
;;
let parseExpTest10 context =
   test_string      "a,b"
      (parseDumpExp "a,b")
;;

(** Statement parsing tests *)
let parseStmtTest1 context =
   test_string           "val a;"
      (parseDumpStmtList "val a;")
;;
let parseStmtTest2 context =
   test_string           "mem a:num,b,c;"
      (parseDumpStmtList "mem a:num,b,c;")
;;
let parseStmtTest3 context =
   test_string           "val a:num=0;"
      (parseDumpStmtList "val a:num=0;")
;;
let parseStmtTest4 context =
   test_string           "mem a=0,b:num=0;"
      (parseDumpStmtList "mem a=0,b:num=0;")
;;
let parseStmtTest5 context =
   test_string           "return (-a);"
      (parseDumpStmtList "return -a;")
;;
let parseStmtTest6 context =
   test_string           "return a,0;"
      (parseDumpStmtList "return a,0;")
;;
let parseStmtTest7 context =
   test_string           "{\n   val a=0;\n   return a;\n}"
      (parseDumpStmtList "{ val a =0; return a; }")
;;
let parseStmtTest8 context =
   test_string           "return a;"
      (parseDumpStmtList "{ return a; }")
;;
let parseStmtTest9 context =
   test_string           "if(a) {\n   val a=0;\n   return a;\n}"
      (parseDumpStmtList "if(a){ val a=0;return a;}")
;;
let parseStmtTest10 context =
   test_string           "if(a) return 0;"
      (parseDumpStmtList "if(a) return 0;")
;;
let parseStmtTest11 context =
   test_string           "if(a) return false;\nelse return true;"
      (parseDumpStmtList "if(a) return false; else return true;")
;;
let parseStmtTest12 context =
   test_string           "fun add(a,b) return (a+b);"
      (parseDumpStmtList "fun add(a,b) return a+b;")
;;
let parseStmtTest13 context =
   test_string           "fun add:int(a:int,b:int) return (a+b);"
      (parseDumpStmtList "fun add:int(a:int,b:int) return a+b;")
;;
let parseStmtTest14 context =
   test_string           "mem a(0),b(0)=a;"
      (parseDumpStmtList "mem a(0),b(0)=a;")
;;

(* Name the test cases and group them together *)
let parser_test =
   "parser">:::
   [
      "parseExpTest1" >:: parseExpTest1 ;
      "parseExpTest2" >:: parseExpTest2 ;
      "parseExpTest3" >:: parseExpTest3 ;
      "parseExpTest4" >:: parseExpTest4 ;
      "parseExpTest5" >:: parseExpTest5 ;
      "parseExpTest6" >:: parseExpTest6 ;
      "parseExpTest7" >:: parseExpTest7 ;
      "parseExpTest8" >:: parseExpTest8 ;
      "parseExpTest9" >:: parseExpTest9 ;
      "parseExpTest10" >:: parseExpTest10 ;

      "parseStmtTest1" >:: parseStmtTest1 ;
      "parseStmtTest2" >:: parseStmtTest2 ;
      "parseStmtTest3" >:: parseStmtTest3 ;
      "parseStmtTest4" >:: parseStmtTest4 ;
      "parseStmtTest5" >:: parseStmtTest5 ;
      "parseStmtTest6" >:: parseStmtTest6 ;
      "parseStmtTest7" >:: parseStmtTest7 ;
      "parseStmtTest8" >:: parseStmtTest8 ;
      "parseStmtTest9" >:: parseStmtTest9 ;
      "parseStmtTest10" >:: parseStmtTest10 ;
      "parseStmtTest11" >:: parseStmtTest11 ;
      "parseStmtTest12" >:: parseStmtTest12 ;
      "parseStmtTest13" >:: parseStmtTest13 ;
      "parseStmtTest14" >:: parseStmtTest14 ;
   ]
;;

let runFactTest context =
   test_string "120."
   (run_program "fun fact(n){ return if n==0 then 1 else n*fact(n-1); } return fact(5);")
;;

let runSwapTest context =
   test_string "(2.,1.)"
   (run_program "val x=1; val y=2; x,y=y,x; return x,y;")
;;

let runMemTest context =
   test_string "(1.,2.,1.,2.)"
   (run_program "fun inc() { mem x; x = x+1; return x; }
                 fun reset() { mem x; x = 0; }
                 val a = i:inc();
                 val b = i:inc();
                 i:reset();
                 val c = i:inc();
                 val d = i:inc();
                 return a,b,c,d;")

(* Name the test cases and group them together *)
let interpreter_test =
   "interpreter">:::
   [
      "runFactTest" >:: runFactTest;
      "runSwapTest" >:: runSwapTest;
      "runMemTest"  >:: runMemTest;
   ]
;;

let suite =
   "suite">:::
   [
      parser_test;
      interpreter_test;
   ]


let () =
   let _ = run_test_tt_main suite in
   ()
;;