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

(** Expression parsing tests *)
let parseExpTest1 test_ctxt = test_string "a"
                              (parseDumpExp "a")
;;
let parseExpTest2 test_ctxt = test_string "a:b"
                              (parseDumpExp "a:b")
;;
let parseExpTest3 test_ctxt = test_string "a(1,2)"
                              (parseDumpExp "a(1,2)")
;;
let parseExpTest4 test_ctxt = test_string "a:b()"
                              (parseDumpExp "a:b()")
;;
let parseExpTest5 test_ctxt = test_string "a:b(1,2)"
                              (parseDumpExp "a:b(1,2)")
;;
let parseExpTest6 test_ctxt = test_string "(((-a(1,2))+(b*c))+(d/2))"
                              (parseDumpExp "-a(1,2)+b*c+d/2")
;;
let parseExpTest7 test_ctxt = test_string "((((a+b)>0)&&(a/b))||(a==b))"
                              (parseDumpExp "a+b>0&&a/b||a==b")
;;
let parseExpTest8 test_ctxt = test_string "()"
                              (parseDumpExp "()")
;;
let parseExpTest9 test_ctxt = test_string "a"
                              (parseDumpExp "(a)")
;;
let parseExpTest10 test_ctxt = test_string "(a,b)"
                              (parseDumpExp "(a,b)")
;;

(** Statement parsing tests *)
let parseStmtTest1 test_ctxt = test_string "val a;"
                              (parseDumpStmtList "val a;")
;;
let parseStmtTest2 test_ctxt = test_string "mem a:num,b,c;"
                              (parseDumpStmtList "mem a:num,b,c;")
;;
let parseStmtTest3 test_ctxt = test_string "val a:num=0;"
                              (parseDumpStmtList "val a:num=0;")
;;
let parseStmtTest4 test_ctxt = test_string "mem a=0,b:num=0;"
                              (parseDumpStmtList "mem a=0,b:num=0;")
;;
let parseStmtTest5 test_ctxt = test_string "return (-a);"
                              (parseDumpStmtList "return -a;")
;;
let parseStmtTest6 test_ctxt = test_string "return (a,0);"
                              (parseDumpStmtList "return (a,0);")
;;
let parseStmtTest7 test_ctxt = test_string "{\n   val a=0;\n   return a;\n}"
                              (parseDumpStmtList "{ val a =0; return a; }")
;;
let parseStmtTest8 test_ctxt = test_string "return a;"
                              (parseDumpStmtList "{ return a; }")
;;
let parseStmtTest9 test_ctxt = test_string "if(a) {\n   val a=0;\n   return a;\n}"
                              (parseDumpStmtList "if(a){ val a=0;return a;}")
;;
let parseStmtTest10 test_ctxt = test_string "if(a) return 0;"
                              (parseDumpStmtList "if(a) return 0;")
;;
let parseStmtTest11 test_ctxt = test_string "if(a) return false;\nelse return true;"
                              (parseDumpStmtList "if(a) return false; else return true;")
;;
let parseStmtTest12 test_ctxt = test_string "fun add(a,b) return (a+b);"
                              (parseDumpStmtList "fun add(a,b) return a+b;")
;;
let parseStmtTest13 test_ctxt = test_string "fun add:int(a:int,b:int) return (a+b);"
                              (parseDumpStmtList "fun add:int(a:int,b:int) return a+b;")
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
  ]
;;

let () =
  run_test_tt_main parser_test
;;