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

type type_descr =
   | CTSimple of string
   | CTArray  of type_descr * int
   [@@deriving show,eq,ord]

type arg_type =
   | Ref of type_descr
   | Var of type_descr
   [@@deriving show,eq,ord]

type cexp =
   | CEInt    of int
   | CEFloat  of float
   | CEBool   of bool
   | CEString of string
   | CEArray  of cexp list
   | CECall   of string * cexp list
   | CEUnOp   of string * cexp
   | CEOp     of string * cexp list
   | CEVar    of string
   | CEIf     of cexp * cexp * cexp
   | CETuple  of (string * cexp) list
   | CENewObj
   [@@deriving show,eq,ord]

type clhsexp =
   | CLWild
   | CLId    of type_descr * string
   | CLTuple of clhsexp list
   [@@deriving show,eq,ord]

type cstmt =
   | CSVarDecl  of clhsexp * cexp option
   | CSBind     of clhsexp * cexp
   | CSFunction of type_descr * string * (arg_type * string) list * cstmt
   | CSReturn   of cexp
   | CSWhile    of cexp * cstmt
   | CSBlock    of cstmt list
   | CSIf       of cexp * cstmt * cstmt option
   | CSType     of string * (type_descr * string) list
   | CSAlias    of string * type_descr
   | CSExtFunc  of type_descr * string * (arg_type * string) list
   | CSEmpty
   [@@deriving show,eq,ord]
