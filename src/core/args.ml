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

type input =
   | File of string
   | Code of string * string
[@@deriving show, eq,ord]

type output =
   | Version of string
   | Message of string
   | Dependencies of string list
   | ParsedCode of string
   | GeneratedCode of (Pla.t * FileKind.t) list
   | Interpret of string
   | CheckOk
   | Errors of Error.t list

type code =
   | NoCode
   | CCode
   | JSCode
   | LuaCode
   | LLVMCode
   | JavaCode
[@@deriving show,eq,ord]

(** Stores the options passed to the command line *)
type args =
   {
      mutable files    : input list;
      mutable dparse   : bool;
      mutable eval     : bool;
      mutable check    : bool;
      mutable code     : code;
      mutable output   : string;
      mutable real     : string;
      mutable template : string;
      mutable show_version : bool;
      mutable includes : string list;
      mutable deps    : bool;
      mutable fs      : float option;
   }
[@@deriving show,eq,ord]


let default_arguments : args =
   {
      files  = [];
      dparse = false;
      code   = NoCode;
      eval   = false;
      check  = false;
      output  = "";
      real    = "float";
      template = "default";
      show_version = false;
      includes = [];
      deps = false;
      fs = None;
   }