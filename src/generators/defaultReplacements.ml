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

(* This file declares all the default type and function replacements performed
   before generating the code. This can be used to define custom functions to
   be used on specific types. For example when replacing the builtin type 'real'
   with a fixed point number.

   The replacements consist of a record of type Replacements.t containing the
   following fields:

      keywords   - Replaces a keyword by another
      types      - Replaces the type name
      cast       - Defines functions to cast among types
      op_to_fun  - Defines operators that should be changed to function calls
      fun_to_fun - Replaces a function call by other
      array_init - Defines the array initialization functions

   Examples of replacements:

   -  keyword: "default", "default_";
      this will change variable or functions called 'default' to 'default_'

   -  types: "real", "float";
      this will change every occurence of type 'real' by a type 'float'

   - casting: ("float", "int32_t"), "float_to_int";
      when converting a 'float' to 'int32_t' the function 'float_to_int' will be used.

   - op_to_fun: ("%", "float"), "fmodf";
      when the operator '%' is used on a type 'float' it will be replaced by a call to 'fmodf'

   - op_to_op: ("<>", "float"), "!=";
      when the operator '<>' is used on a type 'float' it will be replaced by operator '!='

   - fun_to_fun: ("abs", "float"), "fabsf";
      when the function 'abs' is used on type 'float' it will be replaced by a call to 'fabsf'

   - array_init: "float", "float_init_array";
      when an array of type 'float' is initialized the function 'float_init_array' will be used

   Groups of replacements can be extended by using the function Replacements.extendReplacements.
   In this file we extend the default replacements with functions and types for fixed point
   operations. See the definition of FixedPoint.replacements.

*)

(* This module contains the default replacements that turn the 'real' type into 'float' *)
module Default = struct

   let keywords = Replacements.makeKeywords
         [
            "default", "default_";
            "switch",  "switch_";
         ]

   let types = Replacements.makeTypes
         [
            "real", "float";
            "unit", "void";
            "bool", "uint8_t";
            "int",  "int";
         ]

   let cast = Replacements.makeCasts
         [
            ("float", "int"), "float_to_int";
            ("int", "float"), "int_to_float";
         ]

   let op_to_fun = Replacements.makeOperators
         [
            ("%", "float"), "fmodf";
         ]

   let op_to_op = Replacements.makeOperators
         [
            ("<>", "float"),   "!=";
            ("<>", "int"),     "!=";
            ("<>", "uint8_t"), "!=";
         ]

   let fun_to_fun = Replacements.makeFunctions
         [
            ("abs", "float"),   "fabsf";
            ("exp", "float"),   "expf";
            ("floor", "float"), "floorf";
            ("max", "float"),   "fmax";
            ("min", "float"),   "fmin";
            ("sin", "float"),   "sinf";
            ("cos", "float"),   "cosf";
            ("tan", "float"),   "tanf";
            ("tanh", "float"),  "tanhf";
            ("sqrt", "float"),  "sqrtf";
            ("clip", "float"),  "float_clip";
            ("set",  "float"),  "float_set";
            ("set",  "int"),    "int_set";
            ("set",  "uint8_t"), "bool_set";
            ("get",  "float"),  "float_get";
            ("get",  "int"),    "int_get";
            ("get",  "uint8_t"), "bool_get";
            ("not",  "uint8_t"), "bool_not";
         ]

   let array_init = Replacements.makeArrayInitializations
         [
            "float",   "float_init_array";
            "int",     "int_init_array";
            "uint8_t", "bool_init_array";
         ]

   let real_string = Replacements.makeRealToString
         [
            "float", (fun f -> (string_of_float f)^"f")
         ]

   (* This is the default selection of replacements *)
   let replacements =
      Replacements.{
         keywords;
         types;
         cast;
         op_to_fun;
         op_to_op;
         fun_to_fun;
         array_init;
         real_string;
      }

end


module FixedPoint = struct

   let keywords = Replacements.makeKeywords []

   let types = Replacements.makeTypes
         [
            "real", "fix16_t";
         ]

   let cast = Replacements.makeCasts
         [
            ("fix16_t", "int"),     "fix_to_int";
            ("fix16_t", "float"),   "fix_to_float";
            ("int",     "fix16_t"), "int_to_fix";
            ("float",   "fix16_t"), "float_to_fix";
         ]

   let op_to_fun = Replacements.makeOperators
         [
            ("+", "fix16_t"), "fix_add";
            ("-", "fix16_t"), "fix_sub";
            ("*", "fix16_t"), "fix_mul";
            ("/", "fix16_t"), "fix_div";
         ]

   let op_to_op = Replacements.makeOperators
         [
            ("<>", "fix16_t"), "!=";
         ]

   let fun_to_fun = Replacements.makeFunctions
         [
            ("abs", "fix16_t"),   "fix_abs";
            ("exp", "fix16_t"),   "fix_exp";
            ("floor", "fix16_t"), "fix_floor";
            ("max", "fix16_t"),   "fix_max";
            ("min", "fix16_t"),   "fix_min";
            ("sin", "fix16_t"),   "fix_sin";
            ("cos", "fix16_t"),   "fix_cos";
            ("tan", "fix16_t"),   "fix_tan";
            ("tanh", "fix16_t"),  "fix_tanh";
            ("sqrt", "fix16_t"),  "fix_sqrt";
            ("clip", "fix16_t"),  "fix_clip";
            ("set",  "fix16_t"),  "fix_set";
            ("get",  "fix16_t"),  "fix_get";
         ]

   let array_init = Replacements.makeArrayInitializations
         [
            "fix16_t",   "fix_init_array";
         ]


   let toFixed (n:float) : string =
      if n < 0.0 then
         let value = Int32.of_float ((-. n) *. (float_of_int 0x10000)) in
         Printf.sprintf "-0x%lx /* %f */" value n
      else
         let value = Int32.of_float (n *. (float_of_int 0x10000)) in
         Printf.sprintf "0x%lx /* %f */" value n

   let real_string = Replacements.makeRealToString
         [
            "fix16_t", toFixed
         ]

   (* This group of replacements extends/overwrites the Default.replacements *)
   let replacements =
      Replacements.extendReplacements Default.replacements
         Replacements.{
            keywords;
            types;
            cast;
            op_to_fun;
            op_to_op;
            fun_to_fun;
            array_init;
            real_string;
         }

end

module JavaScript = struct

   let keywords = Replacements.makeKeywords
         [
            "default", "default_";
            "switch",  "switch_";
         ]

   let types = Replacements.makeTypes
         [
         ]

   let cast = Replacements.makeCasts
         [

         ]

   let op_to_fun = Replacements.makeOperators
         [
         ]

   let op_to_op = Replacements.makeOperators
         [
            ("<>", "real"),    "!=";
            ("<>", "int"),     "!=";
            ("<>", "bool"),    "!=";
         ]

   let fun_to_fun = Replacements.makeFunctions
         [

         ]

   let array_init = Replacements.makeArrayInitializations
         [
            "int",    "makeArray";
            "real",   "makeArray";
            "bool",   "makeArray";
         ]

   let real_string = Replacements.makeRealToString
         [

         ]

   (* This is the default selection of replacements *)
   let replacements =
      Replacements.{
         keywords;
         types;
         cast;
         op_to_fun;
         op_to_op;
         fun_to_fun;
         array_init;
         real_string;
      }

end

let initialize () =
   Replacements.registerReplacements "float" Default.replacements;
   Replacements.registerReplacements "fixed" FixedPoint.replacements;
   Replacements.registerReplacements "js"    JavaScript.replacements
;;


