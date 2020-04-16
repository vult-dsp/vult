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

   - casting: ("float", "int"), "float_to_int";
     when converting a 'float' to 'int' the function 'float_to_int' will be used.

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

let toFloat (n : float) : string =
   match classify_float n with
   | FP_normal ->
      if abs_float n < 1e-45 then
         "0.0f"
      else
         Float.to_string n ^ "f"
   | FP_subnormal -> "0.0f"
   | FP_zero -> "0.0f"
   | _ -> Float.to_string n


let toFixed (n : float) : string =
   let () =
      if n > 32767.0 || n < -32768.0 then
         let msg = Printf.sprintf "This value '%f' cannot be represented with fixed-point numbers" n in
         (*Error.raiseError msg (attr.loc)*)
         print_endline msg
   in
   if n < 0.0 then
      let value = Int32.of_float (-.n *. float_of_int 0x10000) in
      Printf.sprintf "-0x%lx /* %f */" value n
   else
      let value = Int32.of_float (n *. float_of_int 0x10000) in
      Printf.sprintf "0x%lx /* %f */" value n


(* This module contains the default replacements that turn the 'real' type into 'float' *)
module Default = struct
   let keywords = Replacements.makeKeywords [ "default", "default_"; "switch", "switch_" ]

   let types =
      Replacements.makeTypes
         [ "real", "float"; "unit", "void"; "bool", "uint8_t"; "int", "int"; "abstract", "void*"; "fix16", "fix16_t" ]


   let cast =
      Replacements.makeCasts
         [ ("float", "int"), "float_to_int"
         ; ("int", "float"), "int_to_float"
         ; ("fix16_t", "int"), "fix_to_int"
         ; ("fix16_t", "float"), "fix_to_float"
         ; ("int", "fix16_t"), "int_to_fix"
         ; ("float", "fix16_t"), "float_to_fix"
         ]


   let op_to_fun =
      Replacements.makeOperators [ ("%", "float"), "fmodf"; ("*", "fix16_t"), "fix_mul"; ("/", "fix16_t"), "fix_div" ]


   let op_to_op =
      Replacements.makeOperators
         [ ("<>", "fix16_t"), "!="; ("<>", "float"), "!="; ("<>", "int"), "!="; ("<>", "uint8_t"), "!=" ]


   let fun_to_fun =
      Replacements.makeFunctions
         [ ("mac", "float"), "float_mac"
         ; ("msu", "float"), "float_msu"
         ; ("abs", "float"), "fabsf"
         ; ("exp", "float"), "expf"
         ; ("log10", "float"), "log10f"
         ; ("floor", "float"), "floorf"
         ; ("max", "float"), "fmax"
         ; ("min", "float"), "fmin"
         ; ("sin", "float"), "sinf"
         ; ("cos", "float"), "cosf"
         ; ("tan", "float"), "tanf"
         ; ("tanh", "float"), "tanhf"
         ; ("cosh", "float"), "coshf"
         ; ("sinh", "float"), "sinhf"
         ; ("sqrt", "float"), "sqrtf"
         ; ("pow", "float"), "powf"
         ; ("clip", "float"), "float_clip"
         ; ("clip", "int"), "int_clip"
         ; ("set", "float"), "float_set"
         ; ("set", "int"), "int_set"
         ; ("set", "uint8_t"), "bool_set"
         ; ("get", "float"), "float_get"
         ; ("get", "int"), "int_get"
         ; ("get", "uint8_t"), "bool_get"
         ; ("not", "uint8_t"), "bool_not"
         ; ("eps", "float"), "float_eps"
         ; ("pi", "float"), "float_pi"
         ; ("random", "float"), "float_random"
         ; ("samplerate", "float"), "float_samplerate"
         ; ("wrap_array", "float"), "float_wrap_array"
         ; ("log", "float"), "float_print"
         ; ("log", "int"), "int_print"
         ; ("log", "uint8_t"), "bool_print"
         ; ("log", "string"), "string_print"
         ; ("mac", "fix16_t"), "fix_mac"
         ; ("msu", "fix16_t"), "fix_msu"
         ; ("abs", "fix16_t"), "fix_abs"
         ; ("exp", "fix16_t"), "fix_exp"
         ; ("floor", "fix16_t"), "fix_floor"
         ; ("max", "fix16_t"), "fix_max"
         ; ("min", "fix16_t"), "fix_min"
         ; ("sin", "fix16_t"), "fix_sin"
         ; ("cos", "fix16_t"), "fix_cos"
         ; ("tan", "fix16_t"), "fix_tan"
         ; ("tanh", "fix16_t"), "fix_tanh"
         ; ("sqrt", "fix16_t"), "fix_sqrt"
         ; ("clip", "fix16_t"), "fix_clip"
         ; ("set", "fix16_t"), "fix_set"
         ; ("get", "fix16_t"), "fix_get"
         ; ("eps", "fix16_t"), "fix_eps"
         ; ("pi", "fix16_t"), "fix_pi"
         ; ("random", "fix16_t"), "fix_random"
         ; ("samplerate", "fix16_t"), "fix_samplerate"
         ; ("wrap_array", "fix16_t"), "fix_wrap_array"
         ; ("log", "fix16_t"), "fix_print"
         ]


   let array_init =
      Replacements.makeArrayInitializations
         [ "float", "float_init_array"
         ; "int", "int_init_array"
         ; "uint8_t", "bool_init_array"
         ; "fix16_t", "fix_init_array"
         ]


   let array_copy =
      Replacements.makeArrayCopy
         [ "float", "float_copy_array"
         ; "int", "int_copy_array"
         ; "uint8_t", "bool_copy_array"
         ; "fix16_t", "fix_copy_array"
         ]


   let real_string = Replacements.makeRealToString [ "float", toFloat; "fix16_t", toFixed ]

   (* This is the default selection of replacements *)
   let replacements =
      Replacements.{ keywords; types; cast; op_to_fun; op_to_op; fun_to_fun; array_init; real_string; array_copy }
end

module Java = struct
   let keywords = Replacements.makeKeywords [ "default", "default_"; "switch", "switch_" ]

   let types = Replacements.makeTypes [ "real", "float"; "unit", "void"; "bool", "boolean"; "int", "int" ]

   let cast =
      Replacements.makeCasts
         [ ("float", "int"), "float_to_int"
         ; ("int", "float"), "int_to_float"
         ; ("fix16", "float"), "fix16_to_float"
         ; ("fix16", "int"), "fix16_to_int"
         ; ("float", "fix16"), "float_to_fix16"
         ; ("int", "fix16"), "int_to_fix16"
         ]


   let op_to_fun = Replacements.makeOperators []

   let op_to_op =
      Replacements.makeOperators
         [ ("<>", "float"), "!="; ("<>", "int"), "!="; ("<>", "boolean"), "!="; ("<>", "fix16"), "!=" ]


   let fun_to_fun =
      Replacements.makeFunctions
         [ ("abs", "double"), "Math.abs"
         ; ("abs", "float"), "Math.abs"
         ; (*
              ("max", "double"),   "Math.max";
              ("min", "double"),   "Math.min";
              ("sin", "double"),   "Math.sin";
              ("cos", "double"),   "Math.cos";
              ("tan", "double"),   "Math.tan";
              ("tanh", "double"),  "Math.tanh";
              ("sinh", "double"),  "Math.sinh";
              ("cosh", "double"),  "Math.cosh";
              ("exp", "double"),   "Math.exp";
              ("sqrt", "double"),  "Math.sqrt";
              ("pow", "double"),   "Math.pow";
              ("floor", "double"), "Math.floor";
           *)
           ("log", "float"), "float_print"
         ; ("log", "int"), "int_print"
         ; ("log", "uint8_t"), "bool_print"
         ; ("log", "string"), "string_print"
         ; ("samplerate", "float"), "External.samplerate"
         ]


   let array_init =
      Replacements.makeArrayInitializations
         [ "float", "float_init_array"; "int", "int_init_array"; "uint8_t", "bool_init_array" ]


   let array_copy =
      Replacements.makeArrayCopy [ "float", "float_copy_array"; "int", "int_copy_array"; "uint8_t", "bool_copy_array" ]


   let real_string = Replacements.makeRealToString [ "float", toFloat ]

   (* This is the default selection of replacements *)
   let replacements =
      Replacements.{ keywords; types; cast; op_to_fun; op_to_op; fun_to_fun; array_init; real_string; array_copy }
end

module FixedPoint = struct
   let keywords = Replacements.makeKeywords []

   let types = Replacements.makeTypes [ "real", "fix16_t" ]

   let cast =
      Replacements.makeCasts
         [ ("fix16_t", "int"), "fix_to_int"
         ; ("fix16_t", "float"), "fix_to_float"
         ; ("int", "fix16_t"), "int_to_fix"
         ; ("float", "fix16_t"), "float_to_fix"
         ]


   let op_to_fun = Replacements.makeOperators [ ("*", "fix16_t"), "fix_mul"; ("/", "fix16_t"), "fix_div" ]

   let op_to_op = Replacements.makeOperators [ ("<>", "fix16_t"), "!=" ]

   let fun_to_fun =
      Replacements.makeFunctions
         [ ("abs", "fix16_t"), "fix_abs"
         ; ("exp", "fix16_t"), "fix_exp"
         ; ("floor", "fix16_t"), "fix_floor"
         ; ("max", "fix16_t"), "fix_max"
         ; ("min", "fix16_t"), "fix_min"
         ; ("sin", "fix16_t"), "fix_sin"
         ; ("cos", "fix16_t"), "fix_cos"
         ; ("tan", "fix16_t"), "fix_tan"
         ; ("tanh", "fix16_t"), "fix_tanh"
         ; ("sqrt", "fix16_t"), "fix_sqrt"
         ; ("clip", "fix16_t"), "fix_clip"
         ; ("set", "fix16_t"), "fix_set"
         ; ("get", "fix16_t"), "fix_get"
         ; ("eps", "fix16_t"), "fix_eps"
         ; ("pi", "fix16_t"), "fix_pi"
         ; ("random", "fix16_t"), "fix_random"
         ; ("samplerate", "fix16_t"), "fix_samplerate"
         ; ("wrap_array", "fix16_t"), "fix_wrap_array"
         ; ("log", "fix16_t"), "fix_print"
         ]


   let array_init = Replacements.makeArrayInitializations [ "fix16_t", "fix_init_array" ]

   let array_copy = Replacements.makeArrayCopy [ "fix16_t", "fix_copy_array" ]

   let real_string = Replacements.makeRealToString [ "fix16_t", toFixed ]

   (* This group of replacements extends/overwrites the Default.replacements *)
   let replacements =
      Replacements.extendReplacements
         Default.replacements
         Replacements.{ keywords; types; cast; op_to_fun; op_to_op; fun_to_fun; array_init; real_string; array_copy }
end

module JavaScript = struct
   let keywords =
      Replacements.makeKeywords
         [ "default", "default_"
         ; "switch", "switch_"
         ; "break", "break_"
         ; "case", "case_"
         ; "catch", "catch_"
         ; "class", "class_"
         ; "const", "const_"
         ; "continue", "continue_"
         ; "debugger", "debugger_"
         ; "default", "default_"
         ; "delete", "delete_"
         ; "do", "do_"
         ; "else", "else_"
         ; "export", "export_"
         ; "extends", "extends_"
         ; "finally", "finally_"
         ; "for", "for_"
         ; "function", "function_"
         ; "if", "if_"
         ; "import", "import_"
         ; "in", "in_"
         ; "instanceof", "instanceof_"
         ; "new", "new_"
         ; "return", "return_"
         ; "super", "super_"
         ; "switch", "switch_"
         ; "this", "this_"
         ; "throw", "throw_"
         ; "try", "try_"
         ; "typeof", "typeof_"
         ; "var", "var_"
         ; "void", "void_"
         ; "while", "while_"
         ; "with", "with_"
         ; "yield", "yield_"
         ; "enum", "enum_"
         ; "implements", "implements_"
         ; "interface", "interface_"
         ; "let", "let_"
         ; "package", "package_"
         ; "private", "private_"
         ; "protected", "protected_"
         ; "public", "public_"
         ; "static", "static_"
         ; "await", "await_"
         ; "abstract", "abstract_"
         ; "boolean", "boolean_"
         ; "byte", "byte_"
         ; "char", "char_"
         ; "double", "double_"
         ; "final", "final_"
         ; "float", "float_"
         ; "goto", "goto_"
         ; "int", "int_"
         ; "long", "long_"
         ; "native", "native_"
         ; "short", "short_"
         ; "synchronized", "synchronized_"
         ; "throws", "throws_"
         ; "transient", "transient_"
         ; "volatile", "volatile_"
         ; "undefined", "undefined_"
         ; "null", "null_"
         ; "NaN", "NaN_"
         ; "true", "true_"
         ; "false", "false_"
         ]


   let types = Replacements.makeTypes []

   let cast = Replacements.makeCasts []

   let op_to_fun = Replacements.makeOperators []

   let op_to_op = Replacements.makeOperators [ ("<>", "real"), "!="; ("<>", "int"), "!="; ("<>", "bool"), "!=" ]

   let fun_to_fun = Replacements.makeFunctions []

   let array_init =
      Replacements.makeArrayInitializations [ "int", "makeArray"; "real", "makeArray"; "bool", "makeArray" ]


   let array_copy = Replacements.makeArrayCopy []

   let real_string = Replacements.makeRealToString []

   (* This is the default selection of replacements *)
   let replacements =
      Replacements.{ keywords; types; cast; op_to_fun; op_to_op; fun_to_fun; array_init; real_string; array_copy }
end

module Lua = struct
   let keywords =
      Replacements.makeKeywords
         [ "and", "and_"
         ; "break", "break_"
         ; "do", "do_"
         ; "elseif", "elseif_"
         ; "end", "end_"
         ; "for", "for_"
         ; "function", "function_"
         ; "in", "in_"
         ; "local", "local_"
         ; "nil", "nil_"
         ; "or", "or_"
         ; "repeat", "repeat_"
         ; "return", "return_"
         ; "until", "until_"
         ]


   let types = Replacements.makeTypes []

   let cast = Replacements.makeCasts []

   let op_to_fun =
      Replacements.makeOperators
         [ ("^", "int"), "bit.bxor"
         ; ("&", "int"), "bit.band"
         ; ("|", "int"), "bit.bor"
         ; ("<<", "int"), "bit.lshift"
         ; (">>", "int"), "bit.rshift"
         ]


   let op_to_op =
      Replacements.makeOperators
         [ ("<>", "real"), "~="; ("<>", "int"), "~="; ("<>", "bool"), "~="; ("&&", "bool"), "and"; ("||", "bool"), "or" ]


   let fun_to_fun = Replacements.makeFunctions []

   let array_init =
      Replacements.makeArrayInitializations [ "int", "makeArray"; "real", "makeArray"; "bool", "makeArray" ]


   let array_copy = Replacements.makeArrayCopy []

   let real_string = Replacements.makeRealToString []

   (* This is the default selection of replacements *)
   let replacements =
      Replacements.{ keywords; types; cast; op_to_fun; op_to_op; fun_to_fun; array_init; real_string; array_copy }
end

let initialize () =
   Replacements.registerReplacements "default" Default.replacements ;
   Replacements.registerReplacements "float" Default.replacements ;
   Replacements.registerReplacements "fixed" FixedPoint.replacements ;
   Replacements.registerReplacements "js" JavaScript.replacements ;
   Replacements.registerReplacements "lua" Lua.replacements ;
   Replacements.registerReplacements "java" Java.replacements
