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

module Cpp = struct
  let keywords =
    [ "asm"
    ; "auto"
    ; "bool"
    ; "break"
    ; "case"
    ; "catch"
    ; "char"
    ; "class"
    ; "const"
    ; "const_cast"
    ; "continue"
    ; "default"
    ; "delete"
    ; "do"
    ; "double"
    ; "dynamic_cast"
    ; "else"
    ; "enum"
    ; "explicit"
    ; "export"
    ; "extern"
    ; "false"
    ; "float"
    ; "for"
    ; "friend"
    ; "goto"
    ; "if"
    ; "inline"
    ; "int"
    ; "long"
    ; "mutable"
    ; "namespace"
    ; "new"
    ; "operator"
    ; "private"
    ; "protected"
    ; "public"
    ; "register"
    ; "reinterpret_cast"
    ; "return"
    ; "short"
    ; "signed"
    ; "sizeof"
    ; "static"
    ; "static_cast"
    ; "struct"
    ; "switch"
    ; "template"
    ; "this"
    ; "throw"
    ; "true"
    ; "try"
    ; "typedef"
    ; "typeid"
    ; "typename"
    ; "union"
    ; "unsigned"
    ; "using"
    ; "virtual"
    ; "void"
    ; "volatile"
    ; "wchar_t"
    ; "while"
    ]
    |> Util.Maps.Set.of_list


  let fun_to_fun (path : string) (args : type_ list) (ret : type_) =
    let args = List.map (fun (t : type_) -> t.t) args in
    match path, args, ret.t with
    (* builtins *)
    | "samplerate", [], TReal -> Some "float_samplerate"
    | "samplerate", [], TFix16 -> Some "fix_samplerate"
    | "random", [], TReal -> Some "float_random"
    | "clip", [ TReal; _; _ ], TReal -> Some "float_clip"
    | "clip", [ TInt; _; _ ], TInt -> Some "int_clip"
    | "clip", [ TFix16; _; _ ], TFix16 -> Some "fix_clip"
    | "pi", [], TReal -> Some "float_pi"
    | "pi", [], TFix16 -> Some "fix_pi"
    | "eps", [], TReal -> Some "float_eps"
    | "eps", [], TFix16 -> Some "fix_eps"
    | "sin", [ TFix16 ], TFix16 -> Some "fix_sin"
    | "cos", [ TFix16 ], TFix16 -> Some "fix_cos"
    | "tan", [ TFix16 ], TFix16 -> Some "fix_tan"
    | "sinh", [ TFix16 ], TFix16 -> Some "fix_sinh"
    | "cosh", [ TFix16 ], TFix16 -> Some "fix_cosh"
    | "tanh", [ TFix16 ], TFix16 -> Some "fix_tanh"
    | "exp", [ TFix16 ], TFix16 -> Some "fix_exp"
    | "floor", [ TFix16 ], TFix16 -> Some "fix_floor"
    | "abs", [ TFix16 ], TFix16 -> Some "fix_abs"
    | "sqrt", [ TFix16 ], TFix16 -> Some "fix_sqrt"
    | "sin", [ TReal ], TReal -> Some "sinf"
    | "cos", [ TReal ], TReal -> Some "cosf"
    | "tan", [ TReal ], TReal -> Some "tanf"
    | "sinh", [ TReal ], TReal -> Some "sinhf"
    | "cosh", [ TReal ], TReal -> Some "coshf"
    | "tanh", [ TReal ], TReal -> Some "tanhf"
    | "exp", [ TReal ], TReal -> Some "expf"
    | "floor", [ TReal ], TReal -> Some "floorf"
    | "abs", [ TReal ], TReal -> Some "fabsf"
    | "sqrt", [ TReal ], TReal -> Some "sqrtf"
    (* cast *)
    | "int", [ TReal ], _ -> Some "float_to_int"
    | "int", [ TFix16 ], _ -> Some "fix_to_int"
    | "real", [ TInt ], TReal -> Some "int_to_float"
    | "real", [ TBool ], TReal -> Some "bool_to_float"
    | "real", [ TFix16 ], TReal -> Some "fix_to_float"
    | "real", [ TInt ], TFix16 -> Some "int_to_fix"
    | "real", [ TBool ], TFix16 -> Some "bool_to_fix"
    | "real", [ TFix16 ], TFix16 -> Some "fix_to_fix"
    | "fix16", [ TFix16 ], _ -> Some "fix_to_fix"
    | "fix16", [ TReal ], _ -> Some "float_to_fix"
    | "fix16", [ TInt ], _ -> Some "int_to_fix"
    | "fix16", [ TBool ], _ -> Some "bool_to_fix"
    | "string", [ TInt ], _ -> Some "std::to_string"
    | "string", [ TReal ], _ -> Some "std::to_string"
    | "string", [ TFix16 ], _ -> Some "fix_to_string"
    | "string", [ TBool ], _ -> Some "bool_to_string"
    | "bool", [ TInt ], _ -> Some "int_to_bool"
    | "bool", [ TReal ], _ -> Some "real_to_bool"
    | "bool", [ TFix16 ], _ -> Some "fix_to_bool"
    (* get *)
    | "get", [ TArray (_, { t = TReal; _ }); TInt ], TReal -> Some "float_get"
    | "get", [ TArray (_, { t = TFix16; _ }); TInt ], TFix16 -> Some "fix_get"
    | "get", [ TArray (_, { t = TInt; _ }); TInt ], TInt -> Some "int_get"
    (* set *)
    | "set", [ TArray (_, { t = TReal; _ }); TInt; TReal ], TVoid None -> Some "float_set"
    | "set", [ TArray (_, { t = TFix16; _ }); TInt; TFix16 ], TVoid None -> Some "fix_set"
    | "set", [ TArray (_, { t = TInt; _ }); TInt; TInt ], TVoid None -> Some "int_set"
    (* serialization *)
    | "deserialize_float", _, TFix16 -> Some "deserialize_int"
    | "push_float", [ _; _; TFix16 ], _ -> Some "push_int"
    | _ -> None


  let op_to_fun (op : Core.Prog.operator) (e1 : type_) (e2 : type_) (ret : type_) =
    match op, e1.t, e2.t, ret.t with
    | OpMod, TReal, TReal, TReal -> Some "fmodf"
    | OpMul, TFix16, TFix16, TFix16 -> Some "fix_mul"
    | OpDiv, TFix16, TFix16, TFix16 -> Some "fix_div"
    | _ -> None
end

module Lua = struct
  let keywords =
    [ "and"
    ; "break"
    ; "do"
    ; "elseif"
    ; "end"
    ; "for"
    ; "function"
    ; "in"
    ; "local"
    ; "nil"
    ; "or"
    ; "repeat"
    ; "return"
    ; "until"
    ]
    |> Util.Maps.Set.of_list


  let op_to_fun (op : Core.Prog.operator) (e1 : type_) (e2 : type_) (ret : type_) =
    match op, e1.t, e2.t, ret.t with
    | OpDiv, TInt, TInt, TInt -> Some "intDiv"
    | _ -> None


  let fun_to_fun (path : string) (args : type_ list) (ret : type_) =
    let args = List.map (fun (t : type_) -> t.t) args in
    match path, args, ret.t with
    (* builtins *)
    | "float_to_int", [ TReal ], TInt -> Some "math.floor"
    | _ -> None
end

let fun_to_fun (lang : Util.Args.code) (path : string) (args : type_ list) (ret : type_) =
  match lang with
  | CppCode -> Cpp.fun_to_fun path args ret
  | LuaCode -> Lua.fun_to_fun path args ret
  | _ -> None


let op_to_fun (lang : Util.Args.code) (op : Core.Prog.operator) (e1 : type_) (e2 : type_) (ret : type_) =
  match lang with
  | CppCode -> Cpp.op_to_fun op e1 e2 ret
  | LuaCode -> Lua.op_to_fun op e1 e2 ret
  | _ -> None


let keyword (lang : Util.Args.code) id =
  let keywords =
    match lang with
    | CppCode -> Cpp.keywords
    | LuaCode -> Lua.keywords
    | _ -> Util.Maps.Set.empty
  in
  if Util.Maps.Set.mem id keywords then id ^ "_" else id
