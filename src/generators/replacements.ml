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

let getReturnType (t : type_) = match t.t with TVoid None -> t | TVoid (Some [t]) -> t | _ -> t

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
    ; "while" ]
    |> Util.Maps.Set.of_list

  let fun_to_fun (path : string) (args : type_ list) (ret : type_) =
    let args = CCList.map (fun (t : type_) -> t.t) args in
    match (path, args, (getReturnType ret).t) with
    (* builtins *)
    | "samplerate", [], TReal ->
        Some "float_samplerate"
    | "samplerate", [], TFix16 ->
        Some "fix_samplerate"
    | "random", [], TFix16 ->
        Some "fix_random"
    | "random", [], TReal ->
        Some "float_random"
    | "irandom", [], _ ->
        Some "int_random"
    | "clip", [TReal; _; _], TReal ->
        Some "float_clip"
    | "clip", [TInt; _; _], TInt ->
        Some "int_clip"
    | "clip", [TFix16; _; _], TFix16 ->
        Some "fix_clip"
    | "pi", [], TReal ->
        Some "float_pi"
    | "pi", [], TFix16 ->
        Some "fix_pi"
    | "eps", [], TReal ->
        Some "float_eps"
    | "eps", [], TFix16 ->
        Some "fix_eps"
    | "sin", [TFix16], TFix16 ->
        Some "fix_sin"
    | "cos", [TFix16], TFix16 ->
        Some "fix_cos"
    | "tan", [TFix16], TFix16 ->
        Some "fix_tan"
    | "sinh", [TFix16], TFix16 ->
        Some "fix_sinh"
    | "cosh", [TFix16], TFix16 ->
        Some "fix_cosh"
    | "tanh", [TFix16], TFix16 ->
        Some "fix_tanh"
    | "exp", [TFix16], TFix16 ->
        Some "fix_exp"
    | "floor", [TFix16], TFix16 ->
        Some "fix_floor"
    | "abs", [TFix16], TFix16 ->
        Some "fix_abs"
    | "sqrt", [TFix16], TFix16 ->
        Some "fix_sqrt"
    | "sin", [TReal], TReal ->
        Some "sinf"
    | "cos", [TReal], TReal ->
        Some "cosf"
    | "tan", [TReal], TReal ->
        Some "tanf"
    | "sinh", [TReal], TReal ->
        Some "sinhf"
    | "cosh", [TReal], TReal ->
        Some "coshf"
    | "tanh", [TReal], TReal ->
        Some "tanhf"
    | "exp", [TReal], TReal ->
        Some "expf"
    | "floor", [TReal], TReal ->
        Some "floorf"
    | "abs", [TReal], TReal ->
        Some "fabsf"
    | "sqrt", [TReal], TReal ->
        Some "sqrtf"
    | "log", [TReal], TReal ->
        Some "logf"
    | "log10", [TReal], TReal ->
        Some "log10f"
    | "pow", [TReal; TReal], TReal ->
        Some "powf"
    (* cast *)
    | "int", [TReal], _ ->
        Some "float_to_int"
    | "int", [TFix16], _ ->
        Some "fix_to_int"
    | "int", [TInt16], _ ->
        Some "int16_to_int"
    | "int16", [TInt], _ ->
        Some "int_to_int16"
    | "int16", [TReal], _ ->
        Some "float_to_int16"
    | "int16", [TBool], _ ->
        Some "bool_to_int16"
    | "int16", [TFix16], _ ->
        Some "fix_to_int16"
    | "int16", [TInt16], _ ->
        Some "int16_to_int16"
    | "real", [TInt], TReal ->
        Some "int_to_float"
    | "real", [TInt16], TReal ->
        Some "int16_to_float"
    | "real", [TBool], TReal ->
        Some "bool_to_float"
    | "real", [TFix16], TReal ->
        Some "fix_to_float"
    | "real", [TInt], TFix16 ->
        Some "int_to_fix"
    | "real", [TInt16], TFix16 ->
        Some "int16_to_fix"
    | "real", [TBool], TFix16 ->
        Some "bool_to_fix"
    | "real", [TFix16], TFix16 ->
        Some "fix_to_fix"
    | "fix16", [TFix16], _ ->
        Some "fix_to_fix"
    | "fix16", [TReal], _ ->
        Some "float_to_fix"
    | "fix16", [TInt], _ ->
        Some "int_to_fix"
    | "fix16", [TInt16], _ ->
        Some "int16_to_fix"
    | "fix16", [TBool], _ ->
        Some "bool_to_fix"
    | "string", [TInt], _ ->
        Some "std::to_string"
    | "string", [TInt16], _ ->
        Some "int16_to_string"
    | "string", [TReal], _ ->
        Some "std::to_string"
    | "string", [TFix16], _ ->
        Some "fix_to_string"
    | "string", [TBool], _ ->
        Some "bool_to_string"
    | "bool", [TInt], _ ->
        Some "int_to_bool"
    | "bool", [TInt16], _ ->
        Some "int16_to_bool"
    | "bool", [TReal], _ ->
        Some "float_to_bool"
    | "bool", [TFix16], _ ->
        Some "fix_to_bool"
    (* serialization *)
    | "deserialize_float", _, TFix16 ->
        Some "deserialize_int"
    | "push_float", [_; _; TFix16], _ ->
        Some "push_int"
    | _ ->
        None

  let op_to_fun (op : Core.Prog.operator) (e1 : type_) (e2 : type_) (ret : type_) =
    match (op, e1.t, e2.t, ret.t) with
    | OpMod, TReal, TReal, TReal ->
        Some "fmodf"
    | OpMul, TFix16, TFix16, TFix16 ->
        Some "fix_mul"
    | OpDiv, TFix16, TFix16, TFix16 ->
        Some "fix_div"
    | _ ->
        None
end

module Java = struct
  let keywords =
    [ "abstract"
    ; "assert"
    ; "boolean"
    ; "byte"
    ; "case"
    ; "catch"
    ; "char"
    ; "class"
    ; "const"
    ; "continue"
    ; "default"
    ; "do"
    ; "double"
    ; "else"
    ; "enum"
    ; "extends"
    ; "final"
    ; "finally"
    ; "float"
    ; "for"
    ; "goto"
    ; "if"
    ; "implements"
    ; "import"
    ; "instanceof"
    ; "interface"
    ; "long"
    ; "native"
    ; "new"
    ; "package"
    ; "private"
    ; "protected"
    ; "public"
    ; "return"
    ; "short"
    ; "static"
    ; "strictfp"
    ; "super"
    ; "switch"
    ; "synchronized"
    ; "this"
    ; "throw"
    ; "throws"
    ; "transient"
    ; "try"
    ; "void"
    ; "volatile"
    ; "while" ]
    |> Util.Maps.Set.of_list

  let fun_to_fun (path : string) (args : type_ list) (ret : type_) =
    let args = CCList.map (fun (t : type_) -> t.t) args in
    match (path, args, (getReturnType ret).t) with
    | "samplerate", [], TReal ->
        Some "External.samplerate"
    | "fix16", [TFix16], _ ->
        Some "fix_to_fix"
    | "fix16", [TReal], _ ->
        Some "float_to_fix"
    | "fix16", [TInt], _ ->
        Some "int_to_fix"
    | "fix16", [TInt16], _ ->
        Some "int16_to_fix"
    | "fix16", [TBool], _ ->
        Some "bool_to_fix"
    | "real", [TBool], TReal ->
        Some "bool_to_float"
    | _ ->
        None

  let op_to_fun (_op : Core.Prog.operator) (_e1 : type_) (_e2 : type_) (_ret : type_) = None
end

module Lua = struct
  let keywords =
    ["and"; "break"; "do"; "elseif"; "end"; "for"; "function"; "in"; "local"; "nil"; "or"; "repeat"; "return"; "until"]
    |> Util.Maps.Set.of_list

  let op_to_fun (op : Core.Prog.operator) (e1 : type_) (e2 : type_) (ret : type_) =
    match (op, e1.t, e2.t, ret.t) with OpDiv, TInt, TInt, TInt -> Some "intDiv" | _ -> None

  let fun_to_fun (path : string) (args : type_ list) (ret : type_) =
    let args = CCList.map (fun (t : type_) -> t.t) args in
    match (path, args, (getReturnType ret).t) with
    (* builtins *)
    | "float_to_int", [TReal], TInt ->
        Some "math.floor"
    (* cast - map string directly to tostring to avoid conflict with Lua string library *)
    | "string", [TInt], TString ->
        Some "tostring"
    | "string", [TInt16], TString ->
        Some "tostring"
    | "string", [TReal], TString ->
        Some "tostring"
    | "string", [TFix16], TString ->
        Some "tostring"
    | "string", [TBool], TString ->
        Some "tostring"
    | _ ->
        None
end

module Js = struct
  let keywords =
    [ "abstract"
    ; "arguments"
    ; "await"
    ; "boolean"
    ; "break"
    ; "byte"
    ; "case"
    ; "catch"
    ; "char"
    ; "class"
    ; "const"
    ; "continue"
    ; "debugger"
    ; "default"
    ; "delete"
    ; "do"
    ; "double"
    ; "else"
    ; "enum"
    ; "eval"
    ; "export"
    ; "extends"
    ; "false"
    ; "final"
    ; "finally"
    ; "float"
    ; "for"
    ; "function"
    ; "goto"
    ; "if"
    ; "implements"
    ; "import"
    ; "in"
    ; "instanceof"
    ; "int"
    ; "interface"
    ; "let"
    ; "long"
    ; "native"
    ; "new"
    ; "null"
    ; "package"
    ; "private"
    ; "protected"
    ; "public"
    ; "return"
    ; "short"
    ; "static"
    ; "super"
    ; "switch"
    ; "synchronized"
    ; "this"
    ; "throw"
    ; "throws"
    ; "transient"
    ; "true"
    ; "try"
    ; "typeof"
    ; "var"
    ; "void"
    ; "volatile"
    ; "while"
    ; "with"
    ; "yield" ]
    |> Util.Maps.Set.of_list

  let op_to_fun (op : Core.Prog.operator) (e1 : type_) (e2 : type_) (ret : type_) =
    match (op, e1.t, e2.t, ret.t) with _ -> None

  let fun_to_fun (path : string) (args : type_ list) (ret : type_) =
    let args = CCList.map (fun (t : type_) -> t.t) args in
    match (path, args, (getReturnType ret).t) with _ -> None
end

module Julia = struct
  let keywords =
    [ "abstract"
    ; "baremodule"
    ; "begin"
    ; "break"
    ; "catch"
    ; "const"
    ; "continue"
    ; "do"
    ; "else"
    ; "elseif"
    ; "end"
    ; "export"
    ; "false"
    ; "finally"
    ; "for"
    ; "function"
    ; "global"
    ; "if"
    ; "import"
    ; "let"
    ; "local"
    ; "macro"
    ; "module"
    ; "mutable"
    ; "primitive"
    ; "quote"
    ; "return"
    ; "struct"
    ; "true"
    ; "try"
    ; "type"
    ; "using"
    ; "while" ]
    |> Util.Maps.Set.of_list

  let op_to_fun (op : Core.Prog.operator) (e1 : type_) (e2 : type_) (ret : type_) =
    match (op, e1.t, e2.t, ret.t) with
    | OpDiv, TInt, TInt, TInt ->
        Some "div"
    | OpMod, TReal, TReal, TReal ->
        Some "mod"
    | _ ->
        None

  let fun_to_fun (path : string) (args : type_ list) (ret : type_) =
    let args = CCList.map (fun (t : type_) -> t.t) args in
    match (path, args, (getReturnType ret).t) with
    (* Math functions - Julia has these built-in *)
    | "sin", [TReal], TReal ->
        Some "sin"
    | "cos", [TReal], TReal ->
        Some "cos"
    | "tan", [TReal], TReal ->
        Some "tan"
    | "sinh", [TReal], TReal ->
        Some "sinh"
    | "cosh", [TReal], TReal ->
        Some "cosh"
    | "tanh", [TReal], TReal ->
        Some "tanh"
    | "exp", [TReal], TReal ->
        Some "exp"
    | "floor", [TReal], TReal ->
        Some "floor"
    | "abs", [TReal], TReal ->
        Some "abs"
    | "sqrt", [TReal], TReal ->
        Some "sqrt"
    | "log", [TReal], TReal ->
        Some "log"
    | "log10", [TReal], TReal ->
        Some "log10"
    | "pow", [TReal; TReal], TReal ->
        Some "^"
    (* Math constants *)
    | "pi", [], TReal ->
        Some "π"
    (* Random functions *)
    | "random", [], TReal ->
        Some "rand"
    (* Clipping functions *)
    | "clip", [TReal; TReal; TReal], TReal ->
        Some "clamp"
    | "clip", [TInt; TInt; TInt], TInt ->
        Some "clamp"
    (* Array functions *)
    | "size", [TArray (_, _)], TInt ->
        Some "length"
    | "length", [TString], TInt ->
        Some "length"
    (* Logical operations *)
    | "not", [TBool], TBool ->
        Some "!"
    | _ ->
        None
end

module Python = struct
  let keywords =
    [ "False"
    ; "None"
    ; "True"
    ; "and"
    ; "as"
    ; "assert"
    ; "async"
    ; "await"
    ; "break"
    ; "class"
    ; "continue"
    ; "def"
    ; "del"
    ; "elif"
    ; "else"
    ; "except"
    ; "finally"
    ; "for"
    ; "from"
    ; "global"
    ; "if"
    ; "import"
    ; "in"
    ; "is"
    ; "lambda"
    ; "nonlocal"
    ; "not"
    ; "or"
    ; "pass"
    ; "raise"
    ; "return"
    ; "try"
    ; "while"
    ; "with"
    ; "yield" ]
    |> Util.Maps.Set.of_list

  let op_to_fun (op : Core.Prog.operator) (e1 : type_) (e2 : type_) (ret : type_) =
    match (op, e1.t, e2.t, ret.t) with _ -> None

  let fun_to_fun (path : string) (args : type_ list) (ret : type_) =
    let args = CCList.map (fun (t : type_) -> t.t) args in
    match (path, args, (getReturnType ret).t) with
    (* Math functions - Python uses math module *)
    | "sin", [TReal], TReal ->
        Some "math.sin"
    | "cos", [TReal], TReal ->
        Some "math.cos"
    | "tan", [TReal], TReal ->
        Some "math.tan"
    | "sinh", [TReal], TReal ->
        Some "math.sinh"
    | "cosh", [TReal], TReal ->
        Some "math.cosh"
    | "tanh", [TReal], TReal ->
        Some "math.tanh"
    | "exp", [TReal], TReal ->
        Some "math.exp"
    | "floor", [TReal], TReal ->
        Some "math.floor"
    | "abs", [TReal], TReal ->
        Some "abs"
    | "abs", [TInt], TInt ->
        Some "abs"
    | "sqrt", [TReal], TReal ->
        Some "math.sqrt"
    | "log", [TReal], TReal ->
        Some "math.log"
    | "log10", [TReal], TReal ->
        Some "math.log10"
    (* Math constants *)
    | "pi", [], TReal ->
        Some "math.pi"
    (* Random functions *)
    | "random", [], TReal ->
        Some "random.random"
    (* Cast - map string to str *)
    | "string", [TInt], TString ->
        Some "str"
    | "string", [TInt16], TString ->
        Some "str"
    | "string", [TReal], TString ->
        Some "str"
    | "string", [TFix16], TString ->
        Some "str"
    | "string", [TBool], TString ->
        Some "str"
    | _ ->
        None
end

module Zig = struct
  let keywords =
    [ "addrspace"
    ; "align"
    ; "allowzero"
    ; "and"
    ; "anyframe"
    ; "anytype"
    ; "asm"
    ; "async"
    ; "await"
    ; "break"
    ; "callconv"
    ; "catch"
    ; "comptime"
    ; "const"
    ; "continue"
    ; "defer"
    ; "else"
    ; "enum"
    ; "errdefer"
    ; "error"
    ; "export"
    ; "extern"
    ; "fn"
    ; "for"
    ; "if"
    ; "inline"
    ; "linksection"
    ; "noalias"
    ; "noinline"
    ; "nosuspend"
    ; "opaque"
    ; "or"
    ; "orelse"
    ; "packed"
    ; "pub"
    ; "resume"
    ; "return"
    ; "struct"
    ; "suspend"
    ; "switch"
    ; "test"
    ; "threadlocal"
    ; "try"
    ; "union"
    ; "unreachable"
    ; "usingnamespace"
    ; "var"
    ; "volatile"
    ; "while" ]
    |> Util.Maps.Set.of_list

  let op_to_fun (op : Core.Prog.operator) (e1 : type_) (e2 : type_) (ret : type_) =
    match (op, e1.t, e2.t, ret.t) with
    | OpMod, TReal, TReal, TReal ->
        Some "fmodf"
    | OpDiv, TInt, TInt, TInt ->
        Some "intDiv"
    | OpMod, TInt, TInt, TInt ->
        Some "intMod"
    | _ ->
        None

  (* Zig maps the runtime builtins to the small prelude emitted at the top of the generated file (see
     [Zig.runtime]). The names below must match the helpers defined there. *)
  let fun_to_fun (path : string) (args : type_ list) (ret : type_) =
    let args = CCList.map (fun (t : type_) -> t.t) args in
    match (path, args, (getReturnType ret).t) with
    (* builtins *)
    | "samplerate", [], TReal ->
        Some "float_samplerate"
    | "random", [], TReal ->
        Some "float_random"
    | "irandom", [], _ ->
        Some "int_random"
    | "clip", [TReal; _; _], TReal ->
        Some "float_clip"
    | "clip", [TInt; _; _], TInt ->
        Some "int_clip"
    | "pi", [], TReal ->
        Some "float_pi"
    | "eps", [], TReal ->
        Some "float_eps"
    (* math *)
    | "sin", [TReal], TReal ->
        Some "sinf"
    | "cos", [TReal], TReal ->
        Some "cosf"
    | "tan", [TReal], TReal ->
        Some "tanf"
    | "sinh", [TReal], TReal ->
        Some "sinhf"
    | "cosh", [TReal], TReal ->
        Some "coshf"
    | "tanh", [TReal], TReal ->
        Some "tanhf"
    | "exp", [TReal], TReal ->
        Some "expf"
    | "log", [TReal], TReal ->
        Some "logf"
    | "log10", [TReal], TReal ->
        Some "log10f"
    | "floor", [TReal], TReal ->
        Some "floorf"
    | "abs", [TReal], TReal ->
        Some "fabsf"
    | "abs", [TInt], TInt ->
        Some "int_abs"
    | "sqrt", [TReal], TReal ->
        Some "sqrtf"
    (* cast *)
    | "int", [TReal], _ ->
        Some "float_to_int"
    | "int", [TInt16], _ ->
        Some "int16_to_int"
    | "int16", [TInt], _ ->
        Some "int_to_int16"
    | "int16", [TReal], _ ->
        Some "float_to_int16"
    | "int16", [TBool], _ ->
        Some "bool_to_int16"
    | "int16", [TInt16], _ ->
        Some "int16_to_int16"
    | "real", [TInt], TReal ->
        Some "int_to_float"
    | "real", [TInt16], TReal ->
        Some "int16_to_float"
    | "real", [TBool], TReal ->
        Some "bool_to_float"
    | "bool", [TInt], _ ->
        Some "int_to_bool"
    | "bool", [TInt16], _ ->
        Some "int16_to_bool"
    | "bool", [TReal], _ ->
        Some "float_to_bool"
    | _ ->
        None
end

let fun_to_fun (lang : Util.Args.code) (path : string) (args : type_ list) (ret : type_) =
  match lang with
  | CppCode ->
      Cpp.fun_to_fun path args ret
  | ZigCode ->
      Zig.fun_to_fun path args ret
  | JavaCode ->
      Java.fun_to_fun path args ret
  | LuaCode ->
      Lua.fun_to_fun path args ret
  | JSCode ->
      Js.fun_to_fun path args ret
  | JuliaCode ->
      Julia.fun_to_fun path args ret
  | PythonCode ->
      Python.fun_to_fun path args ret
  | _ ->
      None

let op_to_fun (lang : Util.Args.code) (op : Core.Prog.operator) (e1 : type_) (e2 : type_) (ret : type_) =
  match lang with
  | CppCode ->
      Cpp.op_to_fun op e1 e2 ret
  | ZigCode ->
      Zig.op_to_fun op e1 e2 ret
  | JavaCode ->
      Java.op_to_fun op e1 e2 ret
  | LuaCode ->
      Lua.op_to_fun op e1 e2 ret
  | JSCode ->
      Js.op_to_fun op e1 e2 ret
  | JuliaCode ->
      Julia.op_to_fun op e1 e2 ret
  | PythonCode ->
      Python.op_to_fun op e1 e2 ret
  | _ ->
      None

let keyword (lang : Util.Args.code) id =
  match lang with
  | CppCode ->
      if Util.Maps.Set.mem id Cpp.keywords then id ^ "_" else id
  | ZigCode ->
      if Util.Maps.Set.mem id Zig.keywords then id ^ "_" else id
  | JavaCode ->
      if Util.Maps.Set.mem id Java.keywords then id ^ "_" else id
  | LuaCode ->
      if Util.Maps.Set.mem id Lua.keywords then id ^ "_" else id
  | JSCode ->
      if Util.Maps.Set.mem id Js.keywords then id ^ "_" else id
  | JuliaCode ->
      if Util.Maps.Set.mem id Julia.keywords then id ^ "_" else id
  | PythonCode ->
      if Util.Maps.Set.mem id Python.keywords then id ^ "_" else id
  | _ ->
      id
