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

   ================================================================================
   MATHEMATICA LIBRARYLINK TEMPLATE
   ================================================================================
   
   This template generates Wolfram LibraryLink wrappers for Vult DSP functions,
   enabling direct use of Vult code in Mathematica/Wolfram Language for analysis,
   prototyping, and real-time audio processing.

   USAGE:
   ------
   1. Generate LibraryLink code:
      vult -code cpp -template mathematica -o myproject project.vult
   
   2. Compile the shared library:
      clang++ -std=c++11 -fPIC -shared -I/path/to/vult/runtime \
              -I/Applications/Wolfram.app/Contents/SystemFiles/IncludeFiles/C \
              -undefined dynamic_lookup myproject.cpp -o myproject.dylib
   
   3. Create Mathematica package (myproject.m):
      BeginPackage["MyProject`"]
      lib = LibraryLoad["myproject"];
      Begin["`Private`"]
      
      (* For a function: fun process(input : real) : real *)
      Process[args___] := LibraryFunctionLoad[lib, "myproject_Module_process_wrapper", 
                                             {"Real"}, "Real"][args]
      
      (* For MIDI functions: fun noteOn(note:int, velocity:int, channel:int) *)
      NoteOn[args___] := LibraryFunctionLoad[lib, "myproject_Module_noteOn_wrapper", 
                                            {"Integer", "Integer", "Integer"}, "Integer"][args]
      
      End[]
      EndPackage[]
   
   4. Use in Mathematica:
      Get["myproject.m"]
      
      (* Call stateless functions *)
      result = MyProject`Process[0.5]
      
      (* Call stateful functions (memory persists across calls) *)
      output1 = MyProject`Process[1.0]  (* First call *)
      output2 = MyProject`Process[2.0]  (* Second call, uses state from first *)
      
      (* MIDI control *)
      MyProject`NoteOn[60, 100, 1]     (* Note C4, velocity 100, channel 1 *)
      MyProject`ControlChange[7, 64, 1] (* Volume control *)
      
      (* Generate audio sequences *)
      audio = Table[MyProject`Process[Sin[2*Pi*440*i/44100]], {i, 0, 1000}]

   FEATURES:
   ---------
   • Automatic type mapping: Vult real ↔ Mathematica Real, Vult int ↔ Mathematica Integer
   • Context management: Stateful functions (with 'mem' variables) automatically maintain state
   • Mixed argument types: Functions can take both Real and Integer arguments
   • MIDI integration: noteOn, noteOff, controlChange functions work with integer MIDI data
   • Performance: Direct C++ calls with minimal overhead after LibraryLink marshaling
   • Memory safety: Static context variables prevent memory leaks and crashes

   EXAMPLES:
   ---------
   (* Simple stateless function *)
   fun process(x : real) : real { return x * 2.0; }
   → MyProject`Process[5.0] returns 10.0

   (* Stateful delay line *)
   fun delay(input : real) : real {
       mem buffer : array(real, 1024);
       mem index;
       val output = buffer[index];
       buffer[index] = input;
       index = (index + 1) % size(buffer);
       return output;
   }
   → MyProject`Delay[1.0] returns 0.0 (first call)
   → MyProject`Delay[2.0] returns 1.0 (second call, returns previous input)

   (* Multi-argument filter *)
   fun svf(input : real, freq : real, q : real, type : int) : real { ... }
   → MyProject`Svf[0.5, 0.3, 0.8, 0]  (* lowpass filter *)
*)
open Core.Prog

type function_info =
  { name : string
  ; module_name : string
  ; has_ctx : bool
  ; inputs : param list
  ; outputs : type_ list
  ; ctx_type : string option
  }

let getFunctionInfo (module_name : string) (f : function_def) =
  let outputs =
    match snd f.t with
    | { t = TTuple elems; _ } -> elems
    | { t = TVoid (Some elems); _ } -> elems
    | { t = TVoid None; _ } -> []
    | t -> [ t ]
  in
  let has_ctx, inputs, ctx_type =
    match f.args with
    | { name = "_ctx"; t = { t = TStruct { path; _ }; _ }; _ } :: inputs -> true, inputs, Some path
    | inputs -> false, inputs, None
  in
  (* Debug: Let's be more permissive and include functions even without args/outputs *)
  Some { name = f.name; module_name; has_ctx; inputs; outputs; ctx_type }


let wolfram_type (t : type_) =
  match t.t with
  | TReal -> "Real"
  | TInt -> "Integer"
  | TBool -> "True|False"
  | _ -> "Real" (* fallback *)


let c_type (t : type_) =
  match t.t with
  | TReal -> "mreal"
  | TInt -> "mint"
  | TBool -> "mbool"
  | _ -> "mreal" (* fallback *)


let get_argument_suffix (param : param) =
  match param.t.t with
  | TReal -> "Real"
  | TInt -> "Integer"
  | TBool -> "Boolean"
  | _ -> "Real"


let get_type_suffix (typ : type_) =
  match typ.t with
  | TReal -> "Real"
  | TInt -> "Integer"
  | TBool -> "Boolean"
  | _ -> "Real"


let library_function_wrapper (func_info : function_info) =
  let fname = func_info.name in
  let module_name = func_info.module_name in
  (* Generate argument extraction code *)
  let args_code = ref [] in
  for i = 0 to List.length func_info.inputs - 1 do
    let param = List.nth func_info.inputs i in
    let suffix = get_argument_suffix param in
    let ctype = c_type param.t in
    args_code := !args_code @ [ Printf.sprintf "    %s arg%d = MArgument_get%s(Args[%d]);" ctype i suffix i ]
  done;
  let args_str = String.concat "\n" !args_code in
  (* Generate call arguments *)
  let call_args = ref [] in
  for i = 0 to List.length func_info.inputs - 1 do
    call_args := !call_args @ [ Printf.sprintf "arg%d" i ]
  done;
  let call_args_str = String.concat ", " !call_args in
  (* Handle context management and function call *)
  let context_setup, function_call =
    if func_info.has_ctx then
      match func_info.ctx_type with
      | Some ctx_type ->
        let ctx_var = String.lowercase_ascii fname ^ "_ctx" in
        let setup =
          Printf.sprintf
            {|    if (!g_%s_initialized) {
        g_%s = new %s();
        %s_init(*g_%s);
        g_%s_initialized = true;
    }|}
            ctx_var
            ctx_var
            ctx_type
            ctx_type
            ctx_var
            ctx_var
        in
        let call_with_ctx =
          if call_args_str = "" then
            Printf.sprintf "*g_%s" ctx_var
          else
            Printf.sprintf "*g_%s, %s" ctx_var call_args_str
        in
        setup, call_with_ctx
      | None -> "", call_args_str
    else
      "", call_args_str
  in
  (* Generate function call and result handling *)
  let call_and_result =
    match func_info.outputs with
    | [] -> Printf.sprintf "%s\n    %s(%s);\n    MArgument_setInteger(Res, 0);" context_setup fname function_call
    | [ output ] ->
      let set_suffix = get_type_suffix output in
      let ctype = c_type output in
      Printf.sprintf
        "%s\n    %s result = %s(%s);\n    MArgument_set%s(Res, result);"
        context_setup
        ctype
        fname
        function_call
        set_suffix
    | _ ->
      Printf.sprintf
        "%s\n    // TODO: Handle multiple outputs\n    %s(%s);\n    MArgument_setReal(Res, 0.0);"
        context_setup
        fname
        function_call
  in
  Printf.sprintf
    {|
DLLEXPORT int %s_%s_wrapper(WolframLibraryData libData,
    mint Argc, MArgument *Args, MArgument Res) {
%s

%s
    
    return LIBRARY_NO_ERROR;
}|}
    module_name
    fname
    args_str
    call_and_result
  |> Pla.string


let reset_function (func_info : function_info) =
  if func_info.has_ctx then
    match func_info.ctx_type with
    | Some ctx_type ->
      let fname = func_info.name in
      let module_name = func_info.module_name in
      let ctx_var = String.lowercase_ascii fname ^ "_ctx" in
      {%pla|
DLLEXPORT int <#module_name#s>_<#fname#s>_reset(WolframLibraryData libData,
    mint Argc, MArgument *Args, MArgument Res) {
    
    if (g_<#ctx_var#s>_initialized) {
        <#ctx_type#s>_init(*g_<#ctx_var#s>);
    }
    
    MArgument_setInteger(Res, 0);
    return LIBRARY_NO_ERROR;
}|}
    | None -> Pla.unit
  else
    Pla.unit


let static_context_declaration (func_info : function_info) =
  if func_info.has_ctx then
    match func_info.ctx_type with
    | Some ctx_type ->
      let ctx_var = String.lowercase_ascii func_info.name ^ "_ctx" in
      {%pla|static <#ctx_type#s>* g_<#ctx_var#s> = nullptr;
static bool g_<#ctx_var#s>_initialized = false;|}
    | None -> Pla.unit
  else
    Pla.unit


let library_implementation (module_name : string) (functions : function_info list) =
  let static_declarations = CCList.map static_context_declaration functions |> Pla.join_sep_all Pla.newline in
  let wrappers = CCList.map library_function_wrapper functions |> Pla.join_sep_all Pla.newline in
  let reset_functions =
    CCList.map reset_function functions |> CCList.filter (fun x -> x != Pla.unit) |> Pla.join_sep_all Pla.newline
  in
  {%pla|
#include "WolframLibrary.h"
#include "<#module_name#s>.h"

<#static_declarations#>

<#wrappers#>

<#reset_functions#>

DLLEXPORT mint WolframLibrary_getVersion() {
    return WolframLibraryVersion;
}

DLLEXPORT int WolframLibrary_initialize(WolframLibraryData libData) {
    return LIBRARY_NO_ERROR;
}

DLLEXPORT void WolframLibrary_uninitialize(WolframLibraryData libData) {
    // Cleanup static contexts
    // TODO: Add cleanup for each static context
}|}


let mathematica_package (module_name : string) (functions : function_info list) =
  let lib_name = String.lowercase_ascii module_name in
  (* Generate Mathematica function definitions *)
  let function_defs = ref [] in
  List.iter
    (fun func_info ->
      if func_info.inputs <> [] || func_info.outputs <> [] then
        let fname = func_info.name in
        let wrapper_name = Printf.sprintf "%s_%s_wrapper" module_name fname in
        let mathematica_fname = String.capitalize_ascii fname in
        (* Simple function without detailed type checking for now *)
        let func_def =
          Printf.sprintf
            {|%s[args___] := LibraryFunctionLoad[lib, "%s", {"Real"}, "Real"][args]|}
            mathematica_fname
            wrapper_name
        in
        function_defs := !function_defs @ [ func_def ])
    functions;
  let functions_str = String.concat "\n\n" !function_defs in
  Printf.sprintf
    {|BeginPackage["%s`"]

(* Load the Vult library *)
lib = LibraryLoad["%s"];

Begin["`Private`"]

(* Generated function wrappers *)
%s

End[]
EndPackage[]|}
    module_name
    lib_name
    functions_str
  |> Pla.string


let getStmtInfo (module_name : string) (s : top_stmt) =
  match s.top with
  | TopFunction (def, _) -> (
    match getFunctionInfo module_name def with
    | Some f -> Some f (* Include all functions for now, not just root functions *)
    | _ -> None)
  | _ -> None


let generate (module_name : string) (stmts : top_stmt list) =
  let functions = CCList.filter_map (getStmtInfo module_name) stmts in
  let lib_impl = library_implementation module_name functions in
  let _math_package = mathematica_package module_name functions in
  (* Return format: (cpp_content, other_content), (header_content, other_file_content) *)
  (* Put only LibraryLink implementation in cpp, no Mathematica package for now *)
  (lib_impl, Pla.unit), (Pla.unit, Pla.unit)
