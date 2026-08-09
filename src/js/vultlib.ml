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

open Js_of_ocaml
open Util

(** {1 Vultlib — JS bindings for the Vult compiler}

    This module provides all the JS↔OCaml conversion logic for exposing the Vult compiler
    to JavaScript. It is used by both the browser target ([vultweb]) and can be reused by
    other JS-based entry points (e.g. Node.js).

    {2 JS Object Types}

    The following class types describe the shape of JS objects exchanged across the boundary.

    {3 Error object}
    Returned when compilation fails. Each error contains:
    - [msg]  — the full error message including source indicator
    - [file] — the source file name (or ["-"] for simple errors)
    - [line] — 1-based line number (0 for simple errors)
    - [col]  — 0-based column number

    {3 File/code object} ([js_file_code])
    Used for both input and output. Fields are optional:
    - [file] — filename (defaults to ["live.vult"] for input; always present for output)
    - [code] — source code string (input) or generated code string (output)

    {3 Options object}
    Optional fields to configure code generation:
    - [output]   — output file prefix (e.g. ["synth"] produces ["synth.js"])
    - [real]     — numeric type: ["float"] (default) or ["fixed"]
    - [template] — code template name (e.g. ["performance"])
    - [includes] — array of additional include paths for module resolution

    {2 Result objects}

    All API functions that invoke the compiler return an array of result objects.
    Each result has the shape [{ type: string, value: ... }] where [type] is one of:
    - ["code"]         — [value] is an array of [{file, code}] objects with generated files
    - ["errors"]       — [value] is an array of error objects (see above)
    - ["version"]      — [value] is the version string
    - ["message"]      — [value] is an informational message string
    - ["parsed"]       — [value] is the pretty-printed parse tree
    - ["typed"]        — [value] is the pretty-printed typed AST
    - ["eval"]         — [value] is the evaluation result string
    - ["prog"]         — [value] is the dumped program IR
    - ["interpret"]    — [value] is the interpreter output
    - ["check"]        — [value] is [true] (code is valid)
    - ["audio"]        — [value] is a message about rendered audio
    - ["dependencies"] — [value] is an array of dependency file names

    {2 API Functions}

    - [version]        — compiler version string (property, not a function)
    - [main(files)]    — full compiler invocation; takes an array of [{file?, code?}] objects
    - [generateJs(files, options)]     — generate JavaScript code
    - [generateCpp(files, options)]    — generate C++ code
    - [generateLua(files, options)]    — generate Lua code
    - [generateJava(files, options)]   — generate Java code
    - [generateJulia(files, options)]  — generate Julia code
    - [generatePython(files, options)] — generate Python code

    All [generate*] functions take an array of file/code objects and an options object,
    and return an array of result objects. *)

(** JS representation of a compiler error.
    Corresponds to JS: [{msg: string, file: string, line: number, col: number}] *)
class type error = object
  method msg : Js.js_string Js.t Js.readonly_prop

  method file : Js.js_string Js.t Js.readonly_prop

  method line : int Js.readonly_prop

  method col : int Js.readonly_prop
end

(** JS representation of an input or output file.
    Corresponds to JS: [{file?: string, code?: string}].
    For input: [file] defaults to ["live.vult"], [code] defaults to [""].
    For output: both fields are always present. *)
class type js_file_code = object
  method file : Js.js_string Js.t Js.optdef Js.readonly_prop

  method code : Js.js_string Js.t Js.optdef Js.readonly_prop
end

(** JS representation of code generation options.
    Corresponds to JS: [{output?: string, real?: string, template?: string, includes?: string[]}].
    - [output]   — prefix for generated file names
    - [real]     — ["float"] (default) or ["fixed"]
    - [template] — template name (e.g. ["performance"], ["performance-bun"])
    - [includes] — additional module search paths *)
class type options = object
  method output : Js.js_string Js.t Js.optdef Js.readonly_prop

  method real : Js.js_string Js.t Js.optdef Js.readonly_prop

  method template : Js.js_string Js.t Js.optdef Js.readonly_prop

  method includes : Js.js_string Js.t Js.js_array Js.t Js.optdef Js.readonly_prop
end

(** {2 Conversion functions}

    These functions convert between JS objects and OCaml types used by the compiler. *)

(** Converts a JS file/code object into an [Args.input].
    The [file] field is used as the filename and [code] as the source text.
    If [file] is absent, defaults to ["live.vult"]. If [code] is absent, defaults to [""]. *)
let convertInputFile (file : js_file_code Js.t) : Args.input =
  let file_name = Js.Optdef.case file##.file (fun () -> "live.vult") Js.to_string in
  let code = Js.Optdef.case file##.code (fun () -> "") Js.to_string in
  Args.Code (file_name, code)

(** Converts a JS array of file/code objects into a list of [Args.input]. *)
let convertInputFiles (files : js_file_code Js.t Js.js_array Js.t) : Args.input list =
  files |> Js.to_array |> Array.to_list |> CCList.map convertInputFile

(** Converts an [Error.t] into a JS error object with [msg], [file], [line], and [col] fields. *)
let convertOutputError (error : Error.t) : error Js.t =
  let msg, file, line, col = Error.reportErrorStringNoLoc error in
  object%js
    val msg = Js.string msg

    val file = Js.string file

    val line = line

    val col = col
  end

(** Converts a list of errors into a JS array of error objects. *)
let convertOutputErrors (errors : Error.t list) : error Js.t Js.js_array Js.t =
  errors |> CCList.map convertOutputError |> Array.of_list |> Js.array

(** Converts a generated code file [(Pla.t, filename)] into a JS [{file, code}] object. *)
let convertOutputFile ((code, filename) : Pla.t * string) : js_file_code Js.t =
  object%js
    val file = Js.Optdef.return (Js.string filename)

    val code = Js.Optdef.return (Js.string (Pla.print code))
  end

(** Converts a list of generated files into a JS array of [{file, code}] objects. *)
let convertOutputFiles (files : (Pla.t * string) list) : js_file_code Js.t Js.js_array Js.t =
  files |> CCList.map convertOutputFile |> Array.of_list |> Js.array

(** Converts a single compiler [Args.output] into a JS result object [{type, value}]. *)
let showResult (result : Args.output) : Js.Unsafe.any =
  match result with
  | Version v ->
      Js.Unsafe.inject
        (object%js
           val _type = Js.string "version"

           val value = Js.string v
        end )
  | Message v ->
      Js.Unsafe.inject
        (object%js
           val _type = Js.string "message"

           val value = Js.string v
        end )
  | Dependencies deps ->
      Js.Unsafe.inject
        (object%js
           val _type = Js.string "dependencies"

           val value = deps |> CCList.map Js.string |> Array.of_list |> Js.array
        end )
  | ParsedCode v ->
      Js.Unsafe.inject
        (object%js
           val _type = Js.string "parsed"

           val value = Js.string v
        end )
  | EvalResult v ->
      Js.Unsafe.inject
        (object%js
           val _type = Js.string "eval"

           val value = Js.string v
        end )
  | Typed v ->
      Js.Unsafe.inject
        (object%js
           val _type = Js.string "typed"

           val value = Js.string v
        end )
  | GeneratedCode files ->
      Js.Unsafe.inject
        (object%js
           val _type = Js.string "code"

           val value = convertOutputFiles files
        end )
  | Prog v ->
      Js.Unsafe.inject
        (object%js
           val _type = Js.string "prog"

           val value = Js.string v
        end )
  | Interpret v ->
      Js.Unsafe.inject
        (object%js
           val _type = Js.string "interpret"

           val value = Js.string v
        end )
  | CheckOk ->
      Js.Unsafe.inject
        (object%js
           val _type = Js.string "check"

           val value = Js.bool true
        end )
  | AudioRendered msg ->
      Js.Unsafe.inject
        (object%js
           val _type = Js.string "audio"

           val value = Js.string msg
        end )
  | Errors errors ->
      Js.Unsafe.inject
        (object%js
           val _type = Js.string "errors"

           val value = convertOutputErrors errors
        end )

(** Converts a list of compiler outputs into a JS array of result objects. *)
let showResults (results : Args.output list) : Js.Unsafe.any Js.js_array Js.t =
  results |> CCList.map showResult |> Array.of_list |> Js.array

(** Converts a JS code name string to [Args.code].
    Accepted values: ["c"], ["cpp"], ["js"], ["lua"], ["java"], ["julia"], ["python"].
    Returns [NoCode] for unrecognized values. *)
let convertCodeName (name : Js.js_string Js.t) : Args.code =
  match Js.to_string name with
  | "c" | "cpp" ->
      Args.CppCode
  | "js" ->
      Args.JSCode
  | "lua" ->
      Args.LuaCode
  | "java" ->
      Args.JavaCode
  | "julia" ->
      Args.JuliaCode
  | "python" ->
      Args.PythonCode
  | _ ->
      Args.NoCode

(** Converts a JS real format name to [Args.real_format].
    ["fixed"] maps to [Fixed]; everything else (including ["float"], ["double"], ["js"]) maps to [Float]. *)
let convertRealName (name : Js.js_string Js.t) : Args.real_format =
  match Js.to_string name with "fixed" -> Args.Fixed | _ -> Args.Float

(** Applies a JS options object to an [Args.args] record, setting [output], [real], [template],
    and [includes] fields when present. *)
let applyOptions (args : Args.args) (opts : options Js.t) : unit =
  Js.Optdef.iter opts##.output (fun v -> args.output <- Some (Js.to_string v)) ;
  Js.Optdef.iter opts##.real (fun v -> args.real <- convertRealName v) ;
  Js.Optdef.iter opts##.template (fun v -> args.template <- Some (Js.to_string v)) ;
  Js.Optdef.iter opts##.includes (fun v ->
      args.includes <- v |> Js.to_array |> Array.to_list |> CCList.map Js.to_string )

(** {2 API Functions} *)

(** Compiler version string. *)
let version : Js.js_string Js.t = Js.string Core.Version.version

(** Full compiler invocation. Takes a JS array of [{file?, code?}] input objects and runs
    the compiler with default arguments. Returns an array of result objects.

    JS usage: [vult.main(\[{file: "synth.vult", code: "..."}\])] *)
let main (js_args : js_file_code Js.t Js.js_array Js.t) : Js.Unsafe.any Js.js_array Js.t =
  let args = {Args.default_arguments with files= convertInputFiles js_args} in
  let results = Driver.Cli.driver args in
  showResults results

(** Shared helper for language-specific code generation.
    Configures [Args.args] with the given input files, options, and target language,
    then invokes the compiler and returns the results. *)
let codeGeneration (files : js_file_code Js.t Js.js_array Js.t) (opts : options Js.t) (code_type : Args.code) :
    Js.Unsafe.any Js.js_array Js.t =
  let args = {Args.default_arguments with files= convertInputFiles files; code= code_type} in
  let () = applyOptions args opts in
  let results = Driver.Cli.driver args in
  showResults results

(** Generate JavaScript code from Vult source files.

    JS usage: [vult.generateJs(\[{file: "synth.vult", code: "..."}\], {output: "synth"})] *)
let generateJs (files : js_file_code Js.t Js.js_array Js.t) (opts : options Js.t) : Js.Unsafe.any Js.js_array Js.t =
  codeGeneration files opts Args.JSCode

(** Generate C++ code from Vult source files.

    Returns result objects containing [.h] and [.cpp] files.
    JS usage: [vult.generateCpp(\[{file: "synth.vult", code: "..."}\], {output: "synth"})] *)
let generateCpp (files : js_file_code Js.t Js.js_array Js.t) (opts : options Js.t) : Js.Unsafe.any Js.js_array Js.t =
  codeGeneration files opts Args.CppCode

(** Generate Lua code from Vult source files.

    JS usage: [vult.generateLua(\[{file: "synth.vult", code: "..."}\], {output: "synth"})] *)
let generateLua (files : js_file_code Js.t Js.js_array Js.t) (opts : options Js.t) : Js.Unsafe.any Js.js_array Js.t =
  codeGeneration files opts Args.LuaCode

(** Generate Java code from Vult source files.

    JS usage: [vult.generateJava(\[{file: "synth.vult", code: "..."}\], {output: "synth"})] *)
let generateJava (files : js_file_code Js.t Js.js_array Js.t) (opts : options Js.t) : Js.Unsafe.any Js.js_array Js.t =
  codeGeneration files opts Args.JavaCode

(** Generate Julia code from Vult source files.

    JS usage: [vult.generateJulia(\[{file: "synth.vult", code: "..."}\], {output: "synth"})] *)
let generateJulia (files : js_file_code Js.t Js.js_array Js.t) (opts : options Js.t) : Js.Unsafe.any Js.js_array Js.t =
  codeGeneration files opts Args.JuliaCode

(** Generate Python code from Vult source files.

    JS usage: [vult.generatePython(\[{file: "synth.vult", code: "..."}\], {output: "synth"})] *)
let generatePython (files : js_file_code Js.t Js.js_array Js.t) (opts : options Js.t) : Js.Unsafe.any Js.js_array Js.t =
  codeGeneration files opts Args.PythonCode

(** {2 LSP / Editor Integration API}

    These functions expose language intelligence features for use in web-based editors
    (CodeMirror, Monaco, Ace, etc.) without needing a full LSP server.

    {3 Semantic Tokens}
    - [getSemanticTokens(code)] — returns a flat int array in LSP delta-encoded format:
      each group of 5 ints is [\[deltaLine, deltaStartChar, length, tokenType, tokenModifiers\]].
      Token types: 0=variable, 1=comment, 2=keyword, 3=type, 4=number, 5=string, 6=operator,
      7=punctuation, 8=function.

    {3 Diagnostics}
    - [getDiagnostics(code)] — returns an array of [{message, severity, startLine, startCol, endLine, endCol}].
      Severity: 1=error, 2=warning, 3=info, 4=hint. Lines and columns are 0-based.

    {3 Completions}
    - [getCompletions(code)] — returns an array of [{label, kind, detail, documentation}].
      Kind values follow LSP CompletionItemKind: 3=function, 7=struct, 13=enum, 14=keyword,
      20=enum_member, 21=constant.

    {3 Document Symbols}
    - [getDocumentSymbols(code)] — returns an array of [{name, kind, startLine, startCol, endLine, endCol}].
      Kind values follow LSP SymbolKind: 10=enum, 12=function, 14=constant, 22=enum_member, 23=struct.

    {3 Hover}
    - [getHoverInfo(code, line, col)] — returns a type signature string or [null].
      Line is 1-based, column is 0-based (matching Vult's internal convention). *)

(** Helper: parse a code string into statements. Returns an empty list on error. *)
let parseCode (code : string) : Pparser.Syntax.top_stmt list =
  try
    let args = {Args.default_arguments with files= [Args.Code ("live.vult", code)]; check= true} in
    let parsed, _ = Driver.Loader.loadFiles args args.files in
    CCList.flatten @@ CCList.map (fun (p : Pparser.Parse.parsed_file) -> p.stmts) parsed
  with _ -> []

(** Helper: typecheck a code string into typed statements. Returns [None] on error. *)
let typecheckCode (code : string) : Core.Typed.top_stmt list option =
  try
    let args = {Args.default_arguments with files= [Args.Code ("live.vult", code)]; check= true} in
    let parsed, _ = Driver.Loader.loadFiles args args.files in
    let _env, stmts = Core.Typechecking.typecheck_and_elaborate args parsed in
    Some stmts
  with _ -> None

(** Returns a delta-encoded int array of semantic tokens for syntax highlighting.

    JS usage: [vult.getSemanticTokens("fun foo(x:real):real return x + 1.0;")] *)
let getSemanticTokens (code_string : Js.js_string Js.t) : int Js.js_array Js.t =
  let code = Js.to_string code_string in
  let tokens = Vult_lsp.SemanticTokens.get_semantic_tokens code in
  tokens |> Array.of_list |> Js.array

(** Returns an array of diagnostic objects from type checking.

    JS usage: [vult.getDiagnostics("fun foo(x:real):real return x + 1;")] *)
let getDiagnostics (code_string : Js.js_string Js.t) : Js.Unsafe.any Js.js_array Js.t =
  let code = Js.to_string code_string in
  let diagnostics = Vult_lsp.Diagnostics.get_diagnostics code "live.vult" in
  let convert_diagnostic (d : Yojson.Safe.t) : Js.Unsafe.any =
    let open Yojson.Safe.Util in
    let range = d |> member "range" in
    let start_pos = range |> member "start" in
    let end_pos = range |> member "end" in
    Js.Unsafe.inject
      (object%js
         val message = Js.string (d |> member "message" |> to_string)

         val severity = d |> member "severity" |> to_int

         val startLine = start_pos |> member "line" |> to_int

         val startCol = start_pos |> member "character" |> to_int

         val endLine = end_pos |> member "line" |> to_int

         val endCol = end_pos |> member "character" |> to_int
      end )
  in
  diagnostics |> CCList.map convert_diagnostic |> Array.of_list |> Js.array

(** Returns an array of completion items from the parsed AST.

    JS usage: [vult.getCompletions("fun foo(x:real):real return x + 1.0;")] *)
let getCompletions (code_string : Js.js_string Js.t) : Js.Unsafe.any Js.js_array Js.t =
  let code = Js.to_string code_string in
  let stmts = parseCode code in
  let completions = Vult_lsp.Completion.get_completions stmts in
  let convert_completion (c : Vult_lsp.Completion.completion_item) : Js.Unsafe.any =
    Js.Unsafe.inject
      (object%js
         val label = Js.string c.label

         val kind = c.kind

         val detail = Js.Optdef.option (Option.map Js.string c.detail)

         val documentation = Js.Optdef.option (Option.map Js.string c.documentation)
      end )
  in
  completions |> CCList.map convert_completion |> Array.of_list |> Js.array

(** Returns an array of document symbol objects from the parsed AST.

    JS usage: [vult.getDocumentSymbols("fun foo(x:real):real return x + 1.0;")] *)
let getDocumentSymbols (code_string : Js.js_string Js.t) : Js.Unsafe.any Js.js_array Js.t =
  let code = Js.to_string code_string in
  let stmts = parseCode code in
  let symbols = Vult_lsp.DocumentSymbols.get_document_symbols stmts in
  let convert_symbol (s : Yojson.Safe.t) : Js.Unsafe.any =
    let open Yojson.Safe.Util in
    let range = s |> member "range" in
    let start_pos = range |> member "start" in
    let end_pos = range |> member "end" in
    Js.Unsafe.inject
      (object%js
         val name = Js.string (s |> member "name" |> to_string)

         val kind = s |> member "kind" |> to_int

         val startLine = start_pos |> member "line" |> to_int

         val startCol = start_pos |> member "character" |> to_int

         val endLine = end_pos |> member "line" |> to_int

         val endCol = end_pos |> member "character" |> to_int
      end )
  in
  symbols |> CCList.map convert_symbol |> Array.of_list |> Js.array

(** Returns a type signature string for the symbol at the given position, or [null].

    JS usage: [vult.getHoverInfo("fun foo(x:real):real return x + 1.0;", 1, 8)] *)
let getHoverInfo (code_string : Js.js_string Js.t) (line : int) (col : int) : Js.js_string Js.t Js.opt =
  let code = Js.to_string code_string in
  match typecheckCode code with
  | Some typed_stmts -> (
    match Vult_lsp.Hover.get_hover_info typed_stmts line col with
    | Some text ->
        Js.some (Js.string text)
    | None ->
        Js.null )
  | None ->
      Js.null
