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

(** {1 Vultweb — Browser entry point for the Vult compiler}

    This module is the browser-embeddable entry point for the Vult compiler. It imports
    [Vultlib] and exports a [vult] object to the global scope via [Js.export].

    After loading [vultweb.js] in a browser, the API is available as [vult.*].

    {2 Exported API}

    {b Properties:}
    - [vult.version] — compiler version string

    {b Functions:}
    - [vult.checkCode(code)] — type-check a Vult code string and return Ace-editor
      formatted error annotations. Returns an empty array if the code is valid.
    - [vult.generateJSCode(code)] — compile a Vult code string directly to JavaScript.
      Convenience wrapper that sets [output="live"] and [real="float"].
    - [vult.main(files)] — full compiler invocation (see {!Vultlib.main})
    - [vult.generateJs(files, options)] — generate JavaScript (see {!Vultlib.generateJs})
    - [vult.generateCpp(files, options)] — generate C++ (see {!Vultlib.generateCpp})
    - [vult.generateLua(files, options)] — generate Lua (see {!Vultlib.generateLua})
    - [vult.generateJava(files, options)] — generate Java (see {!Vultlib.generateJava})
    - [vult.generateJulia(files, options)] — generate Julia (see {!Vultlib.generateJulia})
    - [vult.generatePython(files, options)] — generate Python (see {!Vultlib.generatePython})

    {2 Ace Editor Error Format}

    [checkCode] returns errors formatted for the Ace editor annotation API:
    - [text]   — error message string
    - [row]    — 0-based line number
    - [column] — 0-based column number
    - [type]   — always ["error"]
    - [raw]    — raw error message (same as [text]) *)

(** Ace-editor formatted error annotation.
    Corresponds to JS: [{text: string, row: number, column: number, type: string, raw: string}] *)
class type ace_error = object
  method text : Js.js_string Js.t Js.readonly_prop

  method row : int Js.readonly_prop

  method column : int Js.readonly_prop

  method _type : Js.js_string Js.t Js.readonly_prop

  method raw : Js.js_string Js.t Js.readonly_prop
end

(** Wraps a code string into a single-element JS array of [{file, code}] input objects
    using ["live.vult"] as the filename. *)
let makeInputFiles (code_string : Js.js_string Js.t) : Vultlib.js_file_code Js.t Js.js_array Js.t =
  let file : Vultlib.js_file_code Js.t =
    object%js
      val file = Js.Optdef.return (Js.string "live.vult")

      val code = Js.Optdef.return code_string
    end
  in
  Js.array [|file|]

(** Quick JS generation from a code string. Sets [output="live"] and [real="float"]
    with no template. Returns an array of result objects (see {!Vultlib}).

    JS usage: [vult.generateJSCode("fun foo(x:real):real return x + 1.0;")] *)
let generateJSCode (code_string : Js.js_string Js.t) : Js.Unsafe.any Js.js_array Js.t =
  let files = makeInputFiles code_string in
  let opts : Vultlib.options Js.t =
    object%js
      val output = Js.Optdef.return (Js.string "live")

      val real = Js.Optdef.return (Js.string "js")

      val template = Js.Optdef.empty

      val includes = Js.Optdef.empty
    end
  in
  Vultlib.generateJs files opts

(** Converts a compiler error into an Ace-editor annotation object.
    Line numbers are converted from 1-based to 0-based (Ace convention). *)
let convertErrorToAce (error : Error.t) : ace_error Js.t =
  let msg, _file, line, col = Error.reportErrorStringNoLoc error in
  object%js
    val text = Js.string msg

    val row = max (line - 1) 0

    val column = col

    val _type = Js.string "error"

    val raw = Js.string msg
  end

(** Type-checks a Vult code string and returns an array of Ace-editor error annotations.
    Returns an empty array if the code is valid.

    JS usage: [vult.checkCode("fun foo(x:real):real return x + 1.0;")] *)
let checkCode (code_string : Js.js_string Js.t) : ace_error Js.t Js.js_array Js.t =
  let args = {Args.default_arguments with files= [Code ("live.vult", Js.to_string code_string)]; check= true} in
  let results = Driver.Cli.driver args in
  let errors =
    CCList.filter_map
      (fun (result : Args.output) -> match result with Errors errors -> Some errors | _ -> None)
      results
    |> CCList.flatten
  in
  errors |> CCList.map convertErrorToAce |> Array.of_list |> Js.array

(** Export the [vult] object to the global scope. In a browser, this makes
    the API available as [window.vult.*] or [globalThis.vult.*].
    When loaded via Node.js [require()], it is available as [module.exports.vult.*]. *)
let _ =
  Js.export "vult"
    (Js.Unsafe.obj
       [| ("main", Js.Unsafe.inject (Js.wrap_callback Vultlib.main))
        ; ("version", Js.Unsafe.inject Vultlib.version)
        ; ("checkCode", Js.Unsafe.inject (Js.wrap_callback checkCode))
        ; ("generateJSCode", Js.Unsafe.inject (Js.wrap_callback generateJSCode))
        ; ("generateJs", Js.Unsafe.inject (Js.wrap_callback (fun a b -> Vultlib.generateJs a b)))
        ; ("generateCpp", Js.Unsafe.inject (Js.wrap_callback (fun a b -> Vultlib.generateCpp a b)))
        ; ("generateLua", Js.Unsafe.inject (Js.wrap_callback (fun a b -> Vultlib.generateLua a b)))
        ; ("generateJava", Js.Unsafe.inject (Js.wrap_callback (fun a b -> Vultlib.generateJava a b)))
        ; ("generateJulia", Js.Unsafe.inject (Js.wrap_callback (fun a b -> Vultlib.generateJulia a b)))
        ; ("generatePython", Js.Unsafe.inject (Js.wrap_callback (fun a b -> Vultlib.generatePython a b))) |] )
