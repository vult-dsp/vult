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

(** {1 Vultlib JS — Standalone JS entry point for the Vultlib API}

    This module exports the [vultlib] object, providing the core Vult compiler API
    without the browser-specific helpers found in [vultweb].

    After loading [vultlib.js], the API is available as [vultlib.*].

    {2 Exported API}

    {b Properties:}
    - [vultlib.version] — compiler version string

    {b Functions:}
    - [vultlib.main(files)] — full compiler invocation (see {!Vultlib.main})
    - [vultlib.generateJs(files, options)]     — generate JavaScript
    - [vultlib.generateCpp(files, options)]    — generate C++
    - [vultlib.generateLua(files, options)]    — generate Lua
    - [vultlib.generateJava(files, options)]   — generate Java
    - [vultlib.generateJulia(files, options)]  — generate Julia
    - [vultlib.generatePython(files, options)] — generate Python

    See {!Vultlib} for details on input/output object formats. *)

(** Export the [vultlib] object. In a browser this is available as [globalThis.vultlib.*].
    When loaded via Node.js [require()], it is available as [module.exports.vultlib.*]. *)
let _ =
  Js.export "vultlib"
    (Js.Unsafe.obj
       [| ("main", Js.Unsafe.inject (Js.wrap_callback Vultlib.main))
        ; ("version", Js.Unsafe.inject Vultlib.version)
        ; ("generateJs", Js.Unsafe.inject (Js.wrap_callback (fun a b -> Vultlib.generateJs a b)))
        ; ("generateCpp", Js.Unsafe.inject (Js.wrap_callback (fun a b -> Vultlib.generateCpp a b)))
        ; ("generateLua", Js.Unsafe.inject (Js.wrap_callback (fun a b -> Vultlib.generateLua a b)))
        ; ("generateJava", Js.Unsafe.inject (Js.wrap_callback (fun a b -> Vultlib.generateJava a b)))
        ; ("generateJulia", Js.Unsafe.inject (Js.wrap_callback (fun a b -> Vultlib.generateJulia a b)))
        ; ("generatePython", Js.Unsafe.inject (Js.wrap_callback (fun a b -> Vultlib.generatePython a b)))
        ; ("getSemanticTokens", Js.Unsafe.inject (Js.wrap_callback Vultlib.getSemanticTokens))
        ; ("getDiagnostics", Js.Unsafe.inject (Js.wrap_callback Vultlib.getDiagnostics))
        ; ("getCompletions", Js.Unsafe.inject (Js.wrap_callback Vultlib.getCompletions))
        ; ("getDocumentSymbols", Js.Unsafe.inject (Js.wrap_callback Vultlib.getDocumentSymbols))
        ; ("getHoverInfo", Js.Unsafe.inject (Js.wrap_callback (fun a b c -> Vultlib.getHoverInfo a b c))) |] )
