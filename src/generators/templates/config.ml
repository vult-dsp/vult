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

type input =
   | IContext
   | IReal of string
   | IInt of string
   | IBool of string

type output =
   | OFix16
   | OReal
   | OInt
   | OBool

(** Represents the 'plugin' configuration *)
type config =
   { module_name : string
   ; process_inputs : input list
   ; process_outputs : output list
   ; process_found : bool
   ; noteon_inputs : input list
   ; noteoff_inputs : input list
   ; controlchange_inputs : input list
   ; default_inputs : input list
   ; update_inputs : input list
   ; update_outputs : output list
   ; update_found : bool
   }

type target_file =
   | Implementation
   | Header
   | Tables

(** Represents the parameters used during code generation *)
type params =
   { real : string (** 'Real' number representation *)
   ; template : string (** Used template *)
   ; target_file : target_file (** Defines which part of the code we are generating *)
   ; output : string (** Argument given via '-o' *)
   ; repl : Replacements.t (** Replacements used during Vult -> Code conversion *)
   ; module_name : string (** Name of the main mudule *)
   ; prefix : string (** Prefix given to java code *)
   ; config : config (** Plugin configuration *)
   }

(** Empty default configuration *)
let empty_conf module_name =
   { module_name
   ; process_inputs = []
   ; process_outputs = []
   ; process_found = false
   ; noteon_inputs = []
   ; noteoff_inputs = []
   ; controlchange_inputs = []
   ; default_inputs = []
   ; update_inputs = []
   ; update_outputs = []
   ; update_found = false
   }
;;

let inputTypeString (m : input) : string =
   match m with
   | IContext -> "context"
   | IReal _ -> "real"
   | IInt _ -> "int"
   | IBool _ -> "bool"
;;

let outputTypeString (m : output) : string =
   match m with
   | OReal -> "real"
   | OInt -> "int"
   | OBool -> "bool"
   | OFix16 -> "fix16"
;;

let countInputsNoCtx (inputs : input list) : int =
   List.fold_left
      (fun s a ->
          match a with
          | IContext -> s
          | _ -> s + 1)
      0
      inputs
;;

let countOutputs (outputs : output list) : int = List.length outputs
