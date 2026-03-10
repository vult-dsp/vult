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

type js_config =
  { module_name: string
  ; process_fn: function_def option
  ; note_on_fn: function_def option
  ; note_off_fn: function_def option
  ; control_change_fn: function_def option
  ; default_fn: function_def option }

let getModuleName (args : Util.Args.args) : string =
  match args.files with Util.Args.File s :: _ -> Pparser.Parse.moduleName s | _ -> "Top"

let matchOriginalName (suffix : string) (def : function_def) : bool =
  match def.info.original_name with
  | Some name ->
      let len = String.length name in
      let slen = String.length suffix in
      len >= slen && String.equal (String.sub name (len - slen) slen) suffix
  | None ->
      false

let hasCtx (def : function_def) : bool =
  match def.args with {name= "_ctx"; t= {t= TStruct _; _}; _} :: _ -> true | _ -> false

let nonCtxArgs (def : function_def) : param list =
  match def.args with {name= "_ctx"; t= {t= TStruct _; _}; _} :: rest -> rest | args -> args

let outputCount (def : function_def) : int =
  match def.t with
  | _, {t= TTuple elems; _} ->
      CCList.length elems
  | _, {t= TVoid (Some elems); _} ->
      CCList.length elems
  | _, {t= TVoid None; _} ->
      0
  | _, _ ->
      1

let extractConfig (args : Util.Args.args) (stmts : top_stmt list) : js_config =
  let module_name = getModuleName args in
  let process_fn = ref None in
  let note_on_fn = ref None in
  let note_off_fn = ref None in
  let control_change_fn = ref None in
  let default_fn = ref None in
  CCList.iter
    (fun (s : top_stmt) ->
      match s.top with
      | TopFunction (def, _) ->
          if matchOriginalName ".process" def then process_fn := Some def
          else if matchOriginalName ".noteOn" def then note_on_fn := Some def
          else if matchOriginalName ".noteOff" def then note_off_fn := Some def
          else if matchOriginalName ".controlChange" def then control_change_fn := Some def
          else if matchOriginalName ".default" def then default_fn := Some def
      | _ ->
          () )
    stmts ;
  { module_name
  ; process_fn= !process_fn
  ; note_on_fn= !note_on_fn
  ; note_off_fn= !note_off_fn
  ; control_change_fn= !control_change_fn
  ; default_fn= !default_fn }

let browserPost (config : js_config) : Pla.t =
  let m = config.module_name in
  let has_process_ctx = match config.process_fn with Some def -> hasCtx def | None -> false in
  let init_ctx =
    if has_process_ctx then
      {%pla|if(this.<#m#s>_process_type_alloc)  this.context =  this.<#m#s>_process_type_alloc(); else this.context = {};|}
    else {%pla|this.context = {};|}
  in
  let call_default =
    match config.default_fn with
    | Some def when hasCtx def ->
        {%pla|<#>   if(this.<#m#s>_default)      this.<#m#s>_default(this.context);|}
    | Some _ ->
        {%pla|<#>   if(this.<#m#s>_default)      this.<#m#s>_default();|}
    | None ->
        Pla.unit
  in
  let live_note_on =
    {%pla|this.liveNoteOn        = function(note,velocity,channel) { if(this.<#m#s>_noteOn)        this.<#m#s>_noteOn(this.context,note,velocity,channel); };|}
  in
  let live_note_off =
    {%pla|this.liveNoteOff       = function(note,velocity,channel) { if(this.<#m#s>_noteOff)       this.<#m#s>_noteOff(this.context,note,velocity,channel); };|}
  in
  let live_control_change =
    {%pla|this.liveControlChange = function(note,velocity,channel) { if(this.<#m#s>_controlChange) this.<#m#s>_controlChange(this.context,note,velocity,channel); };|}
  in
  let live_process =
    {%pla|this.liveProcess       = function(input)         { if(this.<#m#s>_process)       return this.<#m#s>_process(this.context,input); else return 0; };|}
  in
  let live_default =
    {%pla|this.liveDefault       = function() { if(this.<#m#s>_default)      return this.<#m#s>_default(this.context); };|}
  in
  {%pla|
   <#init_ctx#><#call_default#>
   <#live_note_on#>
   <#live_note_off#>
   <#live_control_change#>
   <#live_process#>
   <#live_default#>
   }|}

let generateBrowser (args : Util.Args.args) (stmts : top_stmt list) : Pla.t * Pla.t =
  let config = extractConfig args stmts in
  let pre = {%pla|function vultProcess() {<#>|} in
  let post = browserPost config in
  (pre, post)

let generateNode (args : Util.Args.args) (stmts : top_stmt list) : Pla.t * Pla.t =
  let config = extractConfig args stmts in
  let pre = {%pla|exports.vultProcess = function() {<#>|} in
  let post = browserPost config in
  (pre, post)
