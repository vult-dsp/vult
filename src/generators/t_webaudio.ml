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

let inputBufferLines (n_inputs : int) : Pla.t =
  CCList.init n_inputs (fun i -> {%pla|var in_<#i#i> = e.inputBuffer.getChannelData(<#i#i>); |})
  |> Pla.join_sep Pla.newline

let outputBufferLines (n_outputs : int) : Pla.t =
  CCList.init n_outputs (fun i -> {%pla|var out_<#i#i> = e.outputBuffer.getChannelData(<#i#i>); |})
  |> Pla.join_sep Pla.newline

let processCallAndCopy (config : T_browser.js_config) : Pla.t =
  let m = config.module_name in
  match config.process_fn with
  | None ->
      Pla.unit
  | Some def ->
      let has_ctx = T_browser.hasCtx def in
      let inputs = T_browser.nonCtxArgs def in
      let n_outputs = T_browser.outputCount def in
      let input_args = CCList.mapi (fun i (_p : param) -> {%pla|in_<#i#i>[n]|}) inputs in
      let all_args = if has_ctx then Pla.string "processor.context" :: input_args else input_args in
      let args = Pla.join_sep Pla.commaspace all_args in
      if n_outputs <= 1 then {%pla|         var ret = processor.<#m#s>_process(<#args#>); <#>out_0[n] = ret;  |}
      else
        let copy =
          CCList.init n_outputs (fun i -> {%pla|out_<#i#i>[n] = <#m#s>_process_ret_<#i#i>(processor.context); |})
          |> Pla.join_sep Pla.newline
        in
        {%pla|         var ret = processor.<#m#s>_process(<#args#>); <#><#copy#> |}

let generate (args : Util.Args.args) (stmts : top_stmt list) : Pla.t * Pla.t =
  let config = T_browser.extractConfig args stmts in
  let m = config.module_name in
  let n_inputs = match config.process_fn with Some def -> CCList.length (T_browser.nonCtxArgs def) | None -> 1 in
  let n_outputs = match config.process_fn with Some def -> T_browser.outputCount def | None -> 1 in
  let n_inputs = max n_inputs 1 in
  let n_outputs = max n_outputs 1 in
  let has_ctx = match config.process_fn with Some def -> T_browser.hasCtx def | None -> false in
  let init_context =
    if has_ctx then {%pla|this.context = this.<#m#s>_process_type_alloc();|} else {%pla|this.context = {};|}
  in
  let call_default =
    match config.default_fn with
    | Some def when T_browser.hasCtx def ->
        {%pla|processor.<#m#s>_default(processor.context);|}
    | Some _ ->
        {%pla|processor.<#m#s>_default();|}
    | None ->
        Pla.unit
  in
  let in_buffers = inputBufferLines n_inputs in
  let out_buffers = outputBufferLines n_outputs in
  let process_call = processCallAndCopy config in
  let note_on =
    match config.note_on_fn with
    | Some def when T_browser.hasCtx def ->
        {%pla|<#>
   node.noteOn = function(note, velocity, channel){
      if(velocity > 0) processor.<#m#s>_noteOn(processor.context,note,velocity,channel);
      else processor.<#m#s>_noteOff(processor.context,note,channel);
   }|}
    | Some _ ->
        {%pla|<#>
   node.noteOn = function(note, velocity, channel){
      if(velocity > 0) processor.<#m#s>_noteOn(note,velocity,channel);
      else processor.<#m#s>_noteOff(note,channel);
   }|}
    | None ->
        Pla.unit
  in
  let note_off =
    match config.note_off_fn with
    | Some def when T_browser.hasCtx def ->
        {%pla|<#>
   node.noteOff = function(note, channel) {
      processor.<#m#s>_noteOff(processor.context,note,channel);
   }|}
    | Some _ ->
        {%pla|<#>
   node.noteOff = function(note, channel) {
      processor.<#m#s>_noteOff(note,channel);
   }|}
    | None ->
        Pla.unit
  in
  let control_change =
    match config.control_change_fn with
    | Some def when T_browser.hasCtx def ->
        {%pla|<#>
   node.controlChange = function(control, value, channel) {
      processor.<#m#s>_controlChange(processor.context,control,value,channel);
   }|}
    | Some _ ->
        {%pla|<#>
   node.controlChange = function(control, value, channel) {
      processor.<#m#s>_controlChange(control,value,channel);
   }|}
    | None ->
        Pla.unit
  in
  let pre = {%pla|
(function(audioContext) {
   var code = function () {
   |} in
  let post =
    {%pla|
      <#init_context#>
      };
   var processor = new code ();
   <#call_default#>
   var node = audioContext.createScriptProcessor(0, <#n_inputs#i>, <#n_outputs#i>);
   node.inputs = <#n_inputs#i>;
   node.outputs = <#n_outputs#i>;
   node.onaudioprocess = function (e) {
<#>
   <#in_buffers#>
<#>
   <#out_buffers#>
<#>
   for (var n = 0; n < e.inputBuffer.length; n++) {
   <#process_call#>
   }
   }
<#note_on#>
<#note_off#>
<#control_change#>
   return node;
   })|}
  in
  (pre, post)
