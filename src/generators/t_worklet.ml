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

let messageHandler (config : T_browser.js_config) : Pla.t =
  let m = config.module_name in
  let note_on =
    match config.note_on_fn with
    | Some def when T_browser.hasCtx def ->
        {%pla|if (e.data.type === 'noteOn') this.vult.<#m#s>_noteOn(this.vult.context, e.data.note, e.data.velocity, e.data.channel);|}
    | Some _ ->
        {%pla|if (e.data.type === 'noteOn') this.vult.<#m#s>_noteOn(e.data.note, e.data.velocity, e.data.channel);|}
    | None ->
        Pla.unit
  in
  let note_off =
    match config.note_off_fn with
    | Some def when T_browser.hasCtx def ->
        {%pla|if (e.data.type === 'noteOff') this.vult.<#m#s>_noteOff(this.vult.context, e.data.note, e.data.channel);|}
    | Some _ ->
        {%pla|if (e.data.type === 'noteOff') this.vult.<#m#s>_noteOff(e.data.note, e.data.channel);|}
    | None ->
        Pla.unit
  in
  let control_change =
    match config.control_change_fn with
    | Some def when T_browser.hasCtx def ->
        {%pla|if (e.data.type === 'controlChange') this.vult.<#m#s>_controlChange(this.vult.context, e.data.control, e.data.value, e.data.channel);|}
    | Some _ ->
        {%pla|if (e.data.type === 'controlChange') this.vult.<#m#s>_controlChange(e.data.control, e.data.value, e.data.channel);|}
    | None ->
        Pla.unit
  in
  let has_handlers = config.note_on_fn <> None || config.note_off_fn <> None || config.control_change_fn <> None in
  if has_handlers then
    {%pla|this.port.onmessage = (e) => {
         <#note_on#>
         <#note_off#>
         <#control_change#>
      };|}
  else Pla.unit

let processBody (config : T_browser.js_config) : Pla.t =
  let m = config.module_name in
  match config.process_fn with
  | None ->
      Pla.unit
  | Some def ->
      let inputs = T_browser.nonCtxArgs def in
      let n_inputs = CCList.length inputs in
      let n_outputs = T_browser.outputCount def in
      let has_ctx = T_browser.hasCtx def in
      let input_args = CCList.mapi (fun i (_p : param) -> {%pla|input[<#i#i>] ? input[<#i#i>][i] : 0|}) inputs in
      let input_args = if n_inputs = 0 then [{%pla|input[0] ? input[0][i] : 0|}] else input_args in
      let all_args = if has_ctx then Pla.string "this.vult.context" :: input_args else input_args in
      let args = Pla.join_sep Pla.commaspace all_args in
      if n_outputs <= 1 then
        {%pla|const input = inputs[0];
      const output = outputs[0];
      for (var i = 0; i < 128; i++) {
         output[0][i] = this.vult.<#m#s>_process(<#args#>);
      }|}
      else
        let copies =
          CCList.init n_outputs (fun i ->
              {%pla|output[<#i#i>][i] = this.vult.<#m#s>_process_ret_<#i#i>(this.vult.context);|} )
          |> Pla.join_sep Pla.newline
        in
        {%pla|const input = inputs[0];
      const output = outputs[0];
      for (var i = 0; i < 128; i++) {
         this.vult.<#m#s>_process(<#args#>);
         <#copies#>
      }|}

let generate (args : Util.Args.args) (stmts : top_stmt list) : Pla.t * Pla.t =
  let config = T_browser.extractConfig args stmts in
  let m = config.module_name in
  let has_ctx = match config.process_fn with Some def -> T_browser.hasCtx def | None -> false in
  let init_context =
    if has_ctx then {%pla|this.context = this.<#m#s>_process_type_alloc();|} else {%pla|this.context = {};|}
  in
  let call_default =
    match config.default_fn with
    | Some def when T_browser.hasCtx def ->
        {%pla|<#>      this.<#m#s>_default(this.context);|}
    | Some _ ->
        {%pla|<#>      this.<#m#s>_default();|}
    | None ->
        Pla.unit
  in
  let msg_handler = messageHandler config in
  let process_body = processBody config in
  let pre =
    {%pla|class VultProcessor extends AudioWorkletProcessor {
   constructor() {
      super();
      this.vult = new (function() {
|}
  in
  let post =
    {%pla|
      <#init_context#><#call_default#>
      })();
      <#msg_handler#>
   }
   process(inputs, outputs, parameters) {
      <#process_body#>
      return true;
   }
}
registerProcessor('vult-processor', VultProcessor);|}
  in
  (pre, post)
