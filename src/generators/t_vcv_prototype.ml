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
module Tags = Pparser.Ptags

(* Values of 'config' that the VCV Prototype host reads once, when the script loads.
   [frame_divider] divides the rate at which 'process' is called and [buffer_size] is the
   number of samples it receives per call. The right values depend on the script, so they
   are taken from the '@[vcv(...)]' tag of the 'process' function. *)
type host_settings = {frame_divider: int; buffer_size: int}

let default_host_settings : host_settings = {frame_divider= 1; buffer_size= 32}

type vcv_config =
  {module_name: string; process_fn: function_def option; update_fn: function_def option; host_settings: host_settings}

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

let positiveSetting (name : string) (loc : Util.Loc.t) (value : int) : int =
  if value < 1 then Util.Error.raiseError ("The VCV Prototype setting '" ^ name ^ "' must be greater than zero") loc
  else value

(* Reads '@[vcv(frameDivider = n, bufferSize = m)]'. Both arguments are optional. *)
let extractHostSettings (def : function_def) : host_settings =
  match Tags.getParameterList def.tags "vcv" [("frameDivider", Tags.TypeInt); ("bufferSize", Tags.TypeInt)] with
  | [frame_divider; buffer_size] ->
      { frame_divider=
          Tags.getIntValueOr ~default:default_host_settings.frame_divider frame_divider
          |> positiveSetting "frameDivider" def.loc
      ; buffer_size=
          Tags.getIntValueOr ~default:default_host_settings.buffer_size buffer_size
          |> positiveSetting "bufferSize" def.loc }
  | _ ->
      default_host_settings

let extractConfig (args : Util.Args.args) (stmts : top_stmt list) : vcv_config =
  let module_name = Common.moduleName args in
  let process_fn = ref None in
  let update_fn = ref None in
  CCList.iter
    (fun (s : top_stmt) ->
      match s.top with
      | TopFunction (def, _) ->
          if matchOriginalName ".process" def then process_fn := Some def
          else if matchOriginalName ".update" def then update_fn := Some def
      | _ ->
          () )
    stmts ;
  let host_settings = match !process_fn with Some def -> extractHostSettings def | None -> default_host_settings in
  {module_name; process_fn= !process_fn; update_fn= !update_fn; host_settings}

let validate (config : vcv_config) : unit =
  let () =
    match config.process_fn with
    | None ->
        Util.Error.raiseErrorMsg
          "The VCV Prototype script requires the functions:\nfun process(...) { }\nand update() { }"
    | Some def ->
        let n_inputs = CCList.length (nonCtxArgs def) in
        let n_outputs = outputCount def in
        if n_inputs > 6 then Util.Error.raiseErrorMsg "The 'process' function can have at most 6 inputs" ;
        if n_outputs > 6 then Util.Error.raiseErrorMsg "The 'process' function can have at most 6 outputs"
  in
  match config.update_fn with
  | None ->
      Util.Error.raiseErrorMsg
        "The VCV Prototype script requires the functions:\nfun process(...) { }\nand update() { }"
  | Some def ->
      let n_inputs = CCList.length (nonCtxArgs def) in
      let n_outputs = outputCount def in
      if n_inputs > 0 then Util.Error.raiseErrorMsg "The 'update' function should not take any input" ;
      if n_outputs > 0 then Util.Error.raiseErrorMsg "The 'update' function should not return any value"

let vcv_runtime : Pla.t =
  {%pla|
-- VCV Prototype functions
local global_block = {}

function stringAppend(s1, s2)
   return s1 .. s2
end

function getKnob(i)
   if i > 6 or i < 1 then
      return 0.0
   else
      return global_block.knobs[i]
   end
end

function getSwitch(i)
   if i > 6 or i < 1 then
      return false
   else
      return global_block.switches[i]
   end
end

function setLight(i, r, g, b)
   if not(i > 6 or i < 1) then
      global_block.lights[i][1] = r
      global_block.lights[i][2] = g
      global_block.lights[i][3] = b
   end
end

function setSwitchLight(i, r, g, b)
   if not(i > 6 or i < 1) then
      global_block.switchLights[i][1] = r
      global_block.switchLights[i][2] = g
      global_block.switchLights[i][3] = b
   end
end

function samplerate()
   return global_block.sampleRate
end

function sampletime()
   return global_block.sampleTime
end

|}

let hostSettingsCode ({frame_divider; buffer_size} : host_settings) : Pla.t =
  {%pla|config.frameDivider = <#frame_divider#i><#>config.bufferSize = <#buffer_size#i><#>|}

let generate (args : Util.Args.args) (stmts : top_stmt list) : Pla.t * Pla.t =
  let config = extractConfig args stmts in
  let () = validate config in
  let m = config.module_name in
  (* Generate process(block) function *)
  let process_def = Option.get config.process_fn in
  let update_def = Option.get config.update_fn in
  let has_process_ctx = hasCtx process_def in
  let has_update_ctx = hasCtx update_def in
  let process_inputs = nonCtxArgs process_def in
  let n_outputs = outputCount process_def in
  let _n_inputs = CCList.length process_inputs in
  (* Processor initialization *)
  let init_processor = if has_process_ctx then {%pla|local processor = <#m#s>_process_type_alloc()<#>|} else Pla.unit in
  (* Read inputs: block.inputs[index][i] / 10.0 *)
  let read_inputs =
    CCList.mapi
      (fun idx (p : param) ->
        let lua_idx = idx + 1 in
        let name = p.name in
        {%pla|      local <#name#s> = block.inputs[<#lua_idx#i>][i] / 10.0|} )
      process_inputs
    |> Pla.join_sep Pla.newline
  in
  (* Build process call arguments *)
  let process_call_args =
    let ctx_arg = if has_process_ctx then [Pla.string "processor"] else [] in
    let input_args = CCList.map (fun (p : param) -> Pla.string p.name) process_inputs in
    Pla.join_sep (Pla.string ", ") (ctx_arg @ input_args)
  in
  (* Build output bindings *)
  let output_bindings =
    if n_outputs = 0 then
      (* No outputs, just call *)
      {%pla|      <#m#s>_process(<#process_call_args#>)|}
    else if n_outputs = 1 then
      (* Single output *)
      {%pla|      block.outputs[1][i] = 10.0 * <#m#s>_process(<#process_call_args#>)|}
    else
      (* Multiple outputs: call, then read from context *)
      let call = {%pla|      <#m#s>_process(<#process_call_args#>)|} in
      let bindings =
        CCList.init n_outputs (fun idx ->
            let lua_idx = idx + 1 in
            {%pla|      block.outputs[<#lua_idx#i>][i] = 10.0 * processor.<#m#s>_process_ret_<#idx#i>|} )
        |> Pla.join_sep Pla.newline
      in
      {%pla|<#call#><#><#bindings#>|}
  in
  (* Build update call *)
  let update_call = if has_update_ctx then {%pla|<#m#s>_update(processor)|} else {%pla|<#m#s>_update()|} in
  let post =
    {%pla|
-- 'display' is provided by the host. This is only a fallback for running the
-- generated code outside VCV Rack.
if display == nil then
   function display(s)
      print(s)
   end
end

<#init_processor#>

function process(block)
   global_block = block
   <#update_call#>
   for i=1,block.bufferSize do
<#read_inputs#>
<#output_bindings#>
   end
end
|}
  in
  let host_settings = hostSettingsCode config.host_settings in
  let pre = {%pla|<#vcv_runtime#><#host_settings#>|} in
  (pre, post)
