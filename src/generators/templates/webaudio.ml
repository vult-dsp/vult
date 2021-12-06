open Config

let inputString (m : input) : Pla.t =
   match m with
   | IContext -> Pla.string "processor.context"
   | IReal name
   |IInt name
   |IBool name ->
      Pla.string name


let inputName (i, acc) s =
   match s with
   | IContext -> i, Pla.string "processor.context" :: acc
   | _ -> i + 1, [%pla {|in_<#i#i>[n]|}] :: acc


let performFunctionCall module_name (config : config) =
   (* generates the aguments for the process call *)
   let args = List.fold_left inputName (0, []) config.process_inputs |> snd |> List.rev |> Pla.join_sep Pla.comma in
   (* declares the return variable and copies the values to the output buffers *)
   let copy =
      match config.process_outputs with
      | [] -> Pla.unit
      | [ _ ] ->
         let value = Pla.string "ret" in
         [%pla {|out_0[n] = <#value#>; |}]
      | o ->
         List.mapi
            (fun i _ ->
                let value = [%pla {|<#module_name#s>_process_ret_<#i#i>(processor.context)|}] in
                [%pla {|out_<#i#i>[n] = <#value#>; |}] )
            o
         |> Pla.join_sep_all Pla.newline
   in
   [%pla
      {|for (var n = 0; n < e.inputBuffer.length; n++) {
          var ret = processor.<#module_name#s>_process(<#args#>); <#><#copy#> <#>}|}]


let noteFunctions (params : params) =
   let module_name = params.module_name in
   let on_args = Pla.map_sep Pla.comma inputString params.config.noteon_inputs in
   let off_args = Pla.map_sep Pla.comma inputString params.config.noteoff_inputs in
   ( [%pla
      {|
   node.noteOn = function(note, velocity, channel){
      if(velocity > 0) processor.<#module_name#s>_noteOn(<#on_args#>);
      else processor.<#module_name#s>_noteOff(<#off_args#>);
   }|}]
   , [%pla {|
   node.noteOff = function(note, channel) {
      processor.<#module_name#s>_noteOff(<#off_args#>);
   }|}]
   )


let controlChangeFunction (params : params) =
   let module_name = params.module_name in
   let ctrl_args = Pla.map_sep Pla.comma inputString params.config.controlchange_inputs in
   [%pla
      {|
   node.controlChange = function(control, value, channel) {
      processor.<#module_name#s>_controlChange(<#ctrl_args#>);
   }|}]


let rec removeContext inputs =
   match inputs with
   | IContext :: t -> removeContext t
   | _ -> inputs


let get (params : params) runtime code : (Pla.t * FileKind.t) list =
   let config = params.config in
   let module_name = params.module_name in
   let inputs = removeContext config.process_inputs in
   let nprocess_inputs = List.length inputs in
   let nprocess_outputs = List.length config.process_outputs in
   let input_var =
      List.mapi (fun i _ -> [%pla {|var in_<#i#i> = e.inputBuffer.getChannelData(<#i#i>); |}]) inputs
      |> Pla.join_sep Pla.newline
   in
   let output_var =
      List.mapi (fun i _ -> [%pla {|var out_<#i#i> = e.outputBuffer.getChannelData(<#i#i>); |}]) config.process_outputs
      |> Pla.join_sep Pla.newline
   in
   let process_call = performFunctionCall params.module_name params.config in
   let note_on, note_off = noteFunctions params in
   let control_change = controlChangeFunction params in
   let text =
      [%pla
         {|
(function(audioContext) {
   var code = function () {
      <#runtime#>
      <#code#>
      this.context = this.<#module_name#s>_process_init();
      };
   var processor = new code ();
   processor.<#module_name#s>_default(processor.context);
   var node = audioContext.createScriptProcessor(0, <#nprocess_inputs#i>, <#nprocess_outputs#i>);
   node.inputs = <#nprocess_inputs#i>;
   node.outputs = <#nprocess_outputs#i>;
   node.onaudioprocess = function (e) {
<#input_var#+>
<#output_var#+>
<#process_call#+>
   }
<#note_on#>
<#note_off#>
<#control_change#>
   return node;
   })
|}]
   in
   [ text, FileKind.ExtOnly "js" ]
