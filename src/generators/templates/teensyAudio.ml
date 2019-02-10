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

(** Template for the Teensy Audio library *)
open Config

let inputsArray n_inputs =
   if n_inputs > 0 then
      {pla|audio_block_t *inputQueueArray[<#n_inputs#i>];|pla}, {pla|inputQueueArray|pla}
   else
      Pla.unit, {pla|NULL|pla}

(** Header function *)
let header (params:params) (code:Pla.t) : Pla.t =
   let file = String.uppercase params.output in
   let output = params.output in
   let module_name = params.module_name in
   let n_inputs = Config.countInputsNoCtx params.config.process_inputs in
   let input_queue_delc, input_queue_name = inputsArray n_inputs in
   {pla|
#ifndef <#file#s>_H
#define <#file#s>_H

#include <stdint.h>
#include <math.h>
#include "vultin.h"
#include "AudioStream.h"

<#code#>

class <#output#s> : public AudioStream
{
public:
  <#output#s>(void) : AudioStream(<#n_inputs#i>, <#input_queue_name#>)
  {
     <#module_name#s>_process_init(data);
  }

  void begin() {
    <#module_name#s>_default(data);
  }

  // Handles note on events
  void noteOn(int note, int velocity, int channel){
    // If the velocity is larger than zero, means that is turning on
    if(velocity) <#module_name#s>_noteOn(data, note, velocity, channel);
    else         <#module_name#s>_noteOff(data, note, channel);
  }

  // Handles note off events
  void noteOff(int note, int velocity, int channel) {
    <#module_name#s>_noteOff(data, note, channel);

  }

  // Handles control change events
  void controlChange(int control, int value, int channel) {
    <#module_name#s>_controlChange(data, control, value, channel);
  }

  virtual void update(void);

private:
  <#module_name#s>_process_type data;
  <#input_queue_delc#>

};

#endif // <#file#s>_H
|pla}


let rec allocateBlocks (block:int) (inputs:int) (outputs:int) =
   match inputs, outputs with
   | 0, 0 -> []
   | 0, _ ->
      let t = {pla|audio_block_t * block<#block#i> = allocate(); if(!block<#block#i>) return;|pla} in
      t :: (allocateBlocks (block + 1) inputs (outputs - 1))
   | _, 0 ->
      let t = {pla|audio_block_t * block<#block#i> = receiveReadOnly(<#block#i>); if(!block<#block#i>) return;|pla} in
      t :: (allocateBlocks (block + 1) (inputs - 1) outputs)
   | _, _ ->
      let t = {pla|audio_block_t * block<#block#i> = receiveWritable(<#block#i>); if(!block<#block#i>) return;|pla} in
      t :: (allocateBlocks (block + 1) (inputs - 1) (outputs - 1))

let transmitBlocks (outputs:int) =
   CCList.init outputs (fun i -> {pla|transmit(block<#i#i>, <#i#i>);|pla})
   |> Pla.join_sep Pla.newline

let releaseBlocks (blocks:int) =
   CCList.init blocks (fun i -> {pla|release(block<#i#i>);|pla})
   |> Pla.join_sep Pla.newline

let castInput params typ i acc =
   match typ with
   | IReal _ when params.real = "fixed" ->
      i + 1, {pla|fix16_t in<#i#i> = short_to_fix(block<#i#i>->data[i]);|pla} :: acc
   | IReal _ ->
      i + 1, {pla|float in<#i#i> = short_to_float(block<#i#i>->data[i]);|pla} :: acc
   | IBool _ ->
      i + 1, {pla|uint8_t in<#i#i> = block<#i#i>->data[i] != 0;|pla} :: acc
   | IInt _ ->
      i + 1, {pla|int in<#i#i> = block<#i#i>->data[i];|pla} :: acc
   | IContext -> i,acc

let castOutput params typ value =
   match typ with
   | OReal when params.real = "fixed" ->
      {pla|fix_to_short(<#value#>)|pla}
   | OReal ->
      {pla|<#value#>|pla}
   | OFix16  ->
      {pla|fix_to_short(<#value#>)|pla}
   | OBool ->
      {pla|<#value#>|pla}
   | OInt ->
      {pla|<#value#>|pla}

let declareInputs params =
   List.fold_left
      (fun (i,acc) a -> castInput params a i acc) (0,[])
      params.config.process_inputs
   |> snd
   |> Pla.join_sep Pla.newline
   |> Pla.indent |> Pla.indent

let declReturn params =
   let output_pla a = Pla.string (Config.outputTypeString a) in
   let underscore = Pla.string "_" in
   match params.config.process_outputs with
   | []  -> Pla.unit, Pla.unit
   | [o] ->
      let current_typ = Replacements.getType params.repl (Config.outputTypeString o) in
      let decl = {pla|<#current_typ#s> out;|pla} in
      let value = castOutput params o (Pla.string "out") in
      let copy = {pla|block0->data[i] = <#value#>; |pla} in
      decl, copy
   | o ->
      let decl = Pla.(string "_tuple___" ++ map_sep underscore output_pla o ++ string "__ out; ") in
      let copy =
         List.mapi
            (fun i o ->
                let value = castOutput params o {pla|ret.field_<#i#i>|pla} in
                {pla|block<#i#i>->data[i] = <#value#>; |pla}) o
         |> Pla.join_sep_all Pla.newline
         |> Pla.indent
      in
      decl, copy

(** Implementation function *)
let implementation (params:params) (code:Pla.t) : Pla.t =
   let output      = params.output in
   let module_name = params.module_name in
   let n_inputs    = Config.countInputsNoCtx params.config.process_inputs in
   let n_outputs   = Config.countOutputs params.config.process_outputs in
   let allocate_blocks = allocateBlocks 0 n_inputs n_outputs |> Pla.join_sep Pla.newline in
   let transmit_blocks = transmitBlocks n_outputs in
   let release_blocks  = releaseBlocks (max n_inputs n_outputs) in
   let inputs = declareInputs params in
   let decl_out, copy_out = declReturn params in
   {pla|
#include "<#output#s>.h"

<#code#>

void <#output#s>::update(void)
{

<#allocate_blocks#+>

   for(int i = 0; i < AUDIO_BLOCK_SAMPLES; i++) { <#inputs#>
      <#decl_out#>
      out = <#module_name#s>_process(data);
<#copy_out#>
   }

<#transmit_blocks#+>
<#release_blocks#+>
}

|pla}

let get (params:params) (header_code:Pla.t) (impl_code:Pla.t) : (Pla.t * FileKind.t) list =
   [
      header params header_code, FileKind.ExtOnly "h";
      implementation params impl_code, FileKind.ExtOnly "cpp"
   ]
