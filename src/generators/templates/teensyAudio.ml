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

(** Implementation function *)
let implementation (params:params) (code:Pla.t) : Pla.t =
   let output      = params.output in
   let module_name = params.module_name in
   let n_inputs    = Config.countInputsNoCtx params.config.process_inputs in
   let n_outputs   = Config.countOutputs params.config.process_outputs in
   let allocate_blocks = allocateBlocks 0 n_inputs n_outputs |> Pla.join_sep Pla.newline in
   let transmit_blocks = transmitBlocks n_outputs in
   let release_blocks  = releaseBlocks (max n_inputs n_outputs) in
   {pla|
#include "<#output#s>.h"

<#code#>

void <#output#s>::update(void)
{

<#allocate_blocks#+>

   for(int i = 0; i < AUDIO_BLOCK_SAMPLES; i++) {
      fix16_t v = <#module_name#s>_process(data, 0);
      *bp++ = (int16_t)(v / 2);
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
