+++
date = "2016-07-01"
description = ""
tags = []
title = "Common patterns"
topics = []
layout = "tutorial"

+++

Generally, when working with the Vult language, there isn't a strict rule on how to integrate the generated code into your project. However, there are some patterns that I've found to be particularly useful, depending on the nature of your project. Let me walk you through a few of these strategies.

*NOTE: I have not compiled all the code shown here, so there may be typos*

## Audio plugins

When crafting an audio plugin, there's typically a primary processing function responsible for rendering the audio. The frequency of its invocation depends on the framework, occurring either per sample or for an entire block (comprising many samples). This function handles audio data, and requires high sample rates, often exceeding 44100 samples per second.

In contrast, adjustments to knobs or parameters in your graphical user interface (GUI) can be processed at a lower sample rate. When a parameter changes, it might be necessary to trigger a set of calculations that don't require repetition with every audio sample. Take, for instance, a filter where coefficients can be recalculated at a lower sample rate, either when a parameter changes or at the completion of each audio block.

A good way of organizing your Vult code for this use case is the following:


<div class="vult_code" id="example1">// Main audio processor.
// The caculations that nned to be performed every audio sample go here.
fun processor(audio_in) {
    mem gain; // mem variables can be used to store parameters
    return gain * audio_in;
}
// Depending on the number of parameters, the values can be passed as
// follows or with multiple functions. e.g. setParam1(), setParam2()
and setParameters(gain_in) {
    // the calculation of the gain could involve a more complex calculation
    gain = gain_in * gain_in;
}
</div>

On the C++ side, the code would look something like this:

<div class="c_code" id="example-2">Processor_process_type processor;
//
void setup() {
    Processor_process_init(processor);
}
//
int main(void) {
    float input_block[32];
    float output_block[32];
    // set the parameters before processing a block.
    Processor_setParameters(processor, 0.5f);
    // process a block of audio
    for(int i = 0; i < 32; i++)
        output_block[i] = Processor_process(processor, input_block[i]);
}
</div>

For a more complete example, you can take a look at the template for VCV Rack plugins in the following repository https://github.com/vult-dsp/RackPlayground

## Polyphonic processor

At present, the Vult language lacks a built-in mechanism for crafting polyphonic instruments. Nonetheless, it's straightforward to achieve by taking the generated code and creating multiple instances. In VCV Rack, I employ the following pattern.

<div class="c_code" id="example-3">static const int N_VOICES = 16;
// Declare an array of processors
Processor_process_type processors[N_VOICES];
//
void setup() {
    // Initialize all the voices
    for(int i = 0; i < N_VOICES; i++)
        Processor_process_init(processor[i]);
}
//
int main(void) {
    float input_block[32];
    float output_block[32];
    // set the parameters for all voices
    for(int i = 0; i < N_VOICES; i++)
        Processor_setParameters(processor[i], 0.5f);
    // process all the voices
    for(int voice = 0; voice < N_VOICES; voice++)
        for(int i = 0; i < 32; i++)
            output_block[i] += Processor_process(processor, input_block[i]);
}
</div>

Some optimizations can enhance performance; for instance, deactivating voices. If the call to `Processor_setParameters` is resource-intensive (perhaps involving coefficient computation) it's feasible to copy the result from one instance to others, as illustrated in the following code:"

<div class="c_code" id="example-4">// set the parameters for the first voice
Processor_setParameters(processor[0], 0.5f);
// copy the relevant data to the other voices
for(int i = 1; i < N_VOICES; i++) {
    processor[i].gain = processor[0].gain;
}
</div>

## Vult in microcontrollers

I leverage the Vult language to write code for my non-analog Eurorack modules, which you can explore at https://www.vult-dsp.com/hardware

Essentially, all my projects with microcontrollers incorporate some underlying Vult code. The way of integrating it differs sligthly depending on the platform.

### Teensy Audio Library
For some platforms the Vult language has a very convenient way of integrating the code in the form of templates. One of such templates is for the Teensy Audio library. You can find an example on how to use it here: https://github.com/modlfo/teensy-vult-example

### Arduino

When dealing with Arduino (and compatible) boards that don't involve audio processing, my preferred approach is to expose certain common Arduino functions to the Vult side through 'external' functions. Here's an example illustrating how to read buttons and knobs while generating analog output.

<div class="vult_code" id="example-5">// file: Test.vult
external digitalRead(pin:int) : bool "stub_digitalRead";
external analogRead(pin:int) : int "stub_analogRead";
external analogWrite(pin:int, value:int) : unit "stub_analogWrite";
//
// Button debouncer using fixed point computations
fun debounce(input:bool) {
   mem x;
   x = x + ((if input then 1.0x else 0.0x) - x) * 0.1x;
   return x > 0.7x;
}
// Edge detector
fun edge(x){
   mem pre;
   val ret = pre == false && x == true;
   pre = x;
   return ret;
}
// toggle button
fun toggle(input:bool) {
   mem state;
   if(edge(input)) {
      state = not(state);
   }
   return state;
}
//
fun process() {
    // read the button and avoid bouncing
    val button = debounce(digitalRead(0));
    // control a toggle
    val on_off = toggle(button);
    // read analog sognals
    val signal1 = analogRead(0);
    val signal2 = analogRead(0);
    // select one output based on the toggle
    val out = if on_off then signal1 else signal2;
    // write the output
    analogWrite(5, out);
}
</div>

In this example, the external function calls like `digitalRead` are replaced by the stub functions `stub_digitalRead`. In order for this to compile nicely, we need to provide such functions. Since Arduino compiles C++ code, we just need to put somewhere in the sketch the definitions. Then we can simply declare, initialize and call the Vult code in the sketch.

<div class="c_code" id="example-6">// file: main.ino
uint8_t stub_digitalRead(int pin) {
    return digitalRead(pin);
}
int stub_analogRead(int pin) {
    return analogRead(pin);
}
void stub_analogWrite(int pin, int value) {
    analogWrite(pin, value);
}
// Declare the processor
Test_process_type processor;
//
void setup() {
    Test_process_init(processor);
}
//
void loop() {
    Test_process(processor);
}
</div>

Alternatively, you can get data out of your `processor` by using functions or accessing the fields directly.

Some of the applications I work on involve audio processing. In such cases, the code closely resembles the 'Audio Plugin' pattern, where the task involves rendering a buffer of samples. The specifics vary from platform to platform. But usually you have something like this on the C/C++ side.


<div class="c_code" id="example-7">// file: main.cpp
Test_process_type processor;
//
void setup() {
    Test_process_init(processor);
}
// Interrupt handler called after a a block of audio
// has been transfered
void transferComplete() {
    // Read buttons, and knobs
    .......
    // Set parameters to the processor
    Test_setParameters(processor, ...);
    // Render a new block of audio
    Test_process(processor, ...);
}
//
void main() {
    // Initialize the system
    setup();
    // Prepare the audio CODEC and enable interruptions
    audioSetup();
    // Loop forever
    while(1) {
//
    };
}

</div>


<script type="text/javascript" src="../../javascripts/external/ace/ace.js"></script>
<script type="text/javascript" src="../../javascripts/main.js"></script>

