+++
date = "2017-06-20T08:40:42+03:00"
title = "Compile-time embedding of WAV files"
description = ""
tags = []
topics = []
layout = "post"
author = "Leonardo Laguna Ruiz"
+++

Around the 2000's I got my first VST plugin; it was a PPG Wave. The sounds produced by this plugin got me intrigued because it sounded very different from other soft synths that I have heard. The main difference was that the PPG was based on wavetables. Since I started working on Vult, making a wavetable synthesizer has been in the list.

<!--more-->


To make a wavetable synthesizer you need waveforms. One of the things I considered was to extract the waves from the PPG ROM, but in the process of trying that I found the work of [Adventure Kid](https://www.adventurekid.se) who has created a large database of waveforms and impulse response files [AKRT](https://www.adventurekid.se/AKRT/). He provides the wavetables and IR as WAV files.

In order to use wavetables in Vult (as the ones provided by Adventure Kid) it's necessary to load them in some way. Since Vult is all about the code, the easiest way is to include the wavetables in the code itself. To that purpose we have now a new tag `@[wave]` that allows to specify a WAV file. The function marked with the tag is replaced at compile-time with a function that returns the data of the file. For example:

<div class="vult_code" id="snipet-1">
external mywave(channel:int, index:int) : real
   @[wave(channels=1, file="wave.wav")];
</div>

The code above declares an external function called `mywave`. The function must take as arguments two integers: the channel and the sample index. The tag `@[wave(channels=1, file="wave.wav")]` specifies that the WAV file has one channel and that the file name is `wave.wav`.

While running, the Vult compiler will look for the file `wave.wav` in the same location of the current file or in the include directories passed with the flag `-i`. To access the data we can call the function providing the channel we want to read and the index of the sample as follows:

<div class="vult_code" id="snipet-2">
fun fun() {
   val channel = 0; // first channel
   val sample = 0;  // first sample
   return mywave(channel, sample);
}
</div>

If we try to read a channel that does not exists the function will return zero. If we pass an index outside the range of the wav file, the mod (%) operation is used to fix the index. This will make it behave as a circular buffer.

The tag also generates a new function with the name `<name>_samples()` that returns the number of samples in the WAV file. In the case above, the function will be called `mywave_samples()`. If another function with the same name exists we will get a compilation error.

As a complete example, the following code will play back in a loop the embedded wave file.

<div class="vult_code" id="snipet-3">
external mywave(channel:int, index:int) : real
   @[wave(channels=1, file="wave.wav")];

// count from 0 to the number of samples in file "wave.wav" minus one
fun index() {
   // use mywave_samples() to reset the counter to zero
   mem i = (i + 1) % mywave_samples();
   return i;
}

fun play() {
   return mywave(0, index());
}
</div>

Currently, only PCM WAV files in 16 and 24 bits are supported.

Embedding of WAV files is intended to handle short WAV files (a few seconds or less). Embedding very large files slows down the code generation and I might cause both, the Vult compiler and target compiler to fail. For such cases is recommended to provide an external function that reads at run-time the file.




<script type="text/javascript" src="../../javascripts/external/ace/ace.js"></script>
<script type="text/javascript" src="../../javascripts/main.js"></script>

