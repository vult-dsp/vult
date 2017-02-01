+++
date = "2017-02-01T22:04:23+01:00"
title = "Compile-time creation of tables"
description = ""
tags = []
topics = []
layout = "post"
+++

<script type="text/javascript"
   src="https://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML">
</script>

This is one of the features that has been in the TODO list since the creation of Vult. When generating code, now is possible to annotate a function and Vult will automatically create a table-based implementation which will be faster. This is specially useful when generating code for microcontrollers.

As an example, take the function used to convert from pitch (MIDI note number) to frequency.

<div>$$f = 440 \cdot 2^{ \frac {d - 69} {12}} $$</div>

Using that formula, we can calculate the `rate` of increment for a saw wave at a certain sampling frequency. For example, to produce a saw wave with pitch `d` at a sampling rate of 44100 Hz, you need to increment a counter at a `rate` to reach a value of `1.0`. Here's the formula:

<div>$$rate = \frac {440 \cdot 2^{ \frac {d - 69} {12}}} {44100} $$</div>

This formula can be simplified (using Mathematica) to:

<div>$$rate = 0.00018539226566085504 \cdot e^{0.057762265046662105 \cdot d} $$</div>

We can see that the formula uses an exponential. Calculating an exponential every sample can be quite heavy in a microntroller (for example when making modulations). To alleviate that problem, we can annotate the function in Vult as follows:

<div class="vult_code" id="snipet-1">fun pitchToRate(d) @[table(size=127,min=0.0,max=127.0)] {
   return 0.00018539226566085504 * exp(0.057762265046662105 * d);
}
</div>

The annotation `@[table(size=128,min=0.0,max=127.0)]` specifies that the function needs to be converted to a table of size `128`, with a minimum value of `0.0` and a maximum of `127.0`. In this case Vult will split the range of the function into 128 segments and for each segment a second order polynomial that fits the values is calculated. All the coefficients are precalculated. The above function will be replaced by the following code (for a size 8 table):

<div class="vult_code" id="snipet-2">
// Precalculated Coefficients
val pitchToRate_c0 = [0.000185392265661,0.000552833927475,0.00450645672882,0.0293666851807,0.154117332865,0.706561261141,2.96582942265,11.7094517663];
val pitchToRate_c1 = [9.2285157274e-06,-2.89774227687e-05,-0.000240335947519,-0.0011351335136,-0.00451980896512,-0.0165475723334,-0.0576226599653,-0.194080429312];
val pitchToRate_c2 = [5.34353264823e-07,1.52390239335e-06,4.34596110343e-06,1.2394086389e-05,3.53462384489e-05,0.000100802635463,0.000287475325559,0.000819840280938];
// function with the new body
fun pitchToRate(d){
   val index = clip(int(0.0551181102362 * d),0,127);
   return get(pitchToRate_c0,index) + d * (get(pitchToRate_c1,index) + get(pitchToRate_c2,index) * d);
}
</div>

This new function is much faster that the one using the exponential. Thanks to this optimization I can run more complex code in the Teensy.

One cool thing is about this feature is that it can convert any function of one input and one output to a table. For example, in the following program the function `wave_table` is replaced by a table avoiding calculating three `sine` functions.


<div class="vult_code" id="snipet-3">fun pitchToRate(d) @[table(size=127,min=0.0,max=127.0)] {
   return 0.00018539226566085504 * exp(0.057762265046662105 * d);
}

fun phasor(pitch){
    mem phase;
    val rate = pitchToRate(pitch);
    phase = (phase + rate) % 1.0;
    return phase;
}

fun wave_table(x) @[table(size=127,min=0.0,max=4.0)] {
    val pi = 3.14159265359;
    return (sin(2.0*pi*x) + sin(2.0*pi*x*2.0) + sin(2.0*pi*x*4.0))/3.0;
}
</div>

This feature works as well with fixed-point code. In one of the benchmarks that I ran I got the following results:

| Representation |    No tables   |  With tables |
|----------------|----------------|--------------|
| Floating-point |    1.57 s      |     1.11 s   |
| Fixed-point    |    3.10 s      |     0.94 s   |

We can see that for fixed-point it really had a impact in the performance. The benchmark now runs 3 times faster.

<script type="text/javascript" src="../../javascripts/external/ace/ace.js"></script>
<script type="text/javascript" src="../../javascripts/main.js"></script>

