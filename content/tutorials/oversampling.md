+++
date = "2016-09-01"
description = ""
tags = []
title = "Oversampling"
topics = []
layout = "tutorial"

+++

One common technique in Digital Signal Processing is Oversampling. Oversampling is often necessary when modeling filters that come from electric circuits or that present nonlinearities or that can become unstable for certain parameters.

In order to oversample a filter we need for every sample perform `N` steps. I a language like Vult, that hides the internal memory of a function, oversampling may seem not very obvious.

In order to perform oversampling, we are gonna use a feature of Vult that allow us to name and reuse the memory created by a function.

In the previous tutorial `Easy DSP with Vult` we coded the following low pass filter. 

<div class="vult_code" id="tut4-1"> // returns true when the input changes
fun change(current:real) : bool {
   mem previous;
   val result = current <> previous;
   previous = current;
   return result;
}

fun lowpass(x,w0,q) {
    mem b0,b1,b2,a1,a2;
    if(change(w0) || change(q)) {
        val cos_w = cos(w0);
        val alpha = sin(w0)/(2.0*q);
        val den =  1.0 + alpha;
        a1 = (-2.0*cos_w)/den;
        a2 = (1.0-alpha)/den;
        b0 = (1.0-cos_w)/(2.0*den);
        b1 = (1.0-cos_w)/den;
        b2 = (1.0-cos_w)/(2.0*den);
    }
    return biquad(x,b0,b1,b2,a1,a2);
}
</div>

This filter becomes unstable when the parameter `q` is high and the frequency `w0` approaching `2Pi` for that reason we cannot completely open the filter.

The following code shows the low pass filter with 2x of oversampling:

<div class="vult_code" id="tut4-2">fun lowpass_2x(x,w0,q) {
    val fixed_w0 = w0/2.0;
    // first call to lowpass with context 'inst'
    _  = inst:lowpass(x,fixed_w0,q);
    // second call to lowpass with the same context 'inst'
    val y = inst:lowpass(x,fixed_w0,q);
    return y;
}
</div>

First we need to fix the frequency `w0`. From Audio-EQ-Cookbook we have the formula `w0 = 2*pi*f0/Fs`, since we have double the sampling rate, it is enough to divide the original `w0` by two. Then we use the feature of Vult that allow us to name the context of a function and reuse them. By preceding a call of a function with memory with an identifier and colon we can assign a name to the context e.g. `inst:lowpass(...)`. If we want to call again the function and not creating a new context we have to use the same name.

if we want to create modify the filter above and make it stereo we can do it as follows:

<div class="vult_code" id="tut4-3">fun lowpass_2x(l,r,w0,q) {
    val fixed_w0 = w0/2.0;
    // process the left side
    _         = left:lowpass(l,fixed_w0,q);
    val l_out = left:lowpass(l,fixed_w0,q);
    // process the right side
    _         = right:lowpass(l,fixed_w0,q);
    val r_out = right:lowpass(r,fixed_w0,q);
    return l_out,r_out;
}
</div>

Notice that we have named the contexts of the functions `left` and `right`. In a similar way, we can easily make this filter with 4x of oversampling.

<div class="vult_code" id="tut4-4">fun lowpass_4x(l,r,w0,q) {
    val fixed_w0 = w0/4.0;
    // process the left side
    _         = left:lowpass(l,fixed_w0,q);
    _         = left:lowpass(l,fixed_w0,q);
    _         = left:lowpass(l,fixed_w0,q);
    val l_out = left:lowpass(l,fixed_w0,q);
    // process the right side
    _         = right:lowpass(l,fixed_w0,q);
    _         = right:lowpass(l,fixed_w0,q);
    _         = right:lowpass(l,fixed_w0,q);
    val r_out = right:lowpass(r,fixed_w0,q);
    return l_out,r_out;
}
</div>

In the next tutorial we are gonna learn how to export code to C/C++.

<script type="text/javascript" src="../../javascripts/external/ace/ace.js"></script>
<script type="text/javascript" src="../../javascripts/main.js"></script>
