+++
date = "2016-10-01"
description = ""
tags = []
title = "Easy DSP with Vult"
topics = []
layout = "tutorial"

+++

Functions with memory simplify writing Digital Signal Processing (DSP) code since a `mem` variable can be used to as a single sample delay `z^1`. I order to implement the Digital Biquad Filter (direct form 2) we can take the equations directly from Wikipedia: https://en.wikipedia.org/wiki/Digital_biquad_filter.

We can see the architecture and the equations here:

{{< figure src="/images/biquad-df2-eq1.svg" title="" >}}
{{< figure src="/images/biquad-df2-eq2.svg" title="" >}}


{{< figure src="/images/biquad-df2-image.png" title="" >}}

We can implement these equations in Vult as follows.

<div class="vult_code" id="tut3-1">fun biquad(x0, b0, b1, b2 ,a1 ,a2) : real {
    mem w1, w2;
    val w0 = x0 - a1*w1 - a2*w2;
    val y0 = b0*w0 + b1*w1 + b2*w2;
    w2, w1 = w1, w0;
    return y0;
}
</div>

The variables `w1` and `w2` represent the terms `w[n-1]` and `w[n-2]`. The variables `y0` and `x0` represent `y[n]` and `x[n]`. The coefficients of the filter are `b0`, `b1`, `b2`, `a0` and `a2` which are calculated depending on the filter. We can see that `w1` and `w2` are declared as `mem` variables since we want them to keep their value every time the function is called.

Now that we have the architecture of the biquad, we can design filters. In order to design a filter we need to calculate the coefficients. Here we are gonna follow the formulae show in the [Audio-EQ-Cookbook](/pages/audio-eq-cookbook.html). The code for the filter is the following:

<div class="vult_code" id="tut3-2">fun lowpass(x,w0,q) {
    val cos_w = cos(w0);
    val alpha = sin(w0)/(2.0*q);
    val den =  1.0 + alpha;
    val a1 =  (-2.0*cos_w)/den;
    val a2 =  (1.0 - alpha)/den;
    val b0 = ((1.0 - cos_w)/(2.0*den));
    val b1 = (1.0 - cos_w)/den;
    val b2 = ((1.0 - cos_w)/(2.0*den));
    return biquad(x,b0,b1,b2,a1,a2);
}
</div>

In the code, `x` is the input signal `w0` is the cut frequency (in radians) and `q` controls the resonance. This filter is variable and we can adjust the cut frequency and the resonance. In order to achieve that we need to calculate the coefficients every time the cut or resonance changes. We can see in the above code that the implementation is not very efficient since it calculates the coefficients every sample. To improve it can use the function `change` that we defined in the `Functions with Memory` tutorial.

<div class="vult_code" id="tut3-3"> // returns true when the input changes
fun change(current:real) : bool {
    mem previous;
    val result = current <> previous;
    previous = current;
    return result;
}
//
fun lowpass(x,w0,q) {
    mem b0,b1,b2,a1,a2;
    if(change(w0) || change(q)) {
        val cos_w = cos(w0);
        val alpha = sin(w0)/(2.0*q);
        val den =  1.0 + alpha;
        a1 =  (-2.0*cos_w)/den;
        a2 =  (1.0 - alpha)/den;
        b0 = ((1.0 - cos_w)/(2.0*den));
        b1 = (1.0 - cos_w)/den;
        b2 = ((1.0 - cos_w)/(2.0*den));
    }
    return biquad(x,b0,b1,b2,a1,a2);
}
</div>

In the example above, we have changed the declaration of the coefficients to `mem` variables because we want to remember the previous values. By wrapping the calculation inside the `if-statement` we recalculate the coefficients every time the values of `fc` and `q` change.

Now that we have the filter, we can use it as part of a bigger program. For example:

<div class="vult_code" id="tut3-4">
fun process(x) : real {
   ... // some other here
   val s_1 = ...; // generate a signal
   val s_2 = ...; // generate a signal
   // the first filter is controlled by control_1 and control_2
   val s_1_filtered = lowpass(s_1, control_1, control_2);
   // the second filter is controlled by control_3 and control_4
   val s_2_filtered = lowpass(s_2, control_3, control_4);
   return s_1_filtered + s_2_filtered;
}
</div>

Just for you to remember, every time you call the function `lowpass` and independent memory space is generated. Therefore, the filter for signal `s_1` and `s_2` do not interfere each other.

In the next tutorial we are gonna create a state-variable filter and we are gonna show how easy is to do oversampling in Vult.


<script type="text/javascript" src="../../javascripts/external/ace/ace.js"></script>
<script type="text/javascript" src="../../javascripts/main.js"></script>