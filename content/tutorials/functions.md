+++
date = "2016-11-01"
description = ""
tags = []
title = "Functions with Memory"
topics = []
layout = "tutorial"

+++


Vult has two types of functions: passive and active. Passive functions are like the examples that we saw in the Vult Language Basics, for example:

<div class="vult_code" id="tut2-0">fun square(x) {
   return x * x;
}
</div>

These functions simply receive arguments, perform computations and return a value.

One of the most useful features of Vult are the active functions. These functions have the characteristic that have internal and independent memory variables. The memory variables are declared with the keyword `mem`. For example, if we would like to make a counter in Vult we could write the following function:

<div class="vult_code" id="tut2-1">fun counter(n) {
   mem count;         // our memory variable
   count = count + n; // increases the value by 'n' and updates it
   return count;      // returns the new count
}

fun loop() {
   val x = counter(1); // every time count is called the value increases 1
}
</div>

In the code shown above, let's assume that an external function calls repeatedly the function `loop`. Every time the function `loop` is called, the function `counter` is called and increases the value of the memory variable `count`.

*One very important thing to note is that every call to an active function creates a new context.*

For example, if we have the following code:

<div class="vult_code" id="tut2-3">fun loop() {
   val first = counter(1);   // first counter, increases by 1
   val second = counter(10); // second counter, increases by 10
}
</div>

the first call to `counter` creates it's own `count` variable and is not affected by the second call. So the values of `first` will be `1, 2, 3 ...` while the values of `second` will be `10, 20, 30, ...`

Another example of using functions with memory is the following:

<div class="vult_code" id="tut2-4">// Returns 'true' every 'n' calls
fun every(n) {
   // increments the variable and returns it to 0 when larger than n
   mem count = (count + 1) % n;
   // Return true if the variable is zero
   return (count == 0);
}
</div>

This function returns `true` every `n` times is called. This can be used to limit the number of times an expensive computation is performed. For example:

<div class="vult_code" id="tut2-5">
fun loop(){
   if(every(10)) {
      .... // perform an expensive computation
   }
}
</div>

Other function that can be very useful is:

<div class="vult_code" id="tut2-6">// Returns 'true' if the input value changes
fun change(current) {
   mem previous;
   val result = current <> previous;
   previous = current;
   return result;
}
</div>

In this case, the function returns `true` every time it's value changes compared to its previous value.

In the following tutorial we will see more advanced use cases focusing on a DSP application.



<script type="text/javascript" src="../../javascripts/external/ace/ace.js"></script>
<script type="text/javascript" src="../../javascripts/main.js"></script>
