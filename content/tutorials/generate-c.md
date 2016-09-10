+++
date = "2015-07-12T21:24:58+02:00"
description = ""
tags = []
title = "Generating C/C++"
topics = []
layout = "tutorial"

+++

Once we have some Vult code written it's time to generate some C/C++ code and run it on a target.

If you have followed the installation steps show in https://github.com/modlfo/vult you will have an executable called `vultc` (most probably `vultc.native` or `vultc.byte`). This is a simple command line application that we will use to generate the code.

Here is the full code of the oversampled lowpass filter which we are gonna save in a file called `filter.vult`.


<div class="vult_code" id="tut5-1">
// returns true when the input changes
fun change(current:real) : bool {
   mem previous;
   val result = current <> previous;
   previous = current;
   return result;
}

fun biquad(x0, b0, b1, b2 ,a1 ,a2) : real {
    mem w1, w2;
    val w0 = x0 - a1*w1 - a2*w2;
    val y0 = b0*w0 + b1*w1 + b2*w2;
    w2, w1 = w1, w0;
    return y0;
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

fun lowpass_2x(x,w0,q) {
    val fixed_w0 = w0/2.0;
    // first call to lowpass with context 'inst'
    _  = inst:lowpass(x,fixed_w0,q);
    // second call to lowpass with the same context 'inst'
    val y = inst:lowpass(x,fixed_w0,q);
    return y;
}
</div>

Next we are gonna call the Vult compiler as follows:

```
$ ./vultc.native -ccode filter.vult
```

The compiler receives the flag `-ccode` which instruct the compiler to generate C/C++ code. This command will print to the standard output the generated code. If we want to save it to a file we have to call the compiler as follows:

```
$ ./vultc.native -ccode filter.vult -o filter
```

This will generate two files: `filter.h` and `filter.cpp`. Vult generates many auxiliary functions and types. For each function with memory (for example a function called `foo` in the file `Bar.vult`) there's gonna be:

- a type with the name `Bar_foo_type`
- a initialization function with the name `Bar_foo_init`
- a function with the body of the original Vult function called `Bar_foo`

In the case of the filter example the names are: `Filter_lowpass_2x_type`, `Filter_lowpass_2x_init` and `Filter_lowpass_2x`.

To use this code in a file you need to:

- create a value of type `Filter_lowpass_2x_type`
- initialize it with the function `Filter_lowpass_2x_init`
- use it with the original function `Filter_lowpass_2x`


For example:

<div class="c_code" id="tut5-2">// file main.cpp

#include "filter.h"

int main(void) {
   Filter_lowpass_2x_type filter;
   // initialization
   Filter_lowpass_2x_init(filter);

   // inputs to the function
   float x = 0.0f;
   float w = 1.0f;
   float q = 1.0f;

   // calling the function
   float result = Filter_lowpass_2x(filter,x,w,q);

   return 0;
}
</div>

In order to compile this code you need to add an include directory pointing to the location of the file `vultin.h`. This file is located in the source tree under the folder `runtime`. To link you will need to compile and link the file `vultin.c` which is located in the same place (https://github.com/modlfo/vult/tree/master/runtime)

One thing to notice is that every function with memory will have as first argument a reference to a value of it's corresponding type. In the above case, the function `Filter_lowpass_2x` returns only one value, therefore the C/C++ will return a value. If the functions return more than one value Vult will automatically generate structures for the types. Here are few Vult functions an their corresponding C/C++ functions:

<div class="vult_code" id="tut5-3">// example.vult
// single value return, no memory
fun foo1(x:real) {
	return x;
}

// single value return, with memory
fun bar1(x:real) {
	mem y = x;
	return x;
}

// multiple value return, no memory
fun foo2(x:real) {
	return x,x;
}

// multiple value return, with memory
fun bar2(x:real) {
	mem y = x;
	return x,x;
}
</div>

C/C++ declarations of the functions above:

<div class="c_code" id="tut5-4">
// single value return, no memory
float Example_foo1(float x);

// single value return, with memory
float Example_bar1(Example__ctx_type_1 &_ctx, float x);

// multiple value return, no memory
void Example_foo2(float x, _tuple_real_real &_output_);

// multiple value return, with memory
void Example_bar2(Example__ctx_type_3 &_ctx, float x,
   _tuple_real_real &_output_);
</div>

In the examples above you can see that Vult adds an extra argument at the end that is used to return multiple values.


## Generating fixed-point code

When generating C/C++ code Vult by default uses floating point arithmetic (`float` numbers). Floating point code is very efficient in big processors like the x86. However when compiled to small microcontrollers (like the ones found in Arduinos or some ARM processors) the code can be very inefficient because these processors do not have a dedicated floating point arithmetic unit.

Alternatively, fixed-point calculations can be used to perform computations with decimals (like `2.0*1.5`). Fixed-point arithmetic encodes the decimal numbers as integers and uses the integer arithmetic unit in small processors to perform calculations (https://en.wikipedia.org/wiki/Fixed-point_arithmetic). The result is that the operations can be performed efficiently at the expense of numeric precision.

Vult can generate all operations with real numbers as fixed-point with the format q16.16. This means that 16 bits a used to represent integers and 16 bits for decimals. This implies that the largest number that can be represented is `32767.0` and the smallest `0.0000152588` (the values are signed). Therefore, when generating code with fixed-point numbers one needs to be careful of not going beyond this numbers.

To generate code with fixed-point numbers we need to call Vult as follows:

```
$ ./vultc.native -ccode -real fixed filter.vult -o filter
```

This command will generate use the type `fix16_t` instead of `float`. For example, the declaration of the function `lowpass_2x` changes to:

<div class="c_code" id="tut5-5">
fix16_t Filter_lowpass_2x(Filter__ctx_type_3 &_ctx, fix16_t x,
                          fix16_t w0, fix16_t q);
</div>

The file `vultin.h` provides functions to convert among `float` `fix16_t` and `int` values.


<script type="text/javascript" src="../../javascripts/external/ace/ace.js"></script>
<script type="text/javascript" src="../../javascripts/main.js"></script>