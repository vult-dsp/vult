+++
date = "2016-06-01"
description = ""
tags = []
title = "Fixed point computations"
topics = []
layout = "tutorial"

+++

The Vult language has support for performing fixed-point arithmetics https://en.wikipedia.org/wiki/Fixed-point_arithmetic

By default, number representations consist of 16 bits for the decimal part and 16 bits for the integer part. The smallest absolute number that can be represented is 0.0000152588, while the largest is approximately 32768.

Fixed-point numbers prove highly beneficial in microcontrollers lacking a floating-point arithmetic unit, such as Arduino or Raspberry Pi Pico boards. When utilizing fixed-point numbers, the processor conducts computations using integers instead of floating-point numbers, often resulting in increased speed. Check out some of the numbers I obtained in the following links:

[Performance of mod](https://vult-dsp.github.io/vult/blog/performance-mod/)

[Creation of tables](https://vult-dsp.github.io/vult/blog/tables/)

There are two alternatives to use fixed-point code:

- Full program in fixed-point
- Mixed fixed and floating point

### Full program in fixed-point

You can generate your code using fixed-point by passing the argument `-real fixed` as shown below:

```
$ vultc -ccode -real fixed test.vult
```

This will replace all computations involving real numbers to fixed-point computations. Take as an example the following code:

<div class="vult_code" id="example1">// file: test.vult
fun foo(d) {
   return 0.00018539226566085504 * exp(0.057762265046662105 * d);
}
//
fun main() {
    val x = 12.5;
    val y = 34.0;
    val z = x + y;
    val w = foo(z);
}
</div>

When generating fixed-point code we get the following C++ code. We can see that all multiplications have been replaced by a call to the function `fix_mul`, the `exp` function is now `fix_exp` and the real numbers have been changed to their corresponding fixed-point representation.

<div class="c_code" id="example2">fix16_t Test_foo(fix16_t d){
   return fix_mul(0xc /* 0.000185 */,fix_exp(fix_mul(0xec9 /* 0.057762 */,d)));
};
//
void Test_main(){
   fix16_t x;
   x = 0xc8000 /* 12.500000 */;
   fix16_t y;
   y = 0x220000 /* 34.000000 */;
   fix16_t z;
   z = (x + y);
   fix16_t w;
   w = Test_foo(z);
}
</div>

One important thing to consider is that fixed-point computations have a larger rounding and trucation error thatn floating point. Some functions like calculating the exponential `exp` in fixed-point can be slower and less accurate. In that case, I recommend to use lookup tables as shown in [Creation of tables](https://vult-dsp.github.io/vult/blog/tables/). Using lookup tables, the `fix_exp` is replaced by a couple additions and multiplications.

Since the range of the fixed point numbers is limited some values need to be scaled. Consider the following example where a function takes the samplerate as parameter and has the literal `44100.0`.

<div class="vult_code" id="example3">fun calculation(hz) {
    return 44100.0 / (2.0 * hz);
}
</div>

If we try to compile this code we get the following error ` This value '44100.0' cannot be represented with fixed-point numbers`.

One way that we could solve this problem is by performing some arithmetic simplifications and writting the expression as `(44100.0 / 2.0) / hz`. Now the code will compile because `44100.0 / 2.0 = 22050.0` and is in the fixed-point range. However, there is still a problem. The input to the function `hs` is given in Hertz and it could be a large value, like `44100`, `48000` etc. The only option is to scale the computations: instead of taking Hertz we take kilo Hertz.


<div class="vult_code" id="example4">fun calculation(khz) {
    return 44.10 / (2.0 * hz);
}
</div>

Now all the calculations are in a safe range and the function is still meaningful.

Generally, when dealing with fixed-point numbers, caution is advised against using excessively large or small values. To mitigate such situations, it is recommended to scale the numbers appropriately.

### Mixed floating and fixed point

In the latest version of the Vult compiler, you can seamlessly blend fixed and floating-point arithmetic without the need to generate the entire code in fixed-point. One practical scenario arises when certain calculations demand higher precision or cannot be conveniently scaled within the fixed-point range. Working with fixed-point numbers is straightforward; for literals, appending the postfix `x` to a number transforms it into fixed-point. Additionally, a new type, aptly named `fix16`, can be employed in arguments or to explicitly enforce a type on a variable.

In the folowing example, we want the function `foo` to be calculated using floating-point.


<div class="vult_code" id="example5">
fun foo(d:fix16) : fix16 {
   return fix16(0.00018539226566085504 * exp(0.057762265046662105 * real(d)));
}

fun main() {
    val x = 12.5x;
    val y = 34.0x;
    val z = x + y;
    val w = foo(z);
}
</div>

We can use the functions `fix16()` and `real()` to convert the values from one numeric type to other. In this case we should NOT call the compiler with the flag `-real fixed`. This will work fine:

```
$ vultc -ccode test.vult
```

<div class="c_code" id="example6">fix16_t Test_foo(fix16_t d){
   return float_to_fix((0.000185392265661f * expf((0.0577622650467f * fix_to_float(d)))));
};
//
void Test_main(){
   fix16_t x;
   x = 0xc8000 /* 12.500000 */;
   fix16_t y;
   y = 0x220000 /* 34.000000 */;
   fix16_t z;
   z = (x + y);
   fix16_t w;
   w = Test_foo(z);
}
</div>

As you see in the code, the `Test_foo` uses floating-point while the rest of the computations are explicity changed to fixed-point.

<script type="text/javascript" src="../../javascripts/external/ace/ace.js"></script>
<script type="text/javascript" src="../../javascripts/main.js"></script>