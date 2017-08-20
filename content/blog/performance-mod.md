+++
date = "2017-08-20T21:21:01+03:00"
title = "Performance Tips: floating-point mod"
layout = "post"

+++

Recently I implemented in Vult a simpler way for me to measure the performance of the generated code. Running the command `$ make perf` generates code for all languages and runs it for most of the examples.

<!--more-->

The results are displayed in ms/s (milliseconds taken to render one second of audio). For most of the examples the C++ with fixed-point is the fastest. In a few instances is slower than C++ with floating point. I'm still investigating these cases. LuaJIT is slightly slower than C++ and then comes JavaScript.

When rendering the different implementations of Saw wave oscillators we get the following result (MacBook Air with 1,4 GHz Intel Core i5):

<table class="table">
<thead>
   <tr> <th> Algorithm </th> <th> C++ float </th> <th> C++ fixed</th> <th> LuaJIT</th> <th> JavaScript</th> </tr>
</thead>
<tbody>
   <tr> <td> Saw EPTR </td> <td> 0.368 ms/s  </td> <td> 0.264 ms/s </td> <td> 0.418 ms/s </td> <td>  0.539 ms/s </td> </tr>
   <tr> <td> Saw PTR W=1</td> <td> 0.385 ms/s  </td> <td> 0.255 ms/s </td> <td> 0.396 ms/s </td> <td> 0.583 ms/s </td> </tr>
   <tr> <td> Saw PTR W=2</td> <td> 0.384 ms/s  </td> <td> 0.286 ms/s </td> <td> 0.434 ms/s </td> <td> 1.057 ms/s </td> </tr>
   <tr> <td> Saw R?</td> <td> 0.821 ms/s  </td> <td> 0.284 ms/s </td> <td> 0.446 ms/s </td> <td> 1.523 ms/s </td> </tr>
   <tr> <td> Saw BLIT</td> <td> 1.593 ms/s  </td> <td> 1.729 ms/s </td> <td> 1.866 ms/s </td> <td> 3.229 ms/s </td> </tr>
</tbody>
</table>

A few strange things can be spotted by looking at the table. The Saw PRT gets almost twice as slow for Js. I'm not gonna investigate that one here. The one that is more interesting is the Saw R? going from fixed-point to floating-point; it gets 3 to 4 times slower.

The implementation (part of it) looks as follows:

<div class="vult_code" id="snipet-1"> // File: saw_r.vult
fun process(cv) {
   mem inc;
   if(Util.change(cv))
      inc = Util.cvToRate(cv);
   val i = if inc < eps() then eps() else inc;
   // generate a ramp from -1.0  to 1.0
   mem phase = (2.0 * inc + phase) % 2.0;
   val ph = phase - 1.0;
   val o = 0.0;
   ...
   ...
}
</div>

After playing a bit I found the problem. The line `mem phase = (2.0 * inc + phase) % 2.0;` uses the `mod` operation on a floating-point number in order to wrap the phase. When this is converted to fixed-point (integers) the `mod` operation is performed on integers which is much faster than on floating-point.

By changing that line to the following lines:

<div class="vult_code" id="snipet-2">mem phase = (2.0 * inc + phase);
phase = if phase > 2.0 then phase - 2.0 else phase;</div>

we avoid calling `fmod` which seems to be more expensive. This will make the fixed-point a little bit slower, but the floating-point becomes faster. The updated table after performing the change is as follows:

<table class="table">
<thead>
   <tr> <th> Algorithm </th> <th> C++ float </th> <th> C++ fixed</th> <th> LuaJIT</th> <th> JavaScript</th> </tr>
</thead>
<tbody>
   <tr> <td> Saw R?</td> <td> 0.426 ms/s  </td> <td> 0.322 ms/s </td> <td> 0.446 ms/s </td> <td> 1.149 ms/s </td> </tr>
</tbody>
</table>

## Conclusion

Avoid using `%` (mod on floats) unless is strictly necessary.

## Used Band Limited Algorithms

EPTR : Efficient Polynomial Transition Regions

PTR : Polynomial Transition Regions

BLIT : Band-Limited Impulse Train

R? : I don't know the name of this algorithm

Among these algorithms, the only one that produces real band-limited waveforms is the BLIT.


<script type="text/javascript" src="../../javascripts/external/ace/ace.js"></script>
<script type="text/javascript" src="../../javascripts/main.js"></script>
