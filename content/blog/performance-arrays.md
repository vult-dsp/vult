+++
date = "2017-08-21T21:07:02+03:00"
title = "Performance Tips: the cost of allocation"
layout = "post"

+++

Looking at the performance results of algorithms using arrays I noticed something very drastic. LuaJIT was terribly slow compared to the other languages.

<!--more-->

<table class="table">
<thead>
   <tr> <th> Algorithm </th> <th> C++ float </th> <th> C++ fixed</th> <th> LuaJIT</th> <th> JavaScript</th> </tr>
</thead>
<tbody>
   <tr> <td> Ladder Euler </td> <td> 4.160 ms/s  </td> <td> 5.600 ms/s </td> <td> <b>306.951</b> ms/s </td> <td>  20.63 ms/s </td> </tr>
   <tr> <td> Ladder Heun</td> <td> 6.480 ms/s  </td> <td> 10.210 ms/s </td> <td> <b>855.072</b> ms/s </td> <td> 64.650 ms/s </td> </tr>
   <tr> <td> Rescomb</td> <td> 2.840 ms/s  </td> <td> 1.870 ms/s </td> <td> 2.286 ms/s </td> <td> 9.01 ms/s </td> </tr>
   <tr> <td> Delay</td> <td> 2.210 ms/s  </td> <td> 9.340 ms/s </td> <td> 3.130 ms/s </td> <td> 6.370 ms/s </td> </tr>
</tbody>
</table>

All these examples use arrays. The main difference is that the `Ladder` examples use small local arrays to functions, while the other examples use large arrays as `mem` variables. Here's a snippet of the code:

<div class="vult_code" id="snipet-1">fun euler(input, cut, res) {
   mem fh;
   mem p[4];
   val dpt[4];  /// local array
   if(Util.change(cut)) {
      fh = tune(cut);
   }
   _ = ladder_step(input, fh, res, p, dpt);
   p[0] = p[0] + dpt[0];
   p[1] = p[1] + dpt[1];
   p[2] = p[2] + dpt[2];
   p[3] = p[3] + dpt[3];
   return p[3];
}
</div>

The Lua code uses the LuaJIT FFI to create C arrays. The motivation of using it was because, as show [here](http://luajit.org/ext_ffi.html), you can get better performance with C arrays compares to Lua arrays. This is because Lua does not have native arrays, they are tables indexed with integers.

For the line `val dpt[4];` the generated Lua code looks as follows:
```
dpt = ffi.new("double[?]",4)
```

It looks like every time a local array is used, even for the simplest thing, a new object is allocated. When the object is not used, the memory needs to be collected. In comparison to C++, the code will look as follows:

```
double dpt[4];
```

The first thing I tried was changing the Lua code to use regular Lua arrays instead of the FFI.


<table class="table">
<thead>
   <tr> <th> Algorithm </th> <th> LuaJIT FFI arrays</th> <th> LuaJIT regular arrays</th>  </tr>
</thead>
<tbody>
   <tr> <td> Ladder Euler </td> <td> 306.951 ms/s  </td> <td> 3.541 ms/s </td> </tr>
   <tr> <td> Ladder Heun</td> <td> 855.072 ms/s  </td> <td> 7.193 ms/s </td> </tr>
   <tr> <td> Rescomb</td> <td> 2.286 ms/s </td> <td> 3.794 ms/s </td> </tr>
   <tr> <td> Delay</td> <td> 3.130 ms/s  </td> <td> 3.715 ms/s </td> </tr>
</tbody>
</table>

Even though for large arrays the performance we pay a little bit in performance, the overall gain is bigger.

## Conclusion

When using LuaJIT, allocating small and short lived objects using FFI has very large impact in the performance of the code.

<script type="text/javascript" src="../../javascripts/external/ace/ace.js"></script>
<script type="text/javascript" src="../../javascripts/main.js"></script>
