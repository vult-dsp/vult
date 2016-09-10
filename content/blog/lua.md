+++
date = "2016-09-08T22:43:03+03:00"
title = "LuaJIT as backend for Vult"
description = ""
tags = []
topics = []
layout = "post"
+++

I have been keeping an eye on LuaJIT (http://luajit.org) for some time. It's a very interesting project and I have read very good things about it. Some time ago I made a small benchmark comparing an optimized algorithm written in C++11 against a lazy coded version in OCaml and LuaJIT. In case you are curious here are the results:

| Language | Execution Time | Lines of Code|
|----------|----------------|--------------|
| C++11    |    0.355 s     |     180      |
| OCaml    |    0.54 s      |     63       |
| LuaJIT   |    0.78 s      |     85       |

Here's a chart that plots the lines of code against the execution time.

{{< figure src="/images/benchmark1.svg" title="" >}}

I have to remark again: *The OCaml and Lua code were not written with optimization in mind*. My intention was to see how fast they program could go without putting too much effort coding.

One can see that the C++ code is much larger. On the other hand, the OCaml code is very compact. The main reason was that in OCaml I used algebraic data types which neither Lua or C++ have (https://en.wikipedia.org/wiki/Algebraic_data_type). Therefore, all the complex data types needed to be implemented with classes or tables in the case of Lua.

Recently I implemented a prototype of code generation Vult -> Lua in order to check if LuaJIT could be used to create a better live coding environment. I took the JavaScript generator and with a few modifications I got Lua support.

To generate Lua code you need to call Vult as follows:

```
$ ./vultc.native -luacode code.vult -o code
```

In order to check the performance I took one of the examples I have in Vult and made a test rendering 1000 s of audio. Here are the results:

| Language | Execution Time |
|----------|----------------|
| C++11    |    2.77 s      |
| LuaJIT   |    3.55 s      |

Here's the graphic view:

{{< figure src="/images/benchmark2.svg" title="" >}}

The results are quite promising. The code was pure Lua with exception of the arrays which are implemented using the LuaJIT FFI as `double[]`. One important thing to notice is that the LuaJIT performs all operations in `double` precision, while the C/C++ uses `float`. I'm not sure if this has a big impact or not in modern processors.

As follow up in this topic, I'm planning to do more testing to see if I can generate code that is faster in LuaJIT taking advantage of the FFI features. I'm gonna try as well to find a nice way of integrating LuaJIT + Vult into a VST or a PD external.


