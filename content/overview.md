+++
date = "2016-07-12T17:59:24+02:00"
description = ""
tags = []
title = "Vult Overview"
topics = []
layout = "page"

+++

## Overview

Vult is a simple and powerful language to program high-performance algorithms that may run in small microprocessors or microcontrollers. Vult is specially useful when programming Digital Signal Processing (DSP) algorithms like audio effects or synthesizers.

## Code Generation

The Vult compiler is a transcompiler which takes Vult code and produces C/C++, JavaScript or LuaJIT code. The C/C++ code that can be compiled in any platform with a C/C++ compiler, for example: Arduino or Teensy boards. The Javascript that can be run directly in the browser.

In addition Vult provides templates that generate code that can be directly compiled into a Pure Data external or an object compatible with the Teensy Audio Library.

Code written in Vult has freedom.

## Language Features

Some of the key feature of Vult are:

- **Fixed-Point type**: The same code can be generated using float or fixed-point arithmetic. This makes it possible to run efficiently in small processors.
- **Functions with context**: Functions in Vult can have local memory and each call automatically creates a new environment. Contexts can be shared among functions.
- **Type inference and type checking**: The type system makes your code safer and will catch many of the errors made when writing code.
- **Automatic creation of interpolating functions**: Complex equations can be turned (at compile time) into optimized second order lookup tables to save computational power.
- **Embedding of WAV files**: Short WAV files can be turned into arrays and used as wave tables.

These key features enable you to write clear and simple code without sacrificing performance.

## Status
Vult is on continuous development, suggestions for new features are welcome.

If you want to receive notifications for future development of this project you can join to the google group [Vult Group](http://groups.google.com/d/forum/vult).

## Credits
Logo design by: John Klimt https://www.facebook.com/JohnKlimt

Vult is maintained by [**modlfö** (Leonardo Laguna Ruiz)](https://modlfo.github.io) with the help of Carl Jönsson and Johan Rhodin.
