![Vult](/other/Images/Vult.png?raw=true "Vult")

[![Build Status](https://travis-ci.org/modlfo/vult.svg?branch=master)](https://travis-ci.org/modlfo/vult) [![Build status](https://ci.appveyor.com/api/projects/status/07x9yqby88bh3q8j?svg=true)](https://ci.appveyor.com/project/modlfo/vult) [![Coverage Status](https://coveralls.io/repos/github/modlfo/vult/badge.svg?branch=master)](https://coveralls.io/github/modlfo/vult?branch=master)

Vult is a simple and powerful language to program high-performance algorithms that may run in small microprocessors or microcontrollers. Vult is specially useful when programming Digital Signal Processing (DSP) algorithms like audio effects or synthesizers.

The Vult compiler is a transcompiler, which takes Vult code and produces plain C/C++ code that can be compiled in any platform with a C/C++ compiler, for example: Arduino or Teensy boards (using fixed-point arithmetics). Vult can also generate JavaScript that can be run directly in the browser or C/C++ that can be compiled as Pure Data externals. Code written in Vult has freedom.

Check out the documentation and tutorial in http://vult-dsp.github.io/vult/ or take a look at the [Wiki](https://github.com/vult-dsp/vult/wiki).

### Basics

To generate C/C++ code with floating point arithmetic you have to execute vult as follows:

```
$ vultc -ccode infile.vult -o outfile
```

This will produce the files `outfile.h` and `outfile.cpp`. In order to compile and link these files you need to include in your project the files `runtime/vultin.h` and `runtime/vultin.cpp`.

To generate code with fixed point arithmetics you need to execute:
```
$ vultc -ccode -real fixed infile.vult -o outfile
```

Fixed point arithmetics are performed in q16 format; 16 bits for the integer part and 16 for the decimal.

Vult provides a few templates; for example to generate objects compatible with the Teensy Audio Library or Pure Data externals.

You can check these repositories for examples:

- PicoADK (Raspberry Pi Pico) https://github.com/DatanoiseTV/PicoADK-Firmware-Template
- WebAudio https://github.com/modlfo/vult-webaudio
- PureData https://github.com/vult-dsp/vult/tree/master/examples
- Teensy Audio https://github.com/modlfo/teensy-vult-example

In the Wiki

- [Language Reference](https://github.com/vult-dsp/vult/wiki/Language-Reference)
- [Command Line Options](https://github.com/vult-dsp/vult/wiki/Command-Line-Options)
- [Videos and Examples](https://github.com/vult-dsp/vult/wiki/Videos-and-Examples)

The DSP code for all [Vult VCV Rack](https://modlfo.github.io/VultModules/) and [Vult Eurorack](https://www.vult-dsp.com/hardware) modules is written in the Vult language.

## Credits

Vult is maintained by: Leonardo Laguna Ruiz with the help of Carl Jönsson and Johan Rhodin

Logo design by: John Klimt https://www.facebook.com/JohnKlimt

The Vult logo is property of Leonardo Laguna Ruiz, all rights reserved.

## Installing

There are three flavors the Vult compiler:
- command line ([native executables](https://github.com/vult-dsp/vult/releases) and [node.js version](https://www.npmjs.com/package/vult))
- [node.js library](https://www.npmjs.com/package/vultlib)
- [web browser library](https://github.com/vult-dsp/vult/releases)

### Installing with npm

You need to have `node.js` and `npm` installed. This version of the compiler is 4x slower than the native version. If you have a large project it is recommended to install the native compiler. To install the Js version with npm run:

```
$ npm install vult -g
```

This will install provide `vultc` command in your path. Vult is updated frequently, you can use the same command to update Vult.

### Installing the native executable

The native executables can be downloaded from the [releases page](https://github.com/vult-dsp/vult/releases). Pick the executable corresponding to your operating system and place it in a location pointed by your PATH variable.

### Installing the compiler as a JavaScript library

To install the node.js library use:
```
$ npm install vultlib
```

### Embedding in a Web page

The compiler can be embedded in a web page providing and it provides all the functionality.
```
<script src="https://modlfo.github.io/vult/javascripts/vultweb.js"></script>
```

For an example check:

https://github.com/modlfo/vult-webaudio

## A glipse of Vult

Vult is based on the idea that a single function can be a full processing unit. Think of it as being a block or a box in your favorite graphical audio environment e.g. a box in PureData. Functions can store information. Consider the following code for a digital biquad filter:

```
fun biquad(x0, b0, b1, b2 ,a1 ,a2) : real {
    mem w1, w2;
    val w0 = x0 - a1 * w1 - a2 * w2;
    val y0 = b0 * w0 + b1 * w1 + b2 * w2;
    w2, w1 = w1, w0;
    return y0;
}
```

That function declares two memory (`mem`) variables `w1` and `w2` which will remember the state of every instance of this filter.

Every time we need a new `biquad` instance, we just need to call the function. For example:

```
fun process(i1, i2) {
   val o1 = biquad(i1, ....);
   val o2 = biquad(i2, ....);
}

```
The filter that process the signal `i1` is completely independent of the one that process `i2`.

Vult also has a bunch of useful features, like the possibility of creating compile-time lookup tables and embedding wav files (with and without interpolation). For example, the following function is replaced by a lookup table which would be more efficient than executing the body of the function.

```
fun pitchToRate(d) @[table(size=127,min=0.0,max=127.0)] {
   return 0.00018539226566085504 * exp(0.057762265046662105 * d);
}
```

The Vult compiler can generate C++ code that performs all the calculations using fixed-point arithmetics. Fixed-point arithmetics executes faster than floating-point arithmetics in processors that do no have a dedicated FPU. For example AVR Arduinos, Teensy 3.2 and Raspberry Pi Pico.


## Text editor support

### SublimeText

Put the [syntax file](https://github.com/vult-dsp/vult/raw/master/other/SublimeTextSyntax/vult.sublime-syntax) in the corresponding Sublime Text `User` packages.

In Mac, the path is the following `$HOME/Library/Application Support/Sublime Text 3/Packages/User/vult.sublime-syntax`.

### Visual Studio Code

Install the extension `vult` available in the [marketplace](https://marketplace.visualstudio.com/items?itemName=modlfo.vult).

## Compile from source

If you want to compile the Vult source, it is recommended to use a Linux or Mac computer. It is possible to compile on Windows but the process is more difficult.

First you need to install Opam. Check the alternatives on https://opam.ocaml.org/

Opam can be installed on Linux using your package manager. If you use a Debian/Ubuntu based distro you can use:

```
$ sudo apt install opam
```

In Mac you can install Opam using brew

```
$ brew install opam
```

### Installing the compiler and dependencies

 The simplest way to install all requirements is with Opam.

```
$ opam switch 4.14.0
$ opam install ocamlbuild containers ppx_deriving pla result ounit js_of_ocaml js_of_ocaml-ppx
```
### Compiling Vult

To compile the native executable:
```
$ make
```

Compile the node.js code:
```
$ make jscompiler
```

Running tests (Linux and macOS):
```
$ make test
```



