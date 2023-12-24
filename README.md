![Vult](/other/Images/Vult.png?raw=true "Vult")

[![Build Status](https://travis-ci.org/modlfo/vult.svg?branch=master)](https://travis-ci.org/modlfo/vult) [![Build status](https://ci.appveyor.com/api/projects/status/07x9yqby88bh3q8j?svg=true)](https://ci.appveyor.com/project/modlfo/vult) [![Coverage Status](https://coveralls.io/repos/github/modlfo/vult/badge.svg?branch=master)](https://coveralls.io/github/modlfo/vult?branch=master)

Vult is a simple and powerful language to program high-performance algorithms that may run in small microprocessors or microcontrollers. Vult is specially useful when programming Digital Signal Processing (DSP) algorithms like audio effects or synthesizers.

The Vult compiler is a transcompiler, which takes Vult code and produces plain C/C++ code that can be compiled in any platform with a C/C++ compiler, for example: Arduino or Teensy boards. Vult can also generate JavaScript that can be run directly in the browser or C/C++ that can be compiled as Pure Data externals. Code written in Vult has freedom.

Check out the documentation and tutorial in http://modlfo.github.io/vult/ or take a look at the [Wiki](https://github.com/vult-dsp/vult/wiki).

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

- WebAudio https://github.com/modlfo/vult-webaudio
- PureData https://github.com/vult-dsp/vult/tree/master/examples
- Teensy Audio https://github.com/modlfo/teensy-vult-example

In the Wiki

- [Language Reference](https://github.com/vult-dsp/vult/wiki/Language-Reference)
- [Command Line Options](https://github.com/vult-dsp/vult/wiki/Command-Line-Options)
- [Videos and Examples](https://github.com/vult-dsp/vult/wiki/Videos-and-Examples)

## Credits

Vult is maintained by: Leonardo Laguna Ruiz with the help of Carl Jönsson and Johan Rhodin

Logo design by: John Klimt https://www.facebook.com/JohnKlimt

The Vult logo is property of Leonardo Laguna Ruiz, all rights reserved.

## Installing

There are three flavors the Vult compiler:
- command line ([native executables](https://github.com/vult-dsp/vult/releases) and [node.js version](https://www.npmjs.com/package/vult))
- [node.js library](https://www.npmjs.com/package/vultlib)
- [web browser library](https://github.com/modlfo/vult/releases)


### Installing with npm

You need to have `node.js` and `npm` installed.

```
$ npm install vult -g
```

This will install provide `vultc` command in your path. Vult is updated frequently, you can use the same command to update Vult.

The native executables can be downloaded from the [releases page](https://github.com/vult-dsp/vult/releases).

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

## Compile from Source

### Requirements

- Ocaml compiler >= 4.03

#### Ocaml Libraries

- containers >= 1.2
- ppx_deriving >= 4.1
- pla >= 1.1
- result >= 0.99
- [Optional] ounit >= 2.0 (to run the tests)
- [Optional] js_of_ocaml >= 3.0 (to build the web functions)

### Installing the tools
 The simplest way to install the requirements is with OPAM (https://opam.ocaml.org/)
```
$ opam switch 4.04.2
$ opam install containers ppx_deriving pla result
```
Optionally to run the tests and building the web functions:
```
$ opam install ounit js_of_ocaml js_of_ocaml-ppx
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



