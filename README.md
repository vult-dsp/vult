![Vult](/other/Images/Vult.png?raw=true "Vult")

[![Build Status](https://travis-ci.org/modlfo/vult.svg?branch=master)](https://travis-ci.org/modlfo/vult) [![Stories in Ready](https://badge.waffle.io/modlfo/vult.svg?label=ready&title=Ready)](http://waffle.io/modlfo/vult)

Vult is a simple and powerful language to program high-performance algorithms that may run in small microprocessors or microcontrollers. Vult is specially useful when programming Digital Signal Processing (DSP) algorithms like audio effects or synthesizers.

The Vult compiler is a transcompiler, which takes Vult code and produces plain C/C++ code that can be compiled in any platform with a C/C++ compiler, for example: Arduino or Teensy boards. Vult can also generate JavaScript that can be run directly in the browser or C/C++ that can be compiled as Pure Data externals. Code written in Vult has freedom.

Check the documentation and tutorial in http://modlfo.github.io/vult/

## Credits

Vult is maintained by: Leonardo Laguna Ruiz with the help of Carl Jönsson and Johan Rhodin

Logo design by: John Klimt https://www.facebook.com/JohnKlimt

## Installing

### Requirements

- Ocaml compiler >= 4.02

#### Ocaml Libraries

- ocaml-containers >= 0.15
- ppx_deriving >= 2.0
- pla >= 1.0
- [Optional] ounit >= 2.0 (to run the tests)
- [Optional] js_of_ocaml >= 2.6 (to run build the web functions)

### Installing the tools
 The simplest way to instal the requirements is with OPAM (https://opam.ocaml.org/)
```
$ opam switch 4.02.3
$ opam install containers ppx_deriving pla
```
Optionally to run the tests and building the web functions:
```
$ opam install ounit js_of_ocaml
```
### Compiling Vult
```
$ ./configure
$ make
```
In platforms without make you can build with the following command:
```
$ ocaml setup.ml -build
```
Running tests:
```
$ ./configure --enable-tests
$ make test
```
Building the web functions:
```
$ ./configure --enable-js
$ make
$ js_of_ocaml vult_node.byte
```
### Basics

To generate C/C++ code you with floating point arithmetics you have execute vult as follows:

```
$ ./vultc -ccode infile.vult -o outfile
```

This will produce the files `outfile.h` and `outfile.cpp`. In order to compile and link these files you need to include in your project the files `runtime/vultin.h` and `runtime/vultin.c`.

To generate code with fixed point arithmetics you need to execute:
```
$ ./vult -ccode -real fixed infile.vult -o outfile
```

Fixed point arithmetics are performed in q16 format; 16 bits for the integer part and 16 for the decimal.

Vult provides a few templates for example to generate objects compatible with the Teensy Audio Library or Pure Data externals.

You can see this repositories for an example:

- Pure Data https://github.com/modlfo/pd-vult-example
- Teensy Audio https://github.com/modlfo/teensy-vult-example

### Roadmap

You can see what we are working on and what's planned for Vult in the waffle.io board https://waffle.io/modlfo/vult

### Contact

If you want to receive notifications for future development of this project you can join to the google group http://groups.google.com/d/forum/vult


