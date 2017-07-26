![Vult](/other/Images/Vult.png?raw=true "Vult")

[![Build Status](https://travis-ci.org/modlfo/vult.svg?branch=master)](https://travis-ci.org/modlfo/vult) [![Build status](https://ci.appveyor.com/api/projects/status/07x9yqby88bh3q8j?svg=true)](https://ci.appveyor.com/project/modlfo/vult) [![Stories in Ready](https://badge.waffle.io/modlfo/vult.svg?label=ready&title=Ready)](http://waffle.io/modlfo/vult)

Vult is a simple and powerful language to program high-performance algorithms that may run in small microprocessors or microcontrollers. Vult is specially useful when programming Digital Signal Processing (DSP) algorithms like audio effects or synthesizers.

The Vult compiler is a transcompiler, which takes Vult code and produces plain C/C++ code that can be compiled in any platform with a C/C++ compiler, for example: Arduino or Teensy boards. Vult can also generate JavaScript that can be run directly in the browser or C/C++ that can be compiled as Pure Data externals. Code written in Vult has freedom.

Check out the documentation and tutorial in http://modlfo.github.io/vult/ or take a look at the [Wiki](https://github.com/modlfo/vult/wiki).

### Basics

To generate C/C++ code with floating point arithmetic you have to execute vult as follows:

```
$ vultc -ccode infile.vult -o outfile
```

This will produce the files `outfile.h` and `outfile.cpp`. In order to compile and link these files you need to include in your project the files `runtime/vultin.h` and `runtime/vultin.c`.

To generate code with fixed point arithmetics you need to execute:
```
$ vultc -ccode -real fixed infile.vult -o outfile
```

Fixed point arithmetics are performed in q16 format; 16 bits for the integer part and 16 for the decimal.

Vult provides a few templates; for example to generate objects compatible with the Teensy Audio Library or Pure Data externals.

You can check these repositories for examples:

- PureData https://github.com/modlfo/vult/tree/master/examples
- Teensy Audio https://github.com/modlfo/teensy-vult-example

In the Wiki

- [Language Reference](https://github.com/modlfo/vult/wiki/Language-Reference)
- [Command Line Options](https://github.com/modlfo/vult/wiki/Command-Line-Options)
- [Videos and Examples](https://github.com/modlfo/vult/wiki/Videos-and-Examples)

## Credits

Vult is maintained by: Leonardo Laguna Ruiz with the help of Carl Jönsson and Johan Rhodin

Logo design by: John Klimt https://www.facebook.com/JohnKlimt

### Contact

If you want to receive notifications for future development of this project you can join to the google group http://groups.google.com/d/forum/vult

E-mail: [my-github-user-name]@gmail.com

## Installing

There are three ways of getting the Vult compiler:
- using the npm (node.js) package manager
- downloading the released binaries
- building from source

The npm (node.js) version is updated frequently based on the trunk branch of this repository. This is the prefered mode of installing if you don't want to build from the sources. The binary version (native executable) is updated with every minor version of Vult.

If you want to get the latest changes as soon as possible then building from the sources is the recommended method.

## Installing (or updating) with npm

You need to have `node.js` and `npm` installed.

```
$ npm install vult -g
```
This will install provide `vultc` command in your system. If `vultc` is not available for any reason. You can run the main file with node as follows:
```
$ node vultjs.js
```
Alternatively you can download the file `vultjs.js` from https://github.com/modlfo/vultjs and run it with node.

## Compile from Source

### Requirements

- Ocaml compiler >= 4.03

#### Ocaml Libraries

- containers >= 1.2
- ppx_deriving >= 4.1
- pla >= 1.1
- ollvm = 0.99
- [Optional] ounit >= 2.0 (to run the tests)
- [Optional] js_of_ocaml >= 3.0 (to build the web functions)

### Installing the tools
 The simplest way to instal the requirements is with OPAM (https://opam.ocaml.org/)
```
$ opam switch 4.04.2
$ opam install containers ppx_deriving pla ollvm
```
Optionally to run the tests and building the web functions:
```
$ opam install ounit js_of_ocaml js_of_ocaml-ppx
```
### Compiling Vult
```
$ make
```
In platforms without make you can build with the following command:
```
$ ocamlbuild -use-ocamlfind src/vultc.native
```
Running tests:
```
$ make test
```
Building the web functions:
```
$ make js
```

