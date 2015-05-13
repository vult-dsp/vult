![Vult](/other/Images/Vult.png?raw=true "Vult")

Vult is a simple and powerful language to program high-performance algorithms that may run in small microprocessors or microcontrollers. Vult is specially useful when programming  Digital Signal Processing (DSP) algorithms like audio effects or synthesizers.

The Vult compiler is actually a transcompiler which takes Vult code and produces plain C code that can be compiled in any platform with a C/C++ compiler, for example: Arduino or Teensy boards. Javascript support is coming soon. Code written in Vult has freedom.

Check the documentation and tutorial in http://modlfo.github.io/vult/

## Credits

Logo design by:
- John Klimt https://www.facebook.com/JohnKlimt

## Installing

### Requirements

- Ocaml compiler >= 4.02
- [Optional] Node.js >= 10.33 (to run the web interpreter)

#### Ocaml Libraries

- ocaml-containers >= 0.6.1
- ppx_deriving >= 2.0
- ocamlgraph >= 1.8.6
- [Optional] js_of_ocaml >= 2.5 (to run the web interpreter)
- [Optional] oUnit >= 2.0 (to run the tests)

### Installing the tools
 The simplest way to instal the requirements is with Opam (https://opam.ocaml.org/)
```
$ opam switch 4.02.1
$ opam install containers
$ opam install ppx_deriving
```
Optionally to run the tests and build the web interpreter:
```
$ opam install js_of_ocaml
$ opam install ounit
```
### Compiling Vult
```
$ ocamlbuild -use-ocamlfind -pkg containers -pkg str vultc.native
```

### Compiling the web interpreter

First you need to install all the node.js dependencies:
```
$ cd node
$ npm install
$ cd ..
```
Then you can compile the Vult web interpreter:
```
$ ./scripts/build_node.sh
```
### Running the web interpreter
```
$ node ./node/app.js
```
Open the browser and go to http://localhost:3000

