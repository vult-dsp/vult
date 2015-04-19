Vult
====

Vult is a high-level language well suited for DSP and controller development.

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

