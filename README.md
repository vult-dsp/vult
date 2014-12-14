Vult
====

Vult is a high-level language well suited for DSP and controller development.

### Requirements

- Ocaml compiler >= 4.02
- [Optional] Node.js >= 10.33 (to run the web interpreter)

#### Ocaml Libraries

- ocaml-containers >= 0.6.1
- [Optional] js_of_ocaml >= 2.5 (to run the web interpreter)
- [Optional] oUnit >= 2.0 (to run the tests)

### Installing the tools
 The simplest way to instal the requirements is with Opam (https://opam.ocaml.org/)
- opam switch 4.02.1
- opam install containers
- [Optional] opam install js_of_ocaml
- [Optional] opam install ounit

### Compiling Vult

- ocamlbuild -use-ocamlfind -pkg containers vultc.native

### Compiling the web interpreter

- ocamlbuild -use-ocamlfind -pkg containers vult_node.byte -syntax camlp4o -pkg js_of_ocaml -pkg js_of_ocaml.syntax
- js_of_ocaml vult_node.byte
- cp vult_node.js ./node/public/javascripts/

### Running the web interpreter

- node ./node/app.js

Open the browser and go to http://localhost:3000

