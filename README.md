Vult
====

Vult is a high-level language well suited for DSP and controller development.

### Requirements
- Ocaml compiler >= 4.02
- ocaml-containers >= 0.6.1

### Installing the tools
 The simplest way to instal the requirements is with Opam (https://opam.ocaml.org/)
- opam switch 4.02.1
- opam install containers

### Compiling Vult

- ocamlbuild -use-ocamlfind -pkg containers vultc.native

