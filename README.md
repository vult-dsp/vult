![Vult](/other/Images/Vult.png?raw=true "Vult")

Vult is a simple and powerful language to program high-performance algorithms that may run in small microprocessors or microcontrollers. Vult is specially useful when programming Digital Signal Processing (DSP) algorithms like audio effects or synthesizers.

The Vult compiler is a transcompiler, which takes Vult code and produces plain C code that can be compiled in any platform with a C/C++ compiler, for example: Arduino or Teensy boards. Vult can also generate JavaScript that can be run directly in the browser. Code written in Vult has freedom.

Check the documentation and tutorial in http://modlfo.github.io/vult/

## Credits

Vult is maintained by: Leonardo Laguna Ruiz with the help of Carl Jönsson and Johan Rhodin

Logo design by: John Klimt https://www.facebook.com/JohnKlimt

## Installing

### Requirements

- Ocaml compiler >= 4.02

#### Ocaml Libraries

- ocaml-containers >= 0.6.1
- ppx_deriving >= 2.0
- ocamlgraph >= 1.8.6
- [Optional] oUnit >= 2.0 (to run the tests)

### Installing the tools
 The simplest way to instal the requirements is with OPAM (https://opam.ocaml.org/)
```
$ opam switch 4.02.1
$ opam install containers ppx_deriving ocamlgraph
```
Optionally to run the tests:
```
$ opam install ounit
```
### Compiling Vult
```
$ ./scripts/build.sh
```
In platforms without bash you can build with the following command:
```
$ ocamlbuild -use-ocamlfind -pkg containers -pkg str -pkg ocamlgraph -pkg ppx_deriving vultc.native
```
### Roadmap

#### v0.2 (branch exp_stmt)
- Better syntax checking
- External functions
- Constant arrays
- More powerful shared context

#### v0.1 (Completed)
- Proof of concept
- C code generation
- Javascript generation

### Contact

If you want to receive notifications for future development of this project you can join to the google group http://groups.google.com/d/forum/vult


