#!/bin/bash
rm vultc.native vultc.d.byte
ocamlbuild -use-ocamlfind -pkg containers -pkg str -pkg ocamlgraph -pkg ppx_deriving vultc.native vultc.d.byte
