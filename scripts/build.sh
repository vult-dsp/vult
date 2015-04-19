#!/bin/bash
rm vultc.native vultc.d.byte
ocamlbuild -use-ocamlfind -pkg containers -pkg str -pkg ocamlgraph vultc.native vultc.d.byte
