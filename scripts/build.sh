#!/bin/bash
rm vultc.native vultc.d.byte
ocamlbuild -use-ocamlfind -pkg containers -pkg str vultc.native vultc.d.byte
