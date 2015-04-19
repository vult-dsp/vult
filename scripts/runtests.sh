#!/bin/bash
ocamlbuild test.native -use-ocamlfind -pkg ocamlgraph -pkg oUnit -pkg containers -pkg str
./test.native
