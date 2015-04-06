#!/bin/bash
ocamlbuild test.native -use-ocamlfind -pkg oUnit -pkg containers -pkg str
./test.native
