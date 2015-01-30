#!/bin/bash
ocamlbuild -use-ocamlfind vult_node.byte -pkg js_of_ocaml -pkg containers
js_of_ocaml vult_node.byte
cp vult_node.js ./node/public/javascripts/
