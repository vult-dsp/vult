#!/bin/bash
make
js_of_ocaml vult_node.byte
cp vult_node.js ./node/public/javascripts/
