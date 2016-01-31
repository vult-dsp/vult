#!/bin/bash
./configure --enable-js
make
js_of_ocaml vult_node.byte
cp vult_node.js ./node/public/javascripts/
